open Core_kernel
open Printf
open Sexplib
open Conv

type user =
  { name : string
  ; tz : string option
  ; tz_offset : int
  ; tz_label : string option
  }
[@@deriving yojson, sexp]

type users = user list [@@deriving yojson, sexp]

let user_of_slack (u : Slacko.user_obj) =
  { name = u.name; tz = u.tz; tz_offset = u.tz_offset; tz_label = u.tz_label }


type timezone =
  { users : users
  ; label : string option
  ; zone : Time.Zone.t
  }

let timezone ~key:tz_offset ~data:users =
  let hours = tz_offset / (60 * 60) in
  let zone = Time.Zone.of_utc_offset ~hours in
  let tz_label = List.find_map users ~f:(fun user -> user.tz_label) in
  let tz = List.find_map users ~f:(fun user -> user.tz) in
  let label = Option.first_some tz_label tz in
  { users; zone; label }


let timezones users =
  let tz_offset_map = Map.empty (module Int) in
  let tz_offset_map =
    List.fold_left users ~init:tz_offset_map ~f:(fun map u ->
        Map.add_multi map ~key:u.tz_offset ~data:u )
  in
  Map.mapi tz_offset_map ~f:timezone


let filter_active (u : Slacko.user_obj) =
  not (u.is_bot || u.deleted || u.is_ultra_restricted)


(** Download users from slack *)
let fetch_users session =
  match%lwt Slacko.users_list session with
  | `Success users ->
      let users = List.filter users ~f:filter_active in
      Lwt.return_ok users
  | #Slacko.parsed_auth_error as error ->
      Lwt.return_error error


(** Save users on disk *)
let save_users users storage =
  let%lwt oc = Lwt_io.open_file ~mode:Lwt_io.Output storage in
  let%lwt () =
    Lwt_io.write_line oc (users |> sexp_of_users |> Sexp.to_string)
  in
  let%lwt () = Lwt_io.close oc in
  Lwt.return_unit


(** Download users from slack and update storage *)
let refresh_users token storage =
  let session = Slacko.start_session token in
  print_endline "connection to slack" ;
  match%lwt fetch_users session with
  | Error e ->
      let error msg =
        Lwt.fail_with (sprintf "unable to fetch the users: %s" msg)
      in
      ( match e with
      | `Account_inactive ->
          error "account inactive"
      | `Not_authed ->
          error "not authed"
      | `Unknown_error ->
          error "unknown error :("
      | `Invalid_auth ->
          error "invalid auth"
      | `ParseFailure e ->
          error @@ sprintf "parse failure: %s" e
      | `Unhandled_error e ->
          error @@ sprintf "unhandled error %s" e )
  | Ok users ->
      printf "the new list contains %d users\n" (List.length users) ;
      let users = List.map users ~f:user_of_slack in
      save_users users storage


(** Read users from storage *)
let load_users storage =
  let%lwt ic = Lwt_io.open_file ~mode:Lwt_io.Input storage in
  let%lwt content = Lwt_io.read ic in
  let%lwt () = Lwt_io.close ic in
  content |> Sexp.of_string |> users_of_sexp |> Lwt.return


module Http = struct
  open Cohttp_lwt_unix

  type tz =
    { names : string list
    ; label : string option
    ; zone : string
    }
  [@@deriving yojson, sexp]

  type tzs = tz list [@@deriving yojson, sexp]

  let timezone tz =
    let names = List.map tz.users ~f:(fun u -> u.name) in
    { names; label = tz.label; zone = Time.Zone.name tz.zone }


  let refresh token storage =
    try%lwt
      let%lwt () = refresh_users token storage in
      Server.respond_string ~status:`OK ~body:"done" ()
    with
    | exn ->
        let body =
          sprintf "unable to refresh users:\n%s" (Exn.to_string exn)
        in
        Server.respond_string ~status:`Internal_server_error ~body ()


  let tz storage =
    let%lwt users = load_users storage in
    let timezones = timezones users in
    let timezones = Map.map timezones ~f:timezone in
    Lwt.return (Map.data timezones)


  let start port (token, storage) =
    let callback _conn req _body =
      let uri = Request.uri req in
      let headers_json = Cohttp.Header.init_with "Content-Type" "application/json" in
      match Uri.path uri with
      | "/version" ->
          Server.respond_string ~status:`OK ~body:"%%VERSION%%" ()
      | "/refresh" ->
          refresh token storage
      | "/users.sexp" ->
          let%lwt users = load_users storage in
          let body = users |> sexp_of_users |> Sexp.to_string_hum in
          Server.respond_string ~status:`OK ~body ()
      | "/users.json" ->
          let%lwt users = load_users storage in
          let body = users |> users_to_yojson |> Yojson.Safe.to_string in
          Server.respond_string ~headers:headers_json ~status:`OK ~body ()
      | "/tz.sexp" ->
          let%lwt timezones = tz storage in
          let body = timezones |> sexp_of_tzs |> Sexp.to_string_hum in
          Server.respond_string ~status:`OK ~body ()
      | "/tz.json" ->
          let%lwt timezones = tz storage in
          let body = timezones |> tzs_to_yojson |> Yojson.Safe.to_string in
          Server.respond_string ~headers:headers_json ~status:`OK ~body ()
      | _ ->
          Server.respond_string ~status:`Not_found ~body:"path not found" ()
    in
    Lwt_main.run
      (let%lwt () = Lwt_io.printlf "starting http server on port %d" port in
       let%lwt () =
         Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())
       in
       Lwt.return (`Ok ()))
end

module Text = struct
  let sexp_display users =
    users
    |> List.sort ~compare:(fun a b -> compare a.tz_offset b.tz_offset)
    |> List.iter ~f:(fun u ->
           u |> sexp_of_user |> Sexp.to_string_hum |> print_endline )


  let tz_display now { users; label; zone } =
    let label = Option.value label ~default:"-" in
    let names = List.map ~f:(fun u -> u.name) users in
    let names = String.concat ~sep:", " names in
    let time = Time.to_sec_string now ~zone in
    printf "%30s | %20s | %s\n" label time names


  let table_display users =
    let timezones = timezones users in
    let now = Time.now () in
    Map.iter timezones ~f:(tz_display now)


  let display refresh (token, storage) =
    Lwt_main.run
      (let%lwt () =
         if refresh then refresh_users token storage else Lwt.return_unit
       in
       let%lwt users = load_users storage in
       table_display users ;
       Lwt.return (`Ok ()))
end

open Cmdliner

let copts token storage = (token, storage)

(* Options common to all commands *)
let copts_t =
  let docs = Manpage.s_common_options in
  let token =
    let doc = "The Slack API access token" in
    Arg.(
      required & pos 0 (some string) None & info [] ~docv:"TOKEN" ~doc ~docs)
  in
  let storage =
    let doc = "The file to store users information" in
    Arg.(
      value
      & opt string "tzbot.sexp"
      & info [ "s"; "storage" ] ~docv:"STORAGE" ~doc ~docs)
  in
  Term.(const copts $ token $ storage)


let refresh =
  let doc = "Refresh the timezone of users" in
  Arg.(value & flag & info [ "r"; "refresh" ] ~docv:"REFRESH" ~doc)


let http_server_cmd =
  let doc = "Http server providing timezones as json" in
  let info = Term.info "server" ~doc in
  let port =
    Arg.(value & opt int 8000 & info [ "p"; "port" ] ~docv:"PORT" ~doc)
  in
  (Term.(const Http.start $ port $ copts_t), info)


let text_table_cmd =
  let doc = "Display the timezones of slack users in a table" in
  let info = Term.info "table" ~doc in
  (Term.(const Text.display $ refresh $ copts_t), info)


let default_cmd =
  let doc = "A simple tool to get the timezones in a slack group" in
  let exits = Term.default_exits in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ copts_t))
  , Term.info "tzbot" ~doc ~exits )


let () =
  Term.(exit @@ eval_choice default_cmd [ text_table_cmd; http_server_cmd ])
