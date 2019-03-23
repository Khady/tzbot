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
[@@deriving sexp]

type users = user list [@@deriving sexp]

let user_of_slack (u : Slacko.user_obj) =
  { name = u.name; tz = u.tz; tz_offset = u.tz_offset; tz_label = u.tz_label }


(** Download users from slack *)
let fetch_users session =
  match%lwt Slacko.users_list session with
  | `Success users ->
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


let sexp_display users =
  users
  |> List.sort ~compare:(fun a b -> compare a.tz_offset b.tz_offset)
  |> List.iter ~f:(fun u ->
         u |> sexp_of_user |> Sexp.to_string_hum |> print_endline )


let tz_display now users =
  let names = List.map ~f:(fun u -> u.name) users in
  let names = String.concat ~sep:", " names in
  let { tz_offset; _ } = List.hd_exn users in
  let tz_label = List.find_map users ~f:(fun user -> user.tz_label) in
  let tz = List.find_map users ~f:(fun user -> user.tz) in
  let hours = tz_offset / (60 * 60) in
  let zone = Time.Zone.of_utc_offset ~hours in
  let time = Time.to_string_trimmed now ~zone in
  let label =
    match (tz_label, tz) with
    | Some label, _ | None, Some label ->
        label
    | None, None ->
        "-"
  in
  printf "%30s | %30s | %s\n" label time names


let table_display users =
  let tz_offset_map = Core_kernel.Map.empty (module Int) in
  let tz_offset_map =
    List.fold_left users ~init:tz_offset_map ~f:(fun map u ->
        Map.add_multi map ~key:u.tz_offset ~data:u )
  in
  let now = Time.now () in
  Map.iter tz_offset_map ~f:(tz_display now)


let execute token refresh storage =
  Lwt_main.run
  @@
  let storage =
    match storage with
    | None ->
        failwith "path to storage file must be provided"
    | Some storage ->
        storage
  in
  let%lwt () =
    if refresh then refresh_users token storage else Lwt.return_unit
  in
  let%lwt users = load_users storage in
  table_display users ;
  Lwt.return_unit


let token =
  let doc = "The Slack API access token" in
  Cmdliner.Arg.(
    required
    & opt (some string) None
    & info [ "t"; "token" ] ~docv:"TOKEN" ~doc)


let storage =
  let doc = "The file to store users information" in
  Cmdliner.Arg.(
    value
    & opt (some string) (Some "tzbot.sexp")
    & info [ "s"; "storage" ] ~docv:"STORAGE" ~doc)


let refresh =
  let doc = "Refresh the timezone of users" in
  Cmdliner.Arg.(value & flag & info [ "r"; "refresh" ] ~docv:"REFRESH" ~doc)


let info =
  let doc = "Posts messages to Slack" in
  Cmdliner.Term.info "tzbot" ~doc


let execute_t = Cmdliner.Term.(pure execute $ token $ refresh $ storage)

let () =
  match Cmdliner.Term.eval (execute_t, info) with
  | `Error _ ->
      exit 1
  | _ ->
      exit 0
