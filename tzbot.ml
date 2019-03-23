open Core_kernel
open Printf
open Sexplib
open Conv

type user = {
  name: string;
  display_name: string option;
  real_name: string option;
  tz: string option;
  tz_offset: int;
  tz_label: string option;
  is_restricted: bool;
  is_ultra_restricted: bool;
} [@@deriving sexp]

type users = user list [@@deriving sexp]

let convert_user (user: Slacko.user_obj) =
  let module U = Yojson.Safe.Util in
  let display_name =
    match
      user.profile
      |> U.member "display_name"
      |> U.to_string
    with
    | exception _ -> None
    | "" -> None
    | s -> Some s
  in
  {
    name = user.name;
    display_name;
    real_name = user.real_name;
    tz = user.tz;
    tz_offset = user.tz_offset;
    tz_label = user.tz_label;
    is_restricted = user.is_restricted;
    is_ultra_restricted = user.is_ultra_restricted;
  }

let fetch_users session =
  match%lwt Slacko.users_list session with
  | #Slacko.parsed_auth_error as error -> Lwt.return_error error
  | `Success users ->
    let valid_users =
      List.filter_map users
        ~f:(fun u ->
          match not u.deleted && not u.is_bot with
          | false -> None
          | true -> Some (convert_user u)
        )
    in
    Lwt.return_ok valid_users

let save_users users storage =
  let%lwt oc = Lwt_io.open_file ~mode:Lwt_io.Output storage in
  let%lwt () = Lwt_io.write oc (users |> sexp_of_users |> Sexp.to_string) in
  let%lwt () = Lwt_io.close oc in
  Lwt.return_unit

let refresh_users token storage =
  let session = Slacko.start_session token in
  print_endline "connection established";
  match%lwt fetch_users session with
  | Ok users ->
    printf "the new list contains %d users\n" (List.length users);
    save_users users storage
  | Error e ->
    let error msg =
      Lwt.fail_with (sprintf "unable to fetch the users: %s" msg)
    in
    match e with
    | `Account_inactive -> error "account inactive"
    | `Not_authed -> error "not authed"
    | `Unknown_error -> error "unknown error :("
    | `Invalid_auth -> error "invalid auth"
    | `ParseFailure e -> error @@ sprintf "parse failure: %s" e
    | `Unhandled_error e -> error @@ sprintf "unhandled error %s" e

let load_users storage =
  try%lwt
    let%lwt ic = Lwt_io.open_file ~mode:Lwt_io.Input storage in
    let%lwt content = Lwt_io.read ic in
    let%lwt () = Lwt_io.close ic in
    content |> Sexp.of_string |> users_of_sexp |> Lwt.return
  with _ ->
    Lwt.fail_with "unable to load users, try to relaunch with --refresh"

let sexp_display users =
  users
  |> List.sort ~compare:(fun a b -> compare a.tz_offset b.tz_offset)
  |> List.iter ~f:(fun u ->
    u
    |> sexp_of_user
    |> Sexp.to_string_hum
    |> print_endline
  )

let table_display users =
  let tz_offset_map = Map.empty (module Int) in
  let tz_offset_map =
    List.fold_left
      users
      ~init:tz_offset_map
      ~f:(fun map u -> Map.add_multi map ~key:u.tz_offset ~data:u)
  in
  let now = Time.now () in
  Map.iteri tz_offset_map
    ~f:(fun ~key:tz_offset ~data:users ->
      let names =
        List.map users
          ~f:(fun u ->
            match u.display_name with
            | Some s -> s
            | None ->
              match u.real_name with
              | Some s -> s
              | None -> u.name
          )
      in
      let names = String.concat ~sep:", " names in
      let hours = tz_offset / (60 * 60) in
      let zone = Time.Zone.of_utc_offset ~hours in
      let time = Time.to_string_trimmed now ~zone in
      printf "%s | %s\n" time names
    )

let execute token refresh storage =
  Lwt_main.run @@
  let storage =
    match storage with
    | None -> failwith "path to storage file must be provided"
    | Some storage -> storage
  in
  let%lwt () =
    if refresh then
      refresh_users token storage
    else
      Lwt.return_unit
  in
  let%lwt users = load_users storage in
  table_display users;
  Lwt.return_unit

let token =
  let doc = "The Slack API access token" in
  Cmdliner.Arg.(required & opt (some string) None & info ["t"; "token"] ~docv:"TOKEN" ~doc)

let storage =
  let doc = "The file to store users information" in
  Cmdliner.Arg.(value & opt (some string) (Some "tzbot.sexp") & info ["s"; "storage"] ~docv:"STORAGE" ~doc)

let refresh =
  let doc = "Refresh the timezone of users" in
  Cmdliner.Arg.(value & flag & info ["r"; "refresh"] ~docv:"REFRESH" ~doc)

let info =
  let doc = "Posts messages to Slack" in
  Cmdliner.Term.info "tzbot" ~doc

let execute_t = Cmdliner.Term.(pure execute $ token $ refresh $ storage)

let () =
  match Cmdliner.Term.eval (execute_t, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
