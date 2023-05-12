(* {{{ COPYING *(

This file is part of tzbot, a program to list timezones in a slack group

Copyright (C) 2019  Louis Roché <louis(_)louisroche.net>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

)* }}} *)

open Core
open Printf
open Sexplib
open Conv

module Tz = struct
  module T = struct
    type t = string option [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)
end

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
;;

type timezone =
  { users : users
  ; label : string option
  ; zone : Time.Zone.t
  ; offset : int
  }

let timezone ~key:tz_offset ~data:users =
  let hours = tz_offset / (60 * 60) in
  let zone = Time.Zone.of_utc_offset ~hours in
  let tz_label = List.find_map users ~f:(fun user -> user.tz_label) in
  let tz = List.find_map users ~f:(fun user -> user.tz) in
  let label = Option.first_some tz_label tz in
  { users; zone; label; offset = tz_offset }
;;

let timezones users =
  let tz_offset_map = Map.empty (module Int) in
  let tz_offset_map =
    List.fold_left users ~init:tz_offset_map ~f:(fun map u ->
      Map.add_multi map ~key:u.tz_offset ~data:u)
  in
  let tz_offset_map = Map.mapi tz_offset_map ~f:timezone in
  let tz_list = Map.to_alist tz_offset_map in
  List.map ~f:snd tz_list
;;

let city ~key:tz ~data:users =
  let tz_offset =
    let u = List.hd_exn users in
    u.tz_offset
  in
  let hours = tz_offset / (60 * 60) in
  let zone = Time.Zone.of_utc_offset ~hours in
  let tz_label = List.find_map users ~f:(fun user -> user.tz_label) in
  let label = Option.first_some tz tz_label in
  { users; zone; label; offset = tz_offset }
;;

let cities users =
  let tz_map = Map.empty (module Tz) in
  let tz_map =
    List.fold_left users ~init:tz_map ~f:(fun map u ->
      Map.add_multi map ~key:u.tz ~data:u)
  in
  let tz_map = Map.mapi tz_map ~f:city in
  let tz_list = Map.to_alist tz_map in
  List.map ~f:snd tz_list
;;

let filter_active (u : Slacko.user_obj) =
  not (u.is_bot || u.deleted || u.is_ultra_restricted)
;;

(** Download users from slack *)
let fetch_users session =
  match%lwt Slacko.users_list session with
  | `Success users ->
    let users = List.filter users ~f:filter_active in
    Lwt.return_ok users
  | #Slacko.parsed_auth_error as error -> Lwt.return_error error
;;

(** Save users on disk *)
let save_users users storage =
  let%lwt oc = Lwt_io.open_file ~mode:Lwt_io.Output storage in
  let%lwt () =
    Lwt_io.write_line oc (users |> sexp_of_users |> Sexp.to_string)
  in
  let%lwt () = Lwt_io.close oc in
  Lwt.return_unit
;;

(** Download users from slack and update storage *)
let refresh_users token storage =
  let session = Slacko.start_session token in
  print_endline "connection to slack";
  match%lwt fetch_users session with
  | Error e ->
    let error msg =
      Lwt.fail_with (sprintf "unable to fetch the users: %s" msg)
    in
    (match e with
     | `Account_inactive -> error "account inactive"
     | `Not_authed -> error "not authed"
     | `Unknown_error -> error "unknown error :("
     | `Invalid_auth -> error "invalid auth"
     | `ParseFailure e -> error @@ sprintf "parse failure: %s" e
     | `Unhandled_error e -> error @@ sprintf "unhandled error %s" e)
  | Ok users ->
    printf "the new list contains %d users\n" (List.length users);
    let users = List.map users ~f:user_of_slack in
    save_users users storage
;;

(** Read users from storage *)
let load_users storage =
  let%lwt ic = Lwt_io.open_file ~mode:Lwt_io.Input storage in
  let%lwt content = Lwt_io.read ic in
  let%lwt () = Lwt_io.close ic in
  content |> Sexp.of_string |> users_of_sexp |> Lwt.return
;;

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
  ;;

  let refresh token storage =
    try%lwt
      let%lwt () = refresh_users token storage in
      Server.respond_string ~status:`OK ~body:"done" ()
    with
    | exn ->
      let body = sprintf "unable to refresh users:\n%s" (Exn.to_string exn) in
      Server.respond_string ~status:`Internal_server_error ~body ()
  ;;

  let tz storage =
    let%lwt users = load_users storage in
    let timezones = timezones users in
    let timezones = List.map timezones ~f:timezone in
    Lwt.return timezones
  ;;

  let start port (token, storage) =
    let callback _conn req _body =
      let uri = Request.uri req in
      let headers_json =
        Cohttp.Header.init_with "Content-Type" "application/json"
      in
      match Uri.path uri with
      | "/version" -> Server.respond_string ~status:`OK ~body:"%%VERSION%%" ()
      | "/refresh" -> refresh token storage
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
      | _ -> Server.respond_string ~status:`Not_found ~body:"path not found" ()
    in
    Lwt_main.run
      (let%lwt () = Lwt_io.printlf "starting http server on port %d" port in
       let%lwt () =
         Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())
       in
       Lwt.return ())
  ;;
end

module Text = struct
  let sexp_display users =
    users
    |> List.sort ~compare:(fun a b -> Int.compare a.tz_offset b.tz_offset)
    |> List.map ~f:(fun u -> u |> sexp_of_user |> Sexp.to_string_hum)
    |> List.iter ~f:print_endline
  ;;

  let tz_display now { users; label; zone; offset = _ } =
    let label = Option.value label ~default:"-" in
    let names = List.map ~f:(fun u -> u.name) users in
    let names = String.concat ~sep:", " names in
    let time = Time.to_sec_string now ~zone in
    printf "%30s | %20s | %s\n" label time names
  ;;

  let table_display group_by users =
    let timezones =
      match group_by with
      | `Tz -> timezones users
      | `City -> cities users
    in
    let now = Time.now () in
    timezones
    |> List.sort ~compare:(fun a b -> Int.compare a.offset b.offset)
    |> List.iter ~f:(tz_display now)
  ;;

  let table refresh group_by (token, storage) =
    Lwt_main.run
      (let%lwt () =
         if refresh then refresh_users token storage else Lwt.return_unit
       in
       let%lwt users = load_users storage in
       table_display group_by users;
       Lwt.return ())
  ;;

  let sexp refresh (token, storage) =
    Lwt_main.run
      (let%lwt () =
         if refresh then refresh_users token storage else Lwt.return_unit
       in
       let%lwt users = load_users storage in
       sexp_display users;
       Lwt.return ())
  ;;
end

open Cmdliner

let copts token storage = token, storage

(* Options common to all commands *)
let copts_t =
  let docs = Manpage.s_common_options in
  let token =
    let doc = "The Slack API access token" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"TOKEN" ~doc ~docs)
  in
  let storage =
    let doc = "The file to store users information" in
    Arg.(
      value
      & opt string "tzbot.sexp"
      & info [ "s"; "storage" ] ~docv:"STORAGE" ~doc ~docs)
  in
  Term.(const copts $ token $ storage)
;;

let refresh =
  let doc = "Refresh the timezone of users" in
  Arg.(value & flag & info [ "r"; "refresh" ] ~docv:"REFRESH" ~doc)
;;

let group_by =
  let doc = "group by timezone or city" in
  let modes = Arg.(enum [ "tz", `Tz; "city", `City ]) in
  Arg.(value & opt modes `Tz & info [ "group-by" ] ~docv:"GROUP_BY" ~doc)
;;

let http_server_cmd =
  let doc = "Http server providing timezones as json" in
  let info = Cmd.info "server" ~doc in
  let port =
    Arg.(value & opt int 8000 & info [ "p"; "port" ] ~docv:"PORT" ~doc)
  in
  let term = Term.(const Http.start $ port $ copts_t) in
  Cmd.v info term
;;

let text_table_cmd =
  let doc = "Display the timezones of slack users in a table" in
  let info = Cmd.info "table" ~doc in
  let term = Term.(const Text.table $ refresh $ group_by $ copts_t) in
  Cmd.v info term
;;

let sexp_cmd =
  let doc = "Display the timezones of slack users as a sexp" in
  let info = Cmd.info "sexp" ~doc in
  let term = Term.(const Text.sexp $ refresh $ copts_t) in
  Cmd.v info term
;;

let () =
  let doc = "A simple tool to get the timezones in a slack group" in
  let info = Cmd.info "tzbot" ~doc in
  let default = Term.(ret (const (fun _ -> `Help (`Pager, None)) $ copts_t)) in
  let group =
    Cmd.group info ~default [ text_table_cmd; http_server_cmd; sexp_cmd ]
  in
  exit @@ Cmd.eval ~catch:true group
;;
