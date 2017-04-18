open Pcre;;
open Fs;;
open Rules;;
open Ini;;
open Inotify;;
open Lwt;;
open Lwt_inotify;;
open Lwt_io;;
open Lwt_main;;
exception InvalidKV of string * Ini.value
let rec timer_thread rule poll path =
  Lwt_unix.sleep (float_of_int poll) >>= fun () ->
  Rule.execute_rule rule >>= fun result ->
  timer_thread rule poll path
let inotify_event_thread rule path events =
  Lwt_inotify.create () >>= fun inotify ->
  add_watch inotify path events >>= fun watch ->
  let rec _event_loop inotify =
    Lwt_inotify.read inotify >>= fun ev ->
    match ev with
    | (_, _, _, Some p) ->
      Rule.execute_rule rule >>= fun _ -> _event_loop inotify
    | _ -> Lwt.return (Success)  >>= fun _ -> _event_loop inotify in
  _event_loop inotify

let thread_from_rule rule =
  match rule with
  | {Rule.path = Some path; watch = true} ->
    inotify_event_thread rule path [S_Create]
  | {Rule.path = Some path; watch = false; poll = Some time} ->
    timer_thread rule time path
  | _ -> raise InvalidRule

let () =
  try
    let ini = Ini.parse_ini_file "./conf.ini" in
    let rules = List.map (fun (path, values) ->
        match Rule.rule_of_ini path Rule.empty_rule values with
        | Ok rule -> rule
        | Result.Error (s, v) -> raise (InvalidKV (s, v))
      ) ini in
  Lwt_main.run (
    rules
    |> List.map thread_from_rule
    |> Lwt.pick
  )
  with InvalidKV (section, _) ->
    print_endline ("Invalid section " ^ section)

;;
