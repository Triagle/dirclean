open Pcre;;
open Fs;;
open Rules;;
open Ini;;
open Inotify;;
open Lwt;;
open Lwt_inotify;;
open Lwt_io;;
open Lwt_main;;
exception InvalidRule
exception InvalidKV of string * Ini.value
let rec timer_thread t fn path selector =
  Lwt_unix.sleep (float_of_int t) >>= (fun () -> fn path selector; timer_thread t fn path selector)
let inotify_event_thread path events fn selector =
  Lwt_inotify.create () >>= fun inotify ->
  add_watch inotify path events >>= fun watch ->
  let rec _event_loop inotify =
    Lwt_inotify.read inotify >>= fun ev ->
    match ev with
    | (_, _, _, Some p) ->
      fn (Filename.concat path p) selector >>= fun _ -> _event_loop inotify
    | _ -> Lwt.return (Success)  >>= fun _ -> _event_loop inotify in
  _event_loop inotify

let thread_from_rule = function
  | {Rule.path = path; selector = selector; action = action; watch = true} ->
    inotify_event_thread path [S_Create] action selector
  | {Rule.path = path; selector = selector; action = action; watch = false; poll = Some time} ->
    timer_thread time action path selector
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
