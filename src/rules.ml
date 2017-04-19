(* Rules contain selectors and actions to carry out *)
open Utils
open Lwt
open Pcre
open Ini
open Fs

exception InvalidRule;;
exception InvalidPath of string;;

type action_result =
  | Success
  | Error

type selector_result =
  | Substr of string list
  | Match

(* Match everything *)
let empty_selector _ = Some Match

(* Create a selector that matches based on a regular expression *)
let rx_selector rx p =
  try
    Some (Substr(Pcre.exec ~rex: rx p
                 |> Pcre.get_substrings
                 |> Array.to_list
                 |> List.tl))
  with Not_found ->
    None

let age_selector age = function
  | p when (Fs.age_of p) > age && not (Sys.is_directory p) -> Some Match
  | _ -> None

(* Construct a destination path relative to the src path, and incorporating the match results of selector_result. *)
let construct_path src_path dest_path selector_result =
  let src_dir = Filename.dirname src_path in
  match selector_result with
  | Substr arr -> Utils.fmt_list (Filename.concat src_dir dest_path) arr
  | Match -> Filename.concat src_dir dest_path

(* Move a matched file to the destination path. Creates directories to move to if need be.  *)
let file_move dest_path (path, selector_result) =
  let dest_path = construct_path path dest_path selector_result in
  Fs.create_path dest_path;
  Fs.move dest_path path

(* Scan src_path, looking for files that match selector. These files are then moved to the dest_path. *)
let sweep_movement_action src_path dest_path selector =
  match Fs.list_all src_path with
  | Ok files -> Utils.filter_map (fun f ->
      match (selector (Filename.basename f)) with
      | Some mtch -> Some (f, mtch)
      | None -> None) files
                |> Lwt_list.iter_p (file_move dest_path)
  | Result.Error path -> raise (InvalidPath path)

(* Delete files in src_path matching selector. *)
let delete_action src_path selector =
  match Fs.list_all src_path with
  | Ok files -> List.filter (fun f -> selector f != None) files
                |> Lwt_list.iter_p Lwt_unix.unlink
  | Result.Error path -> raise (InvalidPath path)

let shell_action src_path cmd selector =
  match Fs.list_all src_path with
  | Ok files -> List.filter (fun f -> selector f != None) files
                |> List.map (fun f -> Lwt_process.shell (cmd ^ " '" ^ f ^ "'"))
                |> Lwt_list.iter_p (fun c -> Lwt_process.exec c >>= fun _ -> Lwt.return_unit)
  | Result.Error path -> raise (InvalidPath path)

module Rule = struct
  type t = {
    path : string option;
    selector : (string -> selector_result option);
    move_to : string option;
    watch : bool;
    delete : bool;
    exec : string option;
    poll : int option;
  }
  let empty_rule = {
    path = None;
    selector = empty_selector;
    move_to = None;
    delete = false;
    watch = false;
    exec = None;
    poll = None
  }
  let rec rule_of_ini path base_rule = function
    | ("selector", Regexp rx)::xs ->  rule_of_ini path {base_rule with selector = (rx_selector rx)} xs
    | ("selector", Time t)::xs -> rule_of_ini path {base_rule with selector = (age_selector t)} xs
    | ("move_to", String folder)::xs -> rule_of_ini path {base_rule with move_to = Some folder} xs
    | ("delete", Bool true)::xs -> rule_of_ini path {base_rule with delete = true} xs
    | ("watch", Bool true)::xs -> rule_of_ini path {base_rule with watch = true} xs
    | ("poll", Time poll)::xs -> rule_of_ini path {base_rule with poll = Some poll} xs
    | ("exec", String cmd)::xs -> rule_of_ini path {base_rule with exec = Some cmd} xs
    | kv::xs -> Result.Error kv
    | [] -> Result.Ok {base_rule with path = Some (Fs.expand_path path)}

  let execute_rule = function
    | {path = Some src_path; move_to = Some dest_path; selector = s} ->
      sweep_movement_action src_path dest_path s
    | {path = Some src_path; delete = true; selector = s} ->
      delete_action src_path s
    | {path = Some src_path; exec = Some cmd;  selector = s} ->
      shell_action src_path cmd s
    | _ -> raise InvalidRule
end
