(* Rules contain selectors and actions to carry out *)
open Utils
open Selector
open Lwt
open Pcre
open Ini
open Fs

exception InvalidPath of string;;

(* Construct a destination path relative to the src path, and incorporating the match results of selector_result. *)
let construct_path src_path dest_path  =
  let src_dir = Filename.dirname src_path in
  Filename.concat src_dir dest_path

(* Move a matched file to the destination path. Creates directories to move to if need be.  *)
let file_move dest_path path =
  let dest_path = construct_path path dest_path  in
  Fs.create_path dest_path;
  Fs.move dest_path path

(* Scan src_path, looking for files that match selector. These files are then moved to the dest_path. *)
let execute_command cmd f =
  let cmd = Printf.sprintf "%s '%s'" cmd f
            |> Lwt_process.shell in
  Lwt_process.exec cmd >>= fun _ -> Lwt.return_unit

let is_some = function
  | Some _ -> true
  | None -> false
module Rule = struct

  type t = {
    path : string option;
    selector : Selector.selector option;
    action : (string -> unit Lwt.t) option;
    watch : bool;
    poll : int option;
  }

  exception InvalidRule of t

  let empty_rule = {
    path = None;
    action = None;
    selector = None;
    watch = false;
    poll = None
  }
  let rec rule_of_ini path base_rule = function
    | ("selector", Selector s)::xs -> rule_of_ini path {base_rule with selector = Some s} xs
    | ("move_to", String folder)::xs when Sys.is_directory path  -> rule_of_ini path {base_rule with action = Some (file_move folder)} xs
    | ("exec", String cmd)::xs -> rule_of_ini path {base_rule with action = Some (execute_command cmd)} xs
    | ("delete", Bool true)::xs -> rule_of_ini path {base_rule with action = Some Lwt_unix.unlink} xs
    | ("watch", Bool true)::xs -> rule_of_ini path {base_rule with watch = true} xs
    | ("poll", Time poll)::xs -> rule_of_ini path {base_rule with poll = Some poll} xs
    | kv::xs -> Result.Error kv
    | [] -> Result.Ok {base_rule with path = Some path}

  let execute_rule = function
    | {path = Some path; selector = Some selector; action = Some action} ->
      (match Fs.list_all path with
      | Ok files -> List.filter (fun f -> eval_selector f selector) files
                    |> Lwt_list.iter_p action
      | Error path -> raise (InvalidPath path))
    | rule -> raise (InvalidRule rule)

  let diagnose_rule = function
    | rule when rule.path == None -> Some "Rule must have path"
    | rule when rule.action == None -> Some "Rule must have action"
    | rule when rule.selector == None -> Some "Rule must have selector"
    | rule when rule.poll == None && rule.watch == false -> Some "Rule must specify polling time or watch flag"
    | _ -> None
end
