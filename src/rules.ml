(* Rules contain selectors and actions to carry out *)
open Lwt;;
open Pcre;;
open Ini;;
open Fs;;
type action_result =
  | Success
  | Error
type selector_result =
  | Substr of string array
  | Match of bool
let empty_selector _ = Match false
let empty_action _ _ = return Success
let rx_selector rx p =
  try
    Substr(Pcre.exec ~rex: rx p
           |> Pcre.get_substrings)
  with Not_found ->
    Match false
let file_movement_action folder p s =
  match s p with
  | Substr groups -> Lwt_io.printl ("Moving " ^ p ^ " to " ^ folder) >>= fun () -> (return Success)
  | Match true -> Lwt_io.printl ("Moving " ^ p ^ " to " ^ folder) >>= fun () -> (return Success)
  | _ -> Lwt_io.printl ("Not performing any action on" ^ p) >>= fun () -> (return Success)
let sweep_movement_action p s = Lwt_io.printl ("Sweeping " ^ folder) >>= fun () -> (return Success)
let delete_action p s = return Success

module Rule = struct
  type t = {
    path : string ;
    selector : (string -> selector_result);
    action : (string -> (string -> selector_result) -> action_result Lwt.t);
    watch : bool;
    poll : int option;
  }
  let empty_rule = {
    path = "";
    selector = empty_selector;
    action = empty_action;
    watch = false;
    poll = None
  }
  let rec rule_of_ini path base_rule = function
    | ("selector", Regexp rx)::xs ->  rule_of_ini path {base_rule with selector = (rx_selector rx)} xs
    | ("move_to", String folder)::xs -> rule_of_ini path {base_rule with action = sweep_movement_action} xs
    | ("delete", Bool true)::xs -> rule_of_ini path {base_rule with action = delete_action} xs
    | ("watch", Bool true)::xs -> rule_of_ini path {base_rule with watch = true;
                                                                   action = (file_movement_action path)} xs
    | ("poll", Time poll)::xs -> rule_of_ini path {base_rule with poll = Some poll} xs
    | kv::xs -> Result.Error kv
    | [] -> Result.Ok {base_rule with path = path}
end
