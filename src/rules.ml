(* Rules contain selectors and actions to carry out *)
open Pcre;;
open Ini;;
type action_result =
  | Success
  | Error
type selector_result =
  | Substr of string array
  | Match of bool
let empty_selector _ = Match false
let empty_action _ = Success
let rx_selector rx p =
  Substr(Pcre.exec ~rex: rx p
         |> Pcre.get_substrings)

  (* Substr (Pcre.get_substrings (Pcre.exec ~rex: rx p)) *)
let movement_action folder p = Success
let delete_action p = Success

module Rule = struct
  type t = {
    path : string ;
    selector : (string -> selector_result);
    action : (string -> action_result);
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
    | ("move_to", String folder)::xs -> rule_of_ini path {base_rule with action = (movement_action folder)} xs
    | ("delete", Bool true)::xs -> rule_of_ini path {base_rule with action = delete_action} xs
    | ("watch", Bool true)::xs -> rule_of_ini path {base_rule with watch = true} xs
    | ("poll", Int poll)::xs -> rule_of_ini path {base_rule with poll = Some poll} xs
    | kv::xs -> Result.Error kv
    | [] -> Result.Ok {base_rule with path = path}
end
