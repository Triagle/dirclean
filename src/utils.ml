(* Utility functions *)
open Pcre;;
open Printf;;

let filter_map f l =
  List.fold_right (fun i acc ->
      match f i with
      | Some i -> i::acc
      | None -> acc) l []

(* Intersperse the elements of a and b
   e.g intersperse [1; 2; 3] [4; 5; 6] -> [1; 4; 2; 5; 3; 6]
   Leftovers are ignored. *)
let intersperse a b =
  let rec _intersperse acc = function
    | ([], l) | (l, []) -> List.append l acc
    | (x::xs, k::ks) -> _intersperse (k::x::acc) (xs, ks) in
  _intersperse [] (a, b)
  |> List.rev

let fmt_list fmt lst =
  let parsed_fmt = Pcre.split ~rex:(Pcre.regexp "{}") fmt in
  intersperse parsed_fmt lst
  |> String.concat ""
