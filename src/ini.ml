open Pcre;;
open Angstrom;;

let section_name = take_while1 (fun x -> x != ']')

let section = char '[' *> section_name <* char ']'

let key = take_while1 (fun x -> x != '=')

type value =
  | Int of int
  | String of string
  | Array of value list
  | Bool of bool
  | Time of int
  | Regexp of Pcre.regexp

let regexp_literal = take_till (fun x -> x == '`') >>| (fun r -> Regexp (regexp r))
let regexp_value = char '`' *> regexp_literal <* char '`'

let bool_value =
  ((string "true") *> (return (Bool true))) <|> ((string "false") *> (return (Bool false)))

let str_value = take_while (fun x -> x != '"') >>| (fun s -> String s)

let string_value = char '"' *> str_value <* char '"'

let digit = function
  | '0' ..  '9' -> true
  | _ -> false

let integer = (take_while1 digit) >>| (fun x -> int_of_string x)

let magnitude_of_char = function
  | 's' -> 1
  | 'm' -> 60
  | 'h' -> 3600
  | _ -> 0

let magnitude =
  choice [char 's'; char 'm'; char 'h'] >>| magnitude_of_char

let time_value = lift2 (fun count magnitude ->
    Time (count * magnitude)) integer magnitude

let negate = ((char '-') *> (return (-))) <|> return (+)

let num_value = lift2 (fun negate digit ->
    Int (negate 0 digit)) negate integer

let empty_space =
  many (choice [char ' '; char '\n'; char '\t'])

let whitespace p =
  empty_space *> p <* empty_space

let array_of p = char '[' *> sep_by (whitespace (char ',')) p <* char ']'
                   >>| (fun l -> Array l)

let value = fix (fun value ->
    let arr = array_of value in
    choice [string_value; time_value; num_value; regexp_value; bool_value;  arr])

let key = take_while1 (fun x -> x != ' ' && x != '=' && x != '[' && x != ']' )

let kv_pair = lift2 (fun key value -> (key, value)) (key <* (whitespace (char '='))) value

let document =
  sep_by empty_space (lift2 (fun section kv_pairs ->
      (section,
        kv_pairs)) (section <* empty_space) (sep_by empty_space kv_pair))

let parse_ini string =
  match parse_only document (`String string) with
  | Result.Ok v -> v
  | Result.Error e -> failwith e

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let parse_ini_file file =
  let file = load_file file in
  parse_ini file
