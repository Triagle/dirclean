open Pcre
open Fs
open Angstrom

type selector =
  | Expr of (string -> bool)
  | Or of selector * selector
  | And of selector * selector
  | Not of selector

let rec eval_selector p = function
  | Or (s1, s2) ->  (eval_selector p s1) || (eval_selector p s2)
  | And (s1, s2) -> (eval_selector p s1) && (eval_selector p s2)
  | Not s1 -> not (eval_selector p s1)
  | Expr e -> e p

let regexp_literal = take_till (fun x -> x == '`') >>| (fun r -> Expr (pmatch ~rex: (regexp r)))
let regexp_selector = char '`' *> regexp_literal <* char '`'

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

let time_value = lift2 (fun count magnitude -> (count * magnitude)) integer magnitude

let comparator = (char '>' >>| (fun _ -> (>))) <|> (char '<' >>| (fun _ -> (<)))

let time_selector = lift2 (fun cmp time -> Expr (fun p -> cmp (age_of p) time)) comparator time_value

let value = choice [regexp_selector; time_selector;]

let parens p =
  char '(' *> p <* char ')'

let expr = fix (fun expr ->
    let not_expr = lift2 (fun _ e -> Not e) (char '!') expr in
    let or_expr = parens (lift3 (fun e1 _ e2 -> Or (e1, e2)) expr (string " || ") expr) in
    let and_expr = parens (lift3 (fun e1 _ e2 -> And (e1, e2)) expr (string " && ") expr) in
    choice [value; not_expr; or_expr; and_expr])
