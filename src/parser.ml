open Angstrom

type literal = LInt of int | LBool of bool

type ast =
  | Var of char
  | Lam of char * ast
  | Cls of char * ast * env
  | App of ast * ast
  | Lit of literal

and env = (char * ast) list

let lookup key xs =
  let f (key', value) acc =
    if key = key' then Some value else acc
  in List.fold_right f xs None

let is_space = function
  | ' '
  | '\t'
  | '\r'
  | '\n' -> true
  | _ -> false

let is_eol = function
  | '\r'
  | '\n' -> true
  | _ -> false

let space = satisfy is_space
let spaces = many space
let spaces1 = many1 space

let surroundedBy p sur = sur *> p <* sur

let tokenize p = surroundedBy p spaces
let tokenize1 p = surroundedBy p spaces1

let symbolic c = char c |> tokenize

let parens p = symbolic '(' *> p <* symbolic ')'

let identifier = satisfy (function 'a' .. 'z' -> true | _ -> false)

let integer =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string
