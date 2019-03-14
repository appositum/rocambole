type token =
  | LParen
  | RParen
  | Lambda
  | Dot
  | Var of char
  | EOF

let pretty_token = function
  | LParen -> '('
  | RParen -> ')'
  | Lambda -> '\\'
  | Dot -> '.'
  | Var c -> c
  | EOF -> ' '

let token_to_string = function
  | LParen -> "LParen"
  | RParen -> "RParen"
  | Lambda -> "Lambda"
  | Dot -> "Dot"
  | Var c -> "Var"
  | EOF -> "<EOF>"

type term =
  | TermVar of char
  | TermLambda of char * term
  | TermApp of term * term

let explode s = List.init (String.length s) (String.get s)

let is_alpha = function
  | 'a' .. 'z'
  | 'A' .. 'Z' -> true
  | _ -> false

let rec tokenize (text : char list) =
  match text with
  | [] -> [EOF]
  | '('::xs -> LParen :: tokenize xs
  | ')'::xs -> RParen :: tokenize xs
  | '.'::xs -> Dot :: tokenize xs
  | '\\'::xs -> Lambda :: tokenize xs
  | x::xs -> Var x :: tokenize xs
