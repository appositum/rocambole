type token =
  | LParen
  | RParen
  | Lambda
  | Dot
  | Var(char)
  | EOF

let pretty_token = fun
  | LParen => '('
  | RParen => ')'
  | Lambda => '\\'
  | Dot => '.'
  | Var(c) => c
  | EOF => ' '

let token_to_string = fun
  | LParen => "LParen"
  | RParen => "RParen"
  | Lambda => "Lambda"
  | Dot => "Dot"
  | Var(c) => "Var(" ++ String.make(1, c) ++ ")"
  | EOF => "<EOF>"

type term =
  | TermVar(char)
  | TermLambda(char, term)
  | TermApp(term, term)

let explode = (str : string) => {
  let rec exp(i, xs) = {
    if (i < 0) { xs } else { exp(i-1, [str.[i], ...xs]) }
  }
  exp(String.length(str) - 1, [])
}

let implode = (xs : list(char)) => {
  xs
  |> List.map(String.make(1))
  |> String.concat("")
}

let alphabet = {
  let chars = explode("abcdefghijklmnopqrstuvwxyz")
  List.append(chars, List.map(Char.uppercase, chars))
}

let rec tokenize = (text : list(char)) => {
  switch(text) {
  | [] => [EOF]
  | ['(', ...rest] => [LParen, ...tokenize(rest)]
  | [')', ...rest] => [RParen, ...tokenize(rest)]
  | ['.', ...rest] => [Dot, ...tokenize(rest)]
  | ['\\', ...rest] => [Lambda, ...tokenize(rest)]
  | [c, ...rest] =>
    if (List.exists(x => x == c, alphabet))
      [Var(c), ...tokenize(rest)]
    else
      tokenize(rest)
  }
}

exception ParseError(string)

let rec parse_single = (tokens : list(token)) => {
  switch(tokens) {
  | [Var(name), ...rest] => (TermVar(name), rest)
  | [Lambda, Var(arg), Dot, ...xs] =>
    let (body, rest) = parse_single(xs);
    (TermLambda(arg, body), rest)
  | [LParen, ...rest] =>
    let (func, after_func) = parse_single(rest)
    let (value, after_value) = parse_single(after_func)

    switch(after_value) {
    | [RParen, ...rest] => (TermApp(func, value), rest)
    | _ => raise(ParseError("Expected ')'"))
    }

  | _ => raise(ParseError("Bad parse"))
  }
}

let parse = (tokens : list(token)) => {
  let (result, _) = parse_single(tokens)
  result
}
