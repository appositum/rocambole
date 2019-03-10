type token =
  | LParen
  | RParen
  | Lambda
  | Dot
  | Var(char)
  | EOF

type term =
  | VarT(char)
  | LambdaT(char, term)
  | AppT(term, term)

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
    if (List.exists(x => x == c, alphabet)) {
      [Var(c), ...tokenize(rest)]
    } else {
      tokenize(rest)
    }
  }
}
