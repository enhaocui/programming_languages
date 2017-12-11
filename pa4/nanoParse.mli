type token =
  | Num of (int)
  | Id of (string)
  | EOF
  | LET
  | EQ
  | IN
  | FUN
  | PLUS
  | MUL
  | AND
  | OR
  | LPAREN
  | RPAREN
  | SEMI
  | COLONCOLON
  | REC
  | ARROW
  | IF
  | THEN
  | ELSE
  | TRUE
  | FALSE
  | DIV
  | MINUS
  | LT
  | LE
  | NE
  | LBRAC
  | RBRAC

val exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Nano.expr
