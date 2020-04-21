type token =
  | SIGNATURE
  | UINTTYPE
  | STROAGE
  | EVENT
  | OF
  | METHOD
  | CONSTRUCTOR
  | ENVIRONMENT
  | GUARD
  | EFFECTS
  | LOGS
  | RETURNS
  | MAP
  | UINTType
  | ASSIGN
  | ARROW
  | MAPASSIGN
  | COLON
  | SEMI
  | PASSIGN
  | COMMA
  | POINT
  | LBRACE
  | RBRACE
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | EQ
  | NEQ
  | LGT
  | ADD
  | SUB
  | MUL
  | DIVIDE
  | AND
  | OR
  | BOOL
  | NUMLITERAL of (int)
  | ID of (string)
  | ADDRESSLIT of (string)
  | END of (string)
  | UNIT of (string)
  | BooLit of (bool)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
