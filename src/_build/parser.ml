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
  | INT
  | NUMLITERAL of (int)
  | ID of (string)
  | ADDRESSLIT of (string)
  | END of (string)
  | UNIT of (string)
  | BooLit of (bool)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast
# 54 "parser.ml"
let yytransl_const = [|
  257 (* SIGNATURE *);
  258 (* UINTTYPE *);
  259 (* STROAGE *);
  260 (* EVENT *);
  261 (* OF *);
  262 (* METHOD *);
  263 (* CONSTRUCTOR *);
  264 (* ENVIRONMENT *);
  265 (* GUARD *);
  266 (* EFFECTS *);
  267 (* LOGS *);
  268 (* RETURNS *);
  269 (* MAP *);
  270 (* UINTType *);
  271 (* ASSIGN *);
  272 (* ARROW *);
  273 (* MAPASSIGN *);
  274 (* COLON *);
  275 (* SEMI *);
  276 (* PASSIGN *);
  277 (* COMMA *);
  278 (* POINT *);
  279 (* LBRACE *);
  280 (* RBRACE *);
  281 (* LPAREN *);
  282 (* RPAREN *);
  283 (* LBRACK *);
  284 (* RBRACK *);
  285 (* EQ *);
  286 (* NEQ *);
  287 (* LGT *);
  288 (* ADD *);
  289 (* SUB *);
  290 (* MUL *);
  291 (* DIVIDE *);
  292 (* AND *);
  293 (* OR *);
  294 (* BOOL *);
  295 (* INT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  296 (* NUMLITERAL *);
  297 (* ID *);
  298 (* ADDRESSLIT *);
  299 (* END *);
  300 (* UNIT *);
  301 (* BooLit *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\005\000\005\000\007\000\
\006\000\008\000\003\000\010\000\010\000\010\000\011\000\011\000\
\009\000\009\000\012\000\012\000\012\000\012\000\012\000\004\000\
\013\000\013\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\000\000\001\000\003\000\001\000\
\003\000\001\000\005\000\000\000\001\000\003\000\001\000\001\000\
\000\000\002\000\005\000\007\000\009\000\007\000\007\000\012\000\
\000\000\002\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\028\000\000\000\000\000\000\000\
\010\000\000\000\000\000\001\000\003\000\004\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\011\000\
\018\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\008\000\009\000\000\000\007\000\016\000\015\000\000\000\
\000\000\000\000\000\000\000\000\000\000\019\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\026\000\000\000\000\000\023\000\022\000\
\020\000\000\000\000\000\000\000\000\000\027\000\000\000\021\000\
\014\000\024\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\008\000\025\000\026\000\043\000\059\000\
\022\000\069\000\070\000\023\000\060\000\061\000"

let yysindex = "\015\000\
\002\255\000\000\233\254\233\254\000\000\019\000\002\255\002\255\
\000\000\253\254\255\254\000\000\000\000\000\000\004\255\240\254\
\241\254\242\254\243\254\244\254\245\254\007\255\004\255\015\255\
\008\255\014\255\018\255\022\255\020\255\021\255\023\255\000\000\
\000\000\001\255\019\255\240\254\226\254\003\255\226\254\226\254\
\226\254\000\000\000\000\040\255\000\000\000\000\000\000\026\255\
\041\255\031\255\032\255\033\255\233\254\000\000\024\255\226\254\
\226\254\226\254\034\255\039\255\233\254\226\254\036\255\037\255\
\038\255\233\254\226\254\000\000\027\255\043\255\000\000\000\000\
\000\000\042\255\046\255\047\255\226\254\000\000\028\255\000\000\
\000\000\000\000"

let yyrindex = "\000\000\
\058\000\000\000\000\000\000\000\000\000\000\000\058\000\058\000\
\000\000\000\000\000\000\000\000\000\000\000\000\035\255\044\255\
\000\000\000\000\000\000\000\000\000\000\000\000\035\255\000\000\
\000\000\045\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\044\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\048\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\048\255\049\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\050\255\000\000\000\000\
\000\000\000\000\000\000\000\000\049\255\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\006\000\000\000\000\000\026\000\000\000\000\000\253\255\
\044\000\247\255\221\255\000\000\008\000\000\000"

let yytablesize = 76
let yytable = "\010\000\
\011\000\048\000\003\000\050\000\051\000\052\000\017\000\018\000\
\004\000\019\000\020\000\046\000\013\000\014\000\047\000\001\000\
\021\000\009\000\012\000\015\000\063\000\064\000\065\000\016\000\
\024\000\027\000\028\000\029\000\030\000\031\000\032\000\075\000\
\034\000\035\000\036\000\037\000\038\000\039\000\040\000\042\000\
\041\000\044\000\053\000\049\000\054\000\055\000\056\000\057\000\
\062\000\058\000\067\000\082\000\076\000\066\000\071\000\072\000\
\073\000\002\000\017\000\025\000\078\000\045\000\074\000\077\000\
\079\000\080\000\033\000\081\000\068\000\005\000\006\000\000\000\
\000\000\000\000\012\000\013\000"

let yycheck = "\003\000\
\004\000\037\000\001\001\039\000\040\000\041\000\003\001\004\001\
\007\001\006\001\007\001\042\001\007\000\008\000\045\001\001\000\
\013\001\041\001\000\000\023\001\056\000\057\000\058\000\025\001\
\041\001\041\001\041\001\041\001\041\001\041\001\024\001\067\000\
\018\001\026\001\021\001\018\001\015\001\018\001\018\001\039\001\
\018\001\023\001\003\001\041\001\019\001\005\001\016\001\016\001\
\025\001\017\001\012\001\024\001\026\001\020\001\019\001\019\001\
\019\001\000\000\024\001\012\001\019\001\036\000\066\000\021\001\
\019\001\019\001\023\000\077\000\061\000\026\001\026\001\255\255\
\255\255\255\255\026\001\026\001"

let yynames_const = "\
  SIGNATURE\000\
  UINTTYPE\000\
  STROAGE\000\
  EVENT\000\
  OF\000\
  METHOD\000\
  CONSTRUCTOR\000\
  ENVIRONMENT\000\
  GUARD\000\
  EFFECTS\000\
  LOGS\000\
  RETURNS\000\
  MAP\000\
  UINTType\000\
  ASSIGN\000\
  ARROW\000\
  MAPASSIGN\000\
  COLON\000\
  SEMI\000\
  PASSIGN\000\
  COMMA\000\
  POINT\000\
  LBRACE\000\
  RBRACE\000\
  LPAREN\000\
  RPAREN\000\
  LBRACK\000\
  RBRACK\000\
  EQ\000\
  NEQ\000\
  LGT\000\
  ADD\000\
  SUB\000\
  MUL\000\
  DIVIDE\000\
  AND\000\
  OR\000\
  BOOL\000\
  INT\000\
  EOF\000\
  "

let yynames_block = "\
  NUMLITERAL\000\
  ID\000\
  ADDRESSLIT\000\
  END\000\
  UNIT\000\
  BooLit\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'defs) in
    Obj.repr(
# 22 "parser.mly"
           (_1)
# 250 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 26 "parser.mly"
  (( [], [] ))
# 256 "parser.ml"
               : 'defs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'interfacedecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'defs) in
    Obj.repr(
# 28 "parser.mly"
                      ( ((_1 :: fst _2), snd _2) )
# 264 "parser.ml"
               : 'defs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'constructordecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'defs) in
    Obj.repr(
# 29 "parser.mly"
                        ( (fst _2, (_1 :: snd _2)) )
# 272 "parser.ml"
               : 'defs))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "parser.mly"
              ( [] )
# 278 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 36 "parser.mly"
         ( [_1] )
# 285 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 37 "parser.mly"
                            ( _1 :: _3 )
# 293 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
          ( Int   )
# 299 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 45 "parser.mly"
               ( Var(_1, _3) )
# 307 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 48 "parser.mly"
      (Id(_1))
# 314 "parser.ml"
               : 'id_ok))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'id_ok) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'interfaceBody_list) in
    Obj.repr(
# 52 "parser.mly"
 (
		{
			signaturename = _2;
			interfacebody =  _4
		}
	)
# 327 "parser.ml"
               : 'interfacedecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
              ( [] )
# 333 "parser.ml"
               : 'builtintypenames))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'builtintypename) in
    Obj.repr(
# 62 "parser.mly"
                   ([_1])
# 340 "parser.ml"
               : 'builtintypenames))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'builtintypename) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'builtintypenames) in
    Obj.repr(
# 63 "parser.mly"
                                          ( _1 :: _3 )
# 348 "parser.ml"
               : 'builtintypenames))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 66 "parser.mly"
           ( Bool(_1) )
# 355 "parser.ml"
               : 'builtintypename))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "parser.mly"
              (Address(_1))
# 362 "parser.ml"
               : 'builtintypename))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
  ( [] )
# 368 "parser.ml"
               : 'interfaceBody_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'interfaceBody) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'interfaceBody_list) in
    Obj.repr(
# 72 "parser.mly"
                                   ( _1::_2 )
# 376 "parser.ml"
               : 'interfaceBody_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'builtintypename) in
    Obj.repr(
# 76 "parser.mly"
                                         (TypeAssign (_2, _4))
# 384 "parser.ml"
               : 'interfaceBody))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'builtintypename) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'builtintypename) in
    Obj.repr(
# 77 "parser.mly"
                                                              (MapAssign (_2, _4, _6))
# 393 "parser.ml"
               : 'interfaceBody))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'builtintypenames) in
    Obj.repr(
# 78 "parser.mly"
                                                             (Event (_2, _7))
# 402 "parser.ml"
               : 'interfaceBody))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'builtintypename) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'builtintypename) in
    Obj.repr(
# 79 "parser.mly"
                                                                  (Constructorexpr (_2, _4, _6))
# 411 "parser.ml"
               : 'interfaceBody))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'builtintypename) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'builtintypename) in
    Obj.repr(
# 80 "parser.mly"
                                                             (Methodexpr (_2, _4, _6))
# 420 "parser.ml"
               : 'interfaceBody))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 10 : 'id_ok) in
    let _4 = (Parsing.peek_val __caml_parser_env 8 : 'param_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 4 : 'constructor_bodylist) in
    let _10 = (Parsing.peek_val __caml_parser_env 2 : 'builtintypename) in
    Obj.repr(
# 85 "parser.mly"
 (
		{
			name = _2;
			params = _4;
			consturctor_body = _8;
			return_type = _10;
		}
	)
# 437 "parser.ml"
               : 'constructordecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
  ( [] )
# 443 "parser.ml"
               : 'constructor_bodylist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'constructor_body) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'constructor_bodylist) in
    Obj.repr(
# 95 "parser.mly"
                                        ( _1::_2 )
# 451 "parser.ml"
               : 'constructor_bodylist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'id_ok) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'id_ok) in
    Obj.repr(
# 98 "parser.mly"
                            (PointAssign(_1, _3))
# 459 "parser.ml"
               : 'constructor_body))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
