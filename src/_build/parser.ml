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
  | STORAGE
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
# 55 "parser.ml"
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
  271 (* STORAGE *);
  272 (* ASSIGN *);
  273 (* ARROW *);
  274 (* MAPASSIGN *);
  275 (* COLON *);
  276 (* SEMI *);
  277 (* PASSIGN *);
  278 (* COMMA *);
  279 (* POINT *);
  280 (* LBRACE *);
  281 (* RBRACE *);
  282 (* LPAREN *);
  283 (* RPAREN *);
  284 (* LBRACK *);
  285 (* RBRACK *);
  286 (* EQ *);
  287 (* NEQ *);
  288 (* LGT *);
  289 (* ADD *);
  290 (* SUB *);
  291 (* MUL *);
  292 (* DIVIDE *);
  293 (* AND *);
  294 (* OR *);
  295 (* BOOL *);
  296 (* INT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  297 (* NUMLITERAL *);
  298 (* ID *);
  299 (* ADDRESSLIT *);
  300 (* END *);
  301 (* UNIT *);
  302 (* BooLit *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\007\000\007\000\007\000\
\009\000\009\000\009\000\010\000\011\000\008\000\012\000\003\000\
\014\000\014\000\014\000\015\000\015\000\013\000\013\000\016\000\
\016\000\016\000\016\000\016\000\005\000\017\000\017\000\018\000\
\006\000\006\000\019\000\020\000\020\000\023\000\021\000\021\000\
\024\000\022\000\022\000\025\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\002\000\000\000\001\000\003\000\
\000\000\001\000\003\000\001\000\001\000\003\000\001\000\005\000\
\000\000\001\000\003\000\001\000\001\000\000\000\002\000\005\000\
\007\000\009\000\007\000\007\000\012\000\000\000\002\000\004\000\
\000\000\002\000\022\000\000\000\002\000\004\000\000\000\002\000\
\004\000\000\000\002\000\006\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\045\000\000\000\000\000\000\000\
\000\000\015\000\000\000\000\000\001\000\003\000\004\000\000\000\
\005\000\000\000\000\000\000\000\000\000\034\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\016\000\023\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\013\000\014\000\000\000\008\000\000\000\000\000\021\000\
\020\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\024\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\031\000\000\000\
\000\000\000\000\028\000\027\000\025\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\032\000\000\000\000\000\000\000\
\037\000\026\000\019\000\029\000\000\000\000\000\038\000\000\000\
\000\000\000\000\000\000\000\000\000\000\040\000\000\000\000\000\
\041\000\000\000\000\000\000\000\000\000\000\000\000\000\043\000\
\000\000\000\000\012\000\000\000\000\000\000\000\000\000\000\000\
\000\000\044\000\011\000\035\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\008\000\009\000\017\000\031\000\032\000\
\124\000\125\000\051\000\069\000\028\000\081\000\082\000\029\000\
\070\000\071\000\018\000\089\000\106\000\116\000\090\000\107\000\
\117\000"

let yysindex = "\008\000\
\013\255\000\000\214\254\214\254\000\000\018\000\013\255\013\255\
\015\255\000\000\005\255\002\255\000\000\000\000\000\000\214\254\
\000\000\015\255\004\255\244\254\006\255\000\000\245\254\247\254\
\248\254\249\254\251\254\014\255\004\255\019\255\016\255\018\255\
\244\254\025\255\023\255\026\255\027\255\028\255\000\000\000\000\
\008\255\020\255\244\254\022\255\009\255\226\254\226\254\226\254\
\226\254\000\000\000\000\035\255\000\000\029\255\047\255\000\000\
\000\000\037\255\038\255\039\255\036\255\214\254\049\255\033\255\
\226\254\226\254\226\254\000\000\040\255\048\255\214\254\041\255\
\226\254\042\255\043\255\044\255\214\254\226\254\000\000\214\254\
\045\255\046\255\000\000\000\000\000\000\050\255\051\255\034\255\
\052\255\214\254\053\255\226\254\000\000\056\255\054\255\059\255\
\000\000\000\000\000\000\000\000\055\255\058\255\000\000\214\254\
\057\255\060\255\214\254\214\254\073\255\000\000\064\255\062\255\
\000\000\076\255\214\254\063\255\076\255\065\255\078\255\000\000\
\066\255\226\254\000\000\067\255\070\255\077\255\079\255\066\255\
\068\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\067\000\000\000\000\000\000\000\000\000\000\000\067\000\067\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\071\255\074\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\071\255\000\000\000\000\075\255\
\074\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\074\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\086\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\086\255\000\000\
\082\255\000\000\000\000\000\000\000\000\000\000\000\000\080\255\
\000\000\083\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\080\255\000\000\082\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\014\255\
\000\000\000\000\014\255\000\000\000\000\000\000\000\000\000\000\
\000\000\087\255\000\000\000\000\087\255\000\000\000\000\000\000\
\084\255\000\000\000\000\000\000\088\255\000\000\000\000\084\255\
\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\019\000\000\000\000\000\000\000\051\000\235\255\000\000\
\228\255\000\000\000\000\255\255\075\000\021\000\214\255\000\000\
\045\000\000\000\000\000\027\000\011\000\002\000\000\000\000\000\
\000\000"

let yytablesize = 264
let yytable = "\010\000\
\033\000\011\000\012\000\058\000\059\000\060\000\061\000\023\000\
\001\000\024\000\025\000\044\000\056\000\003\000\021\000\057\000\
\026\000\013\000\027\000\004\000\016\000\053\000\074\000\075\000\
\076\000\014\000\015\000\020\000\019\000\030\000\034\000\033\000\
\035\000\036\000\037\000\087\000\038\000\041\000\039\000\043\000\
\045\000\046\000\042\000\052\000\047\000\048\000\049\000\050\000\
\054\000\062\000\055\000\064\000\063\000\065\000\066\000\068\000\
\067\000\072\000\073\000\078\000\077\000\083\000\084\000\085\000\
\080\000\095\000\002\000\092\000\022\000\093\000\094\000\091\000\
\098\000\102\000\103\000\086\000\096\000\108\000\088\000\126\000\
\100\000\104\000\112\000\113\000\109\000\114\000\115\000\119\000\
\088\000\122\000\121\000\128\000\132\000\127\000\101\000\022\000\
\129\000\030\000\130\000\131\000\006\000\007\000\105\000\040\000\
\036\000\105\000\111\000\123\000\017\000\018\000\009\000\042\000\
\099\000\118\000\010\000\079\000\097\000\110\000\120\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\033\000\000\000\000\000\000\000\000\000\000\000\033\000"

let yycheck = "\042\001\
\000\000\003\000\004\000\046\000\047\000\048\000\049\000\004\001\
\001\000\006\001\007\001\033\000\043\001\001\001\016\000\046\001\
\013\001\000\000\015\001\007\001\006\001\043\000\065\000\066\000\
\067\000\007\000\008\000\026\001\024\001\042\001\042\001\026\001\
\042\001\042\001\042\001\078\000\042\001\019\001\025\001\022\001\
\016\001\019\001\027\001\024\001\019\001\019\001\019\001\040\001\
\027\001\015\001\042\001\005\001\024\001\017\001\017\001\020\001\
\018\001\009\001\026\001\012\001\021\001\020\001\020\001\020\001\
\024\001\032\001\000\000\022\001\018\000\020\001\020\001\027\001\
\020\001\015\001\020\001\077\000\025\001\021\001\080\000\122\000\
\025\001\024\001\010\001\020\001\025\001\024\001\011\001\025\001\
\090\000\012\001\026\001\022\001\025\001\027\001\041\001\025\001\
\020\001\012\001\020\001\128\000\027\001\027\001\104\000\029\000\
\025\001\107\000\108\000\042\001\027\001\027\001\027\001\025\001\
\092\000\115\000\027\001\071\000\090\000\107\000\117\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\255\255\255\255\255\255\255\255\255\255\007\001"

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
  STORAGE\000\
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
# 21 "parser.mly"
           (_1)
# 325 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 32 "parser.mly"
  ( ([], [] ))
# 331 "parser.ml"
               : 'defs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'interfacedecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'defs) in
    Obj.repr(
# 33 "parser.mly"
                      ( ((_1 :: fst _2), snd _2) )
# 339 "parser.ml"
               : 'defs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'implementationdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'defs) in
    Obj.repr(
# 34 "parser.mly"
                           ( (fst _2, (_1 :: snd _2)) )
# 347 "parser.ml"
               : 'defs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'constructordecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'methoddecls) in
    Obj.repr(
# 38 "parser.mly"
 (
		{
			consturctor = _1;
			methods =  _2
		}
	)
# 360 "parser.ml"
               : 'implementationdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
              ( [] )
# 366 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 47 "parser.mly"
         ( [_1] )
# 373 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 48 "parser.mly"
                            ( _1 :: _3 )
# 381 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
              ( [] )
# 387 "parser.ml"
               : 'arg_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'argument) in
    Obj.repr(
# 53 "parser.mly"
            ( [_1] )
# 394 "parser.ml"
               : 'arg_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'argument) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arg_list) in
    Obj.repr(
# 54 "parser.mly"
                             ( _1 :: _3 )
# 402 "parser.ml"
               : 'arg_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
      (Id(_1))
# 409 "parser.ml"
               : 'argument))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
          ( Int   )
# 415 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'typ) in
    Obj.repr(
# 64 "parser.mly"
               ( Var(_1, _3) )
# 423 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "parser.mly"
      (Id(_1))
# 430 "parser.ml"
               : 'id_ok))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'id_ok) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'interfaceBody_list) in
    Obj.repr(
# 71 "parser.mly"
 (
		{
			signaturename = _2;
			interfacebody =  _4
		}
	)
# 443 "parser.ml"
               : 'interfacedecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
              ( [] )
# 449 "parser.ml"
               : 'builtintypenames))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'builtintypename) in
    Obj.repr(
# 81 "parser.mly"
                   ([_1])
# 456 "parser.ml"
               : 'builtintypenames))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'builtintypename) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'builtintypenames) in
    Obj.repr(
# 82 "parser.mly"
                                          ( _1 :: _3 )
# 464 "parser.ml"
               : 'builtintypenames))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 85 "parser.mly"
           ( Bool(_1) )
# 471 "parser.ml"
               : 'builtintypename))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "parser.mly"
              (Address(_1))
# 478 "parser.ml"
               : 'builtintypename))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
  ( [] )
# 484 "parser.ml"
               : 'interfaceBody_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'interfaceBody) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'interfaceBody_list) in
    Obj.repr(
# 91 "parser.mly"
                                   ( _1::_2 )
# 492 "parser.ml"
               : 'interfaceBody_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'builtintypename) in
    Obj.repr(
# 95 "parser.mly"
                                         (TypeAssign (_2, _4))
# 500 "parser.ml"
               : 'interfaceBody))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'builtintypename) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'builtintypename) in
    Obj.repr(
# 96 "parser.mly"
                                                              (MapAssign (_2, _4, _6))
# 509 "parser.ml"
               : 'interfaceBody))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'builtintypenames) in
    Obj.repr(
# 97 "parser.mly"
                                                             (Event (_2, _7))
# 518 "parser.ml"
               : 'interfaceBody))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'builtintypename) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'builtintypename) in
    Obj.repr(
# 98 "parser.mly"
                                                                  (Constructorexpr (_2, _4, _6))
# 527 "parser.ml"
               : 'interfaceBody))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'builtintypename) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'builtintypename) in
    Obj.repr(
# 99 "parser.mly"
                                                             (Methodexpr (_2, _4, _6))
# 536 "parser.ml"
               : 'interfaceBody))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 10 : 'id_ok) in
    let _4 = (Parsing.peek_val __caml_parser_env 8 : 'param_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 4 : 'constructor_bodylist) in
    let _10 = (Parsing.peek_val __caml_parser_env 2 : 'builtintypename) in
    Obj.repr(
# 104 "parser.mly"
 (
		{
			name = _2;
			params = _4;
			consturctor_body = _8;
			return_type = _10;
		}
	)
# 553 "parser.ml"
               : 'constructordecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
  ( [] )
# 559 "parser.ml"
               : 'constructor_bodylist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'constructor_body) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'constructor_bodylist) in
    Obj.repr(
# 114 "parser.mly"
                                        ( _1::_2 )
# 567 "parser.ml"
               : 'constructor_bodylist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'id_ok) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'id_ok) in
    Obj.repr(
# 117 "parser.mly"
                            (PointAssign(_1, _3))
# 575 "parser.ml"
               : 'constructor_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "parser.mly"
  ( [] )
# 581 "parser.ml"
               : 'methoddecls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'methoddecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'methoddecls) in
    Obj.repr(
# 121 "parser.mly"
                          (_1 :: _2 )
# 589 "parser.ml"
               : 'methoddecls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 20 : 'id_ok) in
    let _4 = (Parsing.peek_val __caml_parser_env 18 : 'param_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 13 : 'guard_bodylist) in
    let _13 = (Parsing.peek_val __caml_parser_env 9 : 'storage_bodylist) in
    let _17 = (Parsing.peek_val __caml_parser_env 5 : 'effects_bodylist) in
    let _20 = (Parsing.peek_val __caml_parser_env 2 : 'builtintypename) in
    Obj.repr(
# 129 "parser.mly"
 (
		{
			methodname = _2;
			params = _4;
			guard_body = _9;
			storage_body = _13;
			effects_body = _17;
			returns = _20;
		}
	)
# 610 "parser.ml"
               : 'methoddecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 141 "parser.mly"
  ( [] )
# 616 "parser.ml"
               : 'guard_bodylist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'guard_body) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'guard_bodylist) in
    Obj.repr(
# 142 "parser.mly"
                            ( _1::_2 )
# 624 "parser.ml"
               : 'guard_bodylist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'id_ok) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 149 "parser.mly"
                               ( Binop(_1, LGT, NumLit(_3)) )
# 632 "parser.ml"
               : 'guard_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 152 "parser.mly"
  ( [] )
# 638 "parser.ml"
               : 'storage_bodylist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'storage_body) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'storage_bodylist) in
    Obj.repr(
# 153 "parser.mly"
                                ( _1::_2 )
# 646 "parser.ml"
               : 'storage_bodylist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'id_ok) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'id_ok) in
    Obj.repr(
# 156 "parser.mly"
                            (PointAssign(_1, _3))
# 654 "parser.ml"
               : 'storage_body))
; (fun __caml_parser_env ->
    Obj.repr(
# 159 "parser.mly"
  ( [] )
# 660 "parser.ml"
               : 'effects_bodylist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'effects_body) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'effects_bodylist) in
    Obj.repr(
# 160 "parser.mly"
                                ( _1::_2 )
# 668 "parser.ml"
               : 'effects_bodylist))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'id_ok) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'arg_list) in
    Obj.repr(
# 163 "parser.mly"
                                          ( Logexpr(_2, _4) )
# 676 "parser.ml"
               : 'effects_body))
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
