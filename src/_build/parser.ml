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

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast
# 53 "parser.ml"
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
    0 (* EOF *);
    0|]

let yytransl_block = [|
  295 (* NUMLITERAL *);
  296 (* ID *);
  297 (* ADDRESSLIT *);
  298 (* END *);
  299 (* UNIT *);
  300 (* BooLit *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\005\000\007\000\003\000\
\009\000\009\000\006\000\006\000\008\000\008\000\010\000\010\000\
\010\000\010\000\010\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\000\000\003\000\003\000\001\000\005\000\
\001\000\003\000\001\000\001\000\000\000\002\000\005\000\007\000\
\009\000\007\000\007\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\020\000\000\000\000\000\007\000\000\000\
\001\000\003\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\008\000\
\014\000\000\000\000\000\000\000\000\000\000\000\012\000\011\000\
\000\000\000\000\000\000\000\000\000\000\015\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\019\000\018\000\016\000\000\000\000\000\010\000\017\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\000\000\000\000\047\000\008\000\017\000\
\048\000\018\000"

let yysindex = "\004\000\
\000\255\000\000\228\254\000\000\013\000\000\255\000\000\251\254\
\000\000\000\000\004\255\235\254\236\254\237\254\238\254\239\254\
\001\255\004\255\006\255\011\255\009\255\010\255\012\255\000\000\
\000\000\221\254\245\254\221\254\221\254\221\254\000\000\000\000\
\013\255\026\255\017\255\018\255\019\255\000\000\014\255\221\254\
\221\254\221\254\221\254\016\255\021\255\022\255\023\255\020\255\
\000\000\000\000\000\000\221\254\024\255\000\000\000\000"

let yyrindex = "\000\000\
\037\000\000\000\000\000\000\000\000\000\037\000\000\000\000\000\
\000\000\000\000\025\255\000\000\000\000\000\000\000\000\000\000\
\000\000\025\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\027\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\032\000\000\000\000\000\000\000\230\255\000\000\024\000\
\249\255\000\000"

let yytablesize = 53
let yytable = "\033\000\
\003\000\035\000\036\000\037\000\001\000\031\000\012\000\013\000\
\032\000\014\000\015\000\007\000\009\000\044\000\045\000\046\000\
\016\000\011\000\019\000\020\000\021\000\022\000\023\000\026\000\
\024\000\027\000\028\000\029\000\034\000\030\000\039\000\038\000\
\040\000\041\000\049\000\042\000\002\000\010\000\043\000\050\000\
\051\000\025\000\055\000\052\000\054\000\053\000\000\000\000\000\
\013\000\000\000\000\000\000\000\009\000"

let yycheck = "\026\000\
\001\001\028\000\029\000\030\000\001\000\041\001\003\001\004\001\
\044\001\006\001\007\001\040\001\000\000\040\000\041\000\042\000\
\013\001\023\001\040\001\040\001\040\001\040\001\040\001\018\001\
\024\001\015\001\018\001\018\001\040\001\018\001\005\001\019\001\
\016\001\016\001\019\001\017\001\000\000\006\000\025\001\019\001\
\019\001\018\000\019\001\021\001\052\000\026\001\255\255\255\255\
\024\001\255\255\255\255\255\255\026\001"

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
# 227 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 25 "parser.mly"
  (( [] ))
# 233 "parser.ml"
               : 'defs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'interfacedecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'defs) in
    Obj.repr(
# 26 "parser.mly"
                     (_1 :: _2)
# 241 "parser.ml"
               : 'defs))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "parser.mly"
              ( [] )
# 247 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 35 "parser.mly"
                            ((_1 :: _3) )
# 255 "parser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'builtintypename) in
    Obj.repr(
# 38 "parser.mly"
                           ( Var(_1, _3) )
# 263 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 41 "parser.mly"
      (Id(_1))
# 270 "parser.ml"
               : 'id_ok))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'id_ok) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'interfaceBody_list) in
    Obj.repr(
# 45 "parser.mly"
 (
		{
			signaturename = _2;
			interfacebody =  _4
		}
	)
# 283 "parser.ml"
               : 'interfacedecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'builtintypename) in
    Obj.repr(
# 54 "parser.mly"
                  ([_1])
# 290 "parser.ml"
               : 'builtintypenames))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'builtintypename) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'builtintypenames) in
    Obj.repr(
# 55 "parser.mly"
                                          ( _1 :: _3 )
# 298 "parser.ml"
               : 'builtintypenames))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 58 "parser.mly"
           ( Bool(_1) )
# 305 "parser.ml"
               : 'builtintypename))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "parser.mly"
              (Address(_1))
# 312 "parser.ml"
               : 'builtintypename))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "parser.mly"
  ( [] )
# 318 "parser.ml"
               : 'interfaceBody_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'interfaceBody) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'interfaceBody_list) in
    Obj.repr(
# 64 "parser.mly"
                                   ( _1::_2 )
# 326 "parser.ml"
               : 'interfaceBody_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'builtintypename) in
    Obj.repr(
# 68 "parser.mly"
                                         (TypeAssign (_2, _4))
# 334 "parser.ml"
               : 'interfaceBody))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'builtintypename) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'builtintypename) in
    Obj.repr(
# 69 "parser.mly"
                                                              (MapAssign (_2, _4, _6))
# 343 "parser.ml"
               : 'interfaceBody))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'builtintypenames) in
    Obj.repr(
# 70 "parser.mly"
                                                             (Event (_2, _7))
# 352 "parser.ml"
               : 'interfaceBody))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'builtintypename) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'builtintypename) in
    Obj.repr(
# 71 "parser.mly"
                                                                  (Constructorexpr (_2, _4, _6))
# 361 "parser.ml"
               : 'interfaceBody))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'builtintypename) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'builtintypename) in
    Obj.repr(
# 72 "parser.mly"
                                                             (Methodexpr (_2, _4, _6))
# 370 "parser.ml"
               : 'interfaceBody))
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
