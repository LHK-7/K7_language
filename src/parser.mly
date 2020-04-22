%{ open Ast
%}

%token SIGNATURE UINTTYPE STROAGE EVENT OF METHOD CONSTRUCTOR ENVIRONMENT GUARD EFFECTS LOGS RETURNS MAP UINTType STORAGE
%token ASSIGN ARROW MAPASSIGN ASSIGN COLON SEMI PASSIGN COMMA POINT
%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK 
%token EQ NEQ LGT ADD SUB MUL DIVIDE AND OR BOOL
%token INT
%token <int> NUMLITERAL 
%token <string> ID ADDRESSLIT END
%token <string> UNIT 
%token <bool> BooLit
%token EOF

%start program
%type <Ast.program> program

%%

program:
	 defs EOF {$1}

/* defs:
	interfacedecls defs {$1 :: $2, $2, $2}
	| constructordecl  { $1 }
	| methoddecls defs  {$1 :: $2} */


/* | interfacedecl defs{$1 :: $2} */
defs: 
   /* nothing */ 
	 { ([], [] )} 
	| interfacedecl defs { (($1 :: fst $2), snd $2) }
	| implementationdecl defs { (fst $2, ($1 :: snd $2)) }

implementationdecl:
	constructordecl methoddecls
	{
		{
			consturctor = $1;
			methods =  $2
		}
	}
/* (owner : Address, spender : Address) */
param_list:
  /*nothing*/ { [] }
	| param { [$1] }
  | param COMMA param_list  { $1 :: $3 }


arg_list:
  /*nothing*/ { [] }
	| argument { [$1] }
  | argument COMMA arg_list  { $1 :: $3 }

argument:
	| ID {Id($1)}

typ:
    INT   { Int   }


param:
  ID COLON typ { Var($1, $3) }

id_ok:
	| ID {Id($1)}

interfacedecl:
	SIGNATURE id_ok LBRACE interfaceBody_list RBRACE
	{
		{
			signaturename = $2;
			interfacebody =  $4
		}
	}


builtintypenames:
  /*nothing*/ { [] }
	|	builtintypename {[$1]}
	| builtintypename COMMA builtintypenames { $1 :: $3 }

builtintypename:
  | BooLit { Bool($1) }
	| ADDRESSLIT {Address($1)}
	/* | UINTTYPE { Int } */

interfaceBody_list:
		{ [] }
	|interfaceBody interfaceBody_list { $1::$2 }

/* TODO builtintypenames !!  */
interfaceBody:
	| STORAGE ID COLON builtintypename SEMI {TypeAssign ($2, $4)}
	| MAP ID COLON builtintypename MAPASSIGN builtintypename SEMI{MapAssign ($2, $4, $6)}
	| EVENT ID ASSIGN ID OF LPAREN builtintypenames RPAREN SEMI {Event ($2, $7)}
	| CONSTRUCTOR ID COLON builtintypename ARROW builtintypename SEMI{Constructorexpr ($2, $4, $6)}
	| METHOD ID COLON builtintypename ARROW builtintypename SEMI{Methodexpr ($2, $4, $6)} 


constructordecl:
	CONSTRUCTOR id_ok LPAREN param_list RPAREN LBRACE STORAGE constructor_bodylist RETURNS builtintypename SEMI RBRACE
	{
		{
			name = $2;
			params = $4;
			consturctor_body = $8;
			return_type = $10;
		}
	}
constructor_bodylist:
		{ [] }
	|constructor_body constructor_bodylist { $1::$2 }

constructor_body:
	| id_ok PASSIGN id_ok SEMI {PointAssign($1, $3)}

methoddecls:
		{ [] }
	|	methoddecl methoddecls {$1 :: $2 }

methoddecl:
	METHOD id_ok LPAREN param_list RPAREN LBRACE 
	GUARD LBRACE guard_bodylist RBRACE 
	STORAGE LBRACE storage_bodylist RBRACE
	EFFECTS LBRACE effects_bodylist RBRACE
	RETURNS builtintypename SEMI RBRACE
	{
		{
			methodname = $2;
			params = $4;
			guard_body = $9;
			storage_body = $13;
			effects_body = $17;
			returns = $20;
		}
	}

guard_bodylist:
		{ [] }
	|guard_body guard_bodylist { $1::$2 }


		
guard_body:
	/* | id_ok POINT id_ok EQ NUMLITERAL SEMI {}  the comment is for new expr for address*/
	/* | id_ok LBRACK id_ok RBRACK LGT id_ok SEMI { Binop() } */
		 | id_ok LGT NUMLITERAL SEMI { Binop($1, LGT, NumLit($3)) }

storage_bodylist:
		{ [] }
	|storage_body storage_bodylist { $1::$2 }

storage_body:
	| id_ok PASSIGN id_ok SEMI {PointAssign($1, $3)}

effects_bodylist:
		{ [] }
	|effects_body effects_bodylist { $1::$2 }

effects_body:
	| LOGS id_ok LPAREN arg_list RPAREN SEMI { Logexpr($2, $4) }