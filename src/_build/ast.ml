type op = Add | Sub | Equal | Neq | Less | And | Or | LGT

type typ = 
	| Bool of bool
	| Int of int 
	| Uint of int 
	| Address of string
	| UNIT of unit
	| Mapstruct of string * string


(* Need change *)
type ocamlbuiltin = Int

type param = 
		Var of string * ocamlbuiltin 

type expr =
	| NumLit of int 
	| BooLit of bool
	| Id of string
	| StrLit of string
	(* | AddressLit of string list *)
	| TypeAssign of string * typ
	| MapAssign of string * typ * typ 
	(* | MapAssigns of string * typ list * typ  *)
	| PointAssign of expr * expr
	| Event of string * typ list
	| Binop of expr * op * expr
	| Constructorexpr of string * typ * typ 
	| Methodexpr of string * typ  * typ 
	| Logexpr of expr * expr list

(* control flow statement: if, while *)
type stmt =
		Block of stmt list
	|	Expr of expr
	| Return of expr

type consturctor_def ={
	name: expr;
	params: param list;
	consturctor_body: expr list;
	return_type: typ;
}

(* for guard body *)
(* type fv_def = ... *)

type method_def = {
	methodname: expr;
	params: param list;
	guard_body: expr list;
	storage_body: expr list;
	effects_body: expr list;
	returns: typ;
}

type interface_def = {
	signaturename: expr;
	interfacebody: expr list;
}

type implementation_def = {
	consturctor: consturctor_def;
	methods: method_def list;
}

(* type program = interface_def list * consturctor_def list *)
(* consturctor list is bad ! *)
type program = interface_def list * implementation_def list
(* type program = interface_def list * consturctor_def list * method_def list  *)

(* pretty printing *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | And -> "&&"
	| Or -> "||"
	| LGT -> ">"

let string_of_builtin = function
		Int -> "int"
	
let rec string_of_typ = function
		Bool(x) -> "bool" ^ string_of_bool x
	| Int(x) -> "int" ^ string_of_int x
	| Uint(x) -> "uint" ^ string_of_int x
	| Address(x) -> "address" ^ x
	| UNIT(x) -> "unit->void"
	| Mapstruct(x, y) -> x ^ "I am mapping struct " ^ y

let string_of_param = function
		Var(x, t) -> x ^ string_of_builtin t

let rec string_of_expr = function
		NumLit(x) -> string_of_int x
	| BooLit(x) -> string_of_bool x
	| Id(x) -> "I am ID" ^ x
	| StrLit(x) -> x
	| TypeAssign(x, y)-> "Type Assign: " ^ x  ^ string_of_typ y
	| MapAssign(x, t1, t2) -> "Map assign: " ^ x ^ (string_of_typ t1) ^ (string_of_typ t2)
	| PointAssign(x, e) -> string_of_expr x ^ (string_of_expr e)
	| Event(x, ty) -> x ^ String.concat "" (List.map string_of_typ ty) 
	| Binop(e1, op, e2) ->  (string_of_expr e1) ^ (string_of_op op) ^ (string_of_expr e2)
	| Constructorexpr(x, ty1, ty2) -> x ^ string_of_typ ty1 ^  string_of_typ ty2
	| Methodexpr(x, ty1, ty2) -> x ^ string_of_typ ty1 ^ string_of_typ ty2
	| Logexpr(e, el) -> string_of_expr e ^ String.concat " " (List.map string_of_expr el)

(* let string_of_expr = function
		NumLit(l) -> string_of_int l
	| BooLit(true) -> "true"
	| BooLit(false) -> "false"
	| Id(s) -> s *)

let string_of_interfacedef interfacedecl =
	string_of_expr interfacedecl.signaturename ^ " " ^
  String.concat "" (List.map string_of_expr interfacedecl.interfacebody)

let string_of_constructordef constructordecl = 
	string_of_expr constructordecl.name ^ " " ^  
	String.concat " " (List.map string_of_param constructordecl.params) ^ 
	String.concat "" (List.map string_of_expr constructordecl.consturctor_body) ^
	" " ^ string_of_typ constructordecl.return_type

let string_of_methoddef methoddecl = 
	string_of_expr methoddecl.methodname ^ " " ^
	String.concat " " (List.map string_of_param methoddecl.params) ^
	String.concat " " (List.map string_of_expr methoddecl.guard_body) ^
	String.concat " " (List.map string_of_expr methoddecl.storage_body) ^
	String.concat " " (List.map string_of_expr methoddecl.effects_body) ^
	string_of_typ methoddecl.returns


let string_of_implementation implementdecl =
	string_of_constructordef implementdecl.consturctor ^ " " ^
	String.concat "\n" (List.map string_of_methoddef implementdecl.methods)

let string_of_program (interfaces, implementations) =
	"\n\nParsed program: \n\n" ^
	String.concat "" (List.map string_of_interfacedef interfaces) ^ "\n"  ^
  String.concat "\n" (List.map string_of_implementation implementations) ^ "Yeah"
