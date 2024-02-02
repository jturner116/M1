type token =
  | EOF
  | DIMENSIONS
  | END
  | OF
  | ASSIGN
  | COMMA
  | LBRACKET
  | RBRACKET
  | DOT_DOT
  | DOT
  | ADD
  | SUB
  | LPAREN
  | RPAREN
  | MULT
  | DIV
  | MODU
  | ID of (string)
  | INT of (int)
  | IF
  | THEN
  | ELSE
  | ELSEIF
  | EQUALS
  | NOTEQUALS
  | LESSTHAN
  | GREATERTHAN
  | LESSTHANEQUALS
  | GREATERTHANEQUALS

open Parsing;;
let _ = parse_error;;
# 17 "parser.mly"

open Common
open Ast
open Printf
open Symbols

(** Raise a syntax error with the given message.
	@param msg	Message of the error. *)
let error msg =
	raise (SyntaxError msg)


(** Restructure the when assignment into selections.
	@param f	Function to build the assignment.
	@param v	Initial values.
	@param ws	Sequence of (condition, expression).
	@return		Built statement. *)
let rec make_when f v ws =
	match ws with
	| [] ->	f v
	| (c, nv)::t ->
		IF_THEN(c, f v, make_when f nv t)

# 59 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* DIMENSIONS *);
  258 (* END *);
  259 (* OF *);
  260 (* ASSIGN *);
  261 (* COMMA *);
  262 (* LBRACKET *);
  263 (* RBRACKET *);
  264 (* DOT_DOT *);
  265 (* DOT *);
  266 (* ADD *);
  267 (* SUB *);
  268 (* LPAREN *);
  269 (* RPAREN *);
  270 (* MULT *);
  271 (* DIV *);
  272 (* MODU *);
  275 (* IF *);
  276 (* THEN *);
  277 (* ELSE *);
  278 (* ELSEIF *);
  279 (* EQUALS *);
  280 (* NOTEQUALS *);
  281 (* LESSTHAN *);
  282 (* GREATERTHAN *);
  283 (* LESSTHANEQUALS *);
  284 (* GREATERTHANEQUALS *);
    0|]

let yytransl_block = [|
  273 (* ID *);
  274 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\005\000\003\000\003\000\
\006\000\006\000\006\000\006\000\006\000\007\000\010\000\010\000\
\010\000\010\000\010\000\011\000\011\000\012\000\012\000\012\000\
\012\000\008\000\008\000\008\000\009\000\009\000\009\000\009\000\
\009\000\009\000\000\000"

let yylen = "\002\000\
\007\000\003\000\001\000\001\000\003\000\005\000\000\000\002\000\
\003\000\003\000\005\000\007\000\009\000\005\000\001\000\001\000\
\001\000\001\000\003\000\002\000\002\000\001\000\003\000\003\000\
\003\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\035\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
\000\000\000\000\000\000\000\000\000\000\000\000\018\000\017\000\
\016\000\000\000\000\000\000\000\015\000\026\000\001\000\008\000\
\000\000\006\000\000\000\000\000\020\000\021\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\019\000\027\000\028\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000\
\024\000\025\000\014\000\011\000\000\000\000\000\000\000\000\000\
\012\000\000\000\000\000\013\000"

let yydgoto = "\002\000\
\004\000\009\000\021\000\010\000\011\000\022\000\033\000\034\000\
\035\000\036\000\037\000\038\000"

let yysindex = "\004\000\
\245\254\000\000\013\255\000\000\026\255\250\254\040\255\008\255\
\059\255\051\255\000\000\049\255\052\255\254\254\056\255\061\255\
\000\000\057\255\070\255\054\255\076\000\254\254\073\255\000\000\
\060\255\074\255\054\255\054\255\054\255\054\255\000\000\000\000\
\000\000\027\255\062\255\025\255\000\000\000\000\000\000\000\000\
\054\255\000\000\063\255\022\255\000\000\000\000\017\255\054\255\
\054\255\054\255\054\255\054\255\054\255\054\255\054\255\254\254\
\054\255\054\255\054\255\022\255\076\255\000\000\000\000\000\000\
\022\255\022\255\022\255\022\255\022\255\022\255\001\255\000\000\
\000\000\000\000\000\000\000\000\254\254\054\255\078\255\064\255\
\000\000\254\254\083\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\084\255\000\000\000\000\000\000\087\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\034\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\013\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\255\
\000\000\000\000\000\000\031\000\000\000\000\000\000\000\000\000\
\068\255\069\255\071\255\072\255\075\255\077\255\000\000\000\000\
\000\000\000\000\000\000\000\000\088\255\000\000\000\000\000\000\
\000\000\088\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\236\255\000\000\078\000\000\000\242\255\250\255\
\016\000\030\000\000\000\217\255"

let yytablesize = 312
let yytable = "\023\000\
\022\000\040\000\076\000\018\000\001\000\007\000\003\000\023\000\
\063\000\064\000\007\000\008\000\010\000\005\000\019\000\013\000\
\020\000\072\000\073\000\074\000\044\000\077\000\078\000\047\000\
\007\000\007\000\048\000\049\000\006\000\062\000\009\000\048\000\
\049\000\007\000\060\000\071\000\048\000\049\000\057\000\058\000\
\059\000\023\000\012\000\065\000\066\000\067\000\068\000\069\000\
\070\000\050\000\051\000\052\000\053\000\054\000\055\000\015\000\
\079\000\045\000\046\000\018\000\014\000\083\000\023\000\028\000\
\029\000\030\000\016\000\023\000\025\000\017\000\031\000\032\000\
\007\000\027\000\026\000\039\000\041\000\042\000\043\000\081\000\
\061\000\056\000\075\000\082\000\084\000\003\000\007\000\029\000\
\030\000\007\000\031\000\032\000\024\000\080\000\033\000\000\000\
\034\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\022\000\000\000\000\000\000\000\022\000\000\000\
\000\000\000\000\022\000\022\000\000\000\022\000\010\000\000\000\
\000\000\022\000\010\000\022\000\022\000\022\000\022\000\022\000\
\022\000\022\000\022\000\022\000\022\000\010\000\000\000\010\000\
\009\000\010\000\010\000\007\000\009\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\009\000\
\000\000\009\000\000\000\009\000\009\000\000\000\007\000\007\000"

let yycheck = "\014\000\
\000\000\022\000\002\001\006\001\001\000\002\001\018\001\022\000\
\048\000\049\000\017\001\018\001\000\000\001\001\017\001\008\001\
\019\001\057\000\058\000\059\000\027\000\021\001\022\001\030\000\
\021\001\022\001\010\001\011\001\003\001\013\001\000\000\010\001\
\011\001\000\000\041\000\056\000\010\001\011\001\014\001\015\001\
\016\001\056\000\003\001\050\000\051\000\052\000\053\000\054\000\
\055\000\023\001\024\001\025\001\026\001\027\001\028\001\005\001\
\077\000\028\000\029\000\006\001\002\001\082\000\077\000\010\001\
\011\001\012\001\018\001\082\000\008\001\018\001\017\001\018\001\
\017\001\004\001\018\001\000\000\004\001\018\001\005\001\002\001\
\018\001\020\001\007\001\020\001\002\001\002\001\000\000\020\001\
\020\001\002\001\020\001\020\001\015\000\078\000\020\001\255\255\
\020\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\255\255\255\255\255\255\006\001\255\255\
\255\255\255\255\010\001\011\001\255\255\013\001\002\001\255\255\
\255\255\017\001\006\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\017\001\255\255\019\001\
\002\001\021\001\022\001\002\001\006\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\017\001\
\255\255\019\001\255\255\021\001\022\001\255\255\021\001\022\001"

let yynames_const = "\
  EOF\000\
  DIMENSIONS\000\
  END\000\
  OF\000\
  ASSIGN\000\
  COMMA\000\
  LBRACKET\000\
  RBRACKET\000\
  DOT_DOT\000\
  DOT\000\
  ADD\000\
  SUB\000\
  LPAREN\000\
  RPAREN\000\
  MULT\000\
  DIV\000\
  MODU\000\
  IF\000\
  THEN\000\
  ELSE\000\
  ELSEIF\000\
  EQUALS\000\
  NOTEQUALS\000\
  LESSTHAN\000\
  GREATERTHAN\000\
  LESSTHANEQUALS\000\
  GREATERTHANEQUALS\000\
  "

let yynames_block = "\
  ID\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'config) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 83 "parser.mly"
 (
		if _1 != 2 then error "only 2 dimension accepted";
		(_4, _6)
	)
# 286 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 91 "parser.mly"
  (
			if _1 >= _3 then error "illegal field values";
			[("", (0, (_1, _3)))]
		)
# 297 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fields) in
    Obj.repr(
# 96 "parser.mly"
  ( set_fields _1 )
# 304 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 101 "parser.mly"
  ( [_1] )
# 311 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 103 "parser.mly"
  (_3 :: _1 )
# 319 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 108 "parser.mly"
  (
			if _3 >= _5 then error "illegal field values";
			(_1, (_3, _5))
		)
# 331 "parser.ml"
               : 'field))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser.mly"
  ( NOP )
# 337 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'opt_statements) in
    Obj.repr(
# 118 "parser.mly"
  ( SEQ (_1, _2) )
# 345 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cell) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 124 "parser.mly"
  (
			if (fst _1) != 0 then error "assigned x must be 0";
			if (snd _1) != 0 then error "assigned Y must be 0";
			SET_CELL (0, _3)
		)
# 357 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 130 "parser.mly"
  (
			SET_VAR (declare_var _1, _3)
		)
# 367 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'conditional) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 134 "parser.mly"
  (
			IF_THEN (_2, _4, NOP)
		)
# 377 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'conditional) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'opt_statements) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 138 "parser.mly"
  (
			IF_THEN (_2, _4, _6)
		)
# 388 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'conditional) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'opt_statements) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'conditional) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 142 "parser.mly"
  (
			IF_THEN (_2, _4, IF_THEN(_6, _8, NOP))
		)
# 400 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 150 "parser.mly"
  (
			if (_2 < -1) || (_2 > 1) then error "x out of range";
			if (_4 < -1) || (_4 > 1) then error "x out of range";
			(_2, _4)
		)
# 412 "parser.ml"
               : 'cell))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eval) in
    Obj.repr(
# 159 "parser.mly"
  ( _1 )
# 419 "parser.ml"
               : 'exps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cell) in
    Obj.repr(
# 161 "parser.mly"
  ( CELL (0, fst _1, snd _1))
# 426 "parser.ml"
               : 'exps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 163 "parser.mly"
  ( CST _1 )
# 433 "parser.ml"
               : 'exps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 165 "parser.mly"
  ( VAR (get_var _1))
# 440 "parser.ml"
               : 'exps))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 167 "parser.mly"
  ( _2 )
# 447 "parser.ml"
               : 'exps))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exps) in
    Obj.repr(
# 173 "parser.mly"
  (_2)
# 454 "parser.ml"
               : 'eval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exps) in
    Obj.repr(
# 175 "parser.mly"
  ( NEG (_2))
# 461 "parser.ml"
               : 'eval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exps) in
    Obj.repr(
# 180 "parser.mly"
  (_1)
# 468 "parser.ml"
               : 'prio))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exps) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'prio) in
    Obj.repr(
# 182 "parser.mly"
  (BINOP (OP_MUL, _1, _3))
# 476 "parser.ml"
               : 'prio))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exps) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'prio) in
    Obj.repr(
# 184 "parser.mly"
  (BINOP (OP_DIV, _1, _3))
# 484 "parser.ml"
               : 'prio))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exps) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'prio) in
    Obj.repr(
# 186 "parser.mly"
  ( BINOP (OP_MOD, _1, _3))
# 492 "parser.ml"
               : 'prio))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'prio) in
    Obj.repr(
# 191 "parser.mly"
  (_1)
# 499 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'prio) in
    Obj.repr(
# 193 "parser.mly"
  ( BINOP (OP_ADD, _1, _3))
# 507 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'prio) in
    Obj.repr(
# 195 "parser.mly"
  ( BINOP (OP_SUB, _1, _3))
# 515 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 200 "parser.mly"
  ( COMP (COMP_EQ, _1, _3))
# 523 "parser.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 202 "parser.mly"
  ( COMP (COMP_NE, _1, _3))
# 531 "parser.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 204 "parser.mly"
  ( COMP (COMP_LT, _1, _3))
# 539 "parser.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 206 "parser.mly"
  ( COMP (COMP_GT, _1, _3))
# 547 "parser.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 208 "parser.mly"
  ( COMP (COMP_LE, _1, _3))
# 555 "parser.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 210 "parser.mly"
  ( COMP (COMP_GE, _1, _3))
# 563 "parser.ml"
               : 'conditional))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)
