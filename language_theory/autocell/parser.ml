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
  | FOR
  | IN
  | DO
  | WHILE
  | MOORE
  | VONNEUMANN

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

# 65 "parser.ml"
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
  285 (* FOR *);
  286 (* IN *);
  287 (* DO *);
  288 (* WHILE *);
  289 (* MOORE *);
  290 (* VONNEUMANN *);
    0|]

let yytransl_block = [|
  273 (* ID *);
  274 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\005\000\003\000\003\000\
\006\000\006\000\006\000\006\000\006\000\006\000\007\000\011\000\
\011\000\011\000\011\000\011\000\012\000\012\000\010\000\013\000\
\013\000\014\000\014\000\014\000\014\000\008\000\008\000\008\000\
\009\000\009\000\009\000\009\000\009\000\009\000\000\000"

let yylen = "\002\000\
\007\000\003\000\001\000\001\000\003\000\005\000\000\000\002\000\
\003\000\003\000\005\000\007\000\009\000\001\000\005\000\001\000\
\001\000\001\000\001\000\003\000\002\000\002\000\007\000\001\000\
\001\000\001\000\003\000\003\000\003\000\001\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\039\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\014\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
\019\000\018\000\017\000\000\000\000\000\000\000\016\000\030\000\
\000\000\001\000\008\000\000\000\006\000\000\000\000\000\021\000\
\022\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\020\000\031\000\032\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\027\000\028\000\029\000\024\000\025\000\
\000\000\015\000\011\000\000\000\000\000\000\000\000\000\000\000\
\000\000\012\000\000\000\023\000\000\000\013\000"

let yydgoto = "\002\000\
\004\000\009\000\022\000\010\000\011\000\023\000\035\000\036\000\
\037\000\025\000\038\000\039\000\081\000\040\000"

let yysindex = "\006\000\
\240\254\000\000\009\255\000\000\013\255\006\255\018\255\005\255\
\024\255\023\255\000\000\020\255\034\255\254\254\019\255\022\255\
\000\000\047\255\040\255\057\255\056\255\076\000\254\254\076\255\
\000\000\000\000\063\255\077\255\057\255\057\255\057\255\057\255\
\000\000\000\000\000\000\030\255\064\255\004\255\000\000\000\000\
\053\255\000\000\000\000\057\255\000\000\067\255\032\255\000\000\
\000\000\251\254\057\255\057\255\057\255\057\255\057\255\057\255\
\057\255\057\255\254\254\057\255\057\255\057\255\026\255\032\255\
\079\255\000\000\000\000\000\000\032\255\032\255\032\255\032\255\
\032\255\032\255\010\255\000\000\000\000\000\000\000\000\000\000\
\059\255\000\000\000\000\254\254\057\255\254\254\089\255\072\255\
\091\255\000\000\254\254\000\000\092\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\093\255\000\000\000\000\000\000\096\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\011\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\029\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\012\255\000\000\000\000\000\000\000\000\035\000\
\000\000\000\000\000\000\000\000\078\255\080\255\081\255\082\255\
\083\255\084\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\095\255\000\000\095\255\000\000\000\000\
\000\000\000\000\095\255\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\236\255\000\000\084\000\000\000\242\255\249\255\
\020\000\000\000\031\000\000\000\000\000\027\000"

let yytablesize = 320
let yytable = "\024\000\
\026\000\003\000\043\000\018\000\051\000\052\000\001\000\066\000\
\024\000\005\000\007\000\083\000\013\000\007\000\019\000\006\000\
\020\000\060\000\061\000\062\000\012\000\047\000\007\000\008\000\
\050\000\014\000\021\000\015\000\010\000\027\000\084\000\085\000\
\007\000\007\000\009\000\007\000\064\000\016\000\075\000\051\000\
\052\000\051\000\052\000\029\000\024\000\069\000\070\000\071\000\
\072\000\073\000\074\000\017\000\053\000\054\000\055\000\056\000\
\057\000\058\000\079\000\080\000\048\000\049\000\018\000\087\000\
\028\000\089\000\030\000\031\000\032\000\024\000\093\000\024\000\
\041\000\033\000\034\000\042\000\024\000\067\000\068\000\044\000\
\045\000\046\000\063\000\059\000\065\000\082\000\076\000\077\000\
\078\000\086\000\090\000\091\000\092\000\094\000\003\000\007\000\
\007\000\033\000\026\000\034\000\035\000\036\000\037\000\038\000\
\088\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\026\000\000\000\000\000\000\000\026\000\000\000\
\000\000\000\000\026\000\026\000\007\000\026\000\000\000\000\000\
\000\000\026\000\000\000\026\000\026\000\026\000\026\000\026\000\
\026\000\026\000\026\000\026\000\026\000\026\000\010\000\007\000\
\007\000\000\000\010\000\000\000\009\000\000\000\000\000\000\000\
\009\000\000\000\000\000\000\000\000\000\010\000\000\000\010\000\
\000\000\010\000\010\000\009\000\000\000\009\000\000\000\009\000\
\009\000\010\000\000\000\000\000\000\000\000\000\000\000\009\000"

let yycheck = "\014\000\
\000\000\018\001\023\000\006\001\010\001\011\001\001\000\013\001\
\023\000\001\001\000\000\002\001\008\001\002\001\017\001\003\001\
\019\001\014\001\015\001\016\001\003\001\029\000\017\001\018\001\
\032\000\002\001\029\001\005\001\000\000\008\001\021\001\022\001\
\021\001\022\001\000\000\017\001\044\000\018\001\059\000\010\001\
\011\001\010\001\011\001\004\001\059\000\053\000\054\000\055\000\
\056\000\057\000\058\000\018\001\023\001\024\001\025\001\026\001\
\027\001\028\001\033\001\034\001\030\000\031\000\006\001\084\000\
\018\001\086\000\010\001\011\001\012\001\084\000\091\000\086\000\
\017\001\017\001\018\001\000\000\091\000\051\000\052\000\004\001\
\018\001\005\001\030\001\020\001\018\001\007\001\060\000\061\000\
\062\000\031\001\002\001\020\001\002\001\002\001\002\001\000\000\
\002\001\020\001\015\000\020\001\020\001\020\001\020\001\020\001\
\085\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
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
\255\255\255\255\010\001\011\001\002\001\013\001\255\255\255\255\
\255\255\017\001\255\255\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\002\001\021\001\
\022\001\255\255\006\001\255\255\002\001\255\255\255\255\255\255\
\006\001\255\255\255\255\255\255\255\255\017\001\255\255\019\001\
\255\255\021\001\022\001\017\001\255\255\019\001\255\255\021\001\
\022\001\029\001\255\255\255\255\255\255\255\255\255\255\029\001"

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
  FOR\000\
  IN\000\
  DO\000\
  WHILE\000\
  MOORE\000\
  VONNEUMANN\000\
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
# 89 "parser.mly"
 (
		if _1 != 2 then error "only 2 dimension accepted";
		(_4, _6)
	)
# 309 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 97 "parser.mly"
  (
			if _1 >= _3 then error "illegal field values";
			[("", (0, (_1, _3)))]
		)
# 320 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fields) in
    Obj.repr(
# 102 "parser.mly"
  ( set_fields _1 )
# 327 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 107 "parser.mly"
  ( [_1] )
# 334 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 109 "parser.mly"
  (_3 :: _1 )
# 342 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 114 "parser.mly"
  (
			if _3 >= _5 then error "illegal field values";
			(_1, (_3, _5))
		)
# 354 "parser.ml"
               : 'field))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "parser.mly"
  ( NOP )
# 360 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'opt_statements) in
    Obj.repr(
# 124 "parser.mly"
  ( SEQ (_1, _2) )
# 368 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cell) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 130 "parser.mly"
  (
			if (fst _1) != 0 then error "assigned x must be 0";
			if (snd _1) != 0 then error "assigned Y must be 0";
			SET_CELL (0, _3)
		)
# 380 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 136 "parser.mly"
  (
			SET_VAR (declare_var _1, _3)
		)
# 390 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'conditional) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 140 "parser.mly"
  (
			IF_THEN (_2, _4, NOP)
		)
# 400 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'conditional) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'opt_statements) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 144 "parser.mly"
  (
			IF_THEN (_2, _4, _6)
		)
# 411 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'conditional) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'opt_statements) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'conditional) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 148 "parser.mly"
  (
			IF_THEN (_2, _4, IF_THEN(_6, _8, NOP))
		)
# 423 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'loop_stmt) in
    Obj.repr(
# 152 "parser.mly"
  ( _1 )
# 430 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 158 "parser.mly"
  (
			if (_2 < -1) || (_2 > 1) then error "x out of range";
			if (_4 < -1) || (_4 > 1) then error "x out of range";
			(_2, _4)
		)
# 442 "parser.ml"
               : 'cell))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eval) in
    Obj.repr(
# 167 "parser.mly"
  ( _1 )
# 449 "parser.ml"
               : 'exps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cell) in
    Obj.repr(
# 169 "parser.mly"
  ( CELL (0, fst _1, snd _1))
# 456 "parser.ml"
               : 'exps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 171 "parser.mly"
  ( CST _1 )
# 463 "parser.ml"
               : 'exps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 173 "parser.mly"
  ( VAR (get_var _1))
# 470 "parser.ml"
               : 'exps))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 175 "parser.mly"
  ( _2 )
# 477 "parser.ml"
               : 'exps))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exps) in
    Obj.repr(
# 181 "parser.mly"
  (_2)
# 484 "parser.ml"
               : 'eval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exps) in
    Obj.repr(
# 183 "parser.mly"
  ( NEG (_2))
# 491 "parser.ml"
               : 'eval))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'neighborhood) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 188 "parser.mly"
  (
			match _4 with
			| MOORE -> FOR(MOORE, declare_var _2, _6)
			| VONNEUMANN -> FOR(VONNEUMANN, declare_var _2, _6)
		)
# 504 "parser.ml"
               : 'loop_stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 197 "parser.mly"
  ( MOORE )
# 510 "parser.ml"
               : 'neighborhood))
; (fun __caml_parser_env ->
    Obj.repr(
# 199 "parser.mly"
  ( VONNEUMANN )
# 516 "parser.ml"
               : 'neighborhood))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exps) in
    Obj.repr(
# 204 "parser.mly"
  (_1)
# 523 "parser.ml"
               : 'prio))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exps) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'prio) in
    Obj.repr(
# 206 "parser.mly"
  (BINOP (OP_MUL, _1, _3))
# 531 "parser.ml"
               : 'prio))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exps) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'prio) in
    Obj.repr(
# 208 "parser.mly"
  (BINOP (OP_DIV, _1, _3))
# 539 "parser.ml"
               : 'prio))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exps) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'prio) in
    Obj.repr(
# 210 "parser.mly"
  ( BINOP (OP_MOD, _1, _3))
# 547 "parser.ml"
               : 'prio))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'prio) in
    Obj.repr(
# 215 "parser.mly"
  (_1)
# 554 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'prio) in
    Obj.repr(
# 217 "parser.mly"
  ( BINOP (OP_ADD, _1, _3))
# 562 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'prio) in
    Obj.repr(
# 219 "parser.mly"
  ( BINOP (OP_SUB, _1, _3))
# 570 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 224 "parser.mly"
  ( COMP (COMP_EQ, _1, _3))
# 578 "parser.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 226 "parser.mly"
  ( COMP (COMP_NE, _1, _3))
# 586 "parser.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 228 "parser.mly"
  ( COMP (COMP_LT, _1, _3))
# 594 "parser.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 230 "parser.mly"
  ( COMP (COMP_GT, _1, _3))
# 602 "parser.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 232 "parser.mly"
  ( COMP (COMP_LE, _1, _3))
# 610 "parser.ml"
               : 'conditional))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 234 "parser.mly"
  ( COMP (COMP_GE, _1, _3))
# 618 "parser.ml"
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
