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

# 49 "parser.ml"
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
    0|]

let yytransl_block = [|
  273 (* ID *);
  274 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\005\000\003\000\003\000\
\006\000\006\000\007\000\007\000\008\000\010\000\011\000\011\000\
\011\000\014\000\014\000\014\000\014\000\018\000\018\000\018\000\
\018\000\018\000\018\000\012\000\012\000\012\000\012\000\013\000\
\013\000\013\000\013\000\015\000\015\000\015\000\015\000\016\000\
\016\000\016\000\016\000\017\000\017\000\017\000\017\000\020\000\
\020\000\019\000\019\000\019\000\009\000\009\000\009\000\009\000\
\000\000"

let yylen = "\002\000\
\007\000\003\000\001\000\001\000\003\000\005\000\000\000\001\000\
\001\000\002\000\003\000\003\000\005\000\003\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\002\000\002\000\002\000\
\002\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\057\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\008\000\000\000\000\000\005\000\
\000\000\000\000\000\000\001\000\010\000\000\000\006\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\012\000\000\000\
\056\000\015\000\016\000\017\000\018\000\019\000\020\000\021\000\
\000\000\011\000\000\000\052\000\051\000\050\000\000\000\023\000\
\000\000\000\000\026\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\014\000\031\000\035\000\039\000\043\000\047\000\000\000\029\000\
\000\000\000\000\033\000\000\000\000\000\037\000\000\000\000\000\
\041\000\000\000\000\000\045\000\000\000"

let yydgoto = "\002\000\
\004\000\009\000\020\000\010\000\011\000\021\000\022\000\054\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\046\000\
\047\000\048\000\049\000\000\000"

let yysindex = "\006\000\
\247\254\000\000\020\255\000\000\019\255\255\254\021\255\015\255\
\024\255\028\255\000\000\016\255\017\255\253\254\022\255\033\255\
\000\000\018\255\040\255\053\000\000\000\253\254\054\255\000\000\
\041\255\061\255\250\254\000\000\000\000\250\254\000\000\049\255\
\039\255\039\255\039\255\000\000\000\000\000\000\000\000\076\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\083\255\000\000\081\255\000\000\000\000\000\000\076\255\000\000\
\083\255\076\255\000\000\083\255\082\255\013\255\013\255\013\255\
\013\255\013\255\039\255\039\255\039\255\039\255\039\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\076\255\000\000\
\083\255\076\255\000\000\083\255\076\255\000\000\083\255\076\255\
\000\000\083\255\076\255\000\000\083\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\087\255\000\000\000\000\000\000\096\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\100\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\013\000\025\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\008\000\000\000\
\020\000\032\000\000\000\037\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\038\000\000\000\
\040\000\046\000\000\000\052\000\054\000\000\000\055\000\060\000\
\000\000\068\000\069\000\000\000\070\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000\086\000\080\000\000\000\244\255\
\073\000\014\000\250\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\009\000\000\000"

let yytablesize = 343
let yytable = "\018\000\
\055\000\023\000\018\000\033\000\034\000\035\000\001\000\024\000\
\003\000\023\000\036\000\037\000\054\000\019\000\038\000\007\000\
\008\000\038\000\018\000\022\000\005\000\006\000\013\000\012\000\
\053\000\014\000\056\000\059\000\061\000\052\000\053\000\027\000\
\015\000\016\000\017\000\026\000\025\000\030\000\007\000\028\000\
\025\000\057\000\060\000\027\000\018\000\034\000\055\000\058\000\
\033\000\034\000\035\000\032\000\028\000\038\000\036\000\052\000\
\053\000\030\000\031\000\042\000\080\000\083\000\086\000\089\000\
\092\000\032\000\051\000\040\000\046\000\044\000\074\000\075\000\
\076\000\077\000\078\000\081\000\084\000\087\000\090\000\093\000\
\079\000\082\000\085\000\088\000\091\000\062\000\063\000\072\000\
\003\000\064\000\065\000\066\000\067\000\068\000\073\000\007\000\
\069\000\070\000\071\000\009\000\024\000\029\000\050\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\055\000\000\000\
\000\000\000\000\052\000\052\000\000\000\024\000\052\000\052\000\
\052\000\055\000\054\000\000\000\024\000\000\000\051\000\051\000\
\024\000\022\000\051\000\051\000\051\000\054\000\053\000\000\000\
\022\000\000\000\050\000\050\000\022\000\027\000\050\000\050\000\
\050\000\053\000\025\000\030\000\027\000\028\000\000\000\000\000\
\027\000\025\000\030\000\034\000\028\000\025\000\030\000\000\000\
\028\000\032\000\034\000\038\000\036\000\000\000\034\000\000\000\
\032\000\042\000\038\000\036\000\032\000\000\000\038\000\036\000\
\042\000\040\000\046\000\044\000\042\000\000\000\000\000\000\000\
\040\000\046\000\044\000\000\000\040\000\046\000\044\000"

let yycheck = "\006\001\
\000\000\014\000\006\001\010\001\011\001\012\001\001\000\000\000\
\018\001\022\000\017\001\018\001\000\000\017\001\027\000\017\001\
\018\001\030\000\006\001\000\000\001\001\003\001\008\001\003\001\
\000\000\002\001\033\000\034\000\035\000\017\001\018\001\000\000\
\005\001\018\001\018\001\018\001\000\000\000\000\017\001\000\000\
\008\001\033\000\034\000\004\001\006\001\000\000\033\000\034\000\
\010\001\011\001\012\001\000\000\000\000\000\000\000\000\017\001\
\018\001\004\001\018\001\000\000\067\000\068\000\069\000\070\000\
\071\000\005\001\018\001\000\000\000\000\000\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\069\000\070\000\071\000\
\067\000\068\000\069\000\070\000\071\000\010\001\011\001\007\001\
\002\001\014\001\015\001\016\001\010\001\011\001\013\001\000\000\
\014\001\015\001\016\001\000\000\015\000\022\000\030\000\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\006\001\255\255\
\255\255\255\255\010\001\011\001\255\255\006\001\014\001\015\001\
\016\001\017\001\006\001\255\255\013\001\255\255\010\001\011\001\
\017\001\006\001\014\001\015\001\016\001\017\001\006\001\255\255\
\013\001\255\255\010\001\011\001\017\001\006\001\014\001\015\001\
\016\001\017\001\006\001\006\001\013\001\006\001\255\255\255\255\
\017\001\013\001\013\001\006\001\013\001\017\001\017\001\255\255\
\017\001\006\001\013\001\006\001\006\001\255\255\017\001\255\255\
\013\001\006\001\013\001\013\001\017\001\255\255\017\001\017\001\
\013\001\006\001\006\001\006\001\017\001\255\255\255\255\255\255\
\013\001\013\001\013\001\255\255\017\001\017\001\017\001"

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
# 70 "parser.mly"
 (
		if _1 != 2 then error "only 2 dimension accepted";
		(_4, _6)
	)
# 275 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 78 "parser.mly"
  (
			if _1 >= _3 then error "illegal field values";
			[("", (0, (_1, _3)))]
		)
# 286 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fields) in
    Obj.repr(
# 83 "parser.mly"
  ( set_fields _1 )
# 293 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 88 "parser.mly"
  ( [_1] )
# 300 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 90 "parser.mly"
  (_3 :: _1 )
# 308 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 95 "parser.mly"
  (
			if _3 >= _5 then error "illegal field values";
			(_1, (_3, _5))
		)
# 320 "parser.ml"
               : 'field))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
  ( NOP )
# 326 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'statements) in
    Obj.repr(
# 105 "parser.mly"
  ( _1 )
# 333 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 109 "parser.mly"
           (_1)
# 340 "parser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statements) in
    Obj.repr(
# 111 "parser.mly"
 (SEQ (_1, _2))
# 348 "parser.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cell) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 116 "parser.mly"
  (
			if (fst _1) != 0 then error "assigned x must be 0";
			if (snd _1) != 0 then error "assigned Y must be 0";
			SET_CELL (0, _3)
		)
# 360 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 122 "parser.mly"
  (
			NOP
		)
# 370 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 130 "parser.mly"
  (
			if (_2 < -1) || (_2 > 1) then error "x out of range";
			if (_4 < -1) || (_4 > 1) then error "x out of range";
			(_2, _4)
		)
# 382 "parser.ml"
               : 'cell))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'eval) in
    Obj.repr(
# 139 "parser.mly"
 (NOP)
# 389 "parser.ml"
               : 'parant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'add) in
    Obj.repr(
# 144 "parser.mly"
  (NOP)
# 396 "parser.ml"
               : 'eval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sub) in
    Obj.repr(
# 146 "parser.mly"
  (NOP)
# 403 "parser.ml"
               : 'eval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'prio) in
    Obj.repr(
# 148 "parser.mly"
  (_1)
# 410 "parser.ml"
               : 'eval))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mult) in
    Obj.repr(
# 153 "parser.mly"
  (NOP)
# 417 "parser.ml"
               : 'prio))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'div) in
    Obj.repr(
# 155 "parser.mly"
  (NOP)
# 424 "parser.ml"
               : 'prio))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'modu) in
    Obj.repr(
# 157 "parser.mly"
  (NOP)
# 431 "parser.ml"
               : 'prio))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'unary) in
    Obj.repr(
# 159 "parser.mly"
  ( _1 )
# 438 "parser.ml"
               : 'prio))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 164 "parser.mly"
  (NOP)
# 445 "parser.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'eval) in
    Obj.repr(
# 166 "parser.mly"
  (NOP)
# 452 "parser.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'parant) in
    Obj.repr(
# 168 "parser.mly"
  (NOP)
# 459 "parser.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 170 "parser.mly"
  (NOP)
# 466 "parser.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'eval) in
    Obj.repr(
# 172 "parser.mly"
  (NOP)
# 473 "parser.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'parant) in
    Obj.repr(
# 174 "parser.mly"
  (NOP)
# 480 "parser.ml"
               : 'unary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 179 "parser.mly"
  (NOP)
# 488 "parser.ml"
               : 'add))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eval) in
    Obj.repr(
# 181 "parser.mly"
  (NOP)
# 496 "parser.ml"
               : 'add))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parant) in
    Obj.repr(
# 183 "parser.mly"
  (NOP)
# 504 "parser.ml"
               : 'add))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parant) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 185 "parser.mly"
  (NOP)
# 512 "parser.ml"
               : 'add))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 190 "parser.mly"
  (NOP)
# 520 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eval) in
    Obj.repr(
# 192 "parser.mly"
  (NOP)
# 528 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parant) in
    Obj.repr(
# 194 "parser.mly"
  (NOP)
# 536 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parant) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 196 "parser.mly"
  (NOP)
# 544 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 201 "parser.mly"
  (NOP)
# 552 "parser.ml"
               : 'mult))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eval) in
    Obj.repr(
# 203 "parser.mly"
  (NOP)
# 560 "parser.ml"
               : 'mult))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parant) in
    Obj.repr(
# 205 "parser.mly"
  (NOP)
# 568 "parser.ml"
               : 'mult))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parant) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 207 "parser.mly"
  (NOP)
# 576 "parser.ml"
               : 'mult))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 212 "parser.mly"
  (NOP)
# 584 "parser.ml"
               : 'div))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eval) in
    Obj.repr(
# 214 "parser.mly"
  (NOP)
# 592 "parser.ml"
               : 'div))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parant) in
    Obj.repr(
# 216 "parser.mly"
  (NOP)
# 600 "parser.ml"
               : 'div))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parant) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 218 "parser.mly"
  (NOP)
# 608 "parser.ml"
               : 'div))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 223 "parser.mly"
  (NOP)
# 616 "parser.ml"
               : 'modu))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'eval) in
    Obj.repr(
# 225 "parser.mly"
  (NOP)
# 624 "parser.ml"
               : 'modu))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'value) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'parant) in
    Obj.repr(
# 227 "parser.mly"
  (NOP)
# 632 "parser.ml"
               : 'modu))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parant) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 229 "parser.mly"
  (NOP)
# 640 "parser.ml"
               : 'modu))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 234 "parser.mly"
        ( NOP )
# 648 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'value) in
    Obj.repr(
# 236 "parser.mly"
        ( _1 )
# 655 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cell) in
    Obj.repr(
# 241 "parser.mly"
  (printf "[%d, %d]\n" (fst _1) (snd _1); NOP)
# 662 "parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 243 "parser.mly"
  (printf "%d\n" _1; NOP)
# 669 "parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 245 "parser.mly"
  (printf "%s\n" _1; NOP)
# 676 "parser.ml"
               : 'value))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cell) in
    Obj.repr(
# 252 "parser.mly"
  ( CELL (0, fst _1, snd _1) )
# 683 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 254 "parser.mly"
  ( CST _1 )
# 690 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 256 "parser.mly"
     ( NONE )
# 697 "parser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'eval) in
    Obj.repr(
# 258 "parser.mly"
  ( NONE )
# 704 "parser.ml"
               : 'expression))
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
