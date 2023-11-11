/*
 * autocell - AutoCell compiler and viewer
 * Copyright (C) 2021  University of Toulouse, France <casse@irit.fr>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

%{

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

%}

%token EOF

/* keywords */
%token DIMENSIONS

%token END
%token OF

/* symbols */
%token ASSIGN
%token COMMA
%token LBRACKET RBRACKET
%token DOT_DOT
%token DOT
%token ADD
%token SUB
%token LPAREN, RPAREN
%token MULT, DIV, MODU
/* values */
%token<string> ID
%token<int> INT

%token IF
%token THEN
%token ELSE
%token ELSEIF

%token EQUALS
%token NOTEQUALS
%token LESSTHAN
%token GREATERTHAN
%token LESSTHANEQUALS
%token GREATERTHANEQUALS


%start program
%type<Ast.prog> program

%%

program: INT DIMENSIONS OF config END opt_statements EOF
	{
		if $1 != 2 then error "only 2 dimension accepted";
		($4, $6)
	}
;

config:
	INT DOT_DOT INT
		{
			if $1 >= $3 then error "illegal field values";
			[("", (0, ($1, $3)))]
		}
|	fields
		{ set_fields $1 }
;

fields:
	field
		{ [$1] }
|	fields COMMA field
		{$3 :: $1 }
;

field:
	ID OF INT DOT_DOT INT
		{
			if $3 >= $5 then error "illegal field values";
			($1, ($3, $5))
		}
;

opt_statements:
	/* empty */
		{ NOP }
|	statement opt_statements
		{ SEQ ($1, $2) }
;


statement:
	cell ASSIGN expression
		{
			if (fst $1) != 0 then error "assigned x must be 0";
			if (snd $1) != 0 then error "assigned Y must be 0";
			SET_CELL (0, $3)
		}
|	ID ASSIGN expression
		{
			SET_VAR (declare_var $1, $3)
		}
|	IF conditional THEN opt_statements END
		{
			IF_THEN ($2, $4, NOP)
		}
|	IF conditional THEN opt_statements ELSE opt_statements END
		{
			IF_THEN ($2, $4, $6)
		}
|	IF conditional THEN opt_statements ELSEIF conditional THEN opt_statements END
		{
			IF_THEN ($2, $4, IF_THEN($6, $8, NOP))
		}
;


cell:
	LBRACKET INT COMMA INT RBRACKET
		{
			if ($2 < -1) || ($2 > 1) then error "x out of range";
			if ($4 < -1) || ($4 > 1) then error "x out of range";
			($2, $4)
		}
;

exps:
	eval
		{ $1 }
|	cell
		{ CELL (0, fst $1, snd $1)}
|	INT
		{ CST $1 }
|	ID
		{ VAR (get_var $1)}
|	LPAREN expression RPAREN
		{ $2 }
;


eval:
	ADD exps
		{$2}
| 	SUB exps
		{ NEG ($2)}
;

prio:
	exps
		{$1}
|	exps MULT prio
		{BINOP (OP_MUL, $1, $3)}
|	exps DIV prio
		{BINOP (OP_DIV, $1, $3)}
|	exps MODU prio
		{ BINOP (OP_MOD, $1, $3)}
;

expression:
	prio
		{$1}
|	expression ADD prio
		{ BINOP (OP_ADD, $1, $3)}
|	expression SUB prio
		{ BINOP (OP_SUB, $1, $3)}
;

conditional:
	expression EQUALS expression
		{ COMP (COMP_EQ, $1, $3)}
|	expression NOTEQUALS expression
		{ COMP (COMP_NE, $1, $3)}
|	expression LESSTHAN expression
		{ COMP (COMP_LT, $1, $3)}
|	expression GREATERTHAN expression
		{ COMP (COMP_GT, $1, $3)}
|	expression LESSTHANEQUALS expression
		{ COMP (COMP_LE, $1, $3)}
|	expression GREATERTHANEQUALS expression
		{ COMP (COMP_GE, $1, $3)}
;