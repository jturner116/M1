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
|	statements
		{ $1 }
;

statements:
	statement {$1}
| statement statements
	{SEQ ($1, $2)}
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
			NOP
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

parant:
	LPAREN eval RPAREN
	{NOP}
;

eval:
	add
		{NOP}
| 	sub
		{NOP}
|	prio
		{$1}
;

prio:
	mult
		{NOP}
|	div
		{NOP}
|	modu
		{NOP}
|	unary
		{ $1 }
;

unary:
	ADD value
		{NOP}
|	ADD eval
		{NOP}
|	ADD parant
		{NOP}
|	SUB value
		{NOP}
|	SUB eval
		{NOP}
|	SUB parant
		{NOP}
;

add:
	value ADD value
		{NOP}
|	value ADD eval
		{NOP}
|	value ADD parant
		{NOP}
|	parant ADD value
		{NOP}
;

sub:
	value SUB value
		{NOP}
|	value SUB eval
		{NOP}
|	value SUB parant
		{NOP}
|	parant SUB value
		{NOP}
;

mult:
	value MULT value
		{NOP}
|	value MULT eval
		{NOP}
|	value MULT parant
		{NOP}
|	parant MULT value
		{NOP}
;

div:
	value DIV value
		{NOP}
|	value DIV eval
		{NOP}
|	value DIV parant
		{NOP}
|	parant DIV value
		{NOP}
;

modu:
	value MODU value
		{NOP}
|	value MODU eval
		{NOP}
|	value MODU parant
		{NOP}
|	parant MODU value
		{NOP}
;

expr:
    expr ADD value
        { NOP }
| 	value
        { $1 }
;

value:
	cell
		{printf "[%d, %d]\n" (fst $1) (snd $1); NOP}
|	INT
		{printf "%d\n" $1; NOP}
|	ID
		{printf "%s\n" $1; NOP}
;


	
expression:
	cell
		{ CELL (0, fst $1, snd $1) }
|	INT
		{ CST $1 }
|   ID
    	{ NONE }
|	eval
		{ NONE }
;