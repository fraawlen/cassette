/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Resources (DR) library.
 *
 * This library is free software; you can redistribute it and/or modify it either under the terms of the GNU
 * Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the
 * License or (at your option) any later version.
 *
 * This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
 * See the LGPL for the specific language governing rights and limitations.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this program. If not,
 * see <http://www.gnu.org/licenses/>.
 */

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <derelict/do.h>

#include "token.h"

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

do_dictionary_t *
dr_token_dictionary_create(void)
{
	do_dictionary_t *dict;

	dict = do_dictionary_create(50, 0.6);

	/* universal tokens */

	do_dictionary_write(dict, "EOF",    0, DR_TOKEN_EOF);
	do_dictionary_write(dict, "",       0, DR_TOKEN_INVALID);
	do_dictionary_write(dict, "//",     0, DR_TOKEN_COMMENT);
	do_dictionary_write(dict, "(",      0, DR_TOKEN_FILLER);
	do_dictionary_write(dict, ")",      0, DR_TOKEN_FILLER);
	do_dictionary_write(dict, "=",      0, DR_TOKEN_FILLER);
	do_dictionary_write(dict, ":=",     0, DR_TOKEN_FILLER);
	do_dictionary_write(dict, "JOIN",   0, DR_TOKEN_JOIN);
	do_dictionary_write(dict, "\\",     0, DR_TOKEN_ESCAPE);
	do_dictionary_write(dict, "$",      0, DR_TOKEN_VAR_INJECTION);
	do_dictionary_write(dict, "%",      0, DR_TOKEN_ITER_INJECTION);

	do_dictionary_write(dict, "<",      0, DR_TOKEN_IF_LESS);
	do_dictionary_write(dict, "<=",     0, DR_TOKEN_IF_LESS_EQ);
	do_dictionary_write(dict, ">",      0, DR_TOKEN_IF_MORE);
	do_dictionary_write(dict, ">=",     0, DR_TOKEN_IF_MORE_EQ);
	do_dictionary_write(dict, "==",     0, DR_TOKEN_IF_EQ);
	do_dictionary_write(dict, "!=",     0, DR_TOKEN_IF_EQ_NOT);
	
	do_dictionary_write(dict, "TIME",   0, DR_TOKEN_TIMESTAMP);
	do_dictionary_write(dict, "PI",     0, DR_TOKEN_CONST_PI);
	do_dictionary_write(dict, "E",      0, DR_TOKEN_CONST_EULER);
	do_dictionary_write(dict, "TRUE",   0, DR_TOKEN_CONST_TRUE);
	do_dictionary_write(dict, "FALSE",  0, DR_TOKEN_CONST_FALSE);

	do_dictionary_write(dict, "SQRT",   0, DR_TOKEN_OP_SQRT);
	do_dictionary_write(dict, "CBRT",   0, DR_TOKEN_OP_CBRT);
	do_dictionary_write(dict, "ABS",    0, DR_TOKEN_OP_ABS);
	do_dictionary_write(dict, "CEIL",   0, DR_TOKEN_OP_CEILING);
	do_dictionary_write(dict, "FLOOR",  0, DR_TOKEN_OP_FLOOR);
	do_dictionary_write(dict, "COS",    0, DR_TOKEN_OP_COS);
	do_dictionary_write(dict, "SIN",    0, DR_TOKEN_OP_SIN);
	do_dictionary_write(dict, "TAN",    0, DR_TOKEN_OP_TAN);
	do_dictionary_write(dict, "ACOS",   0, DR_TOKEN_OP_ACOS);
	do_dictionary_write(dict, "ASIN",   0, DR_TOKEN_OP_ASIN);
	do_dictionary_write(dict, "ATAN",   0, DR_TOKEN_OP_ATAN);
	do_dictionary_write(dict, "COSH",   0, DR_TOKEN_OP_COSH);
	do_dictionary_write(dict, "SINH",   0, DR_TOKEN_OP_SINH);
	do_dictionary_write(dict, "LN",     0, DR_TOKEN_OP_LN);
	do_dictionary_write(dict, "LOG",    0, DR_TOKEN_OP_LOG);

	do_dictionary_write(dict, "+",      0, DR_TOKEN_OP_ADD);
	do_dictionary_write(dict, "-",      0, DR_TOKEN_OP_SUBSTRACT);
	do_dictionary_write(dict, "*",      0, DR_TOKEN_OP_MULTIPLY);
	do_dictionary_write(dict, "/",      0, DR_TOKEN_OP_DIVIDE);
	do_dictionary_write(dict, "MOD",    0, DR_TOKEN_OP_MOD);
	do_dictionary_write(dict, "POW",    0, DR_TOKEN_OP_POW);
	do_dictionary_write(dict, "BIG",    0, DR_TOKEN_OP_BIGGEST);
	do_dictionary_write(dict, "SMALL",  0, DR_TOKEN_OP_SMALLEST);
	do_dictionary_write(dict, "RAND",   0, DR_TOKEN_OP_RANDOM);
	
	do_dictionary_write(dict, "ITRPL",  0, DR_TOKEN_OP_INTERPOLATE);
	do_dictionary_write(dict, "LIMIT",  0, DR_TOKEN_OP_LIMIT);
	
	do_dictionary_write(dict, "CITRPL", 0, DR_TOKEN_CL_INTERPOLATE);
	do_dictionary_write(dict, "RGB",    0, DR_TOKEN_CL_RGB);
	do_dictionary_write(dict, "RGBA",   0, DR_TOKEN_CL_RGBA);

	/* lead tokens */

	do_dictionary_write(dict, "LET",         0, DR_TOKEN_VAR_DECLARATION);
	do_dictionary_write(dict, "SECTION",     0, DR_TOKEN_SECTION_BEGIN);
	do_dictionary_write(dict, "SECTION_ADD", 0, DR_TOKEN_SECTION_ADD);
	do_dictionary_write(dict, "SECTION_DEL", 0, DR_TOKEN_SECTION_DEL);
	do_dictionary_write(dict, "INCLUDE",     0, DR_TOKEN_INCLUDE);
	do_dictionary_write(dict, "ITERATE",     0, DR_TOKEN_ITERATOR);
	do_dictionary_write(dict, "RAND_SEED",   0, DR_TOKEN_RAND_SEED);

	return dict;
}
