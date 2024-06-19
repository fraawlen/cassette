/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Configuration (CCFG) library.
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

#include <cassette/cobj.h>
#include <stdlib.h>

#include "token.h"

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

cobj_dictionary_t *
token_dictionary_create(void)
{
	cobj_dictionary_t *dict;

	dict = cobj_dictionary_create(70, 0.4);

	/* substitution tokens */

	cobj_dictionary_write(dict, "EOF",    0, TOKEN_EOF);
	cobj_dictionary_write(dict, "",       0, TOKEN_INVALID);
	cobj_dictionary_write(dict, "//",     0, TOKEN_COMMENT);
	cobj_dictionary_write(dict, "EOS",    0, TOKEN_COMMENT);
	cobj_dictionary_write(dict, "=",      0, TOKEN_FILLER);
	cobj_dictionary_write(dict, ":=",     0, TOKEN_FILLER);
	cobj_dictionary_write(dict, "JOIN",   0, TOKEN_JOIN);
	cobj_dictionary_write(dict, "\\",     0, TOKEN_ESCAPE);
	cobj_dictionary_write(dict, "$",      0, TOKEN_VAR_INJECTION);
	cobj_dictionary_write(dict, "%",      0, TOKEN_ITER_INJECTION);
	cobj_dictionary_write(dict, "$$",     0, TOKEN_PARAM_INJECTION);
	cobj_dictionary_write(dict, "VAR",    0, TOKEN_VAR_INJECTION);
	cobj_dictionary_write(dict, "ITER",   0, TOKEN_ITER_INJECTION);
	cobj_dictionary_write(dict, "PARAM",  0, TOKEN_PARAM_INJECTION);

	cobj_dictionary_write(dict, "<",      0, TOKEN_IF_LESS);
	cobj_dictionary_write(dict, "<=",     0, TOKEN_IF_LESS_EQ);
	cobj_dictionary_write(dict, ">",      0, TOKEN_IF_MORE);
	cobj_dictionary_write(dict, ">=",     0, TOKEN_IF_MORE_EQ);
	cobj_dictionary_write(dict, "==",     0, TOKEN_IF_EQ);
	cobj_dictionary_write(dict, "!=",     0, TOKEN_IF_EQ_NOT);
	cobj_dictionary_write(dict, "STREQ",  0, TOKEN_IF_STR_EQ);
	
	cobj_dictionary_write(dict, "TIME",   0, TOKEN_TIMESTAMP);
	cobj_dictionary_write(dict, "PI",     0, TOKEN_CONST_PI);
	cobj_dictionary_write(dict, "E",      0, TOKEN_CONST_EULER);
	cobj_dictionary_write(dict, "TRUE",   0, TOKEN_CONST_TRUE);
	cobj_dictionary_write(dict, "FALSE",  0, TOKEN_CONST_FALSE);

	cobj_dictionary_write(dict, "SQRT",   0, TOKEN_OP_SQRT);
	cobj_dictionary_write(dict, "CBRT",   0, TOKEN_OP_CBRT);
	cobj_dictionary_write(dict, "ABS",    0, TOKEN_OP_ABS);
	cobj_dictionary_write(dict, "CEIL",   0, TOKEN_OP_CEILING);
	cobj_dictionary_write(dict, "FLOOR",  0, TOKEN_OP_FLOOR);
	cobj_dictionary_write(dict, "ROUND",  0, TOKEN_OP_ROUND);
	cobj_dictionary_write(dict, "COS",    0, TOKEN_OP_COS);
	cobj_dictionary_write(dict, "SIN",    0, TOKEN_OP_SIN);
	cobj_dictionary_write(dict, "TAN",    0, TOKEN_OP_TAN);
	cobj_dictionary_write(dict, "ACOS",   0, TOKEN_OP_ACOS);
	cobj_dictionary_write(dict, "ASIN",   0, TOKEN_OP_ASIN);
	cobj_dictionary_write(dict, "ATAN",   0, TOKEN_OP_ATAN);
	cobj_dictionary_write(dict, "COSH",   0, TOKEN_OP_COSH);
	cobj_dictionary_write(dict, "SINH",   0, TOKEN_OP_SINH);
	cobj_dictionary_write(dict, "LN",     0, TOKEN_OP_LN);
	cobj_dictionary_write(dict, "LOG",    0, TOKEN_OP_LOG);

	cobj_dictionary_write(dict, "+",      0, TOKEN_OP_ADD);
	cobj_dictionary_write(dict, "-",      0, TOKEN_OP_SUBSTRACT);
	cobj_dictionary_write(dict, "*",      0, TOKEN_OP_MULTIPLY);
	cobj_dictionary_write(dict, "/",      0, TOKEN_OP_DIVIDE);
	cobj_dictionary_write(dict, "MOD",    0, TOKEN_OP_MOD);
	cobj_dictionary_write(dict, "POW",    0, TOKEN_OP_POW);
	cobj_dictionary_write(dict, "BIG",    0, TOKEN_OP_BIGGEST);
	cobj_dictionary_write(dict, "SMALL",  0, TOKEN_OP_SMALLEST);
	cobj_dictionary_write(dict, "RAND",   0, TOKEN_OP_RANDOM);
	
	cobj_dictionary_write(dict, "ITRPL",  0, TOKEN_OP_INTERPOLATE);
	cobj_dictionary_write(dict, "LIMIT",  0, TOKEN_OP_LIMIT);
	
	cobj_dictionary_write(dict, "CITRPL", 0, TOKEN_CL_INTERPOLATE);
	cobj_dictionary_write(dict, "RGB",    0, TOKEN_CL_RGB);
	cobj_dictionary_write(dict, "RGBA",   0, TOKEN_CL_RGBA);

	/* lead tokens */

	cobj_dictionary_write(dict, "LET",           0, TOKEN_VAR_DECLARATION);
	cobj_dictionary_write(dict, "LET_APPEND",    0, TOKEN_VAR_APPEND);
	cobj_dictionary_write(dict, "LET_PREPEND",   0, TOKEN_VAR_PREPEND);
	cobj_dictionary_write(dict, "LET_MERGE",     0, TOKEN_VAR_MERGE);
	cobj_dictionary_write(dict, "LET_ENUM",      0, TOKEN_ENUM_DECLARATION);
	cobj_dictionary_write(dict, "SECTION",       0, TOKEN_SECTION_BEGIN);
	cobj_dictionary_write(dict, "SECTION_ADD",   0, TOKEN_SECTION_ADD);
	cobj_dictionary_write(dict, "SECTION_DEL",   0, TOKEN_SECTION_DEL);
	cobj_dictionary_write(dict, "INCLUDE",       0, TOKEN_INCLUDE);
	cobj_dictionary_write(dict, "ITERATE",       0, TOKEN_ITERATE);
	cobj_dictionary_write(dict, "ITERATE_RAW",   0, TOKEN_ITERATE_RAW);
	cobj_dictionary_write(dict, "SEED_OVERRIDE", 0, TOKEN_RAND_SEED);
	cobj_dictionary_write(dict, "DEBUG_PRINT",   0, TOKEN_PRINT);

	return dict;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

token_kind_t
token_match(cobj_dictionary_t *token_dict, const char *token)
{
	size_t id;

	if (!cobj_dictionary_find(token_dict, token, 0, &id))
	{
		return TOKEN_STRING;
	}

	return id;
}
