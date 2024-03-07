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

#include <assert.h>
#include <stdlib.h>

#include <derelict/du.h>

#include "dr.h"
#include "token.h"

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
dr_token_init_dictionary(du_dictionary_t *dict)
{
	assert(dict);

	du_dictionary_init(dict, 50, 0.6);

	/* universal tokens */

	du_dictionary_set_value(dict, "EOF",    0, DR_TOKEN_EOF);
	du_dictionary_set_value(dict, "",       0, DR_TOKEN_INVALID);
	du_dictionary_set_value(dict, "//",     0, DR_TOKEN_COMMENT);
	du_dictionary_set_value(dict, "(",      0, DR_TOKEN_FILLER);
	du_dictionary_set_value(dict, ")",      0, DR_TOKEN_FILLER);
	du_dictionary_set_value(dict, "=",      0, DR_TOKEN_FILLER);
	du_dictionary_set_value(dict, ":=",     0, DR_TOKEN_FILLER);
	du_dictionary_set_value(dict, "JOIN",   0, DR_TOKEN_JOIN);
	du_dictionary_set_value(dict, "\\",     0, DR_TOKEN_ESCAPE);
	du_dictionary_set_value(dict, "$",      0, DR_TOKEN_VAR_INJECTION);
	du_dictionary_set_value(dict, "%",      0, DR_TOKEN_ITER_INJECTION);

	du_dictionary_set_value(dict, "<",      0, DR_TOKEN_IF_LESS);
	du_dictionary_set_value(dict, "<=",     0, DR_TOKEN_IF_LESS_EQ);
	du_dictionary_set_value(dict, ">",      0, DR_TOKEN_IF_MORE);
	du_dictionary_set_value(dict, ">=",     0, DR_TOKEN_IF_MORE_EQ);
	du_dictionary_set_value(dict, "==",     0, DR_TOKEN_IF_EQ);
	du_dictionary_set_value(dict, "!=",     0, DR_TOKEN_IF_EQ_NOT);
	
	du_dictionary_set_value(dict, "PI",     0, DR_TOKEN_CONST_PI);
	du_dictionary_set_value(dict, "E",      0, DR_TOKEN_CONST_EULER);
	du_dictionary_set_value(dict, "TRUE",   0, DR_TOKEN_CONST_TRUE);
	du_dictionary_set_value(dict, "FALSE",  0, DR_TOKEN_CONST_FALSE);

	du_dictionary_set_value(dict, "SQRT",   0, DR_TOKEN_OP_SQRT);
	du_dictionary_set_value(dict, "CBRT",   0, DR_TOKEN_OP_CBRT);
	du_dictionary_set_value(dict, "ABS",    0, DR_TOKEN_OP_ABS);
	du_dictionary_set_value(dict, "CEIL",   0, DR_TOKEN_OP_CEILING);
	du_dictionary_set_value(dict, "FLOOR",  0, DR_TOKEN_OP_FLOOR);
	du_dictionary_set_value(dict, "COS",    0, DR_TOKEN_OP_COS);
	du_dictionary_set_value(dict, "SIN",    0, DR_TOKEN_OP_SIN);
	du_dictionary_set_value(dict, "TAN",    0, DR_TOKEN_OP_TAN);
	du_dictionary_set_value(dict, "ACOS",   0, DR_TOKEN_OP_ACOS);
	du_dictionary_set_value(dict, "ASIN",   0, DR_TOKEN_OP_ASIN);
	du_dictionary_set_value(dict, "ATAN",   0, DR_TOKEN_OP_ATAN);
	du_dictionary_set_value(dict, "COSH",   0, DR_TOKEN_OP_COSH);
	du_dictionary_set_value(dict, "SINH",   0, DR_TOKEN_OP_SINH);
	du_dictionary_set_value(dict, "LN",     0, DR_TOKEN_OP_LN);
	du_dictionary_set_value(dict, "LOG",    0, DR_TOKEN_OP_LOG);

	du_dictionary_set_value(dict, "+",      0, DR_TOKEN_OP_ADD);
	du_dictionary_set_value(dict, "-",      0, DR_TOKEN_OP_SUBSTRACT);
	du_dictionary_set_value(dict, "*",      0, DR_TOKEN_OP_MULTIPLY);
	du_dictionary_set_value(dict, "/",      0, DR_TOKEN_OP_DIVIDE);
	du_dictionary_set_value(dict, "MOD",    0, DR_TOKEN_OP_MOD);
	du_dictionary_set_value(dict, "POW",    0, DR_TOKEN_OP_POW);
	du_dictionary_set_value(dict, "BIG",    0, DR_TOKEN_OP_BIGGEST);
	du_dictionary_set_value(dict, "SMALL",  0, DR_TOKEN_OP_SMALLEST);
	du_dictionary_set_value(dict, "RAND",   0, DR_TOKEN_OP_RANDOM);
	
	du_dictionary_set_value(dict, "ITRPL",  0, DR_TOKEN_OP_INTERPOLATE);
	du_dictionary_set_value(dict, "LIMIT",  0, DR_TOKEN_OP_LIMIT);
	
	du_dictionary_set_value(dict, "CITRPL", 0, DR_TOKEN_CL_INTERPOLATE);
	du_dictionary_set_value(dict, "RGB",    0, DR_TOKEN_CL_RGB);
	du_dictionary_set_value(dict, "RGBA",   0, DR_TOKEN_CL_RGBA);

	/* lead tokens */

	du_dictionary_set_value(dict, "LET",         0, DR_TOKEN_VAR_DECLARATION);
	du_dictionary_set_value(dict, "SECTION",     0, DR_TOKEN_SECTION_BEGIN);
	du_dictionary_set_value(dict, "SECTION_ADD", 0, DR_TOKEN_SECTION_ADD);
	du_dictionary_set_value(dict, "SECTION_DEL", 0, DR_TOKEN_SECTION_DEL);
	du_dictionary_set_value(dict, "INCLUDE",     0, DR_TOKEN_INCLUDE);
	du_dictionary_set_value(dict, "ITERATE",     0, DR_TOKEN_ITERATOR);
}
