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

#pragma once

#include <cassette/ccfg.h>
#include <cassette/cobj.h>
#include <limits.h>

#include "attributes.h"

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

enum token
{
	/* special tokens */

	TOKEN_INVALID = 0,
	TOKEN_STRING,
	TOKEN_NUMBER,

	/* substitution tokens */

	TOKEN_EOF,
	TOKEN_COMMENT,
	TOKEN_FILLER,
	TOKEN_JOIN,
	TOKEN_ESCAPE,
	TOKEN_VAR_INJECTION,
	TOKEN_ITER_INJECTION,
	TOKEN_PARAM_INJECTION,
	TOKEN_IF_LESS,
	TOKEN_IF_LESS_EQ,
	TOKEN_IF_MORE,
	TOKEN_IF_MORE_EQ,
	TOKEN_IF_EQ,
	TOKEN_IF_EQ_NOT,
	TOKEN_IF_STR_EQ,
	TOKEN_TIMESTAMP,
	TOKEN_CONST_PI,
	TOKEN_CONST_EULER,
	TOKEN_CONST_TRUE,
	TOKEN_CONST_FALSE,
	TOKEN_OP_SQRT,
	TOKEN_OP_CBRT,
	TOKEN_OP_ABS,
	TOKEN_OP_CEILING,
	TOKEN_OP_FLOOR,
	TOKEN_OP_ROUND,
	TOKEN_OP_COS,
	TOKEN_OP_SIN,
	TOKEN_OP_TAN,
	TOKEN_OP_ACOS,
	TOKEN_OP_ASIN,
	TOKEN_OP_ATAN,
	TOKEN_OP_COSH,
	TOKEN_OP_SINH,
	TOKEN_OP_LN,
	TOKEN_OP_LOG,
	TOKEN_OP_ADD,
	TOKEN_OP_SUBSTRACT,
	TOKEN_OP_MULTIPLY,
	TOKEN_OP_DIVIDE,
	TOKEN_OP_MOD,
	TOKEN_OP_POW,
	TOKEN_OP_BIGGEST,
	TOKEN_OP_SMALLEST,
	TOKEN_OP_RANDOM,
	TOKEN_OP_LIMIT,
	TOKEN_OP_INTERPOLATE,
	TOKEN_CL_INTERPOLATE,
	TOKEN_CL_RGB,
	TOKEN_CL_RGBA,

	/* lead tokens */

	TOKEN_VAR_DECLARATION,
	TOKEN_VAR_APPEND,
	TOKEN_VAR_PREPEND,
	TOKEN_VAR_MERGE,
	TOKEN_ENUM_DECLARATION,
	TOKEN_SECTION_BEGIN,
	TOKEN_SECTION_ADD,
	TOKEN_SECTION_DEL,
	TOKEN_INCLUDE,
	TOKEN_SEED,
	TOKEN_PRINT,
	TOKEN_FOR_BEGIN,
	TOKEN_FOR_END,
};

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

#define TOKEN_MAX_LEN 256

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

cdict *
token_dict_create(void);

/************************************************************************************************************/
/* FUNCTION *************************************************************************************************/
/************************************************************************************************************/

enum token
token_match(cdict *token_dict, const char *str)
PURE;
