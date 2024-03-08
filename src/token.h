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

#ifndef DR_TOKEN_PRIVATE_H
#define DR_TOKEN_PRIVATE_H

#include <derelict/du.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DR_TOKEN_N 32

/**
 *
 */
typedef enum {
	/* special tokens */
	DR_TOKEN_INVALID = 0,
	DR_TOKEN_STRING,
	DR_TOKEN_NUMBER,
	/* universal tokens */
	DR_TOKEN_EOF,
	DR_TOKEN_COMMENT,
	DR_TOKEN_FILLER,
	DR_TOKEN_JOIN,
	DR_TOKEN_ESCAPE,
	DR_TOKEN_VAR_INJECTION,
	DR_TOKEN_ITER_INJECTION,
	DR_TOKEN_IF_LESS,
	DR_TOKEN_IF_LESS_EQ,
	DR_TOKEN_IF_MORE,
	DR_TOKEN_IF_MORE_EQ,
	DR_TOKEN_IF_EQ,
	DR_TOKEN_IF_EQ_NOT,
	DR_TOKEN_TIMESTAMP,
	DR_TOKEN_CONST_PI,
	DR_TOKEN_CONST_EULER,
	DR_TOKEN_CONST_TRUE,
	DR_TOKEN_CONST_FALSE,
	DR_TOKEN_OP_SQRT,
	DR_TOKEN_OP_CBRT,
	DR_TOKEN_OP_ABS,
	DR_TOKEN_OP_CEILING,
	DR_TOKEN_OP_FLOOR,
	DR_TOKEN_OP_COS,
	DR_TOKEN_OP_SIN,
	DR_TOKEN_OP_TAN,
	DR_TOKEN_OP_ACOS,
	DR_TOKEN_OP_ASIN,
	DR_TOKEN_OP_ATAN,
	DR_TOKEN_OP_COSH,
	DR_TOKEN_OP_SINH,
	DR_TOKEN_OP_LN,
	DR_TOKEN_OP_LOG,
	DR_TOKEN_OP_ADD,
	DR_TOKEN_OP_SUBSTRACT,
	DR_TOKEN_OP_MULTIPLY,
	DR_TOKEN_OP_DIVIDE,
	DR_TOKEN_OP_MOD,
	DR_TOKEN_OP_POW,
	DR_TOKEN_OP_BIGGEST,
	DR_TOKEN_OP_SMALLEST,
	DR_TOKEN_OP_RANDOM,
	DR_TOKEN_OP_LIMIT,
	DR_TOKEN_OP_INTERPOLATE,
	DR_TOKEN_CL_INTERPOLATE,
	DR_TOKEN_CL_RGB,
	DR_TOKEN_CL_RGBA,
	/* lead tokens */
	DR_TOKEN_VAR_DECLARATION,
	DR_TOKEN_SECTION_BEGIN,
	DR_TOKEN_SECTION_ADD,
	DR_TOKEN_SECTION_DEL,
	DR_TOKEN_INCLUDE,
	DR_TOKEN_ITERATOR,
	DR_TOKEN_RAND_SEED,
} dr_token_kind_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void dr_token_init_dictionary(du_dictionary_t *dict);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DR_TOKEN_PRIVATE_H */
