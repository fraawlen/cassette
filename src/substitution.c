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

#include <math.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <cassette/cobj.h>

#include "context.h"
#include "token.h"
#include "util.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static token_kind_t _comment  (void);
static token_kind_t _eof      (context_t *ctx);
static token_kind_t _escape   (context_t *ctx, char token[static TOKEN_N]);
static token_kind_t _filler   (context_t *ctx, char token[static TOKEN_N], double *math_result);
static token_kind_t _if       (context_t *ctx, char token[static TOKEN_N], double *math_result, token_kind_t type);
static token_kind_t _join     (context_t *ctx, char token[static TOKEN_N]);
static token_kind_t _math     (context_t *ctx, char token[static TOKEN_N], double *math_result, token_kind_t type, size_t n);
static token_kind_t _math_cl  (context_t *ctx, char token[static TOKEN_N], double *math_result, token_kind_t type, size_t n);
static token_kind_t _param    (context_t *ctx, char token[static TOKEN_N]);
static token_kind_t _variable (context_t *ctx, char token[static TOKEN_N], double *math_result);

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

token_kind_t
substitution_apply(context_t *ctx, char token[static TOKEN_N], double *math_result)
{
	token_kind_t type;

	if (ctx->depth >= CONTEXT_MAX_DEPTH)
	{
		return TOKEN_INVALID;
	}
	else
	{
		ctx->depth++;
	}
	
	switch (type = token_match(ctx->tokens, token))
	{
		case TOKEN_COMMENT:
			type = _comment();
			break;

		case TOKEN_EOF:
			type = _eof(ctx);
			break;

		case TOKEN_ESCAPE:
			type = _escape(ctx, token);
			break;

		case TOKEN_FILLER:
			type = _filler(ctx, token, math_result);
			break;

		case TOKEN_JOIN:
			type = _join(ctx, token);
			break;

		case TOKEN_VAR_INJECTION:
			type = _variable(ctx, token, math_result);
			break;

		case TOKEN_PARAM_INJECTION:
			type = _param(ctx, token);
			break;

		case TOKEN_IF_LESS:
		case TOKEN_IF_LESS_EQ:
		case TOKEN_IF_MORE:
		case TOKEN_IF_MORE_EQ:
		case TOKEN_IF_EQ:
		case TOKEN_IF_EQ_NOT:
			type = _if(ctx, token, math_result, type);
			break;

		case TOKEN_TIMESTAMP:
		case TOKEN_CONST_PI:
		case TOKEN_CONST_EULER:
		case TOKEN_CONST_TRUE:
		case TOKEN_CONST_FALSE:
			type = _math(ctx, token, math_result, type, 0);
			break;

		case TOKEN_OP_SQRT:
		case TOKEN_OP_CBRT:
		case TOKEN_OP_ABS:
		case TOKEN_OP_CEILING:
		case TOKEN_OP_FLOOR:
		case TOKEN_OP_ROUND:
		case TOKEN_OP_COS:
		case TOKEN_OP_SIN:
		case TOKEN_OP_TAN:
		case TOKEN_OP_ACOS:
		case TOKEN_OP_ASIN:
		case TOKEN_OP_ATAN:
		case TOKEN_OP_COSH:
		case TOKEN_OP_SINH:
		case TOKEN_OP_LN:
		case TOKEN_OP_LOG:
			type = _math(ctx, token, math_result, type, 1);
			break;

		case TOKEN_OP_ADD:
		case TOKEN_OP_SUBSTRACT:
		case TOKEN_OP_MULTIPLY:
		case TOKEN_OP_DIVIDE:
		case TOKEN_OP_MOD:
		case TOKEN_OP_POW:
		case TOKEN_OP_BIGGEST:
		case TOKEN_OP_SMALLEST:
		case TOKEN_OP_RANDOM:
			type = _math(ctx, token, math_result, type, 2);
			break;

		case TOKEN_OP_LIMIT:
		case TOKEN_OP_INTERPOLATE:
			type = _math(ctx, token, math_result, type, 3);
			break;

		case TOKEN_CL_RGB:
		case TOKEN_CL_INTERPOLATE:
			type = _math_cl(ctx, token, math_result, type, 3);
			break;

		case TOKEN_CL_RGBA:
			type = _math_cl(ctx, token, math_result, type, 4);
			break;

		default:
			break;
	}

	ctx->depth--;

	return type;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static token_kind_t
_comment(void)
{
	return TOKEN_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static token_kind_t
_eof(context_t *ctx)
{
	ctx->eof_reached = true;
	ctx->eol_reached = true;

	return TOKEN_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static token_kind_t
_escape(context_t *ctx, char token[TOKEN_N])
{
	ctx->eol_reached = false;

	return context_get_token_raw(ctx, token);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static token_kind_t
_filler(context_t *ctx, char token[static TOKEN_N], double *math_result)
{
	return context_get_token(ctx, token, math_result);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static token_kind_t
_if(context_t *ctx, char token[static TOKEN_N], double *math_result, token_kind_t type)
{
	char token_2[TOKEN_N];
	bool result;
	double a;
	double b;

	/* get values to compare */

	if (context_get_token_numeral(ctx, token, &a) == TOKEN_INVALID ||
	    context_get_token_numeral(ctx, token, &b) == TOKEN_INVALID)
	{
		return TOKEN_INVALID;	
	}
	
	/* execute comparison */

	switch (type)
	{
		case TOKEN_IF_LESS:
			result = a < b;
			break;

		case TOKEN_IF_LESS_EQ:
			result = a <= b;
			break;

		case TOKEN_IF_MORE:
			result = a > b;
			break;

		case TOKEN_IF_MORE_EQ:
			result = a >= b;
			break;

		case TOKEN_IF_EQ:
			result = a == b;
			break;

		case TOKEN_IF_EQ_NOT:
			result = a != b;
			break;

		default:
			return TOKEN_INVALID;	
	}

	/* get resulting token */

	type = context_get_token(ctx, token, math_result);

	if (result)
	{
		context_get_token(ctx, token_2, NULL);
		return type;
	}
	else
	{
		return context_get_token(ctx, token, math_result);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static token_kind_t
_join(context_t *ctx, char token[static TOKEN_N])
{
	char token_a[TOKEN_N];
	char token_b[TOKEN_N];

	if (context_get_token(ctx, token_a, NULL) == TOKEN_INVALID ||
	    context_get_token(ctx, token_b, NULL) == TOKEN_INVALID) {
		return TOKEN_INVALID;
	}

	if (snprintf(token, TOKEN_N, "%s%s", token_a, token_b) < 0)
	{
		return TOKEN_INVALID;
	}

	return TOKEN_STRING;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static token_kind_t
_math(context_t *ctx, char token[static TOKEN_N], double *math_result, token_kind_t type, size_t n)
{
	double result;
	double d[3] = {0};

	/* get next few numeral tokens as math functions arguments */

	for (size_t i = 0; i < n; i++)
	{
		if (context_get_token_numeral(ctx, token, d + i) == TOKEN_INVALID)
		{
			return TOKEN_INVALID;
		}
	}

	/* apply math operation */

	switch (type)
	{
		/* 0 parameters */

		case TOKEN_TIMESTAMP:
			result = time(NULL);
			break;

		case TOKEN_CONST_PI:
			result = 3.1415926535897932;
			break;

		case TOKEN_CONST_EULER:
			result = 0.5772156649015328;
			break;

		case TOKEN_CONST_TRUE:
			result = 1.0;
			break;

		case TOKEN_CONST_FALSE:
			result = 0.0;
			break;

		/* 1 parameters */

		case TOKEN_OP_SQRT:
			result = sqrt(d[0]);
			break;

		case TOKEN_OP_CBRT:
			result = cbrt(d[0]);
			break;

		case TOKEN_OP_ABS:
			result = fabs(d[0]);
			break;

		case TOKEN_OP_CEILING:
			result = ceil(d[0]);
			break;

		case TOKEN_OP_FLOOR:
			result = floor(d[0]);
			break;

		case TOKEN_OP_ROUND:
			result = round(d[0]);
			break;

		case TOKEN_OP_COS:
			result = cos(d[0]);
			break;

		case TOKEN_OP_SIN:
			result = sin(d[0]);
			break;

		case TOKEN_OP_TAN:
			result = tan(d[0]);
			break;

		case TOKEN_OP_ACOS:
			result = acos(d[0]);
			break;

		case TOKEN_OP_ASIN:
			result = asin(d[0]);
			break;

		case TOKEN_OP_ATAN:
			result = atan(d[0]);
			break;

		case TOKEN_OP_COSH:
			result = cosh(d[0]);
			break;

		case TOKEN_OP_SINH:
			result = sinh(d[0]);
			break;

		case TOKEN_OP_LN:
			result = log(d[0]);
			break;

		case TOKEN_OP_LOG:
			result = log10(d[0]);
			break;

		/* 2 parameters */

		case TOKEN_OP_ADD:
			result = d[0] + d[1];
			break;
		
		case TOKEN_OP_SUBSTRACT:
			result = d[0] - d[1];
			break;
		
		case TOKEN_OP_MULTIPLY:
			result = d[0] * d[1];
			break;
		
		case TOKEN_OP_DIVIDE:
			result = d[0] / d[1];
			break;
		
		case TOKEN_OP_MOD:
			result = fmod(d[0], d[1]);
			break;
		
		case TOKEN_OP_POW:
			result = pow(d[0], d[1]);
			break;
		
		case TOKEN_OP_BIGGEST:
			result = d[0] > d[1] ? d[0] : d[1];
			break;
		
		case TOKEN_OP_SMALLEST:
			result = d[0] < d[1] ? d[0] : d[1];
			break;

		case TOKEN_OP_RANDOM:
			result = cobj_rand_get(ctx->rand, d[0], d[1]);
			break;

		/* 3 parameters */

		case TOKEN_OP_INTERPOLATE:
			result = util_interpolate(d[0], d[1], d[2]);
			break;

		case TOKEN_OP_LIMIT:
			result = util_limit(d[0], d[1], d[2]);
			break;

		/* other */

		default:
			return TOKEN_INVALID;
	}

	/* if needed convert back the result into a string */

	if (math_result)
	{
		*math_result = result;
	}
	else
	{
		snprintf(token, TOKEN_N, "%.8f", result);
	}

	return TOKEN_NUMBER;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static token_kind_t
_math_cl(context_t *ctx, char token[static TOKEN_N], double *math_result, token_kind_t type, size_t n)
{
	cobj_color_t result;
	cobj_color_t cl[4] = {0};

	double d[4] = {0};

	/* get next few numeral tokens as math functions arguments and convert them into colors */

	for (size_t i = 0; i < n; i++)
	{
		if (context_get_token_numeral(ctx, token, d + i) == TOKEN_INVALID)
		{
			return TOKEN_INVALID;
		}
		cl[i] = cobj_color_convert_argb_uint(d[i]);
	}

	/* apply math operation */

	switch (type)
	{
		/* 3 parameters */

		case TOKEN_CL_RGB:
			result = cobj_color_convert_rgba(d[0], d[1], d[2], 255);
			break;

		case TOKEN_CL_INTERPOLATE:
			result = cobj_color_interpolate(cl[0], cl[1], d[2]);
			break;
		
		/* 4 parameters */

		case TOKEN_CL_RGBA:
			result = cobj_color_convert_rgba(d[0], d[1], d[2], d[3]);
			break;

		/* other */

		default:
			return TOKEN_INVALID;
	}

	/* if needed convert back the result into a string */

	if (math_result)
	{
		*math_result = cobj_color_get_argb_uint(result);
	}
	else
	{
		snprintf(token, TOKEN_N, "%u", cobj_color_get_argb_uint(result));
	}

	return TOKEN_NUMBER;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static token_kind_t
_param(context_t *ctx, char token[static TOKEN_N])
{
	size_t i; 

	if (context_get_token(ctx, token, NULL) == TOKEN_INVALID)
	{
		return TOKEN_INVALID;
	}

	if (!cobj_dictionary_find(ctx->ref_params, token, 0, &i))
	{
		return TOKEN_INVALID;
	}

	snprintf(token, TOKEN_N, "%s", cobj_book_get_word(ctx->params, 0, i));
	
	return TOKEN_STRING;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static token_kind_t
_variable(context_t *ctx, char token[static TOKEN_N], double *math_result)
{
	size_t i;

	if (context_get_token(ctx, token, NULL) == TOKEN_INVALID)
	{
		return TOKEN_INVALID;
	}

	if (!cobj_dictionary_find(ctx->ref_variables, token, CONTEXT_DICT_VARIABLE, &i))
	{
		return TOKEN_INVALID;
	}

	cobj_book_reset_iterator(ctx->variables, i);

	return context_get_token(ctx, token, math_result);
}
