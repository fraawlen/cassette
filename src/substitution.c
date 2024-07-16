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

#include <cassette/ccfg.h>
#include <cassette/cobj.h>
#include <float.h>
#include <math.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "context.h"
#include "substitution.h"
#include "token.h"
#include "util.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static enum token _comment       (void);
static enum token _eof           (struct context *)                                                            CCFG_NONNULL(1);
static enum token _escape        (struct context *, char [static TOKEN_MAX_LEN])                               CCFG_NONNULL(1);
static enum token _filler        (struct context *, char [static TOKEN_MAX_LEN], double *)                     CCFG_NONNULL(1);
static enum token _if            (struct context *, char [static TOKEN_MAX_LEN], double *, enum token)         CCFG_NONNULL(1);
static enum token _join          (struct context *, char [static TOKEN_MAX_LEN])                               CCFG_NONNULL(1);
static enum token _math          (struct context *, char [static TOKEN_MAX_LEN], double *, enum token, size_t) CCFG_NONNULL(1);
static enum token _math_cl       (struct context *, char [static TOKEN_MAX_LEN], double *, enum token, size_t) CCFG_NONNULL(1);
static enum token _param         (struct context *, char [static TOKEN_MAX_LEN])                               CCFG_NONNULL(1);
static enum token _variable      (struct context *, char [static TOKEN_MAX_LEN], double *)                     CCFG_NONNULL(1);
static enum token _variable_iter (struct context *, char [static TOKEN_MAX_LEN])                               CCFG_NONNULL(1);

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

enum token
substitution_apply(struct context *ctx, char token[static TOKEN_MAX_LEN], double *math_result)
{
	enum token type;

	if (ctx->depth >= CONTEXT_MAX_DEPTH)
	{
		return TOKEN_INVALID;
	}
	
	ctx->depth++;
	
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

		case TOKEN_ITER_INJECTION:
			type = _variable_iter(ctx, token);
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
		case TOKEN_IF_STR_EQ:
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

static enum token
_comment(void)
{
	return TOKEN_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static enum token
_eof(struct context *ctx)
{
	ctx->eof_reached = true;
	ctx->eol_reached = true;

	return TOKEN_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static enum token
_escape(struct context *ctx, char token[TOKEN_MAX_LEN])
{
	ctx->eol_reached = false;

	return context_get_token_raw(ctx, token);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static enum token
_filler(struct context *ctx, char token[static TOKEN_MAX_LEN], double *math_result)
{
	return context_get_token(ctx, token, math_result);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static enum token
_if(struct context *ctx, char token[static TOKEN_MAX_LEN], double *math_result, enum token type)
{
	char token_2[TOKEN_MAX_LEN];
	bool result;
	double a;
	double b;

	/* get values to compare */

	if (context_get_token_numeral(ctx, token,   &a) == TOKEN_INVALID
	 || context_get_token_numeral(ctx, token_2, &b) == TOKEN_INVALID)
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
			result = fabs(a - b) < DBL_EPSILON;
			break;

		case TOKEN_IF_EQ_NOT:
			result = fabs(a - b) >= DBL_EPSILON;
			break;

		case TOKEN_IF_STR_EQ:
			result = !strcmp(token, token_2);
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

static enum token
_join(struct context *ctx, char token[static TOKEN_MAX_LEN])
{
	char token_a[TOKEN_MAX_LEN];
	char token_b[TOKEN_MAX_LEN];

	if (context_get_token(ctx, token_a, NULL) == TOKEN_INVALID
	 || context_get_token(ctx, token_b, NULL) == TOKEN_INVALID
	 || snprintf(token, TOKEN_MAX_LEN, "%s%s", token_a, token_b) < 0)
	{
		return TOKEN_INVALID;
	}

	return TOKEN_STRING;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static enum token
_math(struct context *ctx, char token[static TOKEN_MAX_LEN], double *math_result, enum token type, size_t n)
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
			result = crand_get(ctx->rand, d[0], d[1]);
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
		snprintf(token, TOKEN_MAX_LEN, "%.8f", result);
	}

	return TOKEN_NUMBER;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static enum token
_math_cl(struct context *ctx, char token[static TOKEN_MAX_LEN], double *math_result, enum token type, size_t n)
{
	struct ccolor result;
	struct ccolor cl[4] = {0};

	double d[4] = {0};

	/* get next few numeral tokens as math functions arguments and convert them into colors */

	for (size_t i = 0; i < n; i++)
	{
		if (context_get_token_numeral(ctx, token, d + i) == TOKEN_INVALID)
		{
			return TOKEN_INVALID;
		}
		cl[i] = ccolor_from_argb_uint(d[i]);
	}

	/* apply math operation */

	switch (type)
	{
		/* 3 parameters */

		case TOKEN_CL_RGB:
			result = ccolor_from_rgba(d[0], d[1], d[2], 255);
			break;

		case TOKEN_CL_INTERPOLATE:
			result = ccolor_interpolate(cl[0], cl[1], d[2]);
			break;
		
		/* 4 parameters */

		case TOKEN_CL_RGBA:
			result = ccolor_from_rgba(d[0], d[1], d[2], d[3]);
			break;

		/* other */

		default:
			return TOKEN_INVALID;
	}

	/* if needed convert back the result into a string */

	if (math_result)
	{
		*math_result = ccolor_to_argb_uint(result);
	}
	else
	{
		snprintf(token, TOKEN_MAX_LEN, "%u", ccolor_to_argb_uint(result));
	}

	return TOKEN_NUMBER;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static enum token
_param(struct context *ctx, char token[static TOKEN_MAX_LEN])
{
	size_t i; 

	if (context_get_token(ctx, token, NULL) == TOKEN_INVALID
	 || !cdict_find(ctx->keys_params, token, 0, &i))
	{
		return TOKEN_INVALID;
	}

	snprintf(token, TOKEN_MAX_LEN, "%s", cbook_word(ctx->params, i));
	
	return TOKEN_STRING;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static enum token
_variable(struct context *ctx, char token[static TOKEN_MAX_LEN], double *math_result)
{
	if (context_get_token(ctx, token, NULL) == TOKEN_INVALID
	 || !cdict_find(ctx->keys_vars, token, CONTEXT_DICT_VARIABLE, &ctx->var_group))
	{
		return TOKEN_INVALID;
	}

	ctx->var_i = 0;

	return context_get_token(ctx, token, math_result);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static enum token
_variable_iter(struct context *ctx, char token[static TOKEN_MAX_LEN])
{
	size_t i;

	if (context_get_token(ctx, token, NULL) == TOKEN_INVALID
	 || !cdict_find(ctx->keys_vars, token, CONTEXT_DICT_ITERATION, &i))
	{
		return TOKEN_INVALID;
	}

	snprintf(token, TOKEN_MAX_LEN, "%s", cbook_word(ctx->vars, i));

	return TOKEN_STRING;
}
