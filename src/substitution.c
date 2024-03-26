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
#include <math.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <derelict/do.h>

#include "context.h"
#include "token.h"
#include "util.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static dr_token_kind_t _comment (void);
static dr_token_kind_t _eof     (dr_context_t *ctx);
static dr_token_kind_t _escape  (dr_context_t *ctx, char token[DR_TOKEN_N]);
static dr_token_kind_t _filler  (dr_context_t *ctx, char token[DR_TOKEN_N], double *math_result);
static dr_token_kind_t _join    (dr_context_t *ctx, char token[DR_TOKEN_N]);
static dr_token_kind_t _math    (dr_context_t *ctx, char token[DR_TOKEN_N], double *math_result, dr_token_kind_t type, size_t n);

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

dr_token_kind_t
dr_subtitution_apply(dr_context_t *ctx, char token[DR_TOKEN_N], double *math_result)
{
	dr_token_kind_t type;

	assert(ctx && token);
	
	switch (type = dr_token_match(ctx->tokens, token))
	{
		// TODO substitution handlers

		case DR_TOKEN_COMMENT:
			return _comment();

		case DR_TOKEN_EOF:
			return _eof(ctx);

		case DR_TOKEN_ESCAPE:
			return _escape(ctx, token);

		case DR_TOKEN_FILLER:
			return _filler(ctx, token, math_result);

		case DR_TOKEN_JOIN:
			return _join(ctx, token);

		case DR_TOKEN_TIMESTAMP:
		case DR_TOKEN_CONST_PI:
		case DR_TOKEN_CONST_EULER:
		case DR_TOKEN_CONST_TRUE:
		case DR_TOKEN_CONST_FALSE:
			return _math(ctx, token, math_result, type, 0);

		case DR_TOKEN_OP_SQRT:
		case DR_TOKEN_OP_CBRT:
		case DR_TOKEN_OP_ABS:
		case DR_TOKEN_OP_CEILING:
		case DR_TOKEN_OP_FLOOR:
		case DR_TOKEN_OP_COS:
		case DR_TOKEN_OP_SIN:
		case DR_TOKEN_OP_TAN:
		case DR_TOKEN_OP_ACOS:
		case DR_TOKEN_OP_ASIN:
		case DR_TOKEN_OP_ATAN:
		case DR_TOKEN_OP_COSH:
		case DR_TOKEN_OP_SINH:
		case DR_TOKEN_OP_LN:
		case DR_TOKEN_OP_LOG:
			return _math(ctx, token, math_result, type, 1);

		case DR_TOKEN_OP_ADD:
		case DR_TOKEN_OP_SUBSTRACT:
		case DR_TOKEN_OP_MULTIPLY:
		case DR_TOKEN_OP_DIVIDE:
		case DR_TOKEN_OP_MOD:
		case DR_TOKEN_OP_POW:
		case DR_TOKEN_OP_BIGGEST:
		case DR_TOKEN_OP_SMALLEST:
		case DR_TOKEN_OP_RANDOM:
			return _math(ctx, token, math_result, type, 2);

		case DR_TOKEN_OP_LIMIT:
		case DR_TOKEN_OP_INTERPOLATE:
			return _math(ctx, token, math_result, type, 3);

		default:
			return type;
	}
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static dr_token_kind_t
_comment(void)
{
	return DR_TOKEN_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dr_token_kind_t
_eof(dr_context_t *ctx)
{
	ctx->eof_reached = true;
	ctx->eol_reached = true;

	return DR_TOKEN_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dr_token_kind_t
_escape(dr_context_t *ctx, char token[DR_TOKEN_N])
{
	ctx->eol_reached = false;

	return dr_context_get_token_raw(ctx, token);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dr_token_kind_t
_filler(dr_context_t *ctx, char token[DR_TOKEN_N], double *math_result)
{
	return dr_context_get_token(ctx, token, math_result);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dr_token_kind_t
_join(dr_context_t *ctx, char token[DR_TOKEN_N])
{
	char token_2[DR_TOKEN_N];

	if (dr_context_get_token(ctx, token,   NULL) == DR_TOKEN_INVALID ||
	    dr_context_get_token(ctx, token_2, NULL) == DR_TOKEN_INVALID) {
		return DR_TOKEN_INVALID;
	}

	strncat(token, token_2, DR_TOKEN_N - strlen(token) - 1);
	token[DR_TOKEN_N - 1] = '\0';

	return DR_TOKEN_STRING;

}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dr_token_kind_t
_math(dr_context_t *ctx, char token[DR_TOKEN_N], double *math_result, dr_token_kind_t type, size_t n)
{
	double result;
	double d[3] = {0};

	/* get next few numeral tokens as math functions arguments */

	for (size_t i = 0; i < n; i++)
	{
		if (dr_context_get_token_numeral(ctx, token, d + i) == DR_TOKEN_INVALID)
		{
			return DR_TOKEN_INVALID;
		}
	}

	/* apply math operation */

	switch (type)
	{
		/* 0 parameters */

		case DR_TOKEN_TIMESTAMP:
			result = time(NULL);
			break;

		case DR_TOKEN_CONST_PI:
			result = 3.1415926535897932;
			break;

		case DR_TOKEN_CONST_EULER:
			result = 0.5772156649015328;
			break;

		case DR_TOKEN_CONST_TRUE:
			result = 1.0;
			break;

		case DR_TOKEN_CONST_FALSE:
			result = 0.0;
			break;

		/* 1 parameters */

		case DR_TOKEN_OP_SQRT:
			result = sqrt(d[0]);
			break;

		case DR_TOKEN_OP_CBRT:
			result = cbrt(d[0]);
			break;

		case DR_TOKEN_OP_ABS:
			result = fabs(d[0]);
			break;

		case DR_TOKEN_OP_CEILING:
			result = ceil(d[0]);
			break;

		case DR_TOKEN_OP_FLOOR:
			result = floor(d[0]);
			break;

		case DR_TOKEN_OP_COS:
			result = cos(d[0]);
			break;

		case DR_TOKEN_OP_SIN:
			result = sin(d[0]);
			break;

		case DR_TOKEN_OP_TAN:
			result = tan(d[0]);
			break;

		case DR_TOKEN_OP_ACOS:
			result = acos(d[0]);
			break;

		case DR_TOKEN_OP_ASIN:
			result = asin(d[0]);
			break;

		case DR_TOKEN_OP_ATAN:
			result = atan(d[0]);
			break;

		case DR_TOKEN_OP_COSH:
			result = cosh(d[0]);
			break;

		case DR_TOKEN_OP_SINH:
			result = sinh(d[0]);
			break;

		case DR_TOKEN_OP_LN:
			result = log(d[0]);
			break;

		case DR_TOKEN_OP_LOG:
			result = log10(d[0]);
			break;

		/* 2 parameters */

		case DR_TOKEN_OP_ADD:
			result = d[0] + d[1];
			break;
		
		case DR_TOKEN_OP_SUBSTRACT:
			result = d[0] - d[1];
			break;
		
		case DR_TOKEN_OP_MULTIPLY:
			result = d[0] * d[1];
			break;
		
		case DR_TOKEN_OP_DIVIDE:
			result = d[0] / d[1];
			break;
		
		case DR_TOKEN_OP_MOD:
			result = fmod(d[0], d[1]);
			break;
		
		case DR_TOKEN_OP_POW:
			result = pow(d[0], d[1]);
			break;
		
		case DR_TOKEN_OP_BIGGEST:
			result = d[0] > d[1] ? d[0] : d[1];
			break;
		
		case DR_TOKEN_OP_SMALLEST:
			result = d[0] < d[1] ? d[0] : d[1];
			break;

		case DR_TOKEN_OP_RANDOM:
			result = do_rand_get(ctx->rand, d[0], d[1]);
			break;

		/* 3 parameters */

		case DR_TOKEN_OP_INTERPOLATE:
			result = dr_util_interpolate(d[0], d[1], d[2]);
			break;

		case DR_TOKEN_OP_LIMIT:
			result = dr_util_limit(d[0], d[1], d[2]);
			break;

		/* other */

		default:
			return DR_TOKEN_INVALID;
	}

	/* if needed convert back the result into a string */

	if (math_result)
	{
		*math_result = result;
	}
	else
	{
		snprintf(token, DR_TOKEN_N, "%.16f", result);
	}

	return DR_TOKEN_NUMBER;
}
