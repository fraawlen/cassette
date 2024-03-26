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
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include <derelict/do.h>

#include "context.h"
#include "token.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static dr_token_kind_t _comment (void);
static dr_token_kind_t _eof     (dr_context_t *ctx);
static dr_token_kind_t _escape  (dr_context_t *ctx, char token[DR_TOKEN_N]);
static dr_token_kind_t _filler  (dr_context_t *ctx, char token[DR_TOKEN_N], double *math_result);
static dr_token_kind_t _join    (dr_context_t *ctx, char token[DR_TOKEN_N]);

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
