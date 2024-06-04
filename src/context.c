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

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <cassette/ccfg.h>
#include <cassette/cobj.h>

#include "context.h"
#include "substitution.h"
#include "token.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool _read_word (context_t *ctx, char token[static CCFG_MAX_WORD_BYTES]);

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

token_kind_t
context_get_token(context_t *ctx, char token[static CCFG_MAX_WORD_BYTES], double *math_result)
{
	assert(ctx);

	if (context_get_token_raw(ctx, token) == TOKEN_INVALID)
	{
		return TOKEN_INVALID;
	}

	return substitution_apply(ctx, token, math_result);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

token_kind_t
context_get_token_numeral(context_t *ctx, char token[static CCFG_MAX_WORD_BYTES], double *math_result)
{
	bool err = false;

	assert(ctx && math_result);

	switch (context_get_token(ctx, token, math_result))
	{
		case TOKEN_NUMBER:
			return TOKEN_NUMBER;
		
		case TOKEN_STRING:
			if (token[0] == '#')
			{
				*math_result = cobj_color_get_argb_uint(cobj_color_convert_str(token, &err));
			}
			else
			{
				*math_result = strtod(token, NULL);
			}
			if (!err)
			{
				return TOKEN_NUMBER;
			}
			/* fallthrough */

		default:
			return TOKEN_INVALID;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

token_kind_t
context_get_token_raw(context_t *ctx, char token[static CCFG_MAX_WORD_BYTES])
{
	assert(ctx);

	if (cobj_book_increment_iterator(ctx->variables))
	{
		snprintf(token, CCFG_MAX_WORD_BYTES, "%s", cobj_book_get_iteration(ctx->variables));
	}
	else if (cobj_book_increment_iterator(ctx->iteration))
	{
		snprintf(token, CCFG_MAX_WORD_BYTES, "%s", cobj_book_get_iteration(ctx->iteration));
	}
	else if (!_read_word(ctx, token))
	{
		return TOKEN_INVALID;
	}
	
	return TOKEN_STRING;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
context_goto_eol(context_t *ctx)
{
	assert(ctx);

	while (!ctx->eol_reached)
	{
		switch (fgetc(ctx->file))
		{
			case EOF:
				ctx->eof_reached = true;
				/* fallthrough */

			case '\n':
				ctx->eol_reached = true;
				break;

			default:
				break;
		}
	}

	cobj_book_lock_iterator(ctx->variables);
	cobj_book_lock_iterator(ctx->iteration);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static bool
_read_word(context_t *ctx, char token[static CCFG_MAX_WORD_BYTES])
{
	size_t i = 0;
	bool quotes_1 = false;
	bool quotes_2 = false;
	int  c;

	if (ctx->eol_reached)
	{
		return false;
	}

	/* skip leading whitespaces */

	for (;;)
	{
		switch ((c = fgetc(ctx->file)))
		{
			case ' ' :
			case '(' :
			case ')' :
			case '\t':
			case '\v':
				break;
			
			default:
				goto exit_lead;
		}
	}

exit_lead:

	/* read word */

	for (;; c = fgetc(ctx->file))
	{
		switch (c)
		{
			case EOF :
				goto exit_word;

			case ' ' :
			case '(' :
			case ')' :
			case '\t':
			case '\v':
			case '\n':
				if (quotes_1 || quotes_2)
				{
					goto char_add;
				}
				goto exit_word;
				
			case '\'':
				if (!quotes_2)
				{
					quotes_1 = !quotes_1;
					break;
				}
				goto char_add;

			case '\"':
				if (!quotes_1)
				{
					quotes_2 = !quotes_2;
					break;
				}
				goto char_add;

			default:
			char_add:
				if (i < CCFG_MAX_WORD_BYTES - 1)
				{
					token[i++] = (char)c;
				}
				break;
		}
	}

exit_word:

	/* forward stream pointer until next word, newline or EOF */

	for (;; c = fgetc(ctx->file))
	{
		switch (c)
		{
			case ' ' :
			case '(' :
			case ')' :
			case '\t':
			case '\v':
				break;
	
			case EOF :
				ctx->eof_reached = true;
				/* fallthrough */

			case '\n':
				ctx->eol_reached = true;
				goto exit_tail;
		
			default:
				fseek(ctx->file, -1, SEEK_CUR);
				goto exit_tail;
		}
	}

exit_tail:
	
	/* end */

	token[i] = '\0';

	return i;
}
