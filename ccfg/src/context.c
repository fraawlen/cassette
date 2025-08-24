/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <assert.h>
#include <cassette/ccfg.h>
#include <cassette/cobj.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "context.h"
#include "substitution.h"
#include "token.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static char read_char    (struct context *);
static bool read_word    (struct context *, char [static CCFG_TOKEN_LENGTH]);
static void update_state (struct context *, char);

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

enum token
context_get_token(struct context *ctx, char token[static CCFG_TOKEN_LENGTH], double *math_result)
{
	if (context_get_token_raw(ctx, token) == TOKEN_INVALID)
	{
		return TOKEN_INVALID;
	}

	return substitution_apply(ctx, token, math_result);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum token
context_get_token_numeral(struct context *ctx, char token[static CCFG_TOKEN_LENGTH], double *math_result)
{
	bool err = false;

	switch (context_get_token(ctx, token, math_result))
	{
		case TOKEN_NUMBER:
			return TOKEN_NUMBER;
		
		case TOKEN_STRING:
			if (token[0] == '#')
			{
				*math_result = ccolor_to_argb_uint(ccolor_from_str(token, &err));
			}
			else
			{
				*math_result = strtod(token, nullptr);
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

enum token
context_get_token_raw(struct context *ctx, char token[static CCFG_TOKEN_LENGTH])
{
	if (ctx->var_i < cbook_group_length(ctx->vars, ctx->var_group))
	{
		snprintf(token, CCFG_TOKEN_LENGTH, "%s", cbook_word_in_group(ctx->vars, ctx->var_group, ctx->var_i++));
	}
	else if (ctx->it_i < cbook_group_length(ctx->iteration, ctx->it_group))
	{
		snprintf(token, CCFG_TOKEN_LENGTH, "%s", cbook_word_in_group(ctx->iteration, ctx->it_group, ctx->it_i++));
	}
	else if (!read_word(ctx, token))
	{
		return TOKEN_INVALID;
	}
	
	return TOKEN_STRING;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
context_goto_eol(struct context *ctx)
{
	while (!ctx->eol_reached)
	{
		update_state(ctx, read_char(ctx));
	}

	ctx->var_i = SIZE_MAX;
	ctx->it_i  = SIZE_MAX;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static char
read_char(struct context *ctx)
{
	return *ctx->buffer != '\0' ? *(ctx->buffer++) : '\0';
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
read_word(struct context *ctx, char token[static CCFG_TOKEN_LENGTH])
{
	char c;
	size_t i = 0;

	if (ctx->eol_reached)
	{
		return false;
	}

	/* skip leading whitespaces */

	for (;;)
	{
		switch ((c = read_char(ctx)))
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

	bool quotes_1 = false;
	bool quotes_2 = false;

	for (;; c = read_char(ctx))
	{
		switch (c)
		{
			case '\0':
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
				if (i < CCFG_TOKEN_LENGTH - 1)
				{
					token[i++] = (char)c;
				}
				break;
		}
	}

exit_word:
	
	/* end */

	update_state(ctx, c);

	token[i] = '\0';

	return i;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
update_state(struct context *ctx, char c)
{
	switch (c)
	{
		case '\0':
			ctx->eof_reached = true;
			/* fallthrough */

		case '\n':
			ctx->eol_reached = true;
			/* fallthrough */	

		default:
			break;
	}
}
