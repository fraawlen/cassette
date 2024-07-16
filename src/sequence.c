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
#include <stdbool.h>
#include <stdlib.h>

#include "context.h"
#include "file.h"
#include "sequence.h"
#include "util.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/* sequences handlers */

static void _combine_var      (struct context *, enum token)   CCFG_NONNULL(1);
static void _declare_enum     (struct context *)               CCFG_NONNULL(1);
static void _declare_resource (struct context *, const char *) CCFG_NONNULL(1);
static void _declare_variable (struct context *)               CCFG_NONNULL(1);
static void _include          (struct context *)               CCFG_NONNULL(1);
static void _iterate          (struct context *)               CCFG_NONNULL(1);
static void _print            (struct context *)               CCFG_NONNULL(1);
static void _section_add      (struct context *)               CCFG_NONNULL(1);
static void _section_begin    (struct context *)               CCFG_NONNULL(1);
static void _section_del      (struct context *)               CCFG_NONNULL(1);
static void _seed             (struct context *)               CCFG_NONNULL(1);

/* iteration sequence preprocessing */

static void   _preproc_iter_new  (struct context *)         CCFG_NONNULL(1);
static size_t _preproc_iter_nest (struct context *, size_t) CCFG_NONNULL(1);

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
sequence_parse(struct context *ctx)
{
	enum token type;
	char token[TOKEN_MAX_LEN];

	if (ctx->depth >= CONTEXT_MAX_DEPTH)
	{
		return;
	}
	
	ctx->depth++;

	if ((type = context_get_token(ctx, token, NULL)) != TOKEN_SECTION_BEGIN && ctx->skip_sequences)
	{
		type = TOKEN_INVALID;
	}

	switch (type)
	{
		case TOKEN_VAR_APPEND:
		case TOKEN_VAR_PREPEND:
		case TOKEN_VAR_MERGE:
			_combine_var(ctx, type);
			break;

		case TOKEN_VAR_DECLARATION:
			_declare_variable(ctx);
			break;

		case TOKEN_ENUM_DECLARATION:
			_declare_enum(ctx);
			break;
		
		case TOKEN_SECTION_BEGIN:
			_section_begin(ctx);
			break;

		case TOKEN_SECTION_ADD:
			_section_add(ctx);
			break;

		case TOKEN_SECTION_DEL:
			_section_del(ctx);
			break;

		case TOKEN_INCLUDE:
			_include(ctx);
			break;

		case TOKEN_FOR_BEGIN:
			_iterate(ctx);
			break;

		case TOKEN_SEED:
			_seed(ctx);
			break;

		case TOKEN_PRINT:
			_print(ctx);
			break;

		case TOKEN_INVALID:
			break;

		case TOKEN_STRING:
		case TOKEN_NUMBER:
		default:
			_declare_resource(ctx, token);
			break;
	}

	context_goto_eol(ctx);

	ctx->depth--;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_combine_var(struct context *ctx, enum token type)
{
	cstr *val;
	enum cbook_group group = CBOOK_NEW;
	char name[TOKEN_MAX_LEN];
	char token_1[TOKEN_MAX_LEN];
	char token_2[TOKEN_MAX_LEN];
	size_t i;
	size_t j;

	val = cstr_create();

	/* get params */

	if (context_get_token(ctx, name,    NULL) == TOKEN_INVALID
	 || context_get_token(ctx, token_1, NULL) == TOKEN_INVALID
	 || context_get_token(ctx, token_2, NULL) == TOKEN_INVALID
	 || !cdict_find(ctx->keys_vars, token_1, CONTEXT_DICT_VARIABLE, &i)
	 || (type == TOKEN_VAR_MERGE && !cdict_find(ctx->keys_vars, token_2, CONTEXT_DICT_VARIABLE, &j)))
	{
		return;
	}

	/* generate new values and write them into the variable book */

	for (size_t k = 0; k < cbook_group_length(ctx->vars, i); k++)
	{
		cstr_clear(val);
		cstr_append(val, cbook_word_in_group(ctx->vars, i, k));
		switch (type)
		{
			case TOKEN_VAR_APPEND:
				cstr_append(val, token_2);
				break;

			case TOKEN_VAR_PREPEND:
				cstr_prepend(val, token_2);
				break;

			case TOKEN_VAR_MERGE:
				cstr_append(val, cbook_word_in_group(ctx->vars, j, k));
				break;

			default:
				break;
		}

		cbook_write(ctx->vars, cstr_chars(val), group);
		group = CBOOK_OLD;
	}

	/* update variable's reference in the variable dict */

	cdict_write(ctx->keys_vars, name, CONTEXT_DICT_VARIABLE, cbook_groups_number(ctx->vars) - 1);

	cstr_destroy(val);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_declare_enum(struct context *ctx)
{
	enum cbook_group group = CBOOK_NEW;
	char name[TOKEN_MAX_LEN];
	char token[TOKEN_MAX_LEN];
	double min;
	double max;
	double steps;
	double precision;
	double ratio;
	int n = 0;

	/* get enum name and params, set defaults on missing params */

	n += context_get_token        (ctx, name,  NULL)       != TOKEN_INVALID ? 1 : 0;
	n += context_get_token_numeral(ctx, token, &min)       != TOKEN_INVALID ? 1 : 0;
	n += context_get_token_numeral(ctx, token, &max)       != TOKEN_INVALID ? 1 : 0;
	n += context_get_token_numeral(ctx, token, &steps)     != TOKEN_INVALID ? 1 : 0;
	n += context_get_token_numeral(ctx, token, &precision) != TOKEN_INVALID ? 1 : 0;

	switch (n)
	{
		case 0:
		case 1:
			return;

		case 2:
			max = min;
			min = 0.0;
			/* fallthrough */

		case 3:
			steps = max - min;
			/* fallthrough */

		case 4:
			precision = 0.0;
			/* fallthrough */

		default:
			break;
	}

	if (steps < 1.0 || steps >= SIZE_MAX || precision < 0.0)
	{
		return;
	}

	if (precision > 16.0)
	{
		precision = 16;
	}
	
	/* generate enum values and write them into the variable book */

	for (size_t i = 0; i <= steps; i++)
	{
		ratio = util_interpolate(min, max, i / steps);
		snprintf(token, TOKEN_MAX_LEN, "%.*f", (int)precision, ratio);
		cbook_write(ctx->vars, token, group);
		group = CBOOK_OLD;
	}

	/* update variable's reference in the variable dict */

	cdict_write(ctx->keys_vars, name, CONTEXT_DICT_VARIABLE, cbook_groups_number(ctx->vars) - 1);	
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_declare_resource(struct context *ctx, const char *namespace)
{
	enum cbook_group group = CBOOK_NEW;
	char name[TOKEN_MAX_LEN];
	char value[TOKEN_MAX_LEN];
	size_t i;
	size_t n = 0;

	/* get resource's name */

	if (context_get_token(ctx, name, NULL) == TOKEN_INVALID)
	{
		return;
	}

	/* write resource's values into the sequence book */

	while (context_get_token(ctx, value, NULL) != TOKEN_INVALID)
	{
		cbook_write(ctx->sequences, value, group);
		group = CBOOK_OLD;
		n++;
	}

	if (n == 0)
	{
		return;
	}

	/* find namespace reference in sequence dict. if not found, create it */

	if (!cdict_find(ctx->keys_sequences, namespace, 0, &i))
	{
		i = cbook_groups_number(ctx->sequences);
		cdict_write(ctx->keys_sequences, namespace, 0, i);
	}

	/* update sequence's reference in the sequence dict         */
	/* use the namespace's dict value as sequence group (i > 0) */

	cdict_write(ctx->keys_sequences, name, i, cbook_groups_number(ctx->sequences) - 1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_declare_variable(struct context *ctx)
{
	enum cbook_group group = CBOOK_NEW;
	char name[TOKEN_MAX_LEN];
	char value[TOKEN_MAX_LEN];
	size_t n = 0;

	/* get variable's name */

	if (context_get_token(ctx, name, NULL) == TOKEN_INVALID)
	{
		return;
	}

	/* write variable's values into the variable book */

	while (context_get_token(ctx, value, NULL) != TOKEN_INVALID)
	{
		cbook_write(ctx->vars, value, group);
		group = CBOOK_OLD;
		n++;
	}

	if (n == 0)
	{
		return;
	}

	/* update variable's reference in the variable dict */

	cdict_write(ctx->keys_vars, name, CONTEXT_DICT_VARIABLE, cbook_groups_number(ctx->vars) - 1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_include(struct context *ctx)
{
	char token[TOKEN_MAX_LEN];
	cstr *filename;

	filename = cstr_create();

	while (context_get_token(ctx, token, NULL) != TOKEN_INVALID)
	{
		if (token[0] != '/')
		{
			cstr_clear(filename);
			cstr_append(filename, ctx->file_dir);
			cstr_append(filename, "/");
			cstr_append(filename, token);
			file_parse_child(ctx, cstr_chars(filename));
		}
		else
		{	
			file_parse_child(ctx, token);
		}
	}

	cstr_destroy(filename);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_iterate(struct context *ctx)
{
	char name[TOKEN_MAX_LEN];
	char token[TOKEN_MAX_LEN];
	size_t group_start;
	size_t group_end;
	size_t i;
	size_t j;
	bool nested;
	bool name_exists = false;

	/* get iteration params and detect if it's nested */

	if (context_get_token(ctx, token,  NULL) == TOKEN_INVALID
	 || !cdict_find(ctx->keys_vars, token, CONTEXT_DICT_VARIABLE, &i))
	{
		return;
	}

	if (context_get_token(ctx, name, NULL) == TOKEN_INVALID)
	{
		snprintf(name, TOKEN_MAX_LEN, "%s", token);
	}

	nested      = cbook_length(ctx->iteration);
	name_exists = nested && cdict_find(ctx->keys_vars, name, CONTEXT_DICT_ITERATION, &j);

	/* In the case of a new iteration, read the file and write raw sequences into the iteration book,    */
	/* but do not do that for nested iterations since the data is already written in the iteration book. */
	/* In both cases, find out which saved sequence marks the end of the iteration block                 */

	if (nested)
	{
		group_start = ctx->it_group + 1;
		group_end   = _preproc_iter_nest(ctx, group_start);
	}
	else
	{
		_preproc_iter_new(ctx);
		group_start = 0;
		group_end   = cbook_groups_number(ctx->iteration);
	}

	/* run iterated sequences */

	for (size_t k = 0; k < cbook_group_length(ctx->vars, i); k++)
	{
		cdict_write(ctx->keys_vars, name, CONTEXT_DICT_ITERATION, cbook_word_index(ctx->vars, i, k));
		for (ctx->it_group = group_start; ctx->it_group < group_end; ctx->it_group++)
		{
			ctx->it_i = 0;
			sequence_parse(ctx);
		}
	}

	/* restore iterator state */

	if (name_exists)
	{
		cdict_write(ctx->keys_vars, name, CONTEXT_DICT_ITERATION, j);
	}
	else
	{
		cdict_erase(ctx->keys_vars, name, CONTEXT_DICT_ITERATION);
	}

	if (!nested)
	{
		cbook_clear(ctx->iteration);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static size_t
_preproc_iter_nest(struct context *ctx, size_t start_group)
{
	char token[TOKEN_MAX_LEN];
	size_t n = 0;
	size_t i;

	for (i = start_group; i < cbook_groups_number(ctx->iteration); i++)
	{
		ctx->it_group = i;
		ctx->it_i     = 0;
		context_get_token_raw(ctx, token);

		/* look for matching TOKEN_FOR_END */

		switch (token_match(ctx->tokens, token))
		{
			case TOKEN_FOR_BEGIN:
				n++;
				break;

			case TOKEN_FOR_END:
				if (n == 0)
				{
					return i;
				}
				n--;
				break;

			default:
				break;
		}
	}

	return i;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_preproc_iter_new(struct context *ctx)
{
	char token[TOKEN_MAX_LEN];
	size_t n = 0;

	context_goto_eol(ctx);

	while (!ctx->eof_reached)
	{
		ctx->eol_reached = false;
		context_get_token_raw(ctx, token);

		/* look for matching TOKEN_FOR_END */

		switch (token_match(ctx->tokens, token))
		{
			case TOKEN_FOR_BEGIN:
				n++;
				break;

			case TOKEN_FOR_END:
				if (n == 0)
				{
					context_goto_eol(ctx);
					return;
				}
				n--;
				break;

			case TOKEN_INVALID:
				context_goto_eol(ctx);
				continue;

			default:
				break;
		}

		/* write down sequences that will be iterated */

		cbook_write(ctx->iteration, token, CBOOK_NEW);
		while (context_get_token_raw(ctx, token) != TOKEN_INVALID)
		{
			cbook_write(ctx->iteration, token, CBOOK_OLD);
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_print(struct context *ctx)
{
	char token[TOKEN_MAX_LEN];
	
	while (context_get_token(ctx, token, NULL) != TOKEN_INVALID)
	{
		fprintf(stderr, "%s,\t", token);
	}

	fprintf(stderr, "\n");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_section_add(struct context *ctx)
{
	char token[TOKEN_MAX_LEN];

	while (context_get_token(ctx, token, NULL) != TOKEN_INVALID)
	{
		cdict_write(ctx->keys_vars, token, CONTEXT_DICT_SECTION, 0);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_section_begin(struct context *ctx)
{
	char token[TOKEN_MAX_LEN];

	while (context_get_token(ctx, token, NULL) != TOKEN_INVALID)
	{
		if (!cdict_find(ctx->keys_vars, token, CONTEXT_DICT_SECTION, NULL))
		{
			ctx->skip_sequences = true;
			return;
		}
	}
	
	ctx->skip_sequences = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_section_del(struct context *ctx)
{
	char token[TOKEN_MAX_LEN];

	while (context_get_token(ctx, token, NULL) != TOKEN_INVALID)
	{
		cdict_erase(ctx->keys_vars, token, CONTEXT_DICT_SECTION);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_seed(struct context *ctx)
{
	char token[TOKEN_MAX_LEN];
	double d;
	
	if (context_get_token_numeral(ctx, token, &d) != TOKEN_INVALID)
	{
		crand_seed(ctx->rand, d);
	}
}
