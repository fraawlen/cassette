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

#include <stdbool.h>
#include <stdlib.h>

#include <cassette/ccfg.h>
#include <cassette/cobj.h>

#include "context.h"
#include "file.h"
#include "sequence.h"
#include "util.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define _MAX_ITER_INJECTIONS 32

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _combine_var      (context_t *ctx, token_kind_t);
static void _declare_enum     (context_t *ctx);
static void _declare_resource (context_t *ctx, const char *namespace);
static void _declare_variable (context_t *ctx);
static void _include          (context_t *ctx);
static void _iterate          (context_t *ctx, token_kind_t type);
static void _print            (context_t *ctx);
static void _section_add      (context_t *ctx);
static void _section_begin    (context_t *ctx);
static void _section_del      (context_t *ctx);
static void _seed             (context_t *ctx);

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
sequence_parse(context_t *ctx)
{
	token_kind_t type;

	char token[TOKEN_N];

	if (ctx->depth >= CONTEXT_MAX_DEPTH)
	{
		return;
	}
	else
	{
		ctx->depth++;
	}

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

		case TOKEN_ITERATE:
		case TOKEN_ITERATE_RAW:
			_iterate(ctx, type);
			break;

		case TOKEN_RAND_SEED:
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
_combine_var(context_t *ctx, token_kind_t type)
{
	cobj_book_group_mode_t mode = COBJ_BOOK_NEW_GROUP;
	cobj_string_t *val;

	char name[TOKEN_N];
	char token_1[TOKEN_N];
	char token_2[TOKEN_N];
	size_t i_var_1;
	size_t i_var_2;

	val = cobj_string_create();

	/* get variables */

	if (context_get_token(ctx, name,    NULL) == TOKEN_INVALID ||
	    context_get_token(ctx, token_1, NULL) == TOKEN_INVALID ||
	    context_get_token(ctx, token_2, NULL) == TOKEN_INVALID)
	{
		return;
	}

	if (!cobj_dictionary_find(ctx->ref_variables, token_1, CONTEXT_DICT_VARIABLE, &i_var_1))
	{
		return;
	}

	if (type == TOKEN_VAR_MERGE)
	{
		if (!cobj_dictionary_find(ctx->ref_variables, token_2, CONTEXT_DICT_VARIABLE, &i_var_2))
		{
			return;
		}
	}

	/* generate new values and write them into the variable book */

	for (size_t i = 0; i < cobj_book_get_group_size(ctx->variables, i_var_1); i++)
	{
		cobj_string_set_raw(val, cobj_book_get_word(ctx->variables, i_var_1, i));
		switch (type)
		{
			case TOKEN_VAR_APPEND:
				cobj_string_append_raw(val, token_2);
				break;

			case TOKEN_VAR_PREPEND:
				cobj_string_prepend_raw(val, token_2);
				break;

			case TOKEN_VAR_MERGE:
				cobj_string_append_raw(val, cobj_book_get_word(ctx->variables, i_var_2, i));
				break;

			default:
				break;
		}

		cobj_book_write_new_word(ctx->variables, cobj_string_get_chars(val), mode);
		mode = COBJ_BOOK_OLD_GROUP;
	}

	if (cobj_book_has_failed(ctx->variables) || cobj_string_has_failed(val))
	{
		cobj_string_destroy(&val);
		return;
	}

	/* update variable's reference in the variable dictionary */

	cobj_dictionary_write(
		ctx->ref_variables,
		name,
		CONTEXT_DICT_VARIABLE,
		cobj_book_get_number_groups(ctx->variables) - 1);

	cobj_string_destroy(&val);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_declare_enum(context_t *ctx)
{
	cobj_book_group_mode_t mode = COBJ_BOOK_NEW_GROUP;

	char name[TOKEN_N];
	char token[TOKEN_N];
	char *tmp;
	double min;
	double max;
	double steps;
	double precision;
	double ratio;

	/* get enum name and parameters */

	if (context_get_token(ctx, name, NULL) == TOKEN_INVALID)
	{
		return;
	}

	if (context_get_token_numeral(ctx, token, &min) == TOKEN_INVALID)
	{
		return;
	}

	if (context_get_token_numeral(ctx, token, &max) == TOKEN_INVALID)
	{
		max = min;
		min = 0.0;
	}

	util_sort_pair(&min, &max);

	if (context_get_token_numeral(ctx, token, &steps) == TOKEN_INVALID)
	{
		steps = max - min;
	}

	if (context_get_token_numeral(ctx, token, &precision) == TOKEN_INVALID)
	{
		precision = 0.0;
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
		if ((tmp = cobj_book_prepare_new_word(ctx->variables, mode)))
		{
			mode  = COBJ_BOOK_OLD_GROUP;
			ratio = util_interpolate(min, max, i / steps);
			snprintf(tmp, TOKEN_N, "%.*f", (int)precision, ratio);
		}
	}

	if (cobj_book_has_failed(ctx->variables))
	{
		return;
	}

	/* update variable's reference in the variable dictionary */

	cobj_dictionary_write(
		ctx->ref_variables,
		name,
		CONTEXT_DICT_VARIABLE,
		cobj_book_get_number_groups(ctx->variables) - 1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_declare_resource(context_t *ctx, const char *namespace)
{
	cobj_book_group_mode_t mode = COBJ_BOOK_NEW_GROUP;

	size_t n = 0;
	size_t m;
	char  name[TOKEN_N];
	char *tmp;

	/* get resource's name */

	if (context_get_token(ctx, name, NULL) == TOKEN_INVALID)
	{
		return;
	}

	/* write resource's values into the variable book */

	while ((tmp = cobj_book_prepare_new_word(ctx->sequences, mode)))
	{
		if (context_get_token(ctx, tmp, NULL) == TOKEN_INVALID)
		{
			cobj_book_erase_last_word(ctx->sequences);
			break;
		}
		mode = COBJ_BOOK_OLD_GROUP;
		n++;
	}

	if (n == 0 || cobj_book_has_failed(ctx->sequences))
	{
		return;
	}

	/* find namespace reference in sequence dictionary. if not found, create it */

	if (!cobj_dictionary_find(ctx->ref_sequences, namespace, 0, &m))
	{
		m = cobj_book_get_number_groups(ctx->sequences);
		cobj_dictionary_write(ctx->ref_sequences, namespace, 0, m);
	}

	/* update variable's reference in the variable dictionary   */
	/* use the namespace's dict value as sequence group (m > 0) */

	cobj_dictionary_write(ctx->ref_sequences, name, m, cobj_book_get_number_groups(ctx->sequences) - 1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_declare_variable(context_t *ctx)
{
	cobj_book_group_mode_t mode = COBJ_BOOK_NEW_GROUP;

	size_t n = 0;
	char  name[TOKEN_N];
	char *tmp;

	/* get variable's name */

	if (context_get_token(ctx, name, NULL) == TOKEN_INVALID)
	{
		return;
	}

	/* write variable's values into the variable book */

	while ((tmp = cobj_book_prepare_new_word(ctx->variables, mode)))
	{
		if (context_get_token(ctx, tmp, NULL) == TOKEN_INVALID)
		{
			cobj_book_erase_last_word(ctx->variables);
			break;
		}
		mode = COBJ_BOOK_OLD_GROUP;
		n++;
	}

	if (n == 0 || cobj_book_has_failed(ctx->variables))
	{
		return;
	}

	/* update variable's reference in the variable dictionary */

	cobj_dictionary_write(
		ctx->ref_variables,
		name,
		CONTEXT_DICT_VARIABLE,
		cobj_book_get_number_groups(ctx->variables) - 1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_include(context_t *ctx)
{
	cobj_string_t *filename;

	char token[TOKEN_N];

	filename = cobj_string_create();

	while (context_get_token(ctx, token, NULL) != TOKEN_INVALID)
	{
		if (token[0] != '/')
		{
			cobj_string_set_raw(filename, ctx->file_dir);
			cobj_string_append_raw(filename, "/");
			cobj_string_append_raw(filename, token);
			file_parse_child(ctx, cobj_string_get_chars(filename));
		} else {	
			file_parse_child(ctx, token);
		}
	}

	cobj_string_destroy(&filename);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_iterate(context_t *ctx, token_kind_t type)
{
	cobj_book_t *iteration;

	char token[TOKEN_N];
	char *tmp;
	bool raw;
	size_t inject[_MAX_ITER_INJECTIONS];
	size_t n_inject_max;
	size_t n_inject = 0;
	size_t i_var    = 0;
	size_t i        = 0;

	raw          = type == TOKEN_ITERATE_RAW;
	n_inject_max = raw ? 1 : _MAX_ITER_INJECTIONS;
	iteration    = cobj_book_create(10, TOKEN_N);

	/* grab variable to iterate through */

	if (context_get_token(ctx, token, NULL) == TOKEN_INVALID)
	{
		return;
	}

	if (!cobj_dictionary_find(ctx->ref_variables, token, CONTEXT_DICT_VARIABLE, &i_var))
	{
		return;
	}

	/* fill the book with the sequence to iterate and keep the location of iteration variable injections */

	while ((tmp = cobj_book_prepare_new_word(iteration, COBJ_BOOK_OLD_GROUP)))
	{
		if (raw)
		{
			type = context_get_token_raw(ctx, tmp);
		}
		else
		{
			type = context_get_token(ctx, tmp, NULL);
		}

		if (type == TOKEN_INVALID)
		{
			cobj_book_erase_last_word(iteration);
			break;
		}
		else if (n_inject < n_inject_max)
		{
			type = raw ? token_match(ctx->tokens, tmp) : type;
			if (type == TOKEN_ITER_INJECTION)
			{
				inject[n_inject++] = i;
			}
		}

		i++;
	}

	if (i == 0)
	{
		return;
	}

	/* process each iteration */

	for (i = 0; i < cobj_book_get_group_size(ctx->variables, i_var); i++)
	{
		for (size_t j = 0; j < n_inject; j++)
		{
			cobj_book_rewrite_word(
				iteration,
				cobj_book_get_word(ctx->variables, i_var, i),
				0,
				inject[j]);
		}

		cobj_book_reset_iterator(iteration, 0);

		ctx->iteration = iteration;
		sequence_parse(ctx);
	}

	/* end */
	
	cobj_book_destroy(&iteration);
	ctx->iteration = cobj_book_get_placeholder();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_print(context_t *ctx)
{
	char token[TOKEN_N];
	
	while (context_get_token(ctx, token, NULL) != TOKEN_INVALID)
	{
		fprintf(stderr, "%s,\t", token);
	}

	fprintf(stderr, "\n");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_section_add(context_t *ctx)
{
	char token[TOKEN_N];

	while (context_get_token(ctx, token, NULL) != TOKEN_INVALID)
	{
		cobj_dictionary_write(ctx->ref_variables, token, CONTEXT_DICT_SECTION, 0);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_section_begin(context_t *ctx)
{
	char token[TOKEN_N];

	while (context_get_token(ctx, token, NULL) != TOKEN_INVALID)
	{
		if (!cobj_dictionary_find(ctx->ref_variables, token, CONTEXT_DICT_SECTION, NULL))
		{
			ctx->skip_sequences = true;
			return;
		}
	}
	
	ctx->skip_sequences = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_section_del(context_t *ctx)
{
	char token[TOKEN_N];

	while (context_get_token(ctx, token, NULL) != TOKEN_INVALID)
	{
		cobj_dictionary_erase(ctx->ref_variables, token, CONTEXT_DICT_SECTION);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_seed(context_t *ctx)
{
	char token[TOKEN_N];
	double d;
	
	if (context_get_token_numeral(ctx, token, &d) != TOKEN_INVALID)
	{
		cobj_rand_seed(ctx->rand, d);
	}
}
