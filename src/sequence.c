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

#include <stdbool.h>
#include <stdlib.h>

#include <derelict/do.h>
#include <derelict/dr.h>

#include "context.h"
#include "file.h"
#include "sequence.h"
#include "util.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _declare_enum     (dr_context_t *ctx);
static void _declare_resource (dr_context_t *ctx, const char *namespace);
static void _declare_variable (dr_context_t *ctx);
static void _include          (dr_context_t *ctx);
static void _iterate          (dr_context_t *ctx);
static void _section_add      (dr_context_t *ctx);
static void _section_begin    (dr_context_t *ctx);
static void _section_del      (dr_context_t *ctx);
static void _seed             (dr_context_t *ctx);

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
dr_sequence_parse(dr_context_t *ctx)
{
	dr_token_kind_t type;

	char token[DR_TOKEN_N];

	if (ctx->depth >= DR_CONTEXT_MAX_DEPTH)
	{
		return;
	}
	else
	{
		ctx->depth++;
	}

	if ((type = dr_context_get_token(ctx, token, NULL)) != DR_TOKEN_SECTION_BEGIN && ctx->skip_sequences)
	{
		type = DR_TOKEN_INVALID;
	}

	switch (type)
	{
		case DR_TOKEN_VAR_DECLARATION:
			_declare_variable(ctx);
			break;

		case DR_TOKEN_ENUM_DECLARATION:
			_declare_enum(ctx);
			break;
		
		case DR_TOKEN_SECTION_BEGIN:
			_section_begin(ctx);
			break;

		case DR_TOKEN_SECTION_ADD:
			_section_add(ctx);
			break;

		case DR_TOKEN_SECTION_DEL:
			_section_del(ctx);
			break;

		case DR_TOKEN_INCLUDE:
			_include(ctx);
			break;

		case DR_TOKEN_ITERATOR:
			_iterate(ctx);
			break;

		case DR_TOKEN_RAND_SEED:
			_seed(ctx);
			break;

		case DR_TOKEN_INVALID:
			break;

		case DR_TOKEN_STRING:
		case DR_TOKEN_NUMBER:
		default:
			_declare_resource(ctx, token);
			break;
	}

	dr_context_goto_eol(ctx);

	ctx->depth--;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_declare_enum(dr_context_t *ctx)
{
	do_book_group_mode_t mode = DO_BOOK_NEW_GROUP;

	char name[DR_TOKEN_N];
	char token[DR_TOKEN_N];
	char *tmp;
	double min;
	double max;
	double steps;
	double precision;
	double ratio;

	/* get enum name and parameters */

	if (dr_context_get_token(ctx, name, NULL) == DR_TOKEN_INVALID)
	{
		return;
	}

	if (dr_context_get_token_numeral(ctx, token, &min) == DR_TOKEN_INVALID)
	{
		return;
	}

	if (dr_context_get_token_numeral(ctx, token, &max) == DR_TOKEN_INVALID)
	{
		max = min;
		min = 0.0;
	}

	dr_util_sort_pair(&min, &max);

	if (dr_context_get_token_numeral(ctx, token, &steps) == DR_TOKEN_INVALID)
	{
		steps = max - min;
	}

	if (dr_context_get_token_numeral(ctx, token, &precision) == DR_TOKEN_INVALID)
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
	
	steps -= 1.0;

	/* generate enum values and write them into the variable book */

	for (size_t i = 0; i <= steps; i++)
	{
		if ((tmp = do_book_prepare_new_word(ctx->variables, mode)))
		{
			mode  = DO_BOOK_OLD_GROUP;
			ratio = dr_util_interpolate(min, max, i / steps);
			snprintf(tmp, DR_TOKEN_N - 1, "%.*f", (int)precision, ratio);
		}
	}

	if (do_book_has_failed(ctx->variables))
	{
		return;
	}

	/* update variable's reference in the variable dictionary */

	do_dictionary_write(
		ctx->ref_variables,
		name,
		DR_CONTEXT_DICT_VARIABLE,
		do_book_get_number_groups(ctx->variables) - 1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_declare_resource(dr_context_t *ctx, const char *namespace)
{
	do_book_group_mode_t mode = DO_BOOK_NEW_GROUP;

	size_t n = 0;
	size_t m;
	char  name[DR_TOKEN_N];
	char *tmp;

	/* get resource's name */

	if (dr_context_get_token(ctx, name, NULL) == DR_TOKEN_INVALID)
	{
		return;
	}

	/* write resource's values into the variable book */

	while ((tmp = do_book_prepare_new_word(ctx->sequences, mode)))
	{
		if (dr_context_get_token(ctx, tmp, NULL) == DR_TOKEN_INVALID)
		{
			do_book_erase_last_word(ctx->sequences);
			break;
		}
		mode = DO_BOOK_OLD_GROUP;
		n++;
	}

	if (n == 0 || do_book_has_failed(ctx->sequences))
	{
		return;
	}

	/* find namespace reference in sequence dictionary if not found, create it */

	if (!do_dictionary_find(ctx->ref_sequences, namespace, 0, &m))
	{
		m = ++ctx->n_namespaces;
		do_dictionary_write(ctx->ref_sequences, namespace, 0, m);
	}

	/* update variable's reference in the variable dictionary   */
	/* use the namespace's dict value as sequence group (m > 0) */

	do_dictionary_write(ctx->ref_sequences, name, m, do_book_get_number_groups(ctx->sequences) - 1);

	/* debug */

/*
	printf("%s.\t%s.", namespace, name);

	do_book_reset_iterator(ctx->sequences, do_book_get_number_groups(ctx->sequences) - 1);
	while (do_book_increment_iterator(ctx->sequences))
	{
		printf("\t%s.", do_book_get_iteration(ctx->sequences));
	}

	printf("\n");
*/
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_declare_variable(dr_context_t *ctx)
{
	do_book_group_mode_t mode = DO_BOOK_NEW_GROUP;

	size_t n = 0;
	char  name[DR_TOKEN_N];
	char *tmp;

	/* get variable's name */

	if (dr_context_get_token(ctx, name, NULL) == DR_TOKEN_INVALID)
	{
		return;
	}

	/* write variable's values into the variable book */

	while ((tmp = do_book_prepare_new_word(ctx->variables, mode)))
	{
		if (dr_context_get_token(ctx, tmp, NULL) == DR_TOKEN_INVALID)
		{
			do_book_erase_last_word(ctx->variables);
			break;
		}
		mode = DO_BOOK_OLD_GROUP;
		n++;
	}

	if (n == 0 || do_book_has_failed(ctx->variables))
	{
		return;
	}

	/* update variable's reference in the variable dictionary */

	do_dictionary_write(
		ctx->ref_variables,
		name,
		DR_CONTEXT_DICT_VARIABLE,
		do_book_get_number_groups(ctx->variables) - 1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_include(dr_context_t *ctx)
{
	do_string_t *filename;

	char token[DR_TOKEN_N];

	filename = do_string_create();

	while (dr_context_get_token(ctx, token, NULL) != DR_TOKEN_INVALID)
	{
		do_string_set_raw(filename, ctx->file_dir);
		do_string_append_raw(filename, "/");
		do_string_append_raw(filename, token);
		dr_file_parse_child(ctx, do_string_get_chars(filename));
	}

	do_string_destroy(&filename);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_iterate(dr_context_t *ctx)
{
	size_t i_var = 0;
	size_t i_inj = 0;
	char token[DR_TOKEN_N];
	char *tmp;

	/* grab variable to iterate through */

	if (dr_context_get_token(ctx, token, NULL) == DR_TOKEN_INVALID)
	{
		return;
	}

	if (!do_dictionary_find(ctx->ref_variables, token, DR_CONTEXT_DICT_VARIABLE, &i_var))
	{
		return;
	}

	/* fill the book with the sequence to iterate */

	do_book_clear(ctx->iteration);
	while ((tmp = do_book_prepare_new_word(ctx->iteration, DO_BOOK_OLD_GROUP)))
	{
		if (dr_context_get_token_raw(ctx, tmp) == DR_TOKEN_INVALID)
		{
			do_book_erase_last_word(ctx->iteration);
			break;
		}
	}

	if (do_book_get_number_words(ctx->iteration) == 0)
	{
		return;
	}
	
	/* locate first iteration variable within the raw sequence */

	do_book_reset_iterator(ctx->iteration, 0);
	while (do_book_increment_iterator(ctx->iteration))
	{
		if (dr_token_match(ctx->tokens, do_book_get_iteration(ctx->iteration)) == DR_TOKEN_ITER_INJECTION)
		{
			break;
		}
		i_inj++;
	}

	/* process each iteration */

	for (size_t i = 0; i < do_book_get_group_size(ctx->variables, i_var); i++)
	{
		do_book_rewrite_word(ctx->iteration, do_book_get_word(ctx->variables, i_var, i), 0, i_inj);
		do_book_reset_iterator(ctx->iteration, 0);
		dr_sequence_parse(ctx);
	}

	/* end */
	
	do_book_lock_iterator(ctx->iteration);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_section_add(dr_context_t *ctx)
{
	char token[DR_TOKEN_N];

	while (dr_context_get_token(ctx, token, NULL) != DR_TOKEN_INVALID)
	{
		do_dictionary_write(ctx->ref_variables, token, DR_CONTEXT_DICT_SECTION, 0);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_section_begin(dr_context_t *ctx)
{
	char token[DR_TOKEN_N];

	while (dr_context_get_token(ctx, token, NULL) != DR_TOKEN_INVALID)
	{
		if (!do_dictionary_find(ctx->ref_variables, token, DR_CONTEXT_DICT_SECTION, NULL))
		{
			ctx->skip_sequences = true;
			return;
		}
	}
	
	ctx->skip_sequences = false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_section_del(dr_context_t *ctx)
{
	char token[DR_TOKEN_N];

	while (dr_context_get_token(ctx, token, NULL) != DR_TOKEN_INVALID)
	{
		do_dictionary_erase(ctx->ref_variables, token, DR_CONTEXT_DICT_SECTION);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_seed(dr_context_t *ctx)
{
	char token[DR_TOKEN_N];
	double d;
	
	if (dr_context_get_token_numeral(ctx, token, &d) != DR_TOKEN_INVALID)
	{
		do_rand_seed(ctx->rand, d);
	}
}
