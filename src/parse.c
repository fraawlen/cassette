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
#include <libgen.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <derelict/du.h>

#include "dr.h"
#include "parse.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define _OVERRIDE_CONTEXT_DATA(X) {if (X) {ctx.X = X;}}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/* top level parsers */

static void            _parse_sequence     (dr_parse_context_t *ctx);
static dr_token_kind_t _parse_token        (dr_parse_context_t *ctx, char *token, double *math_result);
static dr_token_kind_t _parse_token_number (dr_parse_context_t *ctx, char *token, double *math_result);

/* sequence parsing procedures */

static void _sequence_include       (dr_parse_context_t *ctx);
static void _sequence_iterate       (dr_parse_context_t *ctx);
static void _sequence_resource      (dr_parse_context_t *ctx, const char *namespace);
static void _sequence_section_add   (dr_parse_context_t *ctx);
static void _sequence_section_begin (dr_parse_context_t *ctx);
static void _sequence_section_del   (dr_parse_context_t *ctx);
static void _sequence_variable      (dr_parse_context_t *ctx);

/* token transformation procedures */

static dr_token_kind_t _token_comment    (dr_parse_context_t *ctx);
static dr_token_kind_t _token_condition  (dr_parse_context_t *ctx, char *token, double *math_result, dr_token_kind_t type);
static dr_token_kind_t _token_eof        (dr_parse_context_t *ctx);
static dr_token_kind_t _token_escape     (dr_parse_context_t *ctx, char *token);
static dr_token_kind_t _token_filler     (dr_parse_context_t *ctx, char *token, double *math_result);
static dr_token_kind_t _token_join       (dr_parse_context_t *ctx, char *token);
static dr_token_kind_t _token_math       (dr_parse_context_t *ctx, char *token, double *math_result, dr_token_kind_t type, size_t n);
static dr_token_kind_t _token_math_color (dr_parse_context_t *ctx, char *token, double *math_result, dr_token_kind_t type, size_t n);
static dr_token_kind_t _token_variable   (dr_parse_context_t *ctx, char *token);

/* misc procedures */

static void _apply_math_result (char *buf_token, double *buf_math, double value);
static void _end_sequence      (dr_parse_context_t *ctx);
static bool _get_next_token    (dr_parse_context_t *ctx, char *token);
static void _sort_pair         (double *d1, double *d2);

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

bool dr_parse_file(dr_parse_context_t *ctx_parent, const char *filename, du_book_t *book_sequences,
                   du_dictionary_t *dict_sequences, du_dictionary_t *dict_tokens)
{
	assert(ctx_parent || (book_sequences && dict_sequences && dict_tokens));

	/* open source file and get info about it */

	FILE *f;
	char f_dir[PATH_MAX];
	struct stat fs;

	if (!filename || !(f = fopen(filename, "r"))) {
		goto fail_open;
	}

	if (fstat(fileno(f), &fs) < 0) {
		goto fail_stat;
	}

	strncpy(f_dir, filename, PATH_MAX - 1);
	dirname(f_dir);

	/* setup new context */

	int64_t n_namespaces = 0;

	dr_parse_context_t ctx = {
		.parent = ctx_parent,
		.file_inode = fs.st_ino,
		.file_dir = f_dir,
		.file = f,
		.eol_reached = false,
		.eof_reached = false,
		.skip_sequences = false,
		.var_group = SIZE_MAX,
		.var_token = SIZE_MAX,
		.iter_token = SIZE_MAX,
		.book_iteration = NULL,
		.n_namespaces = &n_namespaces,
	};

	if (ctx_parent) {
		ctx.book_sequences = ctx_parent->book_sequences;
		ctx.dict_sequences = ctx_parent->dict_sequences;
		ctx.dict_tokens = ctx_parent->dict_tokens;
		ctx.n_namespaces = ctx_parent->n_namespaces;
	}

	_OVERRIDE_CONTEXT_DATA(book_sequences);
	_OVERRIDE_CONTEXT_DATA(dict_sequences);
	_OVERRIDE_CONTEXT_DATA(dict_tokens);

	/* protect against circular dependencies */

	while (ctx_parent) {
		if (ctx.file_inode == ctx_parent->file_inode) {
			goto fail_circular;
		}
		ctx_parent = ctx_parent->parent;
	}

	/* read source file sequence by sequence */

	while (!ctx.eof_reached) {
		ctx.eol_reached = false;
		ctx.var_group = SIZE_MAX;
		_parse_sequence(&ctx);
	}

	/* end */

	return true;

fail_circular:
fail_stat:
	fclose(f);
fail_open:
	return false;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_apply_math_result(char *buf_token, double *buf_math, double value)
{
	if (buf_math) {
		*buf_math = value;
	} else {
		sprintf(buf_token, "%.16f", value);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_end_sequence(dr_parse_context_t *ctx)
{
	while(!ctx->eol_reached) {
		switch (fgetc(ctx->file)) {
			
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

	ctx->iter_token = SIZE_MAX;
	ctx->var_token = SIZE_MAX;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_get_next_token(dr_parse_context_t *ctx, char *token)
{
	char *tmp;

	if (ctx->var_token != SIZE_MAX) {
		goto var_injection;
	} else if (ctx->iter_token != SIZE_MAX) {
		goto iter_injection;
	}

	/* if no injection is active, get new token from file stream */

	if (ctx->eol_reached) {
		return false;
	} else {
		return !(ctx->eof_reached = !du_misc_read_word(token, DR_TOKEN_N, ctx->file, &ctx->eol_reached));
	}

	/* variable injection */

var_injection:

	if ((tmp = du_book_get_word_in_group(ctx->book_sequences, ctx->var_group, ctx->var_token))) {
		strncpy(token, tmp, DR_TOKEN_N - 1);
	}

	if (++ctx->var_token >= du_book_get_group_length(ctx->book_sequences, ctx->var_group)) {
		ctx->var_token = SIZE_MAX;
	}

	return true;

	/* iteration injection */

iter_injection:

	if ((tmp = du_book_get_word(ctx->book_iteration, ctx->iter_token))) {
		strncpy(token, tmp, DR_TOKEN_N - 1);
	}

	if (++ctx->iter_token >= ctx->book_iteration->n_words) {
		ctx->iter_token = SIZE_MAX;
	}

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_parse_sequence(dr_parse_context_t *ctx)
{
	char token[DR_TOKEN_N] = "";

	dr_token_kind_t id = _parse_token(ctx, token, NULL);

	if (ctx->skip_sequences && id != DR_TOKEN_SECTION_BEGIN) {
		id = DR_TOKEN_INVALID;
	}

	/* sequence type specific sub-parsers */

	switch (id) {

		case DR_TOKEN_VAR_DECLARATION:
			_sequence_variable(ctx);
			break;
		
		case DR_TOKEN_SECTION_BEGIN:
			_sequence_section_begin(ctx);
			break;

		case DR_TOKEN_SECTION_ADD:
			_sequence_section_add(ctx);
			break;

		case DR_TOKEN_SECTION_DEL:
			_sequence_section_del(ctx);
			break;

		case DR_TOKEN_INCLUDE:
			_sequence_include(ctx);
			break;

		case DR_TOKEN_ITERATOR:
			_sequence_iterate(ctx);
			break;

		case DR_TOKEN_INVALID:
			break;

		case DR_TOKEN_STRING:
		case DR_TOKEN_NUMBER:
		default:
			_sequence_resource(ctx, token);
			break;
	}
	
	_end_sequence(ctx);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dr_token_kind_t
_parse_token(dr_parse_context_t *ctx, char *token, double *math_result)
{
	int64_t id;

	/* get token and match its string value to token kind */
	
	if (!_get_next_token(ctx, token)) {
		id = DR_TOKEN_INVALID;
	} else if (!du_dictionary_find_value(ctx->dict_tokens, token, 0, &id)) {
		id = DR_TOKEN_STRING;
	}

	/* apply transformations */

	switch (id) {

		case DR_TOKEN_EOF:
			return _token_eof(ctx);

		case DR_TOKEN_COMMENT:
			return _token_comment(ctx);

		case DR_TOKEN_FILLER:
			return _token_filler(ctx, token, math_result);

		case DR_TOKEN_JOIN:
			return _token_join(ctx, token);

		case DR_TOKEN_ESCAPE:
			return _token_escape(ctx, token);

		case DR_TOKEN_VAR_INJECTION:
			return _token_variable(ctx, token);

		case DR_TOKEN_IF_LESS:
		case DR_TOKEN_IF_LESS_EQ:
		case DR_TOKEN_IF_MORE:
		case DR_TOKEN_IF_MORE_EQ:
		case DR_TOKEN_IF_EQ:
		case DR_TOKEN_IF_EQ_NOT:
			return _token_condition(ctx, token, math_result, id);

		case DR_TOKEN_CONST_PI:
		case DR_TOKEN_CONST_EULER:
		case DR_TOKEN_CONST_TRUE:
		case DR_TOKEN_CONST_FALSE:
			return _token_math(ctx, token, math_result, id, 0);

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
			return _token_math(ctx, token, math_result, id, 1);

		case DR_TOKEN_OP_ADD:
		case DR_TOKEN_OP_SUBSTRACT:
		case DR_TOKEN_OP_MULTIPLY:
		case DR_TOKEN_OP_DIVIDE:
		case DR_TOKEN_OP_MOD:
		case DR_TOKEN_OP_POW:
		case DR_TOKEN_OP_BIGGEST:
		case DR_TOKEN_OP_SMALLEST:
		case DR_TOKEN_OP_RANDOM:
			return _token_math(ctx, token, math_result, id, 2);

		case DR_TOKEN_OP_LIMIT:
		case DR_TOKEN_OP_INTERPOLATE:
			return _token_math(ctx, token, math_result, id, 3);
		
		case DR_TOKEN_CL_RGB:
		case DR_TOKEN_CL_INTERPOLATE:
			return _token_math_color(ctx, token, math_result, id, 3);

		case DR_TOKEN_CL_RGBA:
			return _token_math_color(ctx, token, math_result, id, 4);

		default:
			return id;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dr_token_kind_t
_parse_token_number(dr_parse_context_t *ctx, char *token, double *math_result)
{
	bool err = false;

	switch (_parse_token(ctx, token, math_result)) {
		
		case DR_TOKEN_STRING:
			if (token[0] == '#') {
				*math_result = du_color_to_argb_uint(du_color_from_str(token, &err));
			} else {
				*math_result = strtod(token, NULL);
			}
			/* fallthrough */

		case DR_TOKEN_NUMBER:
			if (!err) {
				return DR_TOKEN_NUMBER;
			}
			/* fallthrough */

		default:
			return DR_TOKEN_INVALID;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_sequence_include(dr_parse_context_t *ctx)
{
	char token[DR_TOKEN_N] = "";

	du_string_t filename;

	du_string_init(&filename, "");

	while (_parse_token(ctx, token, NULL) != DR_TOKEN_INVALID) {
		du_string_replace(&filename, ctx->file_dir);
		du_string_append(&filename, "/");
		du_string_append(&filename, token);
		du_status_test(filename.status, break);
		dr_parse_file(ctx, filename.chars, NULL, NULL, NULL);
	}

	du_string_reset(&filename);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_sequence_iterate(dr_parse_context_t *ctx)
{
	char token[DR_TOKEN_N] = "";
	char *tmp;
	int64_t var_group;
	int64_t var_target;
	du_book_t iteration;

	/* grab variable to iterate through */

	if (_parse_token(ctx, token, NULL) == DR_TOKEN_INVALID) {
		return;
	}

	if (!du_dictionary_find_value(ctx->dict_sequences, token, DR_CONFIG_VARIABLES, &var_group)) {
		return;
	}

	/* init iteration book to store the sequence to iterate */

	du_book_init(&iteration, 10, DR_TOKEN_N);
	du_status_test(iteration.status, return);

	/* read raw sequence and store it into the book */

	do {
		if (!(tmp = du_book_get_new_word(&iteration, false))) {
			goto end;
		}
	} while (_get_next_token(ctx, tmp));
	du_book_erase_last_word(&iteration);
	
	/* find iteration variable position within the raw sequence */

	int64_t id;

	tmp = NULL;
	for (var_target = 0; du_book_get_next_word(&iteration, &tmp); var_target++) {
		if (du_dictionary_find_value(ctx->dict_tokens, tmp, 0, &id) && id == DR_TOKEN_ITER_INJECTION) {
			goto found;
		}
	}
	goto end;

found:

	/* process each iteration */

	for (size_t i = 0; i < du_book_get_group_length(ctx->book_sequences, var_group); i++) {

		strncpy(
			du_book_get_word(&iteration, var_target),
			du_book_get_word_in_group(ctx->book_sequences, var_group, i),
			DR_TOKEN_N - 1);

		ctx->iter_token = 0;
		ctx->book_iteration = &iteration;

		_parse_sequence(ctx);
	}
	
	/* end of iteration */

end:

	ctx->book_iteration = NULL;
	du_book_reset(&iteration);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_sequence_resource(dr_parse_context_t *ctx, const char *namespace)
{
	/* get resource's property name */
	
	char prop[DR_TOKEN_N] = "";
	
	if (_parse_token(ctx, prop, NULL) == DR_TOKEN_INVALID) {
		return;
	}

	/* write resource values into the sequence book */

	size_t n = 0;
	char *tmp;

	while ((tmp = du_book_get_new_word(ctx->book_sequences, n == 0))) {
		if (_parse_token(ctx, tmp, NULL) == DR_TOKEN_INVALID) {
			du_book_erase_last_word(ctx->book_sequences);
			break;
		}
		n++;
	}

	if (n == 0) {
		return;
	}

	/* find reference to namespace in sequence dictionary if not found, create it */
	
	int64_t i_namespace;

	if (!du_dictionary_find_value(ctx->dict_sequences, namespace, DR_CONFIG_NAMESPACES, &i_namespace)) {

		i_namespace = ++(*ctx->n_namespaces);

		du_dictionary_set_value(
			ctx->dict_sequences,
			namespace,
			DR_CONFIG_NAMESPACES,
			i_namespace);
	}

	/* update reference to property in sequence dictionary */

	du_dictionary_set_value(
		ctx->dict_sequences,
		prop,
		DR_CONFIG_NAMESPACES + i_namespace,
		ctx->book_sequences->n_groups - 1);

	/* debug */

	printf("(%li)\t%s.\t%s.", i_namespace, namespace, prop);

	tmp = du_book_get_last_group(ctx->book_sequences);
	do {
		printf("\t%s.", tmp);
	} while (du_book_get_next_word(ctx->book_sequences, &tmp));

	printf("\n");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_sequence_section_add(dr_parse_context_t *ctx)
{
	char token[DR_TOKEN_N] = "";

	while (_parse_token(ctx, token, NULL) != DR_TOKEN_INVALID) {
		du_dictionary_set_value(ctx->dict_sequences, token, DR_CONFIG_SECTIONS, 0);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_sequence_section_begin(dr_parse_context_t *ctx)
{
	char token[DR_TOKEN_N] = "";

	ctx->skip_sequences = false;

	while (_parse_token(ctx, token, NULL) != DR_TOKEN_INVALID) {
		if (!du_dictionary_find_value(ctx->dict_sequences, token, DR_CONFIG_SECTIONS, NULL)) {
			ctx->skip_sequences = true;
			return;
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_sequence_section_del(dr_parse_context_t *ctx)
{
	char token[DR_TOKEN_N] = "";

	while (_parse_token(ctx, token, NULL) != DR_TOKEN_INVALID) {
		du_dictionary_erase_value(ctx->dict_sequences, token, DR_CONFIG_SECTIONS);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_sequence_variable(dr_parse_context_t *ctx)
{
	/* get variable name */
	
	char name[DR_TOKEN_N] = "";

	if (_parse_token(ctx, name, NULL) == DR_TOKEN_INVALID) {
		return;
	}

	/* write resource values into the sequence book */

	size_t n = 0;
	char *tmp;

	while ((tmp = du_book_get_new_word(ctx->book_sequences, n == 0))) {
		if (_parse_token(ctx, tmp, NULL) == DR_TOKEN_INVALID) {
			du_book_erase_last_word(ctx->book_sequences);
			break;
		}
		n++;
	}

	if (n == 0) {
		return;
	}

	/* update reference to variable in sequence dictionary */

	du_dictionary_set_value(
		ctx->dict_sequences,
		name,
		DR_CONFIG_VARIABLES,
		ctx->book_sequences->n_groups - 1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_sort_pair(double *d1, double *d2)
{
	double tmp;

	if (*d1 > *d2) {
		tmp = *d1;
		*d1 = *d2;
		*d2 = tmp;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dr_token_kind_t
_token_comment(dr_parse_context_t *ctx)
{
	_end_sequence(ctx);

	return DR_TOKEN_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dr_token_kind_t
_token_condition(dr_parse_context_t *ctx, char *token, double *math_result, dr_token_kind_t type)
{
	bool result;
	double a;
	double b;

	/* execute condition */

	if (_parse_token_number(ctx, token, &a) == DR_TOKEN_INVALID ||
	    _parse_token_number(ctx, token, &b) == DR_TOKEN_INVALID) {
		return DR_TOKEN_INVALID;	
	}

	switch (type) {

		case DR_TOKEN_IF_LESS:
			result = a < b;
			break;

		case DR_TOKEN_IF_LESS_EQ:
			result = a <= b;
			break;

		case DR_TOKEN_IF_MORE:
			result = a > b;
			break;

		case DR_TOKEN_IF_MORE_EQ:
			result = a >= b;
			break;

		case DR_TOKEN_IF_EQ:
			result = a == b;
			break;

		case DR_TOKEN_IF_EQ_NOT:
			result = a != b;
			break;

		default:
			return DR_TOKEN_INVALID;	
	}

	/* get resulting token */

	char token2[DR_TOKEN_N] = "";

	dr_token_kind_t id = _parse_token(ctx, token, math_result);

	if (result) {
		_parse_token(ctx, token2, NULL);
		return id;
	} else {
		return _parse_token(ctx, token, math_result);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dr_token_kind_t
_token_filler(dr_parse_context_t *ctx, char *token, double *math_result)
{
	return _parse_token(ctx, token, math_result);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dr_token_kind_t
_token_eof(dr_parse_context_t *ctx)
{
	ctx->eof_reached = true;
	ctx->eol_reached = true;

	return DR_TOKEN_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dr_token_kind_t
_token_escape(dr_parse_context_t *ctx, char *token)
{
	ctx->eol_reached = false;

	return _get_next_token(ctx, token) ? DR_TOKEN_STRING : DR_TOKEN_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dr_token_kind_t
_token_join(dr_parse_context_t *ctx, char *token)
{
	char token2[DR_TOKEN_N] = "";

	if (_parse_token(ctx, token,  NULL) == DR_TOKEN_INVALID ||
	    _parse_token(ctx, token2, NULL) == DR_TOKEN_INVALID) {
		return DR_TOKEN_INVALID;
	}

	strncat(token, token2, DR_TOKEN_N - strlen(token) - 1);

	return DR_TOKEN_STRING;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dr_token_kind_t
_token_math(dr_parse_context_t *ctx, char *token, double *math_result, dr_token_kind_t type, size_t n)
{
	double result;
	double d[3] = {0};

	for (size_t i = 0; i < n; i++) {
		if (_parse_token_number(ctx, token, d + i) == DR_TOKEN_INVALID) {
			return DR_TOKEN_INVALID;	
		}
	}

	switch (type) {

		/* 0 parameters */

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

		/* 1 parameter */

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
			_sort_pair(d, d + 1);
			result = d[0] + (double)rand() / (double)RAND_MAX * (d[1] - d[0]);
			break;

		/* 3 parameters */

		case DR_TOKEN_OP_INTERPOLATE:
			du_ratio_bind(d + 2);
			result = d[1] * d[2] + d[0] * (1.0 - d[2]);
			break;

		case DR_TOKEN_OP_LIMIT:
			_sort_pair(d + 1, d + 2);
			result = d[0] < d[1] ? d[1] : (d[0] > d[2] ? d[2] : d[1]);
			break;

		/* other */

		default:
			return DR_TOKEN_INVALID;
	}

	_apply_math_result(token, math_result, result);

	return DR_TOKEN_NUMBER;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dr_token_kind_t
_token_math_color(dr_parse_context_t *ctx, char *token, double *math_result, dr_token_kind_t type, size_t n)
{
	du_color_t result;
	du_color_t cl[4] = {0};
	double d[4] = {0};

	for (size_t i = 0; i < n; i++) {
		if (_parse_token_number(ctx, token, d + i) == DR_TOKEN_INVALID) {
			return DR_TOKEN_INVALID;	
		}
		cl[i] = du_color_from_argb_uint(d[i]);
	}

	switch (type) {

		/* 3 parameters */

		case DR_TOKEN_CL_RGB:
			result = du_color_from_rgba(d[0], d[1], d[2], 255);
			break;

		case DR_TOKEN_CL_INTERPOLATE:
			result = du_color_interpolate(cl[0], cl[1], d[2]);
			break;

		/* 4 parameters */

		case DR_TOKEN_CL_RGBA:
			result = du_color_from_rgba(d[0], d[1], d[2], d[3]);
			break;

		/* other */

		default:
			return DR_TOKEN_INVALID;
	}

	_apply_math_result(token, math_result, du_color_to_argb_uint(result));

	return DR_TOKEN_NUMBER;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static dr_token_kind_t
_token_variable(dr_parse_context_t *ctx, char *token)
{
	int64_t group = 0;

	if (_parse_token(ctx, token, NULL) == DR_TOKEN_INVALID) {
		return DR_TOKEN_INVALID;
	}

	if (!du_dictionary_find_value(ctx->dict_sequences, token, DR_CONFIG_VARIABLES, &group)) {
		return DR_TOKEN_INVALID;
	}

	ctx->var_group = group;
	ctx->var_token = 0;

	return _parse_token(ctx, token, NULL);
}
