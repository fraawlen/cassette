/**
 * Copyright © 2024 Frawwlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Graphics (DG) GUI library.
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
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include </usr/include/string.h>

#include <dg/core/errno.h>

#include "string.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define _PADDING_CHAR "_"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool _is_end_char(char c);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
dg_base_string_append(dg_base_string_t *str, const char *str_raw)
{
	assert(str);

	if (!str_raw) {
		return;
	}

	if (!str->chars) {
		dg_base_string_set(str, str_raw);
		return;
	}

	char *tmp = realloc(str->chars, str->n_chars + strlen(str_raw) + 1);
	if (!tmp) {
		dg_core_errno_set(DG_CORE_ERRNO_MEMORY);
		return;
	}

	strcat(tmp, str_raw);
	str->chars = tmp;

	dg_base_string_recalculate_n_values(str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

dg_base_string_t
dg_base_string_convert_double(double d, int precision)
{
	dg_base_string_t str = DG_BASE_STRING_EMPTY;

	char *tmp = malloc(25);
	if (!tmp) {
		dg_core_errno_set(DG_CORE_ERRNO_MEMORY);
		return str;
	}
		
	sprintf(tmp, "%.*f", precision, d);
	dg_base_string_set(&str, tmp);
	free(tmp);

	return str;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_string_clear(dg_base_string_t *str)
{
	assert(str);

	free(str->chars);

	str->chars = NULL;
	str->n_rows  = 0;
	str->n_cols  = 0;
	str->n_chars = 0;
	str->n_codepoints = 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_string_pad(dg_base_string_t *str, size_t pad_n, bool align_left)
{
	dg_base_string_pad_custom(str, _PADDING_CHAR, pad_n, align_left);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_string_pad_custom(dg_base_string_t *str, const char *padder, size_t pad_n, bool align_left)
{
	assert(str && padder);

	if (pad_n < str->n_codepoints || strlen(padder) == 0) {
		return;
	}

	const size_t n = pad_n - (str->n_codepoints > 0 ? str->n_codepoints - 1 : 0);
	
	char *tmp = malloc(n * strlen(padder) + 1);
	if (!tmp) {
		dg_core_errno_set(DG_CORE_ERRNO_MEMORY);
		return;
	}

	tmp[0] = '\0';
	for (size_t i = 0; i < n; i++) {
		strcat(tmp, padder);
	}

	if (align_left) {
		dg_base_string_append(str, tmp);	
	} else {
		dg_base_string_prepend(str, tmp);	
	}

	free(tmp);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_string_prepend(dg_base_string_t *str, const char *str_raw)
{
	assert(str);

	if (!str_raw) {
		return;
	}

	if (!str->chars) {
		dg_base_string_set(str, str_raw);
		return;
	}

	char *tmp = malloc(str->n_chars + strlen(str_raw) + 1);
	if (!tmp) {
		dg_core_errno_set(DG_CORE_ERRNO_MEMORY);
		return;
	}

	strcpy(tmp, str_raw);
	strcat(tmp, str->chars);

	free(str->chars);
	str->chars = tmp;
	
	dg_base_string_recalculate_n_values(str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_string_recalculate_n_values(dg_base_string_t *str)
{
	assert(str);

	size_t n = 0;
	bool end = false;

	str->n_rows  = 0;
	str->n_cols  = 0;
	str->n_chars = 0;
	str->n_codepoints = 0;

	if (!str->chars) {
		return;
	}

	for (; !end; str->n_chars++) {

		if (!_is_end_char(str->chars[str->n_chars])) {
			continue;
		}

		str->n_codepoints++;

		switch (str->chars[str->n_chars]) {
			
			case '\0':
				end = true;
				/* fallthrough */

			case '\n':
				str->n_cols = n > str->n_cols ? n : str->n_cols;
				str->n_rows++;
				n = 0;
				break;

			default:
				n++;
				break;
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_string_set(dg_base_string_t *str, const char *str_raw)
{
	assert(str);

	char *tmp = NULL;

	if (str_raw) {
		tmp = strdup(str_raw);
		if (!tmp) {
			dg_core_errno_set(DG_CORE_ERRNO_MEMORY);
			return;
		}
	}

	free(str->chars);
	str->chars = tmp;

	dg_base_string_recalculate_n_values(str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
dg_base_string_test_wrap(const dg_base_string_t *str, size_t cw)
{
	assert(str && cw > 0);

	if (cw >= str->n_cols) {
		return str->n_rows;
	}

	size_t row = 1;
	size_t col = 0;
	size_t n   = 0;

	for (; str->chars[n] != '\0'; n++) {
		if (_is_end_char(str->chars[n])) {
			if (str->chars[n] == '\n') {
				col = 0;
				row++;
			} else if (col == cw) {
				col = 1;
				row++;
			} else {
				col++;
			}
		}
	}

	return row;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_string_wrap(dg_base_string_t *str, size_t cw)
{
	assert(str && cw > 0);

	if (cw >= str->n_cols) {
		return;
	}

	size_t i = 0;
	size_t j = 0;
	size_t n = 0;

	char *tmp = malloc((cw + 1) * dg_base_string_test_wrap(str, cw));
	if (!tmp) {
		dg_core_errno_set(DG_CORE_ERRNO_MEMORY);
		return;
	}

	for (; i < str->n_chars - 1; i++) {
		if (_is_end_char(str->chars[i])) {
			if (str->chars[i] == '\n') {
				n = 0;
			} else if (n == cw) {
				tmp[j++] = '\n';
				n = 1;
			} else {
				n++;
			}
		}
		tmp[j++] = str->chars[i];
	}
	tmp[j] = '\0';

	free(str->chars);
	str->chars = tmp;

	dg_base_string_recalculate_n_values(str);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static bool
_is_end_char(char c)
{
	return !!(((uint8_t)c >> 6) ^ 0x02); /* bitmask = 10xxxxxx */
}
