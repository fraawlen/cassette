/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Utilities (DU) library.
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
#include <string.h>

#include "du.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool _is_end_char (char c);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
du_string_append(du_string_t *str, const char *c_str)
{
	assert(str);
	du_status_test(str->status, return);

	if (!c_str) {
		return;
	}

	char *tmp = realloc(str->chars, str->n_chars + strlen(c_str) + 1);
	du_status_assert(str->status, tmp, return);

	strcat(tmp, c_str);
	str->chars = tmp;
	du_string_recalculate_n_values(str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

du_string_t
du_string_duplicate(const du_string_t *str)
{
	assert(str);
	du_status_test(str->status, return *str);

	du_string_t str_dup = DU_STRING_EMPTY;
	du_string_init(&str_dup, str->chars);

	return str_dup;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

du_string_t
du_string_from_double(double d, int precision)
{
	du_string_t str = DU_STRING_EMPTY;

	char *tmp = malloc(25);
	du_status_assert(str.status, tmp, str.status = DU_STATUS_FAILURE; return str);
		
	str.chars = tmp;
	sprintf(str.chars, "%.*f", precision, d);
	du_string_recalculate_n_values(&str);

	return str;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_init(du_string_t *str, const char *c_str)
{
	assert(str);

	str->chars = c_str ? strdup(c_str) : NULL;
	str->status = str->chars || !c_str ? DU_STATUS_SUCCESS : DU_STATUS_FAILURE;
	du_string_recalculate_n_values(str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_pad(du_string_t *str, const char *padder, size_t pad_n, bool align_left)
{
	assert(str && padder);
	du_status_test(str->status, return);

	if (pad_n < str->n_codepoints || strlen(padder) == 0) {
		return;
	}

	const size_t n = pad_n - (str->n_codepoints > 0 ? str->n_codepoints - 1 : 0);
	
	char *tmp = malloc(n * strlen(padder) + 1);
	du_status_assert(str->status, tmp, return);

	tmp[0] = '\0';
	for (size_t i = 0; i < n; i++) {
		strcat(tmp, padder);
	}

	if (align_left) {
		du_string_append(str, tmp);	
	} else {
		du_string_prepend(str, tmp);	
	}

	free(tmp);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_prepend(du_string_t *str, const char *c_str)
{
	assert(str);
	du_status_test(str->status, return);

	if (!c_str) {
		return;
	}

	char *tmp = malloc(str->n_chars + strlen(c_str) + 1);
	du_status_assert(str->status, tmp, return);

	strcpy(tmp, c_str);
	strcat(tmp, str->chars);
	free(str->chars);
	str->chars = tmp;
	du_string_recalculate_n_values(str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_recalculate_n_values(du_string_t *str)
{
	assert(str);
	du_status_test(str->status, return);

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
du_string_replace(du_string_t *str, const char *c_str)
{
	assert(str);
	du_status_test(str->status, return);

	char *tmp = c_str ? strdup(c_str) : NULL;
	du_status_assert(str->status, tmp || !c_str, return);

	free(str->chars);
	str->chars = tmp;
	du_string_recalculate_n_values(str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_reset(du_string_t *str)
{
	assert(str);

	free(str->chars);

	str->chars = NULL;
	str->n_rows = 0;
	str->n_cols = 0;
	str->n_chars = 0;
	str->n_codepoints = 0;
	str->status = DU_STATUS_SUCCESS;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
du_string_test_wrap(const du_string_t *str, size_t max_cols)
{
	assert(str && max_cols > 0);
	du_status_test(str->status, return 0);

	if (max_cols >= str->n_cols) {
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
			} else if (col == max_cols) {
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
du_string_wrap(du_string_t *str, size_t max_cols)
{
	assert(str && max_cols > 0);
	du_status_test(str->status, return);

	if (max_cols >= str->n_cols) {
		return;
	}

	size_t i = 0;
	size_t j = 0;
	size_t n = 0;

	char *tmp = malloc((max_cols + 1) * du_string_test_wrap(str, max_cols));
	du_status_assert(str->status, tmp, return);

	for (; i < str->n_chars - 1; i++) {
		if (_is_end_char(str->chars[i])) {
			if (str->chars[i] == '\n') {
				n = 0;
			} else if (n == max_cols) {
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
	du_string_recalculate_n_values(str);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static bool
_is_end_char(char c)
{
	return !!(((uint8_t)c >> 6) ^ 0x02); /* bitmask = 10xxxxxx */
}
