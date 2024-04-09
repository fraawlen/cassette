/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Objects (DO) library.
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

#include <derelict/do.h>

#include "safe.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct _string_t
{
	char *chars;
	size_t n_rows;
	size_t n_cols;
	size_t n_bytes;
	size_t n_codepoints;
	bool failed;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static size_t       _convert_to_byte_offset (const do_string_t *str, size_t offset);
static const char * _get_next_codepoint     (const char *codepoint);
static bool         _is_end_byte            (char c);
static void         _update_n_values        (do_string_t *str);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static do_string_t _err_str =
{
	.chars        = NULL,
	.n_rows       = 0,
	.n_cols       = 0,
	.n_bytes      = 0,
	.n_codepoints = 0,
	.failed       = true,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
do_string_append(do_string_t *str, const do_string_t *str_src)
{
	assert(str && str_src);

	do_string_insert(str, str_src, SIZE_MAX);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_string_append_raw(do_string_t *str, const char *c_str)
{
	assert(str);

	do_string_insert_raw(str, c_str, SIZE_MAX);
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_string_clear(do_string_t *str)
{
	assert(str);

	do_string_set_raw(str, "");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
do_string_convert_coords_to_offset(const do_string_t *str, size_t row, size_t col)
{
	const char *codepoint;
	size_t offset = 0;

	assert(str);

	if (str->failed)
	{
		return 0;
	}

	if (row >= str->n_rows)
	{
		row = str->n_rows - 1;
	}

	if (col > str->n_cols)
	{
		col = str->n_cols;
	}

	codepoint = str->chars;

	/* skip rows */

	while (row > 0)
	{
		if (*codepoint == '\n')
		{
			row--;
		}
		codepoint = _get_next_codepoint(codepoint);
		offset++;
	}

	/* seek until right column is reached */

	while (col > 0)
	{
		switch (*codepoint)
		{
			case '\0':
			case '\n':
				return offset;

			default:
				break;
		}
		codepoint = _get_next_codepoint(codepoint);
		offset++;
		col--;
	}

	return offset;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
do_string_convert_wrapped_offset(const do_string_t *str, const do_string_t *str_wrap, size_t offset)
{
	const char *codepoint_1;
	const char *codepoint_2;
	size_t diff = 0;

	assert(str && str_wrap);

	if (str->failed || str_wrap->failed)
	{
		return 0;
	}

	if (offset >= str_wrap->n_codepoints)
	{
		return str->n_codepoints;
	}

	codepoint_1 = str->chars;
	codepoint_2 = str_wrap->chars;

	for (size_t i = 0; i < offset; i++)
	{
		if (*codepoint_1 != '\n' && *codepoint_2 == '\n')
		{
			diff++;
		}
		else
		{
			codepoint_1 = _get_next_codepoint(codepoint_1);
		}

		codepoint_2 = _get_next_codepoint(codepoint_2);
	}

	return offset - diff;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

do_string_t *
do_string_create(void)
{
	do_string_t *str;

	if (!(str = malloc(sizeof(do_string_t))))
	{
		return &_err_str;
	}

	str->chars        = NULL;
	str->n_rows       = 0;
	str->n_cols       = 0;
	str->n_bytes      = 0;
	str->n_codepoints = 0;
	str->failed       = false;

	do_string_clear(str);

	return str;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

do_string_t *
do_string_create_double(double d, int precision)
{
	do_string_t *str;

	char tmp[25];

	snprintf(tmp, 25, "%.*f", precision, d);

	str = do_string_create();
	do_string_set_raw(str, tmp);

	return str;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

do_string_t *
do_string_create_duplicate(const do_string_t *str)
{
	assert(str);

	do_string_t *str_dup = do_string_create();

	do_string_set(str_dup, str);

	return str_dup;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_string_cut(do_string_t *str, size_t offset, size_t n_codepoints)
{
	size_t offset_2;

	assert(str);

	if (str->failed)
	{
		return;
	}

	if (offset >= str->n_codepoints || n_codepoints == 0)
	{
		return;
	}

	if (n_codepoints > str->n_codepoints - offset)
	{
		n_codepoints = str->n_codepoints - offset;
	}

	offset_2 = _convert_to_byte_offset(str, offset + n_codepoints);
	offset   = _convert_to_byte_offset(str, offset);

	memmove(str->chars + offset, str->chars + offset_2, str->n_bytes - offset_2);

	_update_n_values(str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_string_destroy(do_string_t **str)
{
	assert(str && *str);

	if (*str == &_err_str)
	{
		return;
	}

	free((*str)->chars);
	free(*str);

	*str = &_err_str;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
do_string_get_alloc_size(const do_string_t *str)
{
	assert(str);

	if (str->failed)
	{
		return 0;
	}

	return str->n_bytes;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
do_string_get_chars(const do_string_t *str)
{
	assert(str);

	if (str->failed)
	{
		return "";
	}

	return str->chars;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
do_string_get_chars_at_coords(const do_string_t *str, size_t row, size_t col)
{
	assert(str);

	if (str->failed)
	{
		return "";
	}

	return str->chars + _convert_to_byte_offset(str, do_string_convert_coords_to_offset(str, row, col));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
do_string_get_chars_at_offset(const do_string_t *str, size_t offset)
{
	assert(str);

	if (str->failed)
	{
		return "";
	}

	return str->chars + _convert_to_byte_offset(str, offset);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
do_string_get_height(const do_string_t *str)
{
	assert(str);

	if (str->failed)
	{
		return 0;
	}

	return str->n_rows;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
do_string_get_length(const do_string_t *str)
{
	assert(str);

	if (str->failed)
	{
		return 0;
	}

	return str->n_codepoints;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

do_string_t *
do_string_get_placeholder(void)
{
	return &_err_str;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
do_string_get_width(const do_string_t *str)
{
	assert(str);

	if (str->failed)
	{
		return 0;
	}

	return str->n_cols;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
do_string_has_failed(const do_string_t *str)
{
	assert(str);

	return str->failed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_string_insert(do_string_t *str, const do_string_t *str_src, size_t offset)
{
	assert(str && str_src);

	if (str_src->failed)
	{
		return;
	}

	do_string_insert_raw(str, str_src->chars, offset);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_string_insert_raw(do_string_t *str, const char *c_str, size_t offset)
{
	size_t n;
	char *tmp;

	assert(str);

	if (str->failed)
	{
		return;
	}

	if (!c_str)
	{
		return;
	}

	if (!do_safe_add(NULL, n = strlen(c_str), str->n_bytes))
	{
		str->failed = true;
		return;
	}

	if (!(tmp = malloc(str->n_bytes + n)))
	{
		str->failed = true;
		return;
	}

	offset = _convert_to_byte_offset(str, offset);

	memcpy(tmp,              str->chars,          offset);
	memcpy(tmp + offset,     c_str,               n);
	memcpy(tmp + offset + n, str->chars + offset, str->n_bytes - offset);

	free(str->chars);
	str->chars = tmp;
	_update_n_values(str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_string_pad(do_string_t *str, const char *pattern, size_t offset, size_t n_codepoints_target)
{
	size_t pad_n_bytes;
	size_t n_codepoint_diff;
	size_t n;
	char  *tmp;
	bool   safe = true;

	assert(str);

	if (str->failed)
	{
		return;
	}

	if (n_codepoints_target <= str->n_codepoints)
	{
		return;
	}

	pad_n_bytes      = strlen(pattern);
	n_codepoint_diff = n_codepoints_target - str->n_codepoints;

	/* create padding string */

	safe &= do_safe_mul(&n, pad_n_bytes, n_codepoint_diff);
	safe &= do_safe_add (&n, n, 1);

	if (!safe)
	{
		str->failed = true;
		return;
	}

	if (!(tmp = malloc(n)))
	{
		str->failed = true;
		return;
	}

	for (size_t i = 0; i < n_codepoint_diff; i++)
	{
		memcpy(tmp + i * pad_n_bytes, pattern, pad_n_bytes);
	}

	tmp[pad_n_bytes * n_codepoint_diff] = '\0';

	/* insert it */

	do_string_insert_raw(str, tmp, offset);

	free(tmp);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_string_prepend(do_string_t *str, const do_string_t *str_src)
{
	assert(str && str_src);

	do_string_insert(str, str_src, 0);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_string_prepend_raw(do_string_t *str, const char *c_str)
{
	assert(str);

	do_string_insert_raw(str, c_str, 0);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_string_realloc(do_string_t *str)
{
	char *tmp;

	assert(str);

	if (str->failed)
	{
		return;
	}

	if (!(tmp = realloc(str->chars, str->n_bytes)))
	{
		str->failed = true;
		return;
	}

	str->chars = tmp;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
do_string_seek_next_codepoint(const char *codepoint)
{
	if (!codepoint)
	{
		return "";
	}

	return _get_next_codepoint(codepoint);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_string_set(do_string_t *str, const do_string_t *str_src)
{
	assert(str && str_src);

	if (str_src->failed)
	{
		return;
	}

	do_string_set_raw(str, str_src->chars);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_string_set_raw(do_string_t *str, const char *c_str)
{
	char *tmp;

	assert(str);

	if (str->failed)
	{
		return;
	}

	if (!c_str)
	{
		c_str = "";
	}

	if (!(tmp = malloc(strlen(c_str) + 1)))
	{
		str->failed = true;
		return;
	}

	strcpy(tmp, c_str);

	free(str->chars);
	str->chars = tmp;
	_update_n_values(str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_string_slice(do_string_t *str, size_t offset, size_t n_codepoints)
{
	assert(str);

	if (str->failed)
	{
		return;
	}

	if (offset >= str->n_codepoints || n_codepoints == 0)
	{
		do_string_clear(str);
	}

	if (n_codepoints < str->n_codepoints - offset)
	{
		do_string_cut(str, offset + n_codepoints, SIZE_MAX);
	}
	
	do_string_cut(str, 0, offset);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
do_string_test_wrap(const do_string_t *str, size_t max_cols)
{
	size_t row = 1;
	size_t col = 0;
	size_t n   = 0;

	assert(str && max_cols > 0);

	if (str->failed)
	{
		return 0;
	}

	if (max_cols >= str->n_cols)
	{
		return str->n_rows;
	}

	for (const char *codepoint = str->chars; *codepoint != '\0'; n++)
	{
		if (*codepoint == '\n')
		{
			col = 0;
			row++;
		}
		else if (col == max_cols)
		{
			col = 1;
			row++;
		}
		else
		{
			col++;
		}

		codepoint = _get_next_codepoint(codepoint);
	}

	return row;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_string_trim(do_string_t *str)
{
	assert(str);

	if (str->failed)
	{
		return;
	}

	/* leading whitespaces */

	for (size_t i = 0;; i++)
	{
		switch (str->chars[i])
		{
			case '\v':
			case '\t':
			case ' ' :
				break;

			default:
				do_string_cut(str, 0, i);
				goto exit_lead;
		}
	}

exit_lead:

	/* trailing whitespaces */

	if (str->n_bytes < 2)
	{
		return;
	}

	for (size_t i = str->n_bytes - 2;; i--)
	{
		switch (str->chars[i])
		{
			case '\v':
			case '\t':
			case ' ' :
				break;

			default:
				do_string_cut(str, i + 1, SIZE_MAX);
				return;
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
do_string_wrap(do_string_t *str, size_t max_cols)
{
	size_t max_slots;
	size_t max_rows;
	size_t max_bytes;
	size_t col;
	char  *tmp;
	bool   safe = true;

	assert(str && max_cols > 0);

	if (str->failed)
	{
		return;
	}

	if (max_cols > str->n_cols)
	{
		return;
	}

	/* calculate (with overflow protection) max required memory to host wrapped string */

	safe &= do_safe_mul(&max_slots, str->n_cols, str->n_rows);

	safe &= do_safe_div(&max_rows,  max_slots, max_cols);
	safe &= do_safe_add(&max_rows,  max_rows,  max_slots % max_cols > 0 ? 1 : 0);

	safe &= do_safe_mul(&max_bytes, max_cols,  4);
	safe &= do_safe_add(&max_bytes, max_bytes, 1);
	safe &= do_safe_mul(&max_bytes, max_bytes, max_rows);

	if (!safe)
	{
		str->failed = true;
		return;
	}

	/* alloc memory */

	if (!(tmp = malloc(max_bytes)))
	{
		str->failed = true;
		return;
	}

	/* wrap string */

	str->n_cols       = max_cols;
	str->n_rows       = 1;
	str->n_bytes      = 0;
	str->n_codepoints = 0;

	col = 0;

	for (size_t i = 0;; i++)
	{
		if (_is_end_byte(str->chars[i]))
		{
			if (str->chars[i] == '\0')
			{
				tmp[str->n_bytes++] = str->chars[i];
				break;
			}
			else if (str->chars[i] == '\n')
			{
				str->n_rows++;
				col = 0;
			}
			else if (col == max_cols)
			{
				tmp[str->n_bytes++] = '\n';
				str->n_rows++;
				col = 1;
			}
			else
			{
				col++;
			}
			str->n_codepoints++;
		}
		tmp[str->n_bytes++] = str->chars[i];
	}

	free(str->chars);
	str->chars = tmp;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static size_t
_convert_to_byte_offset(const do_string_t *str, size_t offset)
{
	size_t i = 0;

	if (offset >= str->n_codepoints)
	{
		return str->n_bytes - 1;
	}

	while (offset > 0)
	{
		if (_is_end_byte(str->chars[i]))
		{
			offset--;
		}
		i++;
	}

	return i;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const char *
_get_next_codepoint(const char *codepoint)
{
	if (*codepoint != '\0')
	{
		do
		{
			codepoint++;
		}
		while (!_is_end_byte(*codepoint));
	}

	return codepoint;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_is_end_byte(char c)
{
	return !!(((uint8_t)c >> 6) ^ 0x02); /* bitmask = 10xxxxxx */
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_update_n_values(do_string_t *str)
{
	size_t col = 0;

	str->n_rows       = 1;
	str->n_cols       = 0;
	str->n_bytes      = 0;
	str->n_codepoints = 0;
	
	for (;;)
	{
		switch (str->chars[str->n_bytes++])
		{
			case '\0':
				str->n_cols = col > str->n_cols ? col : str->n_cols;
				return;

			case '\n':
				str->n_cols = col > str->n_cols ? col : str->n_cols;
				str->n_rows++;
				str->n_codepoints++;
				col = 0;
				break;

			default:
				if (_is_end_byte(str->chars[str->n_bytes]))
				{
					str->n_codepoints++;
					col++;
				}
				break;
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/*
static const char *
_get_prev_codepoint(const char *codepoint, const do_string_t *str)
{
	if (codepoint == str->chars)
	{
		return codepoint;
	}

	while (--codepoint != str->chars)
	{
		if (_is_end_byte(*(codepoint - 1)))
		{
			break;
		}
	}

	return codepoint;
}
*/
