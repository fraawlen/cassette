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

#include <derelict/du.h>

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

static size_t       _convert_to_byte_offset (const du_string_t *str, size_t offset);
static const char * _get_next_codepoint     (const char *codepoint);
static bool         _is_end_byte            (char c);
static void         _update_n_values        (du_string_t *str);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static du_string_t _err_str =
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
du_string_append(du_string_t *str, const du_string_t *str_src)
{
	assert(str && str_src);

	du_string_insert(str, str_src, SIZE_MAX);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_append_raw(du_string_t *str, const char *c_str)
{
	assert(str);

	du_string_insert_raw(str, c_str, SIZE_MAX);
}
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_clear(du_string_t *str)
{
	assert(str);

	du_string_set_raw(str, "");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
du_string_convert_coords_to_offset(const du_string_t *str, size_t row, size_t col)
{
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

	const char *codepoint = str->chars;
	size_t offset = 0;;

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
du_string_convert_wrapped_offset(const du_string_t *str, const du_string_t *str_wrap, size_t offset)
{
	assert(str && str_wrap);

	if (str->failed || str_wrap->failed)
	{
		return 0;
	}

	if (offset >= str_wrap->n_codepoints)
	{
		return str->n_codepoints;
	}

	const char *codepoint_1 = str->chars;
	const char *codepoint_2 = str_wrap->chars;
	size_t diff = 0;

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

du_string_t *
du_string_create(void)
{
	du_string_t *str = malloc(sizeof(du_string_t));
	if (!str)
	{
		return &_err_str;
	}

	str->chars        = NULL;
	str->n_rows       = 0;
	str->n_cols       = 0;
	str->n_bytes      = 0;
	str->n_codepoints = 0;
	str->failed       = false;

	du_string_clear(str);

	return str;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

du_string_t *
du_string_create_double(double d, int precision)
{
	du_string_t *str = du_string_create();
	char tmp[25];

	snprintf(tmp, 25, "%.*f", precision, d);
	du_string_set_raw(str, tmp);

	return str;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

du_string_t *
du_string_create_duplicate(const du_string_t *str)
{
	assert(str);

	du_string_t *str_dup = du_string_create();

	du_string_set(str_dup, str);

	return str_dup;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_cut(du_string_t *str, size_t offset, size_t n_codepoints)
{
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

	size_t offset_2;

	offset_2 = _convert_to_byte_offset(str, offset + n_codepoints);
	offset   = _convert_to_byte_offset(str, offset);

	strcpy(str->chars + offset, str->chars + offset_2);
	
	_update_n_values(str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_destroy(du_string_t **str)
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
du_string_get_alloc_size(const du_string_t *str)
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
du_string_get_chars(const du_string_t *str)
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
du_string_get_chars_at_coords(const du_string_t *str, size_t row, size_t col)
{
	assert(str);

	if (str->failed)
	{
		return "";
	}

	return str->chars + _convert_to_byte_offset(str, du_string_convert_coords_to_offset(str, row, col));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
du_string_get_chars_at_offset(const du_string_t *str, size_t offset)
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
du_string_get_height(const du_string_t *str)
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
du_string_get_length(const du_string_t *str)
{
	assert(str);

	if (str->failed)
	{
		return 0;
	}

	return str->n_codepoints;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
du_string_get_width(const du_string_t *str)
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
du_string_has_failed(const du_string_t *str)
{
	assert(str);

	return str->failed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_insert(du_string_t *str, const du_string_t *str_src, size_t offset)
{
	assert(str && str_src);

	if (str_src->failed)
	{
		return;
	}

	du_string_insert_raw(str, str_src->chars, offset);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_insert_raw(du_string_t *str, const char *c_str, size_t offset)
{
	assert(str);

	if (str->failed)
	{
		return;
	}

	if (!c_str)
	{
		return;
	}

	size_t n;
	char *tmp;

	n = strlen(c_str);
	if (n > SIZE_MAX - str->n_bytes)
	{
		str->failed = true;
		return;
	}

	tmp = malloc(str->n_bytes + n);
	if (!tmp)
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
du_string_pad(du_string_t *str, const char *pattern, size_t offset, size_t n_codepoints_target)
{
	assert(str);

	if (str->failed)
	{
		return;
	}

	if (n_codepoints_target <= str->n_codepoints)
	{
		return;
	}

	size_t pad_n_bytes      = strlen(pattern);
	size_t n_codepoint_diff = n_codepoints_target - str->n_codepoints;

	char *tmp;

	/* create padding string */

	if (pad_n_bytes > (SIZE_MAX - 1) / n_codepoint_diff)
	{
		str->failed = true;
		return;
	}

	tmp = malloc(pad_n_bytes * n_codepoint_diff + 1);
	if (!tmp)
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

	du_string_insert_raw(str, tmp, offset);

	free(tmp);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_prepend(du_string_t *str, const du_string_t *str_src)
{
	assert(str && str_src);

	du_string_insert(str, str_src, 0);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_prepend_raw(du_string_t *str, const char *c_str)
{
	assert(str);

	du_string_insert_raw(str, c_str, 0);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_realloc(du_string_t *str)
{
	assert(str);

	if (str->failed)
	{
		return;
	}

	char *tmp;

	tmp = realloc(str->chars, str->n_bytes);
	if (!tmp)
	{
		str->failed = true;
		return;
	}

	str->chars = tmp;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
du_string_seek_next_codepoint(const char *codepoint)
{
	if (!codepoint)
	{
		return "";
	}

	return _get_next_codepoint(codepoint);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_set(du_string_t *str, const du_string_t *str_src)
{
	assert(str && str_src);

	if (str_src->failed)
	{
		return;
	}

	du_string_set_raw(str, str_src->chars);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_set_raw(du_string_t *str, const char *c_str)
{
	assert(str);

	if (str->failed)
	{
		return;
	}

	if (!c_str)
	{
		c_str = "";
	}

	char *tmp;

	tmp = malloc(strlen(c_str) + 1);
	if (!tmp)
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
du_string_slice(du_string_t *str, size_t offset, size_t n_codepoints)
{
	assert(str);

	if (str->failed)
	{
		return;
	}

	if (offset >= str->n_codepoints || n_codepoints == 0)
	{
		du_string_clear(str);
	}

	if (n_codepoints < str->n_codepoints - offset)
	{
		du_string_cut(str, offset + n_codepoints, SIZE_MAX);
	}
	
	du_string_cut(str, 0, offset);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
du_string_test_wrap(const du_string_t *str, size_t max_cols)
{
	assert(str && max_cols > 0);

	if (str->failed)
	{
		return 0;
	}

	if (max_cols >= str->n_cols)
	{
		return str->n_rows;
	}

	size_t row = 1;
	size_t col = 0;
	size_t n   = 0;

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
du_string_trim(du_string_t *str)
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
				du_string_cut(str, 0, i);
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
				du_string_cut(str, i + 1, SIZE_MAX);
				return;
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_wrap(du_string_t *str, size_t max_cols)
{
	assert(str && max_cols > 0);

	if (str->failed)
	{
		return;
	}

	if (max_cols > str->n_cols)
	{
		return;
	}

	size_t max_slots;
	size_t max_rows;
	size_t col;
	char *tmp;

	/* calculate (with overflow protection) max required memory to host wrapped string */

	if (str->n_rows > SIZE_MAX / str->n_cols)
	{
		str->failed = true;
		return;
	}

	max_slots = str->n_cols * str->n_rows;

	if (max_slots / max_cols > SIZE_MAX - 1)
	{
		str->failed = true;
		return;
	}

	max_rows = max_slots / max_cols + (max_slots % max_cols > 0 ? 1 : 0);

	if (max_cols > (SIZE_MAX - 1) / 4 || max_rows > SIZE_MAX / (max_cols * 4 + 1))
	{
		str->failed = true;
		return;
	}

	/* alloc memory */

	tmp = malloc(max_rows * (max_cols * 4 + 1));
	if (!tmp)
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
_convert_to_byte_offset(const du_string_t *str, size_t offset)
{
	if (offset >= str->n_codepoints)
	{
		return str->n_bytes - 1;
	}

	size_t i = 0;

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
_update_n_values(du_string_t *str)
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
_get_prev_codepoint(const char *codepoint, const du_string_t *str)
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
