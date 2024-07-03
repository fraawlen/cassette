/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Objects (COBJ) library.
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

#include <cassette/cstr.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "safe.h"

#if __GNUC__ > 4
	#define CSTR_CONST __attribute__((const))
#else
	#define CSTR_CONST
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct cstr
{
	char *chars;
	size_t n_rows;
	size_t n_cols;
	size_t n_chars;
	size_t n_alloc;
	size_t n_codepoints;
	size_t tab_width;
	int digits;
	enum cstr_err err;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static size_t      _byte_offset     (const cstr *str, size_t offset) CSTR_NONNULL(1) CSTR_PURE;
static bool        _is_end_byte     (char c)                         CSTR_CONST;
static const char *_next_codepoint  (const char *codepoint)          CSTR_NONNULL(1) CSTR_PURE;
static void        _update_n_values (cstr *str)                      CSTR_NONNULL(1);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

cstr cstr_placeholder_instance =
{
	.chars        = NULL,
	.n_rows       = 0,
	.n_cols       = 0,
	.n_chars      = 0,
	.n_alloc      = 0,
	.n_codepoints = 0,
	.tab_width    = 0,
	.digits       = 0,
	.err          = CSTR_INVALID,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

size_t
cstr_byte_length(const cstr *str)
{
	if (str->err)
	{
		return 0;
	}

	return str->n_chars;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cstr_byte_offset(const cstr *str, size_t offset)
{
	if (str->err)
	{
		return 0;
	}

	return _byte_offset(str, offset);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
cstr_chars(const cstr *str)
{
	if (str->err)
	{
		return "";
	}

	return str->chars;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
cstr_chars_at_coords(const cstr *str, size_t row, size_t col)
{
	if (str->err)
	{
		return "";
	}

	return str->chars + _byte_offset(str, cstr_coords_offset(str, row, col));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
cstr_chars_at_offset(const cstr *str, size_t offset)
{
	if (str->err)
	{
		return "";
	}

	return str->chars + _byte_offset(str, offset);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_clear(cstr *str)
{
	if (str->err)
	{
		return;
	}

	str->chars[0] = '\0';

	_update_n_values(str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cstr *
cstr_clone(const cstr *str)
{
	cstr *str_new;

	if (str->err || !(str_new = malloc(sizeof(cstr))))
	{
		return CSTR_PLACEHOLDER;
	}

	if (!(str_new->chars = malloc(str->n_alloc)))
	{
		free(str_new);
		return CSTR_PLACEHOLDER;
	}

	memcpy(str_new->chars, str->chars, str->n_chars);

	str_new->n_rows       = str->n_rows;
	str_new->n_cols       = str->n_cols;
	str_new->n_chars      = str->n_chars;
	str_new->n_codepoints = str->n_codepoints;
	str_new->n_alloc      = str->n_alloc;
	str_new->tab_width    = str->tab_width;
	str_new->digits       = str->digits;
	str_new->err          = CSTR_OK;
	
	return str_new;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cstr_coords_offset(const cstr *str, size_t row, size_t col)
{
	const char *codepoint;
	size_t offset = 0;

	if (str->err)
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
		codepoint = _next_codepoint(codepoint);
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

			case '\t':
				col -= col >= str->tab_width ? str->tab_width : col;
				break;

			default:
				col--;
				break;
		}
		codepoint = _next_codepoint(codepoint);
		offset++;
	}

	return offset;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cstr *
cstr_create(void)
{
	cstr *str;

	if (!(str = malloc(sizeof(cstr))))
	{
		return CSTR_PLACEHOLDER;
	}

	if (!(str->chars = malloc(1)))
	{
		free(str);
		return CSTR_PLACEHOLDER;
	}

	str->chars[0]  = '\0';
	str->n_alloc   = 1;
	str->tab_width = 1;
	str->digits    = 0;
	str->err       = CSTR_OK;
	
	_update_n_values(str);

	return str;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_cut(cstr *str, size_t offset, size_t length)
{
	size_t offset_2;

	if (str->err || offset >= str->n_codepoints || length == 0)
	{
		return;
	}

	if (length > str->n_codepoints - offset)
	{
		length = str->n_codepoints - offset;
	}

	offset_2 = _byte_offset(str, offset + length);
	offset   = _byte_offset(str, offset);

	memmove(str->chars + offset, str->chars + offset_2, str->n_chars - offset_2);

	_update_n_values(str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_destroy(cstr *str)
{
	if (str == CSTR_PLACEHOLDER)
	{
		return;
	}

	free(str->chars);
	free(str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cstr_err
cstr_error(const cstr *str)
{
	return str->err;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cstr_height(const cstr *str)
{
	if (str->err)
	{
		return 0;
	}

	return str->n_rows;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_insert_cstr(cstr *str, const cstr *str_src, size_t offset)
{
	if (str->err || str_src->err)
	{
		return;
	}

	cstr_insert_raw(str, str_src->chars, offset);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_insert_double(cstr *str, double d, size_t offset)
{
	char tmp[64];

	if (str->err)
	{
		return;
	}

	snprintf(tmp, 64, "%.*f", str->digits, d);

	cstr_insert_raw(str, tmp, offset);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_insert_long(cstr *str, long long l, size_t offset)
{
	char tmp[64];

	if (str->err)
	{
		return;
	}

	snprintf(tmp, 64, "%lli", l);

	cstr_insert_raw(str, tmp, offset);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_insert_raw(cstr *str, const char *raw_str, size_t offset)
{
	size_t n;
	size_t m;
	char *tmp_dst;
	char *tmp_src = NULL;

	if (str->err)
	{
		return;
	}

	if (!safe_add(&m, n = strlen(raw_str), str->n_chars))
	{
		str->err |= CSTR_OVERFLOW;
		return;
	}
	
	/* detect overlapping memory areas */

	if (raw_str >= str->chars && raw_str <= str->chars + str->n_alloc)
	{
		if (!(tmp_src = strdup(raw_str)))
		{
			str->err |= CSTR_MEMORY;
			return;
		}
		raw_str = tmp_src;
	}

	/* extend allocated memory if needed */

	if (m > str->n_alloc)
	{
		if (!(tmp_dst = realloc(str->chars, m)))
		{
			str->err |= CSTR_MEMORY;
			free(tmp_src);
			return;
		}
		str->chars   = tmp_dst;
		str->n_alloc = m;
	}

	/* insert */

	offset = _byte_offset(str, offset);

	memmove(str->chars + offset + n, str->chars + offset, str->n_chars - offset);
	memcpy(str->chars + offset, raw_str, n);
	free(tmp_src);
	
	_update_n_values(str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cstr_length(const cstr *str)
{
	if (str->err)
	{
		return 0;
	}

	return str->n_codepoints;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_pad(cstr *str, const char *pattern, size_t offset, size_t length_target)
{
	size_t pad_n_chars;
	size_t length_diff;
	size_t n;
	char *tmp;

	if (str->err || length_target <= str->n_codepoints)
	{
		return;
	}

	pad_n_chars = strlen(pattern);
	length_diff = length_target - str->n_codepoints;

	/* create padding string */

	if (!safe_mul(&n, pad_n_chars, length_diff)
	 || !safe_add(&n, n, 1))
	{
		str->err |= CSTR_OVERFLOW;
		return;
	}

	if (!(tmp = malloc(n)))
	{
		str->err |= CSTR_MEMORY;
		return;
	}

	for (size_t i = 0; i < length_diff; i++)
	{
		memcpy(tmp + i * pad_n_chars, pattern, pad_n_chars);
	}

	tmp[pad_n_chars * length_diff] = '\0';

	/* insert it */

	cstr_insert_raw(str, tmp, offset);

	free(tmp);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_prealloc(cstr *str, size_t byte_length)
{
	char *tmp;

	if (str->err || byte_length <= str->n_alloc)
	{
		return;
	}

	if (!(tmp = realloc(str->chars, byte_length)))
	{
		str->err |= CSTR_MEMORY;
		return;
	}

	str->chars   = tmp;
	str->n_alloc = byte_length;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_repair(cstr *str)
{
	str->err &= CSTR_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_set_double_digits(cstr *str, int digits)
{
	if (str->err)
	{
		return;
	}

	str->digits = digits;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_set_tab_width(cstr *str, size_t width)
{
	if (str->err)
	{
		return;
	}

	str->tab_width = width;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_slice(cstr *str, size_t offset, size_t length)
{
	if (str->err)
	{
		return;
	}

	if (offset >= str->n_codepoints || length == 0)
	{
		cstr_clear(str);
		return;
	}

	if (length < str->n_codepoints - offset)
	{
		cstr_cut(str, offset + length, SIZE_MAX);
	}

	cstr_cut(str, 0, offset);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cstr_test_wrap(const cstr *str, size_t max_width)
{
	size_t row = 1;
	size_t col = 0;

	if (str->err || max_width == 0)
	{
		return 0;
	}

	if (max_width >= str->n_cols)
	{
		return str->n_rows;
	}

	for (const char *codepoint = str->chars; *codepoint != '\0'; codepoint = _next_codepoint(codepoint))
	{
		if (*codepoint == '\n')
		{
			col = 0;
			row++;
		}
		else if (col == max_width)
		{
			col = 1;
			row++;
		}
		else
		{
			col += *codepoint == '\t' ? str->tab_width : 1;
		}
	}

	return row;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_trim(cstr *str)
{
	if (str->err)
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
				cstr_cut(str, 0, i);
				goto exit_lead;
		}
	}

exit_lead:

	/* trailing whitespaces */

	if (str->n_chars < 2)
	{
		return;
	}

	for (size_t i = str->n_chars - 2;; i--)
	{
		switch (str->chars[i])
		{
			case '\v':
			case '\t':
			case ' ' :
				break;

			default:
				cstr_cut(str, i + 1, SIZE_MAX);
				return;
		}
	}

}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cstr_unwrapped_offset(const cstr *str, const cstr *str_wrap, size_t offset)
{
	const char *codepoint_1;
	const char *codepoint_2;
	size_t diff = 0;

	if (str->err || str_wrap->err)
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
			codepoint_1 = _next_codepoint(codepoint_1);
		}

		codepoint_2 = _next_codepoint(codepoint_2);
	}

	return offset - diff;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cstr_width(const cstr *str)
{
	if (str->err)
	{
		return 0;
	}

	return str->n_cols;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_wrap(cstr *str, size_t max_width)
{
	size_t max_slots;
	size_t max_rows;
	size_t max_bytes;
	size_t col;
	char *tmp;

	if (str->err || max_width == 0 || max_width >= str->n_cols)
	{
		return;
	}

	/* calculate (with overflow protection) max required memory to host wrapped string */

	if (!safe_mul(&max_slots, str->n_cols, str->n_rows)
	 || !safe_div(&max_rows,  max_slots, max_width)
	 || !safe_add(&max_rows,  max_rows, max_slots % max_width > 0 ? 1 : 0)
	 || !safe_mul(&max_bytes, max_width, 4)
	 || !safe_add(&max_bytes, max_bytes, 1)
	 || !safe_mul(&max_bytes, max_bytes, max_rows))
	{
		str->err |= CSTR_OVERFLOW;
		return;
	}

	/* alloc memory */

	if (!(tmp = malloc(max_bytes)))
	{
		str->err |= CSTR_MEMORY;
		return;
	}

	str->n_alloc = max_bytes;

	/* wrap string */

	str->n_cols       = max_width;
	str->n_rows       = 1;
	str->n_chars      = 0;
	str->n_codepoints = 0;

	col = 0;

	for (size_t i = 0;; i++)
	{
		if (_is_end_byte(str->chars[i]))
		{
			if (str->chars[i] == '\0')
			{
				tmp[str->n_chars++] = str->chars[i];
				break;
			}
			else if (str->chars[i] == '\n')
			{
				str->n_rows++;
				col = 0;
			}
			else if (col >= max_width)
			{
				tmp[str->n_chars] = '\n';
				str->n_codepoints++;
				str->n_rows++;
				str->n_chars++;
				col = 1;
			}
			else
			{
				col += str->chars[i] == '\t' ? str->tab_width : 1;
			}
			str->n_codepoints++;
		}
		tmp[str->n_chars++] = str->chars[i];
	}

	free(str->chars);
	str->chars = tmp;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_zero(cstr *str)
{
	if (str->err)
	{
		return;
	}

	memset(str->chars, '\0', str->n_alloc);

	_update_n_values(str);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static size_t
_byte_offset(const cstr *str, size_t offset)
{
	size_t i = 0;

	if (offset >= str->n_codepoints)
	{
		return str->n_chars - 1;
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

static bool
_is_end_byte(char c)
{
	return !!(((uint8_t)c >> 6) ^ 0x02); /* bitmask = 10xxxxxx */
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const char *
_next_codepoint(const char *codepoint)
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

static void
_update_n_values(cstr *str)
{
	size_t col = 0;

	str->n_rows       = 1;
	str->n_cols       = 0;
	str->n_chars      = 0;
	str->n_codepoints = 0;
	
	for (;; str->n_chars++)
	{
		switch (str->chars[str->n_chars])
		{
			case '\0':
				str->n_cols = col > str->n_cols ? col : str->n_cols;
				str->n_chars++;
				return;

			case '\n':
				str->n_cols = col > str->n_cols ? col : str->n_cols;
				str->n_rows++;
				str->n_codepoints++;
				col = 0;
				break;

			case '\t':
				str->n_codepoints++;
				col += str->tab_width;
				break;

			default:
				if (_is_end_byte(str->chars[str->n_chars]))
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
_prev_codepoint(const char *codepoint, const cstr *str)
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
