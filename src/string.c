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

static bool             _is_end_char     (char c);
static du_string_side_t _opposite_side   (du_string_side_t side);
static void             _update_n_values (du_string_t *str);

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
du_string_attach(du_string_t *str, const du_string_t *str_src, du_string_side_t side)
{
	assert(str && str_src);

	if (str_src->failed)
	{
		return;
	}

	du_string_attach_raw(str, str_src->chars, side);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_attach_raw(du_string_t *str, const char *c_str, du_string_side_t side)
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

	size_t n = strlen(c_str);
	char *tmp;

	/* test for overflow */

	if (n > SIZE_MAX - str->n_bytes)
	{
		str->failed = true;
		return;
	}

	/* build new c-string */

	tmp = malloc(str->n_bytes + n);
	if (!tmp)
	{
		str->failed = true;
		return;
	}

	switch (side)
	{
		case DU_STRING_LEAD:
			strcpy(tmp, c_str);
			strcat(tmp, str->chars);
			break;

		case DU_STRING_TAIL:
			strcpy(tmp, str->chars);
			strcat(tmp, c_str);
			break;

		default:
			free(tmp);
			return;
	}

	free(str->chars);
	str->chars = tmp;
	_update_n_values(str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_clear(du_string_t *str)
{
	assert(str);

	du_string_set_raw(str, "");	
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
	char tmp[25];

	du_string_t *str = du_string_create();

	snprintf(tmp, 25, "%.*f", precision, d);
	du_string_set_raw(str, tmp);

	return str;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

du_string_t *
du_string_create_duplicate(const du_string_t *str)
{
	assert(str);

	if (str->failed)
	{
		return &_err_str;
	}

	du_string_t *str_dup = du_string_create();

	du_string_set(str_dup, str);

	return str_dup;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

du_string_t *
du_string_create_slice(const du_string_t *str, size_t n_codepoints, size_t offset, du_string_side_t side)
{
	assert(str);

	if (str->failed)
	{
		return &_err_str;
	}

	du_string_t *str_slice = du_string_create();

	du_string_set(str_slice, str);
	du_string_trim(str_slice, offset, side);
	du_string_limit(str_slice, n_codepoints, side);

	return str_slice;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_cut(du_string_t *str, size_t offset, size_t n_codepoints, du_string_side_t side)
{
	assert(str);

	if (str->failed)
	{
		return;
	}

	if (n_codepoints == 0 || offset >= str->n_codepoints)
	{
		return;
	}

	if (offset == 0)
	{
		du_string_trim(str, n_codepoints, side);
		return;
	}

	if (n_codepoints > str->n_codepoints)
	{
		du_string_limit(str, offset, side);
		return;
	}

	du_string_t *tmp = du_string_create_duplicate(str);

	du_string_limit(str, offset, side);
	du_string_trim(tmp, offset + n_codepoints, side);
	du_string_attach(str, tmp, _opposite_side(side));

	du_string_destroy(&tmp);
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
		return NULL;
	}

	return str->chars;
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
du_string_insert(du_string_t *str, const du_string_t *str_src, size_t offset, du_string_side_t side)
{
	assert(str);

	if (str_src->failed)
	{
		return;
	}

	du_string_insert_raw(str, str_src->chars, offset, side);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_insert_raw(du_string_t *str, const char *c_str, size_t offset, du_string_side_t side)
{
	assert(str);

	if (str->failed)
	{
		return;
	}

	if (offset == 0)
	{
		du_string_attach_raw(str, c_str, side);
		return;
	}

	if (offset >= str->n_codepoints)
	{
		du_string_attach_raw(str, c_str, _opposite_side(side));
		return;
	}

	du_string_t *tmp = du_string_create_duplicate(str);

	du_string_limit(str, offset, side);
	du_string_trim(tmp, offset, side);
	du_string_attach_raw(str, c_str, _opposite_side(side));
	du_string_attach(str, tmp, _opposite_side(side));

	du_string_destroy(&tmp);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
void
du_string_limit(du_string_t *str, size_t n_codepoints, du_string_side_t side)
{
	assert(str);

	if (str->failed)
	{
		return;
	}

	if (n_codepoints >= str->n_codepoints)
	{
		return;
	}

	du_string_trim(str, str->n_codepoints - n_codepoints, _opposite_side(side));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_pad(du_string_t *str, const char *padder, size_t n_pad, du_string_side_t side)
{
	assert(str && padder);

	if (str->failed)
	{
		return;
	}

	if (n_pad <= str->n_codepoints)
	{
		return;
	}

	size_t n = strlen(padder);
	char *tmp;
	
	n_pad -= str->n_codepoints;

	if (n == 0)
	{
		return;
	}

	/* test for overflow */

	if (n > (SIZE_MAX - 1) / n_pad)
	{
		str->failed = true;
		return;
	}

	/* create padding sequence */

	tmp = malloc(n_pad * n + 1);
	if (!tmp)
	{
		str->failed = true;
		return;
	}

	tmp[0] = '\0';
	for (size_t i = 0; i < n_pad; i++)
	{
		strcat(tmp ,padder);
	}

	/* attach padding sequence to string */

	du_string_attach_raw(str, tmp, side);

	free(tmp);
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

	char *tmp = malloc(strlen(c_str) + 1);
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

size_t
du_string_test_wrap(const du_string_t *str, size_t max_cols)
{
	assert(str && max_cols > 0);

	if (str->failed)
	{
		return 0;
	}

	if (max_cols > str->n_cols)
	{
		return str->n_rows;
	}

	size_t row = 1;
	size_t col = 0;
	size_t n   = 0;

	for (; str->chars[n] != '\0'; n++)
	{
		if (_is_end_char(str->chars[n]))
		{
			if (str->chars[n] == '\n')
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
		}
	}

	return row;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_trim(du_string_t *str, size_t n_codepoints, du_string_side_t side)
{
	assert(str);

	if (str->failed)
	{
		return;
	}

	if (n_codepoints == 0)
	{
		return;
	}

	if (n_codepoints >= str->n_codepoints)
	{
		du_string_clear(str);
		return;
	}

	size_t offset;

	switch (side)
	{
		case DU_STRING_LEAD:
			break;

		case DU_STRING_TAIL:
			goto trim_after;

		default:
			return;
	}

	/* trim before */

	for (offset = 0; n_codepoints > 0; offset++)
	{
		if (_is_end_char(str->chars[offset]))
		{
			n_codepoints--;
		}
	}

	du_string_set_raw(str, str->chars + offset);
	return;

	/* trim after */

trim_after:

	n_codepoints++;
	for (offset = str->n_bytes - 1; n_codepoints > 0; offset--)
	{
		if (_is_end_char(str->chars[offset]))
		{
			n_codepoints--;
		}
	}

	str->chars[offset + 1] = '\0';
	du_string_set_raw(str, str->chars);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
du_string_trim_whitespaces(du_string_t *str)
{
	assert(str);

	if (str->failed)
	{
		return;
	}
	
	size_t offset;

	/* leading whitespaces */

	for (offset = 0;; offset++)
	{
		switch (str->chars[offset])
		{
			case '\t':
			case '\v':
			case ' ' :
				break;

			default:
				du_string_trim(str, offset, DU_STRING_LEAD);
				goto end_lead;
		}
	}

end_lead:

	/* trailing whitespaces */

	if (str->n_bytes < 2)
	{
		return;
	}

	for (offset = str->n_bytes - 2;; offset--)
	{
		switch (str->chars[offset])
		{
			case '\t':
			case '\v':
			case ' ' :
				break;

			default:
				du_string_trim(str, str->n_bytes - 2 - offset, DU_STRING_TAIL);
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

	size_t rows = du_string_test_wrap(str, max_cols);
	size_t col  = 0;
	size_t i    = 0;
	size_t j    = 0;

	char *tmp;

	/* test for overflow */

	if (max_cols > SIZE_MAX - 1 || rows > SIZE_MAX / (max_cols + 1))
	{
		str->failed = true;
		return;
	}

	/* copy the string and insert a newline everytime max_col is reached */

	tmp = malloc((max_cols + 1) * rows);
	if (!tmp)
	{
		str->failed = true;
		return;
	}

	for (; i < str->n_bytes - 1; i++)
	{
		if (_is_end_char(str->chars[i]))
		{
			if (str->chars[i] == '\n')
			{
				col = 0;
			}
			else if (col == max_cols)
			{
				tmp[j++] = '\n';
				col = 1;
			}
			else
			{
				col++;
			}
		}
		tmp[j++] = str->chars[i];
	}
	tmp[j] = '\0';

	free(str->chars);
	str->chars = tmp;
	_update_n_values(str);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static bool
_is_end_char(char c)
{
	return !!(((uint8_t)c >> 6) ^ 0x02); /* bitmask = 10xxxxxx */
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static
du_string_side_t _opposite_side(du_string_side_t side)
{
	switch (side)
	{
		case DU_STRING_LEAD:
			return DU_STRING_TAIL;

		case DU_STRING_TAIL:
			return DU_STRING_LEAD;

		default:
			return side;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_update_n_values(du_string_t *str)
{
	size_t col = 0;
	bool   end = false;

	str->n_rows       = 0;
	str->n_cols       = 0;
	str->n_bytes      = 0;
	str->n_codepoints = 0;

	for (; !end; str->n_bytes++)
	{
		switch (str->chars[str->n_bytes])
		{
			case '\0':
				end = true;
				str->n_codepoints--;
				/* fallthrough */

			case '\n':
				str->n_cols = col > str->n_cols ? col : str->n_cols;
				str->n_rows++;
				str->n_codepoints++;
				col = 0;
				break;

			default:
				if (_is_end_char(str->chars[str->n_bytes]))
				{
					str->n_codepoints++;
					col++;
				}
				break;
		}
	}
}
