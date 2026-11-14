/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cassette/cobj.h>
#include <stdbool.h>
#include <stdckdint.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define GUARD(OBJ, ...) if (!OBJ || cerr_critical(OBJ->err)) { return __VA_OPT__(__VA_ARGS__); }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cstr
{
	char *bytes;
	size_t n_rows;
	size_t n_cols;
	size_t n_bytes;
	size_t n_alloc;
	size_t n_codepoints;
	size_t tab_width;
	int digits;
	enum cerr err;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static size_t byte_offset     (const cstr *, size_t);
static bool   is_head_byte    (uint8_t);
static size_t tab_real_width  (size_t, size_t);
static void   update_n_values (cstr *);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

size_t
cstr_byte_length(const cstr *str)
{
	GUARD(str, 0);

	return str->n_bytes;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cstr_byte_offset(const cstr *str, size_t offset)
{
	GUARD(str, 0);

	return byte_offset(str, offset);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
cstr_bytes(const cstr *str)
{
	GUARD(str, "");

	return str->bytes;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
cstr_bytes_at_coords(const cstr *str, size_t row, size_t col)
{
	GUARD(str, "");

	return str->bytes + byte_offset(str, cstr_coords_offset(str, row, col));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
cstr_bytes_at_offset(const cstr *str, size_t offset)
{
	GUARD(str, "");

	return str->bytes + byte_offset(str, offset);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_clear(cstr *str)
{
	GUARD(str);

	str->bytes[0] = '\0';

	update_n_values(str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_clear_warnings(cstr *str)
{
	GUARD(str);

	cerr_clear_warnings(&str->err);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cstr *
cstr_clone(const cstr *str)
{
	GUARD(str, nullptr);

	cstr *str_new;

	if (!(str_new = malloc(sizeof(cstr))))
	{
		return nullptr;
	}

	if (!(str_new->bytes = malloc(str->n_alloc)))
	{
		free(str_new);
		return nullptr;
	}

	memcpy(str_new->bytes, str->bytes, str->n_bytes);

	str_new->n_rows       = str->n_rows;
	str_new->n_cols       = str->n_cols;
	str_new->n_bytes      = str->n_bytes;
	str_new->n_codepoints = str->n_codepoints;
	str_new->n_alloc      = str->n_alloc;
	str_new->tab_width    = str->tab_width;
	str_new->digits       = str->digits;
	str_new->err          = str->err;
	
	return str_new;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cstr_coords_offset(const cstr *str, size_t row, size_t col)
{
	GUARD(str, 0);

	const char *codepoint = str->bytes;
	size_t      offset    = 0;

	if (row >= str->n_rows)
	{
		row = str->n_rows - 1;
	}

	if (col > str->n_cols)
	{
		col = str->n_cols;
	}

	/* skip rows */

	while (row > 0)
	{
		if (*codepoint == '\n')
		{
			row--;
		}
		codepoint = cstr_next_codepoint(codepoint);
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
				if (col <= tab_real_width(str->tab_width, offset))
				{
					return offset;
				}
				col -= tab_real_width(str->tab_width, offset);
				break;

			default:
				col--;
				break;
		}
		codepoint = cstr_next_codepoint(codepoint);
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
		return nullptr;
	}

	if (!(str->bytes = malloc(1)))
	{
		free(str);
		return nullptr;
	}

	str->bytes[0]  = '\0';
	str->n_alloc   = 1;
	str->tab_width = 1;
	str->digits    = 0;
	str->err       = CERR_NONE;
	
	update_n_values(str);

	return str;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_cut(cstr *str, size_t offset, size_t length)
{
	GUARD(str);

	size_t offset_2;

	if (offset >= str->n_codepoints || length == 0)
	{
		return;
	}

	if (length > str->n_codepoints - offset)
	{
		length = str->n_codepoints - offset;
	}

	offset_2 = byte_offset(str, offset + length);
	offset   = byte_offset(str, offset);

	memmove(str->bytes + offset, str->bytes + offset_2, str->n_bytes - offset_2);

	update_n_values(str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

nullptr_t
cstr_destroy(cstr *str)
{
	if (str)
	{
		free(str->bytes);
		free(str);
	}

	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cerr
cstr_error(const cstr *str)
{
	return str ? str->err : CERR_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cstr_height(const cstr *str)
{
	GUARD(str, 0);

	return str->n_rows;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_insert_bytes(cstr *str, const char *bytes, size_t offset)
{
	GUARD(str);

	if (!bytes)
	{
		return;
	}

	size_t n = strlen(bytes);

	/* extend allocated memory if needed */

	size_t m;

	if (ckd_add(&m, n, str->n_bytes))
	{
		cerr_set(&str->err, CERR_OVERFLOW);
		return;
	}

	if (m > str->n_alloc && !CUTIL_REALLOC(str->bytes, str->n_alloc, m, 1, str->err))
	{
		return;
	}
	
	/* detect overlapping memory areas */

	char *tmp = nullptr;

	if (bytes >= str->bytes && bytes <= str->bytes + str->n_alloc)
	{
		if (!(tmp = strdup(bytes)))
		{
			cerr_set(&str->err, CERR_MEMORY);
			return;
		}
		bytes = tmp;
	}

	/* insert */

	offset = byte_offset(str, offset);

	memmove(str->bytes + offset + n, str->bytes + offset, str->n_bytes - offset);
	memcpy(str->bytes + offset, bytes, n);
	
	update_n_values(str);
	free(tmp);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_insert_double(cstr *str, double d, size_t offset)
{
	char tmp[64];

	snprintf(tmp, 64, "%.*f", str->digits, d);

	cstr_insert_bytes(str, tmp, offset);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_insert_long(cstr *str, long long l, size_t offset)
{
	char tmp[64];

	snprintf(tmp, 64, "%lli", l);

	cstr_insert_bytes(str, tmp, offset);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_insert_str(cstr *str, const cstr *str_src, size_t offset)
{
	GUARD(str);
	GUARD(str_src);

	cstr_insert_bytes(str, str_src->bytes, offset);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cstr_length(const cstr *str)
{
	GUARD(str, 0);

	return str->n_codepoints;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
cstr_next_codepoint(const char *byte)
{
	if (*byte != '\0')
	{
		do
		{
			byte++;
		}
		while (!is_head_byte(*byte));
	}

	return byte;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
cstr_next_row(const char *byte, size_t *row_width)
{
	size_t tab_width = 1; /* hardcoded until proper tab support */
	size_t col = 0;

	for (const char *codepoint = byte;; codepoint = cstr_next_codepoint(codepoint))
	{
		switch (*codepoint)
		{
			case '\n':
				codepoint++;
				/* fallthrough */

			case '\0':
				if (row_width)
				{
					*row_width = col;
				}
				return codepoint;

			case '\t':
				col += tab_real_width(tab_width, col);
				break;

			default:
				col++;
				break;
		}
	}

	return byte;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_pad(cstr *str, const char *pattern, size_t offset, size_t length)
{
	GUARD(str);

	size_t n_codepoints = 0;
	size_t length_diff;
	size_t n;
	size_t i;
	size_t j;
	char *tmp;

	if (length <= str->n_codepoints || !pattern || pattern[0] == '\0')
	{
		return;
	}

	length_diff = length - str->n_codepoints;

	/* alloc memory for the padding string */

	if (ckd_mul(&n, length_diff, 4) || ckd_add(&n, n, 1))
	{
		cerr_set(&str->err, CERR_OVERFLOW);
		return;
	}

	if (!(tmp = calloc(n, 1)))
	{
		cerr_set(&str->err, CERR_MEMORY);
		return;
	}

	/* build padding string */
	
	for (i = 0, j = 0;;)
	{
		if (is_head_byte(pattern[j]))
		{
			if (pattern[j] == '\0')
			{
				j = 0;
			}
			if (n_codepoints++ >= length_diff)
			{
				break;
			}
		}
		tmp[i++] = pattern[j++];
	}

	/* insert it */

	cstr_insert_bytes(str, tmp, offset);

	free(tmp);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_prealloc(cstr *str, size_t bytes_number)
{
	GUARD(str);

	if (bytes_number > str->n_alloc)
	{
		CUTIL_REALLOC(str->bytes, str->n_alloc, bytes_number, 1, str->err);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cstr_row_width(const cstr *str, size_t row)
{
	GUARD(str, 0);

	const char *codepoint = str->bytes;
	size_t width = 0;

	if (row >= str->n_rows)
	{
		row = str->n_rows - 1;
	}

	do
	{
		codepoint = cstr_next_row(codepoint, &width);
	}
	while (row-- > 0);

	return width;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_set_precision(cstr *str, int digits)
{
	GUARD(str);

	str->digits = digits;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * TODO temporarily disabled until proper tab support is built in CGUI
 *
void
cstr_set_tab_width(cstr *str, size_t width)
{
	if (str->err || str->tab_width == width)
	{
		return;
	}

	str->tab_width = width;

	update_n_values(str);
}
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_slice(cstr *str, size_t offset, size_t length)
{
	GUARD(str);

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
cstr_test_wrap(const cstr *str, size_t width)
{
	GUARD(str, 0);

	size_t row = 1;
	size_t col = 0;

	if (width == 0)
	{
		return 0;
	}

	if (width >= str->n_cols)
	{
		return str->n_rows;
	}

	for (const char *codepoint = str->bytes; *codepoint != '\0'; codepoint = cstr_next_codepoint(codepoint))
	{
		if (*codepoint == '\n')
		{
			col = 0;
			row++;
		}
		else if (*codepoint == '\t')
		{
			col += tab_real_width(str->tab_width, col);
		}
		else if (col >= width)
		{
			col = 1;
			row++;
		}
		else
		{
			col++;
		}
	}

	return row;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_trim(cstr *str)
{
	GUARD(str);

	/* leading whitespaces */

	for (size_t i = 0;; i++)
	{
		switch (str->bytes[i])
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

	if (str->n_bytes < 2)
	{
		return;
	}

	for (size_t i = str->n_bytes - 2;; i--)
	{
		switch (str->bytes[i])
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
	GUARD(str, 0);
	GUARD(str_wrap, 0);

	const char *codepoint_1;
	const char *codepoint_2;
	size_t diff = 0;

	if (offset >= str_wrap->n_codepoints)
	{
		return str->n_codepoints;
	}

	codepoint_1 = str->bytes;
	codepoint_2 = str_wrap->bytes;

	for (size_t i = 0; i < offset; i++)
	{
		if (*codepoint_1 != '\n' && *codepoint_2 == '\n')
		{
			diff++;
		}
		else
		{
			codepoint_1 = cstr_next_codepoint(codepoint_1);
		}

		codepoint_2 = cstr_next_codepoint(codepoint_2);
	}

	return offset - diff;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cstr_width(const cstr *str)
{
	GUARD(str, 0);

	return str->n_cols;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_wrap(cstr *str, size_t width)
{
	GUARD(str);

	size_t col;
	size_t n;
	char *tmp;

	if (width >= str->n_cols)
	{
		return;
	}

	if (width == 0)
	{
		cerr_set(&str->err, CERR_PARAM);
		return;
	}

	/* alloc memory */

	if (ckd_mul(&n, str->n_alloc, 2))
	{
		cerr_set(&str->err, CERR_OVERFLOW);
		return;
	}

	if (!(tmp = malloc(n)))
	{
		cerr_set(&str->err, CERR_MEMORY);
		return;
	}

	str->n_alloc = n;

	/* wrap string */

	str->n_cols       = 0;
	str->n_rows       = 1;
	str->n_bytes      = 0;
	str->n_codepoints = 0;

	col = 0;

	for (size_t i = 0;; i++)
	{
		if (is_head_byte(str->bytes[i]))
		{
			if (str->bytes[i] == '\0')
			{
				tmp[str->n_bytes++] = str->bytes[i];
				break;
			}
			else if (str->bytes[i] == '\n')
			{
				str->n_rows++;
				col = 0;
			}
			else if (str->bytes[i] == '\t')
			{
				col += tab_real_width(str->tab_width, col);
			}
			else if (col >= width)
			{
				tmp[str->n_bytes] = '\n';
				str->n_cols = col > str->n_cols ? col : str->n_cols;
				str->n_codepoints++;
				str->n_rows++;
				str->n_bytes++;
				col = 1;
			}
			else
			{
				col++;
			}
			str->n_codepoints++;
		}
		tmp[str->n_bytes++] = str->bytes[i];
	}

	free(str->bytes);
	str->bytes = tmp;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cstr_zero(cstr *str)
{
	GUARD(str);

	memset(str->bytes, '\0', str->n_alloc);

	update_n_values(str);
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static size_t
byte_offset(const cstr *str, size_t offset)
{
	const char *codepoint = str->bytes;

	while (offset > 0 && *(codepoint = cstr_next_codepoint(codepoint)) != '\0')
	{
		offset--;
	}

	return codepoint - str->bytes;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
is_head_byte(uint8_t c)
{
	/*
	 * UTF-8 :
	 *
	 * 0xxxxxxx
	 * 110xxxxx 10xxxxxx
	 * 1110xxxx 10xxxxxx 10xxxxxx
	 * 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
	 */

	return (c & 0xC0) != 0x80;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static size_t
tab_real_width(size_t tab_width, size_t col)
{
	return tab_width - (col % tab_width);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
update_n_values(cstr *str)
{
	size_t col = 0;

	str->n_rows       = 1;
	str->n_cols       = 0;
	str->n_bytes      = 0;
	str->n_codepoints = 0;
	
	for (;; str->n_bytes++)
	{
		switch (str->bytes[str->n_bytes])
		{
			case '\0':
				str->n_cols = col > str->n_cols ? col : str->n_cols;
				str->n_bytes++;
				return;

			case '\n':
				str->n_cols = col > str->n_cols ? col : str->n_cols;
				str->n_rows++;
				str->n_codepoints++;
				col = 0;
				break;

			case '\t':
				str->n_codepoints++;
				col += tab_real_width(str->tab_width, col);
				break;

			default:
				if (is_head_byte(str->bytes[str->n_bytes]))
				{
					str->n_codepoints++;
					col++;
				}
				break;
		}
	}
}
