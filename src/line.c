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

#include <cassette/cobj.h>
#include <stdbool.h>
#include <stdint.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void    _bind_length (struct cline *)    CLINE_NONNULL(1);
static void    _bind_origin (struct cline *)    CLINE_NONNULL(1);
static int64_t _scale       (int64_t a, double) CLINE_CONST;
static void    _swap_bounds (struct cline *)    CLINE_NONNULL(1);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cline_bind(struct cline *line)
{
	_swap_bounds(line);
	_bind_origin(line);
	_bind_length(line);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cline_grow(struct cline *line, int64_t length)
{
	if (length > 0)
	{
		line->length = line->length > INT64_MAX - length ? INT64_MAX : line->length + length;
	}
	else
	{
		line->length = line->length < INT64_MIN - length ? INT64_MIN : line->length + length;
	}

	_bind_length(line);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cline_is_in(struct cline line, int64_t point)
{
	if (line.length > 0)
	{
		return point >= line.origin && point <= line.origin + line.length;
	}
	else
	{
		return point <= line.origin && point >= line.origin + line.length;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cline_limit(struct cline *line, int64_t lim_1, int64_t lim_2)
{
	line->min = lim_1;
	line->max = lim_2;

	_swap_bounds(line);
	_bind_origin(line);
	_bind_length(line);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cline_move(struct cline *line, int64_t origin)
{
	line->origin = origin;

	_bind_origin(line);
	_bind_length(line);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cline_offset(struct cline *line, int64_t length)
{
	if (length > 0)
	{
		line->origin = line->origin > INT64_MAX - length ? INT64_MAX : line->origin + length;
	}
	else
	{
		line->origin = line->origin < INT64_MIN - length ? INT64_MIN : line->origin + length;
	}
	
	_bind_origin(line);
	_bind_length(line);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cline_pad(struct cline *line, int64_t length)
{
	int64_t len_2;

	if (length > 0)
	{
		len_2 = length > INT64_MAX / 2 ? INT64_MAX : length * 2;
	}	
	else
	{
		len_2 = length < (INT64_MIN + 1) / 2 ? INT64_MIN + 1 : length * 2;
	}

	cline_offset(line, length);
	cline_grow(line, -len_2);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cline_resize(struct cline *line, int64_t length)
{
	line->length = length;

	_bind_length(line);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cline_scale(struct cline *line, double scale)
{
	line->origin = _scale(line->origin, scale);
	line->length = _scale(line->length, scale);

	_bind_origin(line);
	_bind_length(line);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_bind_length(struct cline *line)
{
	if (line->length > 0)
	{
		if (line->origin > 0)
		{
			if (line->length > line->max - line->origin)
			{
				line->length = line->max - line->origin;
			}
		}
		else
		{
			if (line->origin + line->length > line->max)
			{
				line->length = line->max - line->origin;
			}
		}
	}
	else
	{
		if (line->origin > 0)
		{
			if (line->origin + line->length < line->min)
			{
				line->length = line->min - line->origin;
			}
		}
		else
		{
			if (line->length < line->min - line->origin)
			{
				line->length = line->min - line->origin;
			}
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_bind_origin(struct cline *line)
{
	if (line->origin < line->min)
	{
		line->origin = line->min;
	}
	else if (line->origin > line->max)
	{
		line->origin = line->max;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static int64_t
_scale(int64_t a, double scale)
{
	if (scale > 1.0)
	{
		if (a > 0)
		{
			a = a > INT64_MAX / scale ? INT64_MAX : a * scale;
		}
		else
		{
			a = a < INT64_MIN / scale ? INT64_MIN : a * scale;
		}
	}
	else if (scale < -1.0)
	{
		if (a > 0)
		{
			a = a > INT64_MIN / scale ? INT64_MIN : a * scale;
		}
		else
		{
			a = a < INT64_MAX / scale ? INT64_MAX : a * scale;
		}
	}
	else
	{
		a *= scale;
	}

	return a;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_swap_bounds(struct cline *line)
{
	int64_t tmp;

	if (line->min > line->max)
	{
		tmp       = line->min;
		line->min = line->max;
		line->max = tmp;
	}
}
