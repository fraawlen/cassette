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

static void    _bind_length (struct cseg *)   CSEG_NONNULL(1);
static void    _bind_origin (struct cseg *)   CSEG_NONNULL(1);
static int64_t _scale       (int64_t, double) CSEG_CONST;
static void    _swap_bounds (struct cseg *)   CSEG_NONNULL(1);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cseg_bind(struct cseg *seg)
{
	_swap_bounds(seg);
	_bind_origin(seg);
	_bind_length(seg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cseg_grow(struct cseg *seg, int64_t length)
{
	if (length > 0)
	{
		seg->length = seg->length > INT64_MAX - length ? INT64_MAX : seg->length + length;
	}
	else
	{
		seg->length = seg->length < INT64_MIN - length ? INT64_MIN : seg->length + length;
	}

	_bind_length(seg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cseg_is_in(struct cseg seg, int64_t point)
{
	if (seg.length > 0)
	{
		return point >= seg.origin && point <= seg.origin + seg.length;
	}
	else
	{
		return point <= seg.origin && point >= seg.origin + seg.length;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cseg_limit(struct cseg *seg, int64_t lim_1, int64_t lim_2)
{
	seg->min = lim_1;
	seg->max = lim_2;

	_swap_bounds(seg);
	_bind_origin(seg);
	_bind_length(seg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cseg_move(struct cseg *seg, int64_t origin)
{
	seg->origin = origin;

	_bind_origin(seg);
	_bind_length(seg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cseg_offset(struct cseg *seg, int64_t length)
{
	if (length > 0)
	{
		seg->origin = seg->origin > INT64_MAX - length ? INT64_MAX : seg->origin + length;
	}
	else
	{
		seg->origin = seg->origin < INT64_MIN - length ? INT64_MIN : seg->origin + length;
	}
	
	_bind_origin(seg);
	_bind_length(seg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cseg_pad(struct cseg *seg, int64_t length)
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

	cseg_offset(seg, length);
	cseg_grow(seg, -len_2);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cseg_resize(struct cseg *seg, int64_t length)
{
	seg->length = length;

	_bind_length(seg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cseg_scale(struct cseg *seg, double scale)
{
	seg->origin = _scale(seg->origin, scale);
	seg->length = _scale(seg->length, scale);

	_bind_origin(seg);
	_bind_length(seg);
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
_bind_length(struct cseg *seg)
{
	if (seg->length > 0)
	{
		if (seg->origin > 0)
		{
			if (seg->length > seg->max - seg->origin)
			{
				seg->length = seg->max - seg->origin;
			}
		}
		else
		{
			if (seg->origin + seg->length > seg->max)
			{
				seg->length = seg->max - seg->origin;
			}
		}
	}
	else
	{
		if (seg->origin > 0)
		{
			if (seg->origin + seg->length < seg->min)
			{
				seg->length = seg->min - seg->origin;
			}
		}
		else
		{
			if (seg->length < seg->min - seg->origin)
			{
				seg->length = seg->min - seg->origin;
			}
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_bind_origin(struct cseg *seg)
{
	if (seg->origin < seg->min)
	{
		seg->origin = seg->min;
	}
	else if (seg->origin > seg->max)
	{
		seg->origin = seg->max;
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
_swap_bounds(struct cseg *seg)
{
	int64_t tmp;

	if (seg->min > seg->max)
	{
		tmp      = seg->min;
		seg->min = seg->max;
		seg->max = tmp;
	}
}
