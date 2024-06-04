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

#include <assert.h>
#include <limits.h>
#include <stdlib.h>

#include <cassette/cobj.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static int  _add      (int a,   int b);
static int  _bind_len (int pos, int len);
static bool _is_in    (int pos, int len, int a);
static int  _scale    (int a,   double scale);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cobj_rect_bind(cobj_rect_t *rect)
{
	assert(rect);

	rect->width  = _bind_len(rect->x, rect->width);
	rect->height = _bind_len(rect->y, rect->height);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cobj_rect_t
cobj_rect_create(int x, int y, int width, int height)
{
	cobj_rect_t rect =
	{
		.x      = x,
		.y      = y,
		.width  = _bind_len(x, width),
		.height = _bind_len(y, height),
	};

	return rect;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cobj_rect_grow(cobj_rect_t *rect, int width, int height)
{
	assert(rect);

	rect->width  = _bind_len(rect->x, _add(rect->width,  width));
	rect->height = _bind_len(rect->y, _add(rect->height, height));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cobj_rect_offset(cobj_rect_t *rect, int x, int y)
{
	assert(rect);

	rect->x      = _add(rect->x, x);
	rect->y      = _add(rect->y, y);
	rect->width  = _bind_len(rect->x, rect->width);
	rect->height = _bind_len(rect->y, rect->height);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cobj_rect_pad(cobj_rect_t *rect, int padding)
{
	assert(rect);

	rect->x      = _add(rect->x, padding);
	rect->y      = _add(rect->y, padding);
	rect->width  = _bind_len(rect->x, _add(rect->width,  _scale(padding, -2.0)));
	rect->height = _bind_len(rect->y, _add(rect->height, _scale(padding, -2.0)));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cobj_rect_scale(cobj_rect_t *rect, double scale)
{
	assert(rect);

	rect->x      = _scale(rect->x, scale);
	rect->y      = _scale(rect->y, scale);
	rect->width  = _bind_len(rect->x, _scale(rect->width,  scale));
	rect->height = _bind_len(rect->y, _scale(rect->height, scale));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cobj_rect_test_bounds(cobj_rect_t rect, int x, int y)
{
	return _is_in(rect.x, rect.width, x) && _is_in(rect.y, rect.height, y);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static int
_add(int a, int b)
{
	if (b > 0)
	{
		return a > INT_MAX - b ? INT_MAX : a + b;
	}
	else
	{
		return a < INT_MIN - b ? INT_MIN : a + b;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static int
_bind_len(int pos, int len)
{
	if (len > 0)
	{
		return pos > INT_MAX - len ? INT_MAX - pos : len;
	}
	else
	{
		return pos < INT_MIN - len ? INT_MIN - pos : len;	
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_is_in(int pos, int len, int a)
{
	if (len > 0)
	{
		return a >= pos && a <= pos + len;
	}
	else
	{
		return a <= pos && a >= pos + len;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static int
_scale(int a, double scale)
{
	if (scale >= -1.0 && scale <= 1.0)
	{
		return a * scale;
	}
	else
	{
		return abs(a) > abs((int)(INT_MAX / scale)) ? (a / scale > 0.0 ? INT_MAX : INT_MIN) : a * scale;
	}
}
