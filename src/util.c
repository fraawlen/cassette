/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Graphics (CGUI) library.
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

#include <stdbool.h>
#include <stdlib.h>

#include "util.h"

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

bool
util_env_exists(const char *name)
{
	char *val;

	val = getenv(name);

	return val && val[0] != '\0';
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
util_is_in(double x_test, double y_test, double x, double y, double width, double height)
{
	if (width < 0.0)
	{
		width *= -1.0;
		x     -= width;
	}

	if (height < 0.0)
	{
		height *= -1.0;
		y      -= height;
	}

	return x_test >= x && x_test < x + width &&
	       y_test >= y && y_test < y + height;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
util_limit(double d, double min, double max)
{
	return d > max ? max : (d < min ? min : d);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
util_str_to_double(const char *str, double min, double max)
{
	return util_limit(strtod(str, NULL), min, max);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

long
util_str_to_long(const char *str, long min, long max)
{
	long l;

	l = strtoul(str, NULL, 0);

	return l > max ? max : (l < min ? min : l);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

unsigned long
util_time(void)
{
	struct timespec ts = {0};

	clock_gettime(CLOCK_MONOTONIC, &ts);

	return ts.tv_sec * 1000000 + ts.tv_nsec / 1000;
}
