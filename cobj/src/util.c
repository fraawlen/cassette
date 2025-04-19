/**
 * Copyright © 2024-2025 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette library.
 *
 * This library is free software; you can redistribute it and/or modify it either under the terms of the GNU
 * Lesser General Public License as published by the Free Software Foundation; either version 3.0 of the
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
#include <float.h>
#include <math.h>
#include <stdbool.h>
#include <stdckdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <time.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

bool
cutil_env_exists(const char *name)
{
	char *val;

	return name && (val = getenv(name)) && val[0] != '\0';
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
cutil_clamp(double d, double lim_1, double lim_2)
{
	cutil_sort_pair(&lim_1, &lim_2);

	return d < lim_1 || isnan(d) ? lim_1 : (d > lim_2 || isinf(d) ? lim_2 : d);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
cutil_interpolate(double d_1, double d_2, double ratio)
{
	ratio = cutil_clamp(ratio, 0.0, 1.0);

	return d_2 * ratio + d_1 * (1.0 - ratio);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cutil_point_inside(double x_check, double y_check, double x, double y, double width, double height)
{
	if (width < 0.0)
	{
		width *= -1;
		x -= width;
	}

	if (height < 0.0)
	{
		height *= -1;
		y -= height;
	}

	return !(x_check < x || x_check > x + width || y_check < y || y_check > y + height);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
cutil_ratio(double d, double lim_1, double lim_2)
{
	cutil_sort_pair(&lim_1, &lim_2);

	return lim_2 - lim_1 < DBL_EPSILON ? 1.0 : (cutil_clamp(d, lim_1, lim_2) - lim_1) / (lim_2 - lim_1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cutil_realloc(void **ptr, size_t *n_store, size_t n_new, size_t size, enum cerr *err)
{
	void *tmp;
	size_t r;

	if (!ptr)
	{
		return false;
	}

	if (n_new == 0 || size == 0)
	{
		cerr_set(err, CERR_PARAM);
		return false;
	}

	if (ckd_mul(&r, n_new, size))
	{
		cerr_set(err, CERR_OVERFLOW);
		return false;
	}

	if (!(tmp = realloc(*ptr, r)))
	{
		cerr_set(err, CERR_MEMORY);
		return false;
	}

	if (n_store)
	{
		*n_store = n_new;
	}

	*ptr = tmp;

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cutil_sort_pair(double *d_1, double *d_2)
{
	double tmp;

	if (d_1 && d_2 && *d_1 > *d_2)
	{
		tmp  = *d_1;
		*d_1 = *d_2;
		*d_2 = tmp;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
cutil_str_to_double(const char *str, double lim_1, double lim_2)
{
	return str ? cutil_clamp(strtod(str, NULL), lim_1, lim_2) : lim_1;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

long
cutil_str_to_long(const char *str, long lim_1, long lim_2)
{
	return str ? cutil_clamp(strtoul(str, NULL, 0), lim_1, lim_2) : lim_1;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

unsigned long
cutil_time(void)
{
	struct timespec ts = {0};

	clock_gettime(CLOCK_MONOTONIC, &ts);

	return ts.tv_sec * 1000000 + ts.tv_nsec / 1000;
}
