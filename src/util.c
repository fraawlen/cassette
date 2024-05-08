/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Configuration (CCFG) library.
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

#include "util.h"

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

double
util_limit(double d, double lim_1, double lim_2)
{
	util_sort_pair(&lim_1, &lim_2);

	return d < lim_1 ? lim_1 : (d > lim_2 ? lim_2 : d);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
util_interpolate(double d_1, double d_2, double ratio)
{
	ratio = util_limit(ratio, 0.0, 1.0);

	return d_2 * ratio + d_1 * (1.0 - ratio);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
util_sort_pair(double *d_1, double *d_2)
{
	double tmp;

	if (*d_1 > *d_2)
	{
		tmp  = *d_1;
		*d_1 = *d_2;
		*d_2 = tmp;
	}
}
