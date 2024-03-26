/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Resources (DR) library.
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
#include <stdio.h>
#include <math.h>

#include "rand.h"
#include "util.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define _MAX 140737488355327.0

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

double
dr_rand_get(dr_rand_t *r, double lim_1, double lim_2)
{
	const unsigned long long m = 140737488355328ULL;
	const unsigned long long a = 25214903917ULL;
	const unsigned long long c = 11ULL;

	assert(r);

	dr_util_sort_pair(&lim_1, &lim_2);

	*r = (a * (*r) + c) % m;

	return lim_1 + (*r) / _MAX * (lim_2 - lim_1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dr_rand_seed(dr_rand_t *r, unsigned long long seed)
{
	assert(r);

	*r = dr_rand_get(&seed, 0, _MAX);
}

