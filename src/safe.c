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

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include "safe.h"

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

bool
safe_add(size_t *result, size_t a, size_t b)
{
	bool safe;

	safe = a <= SIZE_MAX - b;

	if (result)
	{
		*result = safe ? a + b : a;
	}

	return safe;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
safe_div(size_t *result, size_t a, size_t b)
{
	bool safe;

	safe = b != 0;

	if (result)
	{
		*result = safe ? a / b : a;
	}

	return safe;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
safe_mul(size_t *result, size_t a, size_t b)
{
	bool safe;

	safe = a <= SIZE_MAX / b;

	if (result)
	{
		*result = safe ? a * b : a;
	}

	return safe;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
safe_sub(size_t *result, size_t a, size_t b)
{
	bool safe;

	safe = a > b;

	if (result)
	{
		*result = safe ? a - b : a;
	}

	return safe;
}
