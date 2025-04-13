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
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cerr_clear_warnings(enum cerr *err)
{
	if (err && !cerr_critical(*err))
	{
		*err = CERR_NONE;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cerr_critical(enum cerr code)
{
	switch (code)
	{
		case CERR_NONE:
		case CERR_PARAM:
			return false;

		default:
			return true;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
cerr_name(enum cerr code)
{
	switch (code)
	{
		case CERR_NONE:
			return "CERR_NONE";

		case CERR_PARAM:
			return "CERR_PARAM";

		case CERR_INVALID:
			return "CERR_INVALID";

		case CERR_OVERFLOW:
			return "CERR_OVERFLOW";

		case CERR_MEMORY:
			return "CERR_MEMORY";

		case CERR_CONFIG:
			return "CERR_CONFIG";

		case CERR_XCB:
			return "CERR_XCB";

		case CERR_CAIRO:
			return "CERR_CAIRO";

		case CERR_MUTEX:
			return "CERR_MUTEX";

		case CERR_INSTANCE:
			return "CERR_INSTANCE";

		case CERR_MALFORMED:
			return "CERR_MALFORMED";

		default:
			return "UNKNOWN";
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cerr_set(enum cerr *err, enum cerr code)
{
	if (err && (*err == CERR_NONE || (!cerr_critical(*err) && cerr_critical(code))))
	{
		*err = code;
	}
}
