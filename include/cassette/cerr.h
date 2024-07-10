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

#pragma once

#include <stdbool.h>
#include <stdlib.h>

#if __GNUC__ > 4
	#define CERR_NONNULL_RETURN __attribute__((returns_nonnull))
	#define CERR_CONST          __attribute__((const))
#else
	#define CERR_NONNULL_RETURN
	#define CERR_CONST
#endif

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 * Cassette errors. Errors values are represented as a bitfield to allow the stacking of multiple errors.
 */
enum cerr
{
	CERR_NONE     = 0,
	CERR_INVALID  = 1,
	CERR_NOT_INIT = 1 << 1,
	CERR_OVERFLOW = 1 << 2,
	CERR_MEMORY   = 1 << 3,
	CERR_INPUT    = 1 << 4,
	CERR_CONFIG   = 1 << 5,
	CERR_XCB      = 1 << 6,
	CERR_CAIRO    = 1 << 7,
	CERR_MUTEX    = 1 << 8,
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif
