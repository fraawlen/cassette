/**
 * Copyright © 2024 Frawwlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Graphics (DG) GUI library.
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

#include "errno.h"
#include "util.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/* persistent data */

static dg_core_errno_t _last_err = DG_CORE_ERRNO_NONE;

static unsigned int _n_err = 0;

static void (*_callback)(dg_core_errno_t err) = NULL;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/* consts */

static const dg_core_util_dict_t _err_str[] = {
	{ "no errors",                           DG_CORE_ERRNO_NONE       },
	{ "memory allocation failed",            DG_CORE_ERRNO_MEMORY     },
	{ "memory allocation on stack failed",   DG_CORE_ERRNO_STACK      },
	{ "memory allocation on hashmap failed", DG_CORE_ERRNO_HASHTABLE  },
	{ "cairo operation(s) failed",           DG_CORE_ERRNO_CAIRO      },
	{ "critical cairo operation(s) failed",  DG_CORE_ERRNO_CAIRO_CRIT },
	{ "xcb operation(s) failed",             DG_CORE_ERRNO_XCB        },
	{ "critical xcb operation(s) failed",    DG_CORE_ERRNO_XCB_CRIT   },
	{ "dependency requirements are not met", DG_CORE_ERRNO_DEPENDENCY },
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

dg_core_errno_t
dg_core_errno_get(void)
{
	const dg_core_errno_t err = _last_err;

	_last_err = DG_CORE_ERRNO_NONE;

	return err;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

dg_core_errno_t
dg_core_errno_get_no_reset(void)
{
	return _last_err;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

unsigned int
dg_core_errno_count(void)
{
	return _n_err;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_errno_set(dg_core_errno_t err)
{
	if (err == DG_CORE_ERRNO_NONE) {
		return;
	}

	_last_err = err;
	_n_err++;

	if (_callback) {
		_callback(err);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
dg_core_errno_to_str(dg_core_errno_t err)
{
	const char *s = dg_core_util_dict_inverse_match(_err_str, DG_CORE_UTIL_DICT_LEN(_err_str), err);

	return s ? s : "unknown dg-core error code";
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_errno_set_callback(void (*fn)(dg_core_errno_t err))
{
	_callback = fn;
}
