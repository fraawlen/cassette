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

#include <assert.h>
#include <stdlib.h>

#include <dg/core/core.h>
#include <dg/core/errno.h>

#include "base.h"
#include "base-private.h"
#include "config.h"
#include "config-private.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool _init = false;

static unsigned int _type_serials[DG_BASE_ENUM_END] = {0};

/************************************************************************************************************/
/* PUBLIC - MAIN ********************************************************************************************/
/************************************************************************************************************/

void
dg_base_init(void)
{
	assert(!_init);

	/* check if core module has been initialised first and config init */

	if (!dg_core_is_init()) {
		dg_core_errno_set(DG_CORE_ERRNO_DEPENDENCY);
		return;
	}

	if (!dg_base_config_init()) {
		return;
	}

	/* get serials for each base cell type */

	for (size_t i = 0; i < DG_BASE_ENUM_END; i++) {
		_type_serials[i] = dg_core_get_serial();
	}

	/* end of initialisation */

	_init = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_base_is_init(void)
{
	return _init;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_reset(void)
{
	dg_base_config_reset();

	for (size_t i = 0; i < DG_BASE_ENUM_END; i++) {
		_type_serials[i] = 0;
	}

	_init = false;
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

unsigned int
dg_base_get_type_serial(dg_base_cell_t type)
{
	return _type_serials[type];
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_base_is_cell_type(dg_core_cell_t *c, dg_base_cell_t type)
{
	return c && dg_core_cell_get_serial(c) == dg_base_get_type_serial(type);
}
