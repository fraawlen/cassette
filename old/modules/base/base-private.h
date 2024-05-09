/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
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

#ifndef DG_BASE_H_PRIVATE
#define DG_BASE_H_PRIVATE

#include <assert.h>

#include <dg/core/core.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DG_BASE_IS_INIT       assert(dg_base_is_init());
#define DG_BASE_IS_CELL(X, Y) assert(dg_base_is_cell_type(X, Y));

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * These enums should not be given values because they are used as array position
 * DG_BASE_ENUM_END has to stay last as it is used to get the amount of component of this enum 
 */
typedef enum {
	/* cell types */
	DG_BASE_BANNER,
	DG_BASE_BUTTON,
	DG_BASE_GAP,
	DG_BASE_GAUGE,
	DG_BASE_INDICATOR,
	DG_BASE_LABEL,
	DG_BASE_PLACEHOLDER,
	DG_BASE_SPINNER,
	DG_BASE_SLIPBOX,
	DG_BASE_SWITCH,
	DG_BASE_TEXTBOX,
	/* enum end */
	DG_BASE_ENUM_END,
} dg_base_cell_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Get the serial identifier associated to a base cell type.
 *
 * @param type : type of cell to get the serial of
 *
 * @return : self-explanatory
 */
unsigned int dg_base_get_type_serial(dg_base_cell_t type);

/**
 * Checks if the given core/base cell is of the matching type. This is done by comparing the cell's serial to
 * the type associated serial.
 *
 * @param c    : cell to check
 * @param type : base cell type to test against the cell
 *
 * @return : true in case of a match, false otherwhise
 */
bool dg_base_is_cell_type(dg_core_cell_t *c, dg_base_cell_t type);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#endif /* DG_BASE_H_PRIVATE */
