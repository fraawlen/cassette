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

#ifndef DG_BASE_BUTTON_H
#define DG_BASE_BUTTON_H

#include <dg/core/core.h>

#include "../origin.h"
#include "../zone.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

typedef enum {
	DG_BASE_BUTTON_ICON_NONE,
	DG_BASE_BUTTON_ICON_YES,
	DG_BASE_BUTTON_ICON_NO,
	DG_BASE_BUTTON_ICON_NEUTRAL,
	DG_BASE_BUTTON_ICON_LINK_IN,
	DG_BASE_BUTTON_ICON_LINK_OUT,
	DG_BASE_BUTTON_ICON_CROSS,
	DG_BASE_BUTTON_ICON_PLUS,
	DG_BASE_BUTTON_ICON_MINUS,
	DG_BASE_BUTTON_ICON_LEFT,
	DG_BASE_BUTTON_ICON_RIGHT,
	DG_BASE_BUTTON_ICON_UP,
	DG_BASE_BUTTON_ICON_DOWN,
	DG_BASE_BUTTON_ICON_CUSTOM,
} dg_base_button_icon_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Instantiates a button-type cell.
 *
 * @return : created cell, NULL in case of failure
 *
 * @error DG_CORE_ERRNO_MEMORY : inherited from dg_core_cell_create()
 * @error DG_CORE_ERRNO_STACK  : inherited from dg_core_cell_create()
 */
dg_core_cell_t *dg_base_button_create(void);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * TODO description
 */
void dg_base_button_set_icon(dg_core_cell_t *c, dg_base_button_icon_t icon);

/**
 * TODO description
 */
void dg_base_button_set_label(dg_core_cell_t *c, const char *str);

/**
 * TODO description
 */
void dg_base_button_set_label_origin(dg_core_cell_t *c, dg_base_origin_t og);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * TODO description
 */
void dg_base_button_set_callback_icon(dg_core_cell_t *c, void (*fn)(dg_core_cell_t *c, dg_base_zone_t *z));

/**
 * TODO description
 */
void dg_base_button_set_callback_pressed(dg_core_cell_t *c, void (*fn)(dg_core_cell_t *c));

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DG_BASE_BUTTON_H */
