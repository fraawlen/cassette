/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Graphics (CGUI) library.
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

#pragma once

#include "cgui-attributes.h"
#include "cgui-cell.h"
#include "cgui-types.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

/**
 * Creates a gauge type instance. By default, its min, max, current values, precision and units are
 * respectively set to 0.0, 100.0, 0.0, 0, "%".
 * To destroy it, use the generic cell destructor cgui_cell_destroy().
 *
 * @return     : Created gauge instance
 * @return_err : CGUI_CELL_PLACEHOLDER
 */
cgui_cell *
cgui_gauge_create(void)
CGUI_NONNULL_RETURN;

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 * Aligns the label text within the cursor.
 *
 * @param cell      : Cell to interact with
 * @param alignment : Label alignment within cursor box
 */
void
cgui_gauge_align_label(cgui_cell *cell, enum cgui_align alignment)
CGUI_NONNULL(1);

/**
 * Sets the min and max values used to calculate the gauge's progression. There's no particular order to 
 * these parameters.
 *
 * @param cell  : Cell to interact with
 * @param lim_1 : First boundary
 * @param lim_2 : Second boundary
 *
 * @error CERR_PARAM : invalid input cell type
 */
void
cgui_gauge_clamp_value(cgui_cell *cell, double lim_1, double lim_2)
CGUI_NONNULL(1);

/** 
 * Hides the text label and reduces the cursor size to 0 (unless specified otherwhise by the config).
 *
 * @param cell : Cell to interact with
 *
 * @error CERR_PARAM : invalid input cell type
 */
void
cgui_gauge_hide_label(cgui_cell *cell)
CGUI_NONNULL(1);

/**
 * Rotates the gauge's bar within the frame, thus enabling reversed or vertical gauges.
 *
 * @param cell     : Cell to interact with
 * @param rotation : Rotation of the gauge's bar
 *
 * @error CERR_PARAM : invalid input cell type
 */
void
cgui_gauge_rotate(cgui_cell *cell, enum cgui_rotation rotation)
CGUI_NONNULL(1);

/**
 * Rotates the label within the cursor independently of the bar's rotation.
 *
 * @param cell     : Cell to interact with
 * @param rotation : Rotation of the gauge's text label
 *
 * @error CERR_PARAM : invalid input cell type
 */
void
cgui_gauge_rotate_label(cgui_cell *cell, enum cgui_rotation rotation)
CGUI_NONNULL(1);

/**
 * Sets the numbers of digits to display in the gauge's label.
 *
 * @param cell      : Cell to interact with
 * @param precision : Number of decimal
 *
 * @error CERR_PARAM : invalid input cell type
 */
void
cgui_gauge_set_precision(cgui_cell *cell, int precision)
CGUI_NONNULL(1);

/**
 * Sets a static string to append to the value in the gauge's label. Newlines characters are possible
 * and result in multiline gauge labels.
 *
 * @param cell  : Cell to interact with
 * @param units : NUL terminated string
 *
 * @error CERR_PARAM : invalid input cell type
 */
void
cgui_gauge_set_units(cgui_cell *cell, const char *units)
CGUI_NONNULL(1, 2);

/**
 * Sets the value of the gauge and updates the bar's progression.
 *
 * @param cell  : Cell to interact with
 * @param value : Value
 *
 * @error CERR_PARAM : invalid input cell type
 */
void
cgui_gauge_set_value(cgui_cell *cell, double value)
CGUI_NONNULL(1);

/** 
 * Shows the label and resizes the cursor to fit it.
 *
 * @param cell : Cell to interact with
 *
 * @error CERR_PARAM : invalid input cell type
 */
void
cgui_gauge_show_label(cgui_cell *cell)
CGUI_NONNULL(1);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

