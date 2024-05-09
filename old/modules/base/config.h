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

#ifndef DG_BASE_CONFIG_H
#define DG_BASE_CONFIG_H

#include <stdbool.h>
#include <stdint.h>

#include <dg/core/color.h>
#include <dg/core/resource.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

#define DG_BASE_CONFIG dg_base_config_get()

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Common colors identifiers names.
 */
typedef enum {
	DG_BASE_CONFIG_COLOR_BLACK          = 0,
	DG_BASE_CONFIG_COLOR_RED            = 1,
	DG_BASE_CONFIG_COLOR_GREEN          = 2,
	DG_BASE_CONFIG_COLOR_YELLOW         = 3,
	DG_BASE_CONFIG_COLOR_BLUE           = 4,
	DG_BASE_CONFIG_COLOR_MAGENTA        = 5,
	DG_BASE_CONFIG_COLOR_CYAN           = 6,
	DG_BASE_CONFIG_COLOR_WHITE          = 7,
	DG_BASE_CONFIG_COLOR_BRIGHT_BLACK   = 8,
	DG_BASE_CONFIG_COLOR_BRIGHT_RED     = 9,
	DG_BASE_CONFIG_COLOR_BRIGHT_GREEN   = 10,
	DG_BASE_CONFIG_COLOR_BRIGHT_YELLOW  = 11,
	DG_BASE_CONFIG_COLOR_BRIGHT_BLUE    = 12,
	DG_BASE_CONFIG_COLOR_BRIGHT_MAGENTA = 13,
	DG_BASE_CONFIG_COLOR_BRIGHT_CYAN    = 14,
	DG_BASE_CONFIG_COLOR_BRIGHT_WHITE   = 15,
	DG_BASE_CONFIG_COLOR_DEFAULT        = 16,
} dg_base_config_color_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Bundle of common cell styling attributes. Cells do not necessarily use all of them at once.
 *
 * @param ft_bold      : use bold font for text
 * @param thick_bd     : border thickness
 * @param thick_sep    : separator thickness
 * @param thick_lim    : limiter (when components are cut off) thickness
 * @param thick_icon   : vector line thickness of icons
 * @param margin       : cell's body inner offset from the max given area
 * @param pad_fg       : padding that defines the foreground area of custom components
 * @param pad_icon     : padding that defines the icon area
 * @param gap_lim      : gap between the limiter and cut-off components
 * @param anim_speed   : animation speed, its interpreation is cell specific
 * @param cl_bg        : color of the background
 * @param cl_bd        : color of the border
 * @param cl_ft        : color of the font
 * @param cl_sep       : color of the separator
 * @param cl_lim       : color of the limiter
 * @param cl_primary   : 1st arbitrary color for custom components
 * @param cl_secondary : 2nd arbitrary color for custom components
 * @param cl_highlight : special arbitrary color for custom components
 */
typedef struct {
	bool ft_bold;
	int16_t thick_bd;
	int16_t thick_sep;
	int16_t thick_lim;
	int16_t thick_icon;
	int16_t margin;
	int16_t pad_fg;
	int16_t pad_icon;
	int16_t gap_lim;
	unsigned long anim_speed;
	dg_core_color_t cl_bg;
	dg_core_color_t cl_bd;
	dg_core_color_t cl_ft;
	dg_core_color_t cl_sep;
	dg_core_color_t cl_lim;
	dg_core_color_t cl_primary;
	dg_core_color_t cl_secondary;
	dg_core_color_t cl_highlight;
} dg_base_config_style_t;

 /**
  * Configuration of the base module. Hosts visual information for cells to draw themselves.
  *
  * @param focus_thick           : thickness of the focus box
  * @param focus_margin          : focus's offset from the cell's area
  * @param focus_cl_primary_lock : color of the primary focus box when its locked
  * @param focus_cl_primary      : color of the primary focus box
  * @param focus_cl_secondary    : color of the secondary focus box
  * @param common_cl             : 16 common colors mapped similarly to .Xresources's colors
  * @param button_style          : button styling attributes for 4 states : idle, focused, pressed, disabled
  * @param gap_style             : gap styling attributes
  * @param gauge_style           : gauge styling attributes for 3 states : default, full, unknown
  * @param indicator_style       : indicator styling attributes for 4 state : off, on, critical_off, critical_on
  * @param label_style           : label styling attributes
  * @param placeholder_style     : placeholder styling attributes
  * @param spinner_style         : spinner styling attributes
  * @param switch_style          : switch styling attributes for 4 states : idle, focused, pressed, disabled
  * @param label_bg_cl_ratio     : color interpolation ratio for the cell's background when a color is applied
  */
typedef struct {
	/* focus */
	int16_t focus_thick;
	int16_t focus_margin;
	dg_core_color_t focus_cl_primary_lock;
	dg_core_color_t focus_cl_primary;
	dg_core_color_t focus_cl_secondary;
	/* common stuff */
	dg_core_color_t common_cl[16];
	/* styles */
	dg_base_config_style_t button_style[4];
	dg_base_config_style_t gap_style;
	dg_base_config_style_t gauge_style[3];
	dg_base_config_style_t indicator_style[4];
	dg_base_config_style_t label_style;
	dg_base_config_style_t placeholder_style;
	dg_base_config_style_t spinner_style;
	dg_base_config_style_t switch_style[4];
	/* styles extras */
	double label_bg_cl_ratio;
} dg_base_config_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Access the module's configuration. If the configuration has not been loaded (either through dg_base_init()
 * in the main core.h module header or manually with the help of dg_base_config_get_group_copy()), then its
 * values will not be properly initialised.
 *
 * @return : self-explanatory
 */
const dg_base_config_t *dg_base_config_get(void);

/**
 * In case loading the configuration is needed without using the main base.h module header, this function
 * gives a copy of the resource group internally used to load it manually.
 * Since this is copy and not a pointer to the internal group it will not interfere will it, and it is
 * possible to use both the copy and the built-in group (throuth the main base.h header) without
 * interference, thought it is redundant to do so.
 *
 * @return : self-explanatory
 */
const dg_core_resource_group_t dg_base_config_get_group_copy(void);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DG_BASE_CONFIG_H */
