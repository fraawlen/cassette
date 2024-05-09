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

#ifndef DG_CORE_CONFIG_H
#define DG_CORE_CONFIG_H

#include <stdbool.h>
#include <stdint.h>

#include "color.h"
#include "resource.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DG_CORE_CONFIG_MAX_ACCELS  12
#define DG_CORE_CONFIG_MAX_BUTTONS 12
#define DG_CORE_CONFIG_MAX_KEYS    200

#define DG_CORE_CONFIG dg_core_config_get()

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Modifier definition.
 * Matches X11's modmasks, see https://xcb.freedesktop.org/manual/group__XCB____API.html
 */
typedef enum dg_core_config_modkey_t {
	DG_CORE_CONFIG_MOD_SHIFT = 1U << 0, /* cannot be used as modkey config option */
	DG_CORE_CONFIG_MOD_LOCK  = 1U << 1, /* cannot be used as modkey config option */
	DG_CORE_CONFIG_MOD_CTRL  = 1U << 2, 
	DG_CORE_CONFIG_MOD_1     = 1U << 3, 
	DG_CORE_CONFIG_MOD_2     = 1U << 4, /* cannot be used as modkey config option */
	DG_CORE_CONFIG_MOD_3     = 1U << 5, /* cannot be used as modkey config option */
	DG_CORE_CONFIG_MOD_4     = 1U << 6,
	DG_CORE_CONFIG_MOD_5     = 1U << 7, /* cannot be used as modkey config option */
} dg_core_config_modkey_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Anti-aliasing levels to be applied to text.
 * From cairo font options.
 */
typedef enum {
	DG_CORE_CONFIG_ANTIALIAS_NONE,
	DG_CORE_CONFIG_ANTIALIAS_GRAY,
	DG_CORE_CONFIG_ANTIALIAS_SUBPIXEL,
} dg_core_config_font_antialias_t;

/**
 * Font subpixel orders to be applied to text.
 * From cairo font options.
 */
typedef enum {
	DG_CORE_CONFIG_SUBPIXEL_RGB,
	DG_CORE_CONFIG_SUBPIXEL_BGR,
	DG_CORE_CONFIG_SUBPIXEL_VRGB,
	DG_CORE_CONFIG_SUBPIXEL_VBGR,
} dg_core_config_font_subpixel_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * input-swap kinds
 */
typedef enum {
	DG_CORE_CONFIG_SWAP_TO_DEFAULT = 0,
	DG_CORE_CONFIG_SWAP_TO_NONE,
	DG_CORE_CONFIG_SWAP_TO_VALUE,
	DG_CORE_CONFIG_SWAP_TO_ACCELERATOR,
	DG_CORE_CONFIG_SWAP_TO_CLIPBOARD_CUT,
	DG_CORE_CONFIG_SWAP_TO_CLIPBOARD_COPY,
	DG_CORE_CONFIG_SWAP_TO_CLIPBOARD_PASTE,
	DG_CORE_CONFIG_SWAP_TO_ACTION_CELL,
	DG_CORE_CONFIG_SWAP_TO_ACTION_FOCUS,
	DG_CORE_CONFIG_SWAP_TO_ACTION_WINDOW,
	DG_CORE_CONFIG_SWAP_TO_ACTION_MISC,
} dg_core_config_swap_kind_t;

/**
 * Specific post-input-swap actions values for DG_CORE_CONFIG_SWAP_TO_ACTION_CELL,
 * DG_CORE_CONFIG_SWAP_TO_ACTION_FOCUS, DG_CORE_CONFIG_SWAP_TO_ACTION_WINDOW and
 * DG_CORE_CONFIG_SWAP_TO_ACTION_MISC swap kinds. Other swap kinds use unamed numeral values.
 */
typedef enum {
	DG_CORE_CONFIG_ACTION_CELL_REDRAW,
	DG_CORE_CONFIG_ACTION_CELL_SELECT_LESS,
	DG_CORE_CONFIG_ACTION_CELL_SELECT_MORE,
	DG_CORE_CONFIG_ACTION_CELL_SELECT_NONE,
	DG_CORE_CONFIG_ACTION_CELL_SELECT_ALL,
	DG_CORE_CONFIG_ACTION_CELL_TRIGGER_1,
	DG_CORE_CONFIG_ACTION_CELL_TRIGGER_2,
	DG_CORE_CONFIG_ACTION_CELL_TRIGGER_3,
	DG_CORE_CONFIG_ACTION_CELL_TRIGGER_4,
	DG_CORE_CONFIG_ACTION_CELL_TRIGGER_5,
	DG_CORE_CONFIG_ACTION_FOCUS_LEFT,
	DG_CORE_CONFIG_ACTION_FOCUS_RIGHT,
	DG_CORE_CONFIG_ACTION_FOCUS_UP,
	DG_CORE_CONFIG_ACTION_FOCUS_DOWN,
	DG_CORE_CONFIG_ACTION_FOCUS_LEFTMOST,
	DG_CORE_CONFIG_ACTION_FOCUS_RIGHTMOST,
	DG_CORE_CONFIG_ACTION_FOCUS_TOP,
	DG_CORE_CONFIG_ACTION_FOCUS_BOTTOM,
	DG_CORE_CONFIG_ACTION_FOCUS_NEXT,
	DG_CORE_CONFIG_ACTION_FOCUS_PREV,
	DG_CORE_CONFIG_ACTION_FOCUS_FIRST,
	DG_CORE_CONFIG_ACTION_FOCUS_LAST,
	DG_CORE_CONFIG_ACTION_FOCUS_NONE,
	DG_CORE_CONFIG_ACTION_WINDOW_LOCK_GRID,
	DG_CORE_CONFIG_ACTION_WINDOW_LOCK_FOCUS,
	DG_CORE_CONFIG_ACTION_WINDOW_REDRAW,
	DG_CORE_CONFIG_ACTION_MISC_RECONFIG,
	DG_CORE_CONFIG_ACTION_MISC_EXIT,
} dg_core_config_action_t;

/**
 * Input swap definition
 *
 * @param kind  : kind of the value to swap-to
 * @param value : value of the input post-swap
 */
typedef struct {
	dg_core_config_swap_kind_t kind;
	uint8_t value;
} dg_core_config_swap_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Identifiers of each resource group used by the core configuration.
 */
typedef enum {
	DG_CORE_CONFIG_MAIN,
	DG_CORE_CONFIG_BUTTON,
	DG_CORE_CONFIG_KEY,
} dg_core_config_group_t;

/**
 * Configuration of the core module.
 * While the core module does not write text on screen, it still uses font data for layouting.
 *
 * @param scale                    : overall scale of the UI
 * @param mod_meta                 : meta-key to access keybinds and shortcuts
 * @param ft_face                  : fontconfig font name
 * @param ft_overrides             : enable font overrides (see ft_override_*)
 * @param ft_size                  : post-scaling value, fontconfig font size
 * @param ft_offset_x              : post-scaling value, text position correction
 * @param ft_offset_y              : post-scaling value, text position correction
 * @param ft_spacing_w             : post-scaling value, horizontal spacing between characters
 * @param ft_spacing_h             : post-scaling value, vertical spacing between characters
 * @param ft_override_ascent       : post-scaling value, font ascent  manual override
 * @param ft_override_descent      : post-scaling value, font descent manual override
 * @param ft_override_pw           : post-scaling value, font width   manual override
 * @param ft_ascent                : post-scaling value, calculated font ascent  (after overrides were applied)
 * @param ft_descent               : post-scaling value, calculated font descent (after overrides were applied)
 * @param ft_pw                    : post-scaling value, calculated font width   (after overrides were applied)
 * @param ft_ph                    : post-scaling value, calculated font height  (after overrides were applied)
 * @param ft_hint_metrics          : enable font hint metrics
 * @param ft_antialias             : font antialiasing type
 * @param ft_subpixel              : font subpixel order
 * @param win_focused_on_activate  : cope option in case the wm does not automatically focus a new window
 * @param win_dynamic_bd           : show window states by coloring the window's border
 * @param win_thick_bd             : post-scaling value, pixel thickness of the window border
 * @param win_pad_outer            : post-scaling value, pixel padding between the window border and cells
 * @param win_pad_inner            : post-scaling value, pixel padding separating the cells
 * @param win_pad_cell             : post-scaling value, pixel padding inside the cells
 * @param win_cl_bg                : window background color
 * @param win_cl_bd                : window border color in its default state
 * @param win_cl_bd_focused        : window border color when it's focused
 * @param win_cl_bd_disabled       : window border color when it's disabled
 * @param win_cl_bd_locked         : window border color when its current panel is locked by the user
 * @param popup_override_position  : force popups to spawn at specific screen coordinates
 * @param popup_override_width     : force popups to spawn with a specific width
 * @param popup_override_height    : force popups to spawn with a specific height
 * @param popup_max_pw             : not affected by scale, maximum allowed popup pixel width
 * @param popup_max_ph             : not affected by scale, maximum allowed popup pixel height
 * @param popup_override_px        : not affected by scale, fixed popup x screen pixel position
 * @param popup_override_py        : not affected by scale, fixed popup y screen pixel position
 * @param popup_override_pw        : not affected by scale, fixed popup pixel width
 * @param popup_override_ph        : not affected by scale, fixed popup pixel height
 * @param cell_auto_lock           : allow cells to request focus locks in response to events
 * @param input_persistent_pointer : keep focus after the pointer leaves the cell's area
 * @param input_persistent_touch   : keep focus after the first touch ends
 * @param anim_divider             : framerate divider based on the screen's refresh rate, 0 to unsync
 * @param swap_key                 : swap-map for keyboard inputs
 * @param swap_but                 : swap-map for pointer button inputs
 */
typedef struct {
	double scale;
	dg_core_config_modkey_t mod_meta;
	/* font */
	char ft_face[DG_CORE_RESOURCE_STR_LEN];
	bool ft_overrides;
	int16_t ft_size;
	int16_t ft_offset_x;
	int16_t ft_offset_y;
	int16_t ft_spacing_w;
	int16_t ft_spacing_h;
	int16_t ft_override_ascent;
	int16_t ft_override_descent;
	int16_t ft_override_pw;
	int16_t ft_ascent;
	int16_t ft_descent;
	int16_t ft_pw;
	int16_t ft_ph;
	bool ft_hint_metrics;
	dg_core_config_font_antialias_t ft_antialias;
	dg_core_config_font_subpixel_t ft_subpixel;
	/* window */
	bool win_focused_on_activate;
	bool win_dynamic_bd;
	int16_t win_thick_bd;
	int16_t win_pad_outer;
	int16_t win_pad_inner;
	int16_t win_pad_cell;
	dg_core_color_t win_cl_bg;
	dg_core_color_t win_cl_bd;
	dg_core_color_t win_cl_bd_focused;
	dg_core_color_t win_cl_bd_disabled;
	dg_core_color_t win_cl_bd_locked;
	/* popup */
	bool popup_override_position;
	bool popup_override_width;
	bool popup_override_height;
	int16_t popup_max_pw;
	int16_t popup_max_ph;
	int16_t popup_override_px;
	int16_t popup_override_py;
	int16_t popup_override_pw;
	int16_t popup_override_ph;
	/* misc */
	bool cell_auto_lock;
	bool input_persistent_pointer;
	bool input_persistent_touch;
	unsigned int anim_divider;
	/* input swaps */
	dg_core_config_swap_t swap_key[DG_CORE_CONFIG_MAX_KEYS    + 1][3];
	dg_core_config_swap_t swap_but[DG_CORE_CONFIG_MAX_BUTTONS + 1][3];
} dg_core_config_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Access the module's configuration. If the configuration has not been loaded (either through dg_core_init()
 * in the main core.h module header or manually with the help of dg_core_config_get_group_copy()), then its
 * values wont be properly initialised.
 *
 * @return : self-explanatory
 */
const dg_core_config_t *dg_core_config_get(void);

/**
 * In case loading the configuration is needed without using the main core.h module header, this function
 * gives a copy of the resource groups internally used to load them manually.
 * Note that there are 3 groups, and to obtain the full config all 3 need to be loaded. The main group has
 * all the "normal properties" while the button and key groups respectively manage the button and key swap
 * tables.
 * The returned groups are copies and not pointers to the internal groups. Therefore they should not interfere
 * with the internal definitions and is possible to use both the copies and the built-in groups
 * (throuth the main core.h header) without interference, thought it is redundant to do so.
 *
 * @param group : specific group to request a copy of
 *
 * @return : self-explanatory
 */
const dg_core_resource_group_t dg_core_config_get_group_copy(dg_core_config_group_t group);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Converts a given string height in characters columns into an equivalent pixel value.
 * To obtain a proper result the config needs to be initialised one way or another.
 * Using a negative ch value is equivalent to calling dg_core_config_convert_str_width(-ch) and can be useful
 * for vertical text.
 *
 * @param cw : char height of string
 *
 * @return : pixel height of the string
 */
int16_t dg_core_config_convert_str_height(int16_t ch);

/**
 * Converts a given string width in characters columns into an equivalent pixel value.
 * To obtain a proper result the config needs to be initialised one way or another.
 * Using a negative cw value is equivalent to calling dg_core_config_convert_str_height(-cw) and can be useful
 * for vertical text.
 *
 * @param cw : char width of string
 *
 * @return : pixel width of the string
 */
int16_t dg_core_config_convert_str_width(int16_t cw);

/**
 * For a given pixel height, returns the maximym amount of text rows that can fully fit inside.
 * To obtain a proper result the config needs to be initialised one way or another.
 *
 * @param ph : pixel height
 *
 * @return : max char height of potential string
 */
int16_t dg_core_config_fit_str_height(int16_t ph);

/**
 * For a given pixel width, returns the maximym amount of text columns that can fully fit inside.
 * To obtain a proper result the config needs to be initialised one way or another.
 *
 * @param ph : pixel width
 *
 * @return : max char width of potential string
 */
int16_t dg_core_config_fit_str_width(int16_t pw);

/**
 * Gets the pixel height of a cell for a given char height. Includes cell padding.
 * To obtain a proper result the config needs to be initialised one way or another.
 * Using a negative ch value is equivalent to calling dg_core_config_get_cell_width(-ch) and can be useful
 * for vertical text.
 *
 * @param n : width the of cell in amount of chars it should be able to show, has to be > 0
 *
 * @return : self-explanatory
 */
int16_t dg_core_config_get_cell_height(int16_t ch);

/**
 * Gets the pixel width of a cell for a given char width. includes cell padding.
 * To obtain a proper result the config needs to be initialised one way or another.
 * Using a negative cw value is equivalent to calling dg_core_config_get cell_height(-cw) and can be useful
 * for vertical text.
 *
 * @param n : width the of cell in amount of chars it should be able to show, has to be > 0
 *
 * @return : self-explanatory
 */
int16_t dg_core_config_get_cell_width(int16_t cw);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DG_CORE_CONFIG_H */
