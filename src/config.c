/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Graphics (CGUI) library.
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
#include <pwd.h>
#include <stdbool.h>
#include <stdio.h>
#include <unistd.h>

#include <cassette/ccfg.h>
#include <cassette/cgui.h>
#include <cassette/cobj.h>

#include "config.h"
#include "util.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

enum _word_group_t
{
	_WORD_MODKEY,
	_WORD_SWAP_TYPE,
	_WORD_ACTION_CELL,
	_WORD_ACTION_FOCUS,
	_WORD_ACTION_WINDOW,
	_WORD_ACTION_MISC,
	_WORD_ANTIALIAS,
	_WORD_SUBPIXEL,
};

typedef enum _word_group_t _word_group_t;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _apply_defaults     (void);
static void _apply_fetched      (void);
static void _apply_generated    (void);
static void _fetch_boolean      (const char *namespace, const char *name, bool *value);
static void _fetch_color        (const char *namespace, const char *name, cobj_color_t *value);
static void _fetch_length       (const char *namespace, const char *name, uint16_t *value);
static void _fetch_position     (const char *namespace, const char *name, int16_t *value);
static void _fetch_string       (const char *namespace, const char *name, cobj_string_t *value);
static void _fetch_style_window (const char *namespace, cgui_style_window_t *style);
static void _fetch_word         (const char *namespace, const char *name, _word_group_t group, int *value);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static ccfg_t *_config_data = NULL;

static cobj_dictionary_t *_words = NULL;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static cgui_config_t _config =
{
	.init = false,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

const cgui_config_t *
cgui_config_get(void)
{
	return &_config;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

ccfg_t *
cgui_config_get_object(void)
{
	return _config_data ? _config_data : ccfg_get_placeholder();
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

bool
config_init(void)
{
	cobj_string_t *home;

	bool fail = false;

	/* parser */

	_config_data = ccfg_create();
	home  = cobj_string_create();

	cobj_string_set_raw(home, util_env_exists("HOME") ? getenv("HOME") : getpwuid(getuid())->pw_dir);
	cobj_string_append_raw(home, "/.config/cgui.conf");

	ccfg_push_source(_config_data, getenv("CGUI_CONFIG_SOURCE"));
	ccfg_push_source(_config_data, cobj_string_get_chars(home));
	ccfg_push_source(_config_data, "/usr/share/cgui/cgui.conf");
	ccfg_push_source(_config_data, "/etc/cgui.conf");

	cobj_string_destroy(&home);

	/* dictionary */

	_words = cobj_dictionary_create(5, 0.6);

	cobj_dictionary_write(_words, "none",     _WORD_ANTIALIAS, CGUI_CONFIG_ANTIALIAS_NONE);
	cobj_dictionary_write(_words, "gray",     _WORD_ANTIALIAS, CGUI_CONFIG_ANTIALIAS_GRAY);
	cobj_dictionary_write(_words, "subpixel", _WORD_ANTIALIAS, CGUI_CONFIG_ANTIALIAS_SUBPIXEL);
	cobj_dictionary_write(_words, "rgb",      _WORD_SUBPIXEL,  CGUI_CONFIG_SUBPIXEL_RGB);
	cobj_dictionary_write(_words, "bgr",      _WORD_SUBPIXEL,  CGUI_CONFIG_SUBPIXEL_BGR);
	cobj_dictionary_write(_words, "vrgb",     _WORD_SUBPIXEL,  CGUI_CONFIG_SUBPIXEL_VRGB);
	cobj_dictionary_write(_words, "vbgr",     _WORD_SUBPIXEL,  CGUI_CONFIG_SUBPIXEL_VBGR);

	/* config fields */

	_config.font_face = cobj_string_create();
	_config.init = true;
	
	/* end */

	fail |= ccfg_has_failed(_config_data);
	fail |= cobj_string_has_failed(_config.font_face);
	fail |= cobj_dictionary_has_failed(_words);

	return !fail;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
config_load(void)
{
	ccfg_load(_config_data);

	_apply_defaults();
	_apply_fetched();
	_apply_generated();

	return !ccfg_has_failed(_config_data) && !cobj_string_has_failed(_config.font_face);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
config_reset(void)
{
	ccfg_destroy(&_config_data);
	cobj_dictionary_destroy(&_words);
	cobj_string_destroy(&_config.font_face);

	_config.init = false;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_apply_defaults(void)
{
	/* font */

	cobj_string_set_raw(_config.font_face, "Monospace");

	_config.font_size                = 14;
	_config.font_spacing_horizontal  = 0;
	_config.font_spacing_vertical    = 2;
	_config.font_offset_x            = 0;
	_config.font_offset_y            = 0;
	_config.font_override_width      = 7;
	_config.font_override_ascent     = 14;
	_config.font_override_descent    = 0;
	_config.font_enable_overrides    = false;
	_config.font_enable_hint_metrics = true;
	_config.font_antialias           = CGUI_CONFIG_ANTIALIAS_SUBPIXEL;
	_config.font_subpixel            = CGUI_CONFIG_SUBPIXEL_RGB;

	/* window */

	_config.window_style.thickness_border = 2;
	_config.window_style.padding_outer    = 10;
	_config.window_style.padding_inner    = 10;
	_config.window_style.padding_cell     = 10;

	_config.window_style.color_background          = (cobj_color_t){0.000, 0.000, 0.000, 1.000};
	_config.window_style.color_background_disabled = (cobj_color_t){0.000, 0.000, 0.000, 1.000};
	_config.window_style.color_background_focused  = (cobj_color_t){0.000, 0.000, 0.000, 1.000};
	_config.window_style.color_background_locked   = (cobj_color_t){0.000, 0.000, 0.000, 1.000};
	_config.window_style.color_border              = (cobj_color_t){0.000, 0.000, 0.000, 1.000};
	_config.window_style.color_border_disabled     = (cobj_color_t){0.000, 0.000, 0.000, 1.000};
	_config.window_style.color_border_focused      = (cobj_color_t){0.000, 0.000, 0.000, 1.000};
	_config.window_style.color_border_locked       = (cobj_color_t){0.000, 0.000, 0.000, 1.000};

	/* popup */

	_config.popup_style.thickness_border = 2;
	_config.popup_style.padding_outer    = 10;
	_config.popup_style.padding_inner    = 10;
	_config.popup_style.padding_cell     = 10;

	_config.popup_style.color_background          = (cobj_color_t){0.000, 0.000, 0.000, 1.000};
	_config.popup_style.color_background_disabled = (cobj_color_t){0.000, 0.000, 0.000, 1.000};
	_config.popup_style.color_background_focused  = (cobj_color_t){0.000, 0.000, 0.000, 1.000};
	_config.popup_style.color_background_locked   = (cobj_color_t){0.000, 0.000, 0.000, 1.000};
	_config.popup_style.color_border              = (cobj_color_t){0.000, 0.000, 0.000, 1.000};
	_config.popup_style.color_border_disabled     = (cobj_color_t){0.000, 0.000, 0.000, 1.000};
	_config.popup_style.color_border_focused      = (cobj_color_t){0.000, 0.000, 0.000, 1.000};
	_config.popup_style.color_border_locked       = (cobj_color_t){0.000, 0.000, 0.000, 1.000};
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_apply_fetched(void)
{
	/* font */

	_fetch_string   ("font", "face",                 _config.font_face);
	_fetch_length   ("font", "size",                &_config.font_size);
	_fetch_length   ("font", "horizontal_spacing",  &_config.font_spacing_horizontal);
	_fetch_length   ("font", "vertical_spacing",    &_config.font_spacing_vertical);
	_fetch_length   ("font", "width_override",      &_config.font_override_width);
	_fetch_length   ("font", "ascent_override",     &_config.font_override_ascent);
	_fetch_length   ("font", "descent_override",    &_config.font_override_descent);
	_fetch_position ("font", "x_offset",            &_config.font_offset_x);
	_fetch_position ("font", "y_offset",            &_config.font_offset_y);
	_fetch_boolean  ("font", "enable_overrides",    &_config.font_enable_overrides);
	_fetch_boolean  ("font", "enable_hint_metrics", &_config.font_enable_hint_metrics);

	_fetch_word ("font", "antialias", _WORD_ANTIALIAS, (int*)&_config.font_antialias);
	_fetch_word ("font", "subpixel",  _WORD_SUBPIXEL,  (int*)&_config.font_subpixel);

	/* window */

	_fetch_style_window("window", &_config.window_style);

	/* popup */

	_fetch_style_window("popup", &_config.popup_style);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_apply_generated(void)
{
	printf(">> %u\n", _config.window_style.padding_cell);
	printf(">> %i\n", _config.font_antialias);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_fetch_boolean(const char *namespace, const char *name, bool *value)
{
	ccfg_fetch_resource(_config_data, namespace, name);
	if (ccfg_pick_next_resource_value(_config_data))
	{
		*value = strtod(ccfg_get_resource_value(_config_data), NULL) != 0.0;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_fetch_color(const char *namespace, const char *name, cobj_color_t *value)
{
	cobj_color_t tmp = {0};

	bool err = false;

	ccfg_fetch_resource(_config_data, namespace, name);
	if (ccfg_pick_next_resource_value(_config_data))
	{
		tmp = cobj_color_convert_str(ccfg_get_resource_value(_config_data), &err);
		if (!err)
		{
			*value = tmp;
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_fetch_length(const char *namespace, const char *name, uint16_t *value)
{
	unsigned long tmp = 0;

	ccfg_fetch_resource(_config_data, namespace, name);
	if (ccfg_pick_next_resource_value(_config_data))
	{
		tmp = strtoul(ccfg_get_resource_value(_config_data), NULL, 0);
		if (tmp <= UINT16_MAX)
		{
			*value = tmp;
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_fetch_position(const char *namespace, const char *name, int16_t *value)
{
	long tmp = 0;

	ccfg_fetch_resource(_config_data, namespace, name);
	if (ccfg_pick_next_resource_value(_config_data))
	{
		tmp = strtol(ccfg_get_resource_value(_config_data), NULL, 0);
		if (tmp <= INT16_MAX && tmp >= INT16_MIN)
		{
			*value = tmp;
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_fetch_string(const char *namespace, const char *name, cobj_string_t *value)
{
	ccfg_fetch_resource(_config_data, namespace, name);
	if (ccfg_pick_next_resource_value(_config_data))
	{
		cobj_string_set_raw(value, ccfg_get_resource_value(_config_data));
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_fetch_style_window(const char *namespace, cgui_style_window_t *style)
{
	_fetch_length(namespace, "border_thickness", &style->thickness_border);
	_fetch_length(namespace, "outer_padding",    &style->padding_outer);
	_fetch_length(namespace, "inner_padding",    &style->padding_inner);
	_fetch_length(namespace, "cell_padding",     &style->padding_cell);

	_fetch_color(namespace, "color_background", &style->color_background);
	_fetch_color(namespace, "color_border",     &style->color_border);

	style->color_background_disabled = style->color_background;
	style->color_background_focused  = style->color_background;
	style->color_background_locked   = style->color_background;
	style->color_border_disabled     = style->color_border;
	style->color_border_focused      = style->color_border;
	style->color_border_locked       = style->color_border;

	_fetch_color(namespace, "color_background_disabled", &style->color_background_disabled);
	_fetch_color(namespace, "color_background_focused",  &style->color_background_focused);
	_fetch_color(namespace, "color_background_locked",   &style->color_background_locked);
	_fetch_color(namespace, "color_boder_disabled",      &style->color_border_disabled);
	_fetch_color(namespace, "color_border_focused",      &style->color_border_focused);
	_fetch_color(namespace, "color_border_locked",       &style->color_border_locked);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_fetch_word(const char *namespace, const char *name, _word_group_t group, int *value)
{
	size_t tmp;

	ccfg_fetch_resource(_config_data, namespace, name);
	if (ccfg_pick_next_resource_value(_config_data))
	{
		if (cobj_dictionary_find(_words, ccfg_get_resource_value(_config_data), group, &tmp))
		{
			*value = tmp;
		}
	}
}
