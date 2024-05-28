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
#include "config-default.h"
#include "util.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define _STYLE_WINDOW(NAMESPACE, TARGET) \
\
	{ NAMESPACE, "border_thickness",          _LENGTH, &TARGET.thickness_border          }, \
	{ NAMESPACE, "outer_padding",             _LENGTH, &TARGET.padding_outer             }, \
	{ NAMESPACE, "inner_padding",             _LENGTH, &TARGET.padding_inner             }, \
	{ NAMESPACE, "cell_padding",              _LENGTH, &TARGET.padding_cell              }, \
	{ NAMESPACE, "color_background",          _COLOR,  &TARGET.color_background          }, \
	{ NAMESPACE, "color_background_disabled", _COLOR,  &TARGET.color_background_disabled }, \
	{ NAMESPACE, "color_background_focused",  _COLOR,  &TARGET.color_background_focused  }, \
	{ NAMESPACE, "color_background_locked",   _COLOR,  &TARGET.color_background_locked   }, \
	{ NAMESPACE, "color_border",              _COLOR,  &TARGET.color_border              }, \
	{ NAMESPACE, "color_border_disabled",     _COLOR,  &TARGET.color_border_disabled     }, \
	{ NAMESPACE, "color_border_focused",      _COLOR,  &TARGET.color_border_focused      }, \
	{ NAMESPACE, "color_border_locked",       _COLOR,  &TARGET.color_border_locked       }, \
	{ NAMESPACE, "enable_disabled_substyle",  _BOOL,   &TARGET.enable_disabled           }, \
	{ NAMESPACE, "enable_focused_substyle",   _BOOL,   &TARGET.enable_focused            }, \
	{ NAMESPACE, "enable_locked_substyle",    _BOOL,   &TARGET.enable_locked             },

#define _STYLE_CELL(NAMESPACE, TARGET) \
\
	{ NAMESPACE, "border_thickness",          _LENGTH, &TARGET.thickness_border          }, \
	{ NAMESPACE, "outline_thickness",         _LENGTH, &TARGET.thickness_outline         }, \
	{ NAMESPACE, "margin",                    _LENGTH, &TARGET.margin                    }, \
	{ NAMESPACE, "color_background",          _COLOR,  &TARGET.color_background          }, \
	{ NAMESPACE, "color_border",              _COLOR,  &TARGET.color_border              }, \
	{ NAMESPACE, "color_outline",             _COLOR,  &TARGET.color_outline             },

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum _value_t
{
	_STRING,
	_COLOR,
	_BOOL,
	_LENGTH,
	_POSITION,
	_ANTIALIAS,
	_SUBPIXEL,
};

typedef enum _value_t _value_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct _resource_t
{
	char *namespace;
	char *name;
	_value_t type;
	void *target;
};

typedef struct _resource_t _resource_t;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _fetch (const _resource_t *resource);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cgui_config_t _config     = config_default;
static ccfg_t *_config_data      = NULL;
static cobj_dictionary_t *_words = NULL;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const _resource_t _resources[] =
{
	{ "font",   "face",                _STRING,    &_config.font_face                },
	{ "font",   "size",                _LENGTH,    &_config.font_size                },
	{ "font",   "horizontal_spacing",  _LENGTH,    &_config.font_spacing_horizontal  },
	{ "font",   "vertical_spacing",    _LENGTH,    &_config.font_spacing_vertical    },
	{ "font",   "width_override",      _LENGTH,    &_config.font_override_width      },
	{ "font",   "ascent_override",     _LENGTH,    &_config.font_override_ascent     },
	{ "font",   "descent_override",    _LENGTH,    &_config.font_override_descent    },
	{ "font",   "x_offset",            _POSITION,  &_config.font_offset_x            },
	{ "font",   "y_offset",            _POSITION,  &_config.font_offset_y            },
	{ "font",   "enable_overrides",    _BOOL,      &_config.font_enable_overrides    },
	{ "font",   "enable_hint_metrics", _BOOL,      &_config.font_enable_hint_metrics },
	{ "font",   "antialias_mode",      _ANTIALIAS, &_config.font_antialias           },
	{ "font",   "subpixel_mode",       _SUBPIXEL,  &_config.font_subpixel            },

	_STYLE_WINDOW("window", _config.window_style)
	_STYLE_WINDOW("popup",  _config.popup_style)
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

const cgui_config_t *
cgui_config_get(void)
{
	return _config_data ? &_config : &config_default;
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

	cobj_dictionary_write(_words, "none",     _ANTIALIAS, CGUI_CONFIG_ANTIALIAS_NONE);
	cobj_dictionary_write(_words, "gray",     _ANTIALIAS, CGUI_CONFIG_ANTIALIAS_GRAY);
	cobj_dictionary_write(_words, "subpixel", _ANTIALIAS, CGUI_CONFIG_ANTIALIAS_SUBPIXEL);
	cobj_dictionary_write(_words, "rgb",      _SUBPIXEL,  CGUI_CONFIG_SUBPIXEL_RGB);
	cobj_dictionary_write(_words, "bgr",      _SUBPIXEL,  CGUI_CONFIG_SUBPIXEL_BGR);
	cobj_dictionary_write(_words, "vrgb",     _SUBPIXEL,  CGUI_CONFIG_SUBPIXEL_VRGB);
	cobj_dictionary_write(_words, "vbgr",     _SUBPIXEL,  CGUI_CONFIG_SUBPIXEL_VBGR);

	/* config fields */

	_config           = config_default;
	_config.font_face = cobj_string_create();
	_config.init      = true;
	
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
	cobj_string_t *font_face;

	bool fail = false;

	/* set defaults */

	font_face         = _config.font_face;
	_config           = config_default;
	_config.font_face = font_face;

	/* retrieve new resources values */

	ccfg_load(_config_data);
	for (size_t i = 0; i < sizeof(_resources) / sizeof(_resource_t); i++)
	{
		_fetch(_resources + i);
	}

	/* generate remaining resources values */

	printf(">> %u\n", _config.window_style.padding_cell);
	printf(">> %i\n", _config.font_antialias);

	/* end */

	fail |= ccfg_has_failed(_config_data);
	fail |= cobj_string_has_failed(_config.font_face);

	return !fail;
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
_fetch(const _resource_t *resource)
{
	cobj_color_t cl;

	bool err = false;
	size_t s;
	long l;

	ccfg_fetch_resource(_config_data, resource->namespace, resource->name);
	if (!ccfg_pick_next_resource_value(_config_data))
	{
		return;
	}

	switch (resource->type)
	{
		case _STRING:
			cobj_string_set_raw(
				(cobj_string_t*)resource->target,
				ccfg_get_resource_value(_config_data));
			break;

		case _COLOR:
			cl = cobj_color_convert_str(ccfg_get_resource_value(_config_data), &err);
			if (!err)
			{
				*(cobj_color_t*)resource->target = cl;
			}
			break;

		case _BOOL:
			*(bool*)resource->target = strtod(ccfg_get_resource_value(_config_data), NULL) != 0.0;
			break;

		case _LENGTH:
			l = strtoul(ccfg_get_resource_value(_config_data), NULL, 0);
			if (l <= UINT16_MAX)
			{
				*(uint16_t*)resource->target = l;
			}
			break;

		case _POSITION:
			l = strtol(ccfg_get_resource_value(_config_data), NULL, 0);
			if (l <= INT16_MAX && l >= INT16_MIN)
			{
				*(int16_t*)resource->target = l;
			}
			break;

		case _ANTIALIAS:
		case _SUBPIXEL:
			if (cobj_dictionary_find(_words, ccfg_get_resource_value(_config_data), resource->type, &s))
			{
				*(int*)resource->target = s;
			}
			break;

		default:
			return;
	}
}
