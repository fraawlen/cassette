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
#include <float.h>
#include <limits.h>
#include <pwd.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <cairo/cairo.h>

#include <cassette/ccfg.h>
#include <cassette/cgui.h>
#include <cassette/cobj.h>

#include "config.h"
#include "config-default.h"
#include "env.h"
#include "util.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define _SCALE(X) X *= _config.scale;

#define _SCALE_CELL(C) \
	C.thickness_border  *= _config.scale; \
	C.thickness_outline *= _config.scale; \
	C.margin            *= _config.scale;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

#define _STYLE_CELL(NAMESPACE, TARGET) \
	{ NAMESPACE, "border_thickness",  _LENGTH, &TARGET.thickness_border  }, \
	{ NAMESPACE, "outline_thickness", _LENGTH, &TARGET.thickness_outline }, \
	{ NAMESPACE, "margin",            _LENGTH, &TARGET.margin            }, \
	{ NAMESPACE, "color_background",  _COLOR,  &TARGET.color_background  }, \
	{ NAMESPACE, "color_border",      _COLOR,  &TARGET.color_border      }, \
	{ NAMESPACE, "color_outline",     _COLOR,  &TARGET.color_outline     },

#define _KEY(VALUE) \
	{ "key",     #VALUE, _MAP_KEY, &_config.keys[VALUE][CGUI_CONFIG_SWAP_DIRECT] }, \
	{ "key", "M" #VALUE, _MAP_KEY, &_config.keys[VALUE][CGUI_CONFIG_SWAP_MOD]    }, \
	{ "key", "S" #VALUE, _MAP_KEY, &_config.keys[VALUE][CGUI_CONFIG_SWAP_SHIFT]  },

#define _BUTTON(VALUE) \
	{ "button",     #VALUE, _MAP_BUTTON, &_config.buttons[VALUE][CGUI_CONFIG_SWAP_DIRECT] }, \
	{ "button", "M" #VALUE, _MAP_BUTTON, &_config.buttons[VALUE][CGUI_CONFIG_SWAP_MOD]    }, \
	{ "button", "S" #VALUE, _MAP_BUTTON, &_config.buttons[VALUE][CGUI_CONFIG_SWAP_SHIFT]  },

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum _value_t
{
	/* primitives */

	_STRING,
	_COLOR,
	_BOOL,
	_POSITION,
	_LENGTH,
	_LONG,
	_ULONG,
	_DOUBLE,
	_UDOUBLE,
	_RATIO,

	/* dict based */

	_MODKEY,
	_FONT_ANTIALIAS,
	_FONT_SUBPIXEL,
	_SWAP_KIND,
	_SWAP_ACTION,

	/* composite */

	_MAP_KEY,
	_MAP_BUTTON,
};

typedef enum _value_t _value_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct _word_t
{
	char *name;
	_value_t type;
	size_t value;
};

typedef struct _word_t _word_t;

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
static void _swap  (const char *str, uint8_t limit, cgui_input_swap_t *target);
static bool _fill  (void);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cgui_config_t _config    = config_default;
static ccfg_t *_parser          = NULL;
static cobj_dictionary_t *_dict = NULL;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const _word_t _words[] =
{
	{ "mod1",       _MODKEY,         CGUI_CONFIG_MOD_1                  },
	{ "mod4",       _MODKEY,         CGUI_CONFIG_MOD_4                  },
	{ "ctrl",       _MODKEY,         CGUI_CONFIG_MOD_CTRL               },

	{ "none",       _FONT_ANTIALIAS, CGUI_CONFIG_ANTIALIAS_NONE         },
	{ "gray",       _FONT_ANTIALIAS, CGUI_CONFIG_ANTIALIAS_GRAY         },
	{ "subpixel",   _FONT_ANTIALIAS, CGUI_CONFIG_ANTIALIAS_SUBPIXEL     },

	{ "rgb",        _FONT_SUBPIXEL,  CGUI_CONFIG_SUBPIXEL_RGB           },
	{ "bgr",        _FONT_SUBPIXEL,  CGUI_CONFIG_SUBPIXEL_BGR           },
	{ "vrgb",       _FONT_SUBPIXEL,  CGUI_CONFIG_SUBPIXEL_VRGB          },
	{ "vbgr",       _FONT_SUBPIXEL,  CGUI_CONFIG_SUBPIXEL_VBGR          },

	{ "default",    _SWAP_KIND,      CGUI_INPUT_SWAP_TO_DEFAULT         },
	{ "none",       _SWAP_KIND,      CGUI_INPUT_SWAP_TO_NONE            },
	{ "value",      _SWAP_KIND,      CGUI_INPUT_SWAP_TO_VALUE           },
	{ "accel",      _SWAP_KIND,      CGUI_INPUT_SWAP_TO_ACCELERATOR     },
	{ "cut",        _SWAP_KIND,      CGUI_INPUT_SWAP_TO_CLIPBOARD_CUT   },
	{ "copy",       _SWAP_KIND,      CGUI_INPUT_SWAP_TO_CLIPBOARD_COPY  },
	{ "paste",      _SWAP_KIND,      CGUI_INPUT_SWAP_TO_CLIPBOARD_PASTE },
	{ "cell",       _SWAP_KIND,      CGUI_INPUT_SWAP_TO_ACTION_CELL     },
	{ "focus",      _SWAP_KIND,      CGUI_INPUT_SWAP_TO_ACTION_FOCUS    },
	{ "window",     _SWAP_KIND,      CGUI_INPUT_SWAP_TO_ACTION_WINDOW   },
	{ "misc",       _SWAP_KIND,      CGUI_INPUT_SWAP_TO_ACTION_MISC     },

	{ "select-",    _SWAP_ACTION,    CGUI_INPUT_SWAP_CELL_SELECT_LESS   },
	{ "select+",    _SWAP_ACTION,    CGUI_INPUT_SWAP_CELL_SELECT_MORE   },
	{ "unselect",   _SWAP_ACTION,    CGUI_INPUT_SWAP_CELL_SELECT_NONE   },
	{ "select_all", _SWAP_ACTION,    CGUI_INPUT_SWAP_CELL_SELECT_ALL    },
	{ "redraw",     _SWAP_ACTION,    CGUI_INPUT_SWAP_CELL_SELECT_ALL    },
	{ "trigger1",   _SWAP_ACTION,    CGUI_INPUT_SWAP_CELL_TRIGGER_1     },
	{ "trigger2",   _SWAP_ACTION,    CGUI_INPUT_SWAP_CELL_TRIGGER_2     },
	{ "trigger3",   _SWAP_ACTION,    CGUI_INPUT_SWAP_CELL_TRIGGER_3     },
	{ "trigger4",   _SWAP_ACTION,    CGUI_INPUT_SWAP_CELL_TRIGGER_4     },
	{ "trigger5",   _SWAP_ACTION,    CGUI_INPUT_SWAP_CELL_TRIGGER_5     },

	{ "left",       _SWAP_ACTION,    CGUI_INPUT_SWAP_FOCUS_LEFT         },
	{ "right",      _SWAP_ACTION,    CGUI_INPUT_SWAP_FOCUS_RIGHT        },
	{ "up",         _SWAP_ACTION,    CGUI_INPUT_SWAP_FOCUS_UP           },
	{ "down",       _SWAP_ACTION,    CGUI_INPUT_SWAP_FOCUS_DOWN         },
	{ "leftmost",   _SWAP_ACTION,    CGUI_INPUT_SWAP_FOCUS_LEFTMOST     },
	{ "rightmost",  _SWAP_ACTION,    CGUI_INPUT_SWAP_FOCUS_RIGHTMOST    },
	{ "top",        _SWAP_ACTION,    CGUI_INPUT_SWAP_FOCUS_TOP          },
	{ "bottom",     _SWAP_ACTION,    CGUI_INPUT_SWAP_FOCUS_BOTTOM       },
	{ "next",       _SWAP_ACTION,    CGUI_INPUT_SWAP_FOCUS_NEXT         },
	{ "previous",   _SWAP_ACTION,    CGUI_INPUT_SWAP_FOCUS_PREV         },
	{ "first",      _SWAP_ACTION,    CGUI_INPUT_SWAP_FOCUS_FIRST        },
	{ "last",       _SWAP_ACTION,    CGUI_INPUT_SWAP_FOCUS_LAST         },
	{ "none",       _SWAP_ACTION,    CGUI_INPUT_SWAP_FOCUS_NONE         },

	{ "lock_grid",  _SWAP_ACTION,    CGUI_INPUT_SWAP_WINDOW_LOCK_GRID   },
	{ "lock_focus", _SWAP_ACTION,    CGUI_INPUT_SWAP_WINDOW_LOCK_FOCUS  },
	{ "redraw",     _SWAP_ACTION,    CGUI_INPUT_SWAP_WINDOW_LOCK_FOCUS  },
	
	{ "reconfig",   _SWAP_ACTION,    CGUI_INPUT_SWAP_RECONFIG           },
	{ "exit",       _SWAP_ACTION,    CGUI_INPUT_SWAP_EXIT               },
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const _resource_t _resources[] =
{
	{ "global",   "scale",                       _UDOUBLE,        &_config.scale                            },
	{ "global",   "modkey",                      _MODKEY,         &_config.modkey                           },

	{ "font",     "face",                        _STRING,          _config.font_face                        },
	{ "font",     "size",                        _LENGTH,         &_config.font_size                        },
	{ "font",     "horizontal_spacing",          _LENGTH,         &_config.font_spacing_horizontal          },
	{ "font",     "vertical_spacing",            _LENGTH,         &_config.font_spacing_vertical            },
	{ "font",     "width_override",              _LENGTH,         &_config.font_override_width              },
	{ "font",     "ascent_override",             _LENGTH,         &_config.font_override_ascent             },
	{ "font",     "descent_override",            _LENGTH,         &_config.font_override_descent            },
	{ "font",     "x_offset",                    _POSITION,       &_config.font_offset_x                    },
	{ "font",     "y_offset",                    _POSITION,       &_config.font_offset_y                    },
	{ "font",     "enable_overrides",            _BOOL,           &_config.font_enable_overrides            },
	{ "font",     "enable_hint_metrics",         _BOOL,           &_config.font_enable_hint_metrics         },
	{ "font",     "antialias_mode",              _FONT_ANTIALIAS, &_config.font_antialias                   },
	{ "font",     "subpixel_mode",               _FONT_SUBPIXEL,  &_config.font_subpixel                    },

	{ "grid",     "padding",                     _LENGTH,         &_config.grid_padding                     },
	{ "grid",     "spacing",                     _LENGTH,         &_config.grid_spacing                     },

	{ "window",   "border_thickness",            _LENGTH,         &_config.window_border                    },
	{ "window",   "padding",                     _LENGTH,         &_config.window_padding                   },
	{ "window",   "color_background",            _COLOR,          &_config.window_color_background          },
	{ "window",   "color_background_disabled",   _COLOR,          &_config.window_color_background_disabled },
	{ "window",   "color_background_focused",    _COLOR,          &_config.window_color_background_focused  },
	{ "window",   "color_background_locked",     _COLOR,          &_config.window_color_background_locked   },
	{ "window",   "color_border",                _COLOR,          &_config.window_color_border              },
	{ "window",   "color_border_disabled",       _COLOR,          &_config.window_color_border_disabled     },
	{ "window",   "color_border_focused",        _COLOR,          &_config.window_color_border_focused      },
	{ "window",   "color_border_locked",         _COLOR,          &_config.window_color_border_locked       },
	{ "window",   "enable_disabled_substyle",    _BOOL,           &_config.window_enable_disabled           },
	{ "window",   "enable_focused_substyle",     _BOOL,           &_config.window_enable_focused            },
	{ "window",   "enable_locked_substyle",      _BOOL,           &_config.window_enable_locked             },

	{ "popup",    "border_thickness",            _LENGTH,         &_config.popup_border                     },
	{ "popup",    "padding",                     _LENGTH,         &_config.popup_padding                    },
	{ "popup",    "color_background",            _COLOR,          &_config.popup_color_background           },
	{ "popup",    "color_border",                _COLOR,          &_config.popup_color_border               },
	{ "popup",    "max_width",                   _LENGTH,         &_config.popup_max_width                  },
	{ "popup",    "max_height",                  _LENGTH,         &_config.popup_max_height                 },
	{ "popup",    "width_override",              _LENGTH,         &_config.popup_override_width             },
	{ "popup",    "height_override",             _LENGTH,         &_config.popup_override_height            },
	{ "popup",    "x_position_override",         _POSITION,       &_config.popup_override_x                 },
	{ "popup",    "y_position_override",         _POSITION,       &_config.popup_override_y                 },
	{ "popup",    "enable_position_overrides",   _BOOL,           &_config.popup_enable_override_position   },
	{ "popup",    "enable_width_override",       _BOOL,           &_config.popup_enable_override_width      },
	{ "popup",    "ennable_height_override",     _BOOL,           &_config.popup_enable_override_height     },
	
	{ "behavior", "enable_cell_auto_lock",       _BOOL,           &_config.cell_auto_lock                   },
	{ "behavior", "enable_persistent_pointer",   _BOOL,           &_config.input_persistent_pointer         },
	{ "behavior", "enable_persistent_touch",     _BOOL,           &_config.input_persistent_touch           },
	{ "behavior", "animation_framerate_divider", _ULONG,          &_config.anim_divider                     },

	_KEY(  1) _KEY(  2) _KEY(  3) _KEY(  4) _KEY(  5) _KEY(  6) _KEY(  7) _KEY(  8) _KEY(  9) _KEY( 10)
	_KEY( 11) _KEY( 12) _KEY( 13) _KEY( 14) _KEY( 15) _KEY( 16) _KEY( 17) _KEY( 18) _KEY( 19) _KEY( 20)
	_KEY( 21) _KEY( 22) _KEY( 23) _KEY( 24) _KEY( 25) _KEY( 26) _KEY( 27) _KEY( 28) _KEY( 29) _KEY( 30)
	_KEY( 31) _KEY( 32) _KEY( 33) _KEY( 34) _KEY( 35) _KEY( 36) _KEY( 37) _KEY( 38) _KEY( 39) _KEY( 40)
	_KEY( 41) _KEY( 42) _KEY( 43) _KEY( 44) _KEY( 45) _KEY( 46) _KEY( 47) _KEY( 48) _KEY( 49) _KEY( 50) 
	_KEY( 51) _KEY( 52) _KEY( 53) _KEY( 54) _KEY( 55) _KEY( 56) _KEY( 57) _KEY( 58) _KEY( 59) _KEY( 60)
	_KEY( 61) _KEY( 62) _KEY( 63) _KEY( 64) _KEY( 65) _KEY( 66) _KEY( 67) _KEY( 68) _KEY( 69) _KEY( 70)
	_KEY( 71) _KEY( 72) _KEY( 73) _KEY( 74) _KEY( 75) _KEY( 76) _KEY( 77) _KEY( 78) _KEY( 79) _KEY( 80)
	_KEY( 81) _KEY( 82) _KEY( 83) _KEY( 84) _KEY( 85) _KEY( 86) _KEY( 87) _KEY( 88) _KEY( 89) _KEY( 90)
	_KEY( 91) _KEY( 92) _KEY( 93) _KEY( 94) _KEY( 95) _KEY( 96) _KEY( 97) _KEY( 98) _KEY( 99) _KEY(100)
	_KEY(101) _KEY(102) _KEY(103) _KEY(104) _KEY(105) _KEY(106) _KEY(107) _KEY(108) _KEY(109) _KEY(110)
	_KEY(111) _KEY(112) _KEY(113) _KEY(114) _KEY(115) _KEY(116) _KEY(117) _KEY(118) _KEY(119) _KEY(120)
	_KEY(121) _KEY(122) _KEY(123) _KEY(124) _KEY(125) _KEY(126) _KEY(127)

	_BUTTON( 1) _BUTTON( 2) _BUTTON( 3) _BUTTON( 4) _BUTTON( 5) _BUTTON( 6) _BUTTON( 7) _BUTTON( 8)
	_BUTTON( 9) _BUTTON(10) _BUTTON(11) _BUTTON(12)
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
cgui_config_get_parser(void)
{
	return _parser ? _parser : ccfg_get_placeholder();
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

bool
config_init(void)
{
	cobj_string_t *home;

	/* instantiation */

	_parser = ccfg_create();
	_dict   = cobj_dictionary_create(sizeof(_words) / sizeof(_word_t), 0.6);
	 home   = cobj_string_create();

	/* parser setup */

	cobj_string_set_raw(home, util_env_exists("HOME") ? getenv("HOME") : getpwuid(getuid())->pw_dir);
	cobj_string_append_raw(home, "/.config/cgui.conf");

	ccfg_push_source(_parser, getenv(ENV_CONF_SOURCE));
	ccfg_push_source(_parser, cobj_string_get_chars(home));
	ccfg_push_source(_parser, "/usr/share/cgui/cgui.conf");
	ccfg_push_source(_parser, "/etc/cgui.conf");

	cobj_string_destroy(&home);

	/* dictionary setup */

	for (size_t i = 0; i < sizeof(_words) / sizeof(_word_t); i++)
	{
		cobj_dictionary_write(_dict, _words[i].name, _words[i].type, _words[i].value);
	}
	
	/* end */

	return !ccfg_has_failed(_parser) && !cobj_dictionary_has_failed(_dict);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
config_load(void)
{
	_config      = config_default;
	_config.init = true;

	if (util_env_exists(ENV_NO_PARSING))
	{
		return true;
	}

	ccfg_load(_parser);
	for (size_t i = 0; i < sizeof(_resources) / sizeof(_resource_t); i++)
	{
		_fetch(_resources + i);
	}

	return _fill() && !ccfg_has_failed(_parser);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
config_reset(void)
{
	ccfg_destroy(&_parser);
	cobj_dictionary_destroy(&_dict);

	_config = config_default;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_fetch(const _resource_t *resource)
{
	const char *str;

	ccfg_fetch(_parser, resource->namespace, resource->name);
	if (ccfg_pick_next_value(_parser))
	{
		str = ccfg_get_value(_parser);
	}
	else
	{
		return;
	}

	switch (resource->type)
	{
		case _STRING:
			snprintf((char*)resource->target, CCFG_MAX_WORD_BYTES, "%s", str);
			break;

		case _COLOR:
			*(cobj_color_t*)resource->target = cobj_color_convert_str(str, NULL);
			break;

		case _BOOL:
			*(bool*)resource->target = strtod(str, NULL) != 0.0;
			break;

		case _POSITION:
			*(int*)resource->target = util_str_to_long(str, INT_MIN, INT_MAX);
			break;

		case _LENGTH:
			*(int*)resource->target = util_str_to_long(str, 0, INT_MAX);
			break;

		case _LONG:
			*(long*)resource->target = util_str_to_long(str, LONG_MIN, LONG_MAX);
			break;

		case _ULONG:
			*(unsigned long*)resource->target = util_str_to_long(str, 0, ULONG_MAX);
			break;

		case _DOUBLE:
			*(double*)resource->target = util_str_to_double(str, DBL_MIN, DBL_MAX);
			break;

		case _UDOUBLE:
			*(double*)resource->target = util_str_to_double(str, 0.0, DBL_MAX);
			break;

		case _RATIO:
			*(double*)resource->target = util_str_to_double(str, 0.0, 1.0);
			break;

		case _MODKEY:
		case _FONT_ANTIALIAS:
		case _FONT_SUBPIXEL:
			cobj_dictionary_find(_dict, str, resource->type, (size_t*)resource->target);
			break;

		case _MAP_KEY:
			_swap(str, CGUI_CONFIG_KEYS, (cgui_input_swap_t*)resource->target);
			break;

		case _MAP_BUTTON:
			_swap(str, CGUI_CONFIG_BUTTONS, (cgui_input_swap_t*)resource->target);
			break;

		default:
			return;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_fill(void)
{
	cairo_font_extents_t f_e;
	cairo_text_extents_t t_e;
	cairo_surface_t *c_srf;
	cairo_t *c_ctx;

	bool failed = false;

	/* geometry and font scaling */

	_SCALE(_config.font_size);
	_SCALE(_config.font_spacing_horizontal);
	_SCALE(_config.font_spacing_vertical);
	_SCALE(_config.font_offset_x);
	_SCALE(_config.font_offset_y);
	_SCALE(_config.font_override_ascent);
	_SCALE(_config.font_override_descent);
	_SCALE(_config.font_override_width);
	_SCALE(_config.grid_padding);
	_SCALE(_config.grid_spacing);
	_SCALE(_config.window_border);
	_SCALE(_config.window_padding);
	_SCALE(_config.popup_border);
	_SCALE(_config.popup_padding);

	/* get font geometry with cairo */

	if (_config.font_enable_overrides)
	{
		_config.font_descent = _config.font_override_descent;
		_config.font_ascent  = _config.font_override_ascent;
		_config.font_width   = _config.font_override_width;
		goto skip_auto_font;
	}

	c_srf = cairo_image_surface_create(CAIRO_FORMAT_A1, 0, 0);
	c_ctx = cairo_create(c_srf);
	if (cairo_surface_status(c_srf) != CAIRO_STATUS_SUCCESS || cairo_status(c_ctx) != CAIRO_STATUS_SUCCESS)
	{
		failed = true;
		goto skip_font_setup;
	}
	
	cairo_set_font_size(c_ctx, _config.font_size);
	cairo_select_font_face(
		c_ctx,
		_config.font_face,
		CAIRO_FONT_SLANT_NORMAL,
		CAIRO_FONT_WEIGHT_NORMAL);

	cairo_font_extents(c_ctx, &f_e);
	cairo_text_extents(c_ctx, "A", &t_e);

	_config.font_descent = f_e.descent;
	_config.font_ascent  = f_e.ascent;
	_config.font_width   = t_e.width;

skip_font_setup:

	cairo_destroy(c_ctx);
	cairo_surface_destroy(c_srf);

skip_auto_font:

	_config.font_height = _config.font_ascent + _config.font_descent;

	/* end */

	return !failed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_swap(const char *str, uint8_t limit, cgui_input_swap_t *target)
{
	char *str_copy;
	char *ctx;
	char *l;
	char *r;
	size_t tmp = 0;

	if (!(str_copy = strdup(str)))
	{
		return;
	}

	l = strtok_r(str_copy, ":", &ctx);
	r = strtok_r(NULL,     ":", &ctx);
	if (!l || !r)
	{
		free(str_copy);
		return;
	}

	cobj_dictionary_find(_dict, l, _SWAP_KIND, &tmp);
	target->kind = tmp;

	switch (target->kind)
	{
		case CGUI_INPUT_SWAP_TO_VALUE:
			target->value = util_str_to_long(r, 0, limit);
			break;

		case CGUI_INPUT_SWAP_TO_ACCELERATOR:
			target->value = util_str_to_long(r, 1, CGUI_CONFIG_ACCELS);
			break;

		case CGUI_INPUT_SWAP_TO_CLIPBOARD_CUT:
		case CGUI_INPUT_SWAP_TO_CLIPBOARD_COPY:
		case CGUI_INPUT_SWAP_TO_CLIPBOARD_PASTE:
			target->value = util_str_to_long(r, 1, CGUI_CONFIG_CLIPBOARDS);
			break;

		case CGUI_INPUT_SWAP_TO_ACTION_CELL:
		case CGUI_INPUT_SWAP_TO_ACTION_FOCUS:
		case CGUI_INPUT_SWAP_TO_ACTION_WINDOW:
		case CGUI_INPUT_SWAP_TO_ACTION_MISC:
			tmp = 0;
			cobj_dictionary_find(_dict, r, _SWAP_KIND, &tmp);
			target->value = tmp;
			break;

		case CGUI_INPUT_SWAP_TO_DEFAULT:
		case CGUI_INPUT_SWAP_TO_NONE:
		default:
			break;
	}

	free(str_copy);
}
