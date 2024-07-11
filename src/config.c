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

#include <cairo/cairo.h>
#include <cassette/ccfg.h>
#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <float.h>
#include <limits.h>
#include <pwd.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "config.h"
#include "config-default.h"
#include "env.h"
#include "util.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define STYLE_CELL(NAMESPACE, TARGET) \
	{ NAMESPACE, "border_thickness",  LENGTH, &TARGET.thickness_border  }, \
	{ NAMESPACE, "outline_thickness", LENGTH, &TARGET.thickness_outline }, \
	{ NAMESPACE, "margin",            LENGTH, &TARGET.margin            }, \
	{ NAMESPACE, "color_background",  COLOR,  &TARGET.color_background  }, \
	{ NAMESPACE, "color_border",      COLOR,  &TARGET.color_border      }, \
	{ NAMESPACE, "color_outline",     COLOR,  &TARGET.color_outline     },

#define KEY(VALUE) \
	{ "key",     #VALUE, MAP_KEY, &_config.keys[VALUE][CGUI_CONFIG_SWAP_DIRECT] }, \
	{ "key", "M" #VALUE, MAP_KEY, &_config.keys[VALUE][CGUI_CONFIG_SWAP_MOD]    }, \
	{ "key", "S" #VALUE, MAP_KEY, &_config.keys[VALUE][CGUI_CONFIG_SWAP_SHIFT]  },

#define BUTTON(VALUE) \
	{ "button",     #VALUE, MAP_BUTTON, &_config.buttons[VALUE][CGUI_CONFIG_SWAP_DIRECT] }, \
	{ "button", "M" #VALUE, MAP_BUTTON, &_config.buttons[VALUE][CGUI_CONFIG_SWAP_MOD]    }, \
	{ "button", "S" #VALUE, MAP_BUTTON, &_config.buttons[VALUE][CGUI_CONFIG_SWAP_SHIFT]  },

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum _value
{
	/* primitives */

	STRING,
	COLOR,
	BOOL,
	POSITION,
	LENGTH,
	LONG,
	ULONG,
	DOUBLE,
	UDOUBLE,
	RATIO,

	/* dict based */

	MOD_KEY,
	ANTIALIAS,
	SUBPIXEL,
	SWAP_KIND,
	SWAP_ACTION,

	/* composite */

	MAP_KEY,
	MAP_BUTTON,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct _word
{
	char *name;
	enum _value type;
	size_t value;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct _resource
{
	char *namespace;
	char *name;
	enum _value type;
	void *target;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct _callback
{
	void (*fn)(ccfg *cfg);
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _fetch     (const struct _resource *resource)                         CGUI_NONNULL(1);
static bool _fill      (void);
static void _scale_len (uint16_t *val)                                            CGUI_NONNULL(1);
static void _scale_pos (int16_t  *val)                                            CGUI_NONNULL(1);
static void _swap      (const char *str, uint8_t limit, struct cgui_swap *target) CGUI_NONNULL(1, 3);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static struct cgui_config _config = config_default;
static cref  *_callbacks = CREF_PLACEHOLDER;
static ccfg  *_parser    = CCFG_PLACEHOLDER;
static cdict *_dict      = CDICT_PLACEHOLDER;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct _word _words[] =
{
	{ "mod1",       MOD_KEY,     CGUI_CONFIG_MOD_1                  },

	{ "mod4",       MOD_KEY,     CGUI_CONFIG_MOD_4                  },
	{ "ctrl",       MOD_KEY,     CGUI_CONFIG_MOD_CTRL               },

	{ "none",       ANTIALIAS,   CGUI_CONFIG_ANTIALIAS_NONE         },
	{ "gray",       ANTIALIAS,   CGUI_CONFIG_ANTIALIAS_GRAY         },
	{ "subpixel",   ANTIALIAS,   CGUI_CONFIG_ANTIALIAS_SUBPIXEL     },

	{ "rgb",        SUBPIXEL,    CGUI_CONFIG_SUBPIXEL_RGB           },
	{ "bgr",        SUBPIXEL,    CGUI_CONFIG_SUBPIXEL_BGR           },
	{ "vrgb",       SUBPIXEL,    CGUI_CONFIG_SUBPIXEL_VRGB          },
	{ "vbgr",       SUBPIXEL,    CGUI_CONFIG_SUBPIXEL_VBGR          },

	{ "default",    SWAP_KIND,   CGUI_INPUT_SWAP_TO_DEFAULT         },
	{ "none",       SWAP_KIND,   CGUI_INPUT_SWAP_TO_NONE            },
	{ "value",      SWAP_KIND,   CGUI_INPUT_SWAP_TO_VALUE           },
	{ "accel",      SWAP_KIND,   CGUI_INPUT_SWAP_TO_ACCELERATOR     },
	{ "cut",        SWAP_KIND,   CGUI_INPUT_SWAP_TO_CLIPBOARD_CUT   },
	{ "copy",       SWAP_KIND,   CGUI_INPUT_SWAP_TO_CLIPBOARD_COPY  },
	{ "paste",      SWAP_KIND,   CGUI_INPUT_SWAP_TO_CLIPBOARD_PASTE },
	{ "cell",       SWAP_KIND,   CGUI_INPUT_SWAP_TO_ACTION_CELL     },
	{ "focus",      SWAP_KIND,   CGUI_INPUT_SWAP_TO_ACTION_FOCUS    },
	{ "window",     SWAP_KIND,   CGUI_INPUT_SWAP_TO_ACTION_WINDOW   },
	{ "misc",       SWAP_KIND,   CGUI_INPUT_SWAP_TO_ACTION_MISC     },

	{ "select-",    SWAP_ACTION, CGUI_INPUT_SWAP_CELL_SELECT_LESS   },
	{ "select+",    SWAP_ACTION, CGUI_INPUT_SWAP_CELL_SELECT_MORE   },
	{ "unselect",   SWAP_ACTION, CGUI_INPUT_SWAP_CELL_SELECT_NONE   },
	{ "select_all", SWAP_ACTION, CGUI_INPUT_SWAP_CELL_SELECT_ALL    },
	{ "redraw",     SWAP_ACTION, CGUI_INPUT_SWAP_CELL_SELECT_ALL    },
	{ "trigger1",   SWAP_ACTION, CGUI_INPUT_SWAP_CELL_TRIGGER_1     },
	{ "trigger2",   SWAP_ACTION, CGUI_INPUT_SWAP_CELL_TRIGGER_2     },
	{ "trigger3",   SWAP_ACTION, CGUI_INPUT_SWAP_CELL_TRIGGER_3     },
	{ "trigger4",   SWAP_ACTION, CGUI_INPUT_SWAP_CELL_TRIGGER_4     },
	{ "trigger5",   SWAP_ACTION, CGUI_INPUT_SWAP_CELL_TRIGGER_5     },

	{ "left",       SWAP_ACTION, CGUI_INPUT_SWAP_FOCUS_LEFT         },
	{ "right",      SWAP_ACTION, CGUI_INPUT_SWAP_FOCUS_RIGHT        },
	{ "up",         SWAP_ACTION, CGUI_INPUT_SWAP_FOCUS_UP           },
	{ "down",       SWAP_ACTION, CGUI_INPUT_SWAP_FOCUS_DOWN         },
	{ "leftmost",   SWAP_ACTION, CGUI_INPUT_SWAP_FOCUS_LEFTMOST     },
	{ "rightmost",  SWAP_ACTION, CGUI_INPUT_SWAP_FOCUS_RIGHTMOST    },
	{ "top",        SWAP_ACTION, CGUI_INPUT_SWAP_FOCUS_TOP          },
	{ "bottom",     SWAP_ACTION, CGUI_INPUT_SWAP_FOCUS_BOTTOM       },
	{ "next",       SWAP_ACTION, CGUI_INPUT_SWAP_FOCUS_NEXT         },
	{ "previous",   SWAP_ACTION, CGUI_INPUT_SWAP_FOCUS_PREV         },
	{ "first",      SWAP_ACTION, CGUI_INPUT_SWAP_FOCUS_FIRST        },
	{ "last",       SWAP_ACTION, CGUI_INPUT_SWAP_FOCUS_LAST         },
	{ "none",       SWAP_ACTION, CGUI_INPUT_SWAP_FOCUS_NONE         },

	{ "lock_grid",  SWAP_ACTION, CGUI_INPUT_SWAP_WINDOW_LOCK_GRID   },
	{ "lock_focus", SWAP_ACTION, CGUI_INPUT_SWAP_WINDOW_LOCK_FOCUS  },
	{ "redraw",     SWAP_ACTION, CGUI_INPUT_SWAP_WINDOW_LOCK_FOCUS  },
	
	{ "reconfig",   SWAP_ACTION, CGUI_INPUT_SWAP_RECONFIG           },
	{ "exit",       SWAP_ACTION, CGUI_INPUT_SWAP_EXIT               },
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct _resource _resources[] =
{
	{ "global",   "scale",                       UDOUBLE,   &_config.scale                            },

	{ "global",   "modkey",                      MOD_KEY,   &_config.modkey                           },

	{ "font",     "face",                        STRING,     _config.font_face                        },
	{ "font",     "size",                        LENGTH,    &_config.font_size                        },
	{ "font",     "horizontal_spacing",          LENGTH,    &_config.font_spacing_horizontal          },
	{ "font",     "vertical_spacing",            LENGTH,    &_config.font_spacing_vertical            },
	{ "font",     "width_override",              LENGTH,    &_config.font_override_width              },
	{ "font",     "ascent_override",             LENGTH,    &_config.font_override_ascent             },
	{ "font",     "descent_override",            LENGTH,    &_config.font_override_descent            },
	{ "font",     "x_offset",                    POSITION,  &_config.font_offset_x                    },
	{ "font",     "y_offset",                    POSITION,  &_config.font_offset_y                    },
	{ "font",     "enable_overrides",            BOOL,      &_config.font_enable_overrides            },
	{ "font",     "enable_hint_metrics",         BOOL,      &_config.font_enable_hint_metrics         },
	{ "font",     "antialias_mode",              ANTIALIAS, &_config.font_antialias                   },
	{ "font",     "subpixel_mode",               SUBPIXEL,  &_config.font_subpixel                    },

	{ "grid",     "padding",                     LENGTH,    &_config.grid_padding                     },
	{ "grid",     "spacing",                     LENGTH,    &_config.grid_spacing                     },

	{ "window",   "border_thickness",            LENGTH,    &_config.window_border                    },
	{ "window",   "padding",                     LENGTH,    &_config.window_padding                   },
	{ "window",   "color_background",            COLOR,     &_config.window_color_background          },
	{ "window",   "color_background_disabled",   COLOR,     &_config.window_color_background_disabled },
	{ "window",   "color_background_focused",    COLOR,     &_config.window_color_background_focused  },
	{ "window",   "color_background_locked",     COLOR,     &_config.window_color_background_locked   },
	{ "window",   "color_border",                COLOR,     &_config.window_color_border              },
	{ "window",   "color_border_disabled",       COLOR,     &_config.window_color_border_disabled     },
	{ "window",   "color_border_focused",        COLOR,     &_config.window_color_border_focused      },
	{ "window",   "color_border_locked",         COLOR,     &_config.window_color_border_locked       },
	{ "window",   "enable_disabled_substyle",    BOOL,      &_config.window_enable_disabled           },
	{ "window",   "enable_focused_substyle",     BOOL,      &_config.window_enable_focused            },
	{ "window",   "enable_locked_substyle",      BOOL,      &_config.window_enable_locked             },

	{ "popup",    "border_thickness",            LENGTH,    &_config.popup_border                     },
	{ "popup",    "padding",                     LENGTH,    &_config.popup_padding                    },
	{ "popup",    "color_background",            COLOR,     &_config.popup_color_background           },
	{ "popup",    "color_border",                COLOR,     &_config.popup_color_border               },
	{ "popup",    "max_width",                   LENGTH,    &_config.popup_max_width                  },
	{ "popup",    "max_height",                  LENGTH,    &_config.popup_max_height                 },
	{ "popup",    "width_override",              LENGTH,    &_config.popup_override_width             },
	{ "popup",    "height_override",             LENGTH,    &_config.popup_override_height            },
	{ "popup",    "x_position_override",         POSITION,  &_config.popup_override_x                 },
	{ "popup",    "y_position_override",         POSITION,  &_config.popup_override_y                 },
	{ "popup",    "enable_position_overrides",   BOOL,      &_config.popup_enable_override_position   },
	{ "popup",    "enable_width_override",       BOOL,      &_config.popup_enable_override_width      },
	{ "popup",    "ennable_height_override",     BOOL,      &_config.popup_enable_override_height     },
	
	{ "behavior", "enable_cell_auto_lock",       BOOL,      &_config.cell_auto_lock                   },
	{ "behavior", "enable_persistent_pointer",   BOOL,      &_config.input_persistent_pointer         },
	{ "behavior", "enable_persistent_touch",     BOOL,      &_config.input_persistent_touch           },
	{ "behavior", "animation_framerate_divider", ULONG,     &_config.anim_divider                     },

	KEY(  1) KEY(  2) KEY(  3) KEY(  4) KEY(  5) KEY(  6) KEY(  7) KEY(  8) KEY(  9) KEY( 10)
	KEY( 11) KEY( 12) KEY( 13) KEY( 14) KEY( 15) KEY( 16) KEY( 17) KEY( 18) KEY( 19) KEY( 20)
	KEY( 21) KEY( 22) KEY( 23) KEY( 24) KEY( 25) KEY( 26) KEY( 27) KEY( 28) KEY( 29) KEY( 30)
	KEY( 31) KEY( 32) KEY( 33) KEY( 34) KEY( 35) KEY( 36) KEY( 37) KEY( 38) KEY( 39) KEY( 40)
	KEY( 41) KEY( 42) KEY( 43) KEY( 44) KEY( 45) KEY( 46) KEY( 47) KEY( 48) KEY( 49) KEY( 50) 
	KEY( 51) KEY( 52) KEY( 53) KEY( 54) KEY( 55) KEY( 56) KEY( 57) KEY( 58) KEY( 59) KEY( 60)
	KEY( 61) KEY( 62) KEY( 63) KEY( 64) KEY( 65) KEY( 66) KEY( 67) KEY( 68) KEY( 69) KEY( 70)
	KEY( 71) KEY( 72) KEY( 73) KEY( 74) KEY( 75) KEY( 76) KEY( 77) KEY( 78) KEY( 79) KEY( 80)
	KEY( 81) KEY( 82) KEY( 83) KEY( 84) KEY( 85) KEY( 86) KEY( 87) KEY( 88) KEY( 89) KEY( 90)
	KEY( 91) KEY( 92) KEY( 93) KEY( 94) KEY( 95) KEY( 96) KEY( 97) KEY( 98) KEY( 99) KEY(100)
	KEY(101) KEY(102) KEY(103) KEY(104) KEY(105) KEY(106) KEY(107) KEY(108) KEY(109) KEY(110)
	KEY(111) KEY(112) KEY(113) KEY(114) KEY(115) KEY(116) KEY(117) KEY(118) KEY(119) KEY(120)
	KEY(121) KEY(122) KEY(123) KEY(124) KEY(125) KEY(126) KEY(127)

	BUTTON( 1) BUTTON( 2) BUTTON( 3) BUTTON( 4) BUTTON( 5) BUTTON( 6) BUTTON( 7) BUTTON( 8)
	BUTTON( 9) BUTTON(10) BUTTON(11) BUTTON(12)
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

const struct cgui_config *
cgui_config_get(void)
{
	return &_config;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_config_pull_on_load(void (*fn)(ccfg *cfg))
{
	CREF_FOR_EACH(_callbacks, i)
	{
		if (((struct _callback*)cref_ptr(_callbacks, i))->fn == fn)
		{
			if (cref_count(_callbacks, i) == 1)
			{
				free((void*)cref_ptr(_callbacks, i));
			}
			cref_pull(_callbacks, i);
			return;
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cgui_config_push_on_load(void (*fn)(ccfg *cfg))
{
	struct _callback *tmp;

	if (!(tmp = malloc(sizeof(struct _callback))))
	{
		return false;
	}

	tmp->fn = fn;

	cref_push(_callbacks, tmp);

	return !cref_error(_callbacks);
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

enum cerr
config_init(const char *app_name, const char *app_class)
{
	cstr *home;

	/* instantiation */

	 home      = cstr_create();
	_callbacks = cref_create();
	_parser    = ccfg_create();
	_dict      = cdict_create();

	/* parser setup */

	cstr_append(home, util_env_exists("HOME") ? getenv("HOME") : getpwuid(getuid())->pw_dir);
	cstr_append(home, "/.config/cgui.conf");

	ccfg_push_source(_parser, util_env_exists(ENV_CONF_SOURCE) ? getenv(ENV_CONF_SOURCE) : "");
	ccfg_push_source(_parser, cstr_chars(home));
	ccfg_push_source(_parser, "/usr/share/cgui/cgui.conf");
	ccfg_push_source(_parser, "/etc/cgui.conf");

	ccfg_push_param(_parser, "app_name",  app_name);
	ccfg_push_param(_parser, "app_class", app_class);

	cstr_destroy(home);

	/* dict setup */

	cdict_prealloc(_dict,  sizeof(_words) / sizeof(struct _word));
	for (size_t i = 0; i < sizeof(_words) / sizeof(struct _word); i++)
	{
		cdict_write(_dict, _words[i].name, _words[i].type, _words[i].value);
	}
	
	/* end */

	if (cref_error(_callbacks)
	 || ccfg_error(_parser)
	 || cdict_error(_dict))
	{
		return CERR_CONFIG;
	}
	
	return CERR_NONE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cerr
config_load(void)
{
	_config      = config_default;
	_config.init = true;

	if (util_env_exists(ENV_NO_PARSING))
	{
		return CERR_NONE;
	}

	ccfg_load(_parser);
	for (size_t i = 0; i < sizeof(_resources) / sizeof(struct _resource); i++)
	{
		_fetch(_resources + i);
	}

	CREF_FOR_EACH(_callbacks, i)
	{
		if (cref_count(_callbacks, i) > 0)
		{
			((struct _callback*)cref_ptr(_callbacks, i))->fn(_parser);
		}
	}

	_scale_len(&_config.font_size);
	_scale_len(&_config.font_spacing_horizontal);
	_scale_len(&_config.font_spacing_vertical);
	_scale_len(&_config.font_override_ascent);
	_scale_len(&_config.font_override_descent);
	_scale_len(&_config.font_override_width);
	_scale_len(&_config.grid_padding);
	_scale_len(&_config.grid_spacing);
	_scale_len(&_config.window_border);
	_scale_len(&_config.window_padding);
	_scale_len(&_config.popup_border);
	_scale_len(&_config.popup_padding);

	_scale_pos(&_config.font_offset_x);
	_scale_pos(&_config.font_offset_y);

	if (!_fill() || ccfg_error(_parser))
	{
		return CERR_CONFIG;
	}

	return CERR_NONE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
config_repair(void)
{
	cref_repair(_callbacks);
	ccfg_repair(_parser);
	cdict_repair(_dict);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
config_reset(void)
{
	cref_destroy(_callbacks);
	ccfg_destroy(_parser);
	cdict_destroy(_dict);

	_config    = config_default;
	_callbacks = CREF_PLACEHOLDER;
	_parser    = CCFG_PLACEHOLDER;
	_dict      = CDICT_PLACEHOLDER;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_fetch(const struct _resource *resource)
{
	const char *str;

	ccfg_fetch(_parser, resource->namespace, resource->name);
	if (ccfg_iterate(_parser))
	{
		str = ccfg_resource(_parser);
	}
	else
	{
		return;
	}

	switch (resource->type)
	{
		case STRING:
			snprintf((char*)resource->target, CGUI_CONFIG_STR_LEN, "%s", str);
			break;

		case COLOR:
			*(struct ccolor*)resource->target = ccolor_from_str(str, NULL);
			break;

		case BOOL:
			*(bool*)resource->target = strtod(str, NULL) != 0.0;
			break;

		case POSITION:
			*(int*)resource->target = util_str_to_long(str, INT16_MIN, INT16_MAX);
			break;

		case LENGTH:
			*(int*)resource->target = util_str_to_long(str, 0, INT16_MAX);
			break;

		case LONG:
			*(long*)resource->target = util_str_to_long(str, LONG_MIN, LONG_MAX);
			break;

		case ULONG:
			*(unsigned long*)resource->target = util_str_to_long(str, 0, ULONG_MAX);
			break;

		case DOUBLE:
			*(double*)resource->target = util_str_to_double(str, DBL_MIN, DBL_MAX);
			break;

		case UDOUBLE:
			*(double*)resource->target = util_str_to_double(str, 0.0, DBL_MAX);
			break;

		case RATIO:
			*(double*)resource->target = util_str_to_double(str, 0.0, 1.0);
			break;

		case MOD_KEY:
		case ANTIALIAS:
		case SUBPIXEL:
			cdict_find(_dict, str, resource->type, (size_t*)resource->target);
			break;

		case MAP_KEY:
			_swap(str, CGUI_CONFIG_KEYS, (struct cgui_swap*)resource->target);
			break;

		case MAP_BUTTON:
			_swap(str, CGUI_CONFIG_BUTTONS, (struct cgui_swap*)resource->target);
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

	struct cline line = { .origin = 0, .length = 0, .min = 0, .max = INT16_MAX};
	bool failed = false;

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
	if (cairo_surface_status(c_srf) != CAIRO_STATUS_SUCCESS
	 || cairo_status(c_ctx)         != CAIRO_STATUS_SUCCESS)
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

	cline_grow(&line, _config.font_ascent);
	cline_grow(&line, _config.font_descent);

	_config.font_height = line.length;

	/* end */

	return !failed;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_scale_len(uint16_t *val)
{
	struct cline line =
	{
		.origin = 0,
		.length = *val,
		.min    = 0,
		.max    = INT16_MAX
	};

	cline_scale(&line, _config.scale);

	*val = line.length;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_scale_pos(int16_t *val)
{
	struct cline line =
	{
		.origin = 0,
		.length = *val,
		.min    = INT16_MIN,
		.max    = INT16_MAX
	};

	cline_scale(&line, _config.scale);

	*val = line.length;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_swap(const char *str, uint8_t limit, struct cgui_swap *target)
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

	cdict_find(_dict, l, SWAP_KIND, &tmp);
	target->type = tmp;

	switch (target->type)
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
			cdict_find(_dict, r, SWAP_KIND, &tmp);
			target->value = tmp;
			break;

		case CGUI_INPUT_SWAP_TO_DEFAULT:
		case CGUI_INPUT_SWAP_TO_NONE:
		default:
			break;
	}

	free(str_copy);
}
