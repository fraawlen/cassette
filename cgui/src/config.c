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

#include <cairo/cairo.h>
#include <cassette/ccfg.h>
#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include <pwd.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "config.h"
#include "config-default.h"
#include "env.h"
#include "main.h"
#include "util.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define LOAD(TARGET) \
	for (size_t i = 0; i < sizeof(TARGET) / sizeof(struct resource); i++) \
	{ \
		fetch(TARGET[i]); \
		scale(TARGET[i]); \
	} \

#define WINDOW(NAMESPACE, TARGET) \
	{ NAMESPACE, "corner_type",      CORNER_TYPE,  TARGET.corner           }, \
	{ NAMESPACE, "corner_size",      CORNER_SIZE,  TARGET.size_corner      }, \
	{ NAMESPACE, "border_thickness", LENGTH,      &TARGET.size_border      }, \
	{ NAMESPACE, "color_border",     COLOR,       &TARGET.color_border     }, \
	{ NAMESPACE, "color_background", COLOR,       &TARGET.color_background },

#define BOX(NAMESPACE, TARGET) \
	{ NAMESPACE, "corner_type",       CORNER_TYPE,  TARGET.corner           }, \
	{ NAMESPACE, "corner_size",       CORNER_SIZE,  TARGET.size_corner      }, \
	{ NAMESPACE, "outline_thickness", LENGTH,      &TARGET.size_outline     }, \
	{ NAMESPACE, "border_thickness",  LENGTH,      &TARGET.size_border      }, \
	{ NAMESPACE, "padding",           POSITION,    &TARGET.padding          }, \
	{ NAMESPACE, "margin",            POSITION,    &TARGET.margin           }, \
	{ NAMESPACE, "shadow_x_offset",   POSITION,    &TARGET.shadow_x_offset  }, \
	{ NAMESPACE, "shadow_y_offset",   POSITION,    &TARGET.shadow_y_offset  }, \
	{ NAMESPACE, "color_outline",     COLOR,       &TARGET.color_outline    }, \
	{ NAMESPACE, "color_border",      COLOR,       &TARGET.color_border     }, \
	{ NAMESPACE, "color_background",  COLOR,       &TARGET.color_background }, \
	{ NAMESPACE, "color_shadow",      COLOR,       &TARGET.color_shadow     }, \
	{ NAMESPACE, "shape_outline",     BOOL,        &TARGET.shape_outline    }, \
	{ NAMESPACE, "shape_border",      BOOL,        &TARGET.shape_border     }, \
	{ NAMESPACE, "draw",              BOOL,        &TARGET.draw             }, \
	{ NAMESPACE, "draw_shadow",       BOOL,        &TARGET.draw_shadow      }, \
	{ NAMESPACE, "smart_corners",     BOOL,        &TARGET.smart_corners    }, \
	{ NAMESPACE, "hit_outline",       BOOL,        &TARGET.draw_shadow      },

#define TEXT(NAMESPACE, TARGET) \
	{ NAMESPACE, "text_color",            COLOR, &TARGET.color            }, \
	{ NAMESPACE, "text_color_background", COLOR, &TARGET.color_background }, \
	{ NAMESPACE, "text_draw_background",  BOOL,  &TARGET.draw_background  }, \
	{ NAMESPACE, "text_bold",             BOOL,  &TARGET.bold             },

#define KEY(VALUE) \
	{ "key",     #VALUE, MAP_KEY, &config.keys[VALUE][CGUI_CONFIG_SWAP_DIRECT] }, \
	{ "key", "M" #VALUE, MAP_KEY, &config.keys[VALUE][CGUI_CONFIG_SWAP_MOD]    }, \
	{ "key", "S" #VALUE, MAP_KEY, &config.keys[VALUE][CGUI_CONFIG_SWAP_SHIFT]  },

#define BUTTON(VALUE) \
	{ "button",     #VALUE, MAP_BUTTON, &config.buttons[VALUE][CGUI_CONFIG_SWAP_DIRECT] }, \
	{ "button", "M" #VALUE, MAP_BUTTON, &config.buttons[VALUE][CGUI_CONFIG_SWAP_MOD]    }, \
	{ "button", "S" #VALUE, MAP_BUTTON, &config.buttons[VALUE][CGUI_CONFIG_SWAP_SHIFT]  },

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum value
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
	SCALE,
	BUTTON_ID,
	KEY_ID,

	/* dict based */

	MOD_KEY,
	ANTIALIAS,
	SUBPIXEL,
	FOCUS,
	SWAP_KIND,
	SWAP_ACTION,

	/* composite */

	CORNER_TYPE,
	CORNER_SIZE,
	MAP_KEY,
	MAP_BUTTON,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct word
{
	const char *name;
	enum value type;
	size_t value;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct resource
{
	const char *namespace;
	const char *name;
	enum value type;
	void *target;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void dummy_fn_load (ccfg *)                                    CGUI_NONNULL(1);
static void fetch         (const struct resource);
static void font_setup    (void);
static void scale         (const struct resource);
static void set_corners   (enum value, void *)                        CGUI_NONNULL(2);
static void swap          (const char *, uint8_t, struct cgui_swap *) CGUI_NONNULL(1, 3);
static void update_err    (void);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static struct cgui_config config       = config_default;
static void (*fn_load)(ccfg *cfg)      = dummy_fn_load;
static ccfg  *parser                   = CCFG_PLACEHOLDER;
static cdict *dict                     = CDICT_PLACEHOLDER;
static bool first_load                 = true;
static cairo_font_options_t *font_opts = NULL;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct word words[] =
{
	{ "mod1",       MOD_KEY,     CGUI_CONFIG_MOD_1              },
	{ "mod4",       MOD_KEY,     CGUI_CONFIG_MOD_4              },
	{ "ctrl",       MOD_KEY,     CGUI_CONFIG_MOD_CTRL           },

	{ "none",       ANTIALIAS,   CGUI_CONFIG_ANTIALIAS_NONE     },
	{ "gray",       ANTIALIAS,   CGUI_CONFIG_ANTIALIAS_GRAY     },
	{ "subpixel",   ANTIALIAS,   CGUI_CONFIG_ANTIALIAS_SUBPIXEL },

	{ "rgb",        SUBPIXEL,    CGUI_CONFIG_SUBPIXEL_RGB       },
	{ "bgr",        SUBPIXEL,    CGUI_CONFIG_SUBPIXEL_BGR       },
	{ "vrgb",       SUBPIXEL,    CGUI_CONFIG_SUBPIXEL_VRGB      },
	{ "vbgr",       SUBPIXEL,    CGUI_CONFIG_SUBPIXEL_VBGR      },

	{ "default",    SWAP_KIND,   CGUI_SWAP_TO_DEFAULT           },
	{ "none",       SWAP_KIND,   CGUI_SWAP_TO_NONE              },
	{ "value",      SWAP_KIND,   CGUI_SWAP_TO_VALUE             },
	{ "focus",      SWAP_KIND,   CGUI_SWAP_TO_FOCUS             },
	{ "accel",      SWAP_KIND,   CGUI_SWAP_TO_ACCELERATOR       },
	{ "cut",        SWAP_KIND,   CGUI_SWAP_TO_CLIPBOARD_CUT     },
	{ "copy",       SWAP_KIND,   CGUI_SWAP_TO_CLIPBOARD_COPY    },
	{ "paste",      SWAP_KIND,   CGUI_SWAP_TO_CLIPBOARD_PASTE   },
	{ "cell",       SWAP_KIND,   CGUI_SWAP_TO_ACTION_CELL       },
	{ "window",     SWAP_KIND,   CGUI_SWAP_TO_ACTION_WINDOW     },
	{ "misc",       SWAP_KIND,   CGUI_SWAP_TO_ACTION_MISC       },

	{ "select-",    SWAP_ACTION, CGUI_SWAP_CELL_SELECT_LESS     },
	{ "select+",    SWAP_ACTION, CGUI_SWAP_CELL_SELECT_MORE     },
	{ "unselect",   SWAP_ACTION, CGUI_SWAP_CELL_SELECT_NONE     },
	{ "select_all", SWAP_ACTION, CGUI_SWAP_CELL_SELECT_ALL      },
	{ "redraw",     SWAP_ACTION, CGUI_SWAP_CELL_SELECT_ALL      },

	{ "lock_grid",  SWAP_ACTION, CGUI_SWAP_WINDOW_LOCK_GRID     },
	{ "lock_focus", SWAP_ACTION, CGUI_SWAP_WINDOW_LOCK_FOCUS    },
	{ "redraw",     SWAP_ACTION, CGUI_SWAP_WINDOW_LOCK_FOCUS    },
	
	{ "reconfig",   SWAP_ACTION, CGUI_SWAP_RECONFIG             },
	{ "exit",       SWAP_ACTION, CGUI_SWAP_EXIT                 },

	{ "straight",   CORNER_TYPE, CGUI_CORNER_STRAIGHT           },
	{ "chamfer",    CORNER_TYPE, CGUI_CORNER_CHAMFER            },
	{ "radii",      CORNER_TYPE, CGUI_CORNER_RADII              },

	{ "next",       FOCUS,       CGUI_FOCUS_NEXT                },
	{ "previous",   FOCUS,       CGUI_FOCUS_PREV                },
	{ "first",      FOCUS,       CGUI_FOCUS_FIRST               },
	{ "last",       FOCUS,       CGUI_FOCUS_LAST                },
	{ "none",       FOCUS,       CGUI_FOCUS_NONE                },
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const struct resource resources_once[] =
{
	{"global",        "alternative_present_mode",    BOOL,          &config.alt_present                    },
};

static const struct resource resources[] =
{
	{ "global",       "scale",                       SCALE,         &config.scale                          },
	{ "global",       "modkey",                      MOD_KEY,       &config.modkey                         },

	{ "font",         "number_padding_pattern",      STRING,         config.font_padding_pattern           },
	{ "font",         "face",                        STRING,         config.font_face                      },
	{ "font",         "size",                        LENGTH,        &config.font_size                      },
	{ "font",         "horizontal_spacing",          LENGTH,        &config.font_spacing_horizontal        },
	{ "font",         "vertical_spacing",            LENGTH,        &config.font_spacing_vertical          },
	{ "font",         "width_override",              LENGTH,        &config.font_override_width            },
	{ "font",         "ascent_override",             LENGTH,        &config.font_override_ascent           },
	{ "font",         "descent_override",            LENGTH,        &config.font_override_descent          },
	{ "font",         "x_offset",                    POSITION,      &config.font_offset_x                  },
	{ "font",         "y_offset",                    POSITION,      &config.font_offset_y                  },
	{ "font",         "enable_overrides",            BOOL,          &config.font_enable_overrides          },
	{ "font",         "enable_hint_metrics",         BOOL,          &config.font_enable_hint_metrics       },
	{ "font",         "antialias_mode",              ANTIALIAS,     &config.font_antialias                 },
	{ "font",         "subpixel_mode",               SUBPIXEL,      &config.font_subpixel                  },
	{ "font",         "background_vertical_pad",     LENGTH,        &config.font_background_vpad           },
	{ "font",         "background_horizontal_pad",   LENGTH,        &config.font_background_hpad           },

	{ "grid",         "padding",                     LENGTH,        &config.grid_padding                   },
	{ "grid",         "spacing",                     LENGTH,        &config.grid_spacing                   },

	{ "window",       "padding",                     LENGTH,        &config.window_padding                 },
	{ "window",       "enable_disabled_substyle",    BOOL,          &config.window_enable_disabled         },
	{ "window",       "enable_focused_substyle",     BOOL,          &config.window_enable_focused          },
	{ "window",       "enable_locked_substyle",      BOOL,          &config.window_enable_locked           },
	{ "window",       "allow_partial_redraws",       BOOL,          &config.window_enable_partial_redraws  },
	{ "window",       "focus_on_activation",         BOOL,          &config.window_focus_on_activation     },

	{ "popup",        "padding",                     LENGTH,        &config.popup_padding                  },

	{ "behavior",     "async_present",               BOOL,          &config.async_present                  },
	{ "behavior",     "enable_cell_auto_lock",       BOOL,          &config.cell_auto_lock                 },
	{ "behavior",     "enable_persistent_pointer",   BOOL,          &config.persistent_pointer             },
	{ "behavior",     "enable_persistent_touch",     BOOL,          &config.persistent_touch               },
	{ "behavior",     "enable_reactive_shadows",     BOOL,          &config.shadows_follow_pointer         },
	{ "behavior",     "shadows_max_light_distance",  LENGTH,        &config.shadows_max_light_distance     },
	{ "behavior",     "shadows_max_offset",          LENGTH,        &config.shadows_max_offset             },
	{ "behavior",     "animation_framerate_divider", ULONG,         &config.anim_divider                   },
	{ "behavior",     "window_button_move",          BUTTON_ID,     &config.wm_button_move                 },
	{ "behavior",     "window_button_resize",        BUTTON_ID,     &config.wm_button_resize               },
	{ "behavior",     "window_button_fullscreen",    BUTTON_ID,     &config.wm_button_fullscreen           },

	{ "stripes",      "line_color",                  COLOR,         &config.stripes_line_color             },
	{ "stripes",      "line_width",                  LENGTH,        &config.stripes_line_width             },
	{ "stripes",      "line_spacing",                LENGTH,        &config.stripes_line_spacing           },

	{ "placeholder",  "line_color",                  COLOR,         &config.placeholder_line_color         },
	{ "placeholder",  "line_width",                  LENGTH,        &config.placeholder_line_width         },

	{ "beacon",       "blink_animation_speed_on",    ULONG,         &config.beacon_blink_speed_on          },
	{ "beacon",       "blink_animation_speed_off",   ULONG,         &config.beacon_blink_speed_off         },

	{ "gauge_cursor", "min_length",                  UDOUBLE,       &config.gauge_min_length               },
	{ "gauge_bar",    "max_thickness",               UDOUBLE,       &config.gauge_max_thickness            },

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

	WINDOW( "window",          config.window          )
	WINDOW( "window_focused",  config.window_focused  )
	WINDOW( "window_disabled", config.window_disabled )
	WINDOW( "window_locked",   config.window_locked   )
	WINDOW( "popup",           config.popup           )

	BOX( "filler",              config.filler_frame          )
	BOX( "stripes",             config.stripes_frame         )
	BOX( "placeholder",         config.placeholder_frame     )
	BOX( "button_idle",         config.button_frame_idle     )
	BOX( "button_focused",      config.button_frame_focused  )
	BOX( "button_pressed",      config.button_frame_pressed  )
	BOX( "button_disabled",     config.button_frame_disabled )
	BOX( "beacon_off",          config.beacon_frame_off      )
	BOX( "beacon_on",           config.beacon_frame_on       )
	BOX( "beacon_critical_off", config.beacon_frame_crit_off )
	BOX( "beacon_critical_on",  config.beacon_frame_crit_on  )
	BOX( "label",               config.label_frame           )
	BOX( "gauge",               config.gauge_frame           )
	BOX( "gauge_bar",           config.gauge_bar             )
	BOX( "gauge_cursor",        config.gauge_cursor          )

	TEXT( "button_idle",         config.button_text_idle     )
	TEXT( "button_focused",      config.button_text_focused  )
	TEXT( "button_pressed",      config.button_text_pressed  )
	TEXT( "button_disabled",     config.button_text_disabled )
	TEXT( "beacon_off",          config.beacon_text_off      )
	TEXT( "beacon_on",           config.beacon_text_on       )
	TEXT( "beacon_critical_off", config.beacon_text_crit_off )
	TEXT( "beacon_critical_on",  config.beacon_text_crit_on  )
	TEXT( "label",               config.label_text           )
	TEXT( "gauge",               config.gauge_text           )
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

size_t
cgui_config_fit_cols(double width)
{
	if (cgui_error() || width <= 0.0)
	{
		return 0;
	}

	return (width + config.font_spacing_horizontal) / (config.font_width + config.font_spacing_horizontal);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
cgui_config_fit_rows(double height)
{
	if (cgui_error() || height <= 0.0)
	{
		return 0;
	}

	return (height + config.font_spacing_vertical) / (config.font_height + config.font_spacing_vertical);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const struct cgui_config *
cgui_config_get(void)
{
	return &config;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

ccfg *
cgui_config_get_parser(void)
{
	return parser;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_config_on_load(void (*fn)(ccfg *cfg))
{
	if (cgui_error())
	{
		return;
	}

	fn_load = fn ? fn : dummy_fn_load;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
cgui_config_str_height(ssize_t rows)
{
	if (cgui_error() || rows == 0)
	{
		return 0;
	}

	if (rows < 0)
	{
		return cgui_config_str_width(-rows);
	}

	return config.font_height * rows + config.font_spacing_vertical * (rows - 1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

double
cgui_config_str_width(ssize_t cols)
{
	if (cgui_error() || cols == 0)
	{
		return 0;
	}

	if (cols < 0)
	{
		return cgui_config_str_height(-cols);
	}

	return config.font_width * cols + config.font_spacing_horizontal * (cols - 1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_config_style_box(const char *name, struct cgui_box *box)
{
	struct resource box_resources[] = {BOX(name, (*box))};
	
	if (cgui_error())
	{
		return;
	}

	LOAD(box_resources);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgui_config_style_text(const char *name, struct cgui_text *text)
{
	struct resource text_resources[] = {TEXT(name, (*text))};
	
	if (cgui_error())
	{
		return;
	}

	LOAD(text_resources);
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

cairo_font_options_t *
config_font_options(void)
{
	return font_opts;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
config_init(const char *app_name, const char *app_class)
{
	cstr *home;
	cstr *home2;

	if (cgui_error())
	{
		return;
	}

	/* instantiation */

	home   = cstr_create();
	home2  = cstr_create();
	parser = ccfg_create();
	dict   = cdict_create();

	/* parser setup */

	cstr_append(home, util_env_exists("HOME") ? getenv("HOME") : getpwuid(getuid())->pw_dir);
	cstr_append(home2, home);
	cstr_append(home,  "/.config/cassette/cgui.ccfg");
	cstr_append(home2, "/.config/cgui.ccfg");

	ccfg_push_source(parser, util_env_exists(ENV_CONF_SOURCE) ? getenv(ENV_CONF_SOURCE) : "");
	ccfg_push_source(parser, cstr_chars(home));
	ccfg_push_source(parser, cstr_chars(home2));
	ccfg_push_source(parser, "/usr/share/cassette/cgui.ccfg");
	ccfg_push_source(parser, "/etc/cassette/cgui.ccfg");
	ccfg_push_source(parser, "/etc/cgui.ccfg");

	ccfg_push_param(parser, "app_name",  app_name);
	ccfg_push_param(parser, "app_class", app_class);

	cstr_destroy(home);
	cstr_destroy(home2);

	/* dict setup */

	cdict_prealloc(dict,   sizeof(words) / sizeof(struct word));
	for (size_t i = 0; i < sizeof(words) / sizeof(struct word); i++)
	{
		cdict_write(dict, words[i].name, words[i].type, words[i].value);
	}

	/* cairo font options */

	font_opts = cairo_font_options_create();
	
	/* end */

	update_err();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
config_load(void)
{
	bool tmp;

	/* apply defaults */

	tmp                = config.alt_present;
	config             = config_default;
	config.init        = true;
	config.alt_present = tmp;

	if (cgui_error() || util_env_exists(ENV_NO_PARSING))
	{
		return;
	}

	/* get new resources */

	ccfg_load(parser);

	if (first_load)
	{
		first_load = false;
		LOAD(resources_once);
	}

	LOAD(resources);

	/* fill in the blanks and check of errors */
	
	font_setup();
	fn_load(parser);
	update_err();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
config_repair(void)
{
	ccfg_repair(parser);
	cdict_repair(dict);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
config_reset(void)
{
	cairo_font_options_destroy(font_opts);
	ccfg_destroy(parser);
	cdict_destroy(dict);

	fn_load    = dummy_fn_load;
	config     = config_default;
	parser     = CCFG_PLACEHOLDER;
	dict       = CDICT_PLACEHOLDER;
	font_opts  = NULL;
	first_load = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cgui_swap
config_swap_input(uint8_t id, struct cgui_mods mods, enum config_swap type)
{
	bool modkey;
	const struct cgui_swap none =
	{
		.type  = CGUI_SWAP_TO_NONE,
		.value = CGUI_SWAP_NONE,
	};

	if (id == 0 || id > (type == CONFIG_SWAP_KEYS ? CGUI_CONFIG_KEYS : CGUI_CONFIG_BUTTONS))
	{
		return none;
	}

	switch (config.modkey)
	{
		case CGUI_CONFIG_MOD_CTRL:
			modkey = mods.ctrl;
			break;

		case CGUI_CONFIG_MOD_1:
			modkey = mods.mod_1;
			break;

		case CGUI_CONFIG_MOD_4:
			modkey = mods.mod_4;
			break;

		default:
			return none;
	}

	switch (type)
	{
		case CONFIG_SWAP_KEYS:
			return config.keys[id][modkey ? (mods.shift ? 2 : 1) : 0];

		case CONFIG_SWAP_BUTTONS:
			return config.buttons[id][modkey ? (mods.shift ? 2 : 1) : 0];

		default:
			return none;
	}
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
dummy_fn_load (ccfg *cfg)
{
	(void)cfg;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
fetch(const struct resource resource)
{
	const char *str;

	ccfg_fetch(parser, resource.namespace, resource.name);
	if (ccfg_iterate(parser))
	{
		str = ccfg_resource(parser);
	}
	else
	{
		return;
	}

	switch (resource.type)
	{
		case STRING:
			snprintf((char*)resource.target, CGUI_CONFIG_STR_LEN, "%s", str);
			break;

		case COLOR:
			*(struct ccolor*)resource.target = ccolor_from_str(str, NULL);
			break;

		case BOOL:
			*(bool*)resource.target = !(fabs(strtod(str, NULL)) < DBL_EPSILON);
			break;

		case POSITION:
			*(double*)resource.target = ceil(util_str_to_double(str, -DBL_MAX, DBL_MAX));
			break;

		case LENGTH:
			*(double*)resource.target = ceil(util_str_to_double(str, 0.0, DBL_MAX));
			break;

		case LONG:
			*(long*)resource.target = util_str_to_long(str, LONG_MIN, LONG_MAX);
			break;

		case ULONG:
			*(unsigned long*)resource.target = util_str_to_long(str, 0, LONG_MAX);
			break;

		case DOUBLE:
			*(double*)resource.target = util_str_to_double(str, -DBL_MAX, DBL_MAX);
			break;

		case SCALE:
		case UDOUBLE:
			*(double*)resource.target = util_str_to_double(str, 0.0, DBL_MAX);
			break;

		case RATIO:
			*(double*)resource.target = util_str_to_double(str, 0.0, 1.0);
			break;

		case BUTTON_ID:
			*(uint8_t*)resource.target = util_str_to_long(str, 0, CGUI_CONFIG_BUTTONS);
			break;
		
		case KEY_ID:
			*(uint8_t*)resource.target = util_str_to_long(str, 0, CGUI_CONFIG_KEYS);
			break;
			
		case MOD_KEY:
		case ANTIALIAS:
		case SUBPIXEL:
			cdict_find(dict, str, resource.type, (size_t*)resource.target);
			break;

		case MAP_KEY:
			swap(str, CGUI_CONFIG_KEYS, (struct cgui_swap*)resource.target);
			break;

		case MAP_BUTTON:
			swap(str, CGUI_CONFIG_BUTTONS, (struct cgui_swap*)resource.target);
			break;

		case CORNER_TYPE:
			set_corners(CORNER_TYPE, resource.target);
			break;

		case CORNER_SIZE:
			set_corners(CORNER_SIZE, resource.target);
			break;

		default:
			return;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
font_setup(void)
{
	cairo_antialias_t      antialias    = CAIRO_ANTIALIAS_DEFAULT;
	cairo_subpixel_order_t subpixel     = CAIRO_SUBPIXEL_ORDER_DEFAULT;
	cairo_hint_metrics_t   hint_metrics = CAIRO_HINT_METRICS_DEFAULT;
	cairo_hint_style_t     hint_style   = CAIRO_HINT_STYLE_SLIGHT;

	cairo_font_extents_t font_ext;
	cairo_text_extents_t text_ext;
	cairo_surface_t *surface;
	cairo_t *context;

	/* font options */

	switch (config.font_antialias)
	{
		case CGUI_CONFIG_ANTIALIAS_NONE:
			antialias = CAIRO_ANTIALIAS_NONE;
			break;

		case CGUI_CONFIG_ANTIALIAS_GRAY:
			antialias = CAIRO_ANTIALIAS_GRAY;
			break;

		case CGUI_CONFIG_ANTIALIAS_SUBPIXEL:
			antialias = CAIRO_ANTIALIAS_SUBPIXEL;
			break;
	}

	switch (config.font_subpixel)
	{
		case CGUI_CONFIG_SUBPIXEL_RGB:
			subpixel = CAIRO_SUBPIXEL_ORDER_RGB;
			break;

		case CGUI_CONFIG_SUBPIXEL_BGR:
			subpixel = CAIRO_SUBPIXEL_ORDER_BGR;
			break;

		case CGUI_CONFIG_SUBPIXEL_VRGB:
			subpixel = CAIRO_SUBPIXEL_ORDER_VRGB;
			break;

		case CGUI_CONFIG_SUBPIXEL_VBGR:
			subpixel = CAIRO_SUBPIXEL_ORDER_VBGR;
			break;
	}

	hint_metrics = config.font_enable_hint_metrics
		? CAIRO_HINT_METRICS_ON : CAIRO_HINT_METRICS_OFF;

	cairo_font_options_set_antialias(font_opts, antialias);
	cairo_font_options_set_subpixel_order(font_opts, subpixel);
	cairo_font_options_set_hint_metrics(font_opts, hint_metrics);
	cairo_font_options_set_hint_style(font_opts, hint_style);

	/* font geometry */

	if (config.font_enable_overrides)
	{
		config.font_descent = config.font_override_descent;
		config.font_ascent  = config.font_override_ascent;
		config.font_width   = config.font_override_width;
		goto skip_auto_font;
	}

	surface = cairo_image_surface_create(CAIRO_FORMAT_A1, 0, 0);
	context = cairo_create(surface);
	if (cairo_surface_status(surface) != CAIRO_STATUS_SUCCESS
	 || cairo_status(context)         != CAIRO_STATUS_SUCCESS)
	{
		main_set_error(CERR_CONFIG);
		goto skip_font_setup;
	}
	
	cairo_set_font_size(context, config.font_size);
	cairo_set_font_options(context, font_opts);
	cairo_select_font_face(
		context,
		config.font_face,
		CAIRO_FONT_SLANT_NORMAL,
		CAIRO_FONT_WEIGHT_NORMAL);
	
	cairo_font_extents(context, &font_ext);
	cairo_text_extents(context, "A", &text_ext);

	config.font_descent = font_ext.descent;
	config.font_ascent  = font_ext.ascent;
	config.font_width   = text_ext.width;

skip_font_setup:

	cairo_destroy(context);
	cairo_surface_destroy(surface);

skip_auto_font:

	config.font_height = config.font_ascent + config.font_descent;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
scale(const struct resource resource)
{
	switch (resource.type)
	{
		case POSITION:
		case LENGTH:
		case DOUBLE:
		case UDOUBLE:
			*(double*)resource.target *= config.scale;
			break;

		case CORNER_SIZE:
			for (size_t i = 0; i < 4; i++)
			{
				((double*)resource.target)[i] *= config.scale;
			}

		default:
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
set_corners(enum value variant, void *target)
{
	enum cgui_corner *types;
	double *sizes;
	const char *str;
	size_t tmp;
	size_t n = 0;

	types = (enum cgui_corner*)target;
	sizes = (double*)target;

	/* fill array values */

	do
	{
		str = ccfg_resource(parser);
		switch (variant)
		{
			case CORNER_TYPE:
				cdict_find(dict, str, CORNER_TYPE, &tmp);
				types[n] = tmp;
				break;

			case CORNER_SIZE:
				sizes[n] = util_str_to_double(str, 0, DBL_MAX);
				break;

			default:
				return;
		}
	}
	while (++n < 4 && ccfg_iterate(parser));

	/* fill remaining value with last set value */

	for (size_t i = n--; i < 4; i++)
	{
		switch (variant)
		{
			case CORNER_TYPE:
				types[i] = types[n];
				break;

			case CORNER_SIZE:
				sizes[i] = sizes[n];
				break;

			default:
				return;
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
swap(const char *str, uint8_t limit, struct cgui_swap *target)
{
	char *str_copy;
	char *ctx;
	char *l;
	char *r;
	size_t tmp = 0;

	if (!(str_copy = strdup(str)))
	{
		main_set_error(CERR_CONFIG);
		return;
	}

	l = strtok_r(str_copy, ":", &ctx);
	r = strtok_r(NULL,     ":", &ctx);
	if (!l || !r)
	{
		free(str_copy);
		return;
	}

	cdict_find(dict, l, SWAP_KIND, &tmp);
	target->type = tmp;

	switch (target->type)
	{
		case CGUI_SWAP_TO_VALUE:
			target->value = util_str_to_long(r, 0, limit);
			break;

		case CGUI_SWAP_TO_ACCELERATOR:
			target->value = util_str_to_long(r, 1, CGUI_CONFIG_ACCELS);
			break;

		case CGUI_SWAP_TO_CLIPBOARD_CUT:
		case CGUI_SWAP_TO_CLIPBOARD_COPY:
		case CGUI_SWAP_TO_CLIPBOARD_PASTE:
			target->value = util_str_to_long(r, 1, CGUI_CLIPBOARDS);
			break;

		case CGUI_SWAP_TO_FOCUS:
			tmp = 0;
			cdict_find(dict, r, FOCUS, &tmp);
			target->value = tmp;
			break;

		case CGUI_SWAP_TO_ACTION_CELL:
		case CGUI_SWAP_TO_ACTION_WINDOW:
		case CGUI_SWAP_TO_ACTION_MISC:
			tmp = 0;
			cdict_find(dict, r, SWAP_ACTION, &tmp);
			target->value = tmp;
			break;

		case CGUI_SWAP_TO_DEFAULT:
		case CGUI_SWAP_TO_NONE:
		default:
			break;
	}

	free(str_copy);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
update_err(void)
{
	if (ccfg_error(parser)
	 || cdict_error(dict)
	 || cairo_font_options_status(font_opts) != CAIRO_STATUS_SUCCESS)
	{
		main_set_error(CERR_CONFIG);
	}
}
