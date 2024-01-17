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

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <cairo/cairo.h>

#include "color.h"
#include "config.h"
#include "config-private.h"
#include "errno.h"
#include "hashtable.h"
#include "resource.h"
#include "util.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

typedef enum {
	_WORD_MODKEY,
	_WORD_SWAP_TYPE,
	_WORD_ACTION_CELL,
	_WORD_ACTION_FOCUS,
	_WORD_ACTION_WINDOW,
	_WORD_ACTION_MISC,
	_WORD_FONT_OPTION,
} _word_group_t;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define _SCALE(X) X *= dg_core_config_get()->scale;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void _parse_button (char *prop, char *value);
static void _parse_key    (char *prop, char *value);
static void _parse_swap   (char *prop, char *value, dg_core_config_swap_t (*swaps)[3], size_t swaps_n);

static void _postprocess   (void);
static bool _preprocess    (void);
static void _set_defaults  (void);
static void _set_generated (void);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/* containers for string values to be post-processed */

static char _raw_meta[DG_CORE_RESOURCE_STR_LEN]      = "";
static char _raw_antialias[DG_CORE_RESOURCE_STR_LEN] = "";
static char _raw_subpixel[DG_CORE_RESOURCE_STR_LEN]  = "";

/* config data */

static dg_core_config_t _conf = {0};

static const dg_core_resource_t _res[] = {
	{ "scale",                            DG_CORE_RESOURCE_UDOUBLE, &_conf.scale                    },
	{ "meta_modifier",                    DG_CORE_RESOURCE_STR,     &_raw_meta                      },

	{ "font_face",                        DG_CORE_RESOURCE_STR,     &_conf.ft_face                  },
	{ "font_enable_overrides",            DG_CORE_RESOURCE_BOOL,    &_conf.ft_overrides             },
	{ "font_size",                        DG_CORE_RESOURCE_UINT16,  &_conf.ft_size                  },
	{ "font_x_offset",                    DG_CORE_RESOURCE_INT16,   &_conf.ft_offset_x              },
	{ "font_y_offset",                    DG_CORE_RESOURCE_INT16,   &_conf.ft_offset_y              },
	{ "font_horizontal_spacing",          DG_CORE_RESOURCE_UINT16,  &_conf.ft_spacing_w             },
	{ "font_vertical_spacing",            DG_CORE_RESOURCE_UINT16,  &_conf.ft_spacing_h             },
	{ "font_override_ascent",             DG_CORE_RESOURCE_UINT16,  &_conf.ft_override_ascent       },
	{ "font_override_descent",            DG_CORE_RESOURCE_UINT16,  &_conf.ft_override_descent      },
	{ "font_override_width",              DG_CORE_RESOURCE_UINT16,  &_conf.ft_override_pw           },
	{ "font_enable_hint_metrics",         DG_CORE_RESOURCE_BOOL,    &_conf.ft_hint_metrics          },
	{ "font_antialias",                   DG_CORE_RESOURCE_STR,     &_raw_antialias                 },
	{ "font_subpixel_order",              DG_CORE_RESOURCE_STR,     &_raw_subpixel                  },

	{ "window_enable_dynamic_border",     DG_CORE_RESOURCE_BOOL,    &_conf.win_dynamic_bd           },
	{ "window_focus_when_activated",      DG_CORE_RESOURCE_BOOL,    &_conf.win_focused_on_activate  },
	{ "window_thickness_border",          DG_CORE_RESOURCE_UINT16,  &_conf.win_thick_bd             },
	{ "window_padding_inner",             DG_CORE_RESOURCE_UINT16,  &_conf.win_pad_inner            },
	{ "window_padding_outer",             DG_CORE_RESOURCE_UINT16,  &_conf.win_pad_outer            },
	{ "window_padding_cell",              DG_CORE_RESOURCE_UINT16,  &_conf.win_pad_cell             },
	{ "window_color_background",          DG_CORE_RESOURCE_COLOR,   &_conf.win_cl_bg                },
	{ "window_color_border",              DG_CORE_RESOURCE_COLOR,   &_conf.win_cl_bd                },
	{ "window_color_border_focused",      DG_CORE_RESOURCE_COLOR,   &_conf.win_cl_bd_focused        },
	{ "window_color_border_disabled",     DG_CORE_RESOURCE_COLOR,   &_conf.win_cl_bd_disabled       },
	{ "window_color_border_locked",       DG_CORE_RESOURCE_COLOR,   &_conf.win_cl_bd_locked         },

	{ "popup_enable_position_overrides",  DG_CORE_RESOURCE_BOOL,    &_conf.popup_override_position  },
	{ "popup_enable_width_override",      DG_CORE_RESOURCE_BOOL,    &_conf.popup_override_width     },
	{ "popup_enable_height_override",     DG_CORE_RESOURCE_BOOL,    &_conf.popup_override_height    },
	{ "popup_max_width",                  DG_CORE_RESOURCE_UINT16,  &_conf.popup_max_pw             },
	{ "popup_max_height",                 DG_CORE_RESOURCE_UINT16,  &_conf.popup_max_ph             },
	{ "popup_override_x_position",        DG_CORE_RESOURCE_INT16,   &_conf.popup_override_px        },
	{ "popup_override_y_position",        DG_CORE_RESOURCE_INT16,   &_conf.popup_override_py        },
	{ "popup_override_width",             DG_CORE_RESOURCE_UINT16,  &_conf.popup_override_pw        },
	{ "popup_override_height",            DG_CORE_RESOURCE_UINT16,  &_conf.popup_override_ph        },

	{ "misc_allow_cell_to_lock_focus",    DG_CORE_RESOURCE_BOOL,    &_conf.cell_auto_lock           },
	{ "misc_enable_persistent_pointer",   DG_CORE_RESOURCE_BOOL,    &_conf.input_persistent_pointer },
	{ "misc_enable_persistent_touch",     DG_CORE_RESOURCE_BOOL,    &_conf.input_persistent_touch   },
	{ "misc_animation_framerate_divider", DG_CORE_RESOURCE_UINT,    &_conf.anim_divider             },
};

static const dg_core_resource_group_t _group_main = {
	.kind           = DG_CORE_RESOURCE_GROUP_DEFAULT,
	.namespace      = "core",
	.fn_preprocess  = _preprocess,
	.fn_postprocess = _postprocess,
	.resources      = _res,
	.n              = DG_CORE_RESOURCE_LEN(_res),
};

static const dg_core_resource_group_t _group_button = {
	.kind           = DG_CORE_RESOURCE_GROUP_CUSTOM,
	.namespace      = "button",
	.fn_preprocess  = _preprocess,
	.fn_postprocess = _postprocess,
	.fn_parse       = _parse_button,
};

static const dg_core_resource_group_t _group_key = {
	.kind           = DG_CORE_RESOURCE_GROUP_CUSTOM,
	.namespace      = "key",
	.fn_preprocess  = _preprocess,
	.fn_postprocess = _postprocess,
	.fn_parse       = _parse_key,
};

static const dg_core_resource_group_t *const _groups[] = {
	&_group_main,
	&_group_button,
	&_group_key,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/* temp data */

static dg_core_hashtable_t _hm = {.slots = NULL, .n = 0, .n_alloc = 0};

static bool _preprocess_done  = false;
static bool _postprocess_done = false;

/* consts */

static const dg_core_util_fat_dict_t _words[] = {
	{ "mod1",      DG_CORE_CONFIG_MOD_1,                    _WORD_MODKEY        },
	{ "mod2",      DG_CORE_CONFIG_MOD_2,                    _WORD_MODKEY        },
	{ "mod3",      DG_CORE_CONFIG_MOD_3,                    _WORD_MODKEY        },
	{ "mod4",      DG_CORE_CONFIG_MOD_4,                    _WORD_MODKEY        },
	{ "mod5",      DG_CORE_CONFIG_MOD_5,                    _WORD_MODKEY        },
	{ "ctrl",      DG_CORE_CONFIG_MOD_CTRL,                 _WORD_MODKEY        },
	{ "lock",      DG_CORE_CONFIG_MOD_LOCK,                 _WORD_MODKEY        },
	{ "shift",     DG_CORE_CONFIG_MOD_SHIFT,                _WORD_MODKEY        },

	{ "default",   DG_CORE_CONFIG_SWAP_TO_DEFAULT,          _WORD_SWAP_TYPE     },
	{ "none",      DG_CORE_CONFIG_SWAP_TO_NONE,             _WORD_SWAP_TYPE     },
	{ "value",     DG_CORE_CONFIG_SWAP_TO_VALUE,            _WORD_SWAP_TYPE     },
	{ "accel",     DG_CORE_CONFIG_SWAP_TO_ACCELERATOR,      _WORD_SWAP_TYPE     },
	{ "cut",       DG_CORE_CONFIG_SWAP_TO_CLIPBOARD_CUT,    _WORD_SWAP_TYPE     },
	{ "copy",      DG_CORE_CONFIG_SWAP_TO_CLIPBOARD_COPY,   _WORD_SWAP_TYPE     },
	{ "paste",     DG_CORE_CONFIG_SWAP_TO_CLIPBOARD_PASTE,  _WORD_SWAP_TYPE     },
	{ "cell",      DG_CORE_CONFIG_SWAP_TO_ACTION_CELL,      _WORD_SWAP_TYPE     },
	{ "focus",     DG_CORE_CONFIG_SWAP_TO_ACTION_FOCUS,     _WORD_SWAP_TYPE     },
	{ "window",    DG_CORE_CONFIG_SWAP_TO_ACTION_WINDOW,    _WORD_SWAP_TYPE     },
	{ "misc",      DG_CORE_CONFIG_SWAP_TO_ACTION_MISC,      _WORD_SWAP_TYPE     },

	{ "select-",   DG_CORE_CONFIG_ACTION_CELL_SELECT_LESS,  _WORD_ACTION_CELL   },
	{ "select+",   DG_CORE_CONFIG_ACTION_CELL_SELECT_MORE,  _WORD_ACTION_CELL   },
	{ "unselect",  DG_CORE_CONFIG_ACTION_CELL_SELECT_NONE,  _WORD_ACTION_CELL   },
	{ "selectAll", DG_CORE_CONFIG_ACTION_CELL_SELECT_ALL,   _WORD_ACTION_CELL   },
	{ "redraw",    DG_CORE_CONFIG_ACTION_CELL_SELECT_ALL,   _WORD_ACTION_CELL   },
	{ "trigger1",  DG_CORE_CONFIG_ACTION_CELL_TRIGGER_1,    _WORD_ACTION_CELL   },
	{ "trigger2",  DG_CORE_CONFIG_ACTION_CELL_TRIGGER_2,    _WORD_ACTION_CELL   },
	{ "trigger3",  DG_CORE_CONFIG_ACTION_CELL_TRIGGER_3,    _WORD_ACTION_CELL   },
	{ "trigger4",  DG_CORE_CONFIG_ACTION_CELL_TRIGGER_4,    _WORD_ACTION_CELL   },
	{ "trigger5",  DG_CORE_CONFIG_ACTION_CELL_TRIGGER_5,    _WORD_ACTION_CELL   },

	{ "left",      DG_CORE_CONFIG_ACTION_FOCUS_LEFT,        _WORD_ACTION_FOCUS  },
	{ "right",     DG_CORE_CONFIG_ACTION_FOCUS_RIGHT,       _WORD_ACTION_FOCUS  },
	{ "up",        DG_CORE_CONFIG_ACTION_FOCUS_UP,          _WORD_ACTION_FOCUS  },
	{ "down",      DG_CORE_CONFIG_ACTION_FOCUS_DOWN,        _WORD_ACTION_FOCUS  },
	{ "leftmost",  DG_CORE_CONFIG_ACTION_FOCUS_LEFTMOST,    _WORD_ACTION_FOCUS  },
	{ "rightmost", DG_CORE_CONFIG_ACTION_FOCUS_RIGHTMOST,   _WORD_ACTION_FOCUS  },
	{ "top",       DG_CORE_CONFIG_ACTION_FOCUS_TOP,         _WORD_ACTION_FOCUS  },
	{ "bottom",    DG_CORE_CONFIG_ACTION_FOCUS_BOTTOM,      _WORD_ACTION_FOCUS  },
	{ "next",      DG_CORE_CONFIG_ACTION_FOCUS_NEXT,        _WORD_ACTION_FOCUS  },
	{ "previous",  DG_CORE_CONFIG_ACTION_FOCUS_PREV,        _WORD_ACTION_FOCUS  },
	{ "first",     DG_CORE_CONFIG_ACTION_FOCUS_FIRST,       _WORD_ACTION_FOCUS  },
	{ "last",      DG_CORE_CONFIG_ACTION_FOCUS_LAST,        _WORD_ACTION_FOCUS  },
	{ "none",      DG_CORE_CONFIG_ACTION_FOCUS_NONE,        _WORD_ACTION_FOCUS  },

	{ "lockGrid",  DG_CORE_CONFIG_ACTION_WINDOW_LOCK_GRID,  _WORD_ACTION_WINDOW },
	{ "lockFocus", DG_CORE_CONFIG_ACTION_WINDOW_LOCK_FOCUS, _WORD_ACTION_WINDOW },
	{ "redraw",    DG_CORE_CONFIG_ACTION_WINDOW_LOCK_FOCUS, _WORD_ACTION_WINDOW },
	
	{ "reconfig",  DG_CORE_CONFIG_ACTION_MISC_RECONFIG,     _WORD_ACTION_MISC   },
	{ "exit",      DG_CORE_CONFIG_ACTION_MISC_EXIT,         _WORD_ACTION_MISC   },

	{ "none",      DG_CORE_CONFIG_ANTIALIAS_NONE,           _WORD_FONT_OPTION   },
	{ "gray",      DG_CORE_CONFIG_ANTIALIAS_GRAY,           _WORD_FONT_OPTION   },
	{ "subpixel",  DG_CORE_CONFIG_ANTIALIAS_SUBPIXEL,       _WORD_FONT_OPTION   },
	{ "rgb",       DG_CORE_CONFIG_SUBPIXEL_RGB,             _WORD_FONT_OPTION   },
	{ "bgr",       DG_CORE_CONFIG_SUBPIXEL_BGR,             _WORD_FONT_OPTION   },
	{ "vrgb",      DG_CORE_CONFIG_SUBPIXEL_VRGB,            _WORD_FONT_OPTION   },
	{ "vbgr",      DG_CORE_CONFIG_SUBPIXEL_VBGR,            _WORD_FONT_OPTION   },
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

int16_t
dg_core_config_convert_str_height(int16_t ch)
{
	if (ch > 0) {
		return _conf.ft_ph *  ch + _conf.ft_spacing_h * (ch - 1);
	} else if (ch < 0) {
		return _conf.ft_pw * -ch - _conf.ft_spacing_w * (ch + 1);
	} else {
		return 0;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

int16_t
dg_core_config_convert_str_width(int16_t cw)
{
	return dg_core_config_convert_str_height(-cw);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const dg_core_config_t *
dg_core_config_get(void)
{
	return &_conf;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

int16_t
dg_core_config_fit_str_height(int16_t ph)
{
	if (ph <= _conf.ft_ph) {
		return 0;
	}
	
	return (ph + _conf.ft_spacing_h) / (_conf.ft_ph + _conf.ft_spacing_h);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

int16_t
dg_core_config_fit_str_width(int16_t pw)
{
	if (pw <= _conf.ft_pw) {
		return 0;
	}

	return (pw + _conf.ft_spacing_w) / (_conf.ft_pw + _conf.ft_spacing_w);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

int16_t
dg_core_config_get_cell_height(int16_t ch)
{
	if (ch == 0) {
		return 0;
	} else {
		return 2 * _conf.win_pad_cell + dg_core_config_convert_str_height(ch);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

int16_t
dg_core_config_get_cell_width(int16_t cw)
{
	return dg_core_config_get_cell_height(-cw);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const dg_core_resource_group_t
dg_core_config_get_group_copy(dg_core_config_group_t group)
{
	switch (group) {
		
		case DG_CORE_CONFIG_BUTTON:
			return _group_button;

		case DG_CORE_CONFIG_KEY:
			return _group_key;

		case DG_CORE_CONFIG_MAIN:
		default:
			return _group_main;	
	}
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

bool
dg_core_config_init(void)
{
	bool err = false;

	err |= !dg_core_resource_push_group(&_group_main);
	err |= !dg_core_resource_push_group(&_group_button);
	err |= !dg_core_resource_push_group(&_group_key);
	if (err) {
		dg_core_config_reset();
		return false;
	}

	dg_core_resource_load_groups(_groups, sizeof(_groups) / sizeof(dg_core_resource_group_t*));

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_config_reset(void)
{
	dg_core_resource_pull_group(&_group_main);
	dg_core_resource_pull_group(&_group_button);
	dg_core_resource_pull_group(&_group_key);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_postprocess(void)
{
	/* postprocessor duplicate guard */

	if (_postprocess_done) {
		return;
	}

	/* convert raw strings */

	bool found;
	size_t val;

	val = dg_core_hashtable_get_value(&_hm, _raw_meta, _WORD_MODKEY, &found);
	if (found && (val & (DG_CORE_CONFIG_MOD_CTRL | DG_CORE_CONFIG_MOD_1 | DG_CORE_CONFIG_MOD_4))) {
		_conf.mod_meta = val;
	}

	val = dg_core_hashtable_get_value(&_hm, _raw_antialias, _WORD_FONT_OPTION, &found);
	if (found) {
		_conf.ft_antialias = val;
	}

	val = dg_core_hashtable_get_value(&_hm, _raw_subpixel, _WORD_FONT_OPTION, &found);
	if (found) {
		_conf.ft_subpixel = val;
	}

	/* apply generated values */

	_set_generated();

	/* cleanup  & end */

	dg_core_hashtable_reset(&_hm);

	_preprocess_done  = false;
	_postprocess_done = true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_preprocess(void)
{
	/* preprocessor duplicate guard */

	if (_preprocess_done) {
		return true;
	}
	
	/* setup keyword hashtable for postprocess and swap parsing */

	if (!dg_core_hashtable_init(&_hm, DG_CORE_UTIL_FAT_DICT_LEN(_words))) {
		return false;
	}

	for (size_t i = 0; i < DG_CORE_UTIL_FAT_DICT_LEN(_words); i++) {
		dg_core_hashtable_set_value(&_hm, _words[i].key, _words[i].group, _words[i].value);
	}

	/* apply default values */

	_set_defaults();

	/* end */

	_preprocess_done  = true;
	_postprocess_done = false;

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_parse_button(char *prop, char *value)
{
	_parse_swap(prop, value, _conf.swap_but, DG_CORE_CONFIG_MAX_BUTTONS + 1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_parse_key(char *prop, char *value)
{
	_parse_swap(prop, value, _conf.swap_key, DG_CORE_CONFIG_MAX_KEYS + 1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_parse_swap(char *prop, char *value, dg_core_config_swap_t (*swaps)[3], size_t swaps_n)
{
	if (!prop || !value) {
		return;
	}

	int  lvl = 0;
	long pos;

	char *e = NULL;

	/* get modifiers and swap position */

	switch (prop[0]) {

		case 'M':
			lvl = 1;
			prop++;
			break;

		case 'S':
			lvl = 2;
			prop++;
			break;

		case '\0':
			return;
	}

	pos = strtol(prop, &e, 10);
	if (*e != '\0' || pos > swaps_n - 1) {
		return;
	}

	/* split value */

	bool found;

	dg_core_config_swap_kind_t i_cat;
	uint8_t i_val;

	char *s_ctx = NULL;
	char *s_cat = dg_core_util_trim_str(strtok_r(value,  ":", &s_ctx));
	char *s_val = dg_core_util_trim_str(strtok_r(NULL, ":", &s_ctx));
	if (!s_cat) {
		return;
	}

	/* get swap kind */

	i_cat = dg_core_hashtable_get_value(&_hm, s_cat, _WORD_SWAP_TYPE, &found);
	if (!found) {
		return;
	}

	if (!s_val && i_cat != DG_CORE_CONFIG_SWAP_TO_DEFAULT && i_cat != DG_CORE_CONFIG_SWAP_TO_NONE) {
		return;
	}

	/* get swap value */

	found = true;
	e = NULL;

	switch (i_cat) {

		case DG_CORE_CONFIG_SWAP_TO_DEFAULT:
		case DG_CORE_CONFIG_SWAP_TO_NONE:
			i_val = 0;
			break;

		case DG_CORE_CONFIG_SWAP_TO_VALUE:
			i_val = strtol(s_val, &e, 10);
			if (*e != '\0' || i_val > swaps_n - 1) {
				return;
			}
			break;

		case DG_CORE_CONFIG_SWAP_TO_ACCELERATOR:
			i_val = strtol(s_val, &e, 10);
			if (*e != '\0' || i_val > DG_CORE_CONFIG_MAX_ACCELS || i_val == 0) {
				return;
			}
			break;

		case DG_CORE_CONFIG_SWAP_TO_CLIPBOARD_CUT:
		case DG_CORE_CONFIG_SWAP_TO_CLIPBOARD_COPY:
		case DG_CORE_CONFIG_SWAP_TO_CLIPBOARD_PASTE:
			i_val = strtol(s_val, &e, 10);
			if (*e != '\0' || i_val >= 2) {
				return;
			}
			break;

		case DG_CORE_CONFIG_SWAP_TO_ACTION_CELL:
			i_val = dg_core_hashtable_get_value(&_hm, s_val, _WORD_ACTION_CELL, &found);
			break;

		case DG_CORE_CONFIG_SWAP_TO_ACTION_FOCUS:
			i_val = dg_core_hashtable_get_value(&_hm, s_val, _WORD_ACTION_FOCUS, &found);
			break;

		case DG_CORE_CONFIG_SWAP_TO_ACTION_WINDOW:
			i_val = dg_core_hashtable_get_value(&_hm, s_val, _WORD_ACTION_WINDOW, &found);
			break;

		case DG_CORE_CONFIG_SWAP_TO_ACTION_MISC:
			i_val = dg_core_hashtable_get_value(&_hm, s_val, _WORD_ACTION_MISC, &found);
			break;

		default:
			return;
	}

	if (!found) {
		return;
	}

	/* apply parsed swap data to swap table */

	swaps[pos][lvl].kind  = i_cat;
	swaps[pos][lvl].value = i_val;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_set_defaults(void)
{
	_conf.scale    = 1.0;
	_conf.mod_meta = DG_CORE_CONFIG_MOD_CTRL;

	/* font */

	strncpy(_conf.ft_face, "Monospace", DG_CORE_RESOURCE_STR_LEN - 1);

	_conf.ft_overrides = false;

	_conf.ft_size      = 14;
	_conf.ft_offset_x  = 0;
	_conf.ft_offset_y  = 0;
	_conf.ft_spacing_w = 0;
	_conf.ft_spacing_h = 2;

	_conf.ft_override_ascent  = 14;
	_conf.ft_override_descent = 0;
	_conf.ft_override_pw      = 7;

	_conf.ft_hint_metrics = true;
	_conf.ft_antialias    = DG_CORE_CONFIG_ANTIALIAS_SUBPIXEL;
	_conf.ft_subpixel     = DG_CORE_CONFIG_SUBPIXEL_RGB;

	/* window */

	_conf.win_focused_on_activate = false;
	_conf.win_dynamic_bd          = false;
	
	_conf.win_thick_bd  = 0;
	_conf.win_pad_outer = 3;
	_conf.win_pad_inner = 3;
	_conf.win_pad_cell  = 15;

	_conf.win_cl_bg          = (dg_core_color_t){0.000, 0.000, 0.000, 1.000};
	_conf.win_cl_bd          = (dg_core_color_t){0.000, 0.000, 0.000, 1.000};
	_conf.win_cl_bd_focused  = (dg_core_color_t){0.671, 0.506, 0.141, 1.000};
	_conf.win_cl_bd_disabled = (dg_core_color_t){0.400, 0.400, 0.400, 1.000};
	_conf.win_cl_bd_locked   = (dg_core_color_t){0.500, 0.100, 0.100, 1.000};

	/* popup */

	_conf.popup_override_position = false;
	_conf.popup_override_width    = false;
	_conf.popup_override_height   = false;

	_conf.popup_override_px = 0;
	_conf.popup_override_py = 0;
	_conf.popup_override_pw = 0;
	_conf.popup_override_ph = 0; 

	_conf.popup_max_pw = 0;
	_conf.popup_max_ph = 0;

	/* misc */

	_conf.cell_auto_lock           = true;
	_conf.input_persistent_pointer = false;
	_conf.input_persistent_touch   = false;
	_conf.anim_divider             = 1;

	/* input swaps */

	memset(_conf.swap_key, 0, (DG_CORE_CONFIG_MAX_KEYS    + 1) * 3 * sizeof(dg_core_config_swap_t));
	memset(_conf.swap_but, 0, (DG_CORE_CONFIG_MAX_BUTTONS + 1) * 3 * sizeof(dg_core_config_swap_t));

	_conf.swap_key[ 67][0] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACCELERATOR,   1};
	_conf.swap_key[ 68][0] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACCELERATOR,   2};
	_conf.swap_key[ 69][0] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACCELERATOR,   3};
	_conf.swap_key[ 70][0] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACCELERATOR,   4};
	_conf.swap_key[ 71][0] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACCELERATOR,   5};
	_conf.swap_key[ 72][0] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACCELERATOR,   6};
	_conf.swap_key[ 73][0] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACCELERATOR,   7};
	_conf.swap_key[ 74][0] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACCELERATOR,   8};
	_conf.swap_key[ 75][0] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACCELERATOR,   9};
	_conf.swap_key[ 76][0] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACCELERATOR,   10};
	_conf.swap_key[ 95][0] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACCELERATOR,   11};
	_conf.swap_key[ 96][0] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACCELERATOR,   12};

	_conf.swap_key[  9][1] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACTION_FOCUS,  DG_CORE_CONFIG_ACTION_FOCUS_NONE};
	_conf.swap_key[ 23][1] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACTION_FOCUS,  DG_CORE_CONFIG_ACTION_FOCUS_NEXT};
	_conf.swap_key[ 34][1] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACTION_FOCUS,  DG_CORE_CONFIG_ACTION_FOCUS_FIRST};
	_conf.swap_key[ 35][1] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACTION_FOCUS,  DG_CORE_CONFIG_ACTION_FOCUS_LAST};
	_conf.swap_key[113][1] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACTION_FOCUS,  DG_CORE_CONFIG_ACTION_FOCUS_LEFT};
	_conf.swap_key[114][1] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACTION_FOCUS,  DG_CORE_CONFIG_ACTION_FOCUS_RIGHT};
	_conf.swap_key[111][1] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACTION_FOCUS,  DG_CORE_CONFIG_ACTION_FOCUS_UP};
	_conf.swap_key[116][1] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACTION_FOCUS,  DG_CORE_CONFIG_ACTION_FOCUS_DOWN};
	_conf.swap_key[ 23][2] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACTION_FOCUS,  DG_CORE_CONFIG_ACTION_FOCUS_PREV};
	_conf.swap_key[113][2] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACTION_FOCUS,  DG_CORE_CONFIG_ACTION_FOCUS_LEFTMOST};
	_conf.swap_key[114][2] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACTION_FOCUS,  DG_CORE_CONFIG_ACTION_FOCUS_RIGHTMOST};
	_conf.swap_key[111][2] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACTION_FOCUS,  DG_CORE_CONFIG_ACTION_FOCUS_TOP};
	_conf.swap_key[116][2] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACTION_FOCUS,  DG_CORE_CONFIG_ACTION_FOCUS_BOTTOM};

	_conf.swap_key[ 22][1] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACTION_CELL,   DG_CORE_CONFIG_ACTION_CELL_REDRAW};
	_conf.swap_key[ 22][2] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACTION_WINDOW, DG_CORE_CONFIG_ACTION_WINDOW_REDRAW};
	_conf.swap_key[ 36][1] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACTION_WINDOW, DG_CORE_CONFIG_ACTION_WINDOW_LOCK_FOCUS};
	_conf.swap_key[ 36][2] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACTION_WINDOW, DG_CORE_CONFIG_ACTION_WINDOW_LOCK_GRID};
	
	_conf.swap_key[ 27][1] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACTION_MISC,   DG_CORE_CONFIG_ACTION_MISC_RECONFIG};
	_conf.swap_key[ 54][1] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_ACTION_MISC,   DG_CORE_CONFIG_ACTION_MISC_EXIT};

	_conf.swap_but[  4][1] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_VALUE,         6};
	_conf.swap_but[  5][1] = (dg_core_config_swap_t){DG_CORE_CONFIG_SWAP_TO_VALUE,         7};
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_set_generated(void)
{
	/* geometry and font scaling */

	_SCALE(_conf.ft_size);
	_SCALE(_conf.ft_spacing_w);
	_SCALE(_conf.ft_spacing_h);
	_SCALE(_conf.ft_offset_x);
	_SCALE(_conf.ft_offset_y);
	_SCALE(_conf.ft_override_ascent);
	_SCALE(_conf.ft_override_descent);
	_SCALE(_conf.ft_override_pw);
	_SCALE(_conf.win_thick_bd);
	_SCALE(_conf.win_pad_inner);
	_SCALE(_conf.win_pad_outer);
	_SCALE(_conf.win_pad_cell);

	/* get font geometry with cairo */

	if (_conf.ft_overrides) {
		_conf.ft_descent = _conf.ft_override_descent;
		_conf.ft_ascent  = _conf.ft_override_ascent;
		_conf.ft_pw      = _conf.ft_override_pw;
		goto skip_auto_font;
	}

	cairo_surface_t      *c_srf = cairo_image_surface_create(CAIRO_FORMAT_A1, 0, 0);
	cairo_t              *c_ctx = cairo_create(c_srf);
	cairo_font_options_t *c_opt = cairo_font_options_create();
	if (cairo_surface_status(c_srf)      != CAIRO_STATUS_SUCCESS ||
	    cairo_status(c_ctx)              != CAIRO_STATUS_SUCCESS ||
		cairo_font_options_status(c_opt) != CAIRO_STATUS_SUCCESS) {
		dg_core_errno_set(DG_CORE_ERRNO_CAIRO);
		goto skip_font_setup;
	}
	
	cairo_font_options_set_subpixel_order(c_opt, CAIRO_SUBPIXEL_ORDER_DEFAULT);
	cairo_font_options_set_hint_metrics(c_opt, CAIRO_HINT_METRICS_DEFAULT);
	cairo_font_options_set_hint_style(c_opt, CAIRO_HINT_STYLE_DEFAULT);
	cairo_font_options_set_antialias(c_opt, CAIRO_ANTIALIAS_DEFAULT);

	cairo_set_font_size(c_ctx, _conf.ft_size);
	cairo_set_font_options(c_ctx, c_opt);
	cairo_select_font_face(
		c_ctx,
		_conf.ft_face,
		CAIRO_FONT_SLANT_NORMAL,
		CAIRO_FONT_WEIGHT_NORMAL);

	cairo_font_extents_t f_e;
	cairo_text_extents_t t_e;

	cairo_font_extents(c_ctx, &f_e);
	cairo_text_extents(c_ctx, "A", &t_e);

	_conf.ft_descent = f_e.descent;
	_conf.ft_ascent  = f_e.ascent;
	_conf.ft_pw      = t_e.width;

skip_font_setup:

	cairo_font_options_destroy(c_opt);
	cairo_destroy(c_ctx);
	cairo_surface_destroy(c_srf);

skip_auto_font:

	_conf.ft_ph = _conf.ft_ascent + _conf.ft_descent;
}
