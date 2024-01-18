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

#include <stdbool.h>

#include <dg/core/color.h>
#include <dg/core/config.h>
#include <dg/core/resource.h>

#include "config.h"
#include "config-private.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define _STYLE_RESOURCES(NAME, STYLE) \
	{ NAME "_use_bold_font",         DG_CORE_RESOURCE_BOOL,   &STYLE.ft_bold      }, \
	{ NAME "_thickness_border",      DG_CORE_RESOURCE_UINT16, &STYLE.thick_bd     }, \
	{ NAME "_thickness_separator",   DG_CORE_RESOURCE_UINT16, &STYLE.thick_sep    }, \
	{ NAME "_thickness_limiter",     DG_CORE_RESOURCE_UINT16, &STYLE.thick_lim    }, \
	{ NAME "_thickness_icon_lines",  DG_CORE_RESOURCE_UINT16, &STYLE.thick_icon   }, \
	{ NAME "_margin",                DG_CORE_RESOURCE_UINT16, &STYLE.margin       }, \
	{ NAME "_padding_foreground",    DG_CORE_RESOURCE_UINT16, &STYLE.pad_fg       }, \
	{ NAME "_padding_icon",          DG_CORE_RESOURCE_UINT16, &STYLE.pad_icon     }, \
	{ NAME "_gap_limiter",           DG_CORE_RESOURCE_UINT16, &STYLE.gap_lim      }, \
	{ NAME "_animation_speed",       DG_CORE_RESOURCE_UINT,   &STYLE.anim_speed   }, \
	{ NAME "_color_background",      DG_CORE_RESOURCE_COLOR,  &STYLE.cl_bg        }, \
	{ NAME "_color_border",          DG_CORE_RESOURCE_COLOR,  &STYLE.cl_bd        }, \
	{ NAME "_color_font",            DG_CORE_RESOURCE_COLOR,  &STYLE.cl_ft        }, \
	{ NAME "_color_separator",       DG_CORE_RESOURCE_COLOR,  &STYLE.cl_sep       }, \
	{ NAME "_color_limiter",         DG_CORE_RESOURCE_COLOR,  &STYLE.cl_lim       }, \
	{ NAME "_color_primary",         DG_CORE_RESOURCE_COLOR,  &STYLE.cl_primary   }, \
	{ NAME "_color_secondary",       DG_CORE_RESOURCE_COLOR,  &STYLE.cl_secondary }, \
	{ NAME "_color_highlight",       DG_CORE_RESOURCE_COLOR,  &STYLE.cl_highlight },

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void _scale_style   (dg_base_config_style_t *style);
static bool _set_defaults  (void);
static void _set_generated (void);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static dg_base_config_t _conf = {0};

static const dg_core_resource_t _res[] = {
	{ "common_color_black",           DG_CORE_RESOURCE_COLOR,   &_conf.common_cl[ 0]         },
	{ "common_color_red",             DG_CORE_RESOURCE_COLOR,   &_conf.common_cl[ 1]         },
	{ "common_color_green",           DG_CORE_RESOURCE_COLOR,   &_conf.common_cl[ 2]         },
	{ "common_color_yellow",          DG_CORE_RESOURCE_COLOR,   &_conf.common_cl[ 3]         },
	{ "common_color_blue",            DG_CORE_RESOURCE_COLOR,   &_conf.common_cl[ 4]         },
	{ "common_color_magenta",         DG_CORE_RESOURCE_COLOR,   &_conf.common_cl[ 5]         },
	{ "common_color_cyan",            DG_CORE_RESOURCE_COLOR,   &_conf.common_cl[ 6]         },
	{ "common_color_white",           DG_CORE_RESOURCE_COLOR,   &_conf.common_cl[ 7]         },
	{ "common_color_bright_black",    DG_CORE_RESOURCE_COLOR,   &_conf.common_cl[ 8]         },
	{ "common_color_bright_red",      DG_CORE_RESOURCE_COLOR,   &_conf.common_cl[ 9]         },
	{ "common_color_bright_green",    DG_CORE_RESOURCE_COLOR,   &_conf.common_cl[10]         },
	{ "common_color_bright_yellow",   DG_CORE_RESOURCE_COLOR,   &_conf.common_cl[11]         },
	{ "common_color_bright_blue",     DG_CORE_RESOURCE_COLOR,   &_conf.common_cl[12]         },
	{ "common_color_bright_magenta",  DG_CORE_RESOURCE_COLOR,   &_conf.common_cl[13]         },
	{ "common_color_bright_cyan",     DG_CORE_RESOURCE_COLOR,   &_conf.common_cl[14]         },
	{ "common_color_bright_white",    DG_CORE_RESOURCE_COLOR,   &_conf.common_cl[15]         },

	{ "focus_thickness",              DG_CORE_RESOURCE_UINT16,  &_conf.focus_thick           },
	{ "focus_margin",                 DG_CORE_RESOURCE_UINT16,  &_conf.focus_margin          },
	{ "focus_color_secondary",        DG_CORE_RESOURCE_COLOR,   &_conf.focus_cl_secondary    },
	{ "focus_color_primary",          DG_CORE_RESOURCE_COLOR,   &_conf.focus_cl_primary      },
	{ "focus_color_locked_primary",   DG_CORE_RESOURCE_COLOR,   &_conf.focus_cl_primary_lock },
	
	{ "label_color_background_ratio", DG_CORE_RESOURCE_UDOUBLE, &_conf.label_bg_cl_ratio     },

	_STYLE_RESOURCES( "gap",                    _conf.gap_style          )
	_STYLE_RESOURCES( "label",                  _conf.label_style        )
	_STYLE_RESOURCES( "placeholder",            _conf.placeholder_style  )
	_STYLE_RESOURCES( "spinner",                _conf.spinner_style      )

	_STYLE_RESOURCES( "gauge_default",          _conf.gauge_style[0]     )
	_STYLE_RESOURCES( "gauge_full",             _conf.gauge_style[1]     )
	_STYLE_RESOURCES( "gauge_unknown",          _conf.gauge_style[2]     )

	_STYLE_RESOURCES( "indicator_off",          _conf.indicator_style[0] )
	_STYLE_RESOURCES( "indicator_on",           _conf.indicator_style[1] )
	_STYLE_RESOURCES( "indicator_critical_off", _conf.indicator_style[2] )
	_STYLE_RESOURCES( "indicator_critical_on",  _conf.indicator_style[3] )

	_STYLE_RESOURCES( "button_idle",            _conf.button_style[0]    )
	_STYLE_RESOURCES( "button_focused",         _conf.button_style[1]    )
	_STYLE_RESOURCES( "button_pressed",         _conf.button_style[2]    )
	_STYLE_RESOURCES( "button_disabled",        _conf.button_style[3]    )

	_STYLE_RESOURCES( "switch_idle",            _conf.switch_style[0]    )
	_STYLE_RESOURCES( "switch_focused",         _conf.switch_style[1]    )
	_STYLE_RESOURCES( "switch_pressed",         _conf.switch_style[2]    )	
	_STYLE_RESOURCES( "switch_disabled",        _conf.switch_style[3]    )	
};

static const dg_core_resource_group_t _group = {
	.kind           = DG_CORE_RESOURCE_GROUP_DEFAULT,
	.namespace      = "base",
	.fn_preprocess  = _set_defaults,
	.fn_postprocess = _set_generated,
	.resources      = _res,
	.n              = DG_CORE_RESOURCE_LEN(_res),
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

const dg_base_config_t *
dg_base_config_get(void)
{
	return &_conf;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const dg_core_resource_group_t
dg_base_config_get_group_copy(void)
{
	return _group;
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

bool
dg_base_config_init(void)
{
	if (dg_core_resource_push_group(&_group)) {
		dg_core_resource_load_group(&_group);
		return true;
	} else {
		return false;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_base_config_reset(void)
{
	dg_core_resource_pull_group(&_group);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_scale_style(dg_base_config_style_t *style)
{
	style->thick_bd   *= DG_CORE_CONFIG->scale;
	style->thick_sep  *= DG_CORE_CONFIG->scale;
	style->thick_icon *= DG_CORE_CONFIG->scale;
	style->thick_lim  *= DG_CORE_CONFIG->scale;
	style->margin     *= DG_CORE_CONFIG->scale; 
	style->pad_fg     *= DG_CORE_CONFIG->scale;
	style->pad_icon   *= DG_CORE_CONFIG->scale;
	style->gap_lim    *= DG_CORE_CONFIG->scale;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_set_defaults(void)
{
	/* common values */

	const int16_t thick_bd    = 0;
	const int16_t thick_sep   = 3;
	const int16_t thick_lim   = 3;
	const int16_t thick_icon  = 2;
	const int16_t pad_fg      = 6;
	const int16_t pad_icon    = 15;
	const int16_t gap_lim     = 3;

	const dg_core_color_t cl_bd          = (dg_core_color_t){0.000, 0.000, 0.000, 1.000};
	const dg_core_color_t cl_fg          = (dg_core_color_t){0.041, 0.041, 0.041, 1.000};
	const dg_core_color_t cl_hili        = (dg_core_color_t){0.671, 0.506, 0.141, 1.000};
	const dg_core_color_t cl_warn        = (dg_core_color_t){0.671, 0.141, 0.141, 1.000};

	const dg_core_color_t cl_bg_passive  = (dg_core_color_t){0.082, 0.082, 0.082, 1.000};
	const dg_core_color_t cl_bg_idle     = (dg_core_color_t){0.392, 0.271, 0.000, 1.000};
	const dg_core_color_t cl_bg_focused  = (dg_core_color_t){0.392, 0.271, 0.000, 1.000};
	const dg_core_color_t cl_bg_pressed  = (dg_core_color_t){0.102, 0.071, 0.000, 1.000};
	const dg_core_color_t cl_bg_disabled = (dg_core_color_t){0.220, 0.153, 0.000, 1.000};
	
	const dg_core_color_t cl_ft_warn     = (dg_core_color_t){0.000, 0.000, 0.000, 1.000};	
	const dg_core_color_t cl_ft_passive  = (dg_core_color_t){0.788, 0.737, 0.706, 1.000};
	const dg_core_color_t cl_ft_idle     = (dg_core_color_t){0.788, 0.737, 0.706, 1.000};
	const dg_core_color_t cl_ft_focused  = (dg_core_color_t){0.788, 0.737, 0.706, 1.000};
	const dg_core_color_t cl_ft_pressed  = (dg_core_color_t){0.788, 0.737, 0.706, 1.000};
	const dg_core_color_t cl_ft_disabled = (dg_core_color_t){0.488, 0.453, 0.429, 1.000};

	/* styles templates */

	const dg_base_config_style_t style_passive = {
		.ft_bold      = false,
		.thick_bd     = thick_bd,
		.thick_sep    = thick_sep,
		.thick_lim    = thick_lim,
		.thick_icon   = thick_icon,
		.margin       = 0, 
		.pad_fg       = pad_fg,
		.pad_icon     = pad_icon,
		.gap_lim      = gap_lim,
		.anim_speed   = 1000,
		.cl_bg        = cl_bg_passive,
		.cl_bd        = cl_bd,
		.cl_ft        = cl_ft_passive,
		.cl_sep       = cl_hili,
		.cl_lim       = cl_warn,
		.cl_primary   = cl_ft_passive,
		.cl_secondary = cl_bd,
		.cl_highlight = cl_hili,
	};

	const dg_base_config_style_t style_clickable_idle = {
		.ft_bold      = false,
		.thick_bd     = thick_bd,
		.thick_sep    = thick_sep,
		.thick_lim    = thick_lim,
		.thick_icon   = thick_icon,
		.margin       = 0, 
		.pad_fg       = pad_fg,
		.pad_icon     = pad_icon,
		.gap_lim      = gap_lim,
		.anim_speed   = 1000,
		.cl_bg        = cl_bg_idle,
		.cl_bd        = cl_bd,
		.cl_ft        = cl_ft_idle,
		.cl_sep       = cl_bd,
		.cl_lim       = cl_warn,
		.cl_primary   = cl_ft_idle,
		.cl_secondary = cl_bd,
		.cl_highlight = cl_hili,
	};

	const dg_base_config_style_t style_clickable_focused = {
		.ft_bold      = false,
		.thick_bd     = thick_bd,
		.thick_sep    = thick_sep,
		.thick_lim    = thick_lim,
		.thick_icon   = thick_icon,
		.margin       = 0, 
		.pad_fg       = pad_fg,
		.pad_icon     = pad_icon,
		.gap_lim      = gap_lim,
		.anim_speed   = 1000,
		.cl_bg        = cl_bg_focused,
		.cl_bd        = cl_bd,
		.cl_ft        = cl_ft_focused,
		.cl_sep       = cl_bd,
		.cl_lim       = cl_warn,
		.cl_primary   = cl_ft_focused,
		.cl_secondary = cl_bd,
		.cl_highlight = cl_hili,
	};

	const dg_base_config_style_t style_clickable_pressed = {
		.ft_bold      = false,
		.thick_bd     = thick_bd,
		.thick_sep    = thick_sep,
		.thick_lim    = thick_lim,
		.thick_icon   = thick_icon,
		.margin       = 0, 
		.pad_fg       = pad_fg,
		.pad_icon     = pad_icon,
		.gap_lim      = gap_lim,
		.anim_speed   = 1000,
		.cl_bg        = cl_bg_pressed,
		.cl_bd        = cl_bd,
		.cl_ft        = cl_ft_pressed,
		.cl_sep       = cl_bd,
		.cl_lim       = cl_warn,
		.cl_primary   = cl_ft_pressed,
		.cl_secondary = cl_bd,
		.cl_highlight = cl_hili,
	};

	const dg_base_config_style_t style_clickable_disabled = {
		.ft_bold      = false,
		.thick_bd     = thick_bd,
		.thick_sep    = thick_sep,
		.thick_lim    = thick_lim,
		.thick_icon   = thick_icon,
		.margin       = 0, 
		.pad_fg       = pad_fg,
		.pad_icon     = pad_icon,
		.gap_lim      = gap_lim,
		.anim_speed   = 1000,
		.cl_bg        = cl_bg_disabled,
		.cl_bd        = cl_bd,
		.cl_ft        = cl_ft_disabled,
		.cl_sep       = cl_bd,
		.cl_lim       = cl_warn,
		.cl_primary   = cl_ft_disabled,
		.cl_secondary = cl_bd,
		.cl_highlight = cl_hili,
	};

	/* common components */

	_conf.common_cl[ 0] = (dg_core_color_t){0.149, 0.149, 0.149, 1.000};
	_conf.common_cl[ 1] = (dg_core_color_t){0.761, 0.133, 0.110, 1.000};
	_conf.common_cl[ 2] = (dg_core_color_t){0.565, 0.561, 0.098, 1.000};
	_conf.common_cl[ 3] = (dg_core_color_t){0.800, 0.569, 0.122, 1.000};
	_conf.common_cl[ 4] = (dg_core_color_t){0.259, 0.494, 0.506, 1.000};
	_conf.common_cl[ 5] = (dg_core_color_t){0.659, 0.365, 0.498, 1.000};
	_conf.common_cl[ 6] = (dg_core_color_t){0.388, 0.584, 0.396, 1.000};
	_conf.common_cl[ 7] = (dg_core_color_t){0.627, 0.569, 0.490, 1.000};
	_conf.common_cl[ 8] = (dg_core_color_t){0.545, 0.486, 0.431, 1.000};
	_conf.common_cl[ 9] = (dg_core_color_t){0.933, 0.271, 0.192, 1.000};
	_conf.common_cl[10] = (dg_core_color_t){0.686, 0.698, 0.141, 1.000};
	_conf.common_cl[11] = (dg_core_color_t){0.933, 0.706, 0.169, 1.000};
	_conf.common_cl[12] = (dg_core_color_t){0.486, 0.616, 0.565, 1.000};
	_conf.common_cl[13] = (dg_core_color_t){0.784, 0.498, 0.576, 1.000};
	_conf.common_cl[14] = (dg_core_color_t){0.529, 0.714, 0.463, 1.000};
	_conf.common_cl[15] = (dg_core_color_t){0.875, 0.816, 0.663, 1.000};

	/* focus for interactable cells */

	_conf.focus_thick  = 3;
	_conf.focus_margin = 0;

	_conf.focus_cl_secondary    = cl_ft_passive;
	_conf.focus_cl_primary      = cl_ft_passive;
	_conf.focus_cl_primary_lock = cl_warn;

	/* cell styles */

	_conf.gap_style          = style_passive;
	_conf.label_style        = style_passive;
	_conf.placeholder_style  = style_passive;
	_conf.button_style[0]    = style_clickable_idle;
	_conf.button_style[1]    = style_clickable_focused;
	_conf.button_style[2]    = style_clickable_pressed;
	_conf.button_style[3]    = style_clickable_disabled;
	_conf.gauge_style[0]     = style_passive;
	_conf.gauge_style[1]     = style_passive;
	_conf.gauge_style[2]     = style_passive;
	_conf.indicator_style[0] = style_passive;
	_conf.indicator_style[1] = style_passive;
	_conf.indicator_style[2] = style_passive;
	_conf.indicator_style[3] = style_passive;
	_conf.switch_style[0]    = style_clickable_idle;
	_conf.switch_style[1]    = style_clickable_focused;
	_conf.switch_style[2]    = style_clickable_pressed;
	_conf.switch_style[3]    = style_clickable_disabled;
	_conf.spinner_style      = style_passive;

	/* style modifications */

	_conf.placeholder_style.pad_fg = 0;
	_conf.placeholder_style.cl_sep = dg_core_color_interpolate(cl_bg_passive, DG_CORE_COLOR_BLACK, 0.3);

	_conf.gauge_style[0].cl_primary   = dg_core_color_interpolate(cl_bg_passive, cl_hili, 0.4);
	_conf.gauge_style[1].cl_primary   = dg_core_color_interpolate(cl_bg_passive, cl_hili, 0.6);
	_conf.gauge_style[2].cl_primary   = cl_hili;
	_conf.gauge_style[1].cl_ft        = cl_hili;
	_conf.gauge_style[2].anim_speed   = 200;
	_conf.gauge_style[0].pad_fg       = 0;
	_conf.gauge_style[1].pad_fg       = 0;
	_conf.gauge_style[0].thick_sep    = 9;
	_conf.gauge_style[1].thick_sep    = 9;

	_conf.indicator_style[0].cl_ft        = cl_ft_disabled;
	_conf.indicator_style[1].cl_ft        = cl_ft_warn;
	_conf.indicator_style[2].cl_ft        = cl_ft_disabled;
	_conf.indicator_style[3].cl_ft        = cl_ft_warn;
	_conf.indicator_style[2].cl_bd        = cl_warn;
	_conf.indicator_style[1].cl_lim       = cl_ft_warn;
	_conf.indicator_style[3].cl_lim       = cl_ft_warn;
	_conf.indicator_style[1].cl_bg        = dg_core_color_interpolate(cl_bg_passive, cl_warn, 0.1);
	_conf.indicator_style[3].cl_bg        = dg_core_color_interpolate(cl_bg_passive, cl_warn, 0.1);
	_conf.indicator_style[0].cl_highlight = cl_fg;
	_conf.indicator_style[1].cl_highlight = cl_warn;
	_conf.indicator_style[2].cl_highlight = cl_fg;
	_conf.indicator_style[3].cl_highlight = cl_warn;
	_conf.indicator_style[2].anim_speed   = 500;
	_conf.indicator_style[3].anim_speed   = 500;

	_conf.switch_style[0].cl_primary = cl_bd;
	_conf.switch_style[1].cl_primary = cl_bd;
	_conf.switch_style[2].cl_primary = cl_bd;
	_conf.switch_style[3].cl_primary = cl_bd;
	
	_conf.spinner_style.anim_speed = 4;

	/* styles extras */

	_conf.label_bg_cl_ratio = 0.05;

	/* end */

	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_set_generated(void)
{
	_conf.focus_thick  *= DG_CORE_CONFIG->scale;
	_conf.focus_margin *= DG_CORE_CONFIG->scale;

	_scale_style(&_conf.gap_style);
	_scale_style(&_conf.label_style);
	_scale_style(&_conf.placeholder_style);
	_scale_style(&_conf.button_style[0]);
	_scale_style(&_conf.button_style[1]);
	_scale_style(&_conf.button_style[2]);
	_scale_style(&_conf.button_style[3]);
	_scale_style(&_conf.gauge_style[0]);
	_scale_style(&_conf.gauge_style[1]);
	_scale_style(&_conf.gauge_style[2]);
	_scale_style(&_conf.switch_style[0]);
	_scale_style(&_conf.switch_style[1]);
	_scale_style(&_conf.switch_style[2]);
	_scale_style(&_conf.switch_style[3]);
	_scale_style(&_conf.indicator_style[0]);
	_scale_style(&_conf.indicator_style[1]);
	_scale_style(&_conf.indicator_style[2]);
	_scale_style(&_conf.indicator_style[3]);
	_scale_style(&_conf.spinner_style);
}
