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

static const cgui_config_t config_default =
{
	.init = false,

	/* font */

	.font_face                = NULL,
	.font_size                = 14,
	.font_spacing_horizontal  = 0,
	.font_spacing_vertical    = 2,
	.font_offset_x            = 0,
	.font_offset_y            = 0,
	.font_override_width      = 7,
	.font_override_ascent     = 14,
	.font_override_descent    = 0,
	.font_enable_overrides    = false,
	.font_enable_hint_metrics = true,
	.font_antialias           = CGUI_CONFIG_ANTIALIAS_SUBPIXEL,
	.font_subpixel            = CGUI_CONFIG_SUBPIXEL_RGB,

	/* window */

	.window_style =
	{
		.thickness_border = 2,
		.padding_outer    = 10,
		.padding_inner    = 10,
		.padding_cell     = 10,

		.color_background          = {.r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000},
		.color_background_disabled = {.r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000},
		.color_background_focused  = {.r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000},
		.color_background_locked   = {.r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000},
		.color_border              = {.r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000},
		.color_border_disabled     = {.r = 0.400, .g = 0.400, .b = 0.400, .a = 1.000},
		.color_border_focused      = {.r = 0.671, .g = 0.671, .b = 0.671, .a = 1.000},
		.color_border_locked       = {.r = 0.500, .g = 0.100, .b = 0.100, .a = 1.000},

		.enable_disabled = true,
		.enable_focused  = true,
		.enable_locked   = true,
	},


	/* popup */

	.popup_style =
	{
		.thickness_border = 2,
		.padding_outer    = 10,
		.padding_inner    = 10,
		.padding_cell     = 10,

		.color_background          = {.r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000},
		.color_background_disabled = {.r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000},
		.color_background_focused  = {.r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000},
		.color_background_locked   = {.r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000},
		.color_border              = {.r = 0.000, .g = 0.000, .b = 0.000, .a = 1.000},
		.color_border_disabled     = {.r = 0.400, .g = 0.400, .b = 0.400, .a = 1.000},
		.color_border_focused      = {.r = 0.671, .g = 0.671, .b = 0.671, .a = 1.000},
		.color_border_locked       = {.r = 0.500, .g = 0.100, .b = 0.100, .a = 1.000},

		.enable_disabled = true,
		.enable_focused  = true,
		.enable_locked   = true,
	},
};
