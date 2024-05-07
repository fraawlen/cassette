/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Objects (DO) library.
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

#ifndef DO_COLOR_H
#define DO_COLOR_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DO_COLOR_TRANSPARENT (do_color_t){0.000, 0.000, 0.000, 0.000}
#define DO_COLOR_WHITE       (do_color_t){1.000, 1.000, 1.000, 1.000}
#define DO_COLOR_BLACK       (do_color_t){0.000, 0.000, 0.000, 1.000}
#define DO_COLOR_RED         (do_color_t){1.000, 0.000, 0.000, 1.000}
#define DO_COLOR_GREEN       (do_color_t){0.000, 1.000, 0.000, 1.000}
#define DO_COLOR_BLUE        (do_color_t){0.000, 0.000, 1.000, 1.000}
#define DO_COLOR_YELLOW      (do_color_t){1.000, 1.000, 0.000, 1.000}
#define DO_COLOR_MAGENTA     (do_color_t){1.000, 0.000, 1.000, 1.000}
#define DO_COLOR_CYAN        (do_color_t){0.000, 1.000, 1.000, 1.000}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct do_color_t
{
	double r;
	double g;
	double b;
	double a;
};

typedef struct do_color_t do_color_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

do_color_t do_color_convert_argb_uint(uint32_t argb);

do_color_t do_color_convert_rgba(uint8_t r, uint8_t g, uint8_t b, uint8_t a);

do_color_t do_color_convert_str(const char *str, bool *err);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

do_color_t do_color_interpolate(do_color_t cl_1, do_color_t cl_2, double ratio);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

uint32_t do_color_get_argb_uint(do_color_t cl);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DO_COLOR_H */

