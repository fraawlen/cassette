/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Objects (COBJ) library.
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

#ifndef COBJ_COLOR_H
#define COBJ_COLOR_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define COBJ_COLOR_TRANSPARENT (cobj_color_t){0.000, 0.000, 0.000, 0.000}
#define COBJ_COLOR_WHITE       (cobj_color_t){1.000, 1.000, 1.000, 1.000}
#define COBJ_COLOR_BLACK       (cobj_color_t){0.000, 0.000, 0.000, 1.000}
#define COBJ_COLOR_RED         (cobj_color_t){1.000, 0.000, 0.000, 1.000}
#define COBJ_COLOR_GREEN       (cobj_color_t){0.000, 1.000, 0.000, 1.000}
#define COBJ_COLOR_BLUE        (cobj_color_t){0.000, 0.000, 1.000, 1.000}
#define COBJ_COLOR_YELLOW      (cobj_color_t){1.000, 1.000, 0.000, 1.000}
#define COBJ_COLOR_MAGENTA     (cobj_color_t){1.000, 0.000, 1.000, 1.000}
#define COBJ_COLOR_CYAN        (cobj_color_t){0.000, 1.000, 1.000, 1.000}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cobj_color_t
{
	double r;
	double g;
	double b;
	double a;
};

typedef struct cobj_color_t cobj_color_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cobj_color_t cobj_color_convert_argb_uint(uint32_t argb);

cobj_color_t cobj_color_convert_rgba(uint8_t r, uint8_t g, uint8_t b, uint8_t a);

cobj_color_t cobj_color_convert_str(const char *str, bool *err);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cobj_color_t cobj_color_interpolate(cobj_color_t cl_1, cobj_color_t cl_2, double ratio);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

uint32_t cobj_color_get_argb_uint(cobj_color_t cl);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* COBJ_COLOR_H */

