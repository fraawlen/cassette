/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Utilities (DU) library.
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

#ifndef DU_COLOR_H
#define DU_COLOR_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DU_COLOR_TRANSPARENT (du_color_t){0.000, 0.000, 0.000, 0.000}
#define DU_COLOR_WHITE       (du_color_t){1.000, 1.000, 1.000, 1.000}
#define DU_COLOR_BLACK       (du_color_t){0.000, 0.000, 0.000, 1.000}
#define DU_COLOR_RED         (du_color_t){1.000, 0.000, 0.000, 1.000}
#define DU_COLOR_GREEN       (du_color_t){0.000, 1.000, 0.000, 1.000}
#define DU_COLOR_BLUE        (du_color_t){0.000, 0.000, 1.000, 1.000}
#define DU_COLOR_YELLOW      (du_color_t){1.000, 1.000, 0.000, 1.000}
#define DU_COLOR_MAGENTA     (du_color_t){1.000, 0.000, 1.000, 1.000}
#define DU_COLOR_CYAN        (du_color_t){0.000, 1.000, 1.000, 1.000}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct du_color_t
{
	double r;
	double g;
	double b;
	double a;
};

typedef struct du_color_t du_color_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

du_color_t du_color_convert_argb_uint(uint32_t argb);

du_color_t du_color_convert_rgba(uint8_t r, uint8_t g, uint8_t b, uint8_t a);

du_color_t du_color_convert_hex_str(const char *str, bool *err);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

du_color_t du_color_interpolate(du_color_t cl_1, du_color_t cl_2, double ratio);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

uint32_t du_color_get_argb_uint(du_color_t cl);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DU_COLOR_H */

