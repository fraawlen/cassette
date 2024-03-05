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

#include <stdbool.h>
#include <stdint.h>

#include "du.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _bind_cl    (du_color_t *cl);
static int  _hex_to_int (char c);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

du_color_t
du_color_from_argb_uint(uint32_t argb)
{
	du_color_t cl;

	cl.a = ((argb >> 24) & 0xFF) / 255.0;
	cl.r = ((argb >> 16) & 0xFF) / 255.0;
	cl.g = ((argb >>  8) & 0xFF) / 255.0;
	cl.b = ((argb >>  0) & 0xFF) / 255.0;

	return cl;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

du_color_t
du_color_from_str(const char *str, bool *err)
{
	int v[8] = {0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0xF, 0xF};
	bool fail = false;
	size_t i;

	if (!str) {
		fail = true;
		goto skip;
	}

	if (str[0] == '#') {
		str++;
	}

	for (i = 0; i < 8 && str[i] != '\0'; i++) {
		if ((v[i] =_hex_to_int(str[i])) == -1) {
			fail = true;
		}
	}

	if (i != 6 && i != 8) {
		fail = true;
	}

skip:;

	/* apply conversion */

	du_color_t cl;

	cl.r = ((v[0] << 4) + v[1]) / 255.0;
	cl.g = ((v[2] << 4) + v[3]) / 255.0;
	cl.b = ((v[4] << 4) + v[5]) / 255.0;
	cl.a = ((v[6] << 4) + v[7]) / 255.0;

	if (err) {
		*err = fail;
	}

	return cl;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

du_color_t
du_color_interpolate(du_color_t cl_1, du_color_t cl_2, double ratio)
{
	_bind_cl(&cl_1);
	_bind_cl(&cl_2);
	du_ratio_bind(&ratio);

	const du_color_t cl = {
		.r = cl_2.r * ratio + cl_1.r * (1.0 - ratio),
		.g = cl_2.g * ratio + cl_1.g * (1.0 - ratio),
		.b = cl_2.b * ratio + cl_1.b * (1.0 - ratio),
		.a = cl_2.a * ratio + cl_1.a * (1.0 - ratio),
	};

	return cl;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

uint32_t
du_color_to_argb_uint(du_color_t cl)
{
	_bind_cl(&cl);

	const uint32_t a = cl.a * 256;
	const uint32_t r = cl.r * 256;
	const uint32_t g = cl.g * 256;
	const uint32_t b = cl.b * 256;

	return (a << 24) + (r << 16) + (g << 8) + b;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_bind_cl(du_color_t *cl)
{
	du_ratio_bind(&cl->r);
	du_ratio_bind(&cl->g);
	du_ratio_bind(&cl->b);
	du_ratio_bind(&cl->a);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static int
_hex_to_int(char c)
{
	switch (c) {
		
		case '0':
			return 0x0;

		case '1':
			return 0x1;

		case '2':
			return 0x2;

		case '3':
			return 0x3;

		case '4':
			return 0x4;

		case '5':
			return 0x5;

		case '6':
			return 0x6;

		case '7':
			return 0x7;

		case '8':
			return 0x8;

		case '9':
			return 0x9;
		
		case 'a':
		case 'A':
			return 0xA;

		case 'b':
		case 'B':
			return 0xB;

		case 'c':
		case 'C':
			return 0xC;

		case 'd':
		case 'D':
			return 0xD;

		case 'e':
		case 'E':
			return 0xE;

		case 'f':
		case 'F':
			return 0xF;

		default:
			return -1;
	}
}
