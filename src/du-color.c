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
#include <stdlib.h>
#include <string.h>

#include "du.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _bind_cl (du_color_t *cl);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

du_color_t
du_color_from_str(const char *str, bool *err)
{
	if (!str) {
		goto err;
	}

	du_color_t c = {0};
	char s[3] = "\0\0\0";
	char *e[4];
	size_t l;

	if (str[0] == '#') {
		str++;
	}

	l = strlen(str);
	if (l != 6 && l != 8) {
		goto err;
	}

	/* rgb base parameters */

	strncpy(s, str, 2);
	c.r = (double)strtol(s, e, 16) / 255;

	strncpy(s, str + 2, 2);
	c.g = (double)strtol(s, e + 1, 16) / 255;

	strncpy(s, str + 4, 2);
	c.b = (double)strtol(s, e + 2, 16) / 255;

	if (*e[0] != '\0' || *e[1] != '\0' || *e[2] != '\0') {
		goto err;
	}

	/* optional alpha parameter */

	if (l != 8) {
		c.a = 1.0;
	} else {
		strncpy(s, str + 6, 2);
		c.a = (double)strtol(s, e + 3, 16) / 255;
		if (*e[3] != '\0') {
			goto err;
		}
	}

	/* end */

	if (err) {
		*err = false;
	}

	return c;

	/* error */

err:

	if (err) {
		*err = true;
	}

	return (du_color_t){0};
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

	const uint32_t a = cl.r * 256;
	const uint32_t r = cl.r * 256;
	const uint32_t g = cl.r * 256;
	const uint32_t b = cl.r * 256;

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
