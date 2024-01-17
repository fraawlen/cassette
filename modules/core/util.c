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
#include <time.h>
#include <unistd.h>

#include "util.h"

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

int16_t
dg_core_util_convert_fp1616_to_int16(int32_t f)
{
	return (int16_t)(f >> 16);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_core_util_dict_match(const dg_core_util_dict_t *dict, size_t dict_n, const char *key, int *target)
{
	assert(dict);

	for (int i = 0; i < dict_n; i++) {
		if (!strcmp(key, dict[i].key)) {
			if (target) {
				*target = dict[i].value;
			}
			return true;
		}
	}

	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
dg_core_util_dict_inverse_match(const dg_core_util_dict_t *dict, size_t dict_n, int value)
{
	assert(dict);

	for (int i = 0; i < dict_n; i++) {
		if (value == dict[i].value) {
			return dict[i].key;
		}
	}

	return NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

unsigned long
dg_core_util_get_time(void)
{
	struct timespec ts = {0};

	clock_gettime(CLOCK_MONOTONIC, &ts);

	return ts.tv_sec * 1000000 + ts.tv_nsec / 1000;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_core_util_test_bounds(int16_t x_test,  int16_t y_test, int16_t x_bound, int16_t y_bound, int16_t w_bound,
	int16_t h_bound)
{
	if (w_bound < 0) {
		w_bound *= -1;
		x_bound -= w_bound;
	}

	if (h_bound < 0) {
		h_bound *= -1;
		y_bound -= h_bound;
	}

	return x_test >= x_bound && x_test < x_bound + w_bound &&
	       y_test >= y_bound && y_test < y_bound + h_bound;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_core_util_test_env(const char *name)
{
	const char *val = getenv(name);

	return val && val[0] != '\0';
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

char *
dg_core_util_trim_str(char *str)
{
	if (!str) {
		return NULL;
	}

	for (size_t i = strlen(str) - 1; i > 0 && (str[i] == ' ' || str[i] == '\t' || str[i] == '\n'); i--) {
		str[i] = '\0';
	}

	while (str[0] != '\0' && (str[0] == ' ' || str[0] == '\t' || str[0] == '\n')) {
		str++;
	}

	return str[0] != '\0' ? str : NULL;
}
