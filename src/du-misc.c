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

#include <assert.h>
#include <pwd.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "du.h"

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

int16_t
du_misc_convert_fp1616_to_int16(int32_t f)
{
	return (int16_t)(f >> 16);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

char *
du_misc_get_home_path(char *buf, size_t n)
{
	assert(buf);

	strncpy(buf, du_misc_test_env("HOME") ? getenv("HOME") : getpwuid(getuid())->pw_dir, n);

	return buf;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

unsigned long
du_misc_get_time(void)
{
	struct timespec ts = {0};

	clock_gettime(CLOCK_MONOTONIC, &ts);

	return ts.tv_sec * 1000000 + ts.tv_nsec / 1000;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
du_misc_test_env(const char *name)
{
	const char *val = getenv(name);

	return val && val[0] != '\0';
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

char *
du_misc_trim_str(char *str)
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
