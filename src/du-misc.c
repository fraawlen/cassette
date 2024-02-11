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
#include <stdio.h>
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
	assert(buf && n > 0);

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

char *
du_misc_read_word(char *buf, size_t n, FILE *f, bool *eol)
{
	assert(buf && n > 0);

	bool quotes  = false;
	bool quotes2 = false;
	size_t i = 0;
	int c;

	if (eol) {
		*eol = false;
	}

	/* check if any char can be read */

	if ((c = fgetc(f)) == EOF) {
		buf[0] = '\0';
		return NULL;
	}

	/* ignore leading whitespace */

	for (;;) {
		switch (c) {
			
			case ' ':
			case '\t':
				c = fgetc(f);
				break;

			default:
				goto exit_lead;
		}
	}

exit_lead:

	/* read chars */

	for (;;) {
		switch (c) {

			case '\'':
				if (!quotes2) {
					quotes = !quotes;
					break;
				}
				goto word_add;

			case '\"':
				if (!quotes) {
					quotes2 = !quotes2;
					break;
				}
				goto word_add;

			case '\n':
				if (eol) {
					*eol = true;
				}
				/* fallthrough */

			case ' ':
			case '\t':
				if (quotes || quotes2) {
					goto word_add;
				}
				goto word_end;

			default:
			word_add:
				if (i < n - 1) {
					buf[i] = (char)c;
					i++;
					break;
				}
				/* fallthrough */

			case EOF:
			word_end:
				buf[i] = '\0';
				return buf;
		}

		c = fgetc(f);
	}
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

	/* trail */

	for (size_t i = strlen(str); i > 0; i--) {
		switch(str[i - 1]) {
			
			case ' ':
			case '\t':
			case '\n':
				break;

			default:
				str[i] = '\0';
				goto exit_trail;
		}
	}

exit_trail:

	/* lead */

	for (;;) {
		switch(str[0]) {

			case ' ':
			case '\t':
			case '\n':
				str++;
				break;

			case '\0':
				return NULL;
		
			default:
				return str;
		}
	}
}
