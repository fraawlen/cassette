/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Configuration (CCFG) library.
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

#define _GNU_SOURCE

#include "context.c"
#include "source.c"
#include "main.c"
#include "sequence.c"
#include "substitution.c"
#include "token.c"
#include "util.c"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static ccfg *cfg = CCFG_PLACEHOLDER;
static char *buf = NULL;

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

__AFL_FUZZ_INIT();

int
main(void)
{
	__AFL_INIT();

	buf = __AFL_FUZZ_TESTCASE_BUF;
	while (__AFL_LOOP(10000))
	{	
		cfg = ccfg_create();

		ccfg_load_internal(cfg, buf);
		ccfg_destroy(cfg);
	}
}
