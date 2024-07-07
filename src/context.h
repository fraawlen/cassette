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

#pragma once

#include <cassette/ccfg.h>
#include <cassette/cobj.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>

#include "attributes.h"
#include "token.h"

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

struct context
{
	/* file data */

	ino_t file_inode;
	char  file_dir[PATH_MAX];
	FILE *file;

	/* context states */

	size_t depth;
	bool eol_reached;
	bool eof_reached;
	bool skip_sequences;

	/* iteration injection */

	size_t it_i;
	size_t it_group;

	/* variable injection */

	size_t var_i;
	size_t var_group;

	/* data storage */

	cbook *params;
	cbook *sequences;
	cbook *vars;
	cbook *iteration;
	cdict *keys_params;
	cdict *keys_sequences;
	cdict *keys_vars;
	cdict *tokens;

	/* misc */
	
	struct context *parent;
	crand *rand;
};

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

#define CONTEXT_DICT_VARIABLE  0
#define CONTEXT_DICT_SECTION   1
#define CONTEXT_DICT_ITERATION 2
#define CONTEXT_MAX_DEPTH      32

/************************************************************************************************************/
/* PROCEDURES ***********************************************************************************************/
/************************************************************************************************************/

enum token
context_get_token(struct context *ctx, char token[static TOKEN_MAX_LEN], double *math_result)
NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum token
context_get_token_numeral(struct context *ctx, char token[static TOKEN_MAX_LEN], double *math_result)
NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum token
context_get_token_raw(struct context *ctx, char token[static TOKEN_MAX_LEN])
NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
context_goto_eol(struct context *ctx)
NONNULL(1);
