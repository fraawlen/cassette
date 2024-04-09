/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Resources (DR) library.
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

#ifndef CONTEXT_H
#define CONTEXT_H

#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>

#include <derelict/do.h>

#include "token.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DR_CONTEXT_DICT_VARIABLE 0
#define DR_CONTEXT_DICT_SECTION  1

#define DR_CONTEXT_MAX_DEPTH 128

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

typedef struct dr_context_t dr_context_t;

struct dr_context_t
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

	/* data storage */

	do_book_t *params;
	do_book_t *sequences;
	do_book_t *variables;
	do_book_t *iteration;
	do_dictionary_t *ref_params;
	do_dictionary_t *ref_sequences;
	do_dictionary_t *ref_variables;
	do_dictionary_t *tokens;

	/* misc */
	
	dr_context_t *parent;
	do_rand_t *rand;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

dr_token_kind_t dr_context_get_token(dr_context_t *ctx, char token[static DR_TOKEN_N], double *math_result);

dr_token_kind_t dr_context_get_token_numeral(dr_context_t *ctx, char token[static DR_TOKEN_N], double *math_result);

dr_token_kind_t dr_context_get_token_raw(dr_context_t *ctx, char token[static DR_TOKEN_N]);

void dr_context_goto_eol(dr_context_t *ctx);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#endif /* CONTEXT_H */

