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

#ifndef CONTEXT_H
#define CONTEXT_H

#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>

#include <cassette/ccfg.h>
#include <cassette/cobj.h>

#include "token.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define CONTEXT_DICT_VARIABLE 0
#define CONTEXT_DICT_SECTION  1
#define CONTEXT_MAX_DEPTH     128

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

typedef struct context_t context_t;

struct context_t
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

	cobj_book_t *params;
	cobj_book_t *sequences;
	cobj_book_t *variables;
	cobj_book_t *iteration;
	cobj_dictionary_t *ref_params;
	cobj_dictionary_t *ref_sequences;
	cobj_dictionary_t *ref_variables;
	cobj_dictionary_t *tokens;

	/* misc */
	
	context_t *parent;
	cobj_rand_t *rand;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

token_kind_t context_get_token(context_t *ctx, char token[static CCFG_MAX_WORD_BYTES], double *math_result);

token_kind_t context_get_token_numeral(context_t *ctx, char token[static CCFG_MAX_WORD_BYTES], double *math_result);

token_kind_t context_get_token_raw(context_t *ctx, char token[static CCFG_MAX_WORD_BYTES]);

void context_goto_eol(context_t *ctx);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#endif /* CONTEXT_H */

