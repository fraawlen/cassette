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

#ifndef DR_PARSE_PRIVATE_H
#define DR_PARSE_PRIVATE_H

#include <stdbool.h>
#include <stdio.h>
#include <sys/types.h>

#include <derelict/du.h>

#include "token.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
typedef struct dr_parse_context_t dr_parse_context_t;
struct dr_parse_context_t {
	/* file stream data */
	dr_parse_context_t *parent;
	ino_t file_inode;
	char *file_dir;
	FILE *file;
	/* context states */
	bool eol_reached;
	bool eof_reached;
	bool skip_sequences;
	/* variable injection */
	size_t var_group;
	size_t var_token;
	/* iteraton injection */
	size_t iter_token;
	du_book_t *book_iteration;
	/* data storage */
	int64_t *n_namespaces;
	du_book_t *book_sequences;
	du_dictionary_t *dict_tokens;
	du_dictionary_t *dict_sequences;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
bool dr_parse_file(dr_parse_context_t *ctx_parent, const char *filename, du_book_t *book_sequences,
                   du_dictionary_t *dict_sequences, du_dictionary_t *dict_tokens);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DR_PARSE_PRIVATE_H */
