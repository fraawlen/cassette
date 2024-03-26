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

#include <assert.h>
#include <libgen.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <derelict/do.h>
#include <derelict/dr.h>

#include "config.h"
#include "context.h"
#include "rand.h"
#include "sequence.h"
#include "token.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool _open_file  (dr_context_t *ctx, const dr_context_t *ctx_parent, const char *filename);
static void _parse_file (dr_context_t *ctx);

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
dr_file_parse_child(dr_context_t *ctx_parent, const char *filename)
{
	dr_context_t ctx;
	
	assert(ctx_parent);

	if (!_open_file(&ctx, ctx_parent, filename))
	{
		return;
	}

	ctx.eol_reached    = false;
	ctx.eof_reached    = false;
	ctx.skip_sequences = false;
	ctx.var_group      = SIZE_MAX;
	ctx.var_token      = SIZE_MAX;
	ctx.iter_token     = SIZE_MAX;
	ctx.iteration      = do_book_create(10, DR_TOKEN_N);
	ctx.n_namespaces   = ctx_parent->n_namespaces;
	ctx.sequences      = ctx_parent->sequences;
	ctx.variables      = ctx_parent->variables;
	ctx.ref_sequences  = ctx_parent->ref_sequences;
	ctx.ref_variables  = ctx_parent->ref_variables;
	ctx.tokens         = ctx_parent->tokens;
	ctx.parent         = ctx_parent;
	ctx.rand           = ctx_parent->rand;

	_parse_file(&ctx);

	do_book_destroy(&ctx.iteration);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dr_file_parse_root(dr_config_t *cfg, const char *filename)
{
	dr_context_t ctx;
	dr_rand_t r;

	bool fail = false;

	assert(cfg);

	if (!_open_file(&ctx, NULL, filename))
	{
		return false;
	}

	dr_rand_seed(&r, cfg->seed);

	ctx.eol_reached    = false;
	ctx.eof_reached    = false;
	ctx.skip_sequences = false;
	ctx.var_group      = SIZE_MAX;
	ctx.var_token      = SIZE_MAX;
	ctx.iter_token     = SIZE_MAX;
	ctx.iteration      = do_book_create(10, DR_TOKEN_N);
	ctx.n_namespaces   = 0;
	ctx.sequences      = cfg->sequences;
	ctx.variables      = do_book_create(10, DR_TOKEN_N);
	ctx.ref_sequences  = cfg->references;
	ctx.ref_variables  = do_dictionary_create(10, 0.6);
	ctx.tokens         = cfg->tokens;
	ctx.parent         = NULL;
	ctx.rand           = &r;

	_parse_file(&ctx);

	fail |= do_book_has_failed(ctx.iteration);
	fail |= do_book_has_failed(ctx.variables);
	fail |= do_dictionary_has_failed(ctx.ref_variables);

	do_book_destroy(&ctx.iteration);
	do_book_destroy(&ctx.variables);
	do_dictionary_destroy(&ctx.ref_variables);

	return !fail;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static bool
_open_file(dr_context_t *ctx, const dr_context_t *ctx_parent, const char *filename)
{
	struct stat fs;

	if (!filename)
	{
		return false;
	}

	if (!(ctx->file = fopen(filename, "r")))
	{
		return false;
	}

	if (fstat(fileno(ctx->file), &fs) < 0)
	{
		fclose(ctx->file);
		return false;
	}

	while (ctx_parent)
	{
		if (fs.st_ino == ctx_parent->file_inode)
		{
			fclose(ctx->file);
			return false;
		}
		ctx_parent = ctx_parent->parent;
	}

	ctx->file_inode = fs.st_ino;
	ctx->file_dir[PATH_MAX - 1] = '\0';

	strncpy(ctx->file_dir, filename, PATH_MAX - 1);
	dirname(ctx->file_dir);

	return true;	
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_parse_file(dr_context_t *ctx)
{
	while (!ctx->eof_reached)
	{
		ctx->eol_reached = false;
		dr_sequence_parse(ctx);
	}
}
