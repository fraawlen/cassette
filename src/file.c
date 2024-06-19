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

#include <cassette/ccfg.h>
#include <cassette/cobj.h>
#include <libgen.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "context.h"
#include "main.h"
#include "sequence.h"
#include "token.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool _open_file  (context_t *ctx, const context_t *ctx_parent, const char *filename);
static void _parse_file (context_t *ctx);

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
file_parse_child(context_t *ctx_parent, const char *filename)
{
	context_t ctx;

	size_t var_iter;
	size_t var_group;
	
	if (ctx_parent->depth >= CONTEXT_MAX_DEPTH)
	{
		return;
	}

	if (!_open_file(&ctx, ctx_parent, filename))
	{
		return;
	}

	ctx.eol_reached    = false;
	ctx.eof_reached    = false;
	ctx.skip_sequences = false;
	ctx.depth          = ctx_parent->depth + 1;
	ctx.params         = ctx_parent->params;
	ctx.sequences      = ctx_parent->sequences;
	ctx.variables      = ctx_parent->variables;
	ctx.iteration      = cobj_book_get_placeholder();
	ctx.ref_params     = ctx_parent->ref_params;
	ctx.ref_sequences  = ctx_parent->ref_sequences;
	ctx.ref_variables  = ctx_parent->ref_variables;
	ctx.tokens         = ctx_parent->tokens;
	ctx.parent         = ctx_parent;
	ctx.rand           = ctx_parent->rand;

	var_iter  = cobj_book_get_iterator_offset(ctx.variables);
	var_group = cobj_book_get_iterator_group(ctx.variables);

	cobj_book_lock_iterator(ctx.variables);

	_parse_file(&ctx);

	if (var_iter > 0)
	{
		cobj_book_reset_iterator(ctx.variables, var_group);
		for (size_t i = 0; i < var_iter; i++)
		{
			cobj_book_increment_iterator(ctx.variables);
		}
	}

	fclose(ctx.file);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
file_parse_root(ccfg_t *cfg, const char *filename)
{
	context_t ctx;
	cobj_rand_t r;

	bool fail = false;

	if (!_open_file(&ctx, NULL, filename))
	{
		return false;
	}

	cobj_rand_seed(&r, cfg->seed);

	ctx.eol_reached    = false;
	ctx.eof_reached    = false;
	ctx.skip_sequences = false;
	ctx.depth          = 0;
	ctx.params         = cfg->params;
	ctx.sequences      = cfg->sequences;
	ctx.variables      = cobj_book_create(10, CCFG_MAX_WORD_BYTES);
	ctx.iteration      = cobj_book_get_placeholder();
	ctx.ref_params     = cfg->ref_params;
	ctx.ref_sequences  = cfg->ref_sequences;
	ctx.ref_variables  = cobj_dictionary_create(10, 0.6);
	ctx.tokens         = cfg->tokens;
	ctx.parent         = NULL;
	ctx.rand           = &r;

	_parse_file(&ctx);

	fail |= cobj_book_has_failed(ctx.variables);
	fail |= cobj_dictionary_has_failed(ctx.ref_variables);

	cobj_book_destroy(&ctx.variables);
	cobj_dictionary_destroy(&ctx.ref_variables);

	fclose(ctx.file);

	return !fail;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static bool
_open_file(context_t *ctx, const context_t *ctx_parent, const char *filename)
{
	struct stat fs;

	if (!filename || filename[0] == '\0')
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

	snprintf(ctx->file_dir, PATH_MAX, "%s", filename);
	dirname(ctx->file_dir);

	return true;	
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_parse_file(context_t *ctx)
{
	while (!ctx->eof_reached)
	{
		ctx->eol_reached = false;
		sequence_parse(ctx);
	}
}
