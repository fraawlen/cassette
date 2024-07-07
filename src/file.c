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

#include "attributes.h"
#include "context.h"
#include "main.h"
#include "sequence.h"
#include "token.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool _open_file  (struct context *ctx, const struct context *ctx_parent, const char *filename) NONNULL(1, 3);
static void _parse_file (struct context *ctx);

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
file_parse_child(struct context *ctx_parent, const char *filename)
{
	struct context ctx;
	size_t var_i;
	size_t var_group;
	
	if (ctx_parent->depth >= CONTEXT_MAX_DEPTH || !_open_file(&ctx, ctx_parent, filename))
	{
		return;
	}

	var_i     = ctx_parent->var_i;
	var_group = ctx_parent->var_group;

	ctx.eol_reached    = false;
	ctx.eof_reached    = false;
	ctx.skip_sequences = false;
	ctx.depth          = ctx_parent->depth + 1;
	ctx.it_i           = SIZE_MAX;
	ctx.it_group       = SIZE_MAX;
	ctx.var_i          = SIZE_MAX;
	ctx.var_group      = SIZE_MAX;
	ctx.params         = ctx_parent->params;
	ctx.sequences      = ctx_parent->sequences;
	ctx.vars           = ctx_parent->vars;
	ctx.iteration      = ctx_parent->iteration;
	ctx.keys_params    = ctx_parent->keys_params;
	ctx.keys_sequences = ctx_parent->keys_sequences;
	ctx.keys_vars      = ctx_parent->keys_vars;
	ctx.tokens         = ctx_parent->tokens;
	ctx.parent         = ctx_parent;
	ctx.rand           = ctx_parent->rand;

	_parse_file(&ctx);

	ctx.var_i     = var_i;
	ctx.var_group = var_group;

	fclose(ctx.file);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
file_parse_root(ccfg *cfg, const char *filename)
{
	struct context ctx;
	enum cbook_err book_err = CBOOK_OK;
	enum cdict_err dict_err = CDICT_OK;
	crand r = 0;

	if (!_open_file(&ctx, NULL, filename))
	{
		return;
	}

	crand_seed(&r, 0);

	ctx.eol_reached    = false;
	ctx.eof_reached    = false;
	ctx.skip_sequences = false;
	ctx.depth          = 0;
	ctx.it_i           = SIZE_MAX;
	ctx.it_group       = SIZE_MAX;
	ctx.var_i          = SIZE_MAX;
	ctx.var_group      = SIZE_MAX;
	ctx.params         = cfg->params;
	ctx.sequences      = cfg->sequences;
	ctx.vars           = cbook_create();
	ctx.iteration      = cbook_create();
	ctx.keys_params    = cfg->keys_params;
	ctx.keys_sequences = cfg->keys_sequences;
	ctx.keys_vars      = cdict_create();
	ctx.tokens         = cfg->tokens;
	ctx.parent         = NULL;
	ctx.rand           = &r;

	_parse_file(&ctx);

	book_err |= cbook_error(ctx.iteration);
	book_err |= cbook_error(ctx.vars);
	dict_err |= cdict_error(ctx.keys_vars);
	cfg->err |= (book_err & CBOOK_OVERFLOW) || (dict_err & CDICT_OVERFLOW) ? CCFG_OVERFLOW : CCFG_OK;
	cfg->err |= (book_err & CBOOK_MEMORY)   || (dict_err & CDICT_MEMORY)   ? CCFG_MEMORY   : CCFG_OK;
	cfg->err |= (book_err & CBOOK_INVALID)  || (dict_err & CDICT_INVALID)  ? CCFG_MEMORY   : CCFG_OK;

	cbook_destroy(ctx.vars);
	cdict_destroy(ctx.keys_vars);

	fclose(ctx.file);
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static bool
_open_file(struct context *ctx, const struct context *ctx_parent, const char *filename)
{
	struct stat fs;

	if (!(ctx->file = fopen(filename, "r")) || fstat(fileno(ctx->file), &fs) < 0)
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
_parse_file(struct context *ctx)
{
	while (!ctx->eof_reached)
	{
		ctx->eol_reached = false;
		sequence_parse(ctx);
	}
}
