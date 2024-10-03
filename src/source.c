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
#include <fcntl.h>
#include <libgen.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "context.h"
#include "main.h"
#include "sequence.h"
#include "source.h"
#include "token.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool  has_err    (struct context *)                                             CCFG_NONNULL(1);
static char *map_source (struct context *, const struct context *, const char *, bool) CCFG_NONNULL(1, 3);
static void  parse      (struct context *)                                             CCFG_NONNULL(1);

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
source_parse_child(struct context *ctx_parent, const char *source)
{
	struct context ctx;
	size_t var_i;
	size_t var_group;
	char  *buffer;
	
	if (ctx_parent->depth >= CONTEXT_MAX_DEPTH
	 || !(buffer = map_source(&ctx, ctx_parent, source, false)))
	{
		return;
	}

	var_i     = ctx_parent->var_i;
	var_group = ctx_parent->var_group;

	ctx.buffer         = buffer;
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
	ctx.restricted     = ctx_parent->restricted;
	ctx.parent         = ctx_parent;
	ctx.rand           = ctx_parent->rand;

	parse(&ctx);

	ctx.var_i     = var_i;
	ctx.var_group = var_group;

	munmap(buffer, ctx.file_size);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
source_parse_root(ccfg *cfg, const char *source, bool internal)
{
	struct context ctx;
	char *buffer;

	if (!(buffer = map_source(&ctx, NULL, source, internal)))
	{
		return;
	}

	ctx.buffer         = buffer;
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
	ctx.restricted     = cfg->restricted || getenv("CCFG_RESTRICT");
	ctx.parent         = NULL;
	ctx.rand           = crand_seed(0);

	parse(&ctx);

	if (has_err(&ctx))
	{	
		cfg->err = CERR_MEMORY;
	}

	if (internal)
	{
		free(buffer);
	}
	else
	{
		munmap(buffer, ctx.file_size);
	}

	cbook_destroy(ctx.iteration);
	cbook_destroy(ctx.vars);
	cdict_destroy(ctx.keys_vars);
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static bool
has_err(struct context *ctx)
{
	return cbook_error(ctx->iteration) || cbook_error(ctx->vars) || cdict_error(ctx->keys_vars);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static char *
map_source(struct context *ctx, const struct context *ctx_parent, const char *source, bool internal)
{
	struct stat fs;
	char *map;
	int fd;

	/* internal buffer setup, source is directly used as buffer */

	if (internal)
	{
		ctx->file_inode  = 0;
		ctx->file_size   = 0;
		ctx->file_dir[0] = '\0';
		return strdup(source);
	}

	/* map file for external sources, source is used as filename */

	if ((fd = open(source, O_RDONLY)) == -1)
	{
		goto fail_open;
	}

	if ((fstat(fd, &fs)) == -1)
	{
		goto fail_stat;
	}

	while (ctx_parent)
	{
		if (fs.st_ino == ctx_parent->file_inode)
		{
			goto fail_loop;
		}
		ctx_parent = ctx_parent->parent;
	}

	if ((map = mmap(0, fs.st_size, PROT_READ, MAP_PRIVATE, fd, 0)) == MAP_FAILED)
	{
		goto fail_map;
	}

	ctx->file_size  = fs.st_size;
	ctx->file_inode = fs.st_ino;
	snprintf(ctx->file_dir, PATH_MAX, "%s", source);
	dirname(ctx->file_dir);
	close(fd);

	return map;

	/* errors */

fail_map:
fail_loop:
fail_stat:
	close(fd);
fail_open:
	return NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
parse(struct context *ctx)
{
	while (!ctx->eof_reached)
	{
		ctx->eol_reached = false;
		sequence_parse(ctx);
	}
}
