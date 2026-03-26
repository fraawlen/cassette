/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cairo/cairo.h>
#include <cassette/ccfg.h>
#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "box.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define GUARD(CB, ...) if (!CB) { return __VA_OPT__(__VA_ARGS__); }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cbox
{
	/* state */

	bool strip;

	/* geometry */

	uint32_t x;
	uint32_t y;
	uint32_t w;
	uint32_t h;

	/* config */

	uint32_t border;
	uint32_t outline;

	struct ccolor clr_border;
	struct ccolor clr_outline;
	struct ccolor clr_bg;

	/* defaults */

	uint32_t df_border;
	uint32_t df_outline;

	struct ccolor df_clr_border;
	struct ccolor df_clr_outline;
	struct ccolor df_clr_bg;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static struct ccolor fetch_clr (ccfg  *, const char *, const char  *, struct ccolor);
static uint32_t      fetch_len (ccfg  *, const char *, const char  *, uint32_t);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

cbox *
cbox_create(void)
{
	cbox *bx;

	if (!(bx = malloc(sizeof(cbox))))
	{
		return nullptr;
	}

	bx->clr_border  = ccolor_black;
	bx->clr_outline = ccolor_black;
	bx->clr_bg      = ccolor_black;
	bx->border      = 0;
	bx->outline     = 0;

	bx->df_clr_border  = ccolor_black;
	bx->df_clr_outline = ccolor_black;
	bx->df_clr_bg      = ccolor_black;
	bx->df_border      = 0;
	bx->df_outline     = 0;

	bx->strip = false;
	bx->x     = 0;
	bx->y     = 0;
	bx->w     = 0;
	bx->h     = 0;

	return bx;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbox_config(cbox *bx, ccfg *cfg, const char *group)
{
	GUARD(bx);

	bx->border      = fetch_len(cfg, group, "border",        bx->df_border);
	bx->outline     = fetch_len(cfg, group, "outline",       bx->df_outline);
	bx->clr_border  = fetch_clr(cfg, group, "border_color",  bx->df_clr_border);
	bx->clr_outline = fetch_clr(cfg, group, "outline_color", bx->df_clr_outline);
	bx->clr_bg      = fetch_clr(cfg, group, "background",    bx->df_clr_bg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbox_default_background(cbox *bx, struct ccolor clr)
{
	GUARD(bx);

	bx->df_clr_bg = clr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbox_default_border(cbox *bx, struct ccolor clr, uint32_t size)
{
	GUARD(bx);

	bx->df_clr_border = clr;
	bx->df_border     = size;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbox_default_outine(cbox *bx, struct ccolor clr, uint32_t size)
{
	GUARD(bx);

	bx->df_clr_outline = clr;
	bx->df_outline     = size;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

nullptr_t
cbox_destroy(cbox *bx)
{
	free(bx);

	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbox_redraw(cbox *bx, cairo_t *ctx)
{
	GUARD(bx);

	struct ccolor c1 = bx->clr_outline;
	struct ccolor c2 = bx->clr_border;
	struct ccolor c3 = bx->clr_bg;

	uint32_t o = bx->outline;
	uint32_t b = bx->border;

	if (o > 0 && !bx->strip)
	{
		cairo_set_source_rgba(ctx, c1.r, c1.g, c1.b, c1.a);
		cairo_rectangle(ctx, bx->x - o, bx->y - o, bx->w + 2 * o, bx->h + 2 * o);
		cairo_fill(ctx);
	}

	if (b > 0)
	{
		cairo_set_source_rgba(ctx, c2.r, c1.g, c1.b, c1.a);
		cairo_rectangle(ctx, bx->x, bx->y, bx->w, bx->h);
		cairo_fill(ctx);
	}

	cairo_set_source_rgba(ctx, c3.r, c3.g, c3.b, c3.a);
	cairo_rectangle(ctx, bx->x + b, bx->y + b, bx->w - 2 * b, bx->h - 2 * b);
	cairo_fill(ctx);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbox_transform(cbox *bx, uint32_t x, uint32_t y, uint32_t h, uint32_t w)
{
	GUARD(bx);

	bx->x = x;
	bx->y = y;
	bx->w = w;
	bx->h = h;
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
box_strip(cbox *bx)
{
	bx->strip = true;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static struct ccolor
fetch_clr(ccfg *cfg, const char *group, const char *name, struct ccolor backup)
{
	ccfg_fetch(cfg, group, name);

	return ccfg_iterate(cfg) 
	     ? ccolor_from_str(ccfg_resource(cfg), nullptr)
	     : backup;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static uint32_t
fetch_len(ccfg *cfg, const char *group, const char *name, uint32_t backup)
{
	ccfg_fetch(cfg, group, name);

	return ccfg_iterate(cfg) 
	     ? cutil_str_to_long(ccfg_resource(cfg), 0, UINT32_MAX)
	     : backup;
}
