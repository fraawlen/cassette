/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cairo/cairo.h>
#include <cassette/ccfg.h>
#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <stdbool.h>
#include <stdckdint.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define GUARD(BX, ...) if (!BX || cerr_critical(BX->err)) { return __VA_OPT__(__VA_ARGS__); }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cbox2_variant
{
	char name[CCFG_TOKEN_LENGTH + 2];

	struct cbox2_style defaults;
	struct cbox2_style resolved;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cbox2
{
	/* properties */

	char namespace[CCFG_TOKEN_LENGTH];

	/* state */
	
	enum   cerr err;
	struct carea shape;
	size_t current;

	/* configs */

	struct cbox2_variant *variants;
	size_t n;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void apply_clr (ccfg *, const char *, const char *, const char *, struct ccolor *);
static void apply_len (ccfg *, const char *, const char *, const char *, uint32_t *);
static void config    (ccfg *, const char *, struct cbox2_variant *);
static bool fetch     (ccfg *, const char *, const char *, const char *);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

struct carea
cbox2_area(const cbox2 *bx)
{
	GUARD(bx, carea_zero);

	struct carea area = bx->shape;
	uint32_t l;

	if (bx->n == 0)
	{
		return area;
	}

	l  = bx->variants[bx->current].resolved.border;
	l += bx->variants[bx->current].resolved.pad;

	area.x = bx->shape.x + l;
	area.y = bx->shape.y + l;
	area.w = bx->shape.w - l * 2;
	area.h = bx->shape.h - l * 2;

	return area;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbox2_config(cbox2 *bx, ccfg *cfg)
{
	GUARD(bx);

	for (size_t i = 0; i < bx->n; i++)
	{
		config(cfg, bx->namespace, bx->variants + i);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cbox2 *
cbox2_create(const char *name, size_t variants)
{
	cbox2 *bx;

	if (variants == 0 || !(bx = malloc(sizeof(cbox2))))
	{
		return nullptr;
	}

	if (!(bx->variants = calloc(variants, sizeof(struct cbox2_variant))))
	{
		free(bx);
		return nullptr;
	}

	snprintf(bx->namespace, CCFG_TOKEN_LENGTH, "%s", name);

	bx->err     = CERR_NONE;
	bx->shape   = carea_zero;
	bx->n       = variants;
	bx->current = 0;

	return bx;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

nullptr_t
cbox2_destroy(cbox2 *bx)
{
	if (bx)
	{
		free(bx->variants);
		free(bx);
	}

	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbox2_draw(cbox2 *bx, cairo_t *ctx)
{
	GUARD(bx);

	struct cbox2_style style = bx->variants[bx->current].resolved;
	struct carea shape = bx->shape;

	struct ccolor c1 = style.clr_outline;
	struct ccolor c2 = style.clr_border;
	struct ccolor c3 = style.clr_bg;

	int32_t o = style.outline;
	int32_t b = style.border;
	
	cairo_set_operator(ctx, CAIRO_OPERATOR_SOURCE);
	if (o > 0 && !style.striped)
	{
		cairo_set_source_rgba(ctx, c1.r, c1.g, c1.b, c1.a);
		cairo_rectangle(ctx, shape.x - o, shape.y - o, shape.w + 2 * o, shape.h + 2 * o);
		cairo_fill(ctx);
	}

	if (b > 0)
	{
		cairo_set_source_rgba(ctx, c2.r, c2.g, c2.b, c2.a);
		cairo_rectangle(ctx, shape.x, shape.y, shape.w, shape.h);
		cairo_fill(ctx);
	}

	cairo_set_source_rgba(ctx, c3.r, c3.g, c3.b, c3.a);
	cairo_rectangle(ctx, shape.x + b, shape.y + b, shape.w - 2 * b, shape.h - 2 * b);
	cairo_fill(ctx);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbox2_select(cbox2 *bx, size_t variant)
{
	GUARD(bx);

	bx->current = variant < bx->n ? variant : bx->n - 1;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbox2_setup(cbox2 *bx, size_t variant, struct cbox2_style defaults, const char *name)
{
	GUARD(bx);

	size_t i = variant < bx->n ? variant : bx->n - 1;

	if (name)
	{
		snprintf(bx->variants[i].name, CCFG_TOKEN_LENGTH + 2, "%s.%s", bx->namespace, name);
	}
	else
	{
		bx->variants[i].name[0] = '\0';
	}

	bx->variants[i].defaults = defaults;
	bx->variants[i].resolved = defaults;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbox2_shape(cbox2 *bx, struct carea shape)
{
	GUARD(bx);

	bx->shape = shape;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
apply_clr(ccfg *cfg, const char *name, const char *name_2, const char *prop, struct ccolor *dst)
{
	if (fetch(cfg, name, name_2, prop))
	{
		*dst = ccolor_from_str(ccfg_resource(cfg), nullptr);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
apply_len(ccfg *cfg, const char *name, const char *name_2, const char *prop, uint32_t *dst)
{
	if (fetch(cfg, name, name_2, prop))
	{
		*dst = cutil_str_to_long(ccfg_resource(cfg), 0, UINT32_MAX);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
config(ccfg *cfg, const char *name, struct cbox2_variant *variant)
{
	struct cbox2_style *defaults = &variant->defaults;
	struct cbox2_style *resolved = &variant->resolved;

	*resolved = *defaults;

	apply_len(cfg, variant->name, name, "pad",              &resolved->pad);
	apply_len(cfg, variant->name, name, "border",           &resolved->border);
	apply_len(cfg, variant->name, name, "outline",          &resolved->outline);
	
	apply_clr(cfg, variant->name, name, "background_color", &resolved->clr_bg);
	apply_clr(cfg, variant->name, name, "border_color",     &resolved->clr_border);
	apply_clr(cfg, variant->name, name, "outline_color",    &resolved->clr_outline);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
fetch(ccfg *cfg, const char *name, const char *name_2, const char *prop)
{
	ccfg_fetch(cfg, name, prop);
	if (!ccfg_iterate(cfg))
	{
		ccfg_fetch(cfg, name_2, prop);
		if (!ccfg_iterate(cfg))
		{
			return false;
		}
	}

	return true;
}
