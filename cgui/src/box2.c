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

struct cbox_variant
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

	/* config */

	struct cbox_variant *variants;
	size_t n_alloc;
	size_t n;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void apply_clr (ccfg *, const char *, const char *, const char *, struct ccolor *);
static void apply_len (ccfg *, const char *, const char *, const char *, uint32_t *);
static void config    (ccfg *, const char *, struct cbox_variant *);
static bool fetch     (ccfg *, const char *, const char *, const char *);
static bool grow      (cbox2 *);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cbox2_add_variant(cbox2 *bx, struct cbox2_style defaults, const char *name)
{
	GUARD(bx);

	if (!grow(bx))
	{
		return;
	}

	snprintf(bx->variants[bx->n].name, CCFG_TOKEN_LENGTH + 2, "%s.%s", bx->namespace, name);
	bx->variants[bx->n].defaults = defaults;
	bx->variants[bx->n].resolved = defaults;
	bx->n++;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

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
cbox2_clear_warnings(cbox2 *bx)
{
	GUARD(bx);

	cerr_clear_warnings(&bx->err);
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
cbox2_create(const char *name)
{
	cbox2 *bx;

	if (!(bx = malloc(sizeof(cbox2))))
	{
		return nullptr;
	}

	snprintf(bx->namespace, CCFG_TOKEN_LENGTH, "%s", name);

	bx->err      = CERR_NONE;
	bx->shape    = carea_zero;
	bx->variants = nullptr;
	bx->current  = 0;
	bx->n_alloc  = 0;
	bx->n        = 0;

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

	if (bx->n == 0)
	{
		return;
	}

	struct cbox2_style style = bx->variants[bx->current].resolved;
	struct carea shape = bx->shape;

	struct ccolor c1 = style.clr_outline;
	struct ccolor c2 = style.clr_border;
	struct ccolor c3 = style.clr_bg;

	int32_t o = style.outline;
	int32_t b = style.border;

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

enum cerr
cbox2_error(const cbox2 *bx)
{
	return bx ? bx->err : CERR_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cbox2_select(cbox2 *bx, size_t id)
{
	GUARD(bx);

	bx->current = id < bx->n ? id : (bx->n > 0 ? bx->n - 1 : 0);
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
config(ccfg *cfg, const char *name, struct cbox_variant *variant)
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

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
grow(cbox2 *bx)
{
	size_t n;

	if (bx->n < bx->n_alloc)
	{
		return true;
	}

	if (ckd_mul(&n, bx->n_alloc, 3))
	{
		cerr_set(&bx->err, CERR_OVERFLOW);
		return false;
	}

	if (ckd_add(&n, n, 1))
	{
		cerr_set(&bx->err, CERR_OVERFLOW);
		return false;
	}

	if (!CUTIL_REALLOC(bx->variants, bx->n_alloc, n, sizeof(struct cbox_variant), bx->err))
	{
		return false;
	}

	return true;
}
