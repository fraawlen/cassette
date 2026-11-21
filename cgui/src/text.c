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

#define GUARD(TX, ...) if (!TX) { return __VA_OPT__(__VA_ARGS__); }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct ctext_variant
{
	char name[CCFG_TOKEN_LENGTH + 2];

	struct ctext_style defaults;
	struct ctext_style resolved;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct ctext
{
	/* properties */

	char namespace[CCFG_TOKEN_LENGTH];

	/* state */
	
	struct carea shape;
	size_t current;

	/* configs */

	struct ctext_variant *variants;
	size_t n;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void apply_clr (ccfg *, const char *, const char *, const char *, struct ccolor *);
static void config    (ccfg *, const char *, struct ctext_variant *);
static bool fetch     (ccfg *, const char *, const char *, const char *);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
ctext_config(ctext *tx, ccfg *cfg)
{
	GUARD(tx);

	for (size_t i = 0; i < tx->n; i++)
	{
		config(cfg, tx->namespace, tx->variants + i);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

ctext *
ctext_create(const char *name, size_t variants)
{
	ctext *tx;

	if (variants == 0 || !(tx = malloc(sizeof(ctext))))
	{
		return nullptr;
	}

	if (!(tx->variants = calloc(variants, sizeof(struct ctext_variant))))
	{
		free(tx);
		return nullptr;
	}

	snprintf(tx->namespace, CCFG_TOKEN_LENGTH, "%s", name);

	tx->shape   = carea_zero;
	tx->n       = variants;
	tx->current = 0;

	return tx;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

nullptr_t
ctext_destroy(ctext *tx)
{
	if (tx)
	{
		free(tx->variants);
		free(tx);
	}

	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ctext_draw(ctext *tx, cairo_t *ctx)
{
	GUARD(tx);

	struct ctext_style style = tx->variants[tx->current].resolved;
	struct carea shape = tx->shape;
	struct ccolor c = style.clr_fg;

	cairo_set_font_size(ctx, 14);
	cairo_set_operator(ctx, CAIRO_OPERATOR_SOURCE);
	cairo_set_source_rgba(ctx, c.r, c.g, c.b, c.a);
	cairo_move_to(ctx, shape.x, shape.y);
	cairo_show_text(ctx, "Test");

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ctext_replace(ctext *tx, const char *str)
{
	GUARD(tx);

	(void)str;

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ctext_select(ctext *tx, size_t variant)
{
	GUARD(tx);

	tx->current = variant < tx->n ? variant : tx->n - 1;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ctext_setup(ctext *tx, size_t variant, struct ctext_style defaults, const char *name)
{
	GUARD(tx);

	size_t i = variant < tx->n ? variant : tx->n - 1;

	if (name)
	{
		snprintf(tx->variants[i].name, CCFG_TOKEN_LENGTH + 2, "%s.%s", tx->namespace, name);
	}
	else
	{
		tx->variants[i].name[0] = '\0';
	}

	tx->variants[i].defaults = defaults;
	tx->variants[i].resolved = defaults;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
ctext_shape(ctext *tx, struct carea shape)
{
	GUARD(tx);

	tx->shape = shape;
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
config(ccfg *cfg, const char *name, struct ctext_variant *variant)
{
	struct ctext_style *defaults = &variant->defaults;
	struct ctext_style *resolved = &variant->resolved;

	*resolved = *defaults;

	apply_clr(cfg, variant->name, name, "back_color",  &resolved->clr_bg);
	apply_clr(cfg, variant->name, name, "front_color", &resolved->clr_fg);
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
