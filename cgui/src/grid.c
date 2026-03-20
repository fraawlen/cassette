/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cassette/ccfg.h>
#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <float.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "event.h"
#include "grid.h"
#include "shell.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define GUARD(GR, ...)     if (!GR || cerr_critical(GR->err)) { return __VA_OPT__(__VA_ARGS__); }
#define GUARD_COL(GR, COL) if (COL >= gr->cols_n) { cerr_set(&gr->err, CERR_CALL);  return; }
#define GUARD_ROW(GR, ROW) if (ROW >= gr->rows_n) { cerr_set(&gr->err, CERR_CALL);  return; }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct line
{
	int32_t size;
	double factor;
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cgrid
{
	/* state */

	cshell *owner;
	enum cerr err;

	/* contents */

	cref *cells;
	struct line *cols;
	struct line *rows;
	uint32_t rows_n;
	uint32_t cols_n;

	/* config */

	uint32_t gutter;
	uint32_t gap;
	uint32_t pad;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static uint32_t config (ccfg  *, uint32_t, const char *);
static void     update (cgrid *);

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cgrid_assign(cgrid *gr, cshell *sh)
{
	GUARD(gr);

	if (!sh || cerr_critical(cshell_error(sh)) || gr->owner)
	{
		cerr_set(&gr->err, CERR_CALL);
	}
	else if (shell_push_grid(sh, gr))
	{
		gr->owner = sh;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgrid_clear_warnings(cgrid *gr)
{
	GUARD(gr);

	cerr_clear_warnings(&gr->err);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgrid *
cgrid_clone(const cgrid *gr)
{
	GUARD(gr, nullptr);

	cgrid *gr_new;

	if (!(gr_new = malloc(sizeof(cgrid))))
	{
		goto fail_alloc;
	}

	if (!(gr_new->rows = malloc(gr->rows_n * sizeof(struct line))))
	{
		goto fail_rows;
	}

	if (!(gr_new->cols = malloc(gr->cols_n * sizeof(struct line))))
	{
		goto fail_cols;
	}

	if (!(gr_new->cells = cref_clone(gr->cells)))
	{
		goto fail_cells;
	}

	memcpy(gr_new->rows, gr->rows, gr->rows_n * sizeof(struct line));
	memcpy(gr_new->cols, gr->cols, gr->cols_n * sizeof(struct line));

	gr_new->owner  = nullptr;
	gr_new->err    = gr->err;
	gr_new->rows_n = gr->rows_n;
	gr_new->cols_n = gr->cols_n;
	gr_new->gutter = 0;
	gr_new->gap    = 0;
	gr_new->pad    = 0;

	return gr_new;

	/* errors */

fail_cells:
	free(gr_new->cols);
fail_cols:
	free(gr_new->rows);
fail_rows:
	free(gr_new);
fail_alloc:
	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cgrid *
cgrid_create(uint32_t rows, uint32_t cols)
{
	cgrid *gr;

	if (!(gr = malloc(sizeof(cgrid))))
	{
		goto fail_alloc;
	}

	if (!(gr->rows = calloc(rows, sizeof(struct line))))
	{
		goto fail_rows;
	}

	if (!(gr->cols = calloc(cols, sizeof(struct line))))
	{
		goto fail_cols;
	}

	if (!(gr->cells = cref_create()))
	{
		goto fail_cells;
	}

	gr->err    = CERR_NONE;
	gr->owner  = nullptr;
	gr->rows_n = 0;
	gr->cols_n = 0;
	gr->gutter = 0;
	gr->gap    = 0;
	gr->pad    = 0;

	return gr;

	/* errors */

fail_cells:
	free(gr->cols);
fail_cols:
	free(gr->rows);
fail_rows:
	free(gr);
fail_alloc:
	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

nullptr_t
cgrid_destroy(cgrid *gr)
{
	if (gr)
	{
		cref_destroy(gr->cells);
		free(gr->cols);
		free(gr->rows);
		free(gr);
	}

	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cerr
cgrid_error(const cgrid *gr)
{
	return gr ? gr->err : CERR_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgrid_flex_col(cgrid *gr, uint32_t col, double factor)
{
	GUARD(gr);
	GUARD_COL(gr, col);

	if (factor < DBL_EPSILON)
	{
		cerr_set(&gr->err, CERR_CALL);
	}
	else
	{
		gr->cols[col].factor = factor;
		update(gr);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgrid_flex_row(cgrid *gr, uint32_t row, double factor)
{
	GUARD(gr);
	GUARD_ROW(gr, row);

	if (factor < DBL_EPSILON)
	{
		cerr_set(&gr->err, CERR_CALL);
	}
	else
	{
		gr->rows[row].factor = factor;
		update(gr);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgrid_resize_col(cgrid *gr, uint32_t col, int32_t size)
{
	GUARD(gr);
	GUARD_COL(gr, col);

	gr->cols[col].size = size;
	update(gr);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgrid_resize_row(cgrid *gr, uint32_t row, int32_t size)
{
	GUARD(gr);
	GUARD_ROW(gr, row);

	gr->rows[row].size = size;
	update(gr);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cgrid_retire(cgrid *gr)
{
	if (gr->owner)
	{
		shell_pull_grid(gr->owner, gr);
	}
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

void
grid_cache_geometry(cgrid *gr)
{
	(void)gr;

	// TODO
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

uint32_t
grid_h(cgrid *gr)
{
	(void)gr;

	// TODO

	return 0;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
grid_send_event(cgrid *gr, struct cevent ev)
{
	event_print(ev, "grid");
	switch (ev.type)
	{
		case CEVENT_CONFIG:
			gr->gutter = config(ev.config, 5, "gutter");
			gr->gap    = config(ev.config, 5, "gap");
			gr->pad    = config(ev.config, 5, "pad");
			break;

		default:
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

uint32_t
grid_w(cgrid *gr)
{
	(void)gr;

	// TODO

	return 0;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static uint32_t
config(ccfg *cfg, uint32_t base, const char *name)
{
	ccfg_fetch(cfg, "grid", name);

	return ccfg_iterate(cfg) ? cutil_str_to_long(ccfg_resource(cfg), 0, UINT32_MAX) : base;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
update(cgrid *gr)
{
	if (gr->owner)
	{
		shell_update_grid(gr->owner);
	}
}
