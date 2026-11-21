/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cairo/cairo.h>
#include <cassette/ccfg.h>
#include <cassette/cgui.h>
#include <cassette/cobj.h>
#include <stddef.h>
#include <stdlib.h>

#include "event.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct data
{
	cbox  *frame;
	ctext *label;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void event     (ccell *, void *, struct cevent);
static void ev_config (struct data *, struct cevent);
static void ev_draw   (struct data *, struct cevent);
static void ev_shape  (struct data *, struct cevent);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static constexpr struct cbox_style default_bx_style =
{
	.striped     = false,
	.pad         = 10,
	.border      = 10,
	.outline     = 10,
	.clr_border  = ccolor_green,
	.clr_outline = ccolor_blue,
	.clr_bg      = ccolor_black,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static constexpr struct ctext_style default_tx_style =
{
	.clr_fg = ccolor_white,
	.clr_bg = ccolor_transparent,
};

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

ccell *
clabel_create(void)
{
	struct data *data;

	if (!(data = malloc(sizeof(struct data))))
	{
		goto fail_data;
	}

	if (!(data->frame = cbox_create("label", 1)))
	{
		goto fail_frame;
	}

	if (!(data->label = ctext_create("label", 1)))
	{
		goto fail_label;
	}

	cbox_setup (data->frame, 0, default_bx_style, "");
	ctext_setup(data->label, 0, default_tx_style, "");

	return ccell_create(event, data);

	/* errors */

fail_label:
	cbox_destroy(data->frame);
fail_frame:
	free(data);
fail_data:
	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

nullptr_t
clabel_destroy(ccell *cl)
{
	struct data *data = ccell_data(cl);

	cbox_destroy (data->frame);
	ctext_destroy(data->label);
	free(ccell_data(cl));

	return ccell_destroy(cl);
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
event(ccell *cl, void *data, struct cevent ev)
{
	(void)cl;

	switch (ev.type)
	{
		case CEVENT_DRAW:
			ev_draw(data, ev);
			break;

		case CEVENT_CONFIG:
			ev_config(data, ev);
			break;

		case CEVENT_SHAPE:
			ev_shape(data, ev);
			break;
		
		default:
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_config(struct data *data, struct cevent ev)
{
	cbox_config (data->frame, ev.config);
	ctext_config(data->label, ev.config);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_draw(struct data *data, struct cevent ev)
{
	cbox_draw (data->frame, ev.drawable);
	ctext_draw(data->label, ev.drawable);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
ev_shape(struct data *data, struct cevent ev)
{
	cbox_shape (data->frame, ev.shape);
	ctext_shape(data->label, cbox_area(data->frame));
}
