/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Graphics (CGUI) library.
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

#include <cassette/cgui.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define N 7

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct popup
{
	cgui_window *window;
	cgui_grid *grid;
	cgui_cell *label;
	cgui_cell *button;
	cgui_cell *filler;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void on_accel      (cgui_window *, int);
static void on_click      (cgui_cell *);
static void on_close      (cgui_window *);
static void popup_destroy (int);
static int  popup_find    (const cgui_cell *);
static void popup_setup   (int);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cstr *str = CSTR_PLACEHOLDER;

static const struct popup popup_placeholder =
{
	.window = CGUI_WINDOW_PLACEHOLDER,
	.label  = CGUI_CELL_PLACEHOLDER,
	.button = CGUI_CELL_PLACEHOLDER,
	.filler = CGUI_CELL_PLACEHOLDER,
};

static struct popup popups[N] =
{
	popup_placeholder,
	popup_placeholder,
	popup_placeholder,
	popup_placeholder,
	popup_placeholder,
	popup_placeholder,
};

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

 int
 main(int argc, char **argv)
 {
	/* Setup */

	cgui_init(argc, argv);

	str = cstr_create();
	for (int i = 0; i < N; i++)
	{
		popup_setup(i);
	}

	/* Run */

	cgui_window_activate(popups[0].window);
	cgui_run();

	/* End */

	if (cgui_error())
	{
		printf("Gui has failed during operation.\n");
	}

	cstr_destroy(str);
	for (int i = 0; i < N; i++)
	{
		popup_destroy(i);
	}

	cgui_reset();

	return 0;
 }

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
on_accel(cgui_window *w, int id)
{
	(void)w;
	(void)id;

	cgui_window_deactivate_all_popups();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
on_click(cgui_cell *c)
{
	cgui_window *w;
	cgui_window *p;
	double x;
	double y;
	int i;

	if ((i = popup_find(c)) == -1)
	{
		return;
	}

	w = popups[i].window;
	p = popups[i + 1].window;
	x = cgui_window_x(w) + cgui_window_width(w);
	y = cgui_window_y(w) + cgui_window_width(w) / 2;

	cgui_window_move_smart(p, x, y, x, y);
	cgui_window_activate(p);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
on_close(cgui_window *w)
{
	cgui_window_deactivate_all_popups();
	cgui_window_deactivate(w);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
popup_destroy(int i)
{
	cgui_window_destroy(popups[i].window);
	cgui_cell_destroy(popups[i].button);
	cgui_cell_destroy(popups[i].filler);
	cgui_cell_destroy(popups[i].label);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static int
popup_find(const cgui_cell *c)
{
	for (int i = 0; i < N; i++)
	{
		if (c == popups[i].button)
		{
			return i;
		}
	}

	return -1;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
popup_setup(int i)
{
	/* Instantiation */

	popups[i].window = cgui_window_create();
	popups[i].grid   = cgui_grid_create(1, 3);
	popups[i].label  = cgui_label_create();
	popups[i].button = cgui_button_create();
	popups[i].filler = cgui_filler_create();

	/* Special cases */
	
	cstr_clear(str);

	if (i == 0)
	{
		cstr_append(str, "Main window");
		cgui_window_on_close(popups[i].window, on_close);
	}
	else
	{
		cstr_append(str, "Popup nº");
		cstr_append(str, i);
		cgui_window_set_type(popups[i].window, CGUI_WINDOW_POPUP);
		if (i == N - 1)
		{
			cgui_button_disable(popups[i].button);
		}
	}

	/* Cell setup */

	cgui_label_set_label(popups[i].label, cstr_chars(str));
	cgui_label_align(popups[i].label, CGUI_ALIGN_CENTER);

	cgui_button_set_label(popups[i].button, "Open next popup");
	cgui_button_on_click(popups[i].button, on_click);

	/* Grid setup */

	cgui_grid_resize_col(popups[i].grid, 0, 15);

	cgui_grid_assign_cell(popups[i].grid, popups[i].label,  0, 0, 1, 1);
	cgui_grid_assign_cell(popups[i].grid, popups[i].filler, 0, 1, 1, 1);
	cgui_grid_assign_cell(popups[i].grid, popups[i].button, 0, 2, 1, 1);
	
	/* Window setup */

	cgui_window_push_grid(popups[i].window, popups[i].grid);
	cgui_window_rename(popups[i].window, cstr_chars(str));
	cgui_window_set_accelerator(popups[i].window, 1, "close popups", on_accel);
}
