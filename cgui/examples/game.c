/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Graphics (CGUI) library.
 *
 * This library is free software; you can redistribute it and/or modify it either under the terms of the GNU
 * Lesser General Public License as published by the Free Software Foundation; either version 3.0 of the
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
#include <cassette/cobj.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define N 7

#define P_MIN  100.0
#define P_MAX  999.0
#define P_END 1100.0
#define P_DEC  300.0
#define P_1    500.0
#define P_2    700.0
#define P_3    800.0
#define P_4    900.0

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

enum game_state
{
	PLAY,
	PAUSE,
	OVER,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct row
{
	cgui_cell *button;
	cgui_cell *gauge;
	cgui_cell *beacon;
	double pressure;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void on_accel      (cgui_window *, int);
static void on_click      (cgui_cell   *);
static void on_close      (cgui_window *);
static void on_draw       (cgui_window *, unsigned long);
static void restart       (void);
static void setup_dialog  (void);
static void setup_row     (int);
static void setup_window  (void);
static void update_gauge  (int);
static void update_info   (void);
static void update_state  (enum game_state);
static void update_valves (bool);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/* UI elements */

static cgui_window *window = CGUI_WINDOW_PLACEHOLDER;
static cgui_window *dialog = CGUI_WINDOW_PLACEHOLDER;
static cgui_grid   *grid_w = CGUI_GRID_PLACEHOLDER;
static cgui_grid   *grid_d = CGUI_GRID_PLACEHOLDER;
static cgui_cell   *info   = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *play   = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *pause  = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *reset  = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *retry  = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *quit   = CGUI_CELL_PLACEHOLDER;
static cgui_cell   *over   = CGUI_CELL_PLACEHOLDER;

static struct row rows[N];

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/* Game state */

enum game_state state = PLAY;
double difficulty = 1.0;
unsigned long timer = 0;
bool first_frame = true;

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

/**
 * In this simple demo game, CGUI's redraw cycle is used as game loop (throught the callback on_draw()).
 * Everytime the main window redraws, on_draw() will update the gauges values, which trigger a new window
 * redraw and primes the next game udpate. 
 * This example also showcases accelerator usage. Try to use "xprop | grep CGUI" on the main window.
 * Accelerators are window-specific application shortcuts that are discoverable by other processes.
 */

 int
 main(int argc, char **argv)
 {
	/* Instantiation */

	cgui_init(argc, argv);

	setup_window();
	setup_dialog();

	/* Run */

	cgui_run();

	/* End & cleanup */

	if (cgui_error())
	{
		printf("Gui has failed during operation (%i).\n", cgui_error());
	}

	cgui_window_destroy(window);
	cgui_window_destroy(dialog);
	cgui_grid_destroy(grid_w);
	cgui_grid_destroy(grid_d);
	cgui_cell_destroy(info);
	cgui_cell_destroy(play);
	cgui_cell_destroy(pause);
	cgui_cell_destroy(reset);
	cgui_cell_destroy(retry);
	cgui_cell_destroy(quit);
	cgui_cell_destroy(over);

	for (int i = 0; i < N; i++)
	{
		cgui_cell_destroy(rows[i].button);
		cgui_cell_destroy(rows[i].gauge);
		cgui_cell_destroy(rows[i].beacon);
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

	switch (id)
	{
		case 1:
			on_click(state == PLAY ? pause : play);
			break;

		case 2:
			on_click(reset);
			break;

		case 3:
			on_click(quit);
			break;
			
		default:
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
on_click(cgui_cell *c)
{
	(void)c;

	if (c == play)
	{
		update_state(PLAY);
	}
	else if (c == pause)
	{
		update_state(PAUSE);
	}
	else if (c == reset)
	{
		restart();
	}
	else if (c == quit)
	{
		cgui_exit();
	}
	else if (c == retry)
	{
		cgui_window_deactivate(dialog);
		cgui_window_enable(window);
		update_state(PLAY);
		restart();
	}
	else
	{
		for (int i = 0; i < N; i++)
		{
			if (c == rows[i].button)
			{
				rows[i].pressure -= P_DEC;
				rows[i].pressure  = rows[i].pressure < P_MIN ? P_MIN : rows[i].pressure;
				update_gauge(i);
			}
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
on_close(cgui_window *w)
{
	(void)w;

	cgui_exit();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
on_draw(cgui_window *w, unsigned long delay)
{
	int i;

	if (state != PLAY)
	{
		return;
	}

	/* hack to skip the first delay that will shows an inaccurate delay */

	if (first_frame)
	{
		cgui_window_redraw(w);
		first_frame = false;
		return;
	}

	/* update metrics */

	timer      += delay;
	difficulty += delay / 5000000.0;

	update_info();

	/* update pressure on one random gauge */

	i = rand() % N;
	if ((rows[i].pressure += difficulty * delay / 10000.0) > P_END)
	{
		update_state(OVER);
	}
	else
	{
		update_gauge(i);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
restart(void)
{
	timer      = 0;
	difficulty = 0.0;
	update_info();

	for (int i = 0; i < N; i++)
	{
		rows[i].pressure = P_MIN;
		update_gauge(i);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
setup_dialog(void)
{
	dialog = cgui_window_create();
	grid_d = cgui_grid_create(2, 2);
	over   = cgui_label_create();
	retry  = cgui_button_create();
	quit   = cgui_button_create();

	/* Cell setup */

	cgui_button_set_label(retry, "RETRY");
	cgui_button_set_label(quit,  "QUIT");
	cgui_button_on_click(retry, on_click);
	cgui_button_on_click(quit,  on_click);

	cgui_label_set(over, "GAME OVER");

	/* Grid setup */

	cgui_grid_resize_col(grid_d, 0, 5);
	cgui_grid_resize_col(grid_d, 1, 5);

	cgui_grid_assign_cell(grid_d, over,  0, 0, 2, 1);
	cgui_grid_assign_cell(grid_d, retry, 0, 1, 1, 1);
	cgui_grid_assign_cell(grid_d, quit,  1, 1, 1, 1);
	
	/* Window setup */

	cgui_window_push_grid(dialog, grid_d);
	cgui_window_rename(dialog, "Game Over");
	cgui_window_on_close(dialog, on_close);
	cgui_window_tack(dialog, window);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
setup_row(int i)
{
	char str[20] = "";

	rows[i].button = cgui_button_create();
	rows[i].gauge  = cgui_gauge_create();
	rows[i].beacon = cgui_beacon_create();

	snprintf(str, 20, "Valve %i", i + 1);

	cgui_button_set_label(rows[i].button, str);
	cgui_button_on_click(rows[i].button, on_click);
	cgui_gauge_clamp_value(rows[i].gauge, P_MIN, P_MAX);
	cgui_gauge_set_units(rows[i].gauge, "kPa");
	cgui_beacon_set_label(rows[i].beacon, "HIGH");

	cgui_grid_assign_cell(grid_w, rows[i].button, 0, i, 1, 1);
	cgui_grid_assign_cell(grid_w, rows[i].gauge,  1, i, 3, 1);
	cgui_grid_assign_cell(grid_w, rows[i].beacon, 4, i, 1, 1);

	cgui_grid_set_row_flex(grid_w, i, 1.0);

	rows[i].pressure = P_MIN;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
setup_window(void)
{
	window = cgui_window_create();
	grid_w = cgui_grid_create(5, 1 + N);
	info   = cgui_label_create();
	play   = cgui_button_create();
	pause  = cgui_button_create();
	reset  = cgui_button_create();

	for (int i = 0; i < N; i++)
	{
		setup_row(i);
	}

	/* Cell setup */

	cgui_button_set_label(play,  "PLAY");
	cgui_button_set_label(pause, "PAUSE");
	cgui_button_set_label(reset, "RESET");
	cgui_button_on_click(play,  on_click);
	cgui_button_on_click(pause, on_click);
	cgui_button_on_click(reset, on_click);
	cgui_button_disable(play);

	cgui_label_align(info, CGUI_ALIGN_LEFT);

	update_info();

	/* Grid setup */

	cgui_grid_resize_col(grid_w, 0,  7);
	cgui_grid_resize_col(grid_w, 1, 25);
	cgui_grid_resize_col(grid_w, 2,  6);
	cgui_grid_resize_col(grid_w, 3,  5);
	cgui_grid_resize_col(grid_w, 4,  5);
	cgui_grid_resize_col(grid_w, 5,  5);

	cgui_grid_set_col_flex(grid_w,     1, 1.0);
	cgui_grid_set_row_flex(grid_w, N + 1, 1.0);

	cgui_grid_assign_cell(grid_w, info,  0, N, 2, 1);
	cgui_grid_assign_cell(grid_w, reset, 2, N, 1, 1);
	cgui_grid_assign_cell(grid_w, pause, 3, N, 1, 1);
	cgui_grid_assign_cell(grid_w, play,  4, N, 1, 1);
	
	/* Window setup */

	cgui_window_push_grid(window, grid_w);
	cgui_window_set_accelerator(window, 1, "Play / Pause", on_accel);
	cgui_window_set_accelerator(window, 2, "Reset", on_accel);
	cgui_window_set_accelerator(window, 3, "Quit", on_accel);
	cgui_window_rename(window, "Demo game");
	cgui_window_on_close(window, on_close);
	cgui_window_on_draw(window, on_draw);
	cgui_window_activate(window);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
update_gauge(int i)
{
	double p = rows[i].pressure;

	if (p > P_4)
	{
		cgui_beacon_set_state(rows[i].beacon, CGUI_BEACON_CRITICAL);
		cgui_beacon_set_blink_speed(rows[i].beacon, 3);
	}
	else if (p > P_3)
	{
		cgui_beacon_set_state(rows[i].beacon, CGUI_BEACON_CRITICAL);
		cgui_beacon_set_blink_speed(rows[i].beacon, 2);
	}
	else if (p > P_2)
	{
		cgui_beacon_set_state(rows[i].beacon, CGUI_BEACON_CRITICAL);
		cgui_beacon_set_blink_speed(rows[i].beacon, 1);
	}
	else if (p > P_1)
	{
		cgui_beacon_set_state(rows[i].beacon, CGUI_BEACON_ON);
	}
	else
	{
		cgui_beacon_set_state(rows[i].beacon, CGUI_BEACON_OFF);
	}

	cgui_gauge_set_value(rows[i].gauge, p);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
update_info(void)
{
	char str[80] = "";

	snprintf(str, 80, "Difficulty = %.2f / Time = %.2fs", difficulty, (double)timer / 1000000);

	cgui_label_set(info, str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
update_state(enum game_state new)
{
	if (new == state)
	{
		return;
	}

	switch ((state = new))
	{
		case PLAY:
			cgui_button_disable(play);
			cgui_button_enable(pause);
			update_valves(true);
			first_frame = true;
			break;

		case PAUSE:
			cgui_button_disable(pause);
			cgui_button_enable(play);
			update_valves(false);
			break;

		case OVER:
			cgui_window_disable(window);
			cgui_window_activate(dialog);
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
update_valves(bool ena)
{
	for (int i = 0; i < N; i++)
	{
		if (ena)
		{
			cgui_button_enable(rows[i].button);
		}
		else
		{
			cgui_button_disable(rows[i].button);
		}
	}
}
