#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <dg/core/core.h>
#include <dg/core/stack.h>
#include <dg/base/base.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

typedef struct {
	dg_core_window_t *w;
	dg_core_grid_t *g;
	dg_core_cell_t *but_spawn;
	dg_core_cell_t *but_close;
	dg_core_cell_t *but_close_all;
} _window_t;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define _WIN(I) ((_window_t*)_windows.ptr[I])

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void _callback_button_spawn     (dg_core_cell_t   *c);
static void _callback_button_close     (dg_core_cell_t   *c);
static void _callback_button_close_all (dg_core_cell_t   *c);
static void _callback_window_close     (dg_core_window_t *w);

static _window_t *_window_find  (dg_core_cell_t *c);
static void       _window_kill  (_window_t *win);
static void       _window_spawn (void);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static dg_core_stack_t _windows = {0};

static unsigned int _counter = 0;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(int argc, char **argv)
{
	/* module initialisation */

	dg_core_init(argc, argv, NULL, NULL, NULL);
	dg_base_init();

	/* initial window */

	_window_spawn();

	/* event loop */

	dg_core_loop_run();

	/* cleanup & end */

	for (size_t i = 0; i < _windows.n; i++) {
		_window_kill(_WIN(i));
	}

	dg_base_reset();
	dg_core_reset();

	return 0;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_callback_button_spawn(dg_core_cell_t *c)
{
	_window_spawn();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_callback_button_close(dg_core_cell_t *c)
{
	_window_kill(_window_find(c));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_callback_button_close_all(dg_core_cell_t *c)
{
	_window_t *win = _window_find(c);

	while (_windows.n > 1) {
		_window_kill(_WIN(_WIN(0) == win ? 1 : 0));
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_callback_window_close(dg_core_window_t *w)
{
	for (size_t i = 0; i < _windows.n; i++) {
		if (_WIN(i)->w == w) {
			_window_kill(_WIN(i));
			break;
		}
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static _window_t *
_window_find(dg_core_cell_t *c)
{
	for (size_t i = 0; i < _windows.n; i++) {
		if (_WIN(i)->but_spawn     == c ||
		    _WIN(i)->but_close     == c ||
			_WIN(i)->but_close_all == c) {
			return _WIN(i);
		}
	}

	return NULL;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_window_kill(_window_t *win)
{
	dg_core_window_destroy(win->w);
	dg_core_grid_destroy(win->g);
	dg_core_cell_destroy(win->but_spawn);
	dg_core_cell_destroy(win->but_close);
	dg_core_cell_destroy(win->but_close_all);

	dg_core_stack_pull(&_windows, win);
	if (_windows.n == 1) {
		dg_core_cell_disable(_WIN(0)->but_close_all);
	}

	free(win);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_window_spawn(void)
{
	/* object instantiation */

	_window_t *win = malloc(sizeof(_window_t));

	win->w = dg_core_window_create(DG_CORE_WINDOW_DEFAULT);
	win->g = dg_core_grid_create(2, 3);

	win->but_spawn     = dg_base_button_create();
	win->but_close     = dg_base_button_create();
	win->but_close_all = dg_base_button_create();

	/* cell configuration */

	dg_base_button_set_label(win->but_spawn,     "Spawn new window");
	dg_base_button_set_label(win->but_close,     "Close this window");
	dg_base_button_set_label(win->but_close_all, "Close other windows");

	dg_base_button_set_icon(win->but_spawn,     DG_BASE_BUTTON_ICON_YES);
	dg_base_button_set_icon(win->but_close,     DG_BASE_BUTTON_ICON_NO);
	dg_base_button_set_icon(win->but_close_all, DG_BASE_BUTTON_ICON_NEUTRAL);

	dg_base_button_set_callback_pressed(win->but_spawn,     _callback_button_spawn);
	dg_base_button_set_callback_pressed(win->but_close,     _callback_button_close);
	dg_base_button_set_callback_pressed(win->but_close_all, _callback_button_close_all);

	if (_windows.n == 0) {
		dg_core_cell_disable(win->but_close_all);
	} else if (_windows.n == 1) {
		dg_core_cell_enable(_WIN(0)->but_close_all);
	}

	/* grid configuration */

	dg_core_grid_set_column_width(win->g, 0, 18);
	dg_core_grid_set_column_width(win->g, 1, -1);

	dg_core_grid_set_column_growth(win->g, 0, 1.0);
	dg_core_grid_set_column_growth(win->g, 1, 0.0);

	dg_core_grid_set_row_height(win->g, 0, 1);
	dg_core_grid_set_row_height(win->g, 1, 1);
	dg_core_grid_set_row_height(win->g, 2, 1);

	dg_core_grid_set_row_growth(win->g, 0, 1.0);
	dg_core_grid_set_row_growth(win->g, 1, 1.0);
	dg_core_grid_set_row_growth(win->g, 2, 1.0);

	dg_core_grid_assign_cell(win->g, win->but_spawn,     0, 0, 2, 1);
	dg_core_grid_assign_cell(win->g, win->but_close,     0, 1, 2, 1);
	dg_core_grid_assign_cell(win->g, win->but_close_all, 0, 2, 2, 1);

	/* window configuration */

	char name[50] = "";

	sprintf(name, "Window %u", _counter++);

	dg_core_window_push_grid(win->w, win->g);
	dg_core_window_set_callback_close(win->w, _callback_window_close);
	dg_core_window_rename(win->w, name, NULL);
	dg_core_window_activate(win->w);

	/* track window */

	dg_core_stack_push(&_windows, win, NULL);
}
