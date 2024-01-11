#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include <dg/core/core.h>
#include <dg/base/base.h>
#include <dg/base/config.h>
#include <dg/base/draw.h>
#include <dg/base/string.h>
#include <dg/base/zone.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define _N_LINES 8

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

typedef enum {
	_GAME_PAUSE,
	_GAME_RUN,
	_GAME_DYING,
	_GAME_OVER,
} _game_state_t;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _callback_controls      (dg_core_cell_t *c);
static void _callback_main_draw     (dg_core_window_t *w, unsigned long delay);
static void _callback_valve         (dg_core_cell_t *c);
static void _draw_end_screen        (void);
static void _game_restart           (void);
static void _game_run               (unsigned long delay);
static void _grow_end_screen        (unsigned long delay);
static void _update_game_state      (_game_state_t state);
static void _update_pressure_visual (int i);
static void _update_info_visual     (void);
static bool _xcb_event_handler      (xcb_generic_event_t *x_ev);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static dg_core_window_t *_w = NULL;
static dg_core_grid_t   *_g = NULL;

static dg_core_cell_t *_c_info  = NULL;
static dg_core_cell_t *_c_play  = NULL;
static dg_core_cell_t *_c_pause = NULL;
static dg_core_cell_t *_c_reset = NULL;

static dg_core_cell_t *_c_valve[_N_LINES] = {NULL};
static dg_core_cell_t *_c_gauge[_N_LINES] = {NULL};
static dg_core_cell_t *_c_alert[_N_LINES] = {NULL};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static _game_state_t _game_state = _GAME_RUN;

static bool _first_frame          = true;
static double _pressure[_N_LINES] = {0};
static double _end_card_size      = 0.0;
static double _difficulty         = 1.0;
static unsigned long _timer       = 0;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const char *str_game_over =
	" ██████   █████  ███    ███ ███████      ██████  ██    ██ ███████ ██████ \n"
	"██       ██   ██ ████  ████ ██          ██    ██ ██    ██ ██      ██   ██\n"
	"██   ███ ███████ ██ ████ ██ █████       ██    ██ ██    ██ █████   ██████ \n"
	"██    ██ ██   ██ ██  ██  ██ ██          ██    ██  ██  ██  ██      ██   ██\n"
	" ██████  ██   ██ ██      ██ ███████      ██████    ████   ███████ ██   ██\n";
                                           

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(int argc, char **argv)
{
	/* module initialisation */

	dg_core_init(argc, argv, NULL, NULL, NULL);
	dg_base_init();

	/* object instantiation */

	_w = dg_core_window_create(DG_CORE_WINDOW_DEFAULT);
	_g = dg_core_grid_create(8, _N_LINES + 1);
	
	_c_info  = dg_base_label_create();
	_c_play  = dg_base_button_create();
	_c_pause = dg_base_button_create();
	_c_reset = dg_base_button_create();
	for (int i = 0; i < _N_LINES; i++) {
		_c_valve[i] = dg_base_button_create();
		_c_gauge[i] = dg_base_gauge_create();
		_c_alert[i] = dg_base_indicator_create();
	}

	/* cell configuration */

	char str_valve[8] = "";

	dg_base_label_set_label(_c_info, "DIfficulty : 1.0");
	dg_base_label_set_origin(_c_info, DG_BASE_ORIGIN_LEFT);

	dg_base_button_set_label(_c_play, "Play");
	dg_base_button_set_label_origin(_c_play, DG_BASE_ORIGIN_CENTER);
	dg_base_button_set_callback_pressed(_c_play, _callback_controls);
	dg_core_cell_disable(_c_play);

	dg_base_button_set_label(_c_pause, "Pause");
	dg_base_button_set_label_origin(_c_pause, DG_BASE_ORIGIN_CENTER);
	dg_base_button_set_callback_pressed(_c_pause, _callback_controls);

	dg_base_button_set_label(_c_reset, "Reset");
	dg_base_button_set_label_origin(_c_reset, DG_BASE_ORIGIN_CENTER);
	dg_base_button_set_callback_pressed(_c_reset, _callback_controls);

	for (int i = 0; i < _N_LINES; i++) {
		_pressure[i] = 100.0;
		sprintf(str_valve, "Valve %i", i);
		dg_base_button_set_label(_c_valve[i], str_valve);
		dg_base_button_set_label_origin(_c_valve[i], DG_BASE_ORIGIN_CENTER);
		dg_base_button_set_callback_pressed(_c_valve[i], _callback_valve);
		dg_base_gauge_set_label_style(_c_gauge[i], 0, "kPa");
		dg_base_gauge_set_limits(_c_gauge[i], 100.0, 900.0);
		dg_base_gauge_set_value(_c_gauge[i], _pressure[i]);
		dg_base_indicator_set_label(_c_alert[i], "HIGH");
	}

	/* grid configuration */

	dg_core_grid_set_column_width(_g, 0, 7);
	dg_core_grid_set_column_width(_g, 1, 40);
	dg_core_grid_set_column_width(_g, 2, 1);
	dg_core_grid_set_column_width(_g, 3, 4);
	dg_core_grid_set_column_width(_g, 4, 1);
	dg_core_grid_set_column_width(_g, 5, 4);
	dg_core_grid_set_column_width(_g, 6, 1);
	dg_core_grid_set_column_width(_g, 7, 4);
	
	dg_core_grid_set_column_growth(_g, 0, 1.0);
	dg_core_grid_set_column_growth(_g, 1, 1.0);
	dg_core_grid_set_column_growth(_g, 2, 1.0);
	dg_core_grid_set_column_growth(_g, 3, 1.0);
	dg_core_grid_set_column_growth(_g, 4, 1.0);
	dg_core_grid_set_column_growth(_g, 5, 1.0);
	dg_core_grid_set_column_growth(_g, 6, 1.0);
	dg_core_grid_set_column_growth(_g, 7, 1.0);

	dg_core_grid_set_row_growth(_g, 0, 1.0);
	dg_core_grid_set_row_growth(_g, 1, 1.0);
	dg_core_grid_set_row_growth(_g, 2, 1.0);
	dg_core_grid_set_row_growth(_g, 3, 1.0);
	dg_core_grid_set_row_growth(_g, 4, 1.0);
	dg_core_grid_set_row_growth(_g, 5, 1.0);
	dg_core_grid_set_row_growth(_g, 6, 1.0);
	dg_core_grid_set_row_growth(_g, 7, 1.0);

	dg_core_grid_assign_cell(_g, _c_info,  0, 8, 2,  1);
	dg_core_grid_assign_cell(_g, _c_reset, 2, 8, 2,  1);
	dg_core_grid_assign_cell(_g, _c_pause, 4, 8, 2,  1);
	dg_core_grid_assign_cell(_g, _c_play,  6, 8, 2,  1);
	for (int i = 0; i < _N_LINES; i++) {
		dg_core_grid_assign_cell(_g, _c_valve[i], 0, i, 1, 1);
		dg_core_grid_assign_cell(_g, _c_gauge[i], 1, i, 6, 1);
		dg_core_grid_assign_cell(_g, _c_alert[i], 7, i, 1, 1);
	}

	/* window configuration */

	dg_core_window_push_grid(_w, _g);
	dg_core_window_set_callback_redraw(_w, _callback_main_draw);
	dg_core_window_rename(_w, "Pressure Control Game", NULL);
	dg_core_window_activate(_w);

	/* event loop */

	dg_core_loop_set_event_preprocessor(_xcb_event_handler);
	dg_core_loop_run();

	/* cleanup & end */

	dg_core_window_destroy(_w);
	dg_core_grid_destroy(_g);
	dg_core_cell_destroy(_c_info);
	dg_core_cell_destroy(_c_play);
	dg_core_cell_destroy(_c_pause);
	dg_core_cell_destroy(_c_reset);
	for (int i = 0; i < _N_LINES; i++) {
		dg_core_cell_destroy(_c_valve[i]);
		dg_core_cell_destroy(_c_gauge[i]);
		dg_core_cell_destroy(_c_alert[i]);
	}

	dg_base_reset();
	dg_core_reset();

	return 0;
}

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void
_callback_controls(dg_core_cell_t *c)
{
	if (c == _c_play) {
		_update_game_state(_GAME_RUN);
	} else if (c == _c_pause) {
		_update_game_state(_GAME_PAUSE);
	} else if (c == _c_reset) {
		_game_restart();
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_callback_main_draw(dg_core_window_t *w, unsigned long delay)
{
	/* Here, we use DG's event loop as game loop and use this callback */
	/* to each window refresh as entry point.                          */

	switch (_game_state) {

		case _GAME_PAUSE:
			/* nothing to do */
			break;

		case _GAME_RUN:
			_game_run(delay);
			break;

		case _GAME_DYING:
			_grow_end_screen(delay);
			/* fallthrough */

		case _GAME_OVER:
			_draw_end_screen();
			break;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_callback_valve(dg_core_cell_t *c)
{
	int i = 0;

	for (; i < _N_LINES; i++) {
		if (_c_valve[i] == c) {
			break;
		}
	}

	_pressure[i] -= 300.0;
	if (_pressure[i] < 100.0) {
		_pressure[i] = 100.0;
	}
	
	_update_pressure_visual(i);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_draw_end_screen(void)
{
	dg_base_zone_t z = dg_base_zone_get_window(_w);

	/* draw body */

	dg_base_draw_rectangle(
		&z,
		DG_BASE_CONFIG->common_cl[DG_BASE_CONFIG_COLOR_YELLOW],
		0.5 - _end_card_size / 2,
		0.5 - _end_card_size / 2,
		0.5 + _end_card_size / 2,
		0.5 + _end_card_size / 2,
		0);

	/* draw status text */

	if (_end_card_size < 1.0) {
		return;
	}

	dg_base_string_t str1 = DG_BASE_STRING_EMPTY;
	dg_base_string_t str2 = dg_base_string_convert_double((double)_timer / 1000000, 2);
	dg_base_string_t str3 = dg_base_string_convert_double(_difficulty, 2);

	dg_base_string_set(&str1, str_game_over);
	dg_base_string_append(&str1, "\nYou survived ");
	dg_base_string_append(&str1, str2.chars);
	dg_base_string_append(&str1, "s\nDifficulty multiplier reached : ");
	dg_base_string_append(&str1, str3.chars);
	dg_base_string_append(&str1, "\n\nPress any key or button to restart");

	dg_base_draw_string(
			&z, 
			DG_BASE_CONFIG->common_cl[DG_BASE_CONFIG_COLOR_BLACK],
			&str1,
			z.pw / 2,
			z.ph / 2,
			true,
			DG_BASE_ORIGIN_CENTER,
			DG_BASE_ROTATION_NORMAL);

	dg_base_string_clear(&str1);
	dg_base_string_clear(&str2);
	dg_base_string_clear(&str3);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void 
_game_restart(void)
{
	for (int i = 0; i < _N_LINES; i++) {
		_pressure[i] = 100.0;
		_update_pressure_visual(i);
	}

	_first_frame   = true;
	_end_card_size = 0.0;
	_difficulty    = 1.0;
	_timer         = 0;

	_update_info_visual();
	dg_core_window_enable(_w);
	dg_core_window_redraw(_w);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_game_run(unsigned long delay)
{
	if (_first_frame) {
		_first_frame = false;         /* hack to skip big delay value after the game was not running      */
		dg_core_cell_redraw(_c_info); /* hack to force a redraw to run the redraw callback for next frame */
		return;
	}

	/* update metrics */

	_timer += delay;
	_difficulty += delay / 5000000.0;

	_update_info_visual();

	/* update pressure on one random gauge */

	long i = rand() % _N_LINES;

	_pressure[i] += _difficulty * delay / 10000.0;
	if (_pressure[i] > 1000.0) {
		_update_game_state(_GAME_DYING);
	} else {
		_update_pressure_visual(i);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_grow_end_screen(unsigned long delay)
{
	_end_card_size += delay / 250000.0;
	if (_end_card_size > 1.0) {
		_update_game_state(_GAME_OVER);
	} else {
		dg_core_window_redraw(_w);
	}	
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_update_game_state(_game_state_t state)
{
	switch (state) {

		case _GAME_PAUSE:
			dg_core_cell_disable(_c_pause);
			dg_core_cell_enable(_c_play);
			for (size_t i = 0; i < _N_LINES; i++) {
				dg_core_cell_disable(_c_valve[i]);
			}
			break;

		case _GAME_RUN:
			_first_frame = true;
			dg_core_cell_disable(_c_play);
			dg_core_cell_enable(_c_pause);
			for (size_t i = 0; i < _N_LINES; i++) {
				dg_core_cell_enable(_c_valve[i]);
			}
			break;

		case _GAME_DYING:
			dg_core_window_disable(_w);
			for (size_t i = 0; i < _N_LINES; i++) {
				dg_base_indicator_set_off(_c_alert[i]);
			}
			break;

		case _GAME_OVER:
			/* nothing to do */
			break;
	}
	
	_game_state = state;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_update_info_visual(void)
{
	char str[80] = "";

	sprintf(str, "Difficulty = %.2f / Time = %.2fs", _difficulty, (double)_timer / 1000000);

	dg_base_label_set_label(_c_info, str);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_update_pressure_visual(int i)
{
	if (_pressure[i] >= 900) {
		dg_base_indicator_set_critical(_c_alert[i], 3);
	} else if (_pressure[i] >= 800) {
		dg_base_indicator_set_critical(_c_alert[i], 2);
	} else if (_pressure[i] >= 700) {
		dg_base_indicator_set_critical(_c_alert[i], 1);
	} else if (_pressure[i] >= 500) {
		dg_base_indicator_set_on(_c_alert[i]);
	} else {
		dg_base_indicator_set_off(_c_alert[i]);
	}
	
	dg_base_gauge_set_value(_c_gauge[i], _pressure[i]);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_xcb_event_handler(xcb_generic_event_t *x_ev)
{
	if (_game_state != _GAME_OVER) {
		return false;
	}	

	switch (x_ev->response_type & ~0x80) {

		case XCB_KEY_PRESS:
		case XCB_BUTTON_PRESS:
			break;

		default:
			return false;
	}

	/* reset game parameters */

	_game_restart();
	_update_game_state(_GAME_RUN);

	return true;
}
