#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include <xcb/xcb.h>
#include <xcb/randr.h>

#include <dg/core/core.h>
#include <dg/core/resource.h>
#include <dg/base/base.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _callback_button_close (dg_core_cell_t *c);
static void _callback_button_conf  (dg_core_cell_t *c);
static void _callback_signal       (uint32_t serial);
static void _callback_reconfig     (void);

static void  _time_reset  (void);
static void  _time_setup  (void);
static void *_time_thread (void *arg);

static void _ui_place (void);
static void _ui_reset (void);
static void _ui_setup (int argc, char **argv);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static dg_core_window_t *_w       = NULL;
static dg_core_grid_t   *_g       = NULL;
static dg_core_cell_t   *_c_gap   = NULL;
static dg_core_cell_t   *_c_conf  = NULL;
static dg_core_cell_t   *_c_close = NULL;
static dg_core_cell_t   *_c_clock = NULL;

static pthread_t _timer;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(int argc, char **argv)
{
	/* setup */

	_ui_setup(argc, argv);
	_time_setup();

	/* event loop */

	dg_core_loop_run();

	/* cleanup & end */

	_time_reset();
	_ui_reset();

	return 0;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_callback_button_close(dg_core_cell_t *c)
{
	dg_core_window_deactivate(_w);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_callback_button_conf(dg_core_cell_t *c)
{
	dg_core_reconfig();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_callback_reconfig(void)
{
	_ui_place();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_callback_signal(uint32_t serial)
{
	/* get time */

	time_t     t = time(NULL);
	struct tm *T = localtime(&t);

	/* update clock label */

	char s[18] = "";

	sprintf(s, "%02i : %02i : %02i : %02i", T->tm_mday, T->tm_hour, T->tm_min, T->tm_sec);

	dg_base_label_set_label(_c_clock, s);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_time_reset(void)
{
	pthread_cancel(_timer);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_time_setup(void)
{
	pthread_create(&_timer, NULL, _time_thread, NULL);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void *
_time_thread(void *arg)
{
	while (1) {
		dg_core_loop_send_signal(1);
		sleep(1);
	}

	pthread_exit(NULL);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_ui_place(void)
{
	int16_t px;
	int16_t py;
	int16_t pw;
	int16_t ph;

	/* get xcb connection and screen to perform xcb operations not provided by DG */

	xcb_connection_t *x_con = dg_core_get_xcb_connection();
	xcb_screen_t     *x_scr = xcb_setup_roots_iterator(xcb_get_setup(x_con)).data;

	/* find primary monitor and retrieve its geometry */

	xcb_randr_get_monitors_cookie_t xc = xcb_randr_get_monitors(x_con, x_scr->root, 1);
	xcb_randr_get_monitors_reply_t *xr = xcb_randr_get_monitors_reply(x_con, xc, NULL);

	xcb_randr_monitor_info_iterator_t xi = xcb_randr_get_monitors_monitors_iterator(xr);
	for (; xi.rem; xcb_randr_monitor_info_next(&xi)) {
		if (xi.data->primary) {
			break;
		}
	}

	px = xi.data->x;
	py = xi.data->y + xi.data->height;
	pw = xi.data->width;

	free(xr);

	/* update the bar's position and size */

	ph  = dg_core_grid_get_min_pixel_height(_g);
	py -= ph;

	dg_core_window_set_fixed_position(_w, px, py);
	dg_core_window_set_fixed_size(_w, pw, ph);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_ui_reset(void)
{
	dg_core_window_destroy(_w);
	dg_core_grid_destroy(_g);
	dg_core_cell_destroy(_c_gap);
	dg_core_cell_destroy(_c_conf);
	dg_core_cell_destroy(_c_close);
	dg_core_cell_destroy(_c_clock);

	dg_base_reset();
	dg_core_reset();
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_ui_setup(int argc, char **argv)
{
	/* module initialisation */

	dg_core_init(argc, argv, NULL, NULL, NULL);
	dg_base_init();

	/* add DG resource tracking to update position of bar on reconfigs */
	/* add DG signal tracking to update the clock's value              */

	dg_core_resource_set_callback(_callback_reconfig);
	dg_core_loop_set_callback_signal(_callback_signal);

	/* object instantiation */

	_w = dg_core_window_create(DG_CORE_WINDOW_FIXED);
	_g = dg_core_grid_create(4, 1);

	_c_gap   = dg_base_gap_create();
	_c_conf  = dg_base_button_create();
	_c_close = dg_base_button_create();
	_c_clock = dg_base_label_create();

	/* cell configuration */

	dg_base_button_set_label(_c_conf,  "Reload resources");
	dg_base_button_set_label(_c_close, "Close bar & exit");
	dg_base_button_set_callback_pressed(_c_close, _callback_button_close);
	dg_base_button_set_callback_pressed(_c_conf,  _callback_button_conf);

	/* grid configuration */

	dg_core_grid_set_column_width(_g, 0, 16);
	dg_core_grid_set_column_width(_g, 1, 16);
	dg_core_grid_set_column_width(_g, 2, 0);
	dg_core_grid_set_column_width(_g, 3, 17);

	dg_core_grid_set_column_growth(_g, 0, 0.0);
	dg_core_grid_set_column_growth(_g, 1, 0.0);
	dg_core_grid_set_column_growth(_g, 2, 1.0);
	dg_core_grid_set_column_growth(_g, 3, 0.0);

	dg_core_grid_set_row_height(_g, 0, 1);
	dg_core_grid_set_row_growth(_g, 0, 1.0);

	dg_core_grid_assign_cell(_g, _c_close, 0, 0, 1, 1);
	dg_core_grid_assign_cell(_g, _c_conf , 1, 0, 1, 1);
	dg_core_grid_assign_cell(_g, _c_gap,   2, 0, 1, 1);
	dg_core_grid_assign_cell(_g, _c_clock, 3, 0, 1, 1);

	/* window configuration */

	_ui_place();

	dg_core_window_push_grid(_w, _g);
	dg_core_window_activate(_w);
}
