#include <stdlib.h>

#include <dg/core/core.h>
#include <dg/base/base.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define _N_C 57

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static dg_core_window_t *_w = NULL;
static dg_core_grid_t   *_g = NULL;

static dg_core_cell_t *_c[_N_C] = {NULL};

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
	_g = dg_core_grid_create(14, 12);

	_c[ 0] = dg_base_button_create();
	_c[ 1] = dg_base_button_create();
	_c[ 2] = dg_base_button_create();
	_c[ 3] = dg_base_button_create();
	_c[ 4] = dg_base_button_create();
	_c[ 5] = dg_base_button_create();
	_c[ 6] = dg_base_button_create();
	_c[ 7] = dg_base_button_create();
	_c[ 8] = dg_base_button_create();
	_c[ 9] = dg_base_button_create();
	_c[10] = dg_base_button_create();
	_c[11] = dg_base_button_create();
	_c[12] = dg_base_button_create();
	_c[13] = dg_base_button_create();
	_c[14] = dg_base_button_create();

	_c[15] = dg_base_switch_create();
	_c[16] = dg_base_switch_create();
	_c[17] = dg_base_switch_create();
	_c[18] = dg_base_switch_create();
	_c[19] = dg_base_switch_create();
	_c[20] = dg_base_switch_create();
	
	_c[21] = dg_base_indicator_create();
	_c[22] = dg_base_indicator_create();
	_c[23] = dg_base_indicator_create();

	_c[24] = dg_base_gauge_create();
	_c[25] = dg_base_gauge_create();
	_c[26] = dg_base_gauge_create();
	_c[27] = dg_base_gauge_create();
	_c[28] = dg_base_gauge_create();
	_c[29] = dg_base_gauge_create();
	_c[30] = dg_base_gauge_create();
	_c[31] = dg_base_gauge_create();
	
	_c[32] = dg_base_spinner_create();
	_c[33] = dg_base_spinner_create();
	
	_c[34] = dg_base_label_create();
	_c[35] = dg_base_label_create();
	_c[36] = dg_base_label_create();
	_c[37] = dg_base_label_create();
	_c[38] = dg_base_label_create();
	_c[39] = dg_base_label_create();
	_c[40] = dg_base_label_create();
	_c[41] = dg_base_label_create();
	_c[42] = dg_base_label_create();
	_c[43] = dg_base_label_create();
	_c[44] = dg_base_label_create();
	_c[45] = dg_base_label_create();
	_c[46] = dg_base_label_create();
	_c[47] = dg_base_label_create();
	_c[48] = dg_base_label_create();
	_c[49] = dg_base_label_create();

	_c[50] = dg_base_label_create();
	_c[51] = dg_base_label_create();
	_c[52] = dg_base_label_create();
	_c[53] = dg_base_label_create();
	_c[54] = dg_base_label_create();
	
	_c[55] = dg_base_gap_create();
	_c[56] = dg_base_placeholder_create();

	/* cell configuration */

	dg_core_cell_disable(_c[14]);
	dg_core_cell_disable(_c[16]);
	dg_core_cell_disable(_c[17]);
	dg_core_cell_disable(_c[19]);
	dg_core_cell_disable(_c[20]);

	dg_base_button_set_icon(_c[ 0], DG_BASE_BUTTON_ICON_YES);
	dg_base_button_set_icon(_c[ 1], DG_BASE_BUTTON_ICON_NO);
	dg_base_button_set_icon(_c[ 2], DG_BASE_BUTTON_ICON_NEUTRAL);
	dg_base_button_set_icon(_c[ 3], DG_BASE_BUTTON_ICON_LINK_IN);
	dg_base_button_set_icon(_c[ 4], DG_BASE_BUTTON_ICON_LINK_OUT);
	dg_base_button_set_icon(_c[ 5], DG_BASE_BUTTON_ICON_CROSS);
	dg_base_button_set_icon(_c[ 6], DG_BASE_BUTTON_ICON_PLUS);
	dg_base_button_set_icon(_c[ 7], DG_BASE_BUTTON_ICON_MINUS);
	dg_base_button_set_icon(_c[ 8], DG_BASE_BUTTON_ICON_LEFT);
	dg_base_button_set_icon(_c[ 9], DG_BASE_BUTTON_ICON_RIGHT);
	dg_base_button_set_icon(_c[10], DG_BASE_BUTTON_ICON_UP);
	dg_base_button_set_icon(_c[11], DG_BASE_BUTTON_ICON_DOWN);
	dg_base_button_set_icon(_c[13], DG_BASE_BUTTON_ICON_NEUTRAL);
	dg_base_button_set_label(_c[12], "Button");
	dg_base_button_set_label(_c[13], "Button");
	dg_base_button_set_label(_c[14], "Button");

	dg_base_switch_set_on(_c[17]);
	dg_base_switch_set_on(_c[20]);
	dg_base_switch_set_label(_c[15], "Switch");
	dg_base_switch_set_label(_c[16], "Switch");
	dg_base_switch_set_label(_c[17], "Switch");

	dg_base_indicator_set_on(_c[22]);
	dg_base_indicator_set_critical(_c[23], 1);
	dg_base_indicator_set_label(_c[21], "Indicator");
	dg_base_indicator_set_label(_c[22], "Indicator");
	dg_base_indicator_set_label(_c[23], "Indicator");

	dg_base_gauge_set_value_unknown(_c[24]);
	dg_base_gauge_set_value_unknown(_c[28]);
	dg_base_gauge_set_label_style(_c[27], 1, "\nUNITS");
	dg_base_gauge_set_label_style(_c[31], 1, "\nUNITS");
	dg_base_gauge_set_limits(_c[27], 100.0, 200.0);
	dg_base_gauge_set_limits(_c[31], 100.0, 200.0);
	dg_base_gauge_set_value(_c[25], 100.0);
	dg_base_gauge_set_value(_c[26], 68.0);
	dg_base_gauge_set_value(_c[27], 120.8);
	dg_base_gauge_set_value(_c[29], 100.0);
	dg_base_gauge_set_value(_c[30], 68.0);
	dg_base_gauge_set_value(_c[31], 120.8);
	dg_base_gauge_set_vertical(_c[28]);
	dg_base_gauge_set_vertical(_c[29]);
	dg_base_gauge_set_vertical(_c[30]);
	dg_base_gauge_set_vertical(_c[31]);

	dg_base_spinner_set_label(_c[32], "Spinner");

	dg_base_label_set_color(_c[34], DG_BASE_CONFIG_COLOR_BLACK);
	dg_base_label_set_color(_c[35], DG_BASE_CONFIG_COLOR_RED);
	dg_base_label_set_color(_c[36], DG_BASE_CONFIG_COLOR_GREEN);
	dg_base_label_set_color(_c[37], DG_BASE_CONFIG_COLOR_YELLOW);
	dg_base_label_set_color(_c[38], DG_BASE_CONFIG_COLOR_BRIGHT_BLACK);
	dg_base_label_set_color(_c[39], DG_BASE_CONFIG_COLOR_BRIGHT_RED);
	dg_base_label_set_color(_c[40], DG_BASE_CONFIG_COLOR_BRIGHT_GREEN);
	dg_base_label_set_color(_c[41], DG_BASE_CONFIG_COLOR_BRIGHT_YELLOW);
	dg_base_label_set_color(_c[42], DG_BASE_CONFIG_COLOR_BLUE);
	dg_base_label_set_color(_c[43], DG_BASE_CONFIG_COLOR_MAGENTA);
	dg_base_label_set_color(_c[44], DG_BASE_CONFIG_COLOR_CYAN);
	dg_base_label_set_color(_c[45], DG_BASE_CONFIG_COLOR_WHITE);
	dg_base_label_set_color(_c[46], DG_BASE_CONFIG_COLOR_BRIGHT_BLUE);
	dg_base_label_set_color(_c[47], DG_BASE_CONFIG_COLOR_BRIGHT_MAGENTA);
	dg_base_label_set_color(_c[48], DG_BASE_CONFIG_COLOR_BRIGHT_CYAN);
	dg_base_label_set_color(_c[49], DG_BASE_CONFIG_COLOR_BRIGHT_WHITE);
	dg_base_label_set_origin(_c[34], DG_BASE_ORIGIN_CENTER);
	dg_base_label_set_origin(_c[35], DG_BASE_ORIGIN_CENTER);
	dg_base_label_set_origin(_c[36], DG_BASE_ORIGIN_CENTER);
	dg_base_label_set_origin(_c[37], DG_BASE_ORIGIN_CENTER);
	dg_base_label_set_origin(_c[38], DG_BASE_ORIGIN_CENTER);
	dg_base_label_set_origin(_c[39], DG_BASE_ORIGIN_CENTER);
	dg_base_label_set_origin(_c[40], DG_BASE_ORIGIN_CENTER);
	dg_base_label_set_origin(_c[41], DG_BASE_ORIGIN_CENTER);
	dg_base_label_set_origin(_c[42], DG_BASE_ORIGIN_CENTER);
	dg_base_label_set_origin(_c[43], DG_BASE_ORIGIN_CENTER);
	dg_base_label_set_origin(_c[44], DG_BASE_ORIGIN_CENTER);
	dg_base_label_set_origin(_c[45], DG_BASE_ORIGIN_CENTER);
	dg_base_label_set_origin(_c[46], DG_BASE_ORIGIN_CENTER);
	dg_base_label_set_origin(_c[47], DG_BASE_ORIGIN_CENTER);
	dg_base_label_set_origin(_c[48], DG_BASE_ORIGIN_CENTER);
	dg_base_label_set_origin(_c[49], DG_BASE_ORIGIN_CENTER);
	dg_base_label_set_label(_c[34], "B");
	dg_base_label_set_label(_c[35], "R");
	dg_base_label_set_label(_c[36], "G");
	dg_base_label_set_label(_c[37], "Y");
	dg_base_label_set_label(_c[38], "B");
	dg_base_label_set_label(_c[39], "R");
	dg_base_label_set_label(_c[40], "G");
	dg_base_label_set_label(_c[41], "Y");
	dg_base_label_set_label(_c[42], "B");
	dg_base_label_set_label(_c[43], "M");
	dg_base_label_set_label(_c[44], "C");
	dg_base_label_set_label(_c[45], "W");
	dg_base_label_set_label(_c[46], "B");
	dg_base_label_set_label(_c[47], "M");
	dg_base_label_set_label(_c[48], "C");
	dg_base_label_set_label(_c[49], "W");

	dg_base_label_set_rotation(_c[51], DG_BASE_ROTATION_RIGHT);
	dg_base_label_set_rotation(_c[53], DG_BASE_ROTATION_INVERTED);
	dg_base_label_set_rotation(_c[54], DG_BASE_ROTATION_LEFT);
	dg_base_label_set_origin(_c[51], DG_BASE_ORIGIN_BOTTOM);
	dg_base_label_set_origin(_c[52], DG_BASE_ORIGIN_CENTER);
	dg_base_label_set_origin(_c[53], DG_BASE_ORIGIN_RIGHT);
	dg_base_label_set_origin(_c[54], DG_BASE_ORIGIN_TOP);
	dg_base_label_set_label(_c[50], "Label");
	dg_base_label_set_label(_c[51], "Label");
	dg_base_label_set_label(_c[52], "Label\nLabel\nLabel");
	dg_base_label_set_label(_c[53], "Label");
	dg_base_label_set_label(_c[54], "Label");

	/* grid configuration */

	dg_core_grid_set_column_width(_g,  0, -1);
	dg_core_grid_set_column_width(_g,  1, -1);
	dg_core_grid_set_column_width(_g,  2, -1);
	dg_core_grid_set_column_width(_g,  3, -1);
	dg_core_grid_set_column_width(_g,  4, -1);
	dg_core_grid_set_column_width(_g,  5,  6);
	dg_core_grid_set_column_width(_g,  6,  6);
	dg_core_grid_set_column_width(_g,  7, -1);
	dg_core_grid_set_column_width(_g,  8, -1);
	dg_core_grid_set_column_width(_g,  9,  9);
	dg_core_grid_set_column_width(_g, 10,  4);
	dg_core_grid_set_column_width(_g, 11,  4);
	dg_core_grid_set_column_width(_g, 12,  4);
	dg_core_grid_set_column_width(_g, 13,  5);

	dg_core_grid_set_row_height(_g, 6, 2);

	dg_core_grid_assign_cell(_g, _c[ 0], 0, 0, 1, 1);
	dg_core_grid_assign_cell(_g, _c[ 1], 1, 0, 1, 1);
	dg_core_grid_assign_cell(_g, _c[ 2], 2, 0, 1, 1);
	dg_core_grid_assign_cell(_g, _c[ 3], 3, 0, 1, 1);
	dg_core_grid_assign_cell(_g, _c[ 4], 0, 1, 1, 1);
	dg_core_grid_assign_cell(_g, _c[ 5], 1, 1, 1, 1);
	dg_core_grid_assign_cell(_g, _c[ 6], 2, 1, 1, 1);
	dg_core_grid_assign_cell(_g, _c[ 7], 3, 1, 1, 1);
	dg_core_grid_assign_cell(_g, _c[ 8], 0, 2, 1, 1);
	dg_core_grid_assign_cell(_g, _c[ 9], 1, 2, 1, 1);
	dg_core_grid_assign_cell(_g, _c[10], 2, 2, 1, 1);
	dg_core_grid_assign_cell(_g, _c[11], 3, 2, 1, 1);
	dg_core_grid_assign_cell(_g, _c[12], 4, 0, 2, 1);
	dg_core_grid_assign_cell(_g, _c[13], 4, 1, 2, 1);
	dg_core_grid_assign_cell(_g, _c[14], 4, 2, 2, 1);

	dg_core_grid_assign_cell(_g, _c[15], 6, 0, 2, 1);
	dg_core_grid_assign_cell(_g, _c[16], 6, 1, 2, 1);
	dg_core_grid_assign_cell(_g, _c[17], 6, 2, 2, 1);
	dg_core_grid_assign_cell(_g, _c[18], 8, 0, 1, 1);
	dg_core_grid_assign_cell(_g, _c[19], 8, 1, 1, 1);
	dg_core_grid_assign_cell(_g, _c[20], 8, 2, 1, 1);

	dg_core_grid_assign_cell(_g, _c[21], 9, 0, 1, 1);
	dg_core_grid_assign_cell(_g, _c[22], 9, 1, 1, 1);
	dg_core_grid_assign_cell(_g, _c[23], 9, 2, 1, 1);

	dg_core_grid_assign_cell(_g, _c[24], 0, 3, 10, 1);
	dg_core_grid_assign_cell(_g, _c[25], 0, 4, 10, 1);
	dg_core_grid_assign_cell(_g, _c[26], 0, 5, 10, 1);
	dg_core_grid_assign_cell(_g, _c[27], 0, 6, 10, 1);

	dg_core_grid_assign_cell(_g, _c[28], 10, 0, 1, 7);
	dg_core_grid_assign_cell(_g, _c[29], 11, 0, 1, 7);
	dg_core_grid_assign_cell(_g, _c[30], 12, 0, 1, 7);
	dg_core_grid_assign_cell(_g, _c[31], 13, 0, 1, 7);
	
	dg_core_grid_assign_cell(_g, _c[32], 0, 7, 3, 1);
	dg_core_grid_assign_cell(_g, _c[33], 3, 7, 1, 1);

	dg_core_grid_assign_cell(_g, _c[34], 0,  8, 1, 1);
	dg_core_grid_assign_cell(_g, _c[35], 1,  8, 1, 1);
	dg_core_grid_assign_cell(_g, _c[36], 2,  8, 1, 1);
	dg_core_grid_assign_cell(_g, _c[37], 3,  8, 1, 1);
	dg_core_grid_assign_cell(_g, _c[38], 0,  9, 1, 1);
	dg_core_grid_assign_cell(_g, _c[39], 1,  9, 1, 1);
	dg_core_grid_assign_cell(_g, _c[40], 2,  9, 1, 1);
	dg_core_grid_assign_cell(_g, _c[41], 3,  9, 1, 1);
	dg_core_grid_assign_cell(_g, _c[42], 0, 10, 1, 1);
	dg_core_grid_assign_cell(_g, _c[43], 1, 10, 1, 1);
	dg_core_grid_assign_cell(_g, _c[44], 2, 10, 1, 1);
	dg_core_grid_assign_cell(_g, _c[45], 3, 10, 1, 1);
	dg_core_grid_assign_cell(_g, _c[46], 0, 11, 1, 1);
	dg_core_grid_assign_cell(_g, _c[47], 1, 11, 1, 1);
	dg_core_grid_assign_cell(_g, _c[48], 2, 11, 1, 1);
	dg_core_grid_assign_cell(_g, _c[49], 3, 11, 1, 1);
	
	dg_core_grid_assign_cell(_g, _c[50], 4,  7, 4, 1);
	dg_core_grid_assign_cell(_g, _c[51], 4,  8, 1, 4);
	dg_core_grid_assign_cell(_g, _c[52], 5,  8, 3, 3);
	dg_core_grid_assign_cell(_g, _c[53], 5, 11, 4, 1);
	dg_core_grid_assign_cell(_g, _c[54], 8,  7, 1, 4);
	
	dg_core_grid_assign_cell(_g, _c[55], 12, 7, 2, 5);
	dg_core_grid_assign_cell(_g, _c[56],  9, 7, 3, 5);

	/* window configuration */

	dg_core_window_push_grid(_w, _g);
	dg_core_window_rename(_w, "Cell showcase", NULL);
	dg_core_window_activate(_w);

	/* event loop */

	dg_core_loop_run();

	/* cleanup & end */

	dg_core_window_destroy(_w);
	dg_core_grid_destroy(_g);
	for (int i = 0; i < _N_C; i++) {
		dg_core_cell_destroy(_c[i]);
	}

	dg_base_reset();
	dg_core_reset();

	return 0;
}
