#include <stdlib.h>

#include <dg/core/core.h>
#include <dg/base/base.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static dg_core_window_t *_w = NULL;
static dg_core_grid_t   *_g = NULL;
static dg_core_cell_t   *_c = NULL;

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
	_g = dg_core_grid_create(1, 1);
	_c = dg_base_label_create();

	/* cell configuration */

	dg_base_label_set_label(_c, "Hello World !");
	dg_base_label_set_origin(_c, DG_BASE_ORIGIN_CENTER);

	/* grid configuration */

	dg_core_grid_set_column_width(_g, 0, 13);
	dg_core_grid_set_column_growth(_g, 0, 1.0);
	dg_core_grid_set_row_height(_g, 0, 1);
	dg_core_grid_set_row_growth(_g, 0, 1.0);
	dg_core_grid_assign_cell(_g, _c,  0, 0, 1, 1);

	/* window configuration */

	dg_core_window_push_grid(_w, _g);
	dg_core_window_set_extra_size(_w, 10, 5);
	dg_core_window_rename(_w, "Hello", NULL);
	dg_core_window_activate(_w);

	/* event loop */

	dg_core_loop_run();

	/* cleanup & end */

	dg_core_window_destroy(_w);
	dg_core_grid_destroy(_g);
	dg_core_cell_destroy(_c);

	dg_base_reset();
	dg_core_reset();

	return 0;
}
