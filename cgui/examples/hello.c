/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cassette/cobj.h>
#include <cassette/cgui.h>
#include <stdio.h>
#include <unistd.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct app
{
	cgrid *gr;
	ccell *gap1;
	ccell *gap2;
	ccell *gap3;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void cb_close (cshell *, void *);
static void cb_open  (cshell *, void *);
static void cb_setup (cshell *, void *);
static void cb_task  (cshell *, void *);

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

int
main(void)
{
	struct app app = {0};
	cshell *sh = cshell_create();

	cshell_on_close(sh, cb_close, &app);
	cshell_on_setup(sh, cb_setup, &app);
	cshell_on_open (sh, cb_open,  &app);
	cshell_open(sh, CSHELL_ANY, "hello");
	cshell_name(sh, "hello world !");
	cshell_wait(sh);

	cshell_post(sh, cb_task, &(int){1});
	cshell_post(sh, cb_task, &(int){2});
	cshell_post(sh, cb_task, &(int){3});

	cshell_join(sh);
	cshell_destroy(sh);

	return 0;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
cb_close(cshell *sh, void *data)
{
	struct app *app = data;

	(void)sh;

	cgrid_destroy(app->gr);
	cgap_destroy(app->gap1);
	cgap_destroy(app->gap2);
	cgap_destroy(app->gap3);
	
	printf("shell closed\n");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cb_open(cshell *sh, void *data)
{
	(void)data;

	printf("shell opened on backend %i\n", cshell_server(sh));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cb_setup(cshell *sh, void *data)
{
	struct app *app = data;

	app->gr   = cgrid_create(3, 1);
	app->gap1 = cgap_create();
	app->gap2 = cgap_create();
	app->gap3 = cgap_create();

//	cgrid_flex_row(app->gr, 0, 1.0);
//	cgrid_flex_col(app->gr, 0, 1.0);
	cgrid_flex_col(app->gr, 1, 1.0);
//	cgrid_flex_col(app->gr, 2, 1.0);

	cgrid_resize_row(app->gr, 0, 20);
	cgrid_resize_col(app->gr, 0, 20);
	cgrid_resize_col(app->gr, 1, 20);
	cgrid_resize_col(app->gr, 2, 20);

	cgrid_assign_cell(app->gr, app->gap1, 0, 0, 0, 1, 1);
	cgrid_assign_cell(app->gr, app->gap2, 0, 1, 0, 1, 1);
	cgrid_assign_cell(app->gr, app->gap3, 0, 2, 0, 1, 1);

	cshell_use_grid(sh, app->gr);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cb_task(cshell *sh, void *data)
{
	(void)sh;

	printf("executed task %i on UI thread\n", *(int *)data);
}
