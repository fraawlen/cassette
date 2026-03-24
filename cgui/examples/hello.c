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

static void cl_close (cshell *, void *);
static void cl_open  (cshell *, void *);
static void cl_setup (cshell *, void *);
static void cl_task  (cshell *, void *);

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

int
main(void)
{
	cshell *sh = cshell_create();

	cshell_on_setup(sh, cl_setup, nullptr);
	cshell_on_open (sh, cl_open,  nullptr);
	cshell_open(sh, CSHELL_ANY, "hello");
	cshell_name(sh, "hello world !");
	cshell_wait(sh);

	cshell_post(sh, cl_task, &(int){1});
	cshell_post(sh, cl_task, &(int){2});
	cshell_post(sh, cl_task, &(int){3});
	cshell_join(sh);

	printf("done, errors: %s\n", cerr_name(cshell_error(sh)));
	cshell_destroy(sh);

	return 0;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
cl_close(cshell *sh, void *gr)
{
	(void)sh;

	printf("shell closed\n");

	cgrid_destroy(gr);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_open(cshell *sh, void *data)
{
	(void)data;

	printf("shell opened on backend %i\n", cshell_server(sh));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_setup(cshell *sh, void *data)
{
	(void)data;

	cgrid *gr = cgrid_create(1, 3);

	cgrid_resize_row(gr, 0, 20);
	cgrid_resize_col(gr, 0, 20);
	cgrid_resize_col(gr, 1, 20);
	cgrid_resize_col(gr, 2, 20);

	cshell_use_grid(sh, gr);
	cshell_on_close(sh, cl_close, gr);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
cl_task(cshell *sh, void *data)
{
	(void)sh;

	printf("executed task %i on UI thread\n", *(int *)data);
}
