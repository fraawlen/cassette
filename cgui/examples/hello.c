/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cassette/cobj.h>
#include <cassette/cgui.h>
#include <pthread.h>
#include <semaphore.h>
#include <stdio.h>
#include <unistd.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void  task   (cshell *, void *);
static void  task2  (cshell *, void *);
static void  task3  (cshell *, void *);
static void  task4  (cshell *, void *);
static void *thread (void   *);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static sem_t sem;

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

int
main(void)
{
	pthread_t tr;
	cshell *sh;

	/* Instantiation */

	sh = cshell_create();
	
	pthread_create(&tr, nullptr, thread, sh);

	/* Setup */

	cshell_on_close(sh, task2, nullptr);
	cshell_on_open(sh,  task4, nullptr);
	cshell_open(sh);

	/* Task */

	for (int i = 0; i < 10; i++)
	{
		sem_init(&sem, 0, 0);
		cshell_invoke(sh, task, &i);
		if (cshell_error(sh) == CERR_NONE)
		{
			sem_wait(&sem);
			sem_destroy(&sem);
		}
	}

	/* End & cleanup */

	cshell_join(sh);
	pthread_join(tr, nullptr);

	if (cshell_error(sh))
	{
		printf("Gui has failed during operation (%s).\n", cerr_name(cshell_error(sh)));
	}

	cshell_destroy(sh);

	return 0;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

static void
task(cshell *sh, void *data)
{
	(void)sh;

	printf("task %i in event thread\n", *(int*)data);

	sem_post(&sem);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
task2(cshell *sh, void *data)
{
	(void)data;
	(void)sh;

	printf("shell closed\n");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
task3(cshell *sh, void *data)
{
	(void)data;
	(void)sh;

	printf("executed special task\n");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
task4(cshell *sh, void *data)
{
	(void)data;

	printf("opened shell on %s\n", cshell_backend(sh) == CSHELL_X11 ? "x11" : "wayland");
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void *
thread(void *arg)
{
	cshell *sh = (cshell *)arg;

	printf("starting secondary work thread\n");

	cshell_wait(sh);
	cshell_invoke(sh, task3, nullptr);
	cshell_invoke(sh, task3, nullptr);
	cshell_invoke(sh, task3, nullptr);
	cshell_join(sh);

	printf("exiting secondary work thread\n");

	pthread_exit(nullptr);
}
