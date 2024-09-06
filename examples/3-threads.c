/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Configuration (CCFG) library.
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

#include <cassette/ccfg.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define N_SIMULATIONS 10

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void *simulation_thread (void *param);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static const char *data =
	"SEED ($$ sim_id)\n"
	"example-3 coordinates (RAND 10 90) (RAND 10 90) (RAND 10 90)";

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * In this 3rd example, we are mimicking a multithreaded software simulator. Each simulation sits in its own
 * thread and requires the same variables to work with (in this example, the coordinates of something).
 * However, each simulation instance needs randomized coordinate values to produce a different output.
 *
 * In this scenario, each simulation thread sets up its own configuration and provides a parameter "sim_id"
 * with a unique value. This parameter is then used as a LCG seed. Thanks to that, during parsing, 'RAND'
 * tokens are substituted with different values in each thread. The parser exclusively opens the source files
 * in read-only mode, making multithreaded access to the same source safe.
 */

int
main(void)
{
	pthread_t threads[N_SIMULATIONS];
	unsigned int ids[N_SIMULATIONS];

	/* Run pseudo-simulations */

	for (unsigned int i = 0; i < N_SIMULATIONS; i++)
	{
		ids[i] = i;
		pthread_create(threads + i, NULL, simulation_thread, ids + i);
	}
	
	for (unsigned int i = 0; i < N_SIMULATIONS; i++)
	{
		pthread_join(threads[i], NULL);
	}

	return 0;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void *
simulation_thread(void *param)
{
	ccfg *cfg;

	unsigned int coords[3] = {0};
	unsigned int id;

	/* Simulation config setup */

	cfg = ccfg_create();
	id  = *(unsigned int*)param;

	ccfg_push_param(cfg, "sim_id", id);
	ccfg_load_internal(cfg, data);

	ccfg_fetch(cfg, "example-3", "coordinates");
	for (unsigned int i = 0; i < 3 && ccfg_iterate(cfg); i++)
	{
		coords[i] = strtoul(ccfg_resource(cfg), NULL, 0);
	}

	/* Simulator algorithm */

	printf(
		"sim %u -> x = %u, y = %u, z = %u\n",
		id,
		coords[0],
		coords[1],
		coords[2]);

	/* Simulation end */

	ccfg_destroy(cfg);

	pthread_exit(NULL);
}
