/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Graphics (CGUI) library.
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

#include <stdio.h>

#include <cassette/cgui.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * Standard hello world example.
 */

 int
 main(int argc, char **argv)
 {
	cgui_cell_t *cell;

	/* setup */

	cgui_init(argc, argv);

	/* object instantiation */

	cell = cgui_cell_create();

	/* cell setup */

	cgui_cell_enable(cell);

	/* run */

	cgui_run();

	/* end */

	if (cgui_has_failed())
	{
		printf("gui has failed during operation.\n");
	}

	cgui_cell_destroy(&cell);

	cgui_reset();

	return 0;
 }
