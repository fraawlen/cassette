/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Objects (COBJ) library.
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

#include <cassette/cobj.h>
#include <stdio.h>
#include <stdlib.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void        print_cl (struct ccolor cl);
static const char *to_str   (double val);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static struct ccolor cl_1;
static struct ccolor cl_2;
static struct ccolor cl_3;
static struct ccolor cl_4;

static cstr *str  = CSTR_PLACEHOLDER;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(void)
{
	str = cstr_create();

	cstr_set_precision(str, 0);

	cl_1 = CCOLOR_BLUE;
	cl_2 = ccolor_from_str("#FF0000", NULL);
	cl_3 = ccolor_from_rgba(128, 128, 128, 255);
	cl_4 = ccolor_interpolate(cl_1, cl_2, 0.5);

	print_cl(cl_1);
	print_cl(cl_2);
	print_cl(cl_3);
	print_cl(cl_4);
	
	if (cstr_error(str))
	{
		printf("String errored during operation\n");	
	}

	cstr_destroy(str);

	return 0;
}

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void 
print_cl(struct ccolor cl)
{
	printf("R = %s, ",to_str(cl.r));
	printf("G = %s, ",to_str(cl.g));
	printf("B = %s, ",to_str(cl.b));
	printf("A = %s\n",to_str(cl.a));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const char *
to_str(double val)
{
	cstr_clear(str);
	cstr_append(str, val * 255.0);
	cstr_pad(str, " ", 0, 4);

	return cstr_chars(str);
}
