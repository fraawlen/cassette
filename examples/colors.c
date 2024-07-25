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

static void         _print_cl (struct ccolor cl);
static const char * _to_str   (double val);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static struct ccolor _cl_1;
static struct ccolor _cl_2;
static struct ccolor _cl_3;
static struct ccolor _cl_4;

static cstr *_str  = CSTR_PLACEHOLDER;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(void)
{
	_str = cstr_create();

	cstr_set_precision(_str, 0);

	_cl_1 = CCOLOR_BLUE;
	_cl_2 = ccolor_from_str("#FF0000", NULL);
	_cl_3 = ccolor_from_rgba(128, 128, 128, 255);
	_cl_4 = ccolor_interpolate(_cl_1, _cl_2, 0.5);

	_print_cl(_cl_1);
	_print_cl(_cl_2);
	_print_cl(_cl_3);
	_print_cl(_cl_4);
	
	if (cstr_error(_str))
	{
		printf("String errored during operation\n");	
	}

	cstr_destroy(_str);

	return 0;
}

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void 
_print_cl(struct ccolor cl)
{
	printf("R = %s, ",_to_str(cl.r));
	printf("G = %s, ",_to_str(cl.g));
	printf("B = %s, ",_to_str(cl.b));
	printf("A = %s\n",_to_str(cl.a));
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const char *
_to_str(double val)
{
	cstr_clear(_str);
	cstr_append(_str, val * 255.0);
	cstr_pad(_str, " ", 0, 4);

	return cstr_chars(_str);
}
