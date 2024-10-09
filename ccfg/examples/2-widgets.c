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
#include <cassette/cobj.h>
#include <stdio.h>
#include <stdlib.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct widget
{
	const char *name;
	unsigned long border_width;
	struct ccolor border_color;
	struct ccolor background_color;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void widget_config (struct widget *w);
static void widget_print  (const struct widget *w);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static ccfg *cfg = CCFG_PLACEHOLDER;

struct widget label  = {.name = "label",  .border_width = 0, .border_color = {0} , .background_color = {0}};
struct widget button = {.name = "button", .border_width = 0, .border_color = {0} , .background_color = {0}};
struct widget gauge  = {.name = "gauge",  .border_width = 0, .border_color = {0} , .background_color = {0}};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static const char *data =
	"LET widget  \\\n"
	"	label  \\\n"
	"	button \\\n"
	"	gauge  \n"
	"\n"
	"label  background_color #126DD0\n"
	"button background_color #A155CC\n"
	"gauge  background_color #111111\n"
	"\n"
	"FOR_EACH widget\n"
	"	(% widget) border_width 5\n"
	"	(% widget) border_color #050505\n"
	"FOR_END";

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

/**
 * In this 2nd example, we query three resources (background colour, border colour, and border width) for
 * four widget types: label, button, switch, and gauge. Unlike the 1st example, the queried resources must be
 * converted into the correct data type. If a resource is not found, hardcoded default values will be used.
 *
 * This example demonstrates how to use the FOR_EACH iteration sequences within the configuration file. This
 * iteration sets the same border width and colour for all widgets.
 */

int
main(void)
{
	/* Setup */

	cfg = ccfg_create();

	/* Operations */

	ccfg_load_internal(cfg, data);

	widget_config(&label);
	widget_config(&button);
	widget_config(&gauge);

	widget_print(&label);
	widget_print(&button);
	widget_print(&gauge);

	/* End */

	if (ccfg_error(cfg))
	{
		printf("Configuration parser failed during operation.\n");
	}

	return 0;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
widget_config(struct widget *w)
{
	/* background color */

	ccfg_fetch(cfg, w->name, "background_color");
	if (ccfg_iterate(cfg))
	{
		w->background_color = ccolor_from_str(ccfg_resource(cfg), NULL);
	}

	/* border color */

	ccfg_fetch(cfg, w->name, "border_color");
	if (ccfg_iterate(cfg))
	{
		w->border_color = ccolor_from_str(ccfg_resource(cfg), NULL);
	}

	/* border width */
	
	ccfg_fetch(cfg, w->name, "border_width");
	if (ccfg_iterate(cfg))
	{
		w->border_width = strtoul(ccfg_resource(cfg), NULL, 0);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
widget_print(const struct widget *w)
{
	printf("%s:\n", w->name);

	printf(
		"\tbackground_color (r,g,b) : %u, %u, %u\n",
		(unsigned int)(w->background_color.r * 255),
		(unsigned int)(w->background_color.g * 255),
		(unsigned int)(w->background_color.b * 255));

	printf(
		"\tbackground_color (r,g,b) : %u, %u, %u\n",
		(unsigned int)(w->border_color.r * 255),
		(unsigned int)(w->border_color.g * 255),
		(unsigned int)(w->border_color.b * 255));

	printf("\tborder_width : %lu\n\n", w->border_width);
}

