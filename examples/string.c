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

static void insert_2d   (const char *text, size_t row, size_t col);
static void update_wrap (void);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static cstr *str_ref  = CSTR_PLACEHOLDER;
static cstr *str_wrap = CSTR_PLACEHOLDER;

static size_t columns = 10; /* size of a supposed widget */

/************************************************************************************************************/
/* MAIN *****************************************************************************************************/
/************************************************************************************************************/

/**
 * This example demonstrates the use of the cstr string objects in an interactable pseudo GUI widget that
 * displays text in monospace. The text is wrapped automatically to fit the width of the widget.
 * For this, two strings are needed. The first one, _str_ref, will hold the text data in its original form,
 * as input by an end-user or set by the application developer without any extra newlines. The second string,
 * _str_wrap, will be a wrapped version of the first string that can fit inside the widget. The first string
 * gets updated only when its content changes, while the second one updates when either the contents of the
 * first one change or if the widget gets resized.
 *
 *
 *       original text                   widget text
 *        (_str_ref)                     (_str_wrap)
 *
 *                                      ┏━━━━━━━━━━━┓
 *                                      ┃ Hello Wor ┃
 *       "Hello World!"       -->       ┃ ld!       ┃ 
 *                                      ┃           ┃
 *                                      ┗━━━━━━━━━━━┛
 */

int
main(void)
{
	/* Setup */

	str_ref  = cstr_create();
	str_wrap = cstr_create();

	cstr_append(str_ref, "This is a lͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲͲng line Ͳf text!");
	update_wrap();

	/* We assume the end-user wrote 'NEW' into the widget at the 3rd row and 6th column          */
	/* (index 2 and 5). Once the new text has been added to the reference string, the wrapped    */
	/* string is updated to display the changes.                                                 */

	insert_2d("NEW", 2, 5);
	update_wrap();

	/* We now assume that the widget's width changes. The reference string is unchanged, but the */
	/* wrapped string needs to be updated to use of all the new available space.                 */

	columns = 25;
	update_wrap();

	/* End */

	if (cstr_error(str_ref))
	{
		printf("Reference string errored during operation\n");
	}

	if (cstr_error(str_wrap))
	{
		printf("Wrapped string errored during operation\n");
	}

	cstr_destroy(str_ref);
	cstr_destroy(str_wrap);

	return 0;
}

/************************************************************************************************************/
/* STATIC ***************************************************************************************************/
/************************************************************************************************************/

/**
 * Because the wrapped string is shown to the end-user, it's also the one being interacted with. This
 * function will first get the codepoint offset that matches the insertion point at the given coordinates of
 * the wrapped string. Then, this offset gets converted into an 'unwrapped' offset that can be used with the
 * reference string. New data can then be added to the reference string that will match the 2d coordinates of
 * the wrapped string.
 */

static void
insert_2d(const char *text, size_t row, size_t col)
{
	size_t offset_ref;
	size_t offset_wrap;

	offset_wrap = cstr_coords_offset(str_wrap, row, col);
	offset_ref  = cstr_unwrapped_offset(str_ref, str_wrap, offset_wrap);

	cstr_insert(str_ref, text, offset_ref);	
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
update_wrap()
{
	cstr_clear(str_wrap);
	cstr_append(str_wrap, str_ref);
	cstr_wrap(str_wrap, columns);

	printf(
		"%s\n\t-> %zu rows x %zu cols / %zu utf8-characters / %zu bytes\n\n",
		cstr_chars(str_wrap),
		cstr_height(str_wrap),
		cstr_width(str_wrap),
		cstr_length(str_wrap),
		cstr_byte_length(str_wrap));
}
