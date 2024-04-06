/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Objects (DO) library.
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

#ifndef DO_STRING_H
#define DO_STRING_H

#include <stdbool.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

typedef struct _string_t do_string_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

do_string_t *do_string_create(void);

do_string_t *do_string_create_double(double d, int precision);

do_string_t *do_string_create_duplicate(const do_string_t *str);

void do_string_destroy(do_string_t **str);

do_string_t *do_string_get_placeholder(void);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void do_string_cut(do_string_t *str, size_t offset, size_t n_codepoints);

void do_string_insert(do_string_t *str, const do_string_t *str_src, size_t offset);

void do_string_set(do_string_t *str, const do_string_t *str_src);

void do_string_wrap(do_string_t *str, size_t max_cols);

void do_string_realloc(do_string_t *str);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void do_string_append(do_string_t *str, const do_string_t *str_src);

void do_string_append_raw(do_string_t *str, const char *c_str);

void do_string_clear(do_string_t *str);

void do_string_insert_raw(do_string_t *str, const char *c_str, size_t offset);

void do_string_pad(do_string_t *str, const char *pattern, size_t offset, size_t n_codepoints_target);

void do_string_prepend(do_string_t *str, const do_string_t *str_src);

void do_string_prepend_raw(do_string_t *str, const char *c_str);

void do_string_set_raw(do_string_t *str, const char *c_str);

void do_string_slice(do_string_t *str, size_t offset, size_t n_codepoints);

void do_string_trim(do_string_t *str);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t do_string_convert_coords_to_offset(const do_string_t *str, size_t row, size_t col);

size_t do_string_convert_wrapped_offset(const do_string_t *str, const do_string_t *str_wrap, size_t offset);

size_t do_string_get_alloc_size(const do_string_t *str);

const char *do_string_get_chars(const do_string_t *str);

const char *do_string_get_chars_at_coords(const do_string_t *str, size_t row, size_t col);

const char *do_string_get_chars_at_offset(const do_string_t *str, size_t offset);

size_t do_string_get_height(const do_string_t *str);

size_t do_string_get_length(const do_string_t *str);

size_t do_string_get_width(const do_string_t *str);

bool do_string_has_failed(const do_string_t *str);

const char *do_string_seek_next_codepoint(const char *codepoint);

size_t do_string_test_wrap(const do_string_t *str, size_t max_cols);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DO_STRING_H */
