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

#pragma once

#include <stdbool.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

typedef struct _string_t cobj_string_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

cobj_string_t *cobj_string_clone(const cobj_string_t *str);

cobj_string_t *cobj_string_create(void);

cobj_string_t *cobj_string_create_double(double d, int precision);

cobj_string_t *cobj_string_get_placeholder(void);

void cobj_string_destroy(cobj_string_t **str);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void cobj_string_cut(cobj_string_t *str, size_t offset, size_t n_codepoints);

void cobj_string_insert(cobj_string_t *str, const cobj_string_t *str_src, size_t offset);

void cobj_string_set(cobj_string_t *str, const cobj_string_t *str_src);

void cobj_string_wrap(cobj_string_t *str, size_t max_cols);

void cobj_string_realloc(cobj_string_t *str);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void cobj_string_append(cobj_string_t *str, const cobj_string_t *str_src);

void cobj_string_append_raw(cobj_string_t *str, const char *c_str);

void cobj_string_clear(cobj_string_t *str);

void cobj_string_insert_raw(cobj_string_t *str, const char *c_str, size_t offset);

void cobj_string_pad(cobj_string_t *str, const char *pattern, size_t offset, size_t n_codepoints_target);

void cobj_string_prepend(cobj_string_t *str, const cobj_string_t *str_src);

void cobj_string_prepend_raw(cobj_string_t *str, const char *c_str);

void cobj_string_set_raw(cobj_string_t *str, const char *c_str);

void cobj_string_slice(cobj_string_t *str, size_t offset, size_t n_codepoints);

void cobj_string_trim(cobj_string_t *str);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t cobj_string_convert_coords_to_offset(const cobj_string_t *str, size_t row, size_t col);

size_t cobj_string_convert_wrapped_offset(const cobj_string_t *str, const cobj_string_t *str_wrap, size_t offset);

size_t cobj_string_get_alloc_size(const cobj_string_t *str);

const char *cobj_string_get_chars(const cobj_string_t *str);

const char *cobj_string_get_chars_at_coords(const cobj_string_t *str, size_t row, size_t col);

const char *cobj_string_get_chars_at_offset(const cobj_string_t *str, size_t offset);

size_t cobj_string_get_height(const cobj_string_t *str);

size_t cobj_string_get_length(const cobj_string_t *str);

size_t cobj_string_get_width(const cobj_string_t *str);

bool cobj_string_has_failed(const cobj_string_t *str);

const char *cobj_string_seek_next_codepoint(const char *codepoint);

size_t cobj_string_test_wrap(const cobj_string_t *str, size_t max_cols);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif
