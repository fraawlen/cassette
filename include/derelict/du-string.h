/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Utilities (DU) library.
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

#ifndef DU_STRING_H
#define DU_STRING_H

#include <stdbool.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

typedef struct _string_t du_string_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

du_string_t *du_string_create(void);

du_string_t *du_string_create_double(double d, int precision);

du_string_t *du_string_create_duplicate(const du_string_t *str);

void du_string_destroy(du_string_t **str);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void du_string_cut(du_string_t *str, size_t offset, size_t n_codepoints);

void du_string_insert(du_string_t *str, const du_string_t *str_src, size_t offset);

void du_string_set(du_string_t *str, const du_string_t *str_src);

void du_string_wrap(du_string_t *str, size_t max_cols);

void du_string_realloc(du_string_t *str);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void du_string_append(du_string_t *str, const du_string_t *str_src);

void du_string_append_raw(du_string_t *str, const char *c_str);

void du_string_clear(du_string_t *str);

void du_string_insert_raw(du_string_t *str, const char *c_str, size_t offset);

void du_string_pad(du_string_t *str, const char *pattern, size_t offset, size_t n_codepoints_target);

void du_string_prepend(du_string_t *str, const du_string_t *str_src);

void du_string_prepend_raw(du_string_t *str, const char *c_str);

void du_string_set_raw(du_string_t *str, const char *c_str);

void du_string_slice(du_string_t *str, size_t offset, size_t n_codepoints);

void du_string_trim(du_string_t *str);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t du_string_convert_coords_to_offset(const du_string_t *str, size_t row, size_t col);

size_t du_string_convert_wrapped_offset(const du_string_t *str, const du_string_t *str_wrap, size_t offset);

size_t du_string_get_alloc_size(const du_string_t *str);

const char *du_string_get_chars(const du_string_t *str);

const char *du_string_get_chars_at_coords(const du_string_t *str, size_t row, size_t col);

const char *du_string_get_chars_at_offset(const du_string_t *str, size_t offset);

size_t du_string_get_height(const du_string_t *str);

size_t du_string_get_length(const du_string_t *str);

size_t du_string_get_width(const du_string_t *str);

bool du_string_has_failed(const du_string_t *str);

const char *du_string_seek_next_codepoint(const char *codepoint);

size_t du_string_test_wrap(const du_string_t *str, size_t max_cols);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DU_STRING_H */
