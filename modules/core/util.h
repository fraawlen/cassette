/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Graphics (DG) GUI library.
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

#ifndef DG_CORE_UTIL_H
#define DG_CORE_UTIL_H

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define DG_CORE_UTIL_DICT_LEN(X)     (sizeof(X) / sizeof(dg_core_util_dict_t))
#define DG_CORE_UTIL_FAT_DICT_LEN(X) (sizeof(X) / sizeof(dg_core_util_fat_dict_t))

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Dictionary element. An array of those can be used as a dictionary.
 *
 * @param key   : key string definition
 * @param value : value associated to key
 */
typedef struct {
	char *key;
	int value;
} dg_core_util_dict_t;

/**
 * Dictionary element. An array of those can be used as a dictionary. This variant includes a group value to
 * match a key-value pair to a specific group of resources.
 *
 * @param key   : key string definition
 * @param value : value associated to key
 * @param group : group associated to key
 */
typedef struct {
	char *key;
	int value; 
	int group;
} dg_core_util_fat_dict_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Converts an fp16.16 variable to int16_t with bound checking.
 *
 * @param f : value to convert
 *
 * @return : self-explanatory
 */
int16_t dg_core_util_convert_fp1616_to_int16(int32_t f);

/**
 * Searches for a matching string def in the given dictionary dg_core_util_dict_t array. Case and blank
 * sensistive. If no match is found, the target parameter is not modified.
 *
 * @param dict   : dictionary array to search through
 * @param dict_n : dictionary array lenght
 * @param key    : key string to match
 * @param target : optional, pointer of the value to set by searching the dictionary
 *
 * @return : true if a match is found, false otherwhise
 */
bool dg_core_util_dict_match(const dg_core_util_dict_t *dict, size_t dict_n, const char *key, int *target);

/**
 * Searches for a ref in the given dictionary dg_core_util_dict_t array.
 *
 * @param dict   : dictionary array to search through
 * @param dict_n : dictionary array lenght
 * @param value  : reference value to match
 *
 * @return : matching string def, if there is none then NULL
 */
const char *dg_core_util_dict_inverse_match(const dg_core_util_dict_t *dict, size_t dict_n, int value);

/**
 * Gets an UNIX timestamp in microseconds.
 *
 * @return : self-explanatory
 */
unsigned long dg_core_util_get_time(void);

/**
 * Checks if the coordinates px_test adn py_test are within a reclangle bound defined by px, py, pw and ph.
 * Negative width and height are allowed (results in a rectangle "facing the other side". A width or height
 * of 0 will always return false.
 *
 * @param x_test  : x coordinate of the point to test
 * @param y_test  : y coordinate of the point to test
 * @param x_bound : x coordinate of the top-left corner of the bounding box
 * @param y_bound : y coordinate of the top-left corner of the bounding box
 * @param w_bound : width  of the bounding box
 * @param h_bound : height of the bounding box
 *
 * @return : true if they are whithin, false otherwhise
 */
bool dg_core_util_test_bounds(int16_t x_test,  int16_t y_test, int16_t x_bound, int16_t y_bound,
                              int16_t w_bound, int16_t h_bound);

/**
 * Check if a given environment variable exists and if its value is not an empty string.
 *
 * @param name : name of the environment variable to check
 *
 * @return : true if said variable is set to a non empty string, false otherwise
 */
bool dg_core_util_test_env(const char *name);

/**
 * Removes trailing and leading spaces and tabs from a string.
 *
 * @param str : string to trim
 *
 * @return : NULL if the resulting string is of lenght 0 or if the input string is NULL, otherwise a pointer
 *           to the modified string is returned
 */
char *dg_core_util_trim_str(char *str);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DG_CORE_UTIL_H */
