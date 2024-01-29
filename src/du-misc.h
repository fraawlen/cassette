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

#ifndef DU_MISC_H
#define DU_MISC_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * Converts an fp16.16 variable to int16_t with bound checking.
 *
 * @param f : value to convert
 *
 * @return : self-explanatory
 */
int16_t du_misc_convert_fp1616_to_int16(int32_t f);

/**
 * Gets an UNIX timestamp in microseconds.
 *
 * @return : self-explanatory
 */
unsigned long du_misc_get_time(void);

/**
 * Check if a given environment variable exists and if its value is not an empty string.
 *
 * @param name : name of the environment variable to check
 *
 * @return : true if said variable is set to a non empty string, false otherwise
 */
bool du_misc_test_env(const char *name);

/**
 * Removes trailing and leading spaces and tabs from a string.
 *
 * @param str : string to trim
 *
 * @return : NULL if the resulting string is of lenght 0 or if the input string is NULL, otherwise a pointer
 *           to the modified string is returned
 */
char *du_misc_trim_str(char *str);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DU_MISC_H */
