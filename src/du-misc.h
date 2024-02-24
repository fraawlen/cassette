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
#include <stdio.h>
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
 * Puts into the given buffer the path of the home directory of the current user.
 *
 * @param buf : string buffer to write to
 * @param n   : size of the buffer
 *
 * @return : pointer to the begining of the buffer.
 */
char *du_misc_get_home_path(char *buf, size_t n);

/**
 * Gets an UNIX timestamp in microseconds.
 *
 * @return : self-explanatory
 */
unsigned long du_misc_get_time(void);

/**
 * Reads characters from the stream f and stores them into the buffer pointed to by buf. Reading stops after
 * an EOF, the numbers of characters added to the buffer is n - 1, or the read word ends. A word is an array
 * of adjacent non whitespace characters (space, tab or newline). Leading and trailing whitespaces are ignored
 * and not added to the buffer. If there are no words on a line, the first character of the buffer will just
 * be set to '\0'. End of lines can be detected with the parameter eol. Whitespaces or newlines within quotes
 * or double quotes are kept and are considered to be part of the word being read. If quotes needs to be part
 * of the word, wrap them with doube quotes. Inversely, double quotes can be wrapped in simple quotes.
 * Newline characters get replaced by the null character in the buffer.
 *
 * @param buf : string buffer to write to
 * @param n   : size of the buffer
 * @param f   : file stream to read from
 * @param eol : optional, if given set to true if a newline has been reached, set to false otherwhise
 *
 * @return : pointer to the begining of the buffer. If no byte has been read before EOF, NULL will be returned
 *           instead (note : buf's first char will still be set to '\0')
 */
char *du_misc_read_word(char *buf, size_t n, FILE *f, bool *eol);

/**
 * Check if a given environment variable exists and if its value is not an empty string.
 *
 * @param name : name of the environment variable to check
 *
 * @return : true if said variable is set to a non empty string, false otherwise
 */
bool du_misc_test_env(const char *name);

/**
 * Removes trailing and leading spaces, tabs and newlines from the given string.
 *
 * @param str : string to trim
 *
 * @return : NULL if the resulting string is of lenght 0 or if the input string is NULL, otherwise a pointer
 *           to the beginning of the modified string is returned
 */
char *du_misc_trim_str(char *str);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DU_MISC_H */
