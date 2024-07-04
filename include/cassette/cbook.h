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

#if __GNUC__ > 4
	#define CBOOK_NONNULL_RETURN __attribute__((returns_nonnull))
	#define CBOOK_NONNULL(...)   __attribute__((nonnull (__VA_ARGS__)))
	#define CBOOK_PURE           __attribute__((pure))
#else
	#define CBOOK_NONNULL_RETURN
	#define CBOOK_NONNULL(...)
	#define CBOOK_PURE
#endif

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 * Opaque book object instance. It stores an automatically extensible stack of strings. Strings can be
 * grouped.
 * This object holds an internal error bitfield that can be checked with cbook_error(). Some functions, upon
 * failure, can trigger specific error bits and will exit early without side effects that affect the contents
 * of the book (but the amount of allocated memory may be modified). If the error bitfield is set to anything
 * else than CBOOK_OK, any function that takes this object as an argument will return early with no side
 * effects and default return values. It is possible to repair the object to get rid of errors. See
 * cbook_repair() for more details.
 */
typedef struct cbook cbook;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Error types.
 */
enum cbook_err
{
	CBOOK_OK       = 0,
	CBOOK_INVALID  = 1,
	CBOOK_OVERFLOW = 1 << 1,
	CBOOK_MEMORY   = 1 << 2,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * String to group addition mode.
 */
enum cbook_group
{
	CBOOK_OLD = false,
	CBOOK_NEW = true,
};

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

/**
 * A macro that gives uninitialized books a non-NULL value that is safe to use with the book's related
 * functions. However, any function called with a handle set to this value will return early and without any
 * side effects.
 */
#define CBOOK_PLACEHOLDER &cbook_placeholder_instance

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Global book instance with the error state set to CBOOK_INVALID. This instance is only made available
 * to allow the static initialization of book pointers with the macro CBOOK_PLACEHOLDER.
 */
extern cbook cbook_placeholder_instance;

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

/**
 * Create a book instance and deep copy the contents of another book instance into it.
 *
 * @param book : Book to copy contents from
 *
 * @return     : New book instance
 * @return_err : CBOOK_PLACEHOLDER
 */
cbook *
cbook_clone(const cbook *book)
CBOOK_NONNULL_RETURN
CBOOK_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Creates an empty book instance.
 *
 * @return     : New book instance
 * @return_err : CBOOK_PLACEHOLDER
 */
cbook *
cbook_create(void)
CBOOK_NONNULL_RETURN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Destroys the given book and frees memory.
 *
 * @param book : Book to interact with
 */
void
cbook_destroy(cbook *book)
CBOOK_NONNULL(1);

/************************************************************************************************************/
/* PROCEDURES ***********************************************************************************************/
/************************************************************************************************************/

/**
 * Clears the contents of a given book. Allocated memory is not freed, use cbook_destroy() for that.
 *
 * @param book : Book to interact with
 */
void
cbook_clear(cbook *book)
CBOOK_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Deletes the last group of words. Allocated memory is not freed, use cbook_destroy() or cbook_trim() for
 * that.
 * 
 * @param book : Book to interact with
 */
void
cbook_pop_group(cbook *book)
CBOOK_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Deletes the last word. Allocated memory is not freed, use cbook_destroy() or cbook_trim() for that.
 * 
 * @param book : Book to interact with
 */
void
cbook_pop_word(cbook *book)
CBOOK_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Puts the iterator at the beginning of a group. Before accessing a word from the said group with
 * cbook_iteration(), cbook_iterate() should be called at least once. If the group_index parameter is
 * out-of-bounds, then this function behaves like cbook_lock_iterator(). Iterator-related functions are
 * intended to replace for-loops as they protect against out-of-bound errors and can adjust the iterator
 * position automatically when an element gets removed. They also maintain their own index internally, thus
 * dispensing the end-user from keeping and passing it around across multiple functions.
 *
 * Usage example :
 *
 *	cbook_init_iterator(book, group_index);
 *	while (cbook_iterate(book))
 *	{
 *		printf("%s\n", cbook_iteration(book));
 *	}
 * 
 * @param book        : Book to interact with
 * @param group_index : Group index within book
 */
void
cbook_init_iterator(cbook *book, size_t group_index)
CBOOK_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Increments the iterator's offset and makes available the next word returned by cbook_iteration(). This
 * function exits early and returns false if the iterator cannot be incremented because it has already reached
 * the end of the group it's set at or because the book has an error. Iterator-related functions are intended
 * to replace for-loops as they protect against out-of-bound errors and can adjust the iterator position
 * automatically when an element gets removed. They also maintain their own index internally, thus dispensing
 * the end-user from keeping and passing it around across multiple functions.
 * 
 * @param book : Book to interact with
 *
 * @return     : True if next word is accessible, false otherwhise
 * @return_err : False
 */
bool
cbook_iterate(cbook *book)
CBOOK_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Blocks the internal iterator so that cbook_iterate() will fail and return false until the iterator is reset
 * with cbook_init_iterator(). Iterator-related functions are intended to replace for-loops as they protect
 * against out-of-bound errors and can adjust the iterator position automatically when an element gets
 * removed. They also maintain their own index internally, thus dispensing the end-user from keeping and
 * passing it around across multiple functions.
 * 
 * @param book : Book to interact with
 */
void
cbook_lock_iterator(cbook *book)
CBOOK_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Preallocates a set number of characters, words, references, and groups to avoid triggering multiple
 * automatic reallocs when adding data to the book. This function has no effect if the requested numbers
 * are smaller than the previously allocated amounts.
 *
 * @param book          : Book to interact with
 * @param bytes_number  : Total number of bytes across all words
 * @param words_number  : Total number of words across all groups
 * @param groups_number : Total number of groups
 *
 * @error CBOOK_OVERFLOW : The size of the resulting book will be > SIZE_MAX
 * @error CBOOK_MEMORY   : Failed memory allocation
 */
void
cbook_prealloc(cbook *book, size_t bytes_number, size_t words_number, size_t groups_number)
CBOOK_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Increments the book word count (and possibly group count) by 1 and returns the start position of a buffer
 * to write to. The value of the length parameter defines the size of the returned buffer. The total char
 * count also gets incremented by the given length regardless of the buffer's usage. This function is
 * intended to be used instead of cbook_write() when reading bytes from a stream to avoid needing extra read
 * and write operations. Exceptionally, in case of failure (due to a memory issue or if the given book already
 * had an error, this function can return NULL. The caller is responsible for respecting the buffer's size
 * when writing to it.
 *
 * Example, instead of this :
 *
 *	char buf[128];
 *	if (fgets(buf, 128, stream))
 *	{
 *    	cbook_write(book, buf, CBOOK_NEW);
 *	}
 *
 * Do this :
 *
 *	char *buf;
 *	if ((buf = cbook_prepare_word(book, 128, CBOOK_NEW)))
 *	{
 *		fgets(buf, 128, stream);
 *	}
 * 
 * @param book       : Book to interact with
 * @param length     : Number of bytes to allocate to buffer
 * @param group_mode : Create (or not) a group for the new word
 *
 * @return     : Pointer to string buffer of size 'length'
 * @return_err : NULL
 *
 * @error CBOOK_OVERFLOW : The size of the resulting book will be > SIZE_MAX
 * @error CBOOK_MEMORY   : Failed memory allocation
 */
char *
cbook_prepare_word(cbook *book, size_t length, enum cbook_group group_mode)
CBOOK_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Clears errors and puts the book back into an usable state. The only unrecoverable error is CBOOK_INVALID.
 *
 * @param book : Book to interact with
 *
 */
void
cbook_repair(cbook *book)
CBOOK_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Appends a new string (called 'word') to the book and increments the book word count (and possibly group
 * count) by 1 as well as the character count by the string's length (NULL terminator included). The book will
 * automatically extend its allocated memory to accommodate the new word.
 * 
 * @param book       : Book to interact with
 * @param raw_str    : C string
 * @param group_mode : Create (or not) a group for the new word
 *
 * @error CBOOK_OVERFLOW : The size of the resulting book will be > SIZE_MAX
 * @error CBOOK_MEMORY   : Failed memory allocation
 */
void
cbook_write(cbook *book, const char *str, enum cbook_group group_mode)
CBOOK_NONNULL(1, 2);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Similar to cbook_clear() but all of the allocated memory is also zeroed.
 * 
 * @param book : Book to interact with
 */
void
cbook_zero(cbook *book)
CBOOK_NONNULL(1);

/************************************************************************************************************/
/* FUNCTIONS ************************************************************************************************/
/************************************************************************************************************/

/**
 * Gets the total length of the book (all NULL terminators included).
 *
 * @param book : Book to interact with
 *
 * @return     : Number of bytes
 * @return_err : 0
 */
size_t
cbook_byte_length(const cbook *book)
CBOOK_NONNULL(1)
CBOOK_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets the errror state.
 *
 * @param book : Book to interact with
 *
 * @return : Error bitfield
 */
enum cbook_err
cbook_error(const cbook *book)
CBOOK_NONNULL(1)
CBOOK_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets a group's word count. If group_index is out of bounds, the default return_err value is returned.
 * 
 * @param book        : Book to interact with
 * @param group_index : Group index within book
 *
 * @return     : Number of words
 * @return_err : 0
 */
size_t
cbook_group_length(const cbook *book, size_t group_index)
CBOOK_NONNULL(1)
CBOOK_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets the total number of groups.
 * 
 * @param book : Book to interact with
 *
 * @return     : Number of groups
 * @return_err : 0
 */
size_t
cbook_groups_number(const cbook *book)
CBOOK_NONNULL(1)
CBOOK_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets the word the iterator points to.  If the iterator is locked, not initialised or hasn't been
 * incremented once after the last initialisation, return_err is returned.
 * 
 * @param book : Book to interact with
 *
 * @return     : C string
 * @return_err : "\0"
 */
const char *
cbook_iteration(const cbook *book)
CBOOK_NONNULL_RETURN
CBOOK_NONNULL(1)
CBOOK_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets the group the iterator is set to.  If the iterator is locked, not initialised or hasn't been
 * incremented once after the last initialisation, return_err is returned.
 * 
 * @param book : Book to interact with
 *
 * @return     : Group index
 * @return_err : SIZE_MAX
 */
size_t
cbook_iterator_group(const cbook *book)
CBOOK_NONNULL(1)
CBOOK_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets the number of time the iterator has been incremented since its last initialisation.
 * 
 * @param book : Book to interact with
 *
 * @return     : Word offset (starts at 1)
 * @return_err : 0
 */
size_t
cbook_iterator_offset(const cbook *book)
CBOOK_NONNULL(1)
CBOOK_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets a word. If word_index is out of bounds, the default return_err value is returned.
 * 
 * @param book       : Book to interact with
 * @param word_index : Word index in book across all groups
 *
 * @return     : C string
 * @return_err : "\0"
 */
const char *
cbook_word(const cbook *book, size_t word_index)
CBOOK_NONNULL_RETURN
CBOOK_NONNULL(1)
CBOOK_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets a word from a specific group. If group_index or word_index are out of bounds, the default return_err
 * value is returned.
 * 
 * @param book             : Book to interact with
 * @param group_index      : Group index within book
 * @param word_local_index : Word index within group
 *
 * @return     : C string
 * @return_err : "\0"
 */
const char *
cbook_word_in_group(const cbook *book, size_t group_index, size_t word_local_index)
CBOOK_NONNULL_RETURN
CBOOK_NONNULL(1)
CBOOK_PURE;

/**
 * Converts a group + local word indexes to a book-wide word index. If group_index or word_index are out of
 * bounds, the default return_err value is returned.
 *
 * @param book             : Book to interact with
 * @param group_index      : Group index within book
 * @param word_local_index : Word index within group
 * 
 * @return     : Word index
 * @return_err : 0
 */
size_t
cbook_word_index(const cbook *book, size_t group_index, size_t word_local_index)
CBOOK_NONNULL(1)
CBOOK_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets the total number of words.
 * 
 * @param book : Book to interact with
 *
 * @return     : Total number of words across all groups
 * @return_err : 0
 */
size_t
cbook_words_number(const cbook *book)
CBOOK_NONNULL(1)
CBOOK_PURE;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif
