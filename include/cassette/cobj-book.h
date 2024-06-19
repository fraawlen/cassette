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

/**
 * Opaque book instance object. A book is a fancy dynamic string array / vector that can autoresize itself to
 * accomodate more words (aka C-strings). Words can be grouped to create sub-string-arrays. All words are
 * stored in a single continuous block of memory. There can't be more groups than words.
 * This object holds an internal fail state boolean that can be checked with cobj_book_has_failed(). If it
 * happens to be put in a failure state due to a memory failure, any function that take this object as
 * argument will exit early with no side effects and return default values. The only 2 functions that are an
 * exception to this rule are cobj_book_destroy() and cobj_book_has_failed().
 */
typedef struct _book_t cobj_book_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * When a new word gets appended to a given book, the following enumeration values determines if said word
 * will be part of a new group, or be added to the last active word group.
 */
enum cobj_book_group_mode_t
{
	COBJ_BOOK_OLD_GROUP = false,
	COBJ_BOOK_NEW_GROUP = true,
};

typedef enum cobj_book_group_mode_t cobj_book_group_mode_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Allocates memory and initializes a book instance.
 * This function always returns a valid and safe-to-use or destroy object instance. Even in the case of memory
 * allocation failure, the returned value points to an internal static book instance set in a failed state.
 * Therefore, checking for a NULL returned value is useless, instead, use cobj_book_has_failed(). Never free()
 * an object obtained with this function, instead use cobj_book_destroy().
 *
 * @param n_alloc Number of word slots to preallocate inside the newly created book, can be 0, since the book
 *                can auto-extend its size as needed.
 * @param word_n Maximum byte size of an individual word
 *
 * @return Created book instance object
 */
cobj_book_t *cobj_book_create(size_t n_alloc, size_t word_n);

/**
 * Gets a valid pointer to an internal book instance set in a failed state. To be used to avoid
 * leaving around uninitialized book instance pointers. Never free() an object obtained with this
 * function, instead use cobj_book_destroy().
 *
 * @return Placeholder book instance object
 */
cobj_book_t *cobj_book_get_placeholder(void);

/**
 * Destroys a given instance and free allocated memory. The pointed value is then replaced by a placeholder
 * value that points to an internal static configuration instance set in a failed state to avoid leaving
 * behind a dangling pointer. Hence, it is safe to call this function multiple times.
 *
 * @param book Book instance to interact with
 */
void cobj_book_destroy(cobj_book_t **book);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Clears the contents of a given book. However no memory is freed, use cobj_book_destroy() for that.
 *
 * @param book Book instance to interact with
 */
void cobj_book_clear(cobj_book_t *book);

/**
 * Removes the last group and words associated with it.
 *
 * @param book Book instance to interact with
 */
void cobj_book_erase_last_group(cobj_book_t *book);

/**
 * Removes the last word. If that word was the first in its group, the group will be deleted too.
 *
 * @param book Book instance to interact with
 */
void cobj_book_erase_last_word(cobj_book_t *book);

/**
 * After the book's iterator has been reset with cobj_book_reset_iterator(), the words returned by
 * cobj_book_get_iteration() are not accessible until this function is called at least once. Each subsequent
 * call to this function will move an internal iterator forward to the next word until the end of the group is
 * reached. If the iterator has been locked with cobj_book_lock_iterator() this function has no effect.
 * Iterator-related functions are intended to replace for-loops as they protect against out-of-bound errors
 * and can adjust the iterator position automatically when an element gets removed.
 *
 * Usage example :
 *
 *	cobj_book_reset_iterator(book, group_index);
 *	while (cobj_book_increment_iterator(book))
 *	{
 *		printf("%s\n", cobj_book_get_iteration(book));
 *	}
 *
 * @param book Book instance to interact with
 *
 * @return True if the iterator could be incremented and the next word accessed, false otherwise
 */
bool cobj_book_increment_iterator(cobj_book_t *book);

/**
 * Blocks the internal iterator so that cobj_book_increment_iterator() will fail and return false until the
 * iterator is reset with cobj_book_reset_iterator().
 * Iterator-related functions are intended to replace for-loops as they protect against out-of-bound errors
 * and can adjust the iterator position automatically when an element gets removed.
 *
 * @param book Book instance to interact with
 */
void cobj_book_lock_iterator(cobj_book_t *book);

/**
 * Increments the book word count (and possibly group count) by 1 and returns the start position of a buffer
 * to write to. The size of the returned buffer is defined by the value of the word_n parameter when the book
 * instance is created with cobj_book_create(). This function is intended to be used instead of cobj_book
 * write_new_word() when reading bytes from a stream to avoid needing extra read / write operations.
 * Exceptionally, in case of failure (due to a memory issue or if the given book was already in a failed
 * state, NULL can be returned. It is also the responsibility of the caller to respect the buffer's size when
 * writing to it.
 *
 * Example, instead of this :
 *
 *	cobj_book_t *book = cobj_book_create(1, 128);
 *	char buf[128] = {0};
 *	fgets(buf, 128, stream);
 *    cobj_book_write_new_word(book, buf, COBJ_BOOK_NEW_GROUP);
 *
 * Do this :
 *
 *	cobj_book_t *book = cobj_book_create(1, 128);
 *	char *buf = cobj_book_prepare_new_word(book, COBJ_BOOK_NEW_GROUP);
 *	if (buf)
 *	{
 *		fgets(buf, 128, stream);
 *	}
 *	
 * @param book Book instance to interact with
 * @param group_mode Put the new word in a new group or not
 *
 * @return Pointer to begining of the char buffer to write to.
 */
char *cobj_book_prepare_new_word(cobj_book_t *book, cobj_book_group_mode_t group_mode);

/**
 * Resets an internal word iterator to the beginning of a given group. Before accessing a word,
 * cobj_book_increment_iterator() should be called at least once.
 * Iterator-related functions are intended to replace for-loops as they protect against out-of-bound errors
 * and can adjust the iterator position automatically when an element gets removed.
 *
 * Usage example :
 *
 *	cobj_book_reset_iterator(book, group_index);
 *	while (cobj_book_increment_iterator(book))
 *	{
 *		printf("%s\n", cobj_book_get_iteration(book));
 *	}
 *
 * @param book Book instance to interact with
 * @param group_index Group to position the iterator at
 */
void cobj_book_reset_iterator(cobj_book_t *book, size_t group_index);

/**
 * Replaces the n-th word value from the n-th group with a new C-string value. If str is NULL, or the given
 * indexes are out of bound, then this function has no effect.
 *
 * @param book Book instance to interact with
 * @param str C-string to set the new value to
 * @param group_index Group position within the book
 * @param word_index Word position within the group
 */
void cobj_book_rewrite_word(cobj_book_t *book, const char *str, size_t group_index, size_t word_index);

/**
 * Removes the excess of trailing allocated memory inside the book.
 *
 * @param book Book instance to interact with
 */
void cobj_book_trim(cobj_book_t *book);

/**
 * Adds a new word to the book and increments the book word count (and possibly group count) by 1. If str is
 * NULL, this function has no effect. The book's allocated memory is automatically increased if needed to
 * accommodate the new word. If str is longer than the maximum word size set during the book's creation, it
 * will be truncated to fit it in (the NULL terminator is included in the resulting string).
 *
 * @param book Book instance to interact with
 * @param str C-string to write into the book
 * @param group_mode Put the new word in a new group or not
 */
void cobj_book_write_new_word(cobj_book_t *book, const char *str, cobj_book_group_mode_t group_mode);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets the number of allocated word slots across all groups within the book.
 * If the given book is in an error state, 0 will be returned.
 *
 * @param book Book instance to interact with
 *
 * @return Total allocated word slots
 */
size_t cobj_book_get_alloc_words(const cobj_book_t *book);

/**
 * Gets the number of words a given group has.
 * If the given book is in an error state, 0 will be returned.
 *
 * @param book Book instance to interact with
 * @param group_index Group position within the book
 *
 * @return Word number in group
 */
size_t cobj_book_get_group_size(const cobj_book_t *book, size_t group_index);

/**
 * Accesses the word pointed to by the internal iterator after it has been reset and then incremented at least
 * once. This function never returns NULL. Instead, in case of failure, an empty string consisting of a single
 * '\0' character is returned instead.
 * Iterator-related functions are intended to replace for-loops as they protect against out-of-bound errors
 * and can adjust the iterator position automatically when an element gets removed.
 *
 * Usage example :
 *
 *	cobj_book_reset_iterator(book, group_index);
 *	while (cobj_book_increment_iterator(book))
 *	{
 *		printf("%s\n", cobj_book_get_iteration(book));
 *	}
 *
 * @param book Book instance to interact with
 *
 * @return C-string pointed by the iterator
 */
const char *cobj_book_get_iteration(const cobj_book_t *book);

/**
 * Gets the group the iterator is currently set at.
 * If the given book is in an error state or the iterator has been locked, SIZE_MAX will be returned (because
 * group 0 refers to the 1st group of the book).
 *
 * @param book Book instance to interact with
 *
 * @return Group index the Iterator is currently set at.
 */
size_t cobj_book_get_iterator_group(const cobj_book_t *book);

/**
 * Gets the word offset the iterator is currently set at (starts at 1 for the first word of the group).
 * If the given book is in an error state, the iterator has been locked or the iterator has not been
 * incremented after a reset, 0 will be returned.
 *
 * @param book Book instance to interact with
 *
 * @return Iterator offset
 */
size_t cobj_book_get_iterator_offset(const cobj_book_t *book);

/**
 * Gets the number of groups within the book.
 * If the given book is in an error state, 0 will be returned.
 *
 * @param book Book instance to interact with
 *
 * @return Total number of groups
 */
size_t cobj_book_get_number_groups(const cobj_book_t *book);

/**
 * Gets the total number of words across all groups.
 * If the given book is in an error state, 0 will be returned.
 *
 * @param book Book instance to interact with
 *
 * @return Total number of words
 */
size_t cobj_book_get_number_words(const cobj_book_t *book);

/**
 * Accesses the n-th word from the n-th group.
 * This function never returns NULL. Instead, in case of failure, an empty string consisting of a single
 * '\0' character is returned instead.
 *
 * @param book Book instance to interact with
 * @param group_index Group position within the book
 * @param word_index Word position within the group
 *
 * @return Word C-string value at the given indexes
 */
const char *cobj_book_get_word(const cobj_book_t *book, size_t group_index, size_t word_index);

/**
 * Gets the maximum length of a word slot in the given book. This value is set during the book's creation by 
 * the word_n parameter in cobj_book_create().
 * If the given book is in an error state, 0 will be returned.
 *
 * @param book Book instance to interact with
 *
 * @return Max word byte length
 */
size_t cobj_book_get_word_max_size(const cobj_book_t *book);

/**
 * Checks if the book is in a failure state due to memory issues.
 *
 * @param book Book instance to interact with
 *
 * @return Book instance error state
 */
bool cobj_book_has_failed(const cobj_book_t *book);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif
