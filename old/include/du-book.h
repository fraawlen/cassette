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

#ifndef DU_BOOK_H
#define DU_BOOK_H

#include <stdbool.h>
#include <stdlib.h>

#include "du-status.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * Fancy dynamic string array / vector that can autoresize itself to accomodate more words (aka C-strings).
 * Words can be grouped to create sub-string-arrays. All words are stored in a single continuous block of
 * memory. There can't be more groups than words. The groups array values points to index position (not the
 * exact byte offset) of the first word of each group. Words are added or erased from a given book
 * sequencially, but their value can be accessed randomly.
 * If status is not set to DU_STATUS_SUCCESS all handler functions will have no effect with the exception of
 * du_book_reset() and du_book_init().
 *
 * @param words    : words array
 * @param groups   : groups array
 * @param word_n   : maximum size (in bytes) of a single word
 * @param n_words  : current amount of words
 * @param n_groups : current amount of groups
 * @param n_alloc  : amount of allocated words (exact amount of allocated bytes is word_n * n_alloc)
 * @param status   : error state
 */
typedef struct {
	char *words;
	size_t *groups;
	size_t word_n;
	size_t n_groups;
	size_t n_words;
	size_t n_alloc;
	du_status_t status;
} du_book_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Pre-allocate memory to the book and set its variables appropriately. If n = 0, no memory is
 * pre-allocated, but the structure will still be considered to have been initialised. In case of error,
 * book->status will be set to DU_STATUS_FAILURE. It's set to DU_STATUS_SUCCESS otherwhise.
 *
 * @param book    : book to init 
 * @param n_alloc : initial size of the word array to pre-allocate
 * @param word_n  : maximum size (in bytes) of a single word
 */
void du_book_init(du_book_t *book, size_t n_alloc, size_t word_n);

/**
 * Allocated memory is freed and the structure will be put in an unitialised state with book->status set to
 * DU_STATUS_NOT_INIT. The given structure itself is not freed, and may require an explicit free operation.
 *
 * @param book : book to reset
 */
void du_book_reset(du_book_t *book);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Removes all words and groups. Internal memory however is not freed, use du_book_reset() for that.
 * The given structure needs to be initialised beforehand.
 *
 * @param book : book to clear
 */
void du_book_clear(du_book_t *book);

/**
 * Removes the last groups and words assotiated to it.
 * The given structure needs to be initialised beforehand.
 *
 * @param book : book to remove data from
 */
void du_book_erase_last_group(du_book_t *book);

/**
 * Removes the last word. If that word was the first in its group, the group will be deleted too.
 * The given structure needs to be initialised beforehand.
 *
 * @param book : book to remove data from
 */
void du_book_erase_last_word(du_book_t *book);

/**
 * Writes the given string as a new word in the book.
 * If needed, the book's internal arrays will be automatically expanded. In case of failure, the book's
 * status will be set to DU_STATUS_FAILURE.
 * The given structure needs to be initialised beforehand.
 *
 * @param book      : book to add the word to
 * @param new_group : make the added word part of a new group
 * @param str       : C-string to add as a word
 */
void du_book_write_new_word(du_book_t *book, bool new_group, const char *str);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets the first word of the index-th group in the given book.
 * The given structure needs to be initialised beforehand.
 *
 * @param book  : book to get data from
 * @param index : index position of the group to look for
 *
 * @return : pointer to the first character of the first word within the target group. If index is out of
 *           bounds, NULL will be returned instead
 */
char *du_book_get_group(const du_book_t *book, size_t index);

/**
 * Gets the length of the index-th group in the given book.
 * The given structure needs to be initialised beforehand.
 *
 * @param book : book to get data from
 * @param index : index position of the group to look for
 *
 * @return : length (in words) of the target group. If index is out of bounds, 0 will be returned instead
 */
size_t du_book_get_group_length(const du_book_t *book, size_t index);

/**
 * Gets the first word of the last group that has been added to the book.
 * The given structure needs to be initialised beforehand.
 *
 * @param book : book to get data from
 *
 * @return : pointer to the first character of the first word within the last group. If no word has been
 *           written into the book yet, NULL will be returned instead
 */
char *du_book_get_last_group(const du_book_t *book);

/**
 * Gets the last word that has been added to the book. It does not takes groups into account.
 * The given structure needs to be initialised beforehand.
 *
 * @param book : book to get data from
 *
 * @return : pointer to the first character of the last word. If no word has been written into the book yet,
 *           NULL will be returned instead
 */
char *du_book_get_last_word(const du_book_t *book);

/**
 * Gets the word following the word pointed to. Use this function to easily iterate over a book.
 * Unlike du_book_get_new_word(), the book is not modified in any manner.
 * The given structure needs to be initialised beforehand.
 *
 * @param book : book to get data from
 * @param word : pointer to current word from book.
 *
 * @return : pointer to the first character of the next word to be read from the book. If the word pointer
 *           is out of bounds of the word array, or if no words have been written yet into the book, NULL
 *           will be returned instead. But if the word pointer is set to NULL, the first character of the
 *           first word within the book will be returned instead.
 */
char *du_book_get_next_word(const du_book_t *book, char **word);

/**
 * Similar to du_book_get_new_word(). However, exeptionally, this getter function has a side effect of
 * incrementing words and possibly group (if new_group is set to true) counters. That's because this function
 * is intended to be an alternative to du_book_write_new_word(), by giving the caller direct access to the
 * exact memory location to write to, all while taking care of incrementing the different counters
 * appropriately.
 * If needed, the book's internal arrays will be automatically expanded. In case of failure, the book's
 * status will be set to DU_STATUS_FAILURE.
 * The given structure needs to be initialised beforehand.
 *
 * @param book      : book to get data from
 * @param new_group : make the returned word part of a new group
 *
 * @return : pointer to the first character of the new word. In case of extension failure, NULL will be
 *           returned instead.
 */
char *du_book_get_new_word(du_book_t *book, bool new_group);

/**
 * Gets the index-th word of a book irrespectively of groups.
 * The given structure needs to be initialised beforehand.
 *
 * @param book  : book to get data from
 * @param index : index position of the word to look for
 *
 * @return : pointer to the first character of the target word. If index is out of bounds, NULL will be
 *           returned instead.
 */
char *du_book_get_word(const du_book_t *book, size_t index);

/**
 * Gets the index_word-th word from the index_group-th group of a book.
 * The given structure needs to be initialised beforehand.
 *
 * @param book        : book to get data from
 * @param index_group : index position of the group to look for
 * @param index_word  : index position of the word to look for within the group
 *
 * @return : pointer to the first character of the target word within the target group. If index_group is out
 *           of bound, or if index_word >= than the target group size, then NULL will be returned instead.
 */
char *du_book_get_word_in_group(const du_book_t *book, size_t index_group, size_t index_word);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DU_BOOK_H */

