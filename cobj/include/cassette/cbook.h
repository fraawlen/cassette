/************************************************************************************************************/
/* PRELUDE **************************************************************************************************/
/************************************************************************************************************/

#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

#include "cerr.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Opaque book object implemented as a dynamic stack of arbitrarily sized strings. Each string,
 * 	called a word, get written into the book sequencially. Words can only be removed starting from
 * 	the last added word. Words can be grouped. The book automatically grow when new words get
 * 	written.
 *
 * 	Words are retrieved using either a global word index, or width a local index along with a
 * 	group index.
 *
 * 	Some methods may fail and set an internal error, which can be checked using cbook_error().
 * 	If an error is set, all methods will exit early with default return values and no side
 * 	effects, leaving only the destruction function available.
 */
typedef struct cbook cbook;

/************************************************************************************************************/
/* LIFECYCLE ************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Destroys a book and frees all associated memory.
 * 	Calling this function on a NULL book has no effect.
 *
 * [Parameters]
 *
 * 	book - Book to destroy.
 *
 * [Returns]
 *
 * 	To prevent dangling pointers while keeping the function a one-liner, this function
 * 	conveniently returns nullptr.
 */
nullptr_t cbook_destroy(cbook *book);

/**
 * [Description]
 *
 * 	Creates a book instance and deep copies the contents of another book into it.
 *
 * [Parameters]
 *
 * 	book - Book to copy.
 *
 * [Returns]
 *
 * 	On succes, a pointer to a newly allocated instance. Returns nullptr on failure.
 * 	If the book is NULL or in a critical error state, this function always returns nullptr.
 * 	The caller is responsible for freeing the returned instance using cbook_destroy().
 */
[[nodiscard]] [[gnu::malloc(cbook_destroy)]] cbook *cbook_clone(const cbook *book);

/**
 * [Description]
 *
 * 	Creates a new, empty book instance.
 *
 * [Returns]
 *
 * 	On succes, a pointer to a newly allocated instance. Returns nullptr on failure.
 * 	The caller is responsible for freeing the returned instance using cbook_destroy().
 */
[[nodiscard]] [[gnu::malloc(cbook_destroy)]] cbook *cbook_create(void);

/************************************************************************************************************/
/* MUTATION *************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Clears the contents of a book.
 * 	Allocated memory is not freed, use cbook_destroy() for that.
 * 	Calling this function on a NULL book has no effect.
 *
 * [Parameters]
 *
 * 	book : Book to modify.
 */
void cbook_clear(cbook *book);

/**
 * [Description]
 *
 * 	Clears any warning error the book may have. Does not clears criticial errors.
 * 	Calling this function on a NULL book has no effect.
 *
 * [Parameters]
 *
 * 	book - Book to modify.
 */
void cbook_clear_warnings(cbook *book);

/**
 * [Description]
 *
 * 	Deletes the last word group.
 * 	Allocated memory is not freed, use cbook_destroy() for that
 * 	Calling this function on a NULL book has no effect.
 * 
 * [Parameters]
 *
 * 	book - Book to modify.
 */
void cbook_pop_group(cbook *book);

/**
 * [Description]
 *
 * 	Deletes the last word.
 * 	Allocated memory is not freed, use cbook_destroy() for that.
 * 	Calling this function on a NULL book has no effect.
 * 
 * [Parameters]
 *
 * 	book - Book to modify.
 */
void cbook_pop_word(cbook *book);

/**
 * [Description]
 *
 * 	Preallocates a set number of bytes, words, and groups to prevent multiple automatic
 * 	reallocations when writing new words. The number of bytes represent the total amount of bytes
 * 	across all words, NUL separators included (because the book store words in a packed
 * 	manner). The number of words and group is needed to reallocate enought space for index arrays.
 *
 * 	This function has no effect if the book is NULL or if the requested numbers are smaller than
 * 	the previously allocated amounts.
 *
 * [Parameters]
 *
 * 	book          - Book to modify.
 * 	bytes_number  - Total number of bytes across all words.
 * 	words_number  - Total number of words across all groups.
 * 	groups_number - Total number of groups.
 *
 * [Errors]
 *
 * 	CERR_OVERFLOW
 * 	CERR_MEMORY
 */
void cbook_prealloc(cbook *book, size_t bytes_number, size_t words_number, size_t groups_number);

/**
 * [Description]
 *
 * 	The words written with cbook_write() after this function is called will belong to a new group.
 * 	Calling this function on a NULL book has no effect.
 *
 * [Parameters]
 *
 * 	book - Book to modify.
 */
void cbook_prepare_new_group(cbook *book);

/**
 * [Description]
 *
 * 	Reverts the effects of cbook_prepare_new_group().
 * 	Calling this function on a NULL book has no effect.
 *
 * [Parameters]
 *
 * 	book - Book to modify.
 */
void cbook_undo_new_group(cbook *book);

/**
 * [Description]
 *
 * 	Appends a new word to the book and increments the book byte and word count (NUL terminator
 * 	included). If cbook_prepare_new_group() has been called beforehand, the word is part of a new
 * 	group, and the group count gets incremented.
 *
 * 	The book will automatically extend its allocated memory to accommodate the new word.
 *
 * 	Calling this function on a NULL book has no effect.
 * 
 * [Parameters]
 *
 * 	book - Book to modify.
 * 	str  - Word to write.
 *
 * [Errors]
 *
 * 	CERR_OVERFLOW
 * 	CERR_MEMORY
 */
void cbook_write(cbook *book, const char *str);

/**
 * [Description]
 *
 * 	Clears the contents of a book and zeroes all of the allocated memory.
 * 	Allocated memory is not freed, use cbook_destroy() for that.
 * 	Calling this function on a NULL book has no effect.
 * 
 * [Parameters]
 *
 * 	book - Book to modify.
 */
void cbook_zero(cbook *book);

/************************************************************************************************************/
/* ACCESS ***************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Convenience for-loop wrapper.
 * 	The I parameter is the global, not local, word index. Therefore, cbook_word() needs to be used
 * 	inside the loop instead of cbook_word_in_group(). This parameter is declared internally, you
 * 	should only provide the desired identifier.
 */
#define CBOOK_FOR_EACH(BOOK, GROUP, I) \
	for( \
		size_t I = cbook_word_index(BOOK, GROUP, 0); \
		I < cbook_word_index(BOOK, GROUP, 0) + cbook_group_length(BOOK, GROUP); \
		I++)

/**
 * [Description]
 *
 * 	Convenience inverse for-loop wrapper.
 * 	The I parameter is the global, not local, word index. Therefore, cbook_word() needs to be used
 * 	inside the loop instead of cbook_word_in_group(). This parameter is declared internally, you
 * 	should only provide the desired identifier.
 */
#define CBOOK_FOR_EACH_REV(BOOK, GROUP, I) \
	for( \
		size_t I = cbook_group_length(BOOK, GROUP) == 0 ? \
			SIZE_MAX : \
			cbook_word_index(BOOK, GROUP, cbook_group_length(BOOK, GROUP) - 1); \
		I - cbook_word_index(BOOK, GROUP, 0) < SIZE_MAX; \
		I--)

/**
 * [Description]
 *
 * 	Retrieves the book's current error state.
 *
 * [Parameters]
 *
 * 	book - Book to inspect.
 *
 * [Returns]
 *
 * 	The current error code.
 * 	If the book is NULL, this function always returns CERR_INVALID.
 */
[[gnu::pure]] enum cerr cbook_error(const cbook *book);

/**
 * [Description]
 *
 * 	Retrieves a group's word count.
 * 
 * [Parameters]
 *
 * 	book        - Book to inspect.
 * 	group_index - Group index within book.
 *
 * [Returns]
 *
 * 	The group's size.
 * 	If the book is NULL, in a critical error state, or the index is out of bounds, this function
 * 	always returns 0.
 */
[[gnu::pure]] size_t cbook_group_length(const cbook *book, size_t group_index);

/**
 * [Description]
 *
 * 	Retrieves the total number of groups a book has.
 * 
 * [Parameters]
 *
 * 	book - Book to inspect.
 *
 * [Returns]
 *
 * 	The number of groups.
 * 	If the book is NULL or in a critical error state, this function always returns 0.
 */
[[gnu::pure]] size_t cbook_groups_number(const cbook *book);

/**
 * [Description]
 *
 * 	Retrieves the total length of the book (separating NUL terminators included).
 *
 * [Parameters]
 *
 * 	book - Book to inspect.
 *
 * [Returns]
 *
 * 	The total number of bytes.
 * 	If the book is NULL or in a critical error state, this function always returns 0.
 */
[[gnu::pure]] size_t cbook_length(const cbook *book);

/**
 * [Description]
 *
 * 	Retrieves a word using a global word index. 
 * 
 * [Parameters]
 *
 * 	book       - Book to inspect.
 * 	word_index - Word index in book across all groups.
 *
 * [Returns]
 *
 * 	The word (a NUL terminated string) at index.
 * 	If the book is NULL, in a critical error state, or the index is out of bounds, this function
 * 	always returns '\0'. This function never returns nullptr.
 */
[[gnu::pure]] [[gnu::returns_nonnull]] const char  *cbook_word(const cbook *book, size_t word_index);

/**
 * [Description]
 *
 * 	Retrieves a word from a specific group.
 *
 * [Parameters]
 *
 * 	book             - Book to inspect.
 * 	group_index      - Group index within book.
 * 	word_local_index - Word index within group.
 *
 * [Returns]
 *
 * 	The word (a NUL terminated string) at index.
 * 	If the book is NULL, in a critical error state, or the indexes are out of bounds, this
 * 	function always returns "\0". This function never returns nullptr.
 */
[[gnu::pure]] [[gnu::returns_nonnull]] const char *cbook_word_in_group(const cbook *book, size_t group_index, size_t word_local_index);

/**
 * [Description]
 *
 * 	Converts a group + local word indexes to a book-wide word index.
 *
 * [Parameters]
 *
 * 	book             - Book to inspect.
 * 	group_index      - Group index within book.
 * 	word_local_index - Word index within group.
 * 
 * [Returns]
 *
 * 	The converted word index.
 * 	If the book is NULL, in a critical error state, or the indexes are out of bounds, this
 * 	function always returns "\0". This function never returns nullptr.
 */
[[gnu::pure]] size_t cbook_word_index(const cbook *book, size_t group_index, size_t word_local_index);

/**
 * [Description]
 *
 * 	Retrieves the total number of words.
 * 
 * [Parameters]
 *
 * 	book - Book to inspect.
 *
 * [Returns]
 *
 * 	The total number of words across all groups.
 * 	If the book is NULL or in a critical error state, this funciton always returns 0.
 */
[[gnu::pure]] size_t cbook_words_number(const cbook *book);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif
