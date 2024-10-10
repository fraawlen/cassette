-- Copyright © 2024 Fraawlen <fraawlen@posteo.net>
--
-- This file is part of the Cassette Ada (CADA) bindings library.
--
-- This library is free software; you can redistribute it and/or modify it either under the terms of the GNU
-- Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the
-- License or (at your option) any later version.
--
-- This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
-- See the LGPL for the specific language governing rights and limitations.
--
-- You should have received a copy of the GNU Lesser General Public License along with this program. If not,
-- see <http://www.gnu.org/licenses/>.

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

with Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package Cassette.Book is

	-------------------------------------------------------------------------------------------------
	-- EXCEPTIONS -----------------------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	-- Exception that gets raised when an impure method or a constructor fails.
	--
	E : exception;

	-------------------------------------------------------------------------------------------------
	-- TYPES ---------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Opaque book object. It stores an automatically extensible array of chars. Chars are grouped
	-- into NUL terminated words, and words can also be grouped. The book behaves like a stack, words
	-- can only be added or erased from the end of the book.
	--
	-- Some methods, upon failure, will set an error and raise an exception E. The exact error code
	-- can be checked with Error(). If any error is set all methods will exit early with default
	-- return values and no side-effects. It's possible to clear errors with Repair().
	--
	type T is tagged limited private;

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	-- Creates a book and deep copy the contents of another book into it.
	--
	-- [Params]
	--
	-- 	Book   : Book to interact with
	-- 	Parent : Book to clone
	--
	-- [Errors]
	--
	--	Error_Invalid : Initialisation failed
	--
	procedure Clone (
		Book   : out T;
		Parent : in  T);

	-- Create an empty book.
	--
	-- [Params]
	--
	-- 	Book : Book to interact with
	--
	-- [Errors]
	--
	--	Error_Invalid : Initialisation failed
	--
	procedure Create (
		Book : out T);

	-- Destroys the book and frees memory.
	--
	-- [Params]
	--
	-- 	Book : Book to interact with
	--
	procedure Destroy (
		Book : in out T);

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Clears the contents of a given book. Allocated memory is not freed, use Destroy() for that.
	--
	-- [Params]
	--
	-- 	Book : Book to interact with
	--
	procedure Clear (
		Book : in out T);

	-- Deletes the last group of words. Allocated memory is not freed, use Destroy() for that.
	-- 
	-- [Params]
	--
	-- 	Book : Book to interact with
	--
	procedure Pop_Group (	
		Book : in out T);

	-- Deletes the last word. Allocated memory is not freed, use Destroy() for that.
	-- 
	-- [Params]
	--
	-- 	Book : Book to interact with
	--
	procedure Pop_Word (
		Book : in out T);

	-- Preallocates a set number of characters, words, references, and groups to avoid triggering
	-- multiple automatic reallocs when adding data to the book. This procedure has no effect if the
	-- requested numbers are smaller than the previously allocated amounts.
	--
	-- [Params]
	--
	-- 	Book          : Book to interact with
	-- 	Bytes_Number  : Total number of bytes across all words
	-- 	Words_Number  : Total number of words across all groups
	-- 	Groups_Number : Total number of groups
	--
	-- [Errors]
	--
	-- 	Error_Overflow : The size of the resulting book will be > Size'Last
	-- 	Error_Memory   : Failed memory allocation
	--
	procedure Prealloc (
		Book   : in out T;
		Bytes  : in Size;
		Words  : in Size;
		Groups : in Size);

	-- After this function is called, the next word that is added with Write() will be part of a new
	-- group.
	--
	-- [Params]
	--
	-- 	Book : Book to interact with
	--
	procedure Prepare_New_Group (
		Book : in out T);

	-- Clears errors and puts the book back into an usable state. The only unrecoverable error is
	-- Error_Invalid.
	--
	-- [Params]
	--
	-- 	Book : Book to interact with
	--
	procedure Repair (
		Book : in out T);

	-- Tries to rewrite a word at the given index. If the new word is longer than the original word,
	-- this function exits without modifying anything.
	--
	-- [Params]
	-- 
	--	Book       : Book to interact with
	-- 	Word_Index : Word index in book across all groups
	-- 	Str        : String
	--
	-- [Return]
	-- 
	-- 	True if the word was rewriten, False otherwhise. If the book has errored, or if the
	--    given Word index is out of bounds, then False is always returned.
	--
	function Rewrite (
		Book : in out T;
		Word : in Index;
		Str  : in String)
			return Boolean;

	-- Reverts the effects of Prepare_New_Group().
	--
	-- [Params]
	--
	-- 	Book : Book to interact with
	--
	procedure Undo_New_Group (
		Book : in out T);

	-- Appends a new word to the book and increments the book word count (and possibly group count)
	-- by 1 as well as the character count by the string's length (NUL terminator included). The book
	-- will automatically extend its allocated memory to accommodate the new word.
	-- 
	-- [Params]
	--
	-- 	Book : Book to interact with
	-- 	Str  : String
	--
	-- [Errors]
	--
	-- 	Error_Overflow : The size of the resulting book will be > Size'Last
	-- 	Error_Memory   : Failed memory allocation
	--
	procedure Write (
		Book : in out T;
		Str  : in String);

	-- Similar to Clear() but all of the allocated memory is also zeroed.
	--
	-- 	Book : Book to interact with
	--
	procedure Zero (
		Book : in out T);

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Gets the error state.
	--
	-- [Params]
	--
	-- 	Book : Book to interact with
	-- 
	-- [Return]
	--
	-- 	Error code.
	--  
	function Error (
		Book : in T)
			return Error_Code;
	
	-- Gets a group's word count.
	-- 
	-- [Params]
	-- 
	-- 	Book  : Book to interact with
	-- 	Group : Group index within book
	--
	-- [Return]
	-- 
	-- 	Number of words. If the book has errored, or if the given Group index is out of
	--	bounds, then 0 is always returned.
	--
	function Group_Length (
		Book  : in T;
		Group : in Index)
			return Size;

	-- Gets the total number of groups.
	-- 
	-- [Params]
	-- 
	-- 	Book : Book to interact with
	--
	-- [Return]
	-- 
	-- 	Number of groups. If the book has errored, then 0 is always returned.
	-- 
	function Groups_Number (
		Book : in T)
			return Size;

	-- Gets the total length of the book (all NUL terminators included).
	--
	-- [Params]
	-- 
	-- 	Book : Book to interact with
	--
	-- [Return]
	-- 
	-- Number of bytes. If the book has errored, then 0 is always returned.
	-- 
	function Length (
		Book : in T)
			return Size;

	-- Gets a word.
	-- 
	-- [Params]
	-- 
	--	Book       : Book to interact with
	-- 	Word_Index : Word index in book across all groups
	--
	-- [Return]
	-- 
	-- 	String. If the book has errored, or if the given Word index is out of bounds, then an
	--	empty string is always returned.
	-- 
	function Word (
		Book : in T;
		Word : in Index)
			return String;

	-- Gets a word from a specific group.
	-- 
	-- [Params]
	-- 
	-- 	Book  : Book to interact with
	-- 	Group : Group index within book
	-- 	Word  : Word index within group
	--
	-- [Return]
	-- 
	-- 	String. If the book has errored, or if the given Group or Word indexes are out of
	--	bounds, then an empty string is always returned.
	-- 
	function Word_In_Group (
		Book  : in T;
		Group : in Index;
		Word  : in Index)
			return String;

	-- Converts a group + local word indexes to a book-wide word index. 
	--
	-- [Params]
	-- 
	-- 	Book  : Book to interact with
	-- 	Group : Group index within book
	-- 	Word  : Word index within group
	-- 
	-- [Return]
	-- 
	-- 	Word index. If the book has errored, or if the given Group or Word indexes are out of
	--	bounds, then 0 is always returned.
	-- 
	function Word_Index (
		Book  : in T;
		Group : in Index;
		Word  : in Index)
			return Index;

	-- Gets the total number of words.
	-- 
	-- [Params]
	-- 
	-- 	Book : Book to interact with
	--
	-- [Return]
	-- 
	-- 	Total number of words across all groups. If the book has errored, then 0 is always
	-- 	returned.
	-- 
	function Words_Number (
		Book : in T)
			return Size;

	-------------------------------------------------------------------------------------------------
	-- PRIVATE -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

private

	C_Placeholder : aliased Placeholder;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	type T is tagged limited record
		Data : System.Address := C_Placeholder'Address;
	end record;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Raise_Error (Book : in T);

	-------------------------------------------------------------------------------------------------
	-- IMPORTS -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure C_Clear             (Book : System.Address);
	procedure C_Destroy           (Book : System.Address);
	procedure C_Pop_Group         (Book : System.Address);
	procedure C_Pop_Word          (Book : System.Address);
	procedure C_Prealloc          (Book : System.Address; Bytes : C.size_t; Words : C.size_t; Groups : C.size_t);
	procedure C_Prepare_New_Group (Book : System.Address);
	procedure C_Repair            (Book : System.Address);
	procedure C_Undo_New_Group    (Book : System.Address);
	procedure C_Write             (Book : System.Address; Str : C.Strings.chars_ptr);
	procedure C_Zero              (Book : System.Address);

	function  C_Clone             (Book : System.Address)                                              return System.Address;
	function  C_Create                                                                                 return System.Address;
	function  C_Error             (Book : System.Address)                                              return Error_Code;
	function  C_Group_Length      (Book : System.Address; Group : C.size_t)                            return C.size_t;
	function  C_Groups_Number     (Book : System.Address)                                              return C.size_t;
	function  C_Length            (Book : System.Address)                                              return C.size_t;
	function  C_Rewrite           (Book : System.Address; Word  : C.size_t; Str : C.Strings.chars_ptr) return C.Extensions.bool;
	function  C_Word              (Book : System.Address; Group : C.size_t)                            return C.Strings.chars_ptr;
	function  C_Word_In_Group     (Book : System.Address; Group : C.size_t; Word : C.size_t)           return C.Strings.chars_ptr;
	function  C_Word_Index        (Book : System.Address; Group : C.size_t; Word : C.size_t)           return C.size_t;
	function  C_Words_Number      (Book : System.Address)                                              return C.size_t;
	
	pragma Import (C, C_Clear,             "cbook_clear");
	pragma Import (C, C_Clone,             "cbook_clone");
	pragma Import (C, C_Create,            "cbook_create");
	pragma Import (C, C_Destroy,           "cbook_destroy");
	pragma Import (C, C_Error,             "cbook_error");
	pragma Import (C, C_Group_Length,      "cbook_group_length");
	pragma Import (C, C_Groups_Number,     "cbook_groups_number");
	pragma Import (C, C_Length,            "cbook_length");
	pragma Import (C, C_Placeholder,       "cbook_placeholder_instance");
	pragma Import (C, C_Pop_Group,         "cbook_pop_group");
	pragma Import (C, C_Pop_Word,          "cbook_pop_word");
	pragma Import (C, C_Prealloc,          "cbook_prealloc");
	pragma Import (C, C_Prepare_New_Group, "cbook_prepare_new_group");
	pragma Import (C, C_Repair,            "cbook_repair");
	pragma Import (C, C_Rewrite,           "cbook_rewrite");
	pragma Import (C, C_Undo_New_Group,    "cbook_undo_new_group");
	pragma Import (C, C_Word,              "cbook_word");
	pragma Import (C, C_Word_In_Group,     "cbook_word_in_group");
	pragma Import (C, C_Word_Index,        "cbook_word_index");
	pragma Import (C, C_Words_Number,      "cbook_words_number");
	pragma Import (C, C_Write,             "cbook_write");
	pragma Import (C, C_Zero,              "cbook_zero");

end Cassette.Book;

