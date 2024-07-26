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

pragma Ada_2012;

with Cassette.Error;
with Interfaces.C; use Interfaces;
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
	-- Some methods, upon failure, will set an error bit in an internal error bitfield and raise an
	-- exception. The exact error code can be checked with Error(). If any error is set all methods
	-- will exit early with default return values and no side-effects. It's possible to clear errors
	-- with Repair().
	--
	type T is tagged limited private;

	-- String to group addition mode.
	--
	type Mode is (OLD_GROUP, NEW_GROUP)
		with Convention => C;

	--  Numerics.
	--
	subtype Index is C.size_t;
	subtype Size  is C.size_t;

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	-- Creates a book and deep copy the contents of another book into it.
	--
	-- [Params]
	--
	-- 	Self   : Book to interact with
	-- 	Parent : Book to clone
	--
	-- [Errors]
	--
	--	INVALID : Initialisation failed
	--
	procedure Clone (
		Self   : out T;
		Parent : in  T);

	-- Create an empty book.
	--
	-- [Params]
	--
	-- 	Self : Book to interact with
	--
	-- [Errors]
	--
	--	INVALID : Initialisation failed
	--
	procedure Create (
		Self : out T);

	-- Destroys the book and frees memory.
	--
	-- [Params]
	--
	-- 	Self : Book to interact with
	--
	procedure Destroy (
		Self : in out T);

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Clears the contents of a given book. Allocated memory is not freed, use Destroy() for that.
	--
	-- [Params]
	--
	-- 	Self : Book to interact with
	--
	procedure Clear (
		Self : in out T);

	-- Deletes the last group of words. Allocated memory is not freed, use Destroy() for that.
	-- 
	-- [Params]
	--
	-- 	Self : Book to interact with
	--
	procedure Pop_Group (	
		Self : in out T);

	-- Deletes the last word. Allocated memory is not freed, use Destroy() for that.
	-- 
	-- [Params]
	--
	-- 	Self : Book to interact with
	--
	procedure Pop_Word (
		Self : in out T);

	-- Preallocates a set number of characters, words, references, and groups to avoid triggering
	-- multiple automatic reallocs when adding data to the book. This procedure has no effect if the
	-- requested numbers are smaller than the previously allocated amounts.
	--
	-- [Params]
	--
	-- 	Self          : Book to interact with
	-- 	Bytes_Number  : Total number of bytes across all words
	-- 	Words_Number  : Total number of words across all groups
	-- 	Groups_Number : Total number of groups
	--
	-- [Errors]
	--
	-- 	OVERFLOW : The size of the resulting book will be > Size'Last
	-- 	MEMORY   : Failed memory allocation
	--
	procedure Prealloc (
		Self   : in out T;
		Bytes  : in Size;
		Words  : in Size;
		Groups : in Size);

	-- Clears errors and puts the book back into an usable state. The only unrecoverable error is
	-- INVALID.
	--
	-- [Params]
	--
	-- 	Self : Book to interact with
	--
	procedure Repair (
		Self : in out T);

	-- Appends a new string (called 'word') to the book and increments the book word count (and
	-- possibly group count) by 1 as well as the character count by the string's length (NUL
	-- terminator included). The book will automatically extend its allocated memory to accommodate
	-- the new word.
	-- 
	-- [Params]
	--
	-- 	Self       : Book to interact with
	-- 	Str        : String
	-- 	Group_Mode : Create (or not) a group for the new word
	--
	-- [Errors]
	--
	-- 	OVERFLOW : The size of the resulting book will be > Size'Last
	-- 	MEMORY   : Failed memory allocation
	--
	procedure Write (
		Self       : in out T;
		Str        : in String;
		Group_Mode : in Mode);

	-- Similar to Clear() but all of the allocated memory is also zeroed.
	--
	-- 	Self : Book to interact with
	--
	procedure Zero (
		Self : in out T);

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Gets the error state.
	--
	-- [Params]
	--
	-- 	Self : Book to interact with
	-- 
	-- [Return]
	--
	-- 	Error code.
	--  
	function Error (
		Self : in T)
			return Cassette.Error.T;
	
	-- Gets a group's word count.
	-- 
	-- [Params]
	-- 
	-- 	Self        : Book to interact with
	-- 	Group_Index : Group index within book
	--
	-- [Return]
	-- 
	-- 	Number of words. If the book has errored, or if the given Group index is out of
	--	bounds, then 0 is always returned.
	--
	function Group_Length (
		Self        : in T;
		Group_Index : in Index)
			return Size;

	-- Gets the total number of groups.
	-- 
	-- [Params]
	-- 
	-- 	Self : Book to interact with
	--
	-- [Return]
	-- 
	-- 	Number of groups. If the book has errored, then 0 is always returned.
	-- 
	function Groups_Number (
		Self : in T)
			return Size;

	-- Gets the total length of the book (all NUL terminators included).
	--
	-- [Params]
	-- 
	-- 	Self : Book to interact with
	--
	-- [Return]
	-- 
	-- Number of bytes. If the book has errored, then 0 is always returned.
	-- 
	function Length (
		Self : in T)
			return Size;

	-- Gets a word.
	-- 
	-- [Params]
	-- 
	--	Self       : Book to interact with
	-- 	Word_Index : Word index in book across all groups
	--
	-- [Return]
	-- 
	-- 	String. If the book has errored, or if the given Word index is out of bounds, then an
	--	empty string is always returned.
	-- 
	function Word (
		Self       : in T;
		Word_Index : in Index)
			return String;

	-- Gets a word from a specific group.
	-- 
	-- [Params]
	-- 
	-- 	Self        : Book to interact with
	-- 	Group_Index : Group index within book
	-- 	Word_Index  : Word index within group
	--
	-- [Return]
	-- 
	-- 	String. If the book has errored, or if the given Group or Word indexes are out of
	--	bounds, then an empty string is always returned.
	-- 
	function Word_In_Group (
		Self        : in T;
		Group_Index : in Index;
		Word_index  : in Index)
			return String;

	-- Converts a group + local word indexes to a book-wide word index. 
	--
	-- [Params]
	-- 
	-- 	Self        : Book to interact with
	-- 	Group_Index : Group index within book
	-- 	Word_Index  : Word index within group
	-- 
	-- [Return]
	-- 
	-- 	Word index. If the book has errored, or if the given Group or Word indexes are out of
	--	bounds, then 0 is always returned.
	-- 
	function Word_Index (
		Self        : in T;
		Group_Index : in Index;
		Word_Index  : in Index)
			return Index;

	-- Gets the total number of words.
	-- 
	-- [Params]
	-- 
	-- 	Self : Book to interact with
	--
	-- [Return]
	-- 
	-- 	Total number of words across all groups. If the book has errored, then 0 is always
	-- 	returned.
	-- 
	function Words_Number (
		Self : in T)
			return Size;

	-------------------------------------------------------------------------------------------------
	-- PRIVATE -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

private

	type Cbook is null record;

	Placeholder : aliased Cbook
		with Import        => True, 
		     Convention    => C, 
		     External_Name => "cbook_placeholder_instance";

	type T is tagged limited record
		Data : System.Address := Placeholder'Address;
	end record;

	procedure Check (Self : in T);

end Cassette.Book;
