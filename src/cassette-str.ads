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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Cassette.Error;
with Interfaces.C;      use Interfaces;
with System;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package Cassette.Str is

	-------------------------------------------------------------------------------------------------
	-- EXCEPTIONS -----------------------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	-- Exception that gets raised when an impure method or a constructor fails.
	--
	E : exception;

	-------------------------------------------------------------------------------------------------
	-- TYPES ---------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Opaque string object. Theses strings have the particularity of storing not just the string
	-- char array along with its size, but they also keep track of the number of UTF-8 multi-byte
	-- characters, the number of rows and columns. It's assumed that all UTF-8 characters are 1
	-- column wide. But it does not differencies between printable or non printable characters. The
	-- only exception to this width rule are tab characters, whose width can be customized with
	-- Set_Tab_Width().
	--
	-- Some methods, upon failure, will set an error bit in an internal error bitfield and raise an
	-- exception. The exact error code can be checked with Error(). If any error is set all methods
	-- will exit early with default return values and no side-effects. It's possible to clear errors
	-- with Repair().
	--
	type T is tagged limited private;

	--  Numerics.
	--
	type Size      is new C.size_t;
	type Index     is new C.size_t;
	type Precision is new C.int range 0 .. 16;

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	-- Creates a string and deep copy the contents of another input tracker into it.
	--
	-- [Params]
	--
	-- 	Self   : String to interact with
	-- 	Parent : String to clone
	--
	-- [Error]
	--
	--	INVALID : Initialisation failed
	--
	procedure Clone (
		Self   : out T;
		Parent : in  T);

	-- Create an empty input string.
	--
	-- [Params]
	--
	-- 	Self : String to interact with
	--
	-- [Error]
	--
	--	INVALID : Initialisation failed
	--
	procedure Create (
		Self : out T);

	-- Destroys the string and frees memory.
	--
	-- [Params]
	--
	-- 	Self : Input tracker to interact with
	--
	procedure Destroy (
		Self : in out T);
	
	-------------------------------------------------------------------------------------------------
	-- WRAPPER METHODS ------------------------------------------------------------------------------ 
	-------------------------------------------------------------------------------------------------

	-- Convenience wrappers for the Insert() method with the offset parameter set to
	-- Index'Last. See Insert() for details.
	--
	procedure Append (Self : in out T; Value : in T);
	procedure Append (Self : in out T; Value : in Float);
	procedure Append (Self : in out T; Value : in Integer);
	procedure Append (Self : in out T; Value : in String);

	-- Convenience wrappers for the Insert() method with the offset parameter set to 0. See
	-- Insert() for details.
	--
	procedure Prepend (Self : in out T; Value : in T);
	procedure Prepend (Self : in out T; Value : in Float);
	procedure Prepend (Self : in out T; Value : in Integer);
	procedure Prepend (Self : in out T; Value : in String);

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Clears the contents of a given string. Allocated memory is not freed, use Destroy() for
	-- that.
	--
	-- [Param]
	--
	--	Self : String to interact with
	--
	procedure Clear (
		Self : in out T);

	-- Removes a set number of UTF-8 characters at a specific offset.
	-- This function is bounds-protected, meaning that offset and length parameters will be
	-- capped at the string's length, even if a Size'Last or Index'Last values are supplied.
	--
	-- [Param]
	--
	-- 	Self   : String to interact with
	-- 	Offset : UTF-8 character position to start cutting from
	-- 	Length : number of UTF-8 characters to remove
	--
	procedure Cut (
		Self   : in out T;
		Offset : in Index;
		Length : in Size);

	-- Insert the contents of Src at a specific offset.
	-- The string's allocated memory will be automatically extended if needed to accommodate
	-- the inserted data. This function comes with overlap detection, so a String obtained from
	-- Chars() can be used. This function is bounds-protected, so the offset parameter is
	-- capped at the string's length, even if a Index'Last value is supplied.
	--
	-- [Param]
	--
	-- 	Self   : String to insert new data to
	-- 	Value  : String to ger new data from
	-- 	Offset : UTF-8 character position to insert the new data at
	--
	-- [Error]
	--
	-- 	OVERFLOW : The size of the resulting string will be > Index'Last
	-- 	MEMORY   : Failed memory allocation
	--
	procedure Insert (
		Self   : in out T;
		Value  : in T;
		Offset : in Index);

	-- Converts a float into a string then inserts it at a specific offset. The number of
	-- digits shown is controlled with Set_Precision().
	-- The string's allocated memory will be automatically extended if needed to accommodate
	-- the inserted data. This function comes with overlap detection, so a raw_str obtained
	-- from Char*() can be used. This function is bounds-protected, so the offset parameter is
	-- capped at the string's length, even if an Index'Last value is supplied.
	--
	-- [Param]
	--
	-- 	Self   : String to insert new data to
	-- 	Value  : Floating value to insert
	-- 	Offset : UTF-8 character position to insert the new data at
	--
	-- [Error]
	--
	-- 	OVERFLOW : The size of the resulting string will be > Index'Last
	-- 	MEMORY   : Failed memory allocation
	--
	procedure Insert (
		Self   : in out T;
		Value  : in Float;
		Offset : in Index);

	-- Converts a long into a string then inserts it at a specific offset.
	-- The string's allocated memory will be automatically extended if needed to accommodate
	-- the inserted data. This function comes with overlap detection, so a raw_str obtained
	-- from Char*() can be used. This function is bounds-protected, so the offset parameter is
	-- capped at the string's length, even if an Index'Last value is supplied.
	--
	-- [Param]
	--
	-- 	Self   : String to insert new data to
	-- 	Value  : Long value to insert
	-- 	Offset : UTF-8 character position to insert the new data at
	--
	-- [Error]
	--
	-- 	OVERFLOW : The size of the resulting string will be > Index'Last
	-- 	MEMORY   : Failed memory allocation
	--
	procedure Insert (
		Self   : in out T;
		Value  : in Integer;
		Offset : in Index);

	-- Insert a standard Ada string at a specific offset.
	-- The string's allocated memory will be automatically extended if needed to accommodate
	-- the inserted data. This function comes with overlap detection, so a raw_str obtained
	-- from cChar*() can be used. This function is bounds-protected, so the offset parameter
	-- is capped at the string's length, even if an Index'Last value is supplied.
	--
	-- [Param]
	--
	-- 	Self   : String to insert new data to
	-- 	Value  : Standard String to insert
	-- 	Offset : UTF-8 character position to insert the new data at
	--
	-- [Error]
	--
	-- 	OVERFLOW : The size of the resulting string will be > Index'Last
	-- 	MEMORY   : Failed memory allocation
	--
	procedure Insert (
		Self   : in out T;
		Value  : in String;
		Offset : in Index);

	-- Pads a string with a repeated sequence of characters set by pattern until its length
	-- matches length_target. This function has no effects if the string's length is bigger
	-- than the target length or if the given pattern is empty. The sequence of padding
	-- characters will be inserted at the given offset. This function is bounds-protected, so
	-- the offset parameter is capped at the string's length, even if an Index'Last value is
	-- supplied.
	--
	-- [Example]
	--
	--	S.Clear;
	--	S.Append ("test");
	--	S.Pad    ("_Ͳ", 1, 9);
	-- 	Put_Line (S.Chars)
	--
	--	--> t_Ͳ_Ͳ_est
	--
	-- [Param]
	--
	-- 	Self          : String to interact with
	-- 	Pattern       : UTF-8 character to use as padding
	-- 	Offset        : UTF-8 character position to insert the padded sequence at
	-- 	Length_Target : Resulting string length that should be reached
	--
	-- [Error]
	--
	-- 	OVERFLOW : The size of the resulting string will be > Size'Last
	-- 	MEMORY   : Failed memory allocation
	--
	procedure Pad (
		Self          : in out T;
		Pattern       : in String;
		Offset        : in Index;
		Length_Target : in Size);

	-- Preallocates a set number of bytes to avoid triggering multiple automatic reallocs when
	-- adding data to the string. This function has no effect if the requested number of bytes
	-- is smaller than the previously allocated number.
	--
	-- [Param]
	--
	-- 	Self        : String to interact with
	-- 	Byte_Length : Number of bytes
	--
	-- [Error]
	--
	-- 	MEMORY : Failed memory allocation
	--
	procedure Prealloc (
		Self        : in out T;
		Byte_Length : in Size);

	-- Clears errors and puts the string back into an usable state. The only unrecoverable
	-- error is CSTR_INVALID.
	--
	-- [Param]
	--
	-- Self : String to interact with
	--
	procedure Repair (
		Self : in out T);

	-- Sets the number of digits to show when a double value gets inserted. The effects of the
	-- int values are limited by the C printf's "%.*Lf" operator.
	--
	-- [Param]
	--
	-- 	Self : String to interact with
	-- 	N    : Number of decimal digits
	--
	procedure Set_Precision (
		Self  : in out T;
		Value : in Precision);

	-- Sets the width of a '\t' character. This will affect the results of 2d functions like
	-- Coords_Offset(), Test_Wrap(), Width() and, Wrap().
	--
	-- [Param]
	--
	-- 	Self  : String to interact with
	-- 	Width : Tab width
	--
	procedure Set_Tab_Width (
		Self  : in out T;
		Width : in Size);

	-- Slices out a set number of UTF-8 characters at a specific offset and discards the rest.
	-- This function is bounds-protected, meaning that offset and length parameters will be
	-- capped at the string's length, even if a Size'Last or Index'Last values are supplied.
	--
	-- [Param]
	--
	-- 	Self   : String to interact with
	-- 	Offset : UTF-8 character position to start slicing from
	-- 	Length : number of UTF-8 characters to slice out
	--
	procedure Slice (
		Self   : in out T;
		Offset : in Index;
		Length : in Size);

	-- Removes extra leading and trailing whitespaces (space and tab characters).
	--
	-- [Param]
	--
	-- 	Self : String to interact with
	--
	procedure Trim (
		Self : in out T);

	-- Wraps a string by adding newlines to rows that are longer than Max_Width. Old newlines
	-- are also kept. This function has no effects if max_width is bigger than the string's
	-- width. A max_width of 0 is illegal.
	--
	-- [Param]
	--
	-- 	Self      : String to interact with
	-- 	Max_Width : Width after which a newline is added to the string
	--
	-- [Error]
	--
	-- 	PARAM    : Illegal width was given
	-- 	OVERFLOW : The size of the resulting string will be > Size'Last
	-- 	MEMORY   : Failed memory allocation
	--
	procedure Wrap (
		Self      : in out T;
		Max_Width : in Size);

	-- Similar to Clear() but all of the allocated memory is also zeroed.
	--
	-- 	Self : String to interact with
	--
	procedure Zero (
		Self : in out T);

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Gets the string's length in bytes, including the NUL terminator.
	--
	-- [Params]
	--
	-- 	Self : String to interact with
	--
	-- [Return]
	--
	-- 	Number of bytes. If the string has errored, then 0 is always returned.
	--
	function Byte_Length (
		Self : in T)
			return Size;

	-- Converts the given UTF-8 character offset into a byte offset.
	-- This function is bounds-protected, so the offset parameter is capped at the string's
	-- length, even if an Index'Last value is supplied.
	--
	-- [Params]
	--
	-- 	Self    : String to interact with
	-- 	Offset : UTF-8 character offset
	--
	-- [Return]
	--
	-- 	Converted offset in bytes. If the string has errored, then 0 is always returned.
	--
	function Byte_Offset (
		Self   : in T;
		Offset : in Index)
			return Index;

	-- Gets the raw NUL terminated C string.
	--
	-- [Params]
	--
	-- 	Self : String to interact with
	--
	-- [Return]
	--
	-- 	String. If the string has errored, an empty string is always returned.
	--
	function Chars (
		Self : in T)
			return String;

	-- Gets the raw NUL terminated C string offseted by 2d coordinates.
	-- This function is bounds-protected, so the row and col parameter are capped at the
	-- string's height and width respectively, even if Index'Last values are supplied.
	--
	-- [Params]
	--
	-- 	Self : String to interact with
	-- 	Row  : Row index
	-- 	Col  : Columns index
	--
	-- [Return]
	--
	-- 	String. If the string has errored, an empty string is always returned.
	--
	function Chars_At_Coords (
		Self : in T;
		Row  : in Index;
		Col  : in Index)
			return String;

	-- Gets the raw NUL terminated C string offseted by an specific number of UTF-8 characters.
	-- This function is bounds-protected, so the offset parameter is capped at the string's
	-- length, even if an Index'Last value is supplied.
	--
	-- [Params]
	--
	-- 	Self   : String to interact with
	-- 	Offset : UTF-8 character offset
	--
	-- [Return]
	--
	-- 	String. If the string has errored, an empty string is always returned.
	--
	function Chars_At_Offset (
		Self   : in T;
		Offset : in Index)
			return String;

	-- Converts the given 2d coordinates into a UTF-8 character offset.
	-- This function is bounds-protected, so the row and col parameter are capped at the
	-- string's height and width respectively, even if Index'Last values are supplied.
	--
	-- [Params]
	--
	-- 	Self : String to interact with
	-- 	Row  : Row index
	-- 	Col  : Columns index
	--
	-- [Return]
	--
	-- 	Converted offset in number of UTF-8 characters. If the string has errored, then 0 is
	-- 	always returned.
	--
	function Coords_Offset (
		Self : in T;
		Row  : in Index;
		Col  : in Index)
			return Index;

	-- Gets the error state.
	--
	-- [Params]
	--
	-- 	Self : Config to interact with
	-- 
	-- [Return]
	--
	-- 	Error code
	--  
	function Error (
		Self : in T)
			return Cassette.Error.T;

	-- Get the number of rows.
	--
	-- [Params]
	--
	-- 	Self : String to interact with
	--
	-- [Return]
	--
	--	Number of rows. If the string has errored, then 0 is always returned.
	--
	function Height (
		Self : in T)
			return Size;

	-- Gets the number of UTF-8 characters a string is made of. Unlike Byte_Length(), the NUL
	-- terminator is not included.
	--
	-- [Params]
	--
	-- 	Self : String to interact with
	--
	--
	-- [Return]
	--
	-- 	Number of UTF-8 characters. If the string has errored, then 0 is always returned.
	--
	function Length (
		Self : in T)
			return Size;

	-- Calculates the number of rows a string wrapped with max_width will have. But unlike
	-- Wrap() the string is not modified.
	--
	-- [Params]
	--
	-- 	Self      : String to interact with
	-- 	Max_Width : Width after which a newline is added to the string
	--
	-- [Return]
	--
	--	Number of rows. If the string has errored, then 0 is always returned.
	--
	function Test_Wrap (
		Self      : in T;
		Max_Width : in Size)
			return Size;

	-- Converts the UTF-8 character offset of a wrapped string into an offset that matches the
	-- character position of the unwrapped string. It is assumed the difference between Wrap
	-- and str is a single Wrap() operation and that the tab width of both strings is equal.
	-- Check out the provided example for more details about this function use case.
	-- This function is bounds-protected, so the offset parameter is capped at the string's
	-- length, even if a Index'Last value is supplied.
	--
	-- [Params]
	--
	-- 	Self   : Reference string
	-- 	Wrap   : Wrapped string
	-- 	Offset : UTF-8 character offset
	--
	-- [Return]
	--
	--	Converted offset in number of UTF-8 characters. If the string has errored, then 0 is
	--	always returned.
	--
	function Unwrapped_Offset (
		Self   : in T;
		Wrap   : in T;
		Offset : Index)
			return Index;

	-- Gets the number of columns. The NUL terminator and newline characters are not included.
	--
	-- [Params]
	--
	-- 	Self : String to interact with
	--
	-- [Return]
	--
	--	Number of columns. If the string has errored, then 0 is always returned.
	--
	function Width (
		Self : in T)
			return Size;

	-------------------------------------------------------------------------------------------------
	-- PRIVATE -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

private

	type Cstr is null record;

	Placeholder : aliased Cstr
		with Import        => True, 
		     Convention    => C, 
		     External_Name => "cstr_placeholder_instance";

	type T is tagged limited record
		Data : System.Address := Placeholder'Address;
	end record;

	procedure Check (Self : in T);


end Cassette.Str;
