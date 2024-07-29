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

package Cassette.Str is

	use type Interfaces.C.size_t;

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
	-- Some methods, upon failure, will set an error and raise an exception E. The exact error code
	-- can be checked with Error(). If any error is set all methods will exit early with default
	-- return values and no side-effects. It's possible to clear errors with Repair().
	--
	type T is tagged limited private;

	-- Numerics.
	--
	type Precision is new C.int range 0 .. 16;

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	-- Creates a string and deep copy the contents of another input tracker into it.
	--
	-- [Params]
	--
	-- 	Str    : String to interact with
	-- 	Parent : String to clone
	--
	-- [Errors]
	--
	--	Error_Invalid : Initialisation failed
	--
	procedure Clone (
		Str    : out T;
		Parent : in  T);

	-- Create an empty input string.
	--
	-- [Params]
	--
	-- 	Str : String to interact with
	--
	-- [Errors]
	--
	--	Error_Invalid : Initialisation failed
	--
	procedure Create (
		Str : out T);

	-- Destroys the string and frees memory.
	--
	-- [Params]
	--
	-- 	Str : String to interact with
	--
	procedure Destroy (
		Str : in out T);
	
	-------------------------------------------------------------------------------------------------
	-- WRAPPER METHODS ------------------------------------------------------------------------------ 
	-------------------------------------------------------------------------------------------------

	-- Convenience wrappers for the Insert() method with the offset parameter set to Index'Last. See
	-- Insert() for details.
	--
	procedure Append (Str : in out T; Value : in T);
	procedure Append (Str : in out T; Value : in Float);
	procedure Append (Str : in out T; Value : in Integer);
	procedure Append (Str : in out T; Value : in String);

	-- Convenience wrappers for the Insert() method with the offset parameter set to 0. See Insert()
	-- for details.
	--
	procedure Prepend (Str : in out T; Value : in T);
	procedure Prepend (Str : in out T; Value : in Float);
	procedure Prepend (Str : in out T; Value : in Integer);
	procedure Prepend (Str : in out T; Value : in String);

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Clears the contents of a given string. Allocated memory is not freed, use Destroy() for that.
	--
	-- [Params]
	--
	--	Str : String to interact with
	--
	procedure Clear (
		Str : in out T);

	-- Removes a set number of UTF-8 characters at a specific offset.
	-- This procedure is bounds-protected, meaning that offset and length parameters will be capped
	-- at the string's length, even if a Size'Last or Index'Last values are supplied.
	--
	-- [Params]
	--
	-- 	Str    : String to interact with
	-- 	Offset : UTF-8 character position to start cutting from
	-- 	Length : number of UTF-8 characters to remove
	--
	procedure Cut (
		Str    : in out T;
		Offset : in Index;
		Length : in Size);

	-- Insert the contents of Src at a specific offset.
	-- The string's allocated memory will be automatically extended if needed to accommodate the
	-- inserted data. This procedure comes with overlap detection, so a String obtained from Chars()
	-- can be used. This procedure is bounds-protected, so the offset parameter is capped at the
	-- string's length, even if a Index'Last value is supplied.
	--
	-- [Params]
	--
	-- 	Str    : String to insert new data to
	-- 	Value  : String to ger new data from
	-- 	Offset : UTF-8 character position to insert the new data at
	--
	-- [Errors]
	--
	-- 	Error_Overflow : The size of the resulting string will be > Index'Last
	-- 	Error_Memory   : Failed memory allocation
	--
	procedure Insert (
		Str    : in out T;
		Value  : in T;
		Offset : in Index);

	-- Converts a float into a string then inserts it at a specific offset. The float is represented
	-- using decimal notation. The number of digits shown is controlled with Set_Precision(). The
	-- string's allocated memory will be automatically extended if needed to accommodate the inserted
	-- data. This procedure comes with overlap detection, so a raw_str obtained from Char*() can be
	-- used. This procedure is bounds-protected, so the offset parameter is capped at the string's
	-- length, even if an Index'Last value is supplied.
	--
	-- [Params]
	--
	-- 	Str    : String to insert new data to
	-- 	Value  : Floating value to insert
	-- 	Offset : UTF-8 character position to insert the new data at
	--
	-- [Errors]
	--
	-- 	Error_Overflow : The size of the resulting string will be > Index'Last
	-- 	Error_Memory   : Failed memory allocation
	--
	procedure Insert (
		Str    : in out T;
		Value  : in Float;
		Offset : in Index);

	-- Converts a long into a string then inserts it at a specific offset.
	-- The string's allocated memory will be automatically extended if needed to accommodate the
	-- inserted data. This procedure comes with overlap detection, so a raw_str obtained from Char*()
	-- can be used. This procedure is bounds-protected, so the offset parameter is capped at the
	-- string's length, even if an Index'Last value is supplied.
	--
	-- [Params]
	--
	-- 	Str    : String to insert new data to
	-- 	Value  : Long value to insert
	-- 	Offset : UTF-8 character position to insert the new data at
	--
	-- [Errors]
	--
	-- 	Error_Overflow : The size of the resulting string will be > Index'Last
	-- 	Error_Memory   : Failed memory allocation
	--
	procedure Insert (
		Str    : in out T;
		Value  : in Integer;
		Offset : in Index);

	-- Insert a standard Ada string at a specific offset.
	-- The string's allocated memory will be automatically extended if needed to accommodate the
	-- inserted data. This procedure comes with overlap detection, so a raw_str obtained from Char*()
	-- can be used. This procedure is bounds-protected, so the offset parameter is capped at the
	-- string's length, even if an Index'Last value is supplied.
	--
	-- [Params]
	--
	-- 	Str    : String to insert new data to
	-- 	Value  : Standard String to insert
	-- 	Offset : UTF-8 character position to insert the new data at
	--
	-- [Errors]
	--
	-- 	Error_Overflow : The size of the resulting string will be > Index'Last
	-- 	Error_Memory   : Failed memory allocation
	--
	procedure Insert (
		Str    : in out T;
		Value  : in String;
		Offset : in Index);

	-- Pads a string with a repeated sequence of characters set by pattern until its length matches 
	-- length_target. This procedure has no effects if the string's length is bigger than the target
	-- length or if the given pattern is empty. The sequence of padding characters will be inserted
	-- at the given offset. This procedure is bounds-protected, so the offset parameter is capped at
	-- the string's length, even if an Index'Last value is supplied.
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
	-- [Params]
	--
	-- 	Str           : String to interact with
	-- 	Pattern       : Standard String to use as padding pattern
	-- 	Offset        : UTF-8 character position to insert the padded sequence at
	-- 	Length_Target : Resulting string length that should be reached
	--
	-- [Errors]
	--
	-- 	Error_Overflow : The size of the resulting string will be > Size'Last
	-- 	Error_Memory   : Failed memory allocation
	--
	procedure Pad (
		Str           : in out T;
		Pattern       : in String;
		Offset        : in Index;
		Length_Target : in Size);

	-- Preallocates a set number of bytes to avoid triggering multiple automatic reallocs when adding
	-- data to the string. This procedure has no effect if the requested number of bytes is smaller
	-- than the previously allocated number.
	--
	-- [Params]
	--
	-- 	Str   : String to interact with
	-- 	Bytes : Number of bytes
	--
	-- [Errors]
	--
	-- 	Error_Memory : Failed memory allocation
	--
	procedure Prealloc (
		Str   : in out T;
		Bytes : in Size);

	-- Clears errors and puts the string back into an usable state. The only unrecoverable error is
	-- Error_Invalid.
	--
	-- [Params]
	--
	-- 	Str : String to interact with
	--
	procedure Repair (
		Str : in out T);

	-- Sets the number of digits to show when a Float gets inserted. 
	--
	-- [Params]
	--
	-- 	Str   : String to interact with
	-- 	Value : Number of decimal digits
	--
	procedure Set_Precision (
		Str   : in out T;
		Value : in Precision);

	-- Sets the width of a '\t' character. This will affect the results of 2d procedures like
	-- Coords_Offset(), Test_Wrap(), Width() and, Wrap().
	--
	-- [Params]
	--
	-- 	Str   : String to interact with
	-- 	Width : Tab width
	--
	procedure Set_Tab_Width (
		Str   : in out T;
		Width : in Size);

	-- Slices out a set number of UTF-8 characters at a specific offset and discards the rest. This
	-- procedure is bounds-protected, meaning that offset and length parameters will be capped at the
	-- string's length, even if a Size'Last or Index'Last values are supplied.
	--
	-- [Params]
	--
	-- 	Str    : String to interact with
	-- 	Offset : UTF-8 character position to start slicing from
	-- 	Length : number of UTF-8 characters to slice out
	--
	procedure Slice (
		Str    : in out T;
		Offset : in Index;
		Length : in Size);

	-- Removes extra leading and trailing whitespaces (space and tab characters).
	--
	-- [Params]
	--
	-- 	Str : String to interact with
	--
	procedure Trim (
		Str : in out T);

	-- Wraps a string by adding newlines to rows that are longer than Max_Width. Old newlines are
	-- also kept. This procedure has no effects if max_width is bigger than the string's width. A
	-- Max_Width of 0 is illegal.
	--
	-- [Params]
	--
	-- 	Str   : String to interact with
	-- 	Width : Width after which a newline is added to the string
	--
	-- [Errors]
	--
	-- 	Error_Param    : Illegal width was given
	-- 	Error_Overflow : The size of the resulting string will be > Size'Last
	-- 	Error_Memory   : Failed memory allocation
	--
	procedure Wrap (
		Str   : in out T;
		Width : in Size)
			with Pre => Width > 0;

	-- Similar to Clear() but all of the allocated memory is also zeroed.
	--
	-- 	Str : String to interact with
	--
	procedure Zero (
		Str : in out T);

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Gets the string's length in bytes, including the NUL terminator.
	--
	-- [Params]
	--
	-- 	Str : String to interact with
	--
	-- [Return]
	--
	-- 	Number of bytes. If the string has errored, then 0 is always returned.
	--
	function Byte_Length (
		Str : in T)
			return Size;

	-- Converts the given UTF-8 character offset into a byte offset. This function is
	-- bounds-protected, so the offset parameter is capped at the string's length, even if an
	-- Index'Last value is supplied.
	--
	-- [Params]
	--
	-- 	Str    : String to interact with
	-- 	Offset : UTF-8 character offset
	--
	-- [Return]
	--
	-- 	Converted offset in bytes. If the string has errored, then 0 is always returned.
	--
	function Byte_Offset (
		Str    : in T;
		Offset : in Index)
			return Index;

	-- Gets the raw NUL terminated C string.
	--
	-- [Params]
	--
	-- 	Str : String to interact with
	--
	-- [Return]
	--
	-- 	String. If the string has errored, an empty string is always returned.
	--
	function Chars (
		Str : in T)
			return String;

	-- Gets the raw NUL terminated C string offseted by 2d coordinates. This function is
	-- bounds-protected, so the row and col parameter are capped at the string's height and width
	-- respectively, even if Index'Last values are supplied.
	--
	-- [Params]
	--
	-- 	Str : String to interact with
	-- 	Row : Row index
	-- 	Col : Columns index
	--
	-- [Return]
	--
	-- 	String. If the string has errored, an empty string is always returned.
	--
	function Chars_At_Coords (
		Str : in T;
		Row : in Index;
		Col : in Index)
			return String;

	-- Gets the raw NUL terminated C string offseted by an specific number of UTF-8 characters. This
	-- function is bounds-protected, so the offset parameter is capped at the string's length, even
	-- if an Index'Last value is supplied.
	--
	-- [Params]
	--
	-- 	Str    : String to interact with
	-- 	Offset : UTF-8 character offset
	--
	-- [Return]
	--
	-- 	String. If the string has errored, an empty string is always returned.
	--
	function Chars_At_Offset (
		Str    : in T;
		Offset : in Index)
			return String;

	-- Converts the given 2d coordinates into a UTF-8 character offset. This function is
	-- bounds-protected, so the row and col parameter are capped at the string's height and width
	-- respectively, even if Index'Last values are supplied.
	--
	-- [Params]
	--
	-- 	Str : String to interact with
	-- 	Row : Row index
	-- 	Col : Columns index
	--
	-- [Return]
	--
	-- 	Converted offset in number of UTF-8 characters. If the string has errored, then 0 is
	-- 	always returned.
	--
	function Coords_Offset (
		Str : in T;
		Row : in Index;
		Col : in Index)
			return Index;

	-- Gets the error state.
	--
	-- [Params]
	--
	-- 	Str : String to interact with
	-- 
	-- [Return]
	--
	-- 	Error code.
	--  
	function Error (
		Str : in T)
			return Error_Code;

	-- Get the number of rows.
	--
	-- [Params]
	--
	-- 	Str : String to interact with
	--
	-- [Return]
	--
	--	Number of rows. If the string has errored, then 0 is always returned.
	--
	function Height (
		Str : in T)
			return Size;

	-- Gets the number of UTF-8 characters a string is made of. Unlike Byte_Length(), the NUL
	-- terminator is not included.
	--
	-- [Params]
	--
	-- 	Str : String to interact with
	--
	-- [Return]
	--
	-- 	Number of UTF-8 characters. If the string has errored, then 0 is always returned.
	--
	function Length (
		Str : in T)
			return Size;

	-- Calculates the number of rows a string wrapped with max_width will have. But unlike Wrap() the
	-- string is not modified.
	--
	-- [Params]
	--
	-- 	Str   : String to interact with
	-- 	Width : Width after which a newline is added to the string
	--
	-- [Return]
	--
	--	Number of rows. If the string has errored, then 0 is always returned.
	--
	function Test_Wrap (
		Str   : in T;
		Width : in Size)
			return Size
			with Pre => Width > 0;

	-- Converts the UTF-8 character offset of a wrapped string into an offset that matches the
	-- character position of the unwrapped string. It is assumed the difference between Wrap and
	-- Str is a single Wrap() operation and that the tab width of both strings is equal. Check out
	-- the provided example for more details about this function use case. This function is
	-- bounds-protected, so the offset parameter is capped at the string's length, even if a
	-- Index'Last value is supplied.
	--
	-- [Params]
	--
	-- 	Str    : Reference string
	-- 	Wrap   : Wrapped version of the reference string
	-- 	Offset : UTF-8 character offset
	--
	-- [Return]
	--
	--	Converted offset in number of UTF-8 characters. If the string has errored, then 0 is
	--	always returned.
	--
	function Unwrapped_Offset (
		Str    : in T;
		Wrap   : in T;
		Offset : Index)
			return Index;

	-- Gets the number of columns. The NUL terminator and newline characters are not included.
	--
	-- [Params]
	--
	-- 	Str : String to interact with
	--
	-- [Return]
	--
	--	Number of columns. If the string has errored, then 0 is always returned.
	--
	function Width (
		Str : in T)
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

	procedure Raise_Error (Str : in T);

	-------------------------------------------------------------------------------------------------
	-- IMPORTS -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure C_Clear            (Str : System.Address);
	procedure C_Cut              (Str : System.Address; Offset : C.size_t; Length : C.size_t);
	procedure C_Destroy          (Str : System.Address);
	procedure C_Insert_Cstr      (Str : System.Address; Value : System.Address; Offset : C.size_t);
	procedure C_Insert_Double    (Str : System.Address; Value : C.double; Offset : C.size_t);
	procedure C_Insert_Long      (Str : System.Address; Value : Long_Long_Integer; Offset : C.size_t);
	procedure C_Insert_Raw       (Str : System.Address; Value : C.Strings.chars_ptr; Offset : C.size_t);
	procedure C_Pad              (Str : System.Address; Pattern : C.Strings.chars_ptr; Offset : C.size_t; Length_Target : C.size_t);
	procedure C_Prealloc         (Str : System.Address; Bytes : C.size_t);
	procedure C_Repair           (Str : System.Address);
	procedure C_Set_Precision    (Str : System.Address; Value : C.int);
	procedure C_Set_Tab_Width    (Str : System.Address; Width : C.size_t);
	procedure C_Slice            (Str : System.Address; Offset : C.size_t; Length : C.size_t);
	procedure C_Trim             (Str : System.Address);
	procedure C_Wrap             (Str : System.Address; Width : C.size_t);
	procedure C_Zero             (Str : System.Address);

	function  C_Byte_Length      (Str : System.Address)                                           return C.size_t;
	function  C_Byte_Offset      (Str : System.Address; Offset : C.size_t)                        return C.size_t;
	function  C_Clone            (Str : System.Address)                                           return System.Address;
	function  C_Create                                                                            return System.Address;
	function  C_Chars            (Str : System.Address)                                           return C.Strings.chars_ptr;
	function  C_Chars_At_Coords  (Str : System.Address; Row : C.size_t; Col : C.size_t)           return C.Strings.chars_ptr;
	function  C_Chars_At_Offset  (Str : System.Address; Offset : C.size_t)                        return C.Strings.chars_ptr;
	function  C_Coords_Offset    (Str : System.Address; Row : C.size_t; Col : C.size_t)           return C.size_t;
	function  C_Error            (Str : System.Address)                                           return Error_Code;
	function  C_Height           (Str : System.Address)                                           return C.size_t;
	function  C_Length           (Str : System.Address)                                           return C.size_t;
	function  C_Test_Wrap        (Str : System.Address; Width : C.size_t)                         return C.size_t;
	function  C_Unwrapped_Offset (Str : System.Address; Wrap : System.Address; Offset : C.size_t) return C.size_t;
	function  C_Width            (Str : System.Address)                                           return C.size_t;

	pragma Import (C, C_Byte_Length,      "cstr_byte_length");
	pragma Import (C, C_Byte_Offset,      "cstr_byte_offset");
	pragma Import (C, C_Chars,            "cstr_chars");
	pragma Import (C, C_Chars_At_Coords,  "cstr_chars_at_coords");
	pragma Import (C, C_Chars_At_Offset,  "cstr_chars_at_offset");
	pragma Import (C, C_Clear,            "cstr_clear");
	pragma Import (C, C_Clone,            "cstr_clone");
	pragma Import (C, C_Coords_Offset,    "cstr_coords_offset");
	pragma Import (C, C_Create,           "cstr_create");
	pragma Import (C, C_Cut,              "cstr_cut");
	pragma Import (C, C_Destroy,          "cstr_destroy");
	pragma Import (C, C_Error,            "cstr_error");
	pragma Import (C, C_Height,           "cstr_height");
	pragma Import (C, C_Insert_Cstr,      "cstr_insert_cstr");
	pragma Import (C, C_Insert_Double,    "cstr_insert_double");
	pragma Import (C, C_Insert_Long,      "cstr_insert_long");
	pragma Import (C, C_Insert_Raw,       "cstr_insert_raw");
	pragma Import (C, C_Length,           "cstr_length");
	pragma Import (C, C_Pad,              "cstr_pad");
	pragma Import (C, C_Placeholder,      "cstr_placeholder_instance");
	pragma Import (C, C_Prealloc,         "cstr_prealloc");
	pragma Import (C, C_Repair,           "cstr_repair");
	pragma Import (C, C_Set_Precision,    "cstr_set_precision");
	pragma Import (C, C_Set_Tab_Width,    "cstr_set_tab_width");
	pragma Import (C, C_Slice,            "cstr_slice");
	pragma Import (C, C_Test_Wrap,        "cstr_test_wrap");
	pragma Import (C, C_Trim,             "cstr_trim");
	pragma Import (C, C_Unwrapped_Offset, "cstr_unwrapped_offset");
	pragma Import (C, C_Width,            "cstr_width");
	pragma Import (C, C_Wrap,             "cstr_wrap");
	pragma Import (C, C_Zero,             "cstr_zero");

end Cassette.Str;

