-- Copyright © 2024 Fraawlen <fraawlen@posteo.net>
--
-- This file is part of the Cassette Ada (CADA) bindings library.
--
-- This library is free software; you can redistribute it and/or modify it either under the terms of the GNU
-- Lesser General Public License as published by the Free Software Foundation; either version 3.0 of the
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

	E : exception;

	-------------------------------------------------------------------------------------------------
	-- TYPES ---------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	type T is tagged limited private;

	type Precision is new C.int range 0 .. 16;

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	procedure Clone (Str : out T; Parent : in  T);

	procedure Create (Str : out T);

	procedure Destroy (Str : in out T);
	
	-------------------------------------------------------------------------------------------------
	-- WRAPPER METHODS ------------------------------------------------------------------------------ 
	-------------------------------------------------------------------------------------------------

	procedure Append (Str : in out T; Value : in T);

	procedure Append (Str : in out T; Value : in Float);

	procedure Append (Str : in out T; Value : in Integer);

	procedure Append (Str : in out T; Value : in String);

	procedure Prepend (Str : in out T; Value : in T);

	procedure Prepend (Str : in out T; Value : in Float);

	procedure Prepend (Str : in out T; Value : in Integer);

	procedure Prepend (Str : in out T; Value : in String);

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Clear (Str : in out T);

	procedure Cut (Str : in out T; Offset : in Index; Length : in Size);

	procedure Insert (Str : in out T; Value : in T; Offset : in Index);

	procedure Insert (Str : in out T; Value : in Float; Offset : in Index);

	procedure Insert (Str : in out T; Value : in Integer; Offset : in Index);

	procedure Insert (Str : in out T; Value : in String; Offset : in Index);

	procedure Pad (Str : in out T; Pattern : in String; Offset : in Index; Length_Target : in Size);

	procedure Prealloc (Str : in out T; Bytes : in Size);

	procedure Repair (Str : in out T);

	procedure Set_Precision (Str : in out T; Value : in Precision);

	procedure Slice (Str : in out T; Offset : in Index; Length : in Size);

	procedure Trim ( Str : in out T);

	procedure Wrap (Str : in out T; Width : in Size) with Pre => Width > 0;

	procedure Zero (Str : in out T);

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function Byte_Length (Str : in T) return Size;

	function Byte_Offset (Str : in T; Offset : in Index) return Index;

	function Chars (Str : in T) return String;

	function Chars_At_Coords (Str : in T; Row : in Index; Col : in Index) return String;

	function Chars_At_Offset (Str : in T; Offset : in Index) return String;

	function Coords_Offset (Str : in T; Row : in Index; Col : in Index) return Index;
 
	function Error (Str : in T) return Error_Code;

	function Height (Str : in T) return Size;

	function Length (Str : in T) return Size;

	function Test_Wrap (Str : in T; Width : in Size) return Size with Pre => Width > 0;

	function Unwrapped_Offset (Str : in T; Wrap : in T; Offset : Index) return Index;

	function Width (Str : in T) return Size;

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

