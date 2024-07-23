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

with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Cassette;
with Cassette.Error;
with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package body Cassette.Str is

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	procedure Clone (Self : out T; Parent : in T)
	is
		function Fn (Parent : System.Address) return System.Address
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_clone";
	begin

		Self.Data := Fn (Parent.Data);
		Self.Check;

	end Clone;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Create (Self : out T)
	is
		function Fn return System.Address
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_create";
	begin

		Self.Data := Fn;
		Self.Check;

	end Create;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Destroy (Self : in out T)
	is
		procedure Fn (Data : System.Address)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_destroy";
	begin

		Fn (Self.Data);
		Self.Data := Placeholder'Address;

	end Destroy;
	
	-------------------------------------------------------------------------------------------------
	-- WRAPPER METHODS ------------------------------------------------------------------------------ 
	-------------------------------------------------------------------------------------------------

	procedure Append (Self : in out T; Value : in T) is
	begin

		Self.Insert (Value, Index'Last);

	end Append;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Append (Self : in out T; Value : in Float) is
	begin

		Self.Insert (Value, Index'Last);

	end Append;
	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Append (Self : in out T; Value : in Integer) is
	begin

		Self.Insert (Value, Index'Last);

	end Append;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Append (Self : in out T; Value : in String) is
	begin

		Self.Insert (Value, Index'Last);

	end Append;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Prepend (Self : in out T; Value : in T) is
	begin

		Self.Insert (Value, 0);

	end Prepend;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Prepend (Self : in out T; Value : in Float) is
	begin

		Self.Insert (Value, 0);

	end Prepend;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Prepend (Self : in out T; Value : in Integer) is
	begin

		Self.Insert (Value, 0);

	end Prepend;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Prepend (Self : in out T; Value : in String) is
	begin

		Self.Insert (Value, 0);

	end Prepend;

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Clear (Self : in out T)
	is
		procedure Fn (Data : System.Address)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_clear";
	begin

		Fn (Self.Data);

	end Clear;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Cut (Self : in out T; Offset : in Index; Length : in Size)
	is
		procedure Fn (Data : System.Address; Offset : size_t; Length : size_t)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_cut";
	begin

		Fn (Self.Data, size_t (Offset), size_t (Length));

	end Cut;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Insert (Self : in out T; Value : in T; Offset : in Index)
	is
		procedure Fn (Data : System.Address; Value : System.Address; Offset : size_t)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_insert_cstr";
	begin

		Fn (Self.Data, Value.Data, size_t (Offset));
		Self.Check;

	end Insert;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Insert (Self : in out T; Value : in Float; Offset : in Index)
	is
		procedure Fn (Data : System.Address; Value : double; Offset : size_t)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_insert_double";
	begin

		Fn (Self.Data, double (Value), size_t (Offset));
		Self.Check;

	end Insert;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Insert (Self : in out T; Value : in Integer; Offset : in Index)
	is
		procedure Fn (Data : System.Address; Value : Long_Long_Integer; Offset : size_t)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_insert_long";
	begin

		Fn (Self.Data, Long_Long_Integer (Value), size_t (Offset));
		Self.Check;

	end Insert;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Insert (Self : in out T; Value : in String; Offset : in Index)
	is
		procedure Fn (Data : System.Address; Value : C.Strings.chars_ptr; Offset : size_t)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_insert_raw";
		
		S : C.Strings.chars_ptr := C.Strings.New_String (Value);
	begin

		Fn (Self.Data, S, size_t (Offset));
		C.Strings.Free (S);
		Self.Check;

	end Insert;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Pad (Self : in out T; Pattern : in String; Offset : in Index; Length_Target : in Size)
	is
		procedure Fn (
			Data          : System.Address;
			Pattern       : C.Strings.chars_ptr;
			Offset        : size_t;
			Length_Target : size_t)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_pad";
		
		S : C.Strings.chars_ptr := C.Strings.New_String (Pattern);
	begin

		Fn (Self.Data, S, size_t (Offset), size_t (Length_Target));
		C.Strings.Free (S);
		Self.Check;

	end Pad;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Prealloc (Self : in out T; Byte_Length : in Size)
	is
		procedure Fn (Data : System.Address; Byte_Length : size_t)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_prealloc";
	begin

		Fn (Self.Data, size_t (Byte_Length));
		Self.Check;

	end Prealloc;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Repair (Self : in out T)
	is
		procedure Fn (Data : System.Address)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_repair";
	begin

		Fn (Self.Data);

	end Repair;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Set_Precision (Self : in out T; Value : in Precision)
	is
		procedure Fn (Data : System.Address; Value : int)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_set_precision";
	begin

		Fn (Self.Data, int (Value));

	end Set_Precision;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Set_Tab_Width (Self : in out T; Width : in Size)
	is
		procedure Fn (Data : System.Address; Width : size_t)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_set_tab_width";
	begin

		Fn (Self.Data, size_t (Width));

	end Set_Tab_Width;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Slice (Self : in out T; Offset : in Index; Length : in Size)
	is
		procedure Fn (Data : System.Address; Offset : size_t; Length : size_t)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_slice";
	begin

		Fn (Self.Data, size_t (Offset), size_t (Length));

	end Slice;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Trim (Self : in out T)
	is
		procedure Fn (Data : System.Address)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_trim";
	begin

		Fn (Self.Data);

	end Trim;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Wrap (Self : in out T; Max_Width : in Size)
	is
		procedure Fn (Data : System.Address; Max_Width : size_t)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_wrap";
	begin

		Fn (Self.Data, size_t (Max_Width));
		Self.Check;

	end Wrap;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Zero (Self : in out T)
	is
		procedure Fn (Data : System.Address)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_zero";
	begin

		Fn (Self.Data);

	end Zero;

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function Byte_Length (Self : in T) return Size
	is
		function Fn (Data : System.Address) return size_t
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_byte_length";
	begin

		return Size (Fn (Self.Data));

	end Byte_Length;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Byte_Offset (Self : in T; Offset : in Index) return Index
	is
		function Fn (Data : System.Address; Offset : size_t) return size_t
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_byte_offset";
	begin

		return Index (Fn (Self.Data, size_t (Offset)));

	end Byte_Offset;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Chars (Self : in T) return String
	is
		function Fn (Data : System.Address) return C.Strings.chars_ptr
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_chars";
	begin

		return C.Strings.Value (Fn (Self.Data));

	end Chars;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Chars_At_Coords (Self : in T; Row : in Index; Col : in Index) return String
	is
		function Fn (Data : System.Address; Row : size_t; Col : size_t) return C.Strings.chars_ptr
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_chars";
	begin

		return C.Strings.Value (Fn (Self.Data, size_t (Row), size_t (Col)));

	end Chars_At_Coords;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Chars_At_Offset (Self : in T; Offset : in Index) return String
	is
		function Fn (Data : System.Address; Offset : size_t) return C.Strings.chars_ptr
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_chars";
	begin

		return C.Strings.Value (Fn (Self.Data, size_t (Offset)));

	end Chars_At_Offset;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Coords_Offset (Self : in T; Row : in Index; Col : in Index) return Index
	is
		function Fn (Data : System.Address; Row : size_t; Col : size_t) return size_t
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_coords_offset";
	begin

		return Index (Fn (Self.Data, size_t (Row), size_t (Col)));

	end Coords_Offset;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Error (Self : in T) return Cassette.Error.T
	is
		function Fn (Data : System.Address) return unsigned
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_error";
	begin

		return Cassette.Error.T (Fn (Self.Data));

	end Error;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Height (Self : in T) return Size
	is
		function Fn (Data : System.Address) return size_t
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_height";
	begin

		return Size (Fn (Self.Data));

	end Height;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Length (Self : in T) return Size
	is
		function Fn (Data : System.Address) return size_t
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_length";
	begin

		return Size (Fn (Self.Data));

	end Length;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Test_Wrap (Self : in T; Max_Width : in Size) return Size
	is
		function Fn (Data : System.Address; Max_Width : size_t) return size_t
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_test_wrap";
	begin

		return Size (Fn (Self.Data, size_t (Max_Width)));

	end Test_Wrap;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Unwrapped_Offset (Self : in T; Wrap : in T; Offset : Index) return Index
	is
		function Fn (Data : System.Address; Wrap : System.Address; Offset : size_t) return size_t
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_unwrapped_offset";
	begin

		return Index (Fn (Self.Data, Wrap.Data, size_t (Offset)));

	end Unwrapped_Offset;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Width (Self : in T) return Size
	is
		function Fn (Data : System.Address) return size_t
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cstr_width";
	begin

		return Size (Fn (Self.Data));

	end Width;

	-------------------------------------------------------------------------------------------------
	-- PRIVATE METHODS ------------------------------------------------------------------------------ 
	-------------------------------------------------------------------------------------------------

	procedure Check (Self : in T) is
	begin

		if Self.Error > 0
		then
			raise E;
		end if;

	end Check;


end Cassette.Str;
