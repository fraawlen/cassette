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

package body Cassette.Str is

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	procedure Clone (Str : out T; Parent : in T)
	is begin

		Str.Data := C_Clone (Parent.Data);
		Str.Raise_Error;

	end Clone;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Create (Str : out T)
	is begin

		Str.Data := C_Create;
		Str.Raise_Error;

	end Create;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Destroy (Str : in out T)
	is begin

		C_Destroy (Str.Data);
		Str.Data := C_Placeholder'Address;

	end Destroy;
	
	-------------------------------------------------------------------------------------------------
	-- WRAPPER METHODS ------------------------------------------------------------------------------ 
	-------------------------------------------------------------------------------------------------

	procedure Append (Str : in out T; Value : in T) is
	begin

		Str.Insert (Value, Index'Last);

	end Append;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Append (Str : in out T; Value : in Float) is
	begin

		Str.Insert (Value, Index'Last);

	end Append;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Append (Str : in out T; Value : in Integer) is
	begin

		Str.Insert (Value, Index'Last);

	end Append;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Append (Str : in out T; Value : in String) is
	begin

		Str.Insert (Value, Index'Last);

	end Append;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Prepend (Str : in out T; Value : in T) is
	begin

		Str.Insert (Value, 0);

	end Prepend;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Prepend (Str : in out T; Value : in Float) is
	begin

		Str.Insert (Value, 0);

	end Prepend;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Prepend (Str : in out T; Value : in Integer) is
	begin

		Str.Insert (Value, 0);

	end Prepend;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Prepend (Str : in out T; Value : in String) is
	begin

		Str.Insert (Value, 0);

	end Prepend;

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Clear (Str : in out T)
	is begin

		C_Clear (Str.Data);

	end Clear;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Cut (Str : in out T; Offset : in Index; Length : in Size)
	is begin

		C_Cut (Str.Data, Offset, Length);

	end Cut;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Insert (Str : in out T; Value : in T; Offset : in Index)
	is begin

		C_Insert_Cstr (Str.Data, Value.Data, Offset);
		Str.Raise_Error;

	end Insert;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Insert (Str : in out T; Value : in Float; Offset : in Index)
	is begin

		C_Insert_Double (Str.Data, C.double (Value), Offset);
		Str.Raise_Error;

	end Insert;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Insert (Str : in out T; Value : in Integer; Offset : in Index)
	is begin

		C_Insert_Long (Str.Data, Long_Long_Integer (Value), Offset);
		Str.Raise_Error;

	end Insert;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Insert (Str : in out T; Value : in String; Offset : in Index)
	is		
		S : C.Strings.chars_ptr := C.Strings.New_String (Value);
	begin

		C_Insert_Raw (Str.Data, S, Offset);
		C.Strings.Free (S);
		Str.Raise_Error;

	end Insert;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Pad (Str : in out T; Pattern : in String; Offset : in Index; Length_Target : in Size)
	is
		S : C.Strings.chars_ptr := C.Strings.New_String (Pattern);
	begin

		C_Pad (Str.Data, S, Offset, Length_Target);
		C.Strings.Free (S);
		Str.Raise_Error;

	end Pad;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Prealloc (Str : in out T; Bytes : in Size)
	is begin

		C_Prealloc (Str.Data, Bytes);
		Str.Raise_Error;

	end Prealloc;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Repair (Str : in out T)
	is begin

		C_Repair (Str.Data);

	end Repair;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Set_Precision (Str : in out T; Value : in Precision)
	is begin

		C_Set_Precision (Str.Data, C.int (Value));

	end Set_Precision;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Set_Tab_Width (Str : in out T; Width : in Size)
	is begin

		C_Set_Tab_Width (Str.Data, Width);

	end Set_Tab_Width;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Slice (Str : in out T; Offset : in Index; Length : in Size)
	is begin

		C_Slice (Str.Data, Offset, Length);

	end Slice;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Trim (Str : in out T)
	is begin

		C_Trim (Str.Data);

	end Trim;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Wrap (Str : in out T; Width : in Size)
	is begin

		C_Wrap (Str.Data, Width);
		Str.Raise_Error;

	end Wrap;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Zero (Str : in out T)
	is begin

		C_Zero (Str.Data);

	end Zero;

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function Byte_Length (Str : in T) return Size
	is begin

		return C_Byte_Length (Str.Data);

	end Byte_Length;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Byte_Offset (Str : in T; Offset : in Index) return Index
	is begin

		return C_Byte_Offset (Str.Data, Offset);

	end Byte_Offset;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Chars (Str : in T) return String
	is begin

		return C.Strings.Value (C_Chars (Str.Data));

	end Chars;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Chars_At_Coords (Str : in T; Row : in Index; Col : in Index) return String
	is begin

		return C.Strings.Value (C_Chars_At_Coords (Str.Data, Row, Col));

	end Chars_At_Coords;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Chars_At_Offset (Str : in T; Offset : in Index) return String
	is begin

		return C.Strings.Value (C_Chars_At_Offset (Str.Data, Offset));

	end Chars_At_Offset;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Coords_Offset (Str : in T; Row : in Index; Col : in Index) return Index
	is begin

		return C_Coords_Offset (Str.Data, Row, Col);

	end Coords_Offset;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Error (Str : in T) return Error_Code
	is begin

		return C_Error (Str.Data);

	end Error;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Height (Str : in T) return Size
	is begin

		return C_Height (Str.Data);

	end Height;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Length (Str : in T) return Size
	is begin

		return C_Length (Str.Data);

	end Length;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Test_Wrap (Str : in T; Width : in Size) return Size
	is begin

		return C_Test_Wrap (Str.Data, Width);

	end Test_Wrap;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Unwrapped_Offset (Str : in T; Wrap : in T; Offset : Index) return Index
	is begin

		return C_Unwrapped_Offset (Str.Data, Wrap.Data, Offset);

	end Unwrapped_Offset;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Width (Str : in T) return Size
	is begin

		return C_Width (Str.Data);

	end Width;

	-------------------------------------------------------------------------------------------------
	-- PRIVATE METHODS ------------------------------------------------------------------------------ 
	-------------------------------------------------------------------------------------------------

	procedure Raise_Error (Str : in T)
	is begin

		case Str.Error
		is
			when Error_None => null;
			when others     => raise E;
		end case;

	end Raise_Error;

end Cassette.Str;
