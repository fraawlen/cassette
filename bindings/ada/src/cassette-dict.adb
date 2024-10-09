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

package body Cassette.Dict is

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	procedure Clone (Dict : out T; Parent : in T)
	is begin

		Dict.Data := C_Clone (Parent.Data);
		Dict.Raise_Error;

	end Clone;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Create (Dict : out T)
	is begin

		Dict.Data := C_Create;
		Dict.Raise_Error;

	end Create;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Destroy (Dict : in out T)
	is begin

		C_Destroy (Dict.Data);
		Dict.Data := C_Placeholder'Address;

	end Destroy;

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Clear (Dict : in out T)
	is begin

		C_Clear (Dict.Data);

	end Clear;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Clear_Group (Dict : in out T; Group : in Index)
	is begin

		C_Clear_Group (Dict.Data, Group);

	end Clear_Group;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Erase (Dict : in out T; Key : in String; Group : in Index)
	is
		S : C.Strings.chars_ptr := C.Strings.New_String (Key);
	begin

		C_Erase (Dict.Data, S, Group);
		C.Strings.Free (S);

	end Erase;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Prealloc (Dict : in out T; Slots : in Size)
	is begin

		C_Prealloc (Dict.Data, Slots);
		Dict.Raise_Error;

	end Prealloc;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Set_Max_Load (Dict : in out T; Load_Factor : in Ratio)
	is begin

		C_Set_Max_Load (Dict.Data, C.double (Load_Factor));
		Dict.Raise_Error;

	end Set_Max_Load;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Repair (Dict : in out T)
	is begin

		C_Repair (Dict.Data);

	end Repair;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Write (Dict : in out T; Key : in String; Group : in Index; Value : in Index)
	is
		S : C.Strings.chars_ptr := C.Strings.New_String (Key);
	begin

		C_Write (Dict.Data, S, Group, Value);
		C.Strings.Free (S);
		Dict.Raise_Error;

	end Write;

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function Error (Dict : in T) return Error_Code
	is begin

		return C_Error (Dict.Data);

	end Error;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Find (Dict : in T; Key : in String; Group : in Index) return Boolean
	is
		B : C.Extensions.bool;
		S : C.Strings.chars_ptr := C.Strings.New_String (Key);
	begin

		B := C_Find (Dict.Data, S, Group, NULL);
		C.Strings.Free (S);

		return Boolean (B);

	end Find;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Find (Dict : in T; Key : in String; Group : in Index; Value : out Index) return Boolean
	is
		B : C.Extensions.bool;
		G : aliased C.size_t;
		S : C.Strings.chars_ptr := C.Strings.New_String (Key);
	begin

		B     := C_Find (Dict.Data, S, Group, G'Access);
		Value := G;
		C.Strings.Free (S);

		return Boolean (B);

	end Find;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Load (Dict : in T) return Size
	is begin

		return C_Load (Dict.Data);

	end Load;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Load_Factor (Dict : in T) return Ratio
	is begin

		return Ratio (C_Load_Factor (Dict.Data));

	end Load_Factor;

	-------------------------------------------------------------------------------------------------
	-- PRIVATE -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Raise_Error (Dict : in T)
	is begin

		case Dict.Error
		is
			when Error_None => null;
			when others     => raise E;
		end case;

	end Raise_Error;

end Cassette.Dict;
