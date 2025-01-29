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

package Cassette.Dict is

	use type Interfaces.C.size_t;

	-------------------------------------------------------------------------------------------------
	-- EXCEPTIONS -----------------------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	E : exception;

	-------------------------------------------------------------------------------------------------
	-- TYPES ---------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	type T is tagged limited private;

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	procedure Clone (Dict : out T; Parent : in  T);

	procedure Create (Dict : out T);

	procedure Destroy (Dict : in out T);

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Clear (Dict : in out T);

	procedure Clear_Group (Dict : in out T; Group : in Index);

	procedure Erase (Dict : in out T; Key : in String; Group : in Index);

	procedure Prealloc (Dict : in out T; Slots : in Size);

	procedure Set_Max_Load (Dict : in out T; Load_Factor : in Ratio) with Pre => Load_Factor > 0.0;

	procedure Repair (Dict : in out T);

	procedure Write (Dict : in out T; Key : in String; Group : in Index; Value : in Index);

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function Error (Dict : in T) return Error_Code;

	function Find (Dict : in T; Key : in String; Group : in Index) return Boolean;

	function Find (Dict : in T; Key : in  String; Group : in  Index; Value : out Index) return Boolean;

	function Load (Dict : in T) return Size;

	function Load_Factor (Dict : in T) return Ratio;

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

	procedure Raise_Error (Dict : in T);

	-------------------------------------------------------------------------------------------------
	-- IMPORTS -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure C_Clear        (Dict : System.Address);
	procedure C_Clear_Group  (Dict : System.Address; Group : C.size_t);
	procedure C_Destroy      (Dict : System.Address);
	procedure C_Erase        (Dict : System.Address; Key : C.Strings.chars_ptr; Group : C.size_t);
	procedure C_Prealloc     (Dict : System.Address; Slots : C.size_t);
	procedure C_Set_Max_Load (Dict : System.Address; Load_Factor : C.double);
	procedure C_Repair       (Dict : System.Address);
	procedure C_Write        (Dict : System.Address; Key : C.Strings.chars_ptr; Group : C.size_t; Value : C.size_t);

	function  C_Clone        (Dict : System.Address) return System.Address;
	function  C_Create                               return System.Address;
	function  C_Error        (Dict : System.Address) return Error_Code;
	function  C_Find         (Dict : System.Address; Key : C.Strings.chars_ptr; Group : C.size_t; Value : access C.size_t) return C.Extensions.bool;
	function  C_Load         (Dict : System.Address) return C.size_t;
	function  C_Load_Factor  (Dict : System.Address) return C.double;

	pragma Import (C, C_Clear,        "cdict_clear");
	pragma Import (C, C_Clear_Group,  "cdict_clear_group");
	pragma Import (C, C_Clone,        "cdict_clone");
	pragma Import (C, C_Create,       "cdict_create");
	pragma Import (C, C_Destroy,      "cdict_destroy");
	pragma Import (C, C_Erase,        "cdict_erase");
	pragma Import (C, C_Error,        "cdict_error");
	pragma Import (C, C_Find,         "cdict_find");
	pragma Import (C, C_Load,         "cdict_load");
	pragma Import (C, C_Load_Factor,  "cdict_load_factor");	
	pragma Import (C, C_Placeholder,  "cdict_placeholder_instance");
	pragma Import (C, C_Prealloc,     "cdict_prealloc");
	pragma Import (C, C_Set_Max_Load, "cdict_set_max_load");
	pragma Import (C, C_Repair,       "cdict_repair");
	pragma Import (C, C_Write,        "cdict_write");

end Cassette.Dict;
