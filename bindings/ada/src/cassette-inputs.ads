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

with Interfaces; use Interfaces;
with Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package Cassette.Inputs is

	use type Interfaces.C.size_t;

	-------------------------------------------------------------------------------------------------
	-- EXCEPTIONS -----------------------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	E : exception;

	-------------------------------------------------------------------------------------------------
	-- TYPES ---------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	type T is tagged limited private;

	type Identifier is new C.unsigned;
	type Position   is new Integer_16;

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	procedure Clone (Inputs : out T; Parent : in  T);

	procedure Create (Inputs : out T; Length : in  Size) with Pre => Length > 0;

	procedure Destroy (Inputs : in out T);

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Clear (Inputs : in out T);

	procedure Pull_ID (Inputs : in out T; ID : in Identifier);

	procedure Pull_Index (Inputs : in out T; I : in Index);

	procedure Push (Inputs : in out T; ID : in Identifier; X : in Position := 0; Y : in Position := 0; Addr : in System.Address := System.Null_Address);

	procedure Repair (Inputs : in out T);

	procedure Resize (Inputs : in out T; Length : in Size) with Pre => Length > 0;

	procedure Set_Default_Address ( Inputs : in out T; Addr : in System.Address);

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function Address (Inputs : in T; I : in Index) return System.Address;

	function Error (Inputs : in T) return Error_Code;

	function Find (Inputs : in T; ID : in Identifier) return Boolean;
	
	function Find (Inputs : in  T; ID : in  Identifier; I : out Index) return Boolean;

	function ID (Inputs : in T; I : in Index) return Identifier;

	function Load (Inputs : in T) return Size;

	function X (Inputs : in T; I : in Index) return Position;

	function Y (Inputs : in T; I : in Index) return Position;

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

	procedure Raise_Error (Inputs : in T);

	-------------------------------------------------------------------------------------------------
	-- IMPORTS -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure C_Clear           (Inputs : System.Address);
	procedure C_Destroy         (Inputs : System.Address);
	procedure C_Pull_ID         (Inputs : System.Address; ID : C.unsigned);
	procedure C_Pull_Index      (Inputs : System.Address; I : C.size_t);
	procedure C_Push            (Inputs : System.Address; ID : C.unsigned; X : Integer_16; Y : Integer_16; Ptr : System.Address);
	procedure C_Repair          (Inputs : System.Address);
	procedure C_Resize          (Inputs : System.Address; Length : C.size_t);
	procedure C_Set_Default_Ptr (Inputs : System.Address; Ptr : System.Address);

	function  C_Clone           (Inputs : System.Address)                                       return System.Address;
	function  C_Create          (Length : C.size_t)                                             return System.Address;
	function  C_Error           (Inputs : System.Address)                                       return Error_Code;
	function  C_Find            (Inputs : System.Address; ID : C.unsigned; I : access C.size_t) return C.Extensions.bool;
	function  C_ID              (Inputs : System.Address; I : C.size_t)                         return C.unsigned;
	function  C_Load            (Inputs : System.Address)                                       return C.size_t;
	function  C_Ptr             (Inputs : System.Address; I : C.size_t)                         return System.Address;
	function  C_X               (Inputs : System.Address; I : C.size_t)                         return Integer_16;
	function  C_Y               (Inputs : System.Address; I : C.size_t)                         return Integer_16;

	pragma Import (C, C_Ptr,             "cinputs_ptr");
	pragma Import (C, C_Clear,           "cinputs_clear");
	pragma Import (C, C_Clone,           "cinputs_clone");
	pragma Import (C, C_Create,          "cinputs_create");
	pragma Import (C, C_Destroy,         "cinputs_destroy");
	pragma Import (C, C_Error,           "cinputs_error");
	pragma Import (C, C_Find,            "cinputs_find");
	pragma Import (C, C_ID,              "cinputs_id");
	pragma Import (C, C_Load,            "cinputs_load");
	pragma Import (C, C_Placeholder,     "cinputs_placeholder_instance");
	pragma Import (C, C_Pull_ID,         "cinputs_pull_id");
	pragma Import (C, C_Pull_Index,      "cinputs_pull_index");
	pragma Import (C, C_Push,            "cinputs_push");
	pragma Import (C, C_Repair,          "cinputs_repair");
	pragma Import (C, C_Resize,          "cinputs_resize");
	pragma Import (C, C_Set_Default_Ptr, "cinputs_set_default_ptr");
	pragma Import (C, C_X,               "cinputs_x");
	pragma Import (C, C_Y,               "cinputs_y");

end Cassette.Inputs;
