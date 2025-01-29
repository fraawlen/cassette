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
with System;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package Cassette.Ref is

	-------------------------------------------------------------------------------------------------
	-- EXCEPTIONS -----------------------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	E : exception;

	-------------------------------------------------------------------------------------------------
	-- TYPES ---------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	type T is tagged limited private;

	type Counter is new C.unsigned;

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	procedure Clone (Ref : out T; Parent : in  T);

	procedure Create (Ref : out T);
		
	procedure Destroy (Ref : in out T);

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Clear (Ref : in out T);

	procedure Prealloc (Ref : in out T; Slots : in SIze);

	procedure Pull (Ref : in out T; I : in Index);

	procedure Pull (Ref : in out T; Addr : in System.Address);

	procedure Purge (Ref : in out T; I : in Index);

	procedure Purge (Ref : in out T; Addr : in System.Address);

	procedure Push (Ref : in out T; Addr : in System.Address);

	procedure Repair (Ref : in out T);

	procedure Set_Default_Address (Ref : in out T; Addr : in System.Address);

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function Address (Ref : in T; I : in Index) return System.Address;

	function Count (Ref : in T; I : in Index) return Counter;

	function Error (Ref : in T) return Error_Code;

	function Find (Ref : in T; Addr : in System.Address) return Boolean;

	function Find (Ref : in T; Addr : in System.Address; I : out Index) return Boolean;

	function Length (Ref : in T) return Size;

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

	procedure Raise_Error (Ref : in T);

	-------------------------------------------------------------------------------------------------
	-- IMPORTS -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure C_Clear           (Ref : System.Address);
	procedure C_Destroy         (Ref : System.Address);
	procedure C_Prealloc        (Ref : System.Address; Slots : C.size_t);
	procedure C_Pull_Index      (Ref : System.Address; Index : C.size_t);
	procedure C_Pull_Ptr        (Ref : System.Address; Ptr : System.Address);
	procedure C_Purge_Index     (Ref : System.Address; Index : C.size_t);
	procedure C_Purge_Ptr       (Ref : System.Address; Ptr : System.Address);
	procedure C_Push            (Ref : System.Address; Ptr : System.Address);
	procedure C_Repair          (Ref : System.Address);
	procedure C_Set_Default_Ptr (Ref : System.Address; Addr : System.Address);

	function  C_Clone           (Ref : System.Address)                                             return System.Address;
	function  C_Count           (Ref : System.Address; I : C.size_t)                               return C.unsigned;
	function  C_Create                                                                             return System.Address;
	function  C_Error           (Ref : System.Address)                                             return Error_Code;
	function  C_Find            (Ref : System.Address; Addr : System.Address; I : access C.size_t) return C.Extensions.bool;
	function  C_Length          (Ref : System.Address)                                             return C.size_t;
	function  C_Ptr             (Ref : System.Address; I : C.size_t)                               return System.Address;

	pragma Import (C, C_Clear,           "cref_clear");
	pragma Import (C, C_Clone,           "cref_clone");
	pragma Import (C, C_Count,           "cref_cpunt");
	pragma Import (C, C_Create,          "cref_create");
	pragma Import (C, C_Destroy,         "cref_destroy");
	pragma Import (C, C_Error,           "cref_error");
	pragma Import (C, C_Find,            "cref_find");
	pragma Import (C, C_Length,          "cref_length");
	pragma Import (C, C_Placeholder,     "cref_placeholder_instance");
	pragma Import (C, C_Prealloc,        "cref_prealloc");
	pragma Import (C, C_Ptr,             "cref_ptr");
	pragma Import (C, C_Pull_Index,      "cref_pull_index");
	pragma Import (C, C_Pull_Ptr,        "cref_pull_ptr");
	pragma Import (C, C_Purge_Index,     "cref_purge_index");
	pragma Import (C, C_Purge_Ptr,       "cref_purge_ptr");
	pragma Import (C, C_Push,            "cref_push");
	pragma Import (C, C_Repair,          "cref_repair");
	pragma Import (C, C_Set_Default_Ptr, "cref_set_default_ptr");

end Cassette.Ref;
