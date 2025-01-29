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

package Cassette.Config is

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

	procedure Clone (Cfg : out T; Parent : in  T);

	procedure Create (Cfg : out T);

	procedure Destroy (Cfg : in out T);

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Clear_Params (Cfg : in out T);

	procedure Clear_Resources (Cfg : in out T);

	procedure Clear_Sources (Cfg : in out T);

	procedure Fetch (Cfg : in out T; Namespace : in String; Property : in String);

	function  Iterate (Cfg : in out T) return Boolean;

	procedure Load (Cfg : in out T);

	procedure Load_Internal (Cfg : in out T; Buffer : in String);

	procedure Push_Param (Cfg : in out T; Name : in String; Value : in Float);

	procedure Push_Param (Cfg : in out T; Name : in String; Value : in Integer);

	procedure Push_Param (Cfg : in out T; Name : in String; Value : in String);

	procedure Push_Source (Cfg : in out T; Filename : in String);

	procedure Repair (Cfg : in out T);

	procedure Restrict (Cfg : in out T);

	procedure Unrestrict (Cfg : in out T);

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function Can_Open_Sources (Cfg : in T) return Boolean;

	function Can_Open_Sources (Cfg : in T; Rank : out Index) return Boolean;

	function Error (Cfg : in T) return Error_Code;

	function Resource (Cfg : in T) Return String;

	function Resource_Length (Cfg : in T) return Size;

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

	procedure Raise_Error (Cfg : in T);

	-------------------------------------------------------------------------------------------------
	-- IMPORTS -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------
	
	procedure C_CLear_Params      (Cfg : System.Address);
	procedure C_CLear_Resources   (Cfg : System.Address);
	procedure C_Clear_Sources     (Cfg : System.Address);
	procedure C_Destroy           (Cfg : System.Address);
	procedure C_Fetch             (Cfg : System.Address; Namespace : C.Strings.chars_ptr; Property : C.Strings.chars_ptr);
	procedure C_Load              (Cfg : System.Address);
	procedure C_Load_Internal     (Cfg : System.Address; Buffer : C.Strings.chars_ptr);
	procedure C_Push_Param_Double (Cfg : System.Address; Name : C.Strings.chars_ptr; Value : C.double);
	procedure C_Push_Param_Long   (Cfg : System.Address; Name : C.Strings.chars_ptr; Value : Long_Long_Integer);
	procedure C_Push_Param_Str    (Cfg : System.Address; Name : C.Strings.chars_ptr; Value : C.Strings.chars_ptr);
	procedure C_Push_Source       (Cfg : System.Address; Filename : C.Strings.chars_ptr);
	procedure C_Repair            (Cfg : System.Address);
	procedure C_Restrict          (Cfg : System.Address);
	procedure C_Unrestrict        (Cfg : System.Address);

	function  C_Can_Open_Sources  (Cfg : System.Address; Rank : access C.size_t) return C.Extensions.bool;
	function  C_Clone             (Cfg : System.Address)                         return System.Address;
	function  C_Create                                                           return System.Address;
	function  C_Error             (Cfg : System.Address)                         return Error_Code;
	function  C_Iterate           (Cfg : System.Address)                         return C.Extensions.bool;
	function  C_Resource          (Cfg : System.Address)                         return C.Strings.chars_ptr;
	function  C_Resource_Length   (Cfg : System.Address)                         return C.size_t;

	pragma Import (C, C_Clear_Params,      "ccfg_clear_params");
	pragma Import (C, C_Clear_Resources,   "ccfg_clear_resources");
	pragma Import (C, C_Clear_Sources,     "ccfg_clear_sources");
	pragma Import (C, C_Can_Open_Sources,  "ccfg_can_open_sources");
	pragma Import (C, C_Clone,             "ccfg_clone");
	pragma Import (C, C_Create,            "ccfg_create");
	pragma Import (C, C_Destroy,           "ccfg_destroy");
	pragma Import (C, C_Error,             "ccfg_error");
	pragma Import (C, C_Fetch,             "ccfg_fetch");
	pragma Import (C, C_Iterate,           "ccfg_iterate");
	pragma Import (C, C_Load,              "ccfg_load");
	pragma Import (C, C_Load_Internal,     "ccfg_load_internal");
	pragma Import (C, C_Placeholder,       "ccfg_placeholder_instance");
	pragma Import (C, C_Push_Param_Double, "ccfg_push_param_double");
	pragma Import (C, C_Push_Param_Long,   "ccfg_push_param_long");
	pragma Import (C, C_Push_Param_Str,    "ccfg_push_param_str");
	pragma Import (C, C_Push_Source,       "ccfg_push_source");
	pragma Import (C, C_Repair,            "ccfg_repair");
	pragma Import (C, C_Resource,          "ccfg_resource");
	pragma Import (C, C_Resource_Length,   "ccfg_resource_length");
	pragma Import (C, C_Restrict,          "ccfg_restrict");
	pragma Import (C, C_Unrestrict,        "ccfg_unrestrict");

end Cassette.Config;
