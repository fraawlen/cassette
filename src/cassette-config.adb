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

package body Cassette.Config is

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	procedure Clone (Cfg : out T; Parent : in T)
	is begin

		Cfg.Data := C_Clone (Parent.Data);
		Cfg.Raise_Error;

	end Clone;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Create (Cfg : out T)
	is begin

		Cfg.Data := C_Create;
		Cfg.Raise_Error;

	end Create;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Destroy (Cfg : in out T)
	is begin

		C_Destroy (Cfg.Data);
		Cfg.Data := C_Placeholder'Address;

	end Destroy;

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Clear_Params (Cfg : in out T)
	is begin

		C_Clear_Params (Cfg.Data);

	end Clear_Params;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Clear_Resources (Cfg : in out T)
	is begin

		C_Clear_Resources (Cfg.Data);

	end Clear_Resources;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Clear_Sources (Cfg : in out T)
	is begin

		C_Clear_Sources (Cfg.Data);

	end Clear_Sources;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Fetch (Cfg : in out T; Namespace : in String; Property : in String)
	is
		S1 : C.Strings.chars_ptr := C.Strings.New_String (Namespace);
		S2 : C.Strings.chars_ptr := C.Strings.New_String (Property);
	begin

		C_Fetch (Cfg.Data, S1, S2);
		C.Strings.Free (S1);
		C.Strings.Free (S2);

	end Fetch;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Iterate (Cfg : in out T) return Boolean
	is begin

		return Boolean (C_Iterate (Cfg.Data));

	end Iterate;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Load (Cfg : in out T)
	is begin

		C_Load (Cfg.Data);
		Cfg.Raise_Error;

	end Load;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Load_Internal (Cfg : in out T; Buffer : in String)
	is
		S : C.Strings.chars_ptr := C.Strings.New_String (Buffer);
	begin

		C_Load_Internal (Cfg.Data, S);
		C.Strings.Free (S);
		Cfg.Raise_Error;

	end Load_Internal;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Push_Param (Cfg : in out T; Name : in String; Value : in Float)
	is
		S : C.Strings.chars_ptr := C.Strings.New_String (Name);
	begin

		C_Push_Param_Double (Cfg.Data, S, C.double (Value));
		C.Strings.Free (S);
		Cfg.Raise_Error;

	end Push_Param;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Push_Param (Cfg : in out T; Name : in String; Value : in Integer)
	is
		S : C.Strings.chars_ptr := C.Strings.New_String (Name);
	begin

		C_Push_Param_Long (Cfg.Data, S, Long_Long_Integer (Value));
		C.Strings.Free    (S);
		Cfg.Raise_Error;

	end Push_Param;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Push_Param (Cfg : in out T; Name : in String; Value : in String)
	is
		S1 : C.Strings.chars_ptr := C.Strings.New_String (Name);
		S2 : C.Strings.chars_ptr := C.Strings.New_String (Value);
	begin

		C_Push_Param_Str (Cfg.Data, S1, S2);
		C.Strings.Free (S1);
		C.Strings.Free (S2);
		Cfg.Raise_Error;

	end Push_Param;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Push_Source (Cfg : in out T; Filename : in String)
	is
		S : C.Strings.chars_ptr := C.Strings.New_String (Filename);
	begin

		C_Push_Source (Cfg.Data, S);
		C.Strings.Free (S);
		Cfg.Raise_Error;

	end Push_Source;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Repair (Cfg : in out T)
	is begin

		C_Repair (Cfg.Data);

	end Repair;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Restrict (Cfg : in out T)
	is begin

		C_Restrict (Cfg.Data);

	end Restrict;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Unrestrict (Cfg : in out T)
	is begin

		C_Unrestrict (Cfg.Data);

	end Unrestrict;

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function Can_Open_Sources (Cfg : in T) return Boolean
	is begin

		return Boolean (C_Can_Open_Sources (Cfg.Data, NULL));

	end Can_Open_Sources;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Can_Open_Sources (Cfg : in T; Rank : out Index) return Boolean
	is
		B : C.Extensions.bool;
		I : aliased C.size_t;
	begin

		B    := C_Can_Open_Sources (Cfg.Data, I'Access);
		Rank := Index (I);

		return Boolean (B);

	end Can_Open_Sources;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Error (Cfg : in T) return Error_Code
	is begin

		return C_Error (Cfg.Data);

	end Error;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Resource (Cfg : in T) Return String
	is begin

		return C.Strings.Value (C_Resource (Cfg.Data));

	end Resource;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Resource_Length (Cfg : in T) return Size
	is begin

		return C_Resource_Length (Cfg.Data);

	end Resource_Length;

	-------------------------------------------------------------------------------------------------
	-- PRIVATE METHODS ------------------------------------------------------------------------------ 
	-------------------------------------------------------------------------------------------------

	procedure Raise_Error (Cfg : in T)
	is begin

		case Cfg.Error
		is
			when Error_None => null;
			when others     => raise E;
		end case;

	end Raise_Error;

end Cassette.Config;
