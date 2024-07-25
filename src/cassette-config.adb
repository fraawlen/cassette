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

package body Cassette.Config is

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	procedure Clone (Self : out T; Parent : in T)
	is
		function Fn (Parent : System.Address) return System.Address
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccfg_clone";
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
			     External_Name => "ccfg_create";
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
			     External_Name => "ccfg_destroy";
	begin

		Fn (Self.Data);
		Self.Data := Placeholder'Address;

	end Destroy;

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Clear_Params (Self : in out T)
	is
		procedure Fn (Data : System.Address)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccfg_clear_params";
	begin

		Fn (Self.Data);

	end Clear_Params;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Clear_Resources (Self : in out T)
	is
		procedure Fn (Data : System.Address)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccfg_clear_resources";
	begin

		Fn (Self.Data);

	end Clear_Resources;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Clear_Sources (Self : in out T)
	is
		procedure Fn (Data : System.Address)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccfg_clear_sources";
	begin

		Fn (Self.Data);

	end Clear_Sources;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Fetch (Self : in out T; Namespace : in String; Property : in String)
	is
		procedure Fn (
			Data      : System.Address;
			Namespace : C.Strings.chars_ptr;
			Property  : C.Strings.chars_ptr)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccfg_fetch";

		S1 : C.Strings.chars_ptr := C.Strings.New_String (Namespace);
		S2 : C.Strings.chars_ptr := C.Strings.New_String (Property);
	begin

		Fn (Self.Data, S1, S2);
		C.Strings.Free (S1);
		C.Strings.Free (S2);

	end Fetch;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Iterate (Self : in out T) return Boolean
	is
		function Fn (Data : System.Address) return bool
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccfg_iterate";
	begin

		return Boolean (Fn (Self.Data));

	end Iterate;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Load (Self : in out T)
	is
		procedure Fn (Data : System.Address)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccfg_load";
	begin
	
		Fn (Self.Data);
		Self.Check;

	end Load;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Push_Param (Self : in out T; Name : in String; Value : in Float)
	is
		procedure Fn (Data : System.Address; Name : C.Strings.chars_ptr; Val : double)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccfg_push_param_double";

		S : C.Strings.chars_ptr := C.Strings.New_String (Name);
	begin

		Fn (Self.Data, S, double (Value));
		C.Strings.Free (S);
		Self.Check;

	end Push_Param;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Push_Param (Self : in out T; Name : in String; Value : in Integer)
	is
		procedure Fn (Data : System.Address; Name : C.Strings.chars_ptr; Val : Long_Long_Integer)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccfg_push_param_long";

		S : C.Strings.chars_ptr := C.Strings.New_String (Name);
	begin

		Fn (Self.Data, S, Long_Long_Integer (Value));
		C.Strings.Free (S);
		Self.Check;

	end Push_Param;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Push_Param (Self : in out T; Name : in String; Value : in String)
	is
		procedure Fn (Data : System.Address; Name : C.Strings.chars_ptr; Val : C.Strings.chars_ptr)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccfg_push_param_str";

		S1 : C.Strings.chars_ptr := C.Strings.New_String (Name);
		S2 : C.Strings.chars_ptr := C.Strings.New_String (Value);
	begin

		Fn (Self.Data, S1, S2);
		C.Strings.Free (S1);
		C.Strings.Free (S2);
		Self.Check;

	end Push_Param;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Push_Source (Self : in out T; Filename : in String)
	is
		procedure Fn (Data : System.Address; Filename : C.Strings.chars_ptr)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccfg_push_source";

		S : C.Strings.chars_ptr := C.Strings.New_String (Filename);
	begin

		Fn (Self.Data, S);
		C.Strings.Free (S);
		Self.Check;

	end Push_Source;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Repair (Self : in out T)
	is
		procedure Fn (Data : System.Address)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccfg_repair";
	begin
	
		Fn (Self.Data);

	end Repair;
	
	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function Can_Open_Sources (Self : in T) return Boolean
	is
		function Fn (Data : System.Address; I : access size_t) return bool
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccfg_can_open_sources";
	begin

		return Boolean (Fn (Self.Data, NULL));

	end Can_Open_Sources;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Can_Open_Sources (Self : in T; I : out Index) return Boolean
	is
		function Fn (Data : System.Address; I : access size_t) return bool
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccfg_can_open_sources";

		B : bool;
		J : aliased size_t;
	begin

		B := Fn    (Self.Data, J'Access);
		I := Index (J);

		return Boolean (B);

	end Can_Open_Sources;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Error (Self : in T) return Cassette.Error.T
	is
		function Fn (Data : System.Address) return unsigned
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccfg_error";
	begin

		return Fn (Self.Data);

	end Error;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Resource (Self : in T) Return String
	is
		function Fn (Data : System.Address) return C.Strings.chars_ptr
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccfg_resource";
	begin

		return C.Strings.Value (Fn (Self.Data));

	end Resource;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Resource_Length (Self : in T) return Size
	is
		function Fn (Data : System.Address) return size_t
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccfg_resource_length";
	begin

		return Fn (Self.Data);

	end Resource_Length;

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

end Cassette.Config;
