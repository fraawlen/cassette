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

with Cassette.Error;
with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package body Cassette.Dict is

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	procedure Clone (Self : out T; Parent : in T)
	is
		function Fn (Parent : System.Address) return System.Address
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cdict_clone";
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
			     External_Name => "cdict_create";
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
			     External_Name => "cdict_destroy";
	begin

		Fn (Self.Data);
		Self.Data := Placeholder'Address;

	end Destroy;

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Clear (Self : in out T)
	is	
		procedure Fn (Data : System.Address)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cdict_clear";
	begin

		Fn (Self.Data);

	end Clear;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Clear_Group (Self : in out T; Group : in Group_Value)
	is
		procedure Fn (Data : System.Address; Group : size_t)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cdict_clear_group";
	begin

		Fn (Self.Data, Group);

	end Clear_Group;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Erase (Self : in out T; Key : in String; Group : in Group_Value)
	is
		procedure Fn (Data : System.Address; Key : C.Strings.chars_ptr; Group : size_t)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cdict_erase";

		S : C.Strings.chars_ptr := C.Strings.New_String (Key);
	begin

		Fn (Self.Data, S, Group);
		C.Strings.Free (S);

	end Erase;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Prealloc (Self : in out T; Slots_Number : in Size)
	is
		procedure Fn (Data : System.Address; Slots_Number : size_t)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cdict_prealloc";
	begin

		Fn (Self.Data, Slots_Number);
		Self.Check;

	end Prealloc;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Set_Max_Load (Self : in out T; Load_Factor : in Ratio)
	is
		procedure Fn (Data : System.Address; Load_Factor : double)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cdict_set_max_load";
	begin

		Fn (Self.Data, double (Load_Factor));
		Self.Check;

	end Set_Max_Load;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Repair (Self : in out T)
	is
		procedure Fn (Data : System.Address)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cdict_repair";
	begin

		Fn (Self.Data);

	end Repair;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Write (Self : in out T; Key : in String; Group : in Group_Value; Value : in Slot_Value)
	is
		procedure Fn (
			Data  : System.Address;
			Key   : C.Strings.chars_ptr;
			Group : size_t;
			Value : size_t)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cdict_write";

		S : C.Strings.chars_ptr := C.Strings.New_String (Key);
	begin

		Fn (Self.Data, S, Group, Value);
		C.Strings.Free (S);
		Self.Check;

	end Write;

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function Error (Self : in T) return Cassette.Error.T
	is
		function Fn (Data : System.Address) return unsigned
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cdict_error";
	begin

		return Fn (Self.Data);

	end Error;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Find (Self : in T; Key : in String; Group : in Group_Value) return Boolean
	is
		function Fn (
			Data  : System.Address;
			Key   : C.Strings.chars_ptr;
			Group : size_t;
			Value : access size_t)
				return bool
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cdict_find";

		B : bool;
		S : C.Strings.chars_ptr := C.Strings.New_String (Key);
	begin

		B := Fn (Self.Data, S, Group, NULL);
		C.Strings.Free (S);

		return Boolean (B);

	end Find;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Find (Self : in T; Key : in String; Group : in Group_Value; Value : out Slot_Value)
		return Boolean
	is
		function Fn (
			Data  : System.Address;
			Key   : C.Strings.chars_ptr;
			Group : size_t;
			Value : access size_t)
				return bool
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cdict_find";

		B : bool;
		G : aliased size_t;
		S : C.Strings.chars_ptr := C.Strings.New_String (Key);
	begin

		B     := Fn (Self.Data, S, Group, G'Access);
		Value := G;
		C.Strings.Free (S);

		return Boolean (B);

	end Find;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Load (Self : in T) return Size
	is 
		function Fn (Data : System.Address) return size_t
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cdict_load";
	begin

		return Fn (Self.Data);

	end Load;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Load_Factor (Self : in T) return Ratio
	is
		function Fn (Data : System.Address) return double
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cdict_load_factor";
	begin

		return Ratio (Fn (Self.Data));

	end Load_Factor;

	-------------------------------------------------------------------------------------------------
	-- PRIVATE -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Check (Self : in T) is
	begin

		if Self.Error > 0
		then
			raise E;
		end if;

	end Check;

end Cassette.Dict;
