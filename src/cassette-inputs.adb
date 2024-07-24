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
with System;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package body Cassette.Inputs is

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS ------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Clone (Self : out T; Parent : in T)
	is
		function Fn (Parent : System.Address) return System.Address
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cinputs_clone";
	begin

		Self.Data := Fn (Parent.Data);
		Self.Check;

	end Clone;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Create (Self : out T; Max_Inputs : in Size_Input)
	is
		function Fn (Max_Inputs : size_t) return System.Address
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cinputs_create";
	begin

		Self.Data := Fn (size_t (Max_Inputs));
		Self.Check;

	end Create;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Destroy (Self : in out T)
	is
		procedure Fn (Data : System.Address)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cinputs_destroy";
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
			     External_Name => "cinputs_clear";
	begin

		Fn (Self.Data);

	end Clear;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	 procedure Pull_ID (Self : in out T; ID : in Identifier)
	 is
		procedure Fn (Data : System.Address; ID : unsigned)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cinputs_pull_id";
	 begin

		Fn (Self.Data, unsigned (ID));

	 end Pull_ID;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

 	procedure Pull_Index (Self : in out T; I : in Index)
	is
		procedure Fn (Data : System.Address; Index : size_t)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cinputs_pull_index";
	begin

		Fn (Self.Data, size_t (I));

	end Pull_Index;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Push (Self : in out T; ID : in Identifier; X : in Position := 0; Y : in Position := 0;
	                Addr : in System.Address := System.Null_Address)
	is
		procedure Fn (
			Data : System.Address; ID : unsigned; X : short; Y : short; Addr : System.Address)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cinputs_push";
	begin
	
		Fn (Self.Data, unsigned (ID), short (X), short (Y), Addr);

	end Push;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Repair (Self : in out T)
	is
		procedure Fn (Data : System.Address)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cinputs_repair";
	begin
	
		Fn (Self.Data);

	end Repair;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Resize (Self : in out T; Max_Inputs : in Size_Input)
	is
		procedure Fn (Data : System.Address; Max_Inputs : size_t)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cinputs_resize";
	begin
	
		Fn (Self.Data, size_t (Max_Inputs));
		Self.Check;

	end Resize;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Set_Default_Address (Self : in out T; Addr : in System.Address)
	is
		procedure Fn (Data : System.Address; Addr : System.Address)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cinputs_set_default_ptr";
	begin
	
		Fn (Self.Data, Addr);

	end Set_Default_Address;

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function Address (Self : in T; I : Index) return System.Address
	is
		function Fn (Data : System.Address; Index : size_t) return System.Address
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cinputs_ptr";
	begin

		return Fn (Self.Data, size_t (I));

	end Address;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Error (Self : in T) return Cassette.Error.T
	is
		function Fn (Data : System.Address) return unsigned
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cinputs_error";
	begin

		return Cassette.Error.T (Fn (Self.Data));

	end Error;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Find (Self : in T; ID : in Identifier) return Boolean
	is
		function Fn (Data : System.Address; ID : unsigned; Index : access size_t) return bool
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cinputs_find";
	begin

		return Boolean (Fn (Self.Data, unsigned (ID), NULL));

	end Find;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
	
	function Find (Self : in T; ID : in Identifier; I : out Index) return Boolean
	is
		function Fn (Data : System.Address; ID : unsigned; Index : access size_t) return bool
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cinputs_find";

		B : bool;
		J : aliased size_t;
	begin
	
		B := Fn    (Self.Data, unsigned(ID), J'Access);
		I := Index (J);

		return Boolean (B);

	end Find;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function ID (Self : in T; I : Index) return Identifier
	is
		function Fn (Data : System.Address; Index : size_t) return unsigned
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cinputs_id";
	begin

		return Identifier (Fn (Self.Data, size_t (I)));

	end ID;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Load (Self : in T) return Size
	is
		function Fn (Data : System.Address) return size_t
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cinputs_load";
	begin

		return Size (Fn (Self.Data));

	end Load;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function X (Self : in T; I : Index) return Position
	is
		function Fn (Data : System.Address; Index : size_t) return short
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cinputs_x";
	begin

		return Position (Fn (Self.Data, size_t (I)));

	end X;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Y (Self : in T; I : Index) return Position
	is
		function Fn (Data : System.Address; Index : size_t) return short
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cinputs_y";
	begin

		return Position (Fn (Self.Data, size_t (I)));

	end Y;

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

end Cassette.Inputs;
