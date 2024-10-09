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

with Interfaces;
with Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package body Cassette.Inputs is

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS ------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Clone (Inputs : out T; Parent : in T)
	is begin

		Inputs.Data := C_Clone (Parent.Data);
		Inputs.Raise_Error;

	end Clone;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Create (Inputs : out T; Length : in Size)
	is begin

		Inputs.Data := C_Create (Length);
		Inputs.Raise_Error;

	end Create;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Destroy (Inputs : in out T)
	is begin

		C_Destroy (Inputs.Data);
		Inputs.Data := C_Placeholder'Address;

	end Destroy;

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Clear (Inputs : in out T)
	is begin

		C_Clear (Inputs.Data);

	end Clear;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	 procedure Pull_ID (Inputs : in out T; ID : in Identifier)
	 is begin

		C_Pull_ID (Inputs.Data, C.unsigned (ID));

	 end Pull_ID;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

 	procedure Pull_Index (Inputs : in out T; I : in Index)
	is begin

		C_Pull_Index (Inputs.Data, I);

	end Pull_Index;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Push (Inputs : in out T; ID : in Identifier; X : in Position := 0;
	                Y : in Position := 0; Addr : in System.Address := System.Null_Address)
	is begin
	
		C_Push (Inputs.Data, C.unsigned (ID), Integer_16 (X), Integer_16 (Y), Addr);

	end Push;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Repair (Inputs : in out T)
	is begin
	
		C_Repair (Inputs.Data);

	end Repair;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Resize (Inputs : in out T; Length : in Size)
	is begin
	
		C_Resize (Inputs.Data, Length);
		Inputs.Raise_Error;

	end Resize;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Set_Default_Address (Inputs : in out T; Addr : in System.Address)
	is begin
	
		C_Set_Default_Ptr (Inputs.Data, Addr);

	end Set_Default_Address;

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function Address (Inputs : in T; I : Index) return System.Address
	is begin

		return C_Ptr (Inputs.Data, I);

	end Address;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Error (Inputs : in T) return Error_Code
	is begin

		return C_Error (Inputs.Data);

	end Error;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Find (Inputs : in T; ID : in Identifier) return Boolean
	is begin

		return Boolean (C_Find (Inputs.Data, C.unsigned (ID), NULL));

	end Find;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
	
	function Find (Inputs : in T; ID : in Identifier; I : out Index) return Boolean
	is
		B : C.Extensions.bool;
		J : aliased C.size_t;
	begin
	
		B := C_Find (Inputs.Data, C.unsigned (ID), J'Access);
		I := J;

		return Boolean (B);

	end Find;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function ID (Inputs : in T; I : Index) return Identifier
	is begin

		return Identifier (C_ID (Inputs.Data, I));

	end ID;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Load (Inputs : in T) return Size
	is begin

		return C_Load (Inputs.Data);

	end Load;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function X (Inputs : in T; I : Index) return Position
	is begin

		return Position (C_X (Inputs.Data, I));

	end X;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Y (Inputs : in T; I : Index) return Position
	is begin

		return Position (C_Y (Inputs.Data, I));

	end Y;

	-------------------------------------------------------------------------------------------------
	-- PRIVATE METHODS ------------------------------------------------------------------------------ 
	-------------------------------------------------------------------------------------------------

	procedure Raise_Error (Inputs : in T)
	is begin

		case Inputs.Error
		is
			when Error_None => null;
			when others     => raise E;
		end case;

	end Raise_Error;

end Cassette.Inputs;
