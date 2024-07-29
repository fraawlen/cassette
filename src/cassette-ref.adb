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
with System;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package body Cassette.Ref is

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	procedure Clone (Ref : out T; Parent : in T)
	is begin

		Ref.Data := C_Clone (Parent.Data);
		Ref.Raise_Error;

	end Clone;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Create (Ref : out T)
	is begin

		Ref.Data := C_Create;
		Ref.Raise_Error;

	end Create;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Destroy (Ref : in out T)
	is begin

		C_Destroy (Ref.Data);
		Ref.Data := C_Placeholder'Address;

	end Destroy;

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Clear (Ref : in out T)
	is begin

		C_Clear (Ref.Data);

	end Clear;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Prealloc (Ref : in out T; Slots : in SIze)
	is begin

		C_Prealloc (Ref.Data, Slots);
		Ref.Raise_Error;

	end Prealloc;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Pull (Ref : in out T; I : in Index)
	is begin

		C_Pull_Index (Ref.Data, I);

	end Pull;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Pull (Ref : in out T; Addr : in System.Address)
	is begin

		C_Pull_Ptr (Ref.Data, Addr);

	end Pull;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Purge (Ref : in out T; I : in Index)
	is begin

		C_Purge_Index (Ref.Data, I);

	end Purge;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Purge (Ref : in out T; Addr : in System.Address)
	is begin

		C_Purge_Ptr (Ref.Data, Addr);

	end Purge;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Push (Ref : in out T; Addr : in System.Address)
	is begin

		C_Push (Ref.Data, Addr);
		Ref.Raise_Error;

	end Push;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Repair (Ref : in out T)
	is begin

		C_Repair (Ref.Data);

	end Repair;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Set_Default_Address (Ref : in out T; Addr : in System.Address)
	is begin

		C_Set_Default_Ptr (Ref.Data, Addr);

	end Set_Default_Address;

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function Address (Ref : in T; I : in Index) return System.Address
	is begin

		return C_Ptr (Ref.Data, I);

	end Address;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Count (Ref : in T; I : in Index) return Counter
	is begin

		return Counter (C_Count (Ref.Data, I));

	end Count;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Error (Ref : in T) return Error_Code
	is begin

		return C_Error (Ref.Data);

	end Error;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Find (Ref : in T; Addr : in System.Address) return Boolean
	is begin

		return Boolean (C_Find (Ref.Data, Addr, NULL));

	end Find;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Find (Ref : in T; Addr : in System.Address; I : out Index) return Boolean
	is
		B : C.Extensions.bool;
		J : aliased C.size_t;
	begin

		B := C_Find (Ref.Data, Addr, J'Access);
		I := J;

		return Boolean (B);

	end Find;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Length (Ref : in T) return Size
	is begin

		return C_Length (Ref.Data);

	end Length;

	-------------------------------------------------------------------------------------------------
	-- PRIVATE METHODS ------------------------------------------------------------------------------ 
	-------------------------------------------------------------------------------------------------

	procedure Raise_Error (Ref : in T)
	is begin

		case Ref.Error
		is
			when Error_None => null;
			when others     => raise E;
		end case;

	end Raise_Error;

end Cassette.Ref;

