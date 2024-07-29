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

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package Cassette.Rand is

	-------------------------------------------------------------------------------------------------
	-- TYPES ---------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Container type that keeps track of the LCG (rand48-based) state.
	--
	type T is tagged record
		State : aliased C.unsigned_long_long;
	end record;

	-- Numerics.
	--
	type Seed_Value is new C.unsigned_long_long;

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Gets the next random value bound between lim_1 and lim_2 for the given container.
	--
	-- [Params]
	--
	-- 	Rand  : Container to interact with
	-- 	Lim_1 : First bound 
	-- 	Lim_2 : Second bound
	--
	-- [Return]
	--
	-- 	Generated random value
	--
	function Next (
		Rand  : in out T;
		Lim_1 : in Float;
		Lim_2 : in Float)
			return Float;

	-- Sets the initial value of the container.
	--
	-- [Params]
	--
	-- 	Rand  : Container to interact with
	-- 	Value : Initial value to apply
	--
	procedure Seed (
		Rand  : in out T;
		Value : in Seed_Value);

	-------------------------------------------------------------------------------------------------
	-- IMPORTS -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

private

	function  C_Next (Rand : access C.unsigned_long_long; lim_1 : C.double; lim_2  : C.double) return C.double;
	procedure C_Seed (Rand : access C.unsigned_long_long; Value : C.unsigned_long_long);

	pragma Import (C, C_Next, "crand_next");
	pragma Import (C, C_Seed, "crand_seed");

end Cassette.Rand;
