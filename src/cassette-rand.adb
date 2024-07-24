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

pragma Ada_2012;

with Interfaces.C; use Interfaces.C;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package body Cassette.Rand is

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function Next (Self : in out T; Lim_1 : in Float; Lim_2 : in Float) return Float
	is
		function Fn (
			Keeper : access unsigned_long_long;
			lim_1  : double;
			lim_2  : double) return double
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "crand_next";
	begin

		return Float (Fn (Self.State'Access, double (lim_1), double (lim_2)));

	end Next;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Seed (Self : in out T; Value : in Seed_Value)
	is
		procedure Fn (
			Keeper : access unsigned_long_long;
			Value  :       unsigned_long_long)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "crand_seed";
	begin

		Fn (Self.State'Access, unsigned_long_long (Value));

	end Seed;

end Cassette.Rand;
