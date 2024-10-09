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

package Cassette is

	package C renames Interfaces.C;

	-------------------------------------------------------------------------------------------------
	-- TYPES ----------------------------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	-- Common numeric types.
	--
	subtype Index is C.size_t;
	subtype Size  is C.size_t;
	subtype Ratio is Float range 0.0 .. 1.0;

	-- Cassette errors.
	--
	type Error_Code is (
		Error_None,
		Error_Invalid,
		Error_Overflow,
		Error_Memory,
		Error_Param,
		Error_Config,
		Error_Xcb,
		Error_Cairo,
		Error_Mutex,
		Error_Instance)
			with Convention => C;

	-------------------------------------------------------------------------------------------------
	-- PRIVATE --------------------------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

private

	-- Type for imports of incomplete C struct placholders.
	--
	type Placeholder is null record;

end Cassette;

