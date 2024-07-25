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

with Interfaces.C; use Interfaces;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package Cassette.Error is

	-------------------------------------------------------------------------------------------------
	-- TYPES ---------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Cassette errors codes. Values are represented as a bitfield to allow the stacking of multiple
	-- errors.
	--
	subtype T is C.unsigned;

	-------------------------------------------------------------------------------------------------
	-- GLOBALS -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Error codes values.
	--
	NONE     : constant T :=   0;
	INVALID  : constant T :=   1;
	OVERFLOW : constant T :=   2;
	MEMORY   : constant T :=   4;
	PARAM    : constant T :=   8;
	CONFIG   : constant T :=  16;
	XCB      : constant T :=  32;
	CAIRO    : constant T :=  64;
	MUTEX    : constant T := 128;

end Cassette.Error;
