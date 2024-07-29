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

with Ada.Text_IO;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package body Cassette.Color is

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function From_ARGB_Uint (Argb : in ARGB_Uint) return T
	is begin

		return C_From_ARGB_Uint (Argb);

	end From_ARGB_Uint;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function From_RGBA (R : in Byte; G : in Byte; B : in Byte; A : in Byte := 255) return T
	is begin

		return C_From_RGBA (R, G, B, A);

	end From_RGBA;
		
	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function From_Str (Str : in  String) return T
	is
		B  : aliased C.Extensions.bool;
		Cl : T;
		S  : C.Strings.chars_ptr := C.Strings.New_String (Str);
	begin

		Cl := C_From_Str (S, B'Access);
		C.Strings.Free (S);

		if B
		then
			raise E;
		end if;

		return Cl;

	end From_Str;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function From_Str_Unchecked (Str : in String) return T
	is
		Cl : T;
		S  : C.Strings.chars_ptr := C.Strings.New_String (Str);
	begin

		Cl := C_From_Str (S, NULL);
		C.Strings.Free (S);

		return Cl;

	end From_Str_Unchecked;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Interpolate (Color_1 : in T; Color_2 : in T; Side : Ratio) return T
	is begin

		return C_Interpolate (Color_1, Color_2, C.double (Side));

	end Interpolate;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function To_ARGB_Uint (Color : in T) return ARGB_Uint
	is begin

		return C_To_ARGB_Uint (Color);

	end To_ARGB_Uint;

end Cassette.Color;
