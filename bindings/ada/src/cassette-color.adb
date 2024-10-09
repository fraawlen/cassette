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

package body Cassette.Color is

	-------------------------------------------------------------------------------------------------
	-- PRESETS --------------------------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	function Transparent return T
	is
		Cl : T;
	begin

		Cl.Data.R := 0.0;
		Cl.Data.G := 0.0;
		Cl.Data.B := 0.0;
		Cl.Data.A := 0.0;

		return Cl;

	end Transparent;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function White return T
	is
		Cl : T;
	begin

		Cl.Data.R := 1.0;
		Cl.Data.G := 1.0;
		Cl.Data.B := 1.0;
		Cl.Data.A := 1.0;

		return Cl;

	end White;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Black return T
	is
		Cl : T;
	begin

		Cl.Data.R := 0.0;
		Cl.Data.G := 0.0;
		Cl.Data.B := 0.0;
		Cl.Data.A := 1.0;

		return Cl;

	end Black;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Red return T
	is
		Cl : T;
	begin

		Cl.Data.R := 1.0;
		Cl.Data.G := 0.0;
		Cl.Data.B := 0.0;
		Cl.Data.A := 1.0;

		return Cl;

	end Red;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Green return T
	is
		Cl : T;
	begin

		Cl.Data.R := 0.0;
		Cl.Data.G := 1.0;
		Cl.Data.B := 0.0;
		Cl.Data.A := 1.0;

		return Cl;

	end Green;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Blue return T
	is
		Cl : T;
	begin

		Cl.Data.R := 0.0;
		Cl.Data.G := 0.0;
		Cl.Data.B := 1.0;
		Cl.Data.A := 1.0;

		return Cl;

	end Blue;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Yellow return T
	is
		Cl : T;
	begin

		Cl.Data.R := 1.0;
		Cl.Data.G := 1.0;
		Cl.Data.B := 0.0;
		Cl.Data.A := 1.0;

		return Cl;

	end Yellow;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Magenta return T
	is
		Cl : T;
	begin

		Cl.Data.R := 1.0;
		Cl.Data.G := 0.0;
		Cl.Data.B := 1.0;
		Cl.Data.A := 1.0;

		return Cl;

	end Magenta;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Cyan return T
	is
		Cl : T;
	begin

		Cl.Data.R := 0.0;
		Cl.Data.G := 1.0;
		Cl.Data.B := 1.0;
		Cl.Data.A := 1.0;

		return Cl;

	end Cyan;

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	function From_ARGB_Uint (Argb : in ARGB_Uint) return T
	is
		Cl : T;
	begin

		Cl.Data := C_From_ARGB_Uint (Argb);

		return Cl;

	end From_ARGB_Uint;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function From_RGBA (R : in Channel; G : in Channel; B : in Channel; A : in Channel := 1.0)
		return T
	is
		Cl : T;
	begin

		Cl.Data.R := R;
		Cl.Data.G := G;
		Cl.Data.B := B;
		Cl.Data.A := A;

		return Cl;

	end From_RGBA;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function From_RGBA (R : in Byte; G : in Byte; B : in Byte; A : in Byte := 255) return T
	is
		Cl : T;
	begin

		Cl.Data := C_From_RGBA (R, G, B, A);

		return Cl;

	end From_RGBA;
		
	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function From_Str (Str : in  String) return T
	is
		Cl : T;
		B  : aliased C.Extensions.bool;
		S  : C.Strings.chars_ptr := C.Strings.New_String (Str);
	begin

		Cl.Data := C_From_Str (S, B'Access);
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

		Cl.Data := C_From_Str (S, NULL);
		C.Strings.Free (S);

		return Cl;

	end From_Str_Unchecked;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Interpolate (Color_1 : in T; Color_2 : in T; Side : Ratio) return T
	is
		Cl : T;
	begin

		Cl.Data := C_Interpolate (Color_1.Data, Color_2.Data, C.double (Side));

		return Cl;

	end Interpolate;

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function A (Color : in T) return Channel
	is begin
		
		return Color.Data.A;

	end A;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function B (Color : in T) return Channel
	is begin
		
		return Color.Data.B;

	end B;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function G (Color : in T) return Channel
	is begin
		
		return Color.Data.G;

	end G;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function R (Color : in T) return Channel
	is begin
		
		return Color.Data.R;

	end R;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function To_ARGB_Uint (Color : in T) return ARGB_Uint
	is begin

		return C_To_ARGB_Uint (Color.Data);

	end To_ARGB_Uint;

end Cassette.Color;
