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

with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package body Cassette.Color is

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function From_ARGB_Uint (Argb : in ARGB_Uint) return T
	is
		function Fn (Argb : ARGB_Uint) return T
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccolor_from_argb_uint";
	begin

		return Fn (Argb);

	end From_ARGB_Uint;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function From_RGBA (R : in Byte; G : in Byte; B : in Byte; A : in Byte := 255) return T
	is
		function Fn (R : Byte; G : Byte; B : Byte; A : Byte) return T
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccolor_from_rgba";
	begin

		return Fn (R, G, B, A);

	end From_RGBA;
		
	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function From_Str (Str : in String) return T
	is
		function Fn (Str : C.Strings.chars_ptr; Err : access bool) return T
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccolor_str";

		Cl : T;
		S  : C.Strings.chars_ptr := C.Strings.New_String (Str);
	begin

		Cl := Fn (S, NULL);
		C.Strings.Free (S);

		return Cl;

	end From_Str;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function From_Str (Str : in  String; Error : out Boolean) return T
	is
		function Fn (Str : C.Strings.chars_ptr; Err : access bool) return T
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccolor_str";
 
		B  : aliased bool;
		Cl : T;
		S  : C.Strings.chars_ptr := C.Strings.New_String (Str);

	begin

		Cl := Fn (S, B'Access);
		C.Strings.Free (S);

		return Cl;

	end From_Str;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Interpolate (Cl_1 : in T; Cl_2 : in T; Ratio : in Ratio_Value) return T
	is
		function Fn (Cl_1 : T; Cl_2 : T; Ratio : double) return T
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccolor_interpolate";
	begin

		return Fn (Cl_1, Cl_2, double (Ratio));

	end Interpolate;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function To_ARGB_Uint (Cl : in T) return ARGB_Uint
	is
		function Fn (Cl : T) return ARGB_Uint
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "ccolor_to_argb_uint";
	begin

		return Fn (Cl);

	end To_ARGB_Uint;

end Cassette.Color;
