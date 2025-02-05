-- Copyright © 2024 Fraawlen <fraawlen@posteo.net>
--
-- This file is part of the Cassette Ada (CADA) bindings library.
--
-- This library is free software; you can redistribute it and/or modify it either under the terms of the GNU
-- Lesser General Public License as published by the Free Software Foundation; either version 3.0 of the
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
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package Cassette.Color is

	-------------------------------------------------------------------------------------------------
	-- EXCEPTIONS -----------------------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	E : exception;

	-------------------------------------------------------------------------------------------------
	-- TYPES ---------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	type Channel   is new C.double         range 0.0 .. 1.0;
	type ARGB_Uint is new C.unsigned_long  range   0 .. (2 ** 32 - 1);
	type Byte      is new C.unsigned_short range   0 .. (2 **  8 - 1);

	type T is tagged private;

	-------------------------------------------------------------------------------------------------
	-- PRESETS --------------------------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	function Transparent return T;

	function White return T;

	function Black return T;

	function Red return T;

	function Green return T;

	function Blue return T;

	function Yellow return T;

	function Magenta return T;

	function Cyan return T;

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	function From_ARGB_Uint (Argb : in ARGB_Uint) return T;

	function From_RGBA (R : in Channel; G : in Channel; B : in Channel; A : in Channel := 1.0)
		return T;

	function From_RGBA (R : in Byte; G : in Byte; B : in Byte; A : in Byte := 255) return T;
		
	function From_Str (Str : in String) return T; -- Checked version: exception in case of bad format
	
	function From_Str_Unchecked (Str : in  String) return T;

	function Interpolate (Color_1 : in T; Color_2 : in T; Side : in Ratio) return T;

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function A (Color : in T) return Channel;

	function B (Color : in T) return Channel;

	function G (Color : in T) return Channel;

	function R (Color : in T) return Channel;

	function To_ARGB_Uint (Color : in T) return ARGB_Uint;

	-------------------------------------------------------------------------------------------------
	-- PRIVATE -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------
	
private

	type C_T is record
		R : aliased Channel := 0.0;
		G : aliased Channel := 0.0;
		B : aliased Channel := 0.0;
		A : aliased Channel := 0.0;
	end record
		with Convention => C_Pass_By_Copy;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	type T is tagged record
		Data : aliased C_T;
	end record;

	-------------------------------------------------------------------------------------------------
	-- IMPORTS -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function C_From_ARGB_Uint (Argb : ARGB_Uint)                                          return C_T;
	function C_From_RGBA      (R : Byte; G : Byte; B : Byte; A : Byte)                    return C_T;
	function C_From_Str       (Str : C.Strings.chars_ptr; Err : access C.Extensions.bool) return C_T;
	function C_Interpolate    (Color_1 : C_T; Color_2 : C_T; Ratio : C.double)            return C_T;
	function C_To_ARGB_Uint   (Color : C_T)                                               return ARGB_Uint;

	pragma Import (C, C_From_ARGB_Uint, "ccolor_from_argb_uint");
	pragma Import (C, C_From_RGBA,      "ccolor_from_rgba");
	pragma Import (C, C_From_Str,       "ccolor_from_str");
	pragma Import (C, C_Interpolate,    "ccolor_interpolate");
	pragma Import (C, C_To_ARGB_Uint,   "ccolor_to_argb_uint");

end Cassette.Color;
