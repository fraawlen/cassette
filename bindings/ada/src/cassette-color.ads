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

	-- Exception that gets raised when an impure method fails.
	--
	E : exception;

	-------------------------------------------------------------------------------------------------
	-- TYPES ---------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	--  Numerics.
	--
	type Channel   is new C.double         range 0.0 .. 1.0;
	type ARGB_Uint is new C.unsigned_long  range   0 .. (2 ** 32 - 1);
	type Byte      is new C.unsigned_short range   0 .. (2 **  8 - 1);

	-- Represention of a RGBA color.
	--
	type T is tagged private;

	-------------------------------------------------------------------------------------------------
	-- PRESETS --------------------------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	-- Creates a color from a preset.
	--
	-- [Return]
	--
	--	Color.
	--
	function Transparent return T;
	function White       return T;
	function Black       return T;
	function Red         return T;
	function Green       return T;
	function Blue        return T;
	function Yellow      return T;
	function Magenta     return T;
	function Cyan        return T;

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	-- Converts a 32-bits ARGB color representation into a color object.
	--
	-- [Params]
	--
	-- 	Argb : Color uint to convert
	--
	-- [Return]
	--
	-- 	Color.
	--
	function From_ARGB_Uint (
		Argb : in ARGB_Uint)
			return T;

	-- Converts a RGBA color representation into a color object.
	--
	-- @param R : Red   color component
	-- @param G : Green color component
	-- @param B : Blue  color component
	-- @param A : Alpha color component, Optional
	--
	-- [Return]
	--
	-- 	Color.
	--
	function From_RGBA (
		R : in Channel;
		G : in Channel;
		B : in Channel;
		A : in Channel := 1.0)
			return T;

	-- Converts a RGBA color representation with channels represented as bytes into a color object.
	--
	-- @param R : Red   color component
	-- @param G : Green color component
	-- @param B : Blue  color component
	-- @param A : Alpha color component, Optional
	--
	-- [Return]
	--
	-- 	Color.
	--
	function From_RGBA (
		R : in Byte;
		G : in Byte;
		B : in Byte;
		A : in Byte := 255)
			return T;
		
	-- Converts a string into a color object. The given string is interpreted as a char
	-- representation of an unsigned 32-bit ARGB value (which will get converted with C's stroul();
	-- see the documentation of this function for more information), unless there is a leading '#'
	-- character, in which case the string gets interpreted as a 6-8 digit "#rrggbbaa" hex. In this
	-- representation, if the optional alpha parameter is omitted, a 0xFF value is assumed. 
	--
	-- Unlike the unchecked version of this function, if there is a formatting error in the given
	-- string, an exception will be raised.
	--
	-- [Params]
	--
	--	Str : String to convert
	--
	-- [Return]
	--
	-- 	Color.
	--
	-- [Errors]
	--
	--	Error_Param : Invalid input string
	--
	function From_Str (
		Str : in String)
			return T;
	
	-- Converts a string into a color object. The given string is interpreted as a char
	-- representation of an unsigned 32-bit ARGB value (which will get converted with C's stroul();
	-- see the documentation of this function for more information), unless there is a leading '#'
	-- character, in which case the string gets interpreted as a 6-8 digit "#rrggbbaa" hex. In this
	-- representation, if the optional alpha parameter is omitted, a 0xFF value is assumed. 
	--
	-- [Params]
	--
	--	Str  : String to convert
	--
	-- [Return]
	--
	-- 	Color.
	--
	function From_Str_Unchecked (
		Str : in  String)
			return T;

	-- Interpolates a color between two given colors.
	--
	-- [Params]
	--
	-- 	Color_1 : First  color
	-- 	Color_2 : Second color
	-- 	Ratio   : Second / first color ratio
	--
	-- [Return]
	--
	-- 	Color.
	--
	function Interpolate (
		Color_1 : in T;
		Color_2 : in T;
		Side    : in Ratio)
			return T;

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Gets the alpha color component.
	--
	-- [Param]
	--
	--	Color : Color to interact with
	--
	-- [Return]
	--
	--	Channel color value.
	--
	function A (
		Color : in T)
			return Channel;

	-- Gets the blue color component.
	--
	-- [Param]
	--
	--	Color : Color to interact with
	--
	-- [Return]
	--
	--	Channel color value.
	--
	function B (
		Color : in T)
			return Channel;

	-- Gets the green color component.
	--
	-- [Param]
	--
	--	Color : Color to interact with
	--
	-- [Return]
	--
	--	Channel color value.
	--
	function G (
		Color : in T)
			return Channel;

	-- Gets the red color component.
	--
	-- [Param]
	--
	--	Color : Color to interact with
	--
	-- [Return]
	--
	--	Channel color value.
	--
	function R (
		Color : in T)
			return Channel;

	-- Converts a given color object to its equivalent ARGB representation within a single 32-bit
	-- unsigned int. Useful when using colors directly with XCB.
	--
	-- [Params]
	--
	-- 	Cl : Color to convert
	--
	-- [Return]
	--
	-- 	32-bit argb color value
	--
	function To_ARGB_Uint (
		Color : in T)
			return ARGB_Uint;

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
