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

with Interfaces;   use Interfaces;
with Interfaces.C; use Interfaces.C;
with System;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package Cassette.Color is

	-------------------------------------------------------------------------------------------------
	-- TYPES ---------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	--  Numerics.
	--
	subtype Ratio_Value is C.double         range 0.0 .. 1.0;
	subtype Channel     is C.double         range 0.0 .. 1.0;
	subtype ARGB_Uint   is C.unsigned_long  range   0 .. (2 ** 32 - 1);
	subtype Byte        is C.unsigned_short range   0 .. (2 **  8 - 1);

	-- Represention of a RGBA color. Double types bound inside the [0.0 and 1.0] range are used, so
	-- that they could be passed to cairo's function without conversion.
	--
	-- [Params]
	--
	-- 	R : Red   color component
	-- 	G : Green color component
	-- 	B : Blue  color component
	-- 	A : Alpha color component
	--
	type T is record
		R : aliased Channel := 0.0;
		G : aliased Channel := 0.0;
		B : aliased Channel := 0.0;
		A : aliased Channel := 0.0;
	end record
		with Convention => C_Pass_By_Copy;

	-------------------------------------------------------------------------------------------------
	-- GLOBALS --------------------------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	TRANSPARENT : constant T := (R => 0.000, G => 0.000, B => 0.000, A => 0.000);
	WHITE       : constant T := (R => 1.000, G => 1.000, B => 1.000, A => 1.000);
	BLACK       : constant T := (R => 0.000, G => 0.000, B => 0.000, A => 1.000);
	RED         : constant T := (R => 1.000, G => 0.000, B => 0.000, A => 1.000);
	GREEN       : constant T := (R => 0.000, G => 1.000, B => 0.000, A => 1.000);
	BLUE        : constant T := (R => 0.000, G => 0.000, B => 1.000, A => 1.000);
	YELLOW      : constant T := (R => 1.000, G => 1.000, B => 0.000, A => 1.000);
	MAGENTA     : constant T := (R => 1.000, G => 0.000, B => 1.000, A => 1.000);
	CYAN        : constant T := (R => 0.000, G => 1.000, B => 1.000, A => 1.000);

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Converts a 32-bits ARGB color representation into a color object.
	--
	-- [Params]
	--
	-- 	Argb : Color uint to convert
	--
	-- [Return]
	--
	-- 	Color record
	--
	function From_ARGB_Uint (
		Argb : in ARGB_Uint)
			return T;

	-- Converts a RGBA color representation with channel values bounded between 0 and 255 into a
	-- color object.
	--
	-- @param r : Red   color component
	-- @param g : Green color component
	-- @param b : Blue  color component
	-- @param a : Alpha color component
	--
	-- [Return]
	--
	-- 	Color record
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
	-- [Params]
	--
	--	Str : String to convert
	--
	-- [Return]
	--
	-- 	Color record.
	--
	function From_Str (
		Str : in String)
			return T;
	
	-- Converts a string into a color object. The given string is interpreted as a char
	-- representation of an unsigned 32-bit ARGB value (which will get converted with C's stroul();
	-- see the documentation of this function for more information), unless there is a leading '#'
	-- character, in which case the string gets interpreted as a 6-8 digit "#rrggbbaa" hex. In this
	-- representation, if the optional alpha parameter is omitted, a 0xFF value is assumed. This
	-- function will set the Error parameter to True if the string to convert is invalid. Otherwise,
	-- it's set to False.
	--
	-- [Params]
	--
	--	Str   : String to convert
	-- 	Error : Conversion error check
	--
	-- [Return]
	--
	-- 	Color record. Check Error to be certain of the return's validity
	--
	function From_Str (
		Str   : in  String;
		Error : out Boolean)
			return T;

	-- Interpolates a color between two given colors.
	--
	-- [Params]
	--
	-- 	Color_1 : First  color
	-- 	Color_2 : Second color
	-- 	Ratio   : Second / first color ratio in the [0.0 1.0] range used for the
	--              interpolation.
	--
	-- [Return]
	--
	-- 	Interpolated color
	--
	function Interpolate (
		Cl_1  : in T;
		Cl_2  : in T;
		Ratio : in Ratio_Value)
			return T;

	-- Converts a given color object to its equivalent ARGB representation within a single 32-bit
	-- unsigned int. Useful when using colors directly with XCB.
	--
	-- [Params]
	--
	-- 	Cl : Color record to convert
	--
	-- [Return]
	--
	-- 	32-bit argb color value
	--
	function To_ARGB_Uint (
		Cl : in T)
			return ARGB_Uint;

end Cassette.Color;
