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

with Cassette.Error;
with Interfaces.C; use Interfaces;
with System;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package Cassette.Inputs is

	-------------------------------------------------------------------------------------------------
	-- EXCEPTIONS -----------------------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	-- Exception that gets raised when an impure method or a constructor fails.
	--
	E : exception;

	-------------------------------------------------------------------------------------------------
	-- TYPES ---------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Opaque input tracker object. An input tracker stores inputs such as screen touches, key or
	-- button presses in the order they get added. The array that holds them is fixed size. If that
	-- array is full, new inputs get ignored.
	--
	-- Some methods, upon failure, will set an error bit in an internal error bitfield and raise an
	-- exception. The exact error code can be checked with Error(). If any error is set all methods
	-- will exit early with default return values and no side-effects. It's possible to clear errors
	-- with Repair().

	--
	type T is tagged limited private;

	--  Numerics.
	--
	type Identifier is new C.unsigned;
	type Size       is new C.size_t;
	type Index      is new C.size_t;
	type Position   is new Integer range -(2 ** 15) .. (2 ** 15 - 1);

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	-- Creates an input tracker and deep copy the contents of another input tracker into it.
	--
	-- [Params]
	--
	-- 	Self   : Input tracker to interact with
	-- 	Parent : Input tracker to clone
	--
	-- [Error]
	--
	--	INVALID : Initialisation failed
	--
	procedure Clone (
		Self   : out T;
		Parent : in  T);

	-- Create an empty input tracker.
	--
	-- [Params]
	--
	-- 	Self       : Input tracker to interact with
	-- 	Max_Inputs : Maximum number of inputs to track at a time. 0 is an illegal value.
	--
	-- [Error]
	--
	--	INVALID : Initialisation failed
	--
	procedure Create (
		Self       : out T;
		Max_Inputs : in  Size);

	-- Destroys the input tracker and frees memory.
	--
	-- [Params]
	--
	-- 	Self : Input tracker to interact with
	--
	procedure Destroy (
		Self : in out T);

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Clears the contents of a given input tracker. Allocated memory is not freed, use Destroy() for
	-- that.
	--
	-- [Params]
	--
	-- 	Self : Input tracker to interact with
	--
	procedure Clear (
		Self : in out T);

	-- If present, untracks an input with the matching id.
	--
	-- [Params]
	--
	-- 	Self : Input tracker to interact with
	-- 	ID   : Identifier to match
	--  
	procedure Pull_ID (
		Self : in out T;
		ID   : in Identifier);

	-- Untracks an input at the given index. This procedure has no effects if index is out of bounds.
	--
	-- [Params]
	--
	-- 	Self : Input tracker to interact with
	-- 	I    : Index within the array
	--  
	procedure Pull_Index (
		Self : in out T;
		I    : in Index);

	-- Adds in input at the end of the input tracking array. If an input with a matching id already
	-- exists within the array, it is pushed to the end of the array and its Addr, X and Y details
	-- are updated. This procedure has no effect if the array is full.
	--
	-- [Params]
	--
	-- 	Self : Input tracker to interact with
	-- 	ID   : Identifier
	-- 	X    : X coordinate
	-- 	Y    : Y coordinate
	-- 	Addr : Arbitrary address to something related to the input
	--
	procedure Push (
		Self : in out T;
		ID   : in Identifier;
		X    : in Position       := 0;
		Y    : in Position       := 0;
		Addr : in System.Address := System.Null_Address);

	-- Clears errors and puts the input tracker back into an usable state. The only unrecoverable
	-- error is INVALID.
	--
	-- [Params]
	--
	-- 	Self : Input tracker to interact with
	--
	procedure Repair (
		Self : in out T);

	-- Updates the size of input tracker. If the requested size is smaller than the current load,
	-- tailing inputs will be pulled.
	--
	-- [Params]
	--
	-- 	Self       : Input tracker to interact with
	-- 	Max_Inputs : Maximum number of inputs to track at a time. 0 is an illegal value.
	--
	-- [Errors]
	--
	-- 	OVERFLOW : The size of the resulting input tracking array will be > Size'Last
	-- 	INVALID  : Failed memory allocation
	--
	procedure Resize (
		Self       : in out T;
		Max_Inputs : in Size);

	-- Sets a new default address value to return when Get() cannot return a proper value.
	--
	-- [Params]
	--
	-- 	Self : Input tracker to interact with
	-- 	Addr : Address
	--
	procedure Set_Default_Address (
		Self : in out T;
		Addr : in System.Address);

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Gets the input's associated address at the given index.
	--
	-- [Params]
	--
	-- 	Self : Input tracker to interact with
	-- 	I    : Index within the array
	--
	-- [Return]
	--
	-- 	Arbitrary adddress. If the object has errored, or I is out of bounds, then the
	-- 	default Address value set with Set_Default_Address or System.Null_Address (if it was
	-- 	not set) is always returned.
	--  
	function Address (
		Self : in T;
		I    : in Index)
			return System.Address;

	-- Gets the error state.
	--
	-- [Params]
	--
	-- 	Self : Input tracker to interact with
	-- 
	-- [Return]
	--
	-- 	Error code
	--  
	function Error (
		Self : in T)
			return Cassette.Error.T;

	-- Tries to find an input with the matching id. If found, True is returned,
	--
	-- [Params]
	--
	-- 	Self : Input tracker to interact with
	-- 	ID   : Identifier to match
	--
	-- [Return]
	--
	--	ID match. If the object has errored, then always return False.
	--
	function Find (
		Self : in T;
		ID   : in Identifier)
			return Boolean;
	
	-- Tries to find an input with the matching id. If found, True is returned, and the array index 
	-- of the found input will be written into the parameter 'Index'.
	--
	-- [Params]
	--
	-- 	Self : Input tracker to interact with
	-- 	ID   : Identifier to match
	-- 	I    : Index of the found input
	--
	-- [Return]
	--
	--	ID match. If the object has errored, then always return False.
	--  
	function Find (
		Self : in  T;
		ID   : in  Identifier;
		I    : out Index)
			return Boolean;

	-- Gets the input's ID at the given index.
	--
	-- [Params]
	--
	-- 	Self : Input tracker to interact with
	-- 	I    : Index within the array
	--
	-- [Return]
	--
	--	Identifier. If the object has errored, or I is out of bounds, then always return 0.
	--  
	function ID (
		Self : in T;
		I    : in Index)
			return Identifier;

	-- Gets the total number of different tracked inputs.
	--
	-- [Params]
	--
	-- 	Self : Input tracker to interact with
	--
	-- [Return]
	--
	--	Number of tracked inputs. If the object has errored, then always return 0.
	--
	function Load (
		Self : in T)
			return Size;

	-- Gets the input's X coordinate at the given index.
	--
	-- [Params]
	--
	-- 	Self : Input tracker to interact with
	-- 	I    : Index within the array
	--
	-- [Return]
	--
	--	X Coordinate. If the object has errored, or I is out of bounds, then always return 0.
	--  
	function X (Self : in T; I : in Index)
		return Position;

	-- Gets the input's Y coordinate at the given index. 
	--
	-- [Params]
	--
	-- 	Self : Input tracker to interact with
	-- 	I    : Index within the array
	--
	-- [Return]
	--
	--	Y Coordinate. If the object has errored, or I is out of bounds, then always return 0.
	--  
	function Y (
		Self : in T;
		I : in Index)
			return Position;

	-------------------------------------------------------------------------------------------------
	-- PRIVATE -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

private

	type Cinputs is null record;

	Placeholder : aliased Cinputs
		with Import        => True, 
		     Convention    => C, 
		     External_Name => "cinputs_placeholder_instance";

	type T is tagged limited record
		Data : System.Address := Placeholder'Address;
	end record;

	procedure Check (Self : in T);

end Cassette.Inputs;
