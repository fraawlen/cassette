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

with Interfaces; use Interfaces;
with Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package Cassette.Inputs is

	use type Interfaces.C.size_t;

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
	-- Some methods, upon failure, will set an error and raise an exception E. The exact error code
	-- can be checked with Error(). If any error is set all methods will exit early with default
	-- return values and no side-effects. It's possible to clear errors with Repair().
	--
	type T is tagged limited private;

	-- Numerics.
	--
	type Identifier is new C.unsigned;
	type Position   is new Integer_16;

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	-- Creates an input tracker and deep copy the contents of another input tracker into it.
	--
	-- [Params]
	--
	-- 	Inputs : Input tracker to interact with
	-- 	Parent : Input tracker to clone
	--
	-- [Errors]
	--
	--	Error_Invalid : Initialisation failed
	--
	procedure Clone (
		Inputs : out T;
		Parent : in  T);

	-- Create an empty input tracker.
	--
	-- [Params]
	--
	-- 	Inputs : Input tracker to interact with
	-- 	Length : Maximum number of inputs to track at a time. 0 is an illegal value.
	--
	-- [Errors]
	--
	--	Error_Invalid : Initialisation failed
	--
	procedure Create (
		Inputs : out T;
		Length : in  Size)
			with Pre => Length > 0;

	-- Destroys the input tracker and frees memory.
	--
	-- [Params]
	--
	-- 	Inputs : Input tracker to interact with
	--
	procedure Destroy (
		Inputs : in out T);

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Clears the contents of a given input tracker. Allocated memory is not freed, use Destroy() for
	-- that.
	--
	-- [Params]
	--
	-- 	Inputs : Input tracker to interact with
	--
	procedure Clear (
		Inputs : in out T);

	-- If present, untracks an input with the matching id.
	--
	-- [Params]
	--
	-- 	Inputs : Input tracker to interact with
	-- 	ID   : Identifier to match
	--  
	procedure Pull_ID (
		Inputs : in out T;
		ID     : in Identifier);

	-- Untracks an input at the given index. This procedure has no effects if index is out of bounds.
	--
	-- [Params]
	--
	-- 	Inputs : Input tracker to interact with
	-- 	I    : Index within the array
	--  
	procedure Pull_Index (
		Inputs : in out T;
		I      : in Index);

	-- Adds in input at the end of the input tracking array. If an input with a matching id already
	-- exists within the array, it is pushed to the end of the array and its Addr, X and Y details
	-- are updated. This procedure has no effect if the array is full.
	--
	-- [Params]
	--
	-- 	Inputs : Input tracker to interact with
	-- 	ID     : Identifier
	-- 	X      : Optional, X coordinate
	-- 	Y      : Optional, Y coordinate
	-- 	Addr   : Optional, Arbitrary address to something related to the input
	--
	procedure Push (
		Inputs : in out T;
		ID     : in Identifier;
		X      : in Position       := 0;
		Y      : in Position       := 0;
		Addr   : in System.Address := System.Null_Address);

	-- Clears errors and puts the input tracker back into an usable state. The only unrecoverable
	-- error is Error_Invalid.
	--
	-- [Params]
	--
	-- 	Inputs : Input tracker to interact with
	--
	procedure Repair (
		Inputs : in out T);

	-- Updates the size of input tracker. If the requested size is smaller than the current load,
	-- tailing inputs will be pulled.
	--
	-- [Params]
	--
	-- 	Inputs : Input tracker to interact with
	-- 	Length : Maximum number of inputs to track at a time
	--
	-- [Errors]
	--
	-- 	Error_Overflow : The size of the resulting input tracking array will be > Size'Last
	-- 	Error_Memory   : Failed memory allocation
	--	Error_Param    : Illegal length = 0 has been given
	--
	procedure Resize (
		Inputs : in out T;
		Length : in Size)
			with Pre => Length > 0;

	-- Sets a new default address value to return when Address() cannot return a proper value.
	--
	-- [Params]
	--
	-- 	Inputs : Input tracker to interact with
	-- 	Addr   : Address
	--
	procedure Set_Default_Address (
		Inputs : in out T;
		Addr   : in System.Address);

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Gets the input's associated address at the given index.
	--
	-- [Params]
	--
	-- 	Inputs : Input tracker to interact with
	-- 	I      : Index within the array
	--
	-- [Return]
	--
	-- 	Arbitrary address. If the object has errored, or I is out of bounds, then the
	-- 	default Address value set with Set_Default_Address or System.Null_Address (if it was
	-- 	not set) is always returned.
	--  
	function Address (
		Inputs : in T;
		I      : in Index)
			return System.Address;

	-- Gets the error state.
	--
	-- [Params]
	--
	-- 	Inputs : Input tracker to interact with
	-- 
	-- [Return]
	--
	-- 	Error code.
	--  
	function Error (
		Inputs : in T)
			return Error_Code;

	-- Tries to find an input with the matching id. If found, True is returned.
	--
	-- [Params]
	--
	-- 	Inputs : Input tracker to interact with
	-- 	ID     : Identifier to match
	--
	-- [Return]
	--
	--	ID match. If the object has errored, then always return False.
	--
	function Find (
		Inputs : in T;
		ID     : in Identifier)
			return Boolean;
	
	-- Tries to find an input with the matching id. If found, True is returned, and the array index 
	-- of the found input will be written into the parameter I.
	--
	-- [Params]
	--
	-- 	Inputs : Input tracker to interact with
	-- 	ID     : Identifier to match
	-- 	I      : Index of the found input
	--
	-- [Return]
	--
	--	ID match. If the object has errored, then always return False.
	--  
	function Find (
		Inputs : in  T;
		ID     : in  Identifier;
		I      : out Index)
			return Boolean;

	-- Gets the input's ID at the given index.
	--
	-- [Params]
	--
	-- 	Inputs : Input tracker to interact with
	-- 	I      : Index within the array
	--
	-- [Return]
	--
	--	Identifier. If the object has errored, or I is out of bounds, then always return 0.
	--  
	function ID (
		Inputs : in T;
		I      : in Index)
			return Identifier;

	-- Gets the total number of different tracked inputs.
	--
	-- [Params]
	--
	-- 	Inputs : Input tracker to interact with
	--
	-- [Return]
	--
	--	Number of tracked inputs. If the object has errored, then always return 0.
	--
	function Load (
		Inputs : in T)
			return Size;

	-- Gets the input's X coordinate at the given index.
	--
	-- [Params]
	--
	-- 	Inputs : Input tracker to interact with
	-- 	I      : Index within the array
	--
	-- [Return]
	--
	--	X Coordinate. If the object has errored, or I is out of bounds, then always return 0.
	--  
	function X (
		Inputs : in T;
		I      : in Index)
			return Position;

	-- Gets the input's Y coordinate at the given index. 
	--
	-- [Params]
	--
	-- 	Inputs : Input tracker to interact with
	-- 	I      : Index within the array
	--
	-- [Return]
	--
	--	Y Coordinate. If the object has errored, or I is out of bounds, then always return 0.
	--  
	function Y (
		Inputs : in T;
		I      : in Index)
			return Position;

	-------------------------------------------------------------------------------------------------
	-- PRIVATE -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

private

	C_Placeholder : aliased Placeholder;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	type T is tagged limited record
		Data : System.Address := C_Placeholder'Address;
	end record;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Raise_Error (Inputs : in T);

	-------------------------------------------------------------------------------------------------
	-- IMPORTS -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure C_Clear           (Inputs : System.Address);
	procedure C_Destroy         (Inputs : System.Address);
	procedure C_Pull_ID         (Inputs : System.Address; ID : C.unsigned);
	procedure C_Pull_Index      (Inputs : System.Address; I : C.size_t);
	procedure C_Push            (Inputs : System.Address; ID : C.unsigned; X : Integer_16; Y : Integer_16; Ptr : System.Address);
	procedure C_Repair          (Inputs : System.Address);
	procedure C_Resize          (Inputs : System.Address; Length : C.size_t);
	procedure C_Set_Default_Ptr (Inputs : System.Address; Ptr : System.Address);

	function  C_Clone           (Inputs : System.Address)                                       return System.Address;
	function  C_Create          (Length : C.size_t)                                             return System.Address;
	function  C_Error           (Inputs : System.Address)                                       return Error_Code;
	function  C_Find            (Inputs : System.Address; ID : C.unsigned; I : access C.size_t) return C.Extensions.bool;
	function  C_ID              (Inputs : System.Address; I : C.size_t)                         return C.unsigned;
	function  C_Load            (Inputs : System.Address)                                       return C.size_t;
	function  C_Ptr             (Inputs : System.Address; I : C.size_t)                         return System.Address;
	function  C_X               (Inputs : System.Address; I : C.size_t)                         return Integer_16;
	function  C_Y               (Inputs : System.Address; I : C.size_t)                         return Integer_16;

	pragma Import (C, C_Ptr,             "cinputs_ptr");
	pragma Import (C, C_Clear,           "cinputs_clear");
	pragma Import (C, C_Clone,           "cinputs_clone");
	pragma Import (C, C_Create,          "cinputs_create");
	pragma Import (C, C_Destroy,         "cinputs_destroy");
	pragma Import (C, C_Error,           "cinputs_error");
	pragma Import (C, C_Find,            "cinputs_find");
	pragma Import (C, C_ID,              "cinputs_id");
	pragma Import (C, C_Load,            "cinputs_load");
	pragma Import (C, C_Placeholder,     "cinputs_placeholder_instance");
	pragma Import (C, C_Pull_ID,         "cinputs_pull_id");
	pragma Import (C, C_Pull_Index,      "cinputs_pull_index");
	pragma Import (C, C_Push,            "cinputs_push");
	pragma Import (C, C_Repair,          "cinputs_repair");
	pragma Import (C, C_Resize,          "cinputs_resize");
	pragma Import (C, C_Set_Default_Ptr, "cinputs_set_default_ptr");
	pragma Import (C, C_X,               "cinputs_x");
	pragma Import (C, C_Y,               "cinputs_y");

end Cassette.Inputs;
