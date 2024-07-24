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

package Cassette.Dict is

	-------------------------------------------------------------------------------------------------
	-- EXCEPTIONS -----------------------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	-- Exception that gets raised when an impure method or a constructor fails.
	--
	E : exception;

	-------------------------------------------------------------------------------------------------
	-- TYPES ---------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Opaque dictionary object. It's implemented using the FNV1-A hash function and collisions are
	-- resolved using linear probing. A dictionary can automatically grow to maintain a maximum load
	-- factor (set by default to 0.6). Values are retrieved using both a NUL terminated string key
	-- and a group value.
	--
	-- Some methods, upon failure, will set an error bit in an internal error bitfield and raise an
	-- exception. The exact error code can be checked with Error(). If any error is set all methods
	-- will exit early with default return values and no side-effects. It's possible to clear errors
	-- with Repair().
	--
	type T is tagged limited private;

	--  Numerics.
	--
	subtype Group_Value is C.size_t;
	subtype Slot_Value  is C.size_t;
	subtype Size        is C.size_t;
	subtype Ratio       is Float range Float'Succ (0.0) .. 1.0;

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	-- Creates a string and deep copy the contents of another input tracker into it.
	--
	-- [Params]
	--
	-- 	Self   : Dictionary to interact with
	-- 	Parent : Dictionary to clone
	--
	-- [Error]
	--
	--	INVALID : Initialisation failed
	--
	procedure Clone (
		Self   : out T;
		Parent : in  T);

	-- Create an empty input string.
	--
	-- [Params]
	--
	-- 	Self : Dictionary to interact with
	--
	-- [Error]
	--
	--	INVALID : Initialisation failed
	--
	procedure Create (
		Self : out T);

	-- Destroys the dictionary and frees memory.
	--
	-- [Params]
	--
	-- 	Self : Dictionary to interact with
	--
	procedure Destroy (
		Self : in out T);

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Clears all active slots. Allocated memory is not freed, use cdict_destroy() for that.
	--
	-- [Params]
	--
	-- 	Self : Dictionary to interact with
	--
	procedure Clear (
		Self : in out T);

	-- Clears all active slots of a specific group. Allocated memory is not freed, use Destroy() for
	-- that.
	--
	-- [Params]
	--
	-- 	Self  : Dictionary to interact with
	-- 	Group : Group to match
	--
	procedure Clear_Group (
		Self  : in out T;
		Group : in Group_Value);

	-- Deletes the slot that matches the given key and group. This procedure has no effect if there
	-- are no matching slots. Allocated memory is not freed, use Destroy() for that.
	--
	-- [Params]
	--
	-- 	Self  : Dictionary to interact with
	-- 	Key   : Key to match
	-- 	Group : Group to match
	--
	procedure Erase (
		Self  : in out T;
		Key   : in String;
		Group : in Group_Value);

	-- Preallocates a set amount of slots to avoid triggering multiple automatic reallocs and
	-- rehashes when adding data to the dictionary. To stay under the set maximum load factor
	-- (default = 0.6), the actual amount of allocated hashtable slots is
	-- Slot_Number / max_load_factor. This procedure has no effect if the requested number of slots
	-- is smaller than the previously allocated amount.
	--
	-- [Params]
	--
	-- 	Self         : Dictionary to interact with
	-- 	Slots_Number : Number of slots
	--
	-- [Error]
	--
	-- 	OVERFLOW : The size of the resulting dictionary will be > Size'Last
	-- 	MEMORY   : Failed memory allocation
	--
	procedure Prealloc (
		Self         : in out T;
		Slots_Number : in Size);

	-- Sets the maximum load factor. To stay under it, the dictionary may automatically extend its
	-- number of allocated slots. Default value = 0.6.
	--
	-- [Params]
	--
	-- 	Self        : Dictionary to interact with
	-- 	Load_Factor : Maximum load factor to set
	--
	-- [Error]
	--
	-- 	OVERFLOW : The size of the resulting dictionary will be > Size'Last
	-- 	MEMORY   : Failed memory allocation
	--
	procedure Set_Max_Load (
		Self        : in out T;
		Load_Factor : in Ratio);

	-- Clears errors and puts the dictionary back into an usable state. The only unrecoverable error
	-- is INVALID.
	--
	-- [Params]
	--
	-- 	Self : Dictionary to interact with
	--
	procedure Repair (
		Self : in out T);

	-- Activates a slot in the dictionary's hashtable. The given key, group, and values will be
	-- associated with that slot. If a slot with a matching key and group already exists, this
	-- procedure will only overwrite its associated value. The dictionary can automatically extend
	-- the total number of allocated slots to stay under its maximum load factor (default = 0.6).
	--
	-- [Params]
	--
	-- 	Self  : Dictionary to interact with
	-- 	Key   : Key to match
	-- 	Group : Group to match
	-- 	Value : Value to associate with the slot
	--
	-- [Error]
	--
	-- 	OVERFLOW : The size of the resulting dictionary will be > Size'Last
	-- 	MEMORY   : Failed memory allocation
	--
	procedure Write (
		Self  : in out T;
		Key   : in String;
		Group : in Group_Value;
		Value : in Slot_Value);

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Gets the error state.
	--
	-- [Params]
	--
	-- 	Self : Dictionary to interact with
	-- 
	-- [Return]
	--
	-- 	Error code
	--  
	function Error (
		Self : in T)
			return Cassette.Error.T;

	-- Tries to find a slot that matches the given key and group. If found, True is returned.
	--
	-- [Params]
	--
	-- 	Self  : Dictionary to interact with
	-- 	Key   : Key to match
	-- 	Group : Group to match
	--
	-- [Return]
	--
	--	Key + Group match. If the dictionary has errored, then False is always returned.
	--
	function Find (
		Self  : in T;
		Key   : in String;
		Group : in Group_Value)
			return Boolean;

	-- Tries to find a slot that matches the given key and group. If found, True is returned, and the
	-- value associated to that slot is written into the Value parameter.
	--
	-- [Params]
	--
	-- 	Self  : Dictionary to interact with
	-- 	Key   : Key to match
	-- 	Group : Group to match
	-- 	Value : Value associated to the found slot
	--
	-- [Return]
	--
	--	Key + Group match. If the dictionary has errored, then False is always returned.
	--
	function Find (
		Self  : in T;
		Key   : in String;
		Group : in Group_Value;
		Value : out Slot_Value)
			return Boolean;

	-- Gets the number of active slots.
	--
	-- [Params]
	--
	-- 	Self : Dictionary to interact with
	--
	-- [Return]
	--
	--	Number of slots. If the dictionary has errored, then 0 is always returned.
	--
	function Load (
		Self : in T)
			return Size;

	-- Gets a ratio of the number of active slots by the number of allocated slots.
	--
	-- [Params]
	--
	-- 	Self : Dictionary to interact with
	--
	-- [Return]
	--
	--	Load factor between 0.0 and 1.0. If the dictionary has errored, then 0.0 is always
	--	returned.
	--
	function Load_Factor (
		Self : in T)
			return Ratio;

	-------------------------------------------------------------------------------------------------
	-- PRIVATE -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

private

	type Cdict is null record;

	Placeholder : aliased Cdict
		with Import        => True, 
		     Convention    => C, 
		     External_Name => "cdict_placeholder_instance";

	type T is tagged limited record
		Data : System.Address := Placeholder'Address;
	end record;

	procedure Check (Self : in T);

end Cassette.Dict;
