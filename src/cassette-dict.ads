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

with Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package Cassette.Dict is

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

	-- Opaque dictionary object. It's implemented using the FNV1-A hash function and collisions are
	-- resolved using linear probing. A dictionary can automatically grow to maintain a maximum load
	-- factor (set by default to 0.6). Values are retrieved using both a NUL terminated string key
	-- and a group value. The Index type is used for values, because this dictionary object is
	-- intended to store various index values of other Cassette objects.
	--
	-- Some methods, upon failure, will set an error and raise an exception E. The exact error code
	-- can be checked with Error(). If any error is set all methods will exit early with default
	-- return values and no side-effects. It's possible to clear errors with Repair().
	--
	type T is tagged limited private;

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	-- Creates a string and deep copy the contents of another input tracker into it.
	--
	-- [Params]
	--
	-- 	Dict   : Dictionary to interact with
	-- 	Parent : Dictionary to clone
	--
	-- [Errors]
	--
	--	INVALID : Initialisation failed
	--
	procedure Clone (
		Dict   : out T;
		Parent : in  T);

	-- Create an empty input string.
	--
	-- [Params]
	--
	-- 	Dict : Dictionary to interact with
	--
	-- [Errors]
	--
	--	INVALID : Initialisation failed
	--
	procedure Create (
		Dict : out T);

	-- Destroys the dictionary and frees memory.
	--
	-- [Params]
	--
	-- 	Dict : Dictionary to interact with
	--
	procedure Destroy (
		Dict : in out T);

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Clears all active slots. Allocated memory is not freed, use cdict_destroy() for that.
	--
	-- [Params]
	--
	-- 	Dict : Dictionary to interact with
	--
	procedure Clear (
		Dict : in out T);

	-- Clears all active slots of a specific group. Allocated memory is not freed, use Destroy() for
	-- that.
	--
	-- [Params]
	--
	-- 	Dict  : Dictionary to interact with
	-- 	Group : Group to match
	--
	procedure Clear_Group (
		Dict  : in out T;
		Group : in Index);

	-- Deletes the slot that matches the given key and group. This procedure has no effect if there
	-- are no matching slots. Allocated memory is not freed, use Destroy() for that.
	--
	-- [Params]
	--
	-- 	Dict  : Dictionary to interact with
	-- 	Key   : Key to match
	-- 	Group : Group to match
	--
	procedure Erase (
		Dict  : in out T;
		Key   : in String;
		Group : in Index);

	-- Preallocates a set amount of slots to avoid triggering multiple automatic reallocs and
	-- rehashes when adding data to the dictionary. To stay under the set maximum load factor
	-- (default = 0.6), the actual amount of allocated hashtable slots is
	-- Slot_Number / max_load_factor. This procedure has no effect if the requested number of slots
	-- is smaller than the previously allocated amount.
	--
	-- [Params]
	--
	-- 	Dict  : Dictionary to interact with
	-- 	Slots : Number of slots
	--
	-- [Errors]
	--
	-- 	OVERFLOW : The size of the resulting dictionary will be > Size'Last
	-- 	MEMORY   : Failed memory allocation
	--
	procedure Prealloc (
		Dict  : in out T;
		Slots : in Size);

	-- Sets the maximum load factor. To stay under it, the dictionary may automatically extend its
	-- number of allocated slots. Default value = 0.6.
	--
	-- [Params]
	--
	-- 	Dict        : Dictionary to interact with
	-- 	Load_Factor : Maximum load factor to set
	--
	-- [Errors]
	--
	-- 	OVERFLOW : The size of the resulting dictionary will be > Size'Last
	-- 	MEMORY   : Failed memory allocation
	--
	procedure Set_Max_Load (
		Dict        : in out T;
		Load_Factor : in Ratio)
			with Pre => Load_Factor > 0.0;

	-- Clears errors and puts the dictionary back into an usable state. The only unrecoverable error
	-- is Error_Invalid.
	--
	-- [Params]
	--
	-- 	Dict : Dictionary to interact with
	--
	procedure Repair (
		Dict : in out T);

	-- Activates a slot in the dictionary's hashtable. The given key, group, and values will be
	-- associated with that slot. If a slot with a matching key and group already exists, this
	-- procedure will only overwrite its associated value. The dictionary can automatically extend
	-- the total number of allocated slots to stay under its maximum load factor (default = 0.6).
	--
	-- [Params]
	--
	-- 	Dict  : Dictionary to interact with
	-- 	Key   : Key to match
	-- 	Group : Group to match
	-- 	Value : Value to associate with the slot
	--
	-- [Errors]
	--
	-- 	OVERFLOW : The size of the resulting dictionary will be > Size'Last
	-- 	MEMORY   : Failed memory allocation
	--
	procedure Write (
		Dict  : in out T;
		Key   : in String;
		Group : in Index;
		Value : in Index);

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Gets the error state.
	--
	-- [Params]
	--
	-- 	Dict : Dictionary to interact with
	-- 
	-- [Return]
	--
	-- 	Error code
	--  
	function Error (
		Dict : in T)
			return Error_Code;

	-- Tries to find a slot that matches the given key and group. If found, True is returned.
	--
	-- [Params]
	--
	-- 	Dict  : Dictionary to interact with
	-- 	Key   : Key to match
	-- 	Group : Group to match
	--
	-- [Return]
	--
	--	Key + Group match. If the dictionary has errored, then False is always returned.
	--
	function Find (
		Dict  : in T;
		Key   : in String;
		Group : in Index)
			return Boolean;

	-- Tries to find a slot that matches the given key and group. If found, True is returned, and the
	-- value associated to that slot is written into the Value parameter.
	--
	-- [Params]
	--
	-- 	Dict  : Dictionary to interact with
	-- 	Key   : Key to match
	-- 	Group : Group to match
	-- 	Value : Value associated to the found slot
	--
	-- [Return]
	--
	--	Key + Group match. If the dictionary has errored, then False is always returned.
	--
	function Find (
		Dict  : in  T;
		Key   : in  String;
		Group : in  Index;
		Value : out Index)
			return Boolean;

	-- Gets the number of active slots.
	--
	-- [Params]
	--
	-- 	Dict : Dictionary to interact with
	--
	-- [Return]
	--
	--	Number of slots. If the dictionary has errored, then 0 is always returned.
	--
	function Load (
		Dict : in T)
			return Size;

	-- Gets a ratio of the number of active slots by the number of allocated slots.
	--
	-- [Params]
	--
	-- 	Dict : Dictionary to interact with
	--
	-- [Return]
	--
	--	Load factor between 0.0 and 1.0. If the dictionary has errored, then 0.0 is always
	--	returned.
	--
	function Load_Factor (
		Dict : in T)
			return Ratio;

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

	procedure Raise_Error (Dict : in T);

	-------------------------------------------------------------------------------------------------
	-- IMPORTS -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure C_Clear        (Dict : System.Address);
	procedure C_Clear_Group  (Dict : System.Address; Group : C.size_t);
	procedure C_Destroy      (Dict : System.Address);
	procedure C_Erase        (Dict : System.Address; Key : C.Strings.chars_ptr; Group : C.size_t);
	procedure C_Prealloc     (Dict : System.Address; Slots : C.size_t);
	procedure C_Set_Max_Load (Dict : System.Address; Load_Factor : C.double);
	procedure C_Repair       (Dict : System.Address);
	procedure C_Write        (Dict : System.Address; Key : C.Strings.chars_ptr; Group : C.size_t; Value : C.size_t);

	function  C_Clone        (Dict : System.Address) return System.Address;
	function  C_Create                               return System.Address;
	function  C_Error        (Dict : System.Address) return Error_Code;
	function  C_Find         (Dict : System.Address; Key : C.Strings.chars_ptr; Group : C.size_t; Value : access C.size_t) return C.Extensions.bool;
	function  C_Load         (Dict : System.Address) return C.size_t;
	function  C_Load_Factor  (Dict : System.Address) return C.double;

	pragma Import (C, C_Clear,        "cdict_clear");
	pragma Import (C, C_Clear_Group,  "cdict_clear_group");
	pragma Import (C, C_Clone,        "cdict_clone");
	pragma Import (C, C_Create,       "cdict_create");
	pragma Import (C, C_Destroy,      "cdict_destroy");
	pragma Import (C, C_Erase,        "cdict_erase");
	pragma Import (C, C_Error,        "cdict_error");
	pragma Import (C, C_Find,         "cdict_find");
	pragma Import (C, C_Load,         "cdict_load");
	pragma Import (C, C_Load_Factor,  "cdict_load_factor");	
	pragma Import (C, C_Placeholder,  "cdict_placeholder_instance");
	pragma Import (C, C_Prealloc,     "cdict_prealloc");
	pragma Import (C, C_Set_Max_Load, "cdict_set_max_load");
	pragma Import (C, C_Repair,       "cdict_repair");
	pragma Import (C, C_Write,        "cdict_write");

end Cassette.Dict;
