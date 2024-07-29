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
with System;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package Cassette.Ref is

	-------------------------------------------------------------------------------------------------
	-- EXCEPTIONS -----------------------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	-- Exception that gets raised when an impure method or a constructor fails.
	--
	E : exception;

	-------------------------------------------------------------------------------------------------
	-- TYPES ---------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Opaque reference counter object. It stores arbitrary addresses in an automatically extensible
	-- array. When an address gets pushed to this object, its reference count gets incremented. A
	-- saved address only gets removed when its counts reaches 0.
	--
	-- Some methods, upon failure, will set an error and raise an exception E. The exact error code
	-- can be checked with Error(). If any error is set all methods will exit early with default
	-- return values and no side-effects. It's possible to clear errors with Repair().
	--
	type T is tagged limited private;

	-- Numerics.
	--
	type Counter is new C.unsigned;

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	-- Creates a reference counter and deep copy the contents of another reference counter into it.
	--
	-- [Params]
	--
	-- 	Ref    : Reference counter to copy contents from
	-- 	Parent : Reference counter to clone
	--
	-- [Errors]
	--
	--	Error_Invalid : Initialisation failed
	--
	procedure Clone (
		Ref    : out T;
		Parent : in  T);

	-- Creates an empty reference counter.
	--
	-- [Errors]
	--
	--	Error_Invalid : Initialisation failed
	--
	procedure Create (
		Ref : out T);
		
	-- Destroys the reference counter and frees memory.
	--
	-- [Params]
	--
	-- 	Ref : Reference counter to interact with
	--
	procedure Destroy (
		Ref : in out T);

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Clears the contents of a given reference counter. Allocated memory is not freed, use Destroy()
	-- for that.
	--
	-- [Params]
	--
	-- 	Ref : Reference counter to interact with
	--
	procedure Clear (
		Ref : in out T);

	-- Preallocates slots for the reference array to avoid triggering multiple automatic reallocs
	-- when pushing new references. This function has no effect if the requested number of slots is
	-- smaller than the previously allocated amounts.
	--
	-- [Params]
	--
	-- 	Ref   : Reference counter to interact with
	--	Slots : Number of slots
	--
	-- [Errors]
	--
	-- 	Error_Overflow : The size of the resulting reference array will be > Index'Last
	-- 	Error_Memory   : Failed memory allocation
	--
	procedure Prealloc (
		Ref   : in out T;
		Slots : in SIze);

	-- Decrements the counter of a reference at the given index. If the counter reaches 0, the
	-- reference gets removed from the reference array. This function has no effects if I is out of
	-- bounds.
	--
	-- [Params]
	--
	-- 	Ref : Reference counter to interact with
	-- 	I   : Index within the array
	--
	procedure Pull (
		Ref : in out T;
		I   : in Index);

	-- Searches for a reference with the matching address. If found, it's counter gets decremented.
	-- If the counter then reached 0, the reference gets removed from the reference array.
	--
	-- [Params]
	--
	-- 	Ref  : Reference counter to interact with
	-- 	Addr : Address
	--
	procedure Pull (
		Ref  : in out T;
		Addr : in System.Address);

	-- Removes a reference at the given index regardless of its count. This function has no effects
	-- if I is out of bounds.
	--
	-- [Params]
	--
	-- 	Ref : Reference counter to interact with
	-- 	I   : Index within the array
	--
	procedure Purge (
		Ref : in out T;
		I   : in Index);

	-- Searches for a reference with the matching address. If found, the reference gets removed
	-- regardless of its count.
	--
	-- [Params]
	--
	-- 	Ref  : Reference counter to interact with
	-- 	Addr : Address
	--
	procedure Purge (
		Ref  : in out T;
		Addr : in System.Address);

	-- Searches for a reference with the matching address. If found, its counter gets incremented. If
	-- not, the reference gets added at the end of the reference array with its count = 1. The array
	-- get automatically extended as needed.
	--
	-- [Params]
	--
	-- 	Ref  : Reference counter to interact with
	-- 	Addr : Address
	--
	-- [Errors]
	--
	-- 	Error_Overflow : The size of the resulting reference array will be > Index'Last
	-- 	Error_Memory   : Failed memory allocation
	--
	procedure Push (
		Ref  : in out T;
		Addr : in System.Address);

	-- Clears errors and puts the reference counter back into an usable state. The only unrecoverable
	-- error is Error_Invalid.
	--
	-- [Params]
	--
	-- 	Ref : Reference counter to interact with
	--
	procedure Repair (
		Ref : in out T);

	-- Sets a new default address value to return when Address() cannot return a proper value.
	--
	-- [Params]
	--
	-- 	Ref  : Reference counter to interact with
	-- 	Addr : Address
	--
	procedure Set_Default_Address (
		Ref  : in out T;
		Addr : in System.Address);

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Gets the reference's address at the given index. 
	--
	-- [Params]
	--
	-- 	Ref : Reference counter to interact with
	-- 	I   : Index within the array
	--
	-- [Return]
	--
	-- 	Arbitrary address. If the object has errored, or I is out of bounds, then the
	-- 	default Address value set with Set_Default_Address or System.Null_Address (if it was
	-- 	not set) is always returned.
	--
	function Address (
		Ref : in T;
		I   : in Index)
			return System.Address;

	-- Gets the reference's count at the given index.
	--
	-- [Params]
	--
	-- 	Ref : Reference counter to interact with
	-- 	I   : Index within the array
	--
	-- [Return]
	--
	-- 	Reference count. If the reference counter has errored, or if I is out of bounds, then
	--	0 is always returned.
	--
	function Count (
		Ref : in T;
		I   : in Index)
			return Counter;

	-- Gets the error state.
	--
	-- [Params]
	--
	-- 	Ref : Reference counter to interact with
	--
	-- [Return]
	--
	-- 	Error value.
	--
	function Error (
		Ref : in T)
			return Error_Code;

	-- Tries to find a reference with the matching address. If found, the reference's count is
	-- returned (>0).
	--
	-- [Params]
	--
	-- 	Ref  : Reference counter to interact with
	-- 	Addr : Address to search
	--
	-- [Return]
	--
	--	Reference count of a matching address. If the object has errored, or the address was
	-- 	not found, then always return 0.
	--
	function Find (
		Ref  : in T;
		Addr : in System.Address)
			return Boolean;

	-- Tries to find a reference with the matching address. If found, the reference's count is
	-- returned (>0), and the array index  of the found input will be written into the
	-- parameter I.
	--
	-- [Params]
	--
	-- 	Ref  : Reference counter to interact with
	-- 	Addr : Address to search
	-- 	I    : Index of the found reference
	--
	-- [Return]
	--
	--	Reference count of a matching address. If the object has errored, or the address was
	-- 	not found, then always return 0.
	--
	function Find (
		Ref  : in T;
		Addr : in System.Address;
		I    : out Index)
			return Boolean;

	-- Gets the total number of different tracked references.
	--
	-- [Params]
	--
	-- 	Ref : Reference counter to interact with
	--
	-- [Return]
	--
	-- 	Number of different references. If the reference counter has errored, then 0 is
	--	always returned.
	--
	function Length (
		Ref : in T)
			return Size;

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

	procedure Raise_Error (Ref : in T);

	-------------------------------------------------------------------------------------------------
	-- IMPORTS -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure C_Clear           (Ref : System.Address);
	procedure C_Destroy         (Ref : System.Address);
	procedure C_Prealloc        (Ref : System.Address; Slots : C.size_t);
	procedure C_Pull_Index      (Ref : System.Address; Index : C.size_t);
	procedure C_Pull_Ptr        (Ref : System.Address; Ptr : System.Address);
	procedure C_Purge_Index     (Ref : System.Address; Index : C.size_t);
	procedure C_Purge_Ptr       (Ref : System.Address; Ptr : System.Address);
	procedure C_Push            (Ref : System.Address; Ptr : System.Address);
	procedure C_Repair          (Ref : System.Address);
	procedure C_Set_Default_Ptr (Ref : System.Address; Addr : System.Address);

	function  C_Clone           (Ref : System.Address)                                             return System.Address;
	function  C_Count           (Ref : System.Address; I : C.size_t)                               return C.unsigned;
	function  C_Create                                                                             return System.Address;
	function  C_Error           (Ref : System.Address)                                             return Error_Code;
	function  C_Find            (Ref : System.Address; Addr : System.Address; I : access C.size_t) return C.Extensions.bool;
	function  C_Length          (Ref : System.Address)                                             return C.size_t;
	function  C_Ptr             (Ref : System.Address; I : C.size_t)                               return System.Address;

	pragma Import (C, C_Clear,           "cref_clear");
	pragma Import (C, C_Clone,           "cref_clone");
	pragma Import (C, C_Count,           "cref_cpunt");
	pragma Import (C, C_Create,          "cref_create");
	pragma Import (C, C_Destroy,         "cref_destroy");
	pragma Import (C, C_Error,           "cref_error");
	pragma Import (C, C_Find,            "cref_find");
	pragma Import (C, C_Length,          "cref_length");
	pragma Import (C, C_Placeholder,     "cref_placeholder_instance");
	pragma Import (C, C_Prealloc,        "cref_prealloc");
	pragma Import (C, C_Ptr,             "cref_ptr");
	pragma Import (C, C_Pull_Index,      "cref_pull_index");
	pragma Import (C, C_Pull_Ptr,        "cref_pull_ptr");
	pragma Import (C, C_Purge_Index,     "cref_purge_index");
	pragma Import (C, C_Purge_Ptr,       "cref_purge_ptr");
	pragma Import (C, C_Push,            "cref_push");
	pragma Import (C, C_Repair,          "cref_repair");
	pragma Import (C, C_Set_Default_Ptr, "cref_set_default_ptr");

end Cassette.Ref;
