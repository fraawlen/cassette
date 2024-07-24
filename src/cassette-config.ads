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

package Cassette.Config is

	-------------------------------------------------------------------------------------------------
	-- EXCEPTIONS -----------------------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	-- Exception that gets raised when an impure method or a constructor fails.
	--
	E : exception;

	-------------------------------------------------------------------------------------------------
	-- TYPES ---------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Opaque config object that holds all settings like sources and parameters as well as resolved
	-- parsed resources. A decision was made to use a parser that saves all resources instead of
	-- setting target values as the resources get read and resolved so that on a source file is read,
	-- a configuration object can be shared and re-used in software plugins.
	--
	-- Some methods, upon failure, will set an error bit in an internal error bitfield and raise an
	-- exception. The exact error code can be checked with Error(). If any error is set all methods
	-- will exit early with default return values and no side-effects. It's possible to clear errors
	-- with Repair().
	--
	type T is tagged limited private;

	--  Numerics.
	--
	type Size  is new C.size_t;
	type Index is new C.size_t;

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	-- Creates a config and deep copy the contents of another input tracker into it.
	--
	-- [Params]
	--
	-- 	Self   : Config to interact with
	-- 	Parent : Config to clone
	--
	-- [Error]
	--
	--	INVALID : Initialisation failed
	--
	procedure Clone (
		Self   : out T;
		Parent : in  T);

	-- Create an empty input config.
	--
	-- [Params]
	--
	-- 	Self : Config to interact with
	--
	-- [Error]
	--
	--	INVALID : Initialisation failed
	--
	procedure Create (
		Self : out T);

	-- Destroys the config and frees memory.
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

	-- Removes all added parameters.
	--
	-- [Params]
	--
	-- 	Self : Config to interact with
	--
	procedure Clear_Params (
		Self : in out T);

	-- Removes all parsed resources.
	--
	-- [Params]
	--
	-- 	Self : Config to interact with
	--
	procedure Clear_Resources (
		Self : in out T);

	-- Removes all added sources.
	--
	-- [Params]
	--
	-- 	Self : Config to interact with
	--
	procedure Clear_Sources (
		Self : in out T);

	-- Looks-up a resource by its namespace and property name. If found, its reference is kept around
	-- and the resource values will become accessible through Iterate() and Resource(). To get the
	-- nunmber of values a resource has, use Resouce_Length().
	--
	-- [Example]
	--
	--	Conf.Fetch ("something", "something");
	--	while Conf.Iterate loop
	--		Put_Line (Conf.Resource);
	--	end loop;
	--
	-- [Params]
	--
	-- 	Self      : Config instance to interact with
	-- 	Namespace : Resource namespace
	-- 	Property  : Resource property name
	--
	procedure Fetch (
		Self      : in out T;
		Namespace : in String;
		Property  : in String);

	-- Increments an internal iterator offset and makes available the next value associated to a
	-- resource fetched with Fetch(). Said value can be accessed with Resource(). This function exits
	-- early and returns False if the iterator cannot be incremented because it has already reached
	-- the last resource value.
	--
	-- [Params]
	--
	-- 	Self : Config instance to interact with
	--
	-- [Return]
	--
	-- 	True is the next value could be picked. If the config has errored, then False will always
	-- 	be returned.
	--
	function Iterate (
		Self : in out T)
			return Boolean;

	-- Reads the first source file that can be opened, parses it, and stores the resolved resources.
	-- Every time this function is called the previously parsed resources will be cleared first
	-- before reading the source. This function has no effects if no source file can be read. It
	-- should be noted that not being able to open any source files is not considered to be an error
	-- by default. If such a check is needed, use Can_Open_Sources().
	--
	-- [Params]
	--
	-- 	Self : Config instance to interact with
	--
	-- [Errors]
	--
	-- 	OVERFLOW : The size of an internal components was about to overflow
	-- 	MEMORY   : Failed memory allocation
	--
	procedure Load (
		Self : in out T);

	-- Adds an floating value as a config parameter. This parameter's value can then be accessed from
	-- a config source file. Unlike user-defined variables, only one value per parameter can be
	-- defined.
	--
	-- [Params]
	--
	-- 	Self     : Config instance to interact with
	-- 	Name     : Name of the parameter to reference in the source file
	--	Value    : Floating value
	--
	-- [Errors]
	--
	-- 	OVERFLOW : The size of an internal components was about to overflow
	-- 	MEMORY   : Failed memory allocation
	--
	procedure Push_Param (
		Self  : in out T;
		Name  : in String;
		Value : in Float);

	-- Adds an integer as a config parameter. This parameter's value can then be accessed from a
	-- config source file. Unlike user-defined variables, only one value per parameter can be
	-- defined.
	--
	-- [Params]
	--
	-- 	Self     : Config instance to interact with
	-- 	Name     : Name of the parameter to reference in the source file
	--	Value    : Integer value
	--
	-- [Errors]
	--
	-- 	OVERFLOW : The size of an internal components was about to overflow
	-- 	MEMORY   : Failed memory allocation
	--
	procedure Push_Param (
		Self  : in out T;
		Name  : in String;
		Value : in Integer);

	-- Adds a string as a config parameter. This parameter's value can then be accessed from a config
	-- source file. Unlike user-defined variables, only one value per parameter can be defined.
	--
	-- [Params]
	--
	-- 	Self     : Config instance to interact with
	-- 	Name     : Name of the parameter to reference in the source file
	--	Value    : String value
	--
	-- [Errors]
	--
	-- 	OVERFLOW : The size of an internal components was about to overflow
	-- 	MEMORY   : Failed memory allocation
	--
	procedure Push_Param (
		Self  : in out T;
		Name  : in String;
		Value : in String);

	-- Adds a file as a config source. Only the first source that can be opened will be parsed. The
	-- remaining sources act as fallback.
	--
	-- [Params]
	--
	-- 	Self     : Config instance to interact with
	-- 	Filename : Full path to the source file
	--
	-- [Errors]
	--
	-- 	OVERFLOW : The size of an internal components was about to overflow
	-- 	MEMORY   : Failed memory allocation
	--
	procedure Push_Source (
		Self     : in out T;
		Filename : in String);

	-- Clears errors and puts the config back into an usable state. The only unrecoverable error is
	-- INVALID.
	--
	-- [Params]
	--
	-- 	Self : Config to interact with
	--
	procedure Repair (
		Self : in out T);

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------
	
	-- Returns True if any added source file can be opened up and read. 
	--
	-- [Params]
	--
	-- 	Self : Config to interact with
	--
	-- [Return]
	--
	-- 	Source availability. If the config has errored, False will alway be returned.
	--
	function Can_Open_Sources (
		Self : in T)
			return Boolean;

	-- Returns true if any added source file can be opened up and read. This variant will write into
	-- the Index the rank of the opened source.
	--
	-- [Params]
	--
	-- 	Self  : Config to interact with
	-- 	Index : Source rank
	--
	-- [Return]
	--
	-- 	Source availability. If the config has errored, False will alway be returned.
	--
	function Can_Open_Sources (
		Self : in T;
		I    : out Index)
			return Boolean;

	-- Gets the error state.
	--
	-- [Params]
	--
	-- 	Self : Config to interact with
	-- 
	-- [Return]
	--
	-- 	Error code
	--  
	function Error (
		Self : in T)
			return Cassette.Error.T;

	-- Gets the resource value an internal iterator is pointing at. It's the responsibility of the
	-- caller to convert it into the required type.
	--
	-- [Param]
	--
	--	Self : Config instance to interact with
	--
	-- [Return]
	--
	-- 	Resource value as a String. If no resource was pre-fetched, the iterator hasn't been
	--	incremented once before this function gets called, or if the config has errored, an
	-- 	empty string will then be returned.
	--
	function Resource (
		Self : in T)
			Return String;

	-- Gets the number of values a pre-fetched resource has.
	--
	-- [Params]
	--
	-- 	Self : Config to interact with
	--
	-- [Return]
	--
	-- 	Number of values. If the config has errored, 0 will always be returned.
	--
	function Resource_Length (
		Self : in T)
			return Size;

	-------------------------------------------------------------------------------------------------
	-- PRIVATE -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

private

	type Cfg is null record;

	Placeholder : aliased Cfg
		with Import        => True, 
		     Convention    => C, 
		     External_Name => "ccfg_placeholder_instance";

	type T is tagged limited record
		Data : System.Address := Placeholder'Address;
	end record;

	procedure Check (Self : in T);


end Cassette.Config;
