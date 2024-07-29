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
	-- Some methods, upon failure, will set an error and raise an exception E. The exact error code
	-- can be checked with Error(). If any error is set all methods will exit early with default
	-- return values and no side-effects. It's possible to clear errors with Repair().
	--
	type T is tagged limited private;

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	-- Creates a config and deep copy the contents of another input tracker into it.
	--
	-- [Params]
	--
	-- 	Cfg    : Config to interact with
	-- 	Parent : Config to clone
	--
	-- [Errors]
	--
	--	Error_Invalid : Initialisation failed
	--
	procedure Clone (
		Cfg    : out T;
		Parent : in  T);

	-- Create an empty input config.
	--
	-- [Params]
	--
	-- 	Cfg : Config to interact with
	--
	-- [Errors]
	--
	--	Error_Invalid : Initialisation failed
	--
	procedure Create (
		Cfg : out T);

	-- Destroys the config and frees memory.
	--
	-- [Params]
	--
	-- 	Cfg : Input tracker to interact with
	--
	procedure Destroy (
		Cfg : in out T);

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Removes all added parameters.
	--
	-- [Params]
	--
	-- 	Cfg : Config to interact with
	--
	procedure Clear_Params (
		Cfg : in out T);

	-- Removes all parsed resources.
	--
	-- [Params]
	--
	-- 	Cfg : Config to interact with
	--
	procedure Clear_Resources (
		Cfg : in out T);

	-- Removes all added sources.
	--
	-- [Params]
	--
	-- 	Cfg : Config to interact with
	--
	procedure Clear_Sources (
		Cfg : in out T);

	-- Looks-up a resource by its namespace and property name. If found, its reference is kept around
	-- and the resource values will become accessible through Iterate() and Resource(). To get the
	-- nunmber of values a resource has, use Resouce_Length().
	--
	-- [Example]
	--
	--	Conf.Fetch ("something", "something");
	--	while Conf.Iterate
	--	loop
	--		Put_Line (Conf.Resource);
	--	end loop;
	--
	-- [Params]
	--
	-- 	Cfg       : Config instance to interact with
	-- 	Namespace : Resource namespace
	-- 	Property  : Resource property name
	--
	procedure Fetch (
		Cfg       : in out T;
		Namespace : in String;
		Property  : in String);

	-- Increments an internal iterator offset and makes available the next value associated to a
	-- resource fetched with Fetch(). Said value can be accessed with Resource(). This function exits
	-- early and returns False if the iterator cannot be incremented because it has already reached
	-- the last resource value.
	--
	-- [Params]
	--
	-- 	Cfg : Config instance to interact with
	--
	-- [Return]
	--
	-- 	True is the next value could be picked. If the config has errored, then False will always
	-- 	be returned.
	--
	function Iterate (
		Cfg : in out T)
			return Boolean;

	-- Reads the first source file that can be opened, parses it, and stores the resolved resources.
	-- Every time this function is called the previously parsed resources will be cleared first
	-- before reading the source. This function has no effects if no source file can be read. It
	-- should be noted that not being able to open any source files is not considered to be an error
	-- by default. If such a check is needed, use Can_Open_Sources().
	--
	-- [Params]
	--
	-- 	Cfg : Config instance to interact with
	--
	-- [Errors]
	--
	-- 	Error_Overflow : The size of an internal components was about to overflow
	-- 	Error_Memory   : Failed memory allocation
	--
	procedure Load (
		Cfg : in out T);

	-- Adds an floating value as a config parameter. This parameter's value can then be accessed from
	-- a config source file. Unlike user-defined variables, only one value per parameter can be
	-- defined.
	--
	-- [Params]
	--
	-- 	Cfg   : Config instance to interact with
	-- 	Name  : Name of the parameter to reference in the source file
	--	Value : Floating value
	--
	-- [Errors]
	--
	-- 	Error_Overflow : The size of an internal components was about to overflow
	-- 	Error_Memory   : Failed memory allocation
	--
	procedure Push_Param (
		Cfg   : in out T;
		Name  : in String;
		Value : in Float);

	-- Adds an integer as a config parameter. This parameter's value can then be accessed from a
	-- config source file. Unlike user-defined variables, only one value per parameter can be
	-- defined.
	--
	-- [Params]
	--
	-- 	Cfg   : Config instance to interact with
	-- 	Name  : Name of the parameter to reference in the source file
	--	Value : Integer value
	--
	-- [Errors]
	--
	-- 	Error_Overflow : The size of an internal components was about to overflow
	-- 	Error_Memory   : Failed memory allocation
	--
	procedure Push_Param (
		Cfg   : in out T;
		Name  : in String;
		Value : in Integer);

	-- Adds a string as a config parameter. This parameter's value can then be accessed from a config
	-- source file. Unlike user-defined variables, only one value per parameter can be defined.
	--
	-- [Params]
	--
	-- 	Cfg   : Config instance to interact with
	-- 	Name  : Name of the parameter to reference in the source file
	--	Value : String value
	--
	-- [Errors]
	--
	-- 	Error_Overflow : The size of an internal components was about to overflow
	-- 	Error_Memory   : Failed memory allocation
	--
	procedure Push_Param (
		Cfg   : in out T;
		Name  : in String;
		Value : in String);

	-- Adds a file as a config source. Only the first source that can be opened will be parsed. The
	-- remaining sources act as fallback.
	--
	-- [Params]
	--
	-- 	Cfg      : Config instance to interact with
	-- 	Filename : Full path to the source file
	--
	-- [Errors]
	--
	-- 	Error_Overflow : The size of an internal components was about to overflow
	-- 	Error_Memory   : Failed memory allocation
	--
	procedure Push_Source (
		Cfg      : in out T;
		Filename : in String);

	-- Clears errors and puts the config back into an usable state. The only unrecoverable error is
	-- Error_Invalid.
	--
	-- [Params]
	--
	-- 	Cfg : Config to interact with
	--
	procedure Repair (
		Cfg : in out T);

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	-- Checks if any added source file can be opened up and read. 
	--
	-- [Params]
	--
	-- 	Cfg : Config to interact with
	--
	-- [Return]
	--
	-- 	Source availability. If the config has errored, False will alway be returned.
	--
	function Can_Open_Sources (
		Cfg : in T)
			return Boolean;

	-- Checks if any added source file can be opened up and read. This variant will write into the
	-- Index the rank of the opened source.
	--
	-- [Params]
	--
	-- 	Cfg  : Config to interact with
	-- 	Rank : Source rank
	--
	-- [Return]
	--
	-- 	Source availability. If the config has errored, False will alway be returned.
	--
	function Can_Open_Sources (
		Cfg  : in T;
		Rank : out Index)
			return Boolean;

	-- Gets the error state.
	--
	-- [Params]
	--
	-- 	Cfg : Config to interact with
	--
	-- [Return]
	--
	-- 	Error code.
	--
	function Error (
		Cfg : in T)
			return Error_Code;

	-- Gets the resource value an internal iterator is pointing at. It's the responsibility of the
	-- caller to convert it into the required type.
	--
	-- [Params]
	--
	--	Cfg : Config instance to interact with
	--
	-- [Return]
	--
	-- 	Resource value as a String. If no resource was pre-fetched, the iterator hasn't been
	--	incremented once before this function gets called, or if the config has errored, an
	-- 	empty string will then be returned.
	--
	function Resource (
		Cfg : in T)
			Return String;

	-- Gets the number of values a pre-fetched resource has.
	--
	-- [Params]
	--
	-- 	Cfg : Config to interact with
	--
	-- [Return]
	--
	-- 	Number of values. If the config has errored, 0 will always be returned.
	--
	function Resource_Length (
		Cfg : in T)
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

	procedure Raise_Error (Cfg : in T);

	-------------------------------------------------------------------------------------------------
	-- IMPORTS -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------
	
	procedure C_CLear_Params      (Cfg : System.Address);
	procedure C_CLear_Resources   (Cfg : System.Address);
	procedure C_Clear_Sources     (Cfg : System.Address);
	procedure C_Destroy           (Cfg : System.Address);
	procedure C_Fetch             (Cfg : System.Address; Namespace : C.Strings.chars_ptr; Property : C.Strings.chars_ptr);
	procedure C_Load              (Cfg : System.Address);
	procedure C_Push_Param_Double (Cfg : System.Address; Name : C.Strings.chars_ptr; Value : C.double);
	procedure C_Push_Param_Long   (Cfg : System.Address; Name : C.Strings.chars_ptr; Value : Long_Long_Integer);
	procedure C_Push_Param_Str    (Cfg : System.Address; Name : C.Strings.chars_ptr; Value : C.Strings.chars_ptr);
	procedure C_Push_Source       (Cfg : System.Address; Filename : C.Strings.chars_ptr);
	procedure C_Repair            (Cfg : System.Address);

	function  C_Can_Open_Sources  (Cfg : System.Address; Rank : access C.size_t) return C.Extensions.bool;
	function  C_Clone             (Cfg : System.Address)                         return System.Address;
	function  C_Create                                                           return System.Address;
	function  C_Error             (Cfg : System.Address)                         return Error_Code;
	function  C_Iterate           (Cfg : System.Address)                         return C.Extensions.bool;
	function  C_Resource          (Cfg : System.Address)                         return C.Strings.chars_ptr;
	function  C_Resource_Length   (Cfg : System.Address)                         return C.size_t;

	pragma Import (C, C_Clear_Params,      "ccfg_clear_params");
	pragma Import (C, C_Clear_Resources,   "ccfg_clear_resources");
	pragma Import (C, C_Clear_Sources,     "ccfg_clear_sources");
	pragma Import (C, C_Can_Open_Sources,  "ccfg_can_open_sources");
	pragma Import (C, C_Clone,             "ccfg_clone");
	pragma Import (C, C_Create,            "ccfg_create");
	pragma Import (C, C_Destroy,           "ccfg_destroy");
	pragma Import (C, C_Error,             "ccfg_error");
	pragma Import (C, C_Fetch,             "ccfg_fetch");
	pragma Import (C, C_Iterate,           "ccfg_iterate");
	pragma Import (C, C_Load,              "ccfg_load");
	pragma Import (C, C_Placeholder,       "ccfg_placeholder_instance");
	pragma Import (C, C_Push_Param_Double, "ccfg_push_param_double");
	pragma Import (C, C_Push_Param_Long,   "ccfg_push_param_long");
	pragma Import (C, C_Push_Param_Str,    "ccfg_push_param_str");
	pragma Import (C, C_Push_Source,       "ccfg_push_source");
	pragma Import (C, C_Repair,            "ccfg_repair");
	pragma Import (C, C_Resource,          "ccfg_resource");
	pragma Import (C, C_Resource_Length,   "ccfg_resource_length");

end Cassette.Config;
