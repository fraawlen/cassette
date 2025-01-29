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

with Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package Cassette.Book is

	-------------------------------------------------------------------------------------------------
	-- EXCEPTIONS -----------------------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	E : exception;

	-------------------------------------------------------------------------------------------------
	-- TYPES ---------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	type T is tagged limited private;

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	procedure Clone (Book : out T; Parent : in  T);

	procedure Create (Book : out T);

	procedure Destroy (Book : in out T);

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Clear (Book : in out T);

	procedure Pop_Group (Book : in out T);

	procedure Pop_Word (Book : in out T);

	procedure Prealloc (Book : in out T; Bytes : in Size; Words : in Size; Groups : in Size);

	procedure Prepare_New_Group (Book : in out T);

	procedure Repair (Book : in out T);

	function  Rewrite (Book : in out T; Word : in Index; Str : in String) return Boolean;

	procedure Undo_New_Group (Book : in out T);

	procedure Write (Book : in out T; Str : in String);

	procedure Zero (Book : in out T);

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function Error (Book : in T) return Error_Code;
	
	function Group_Length (Book : in T; Group : in Index) return Size;

	function Groups_Number (Book : in T) return Size;

	function Length (Book : in T) return Size;

	function Word (Book : in T; Word : in Index) return String;

	function Word_In_Group (Book : in T; Group : in Index; Word : in Index) return String;

	function Word_Index (Book : in T; Group : in Index; Word : in Index) return Index;

	function Words_Number (Book : in T) return Size;

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

	procedure Raise_Error (Book : in T);

	-------------------------------------------------------------------------------------------------
	-- IMPORTS -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure C_Clear             (Book : System.Address);
	procedure C_Destroy           (Book : System.Address);
	procedure C_Pop_Group         (Book : System.Address);
	procedure C_Pop_Word          (Book : System.Address);
	procedure C_Prealloc          (Book : System.Address; Bytes : C.size_t; Words : C.size_t; Groups : C.size_t);
	procedure C_Prepare_New_Group (Book : System.Address);
	procedure C_Repair            (Book : System.Address);
	procedure C_Undo_New_Group    (Book : System.Address);
	procedure C_Write             (Book : System.Address; Str : C.Strings.chars_ptr);
	procedure C_Zero              (Book : System.Address);

	function  C_Clone             (Book : System.Address)                                              return System.Address;
	function  C_Create                                                                                 return System.Address;
	function  C_Error             (Book : System.Address)                                              return Error_Code;
	function  C_Group_Length      (Book : System.Address; Group : C.size_t)                            return C.size_t;
	function  C_Groups_Number     (Book : System.Address)                                              return C.size_t;
	function  C_Length            (Book : System.Address)                                              return C.size_t;
	function  C_Rewrite           (Book : System.Address; Word  : C.size_t; Str : C.Strings.chars_ptr) return C.Extensions.bool;
	function  C_Word              (Book : System.Address; Group : C.size_t)                            return C.Strings.chars_ptr;
	function  C_Word_In_Group     (Book : System.Address; Group : C.size_t; Word : C.size_t)           return C.Strings.chars_ptr;
	function  C_Word_Index        (Book : System.Address; Group : C.size_t; Word : C.size_t)           return C.size_t;
	function  C_Words_Number      (Book : System.Address)                                              return C.size_t;
	
	pragma Import (C, C_Clear,             "cbook_clear");
	pragma Import (C, C_Clone,             "cbook_clone");
	pragma Import (C, C_Create,            "cbook_create");
	pragma Import (C, C_Destroy,           "cbook_destroy");
	pragma Import (C, C_Error,             "cbook_error");
	pragma Import (C, C_Group_Length,      "cbook_group_length");
	pragma Import (C, C_Groups_Number,     "cbook_groups_number");
	pragma Import (C, C_Length,            "cbook_length");
	pragma Import (C, C_Placeholder,       "cbook_placeholder_instance");
	pragma Import (C, C_Pop_Group,         "cbook_pop_group");
	pragma Import (C, C_Pop_Word,          "cbook_pop_word");
	pragma Import (C, C_Prealloc,          "cbook_prealloc");
	pragma Import (C, C_Prepare_New_Group, "cbook_prepare_new_group");
	pragma Import (C, C_Repair,            "cbook_repair");
	pragma Import (C, C_Rewrite,           "cbook_rewrite");
	pragma Import (C, C_Undo_New_Group,    "cbook_undo_new_group");
	pragma Import (C, C_Word,              "cbook_word");
	pragma Import (C, C_Word_In_Group,     "cbook_word_in_group");
	pragma Import (C, C_Word_Index,        "cbook_word_index");
	pragma Import (C, C_Words_Number,      "cbook_words_number");
	pragma Import (C, C_Write,             "cbook_write");
	pragma Import (C, C_Zero,              "cbook_zero");

end Cassette.Book;

