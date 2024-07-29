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

package body Cassette.Book is

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	procedure Clone (Book : out T; Parent : in T)
	is begin

		Book.Data := C_Clone (Parent.Data);
		Book.Raise_Error;

	end Clone;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Create (Book : out T)
	is begin

		Book.Data := C_Create;
		Book.Raise_Error;

	end Create;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Destroy (Book : in out T)
	is begin

		C_Destroy (Book.Data);
		Book.Data := C_Placeholder'Address;

	end Destroy;

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Clear (Book : in out T)
	is begin

		C_Clear (Book.Data);

	end Clear;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Pop_Group (Book : in out T)
	is begin

		C_Pop_Group (Book.Data);

	end Pop_Group;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Pop_Word (Book : in out T)
	is begin

		C_Pop_Word (Book.Data);

	end Pop_Word;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Prealloc (Book : in out T; Bytes : in Size; Words : in Size; Groups : in Size)
	is begin
		
		C_Prealloc (Book.Data, Bytes, Words, Groups);
		Book.Raise_Error;

	end Prealloc;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Prepare_New_Group (Book : in out T)
	is begin

		C_Prepare_New_Group (Book.Data);

	end Prepare_New_Group;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Repair (Book : in out T)
	is begin

		C_Repair (Book.Data);

	end Repair;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Undo_New_Group (Book : in out T)
	is begin

		C_Undo_New_Group (Book.Data);

	end Undo_New_Group;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Write (Book : in out T; Str : in String)
	is
		S : C.Strings.chars_ptr := C.Strings.New_String (Str);	
	begin

		C_Write (Book.Data, S);
		C.Strings.Free (S);
		Book.Raise_Error;

	end Write;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Zero (Book : in out T)
	is begin

		C_Zero (Book.Data);

	end Zero;

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function Error (Book : in T) return Error_Code
	is begin

		return C_Error (Book.Data);

	end Error;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Group_Length (Book : in T; Group : in Index) return Size
	is begin

		return C_Group_Length (Book.Data, Group);

	end Group_Length;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Groups_Number (Book : in T) return Size
	is begin

		return C_Groups_Number (Book.Data);

	end Groups_Number;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Length (Book : in T) return Size
	is begin

		return C_Length (Book.Data);

	end Length;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Word (Book : in T; Word : in Index) return String
	is begin

		return C.Strings.Value (C_Word (Book.Data, Word));

	end Word;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Word_In_Group (Book : in T; Group : in Index; Word : in Index) return String
	is begin

		return C.Strings.Value (C_Word_In_Group (Book.Data, Group, Word));

	end Word_In_Group;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Word_Index (Book : in T; Group : in Index; Word : in Index) return Index
	is begin

		return C_Word_Index (Book.Data, Group, Word);

	end Word_Index;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Words_Number (Book : in T) return Size
	is begin

		return C_Words_Number (Book.Data);

	end Words_Number;

	-------------------------------------------------------------------------------------------------
	-- PRIVATE -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Raise_Error (Book : in T)
	is begin

		case Book.Error
		is
			when Error_None => null;
			when others     => raise E;
		end case;

	end Raise_Error;

end Cassette.Book;


