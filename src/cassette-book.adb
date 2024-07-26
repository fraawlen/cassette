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

with Cassette;
with Cassette.Error;
with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Extensions; use Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package body Cassette.Book is

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS -------------------------------------------------------------------
	-------------------------------------------------------------------------------------------------

	procedure Clone (Self : out T; Parent : in T)
	is
		function Fn (Parent : System.Address) return System.Address
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cbook_clone";
	begin

		Self.Data := Fn (Parent.Data);
		Self.Check;

	end Clone;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Create (Self : out T)
	is
		function Fn return System.Address
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cbook_create";
	begin

		Self.Data := Fn;
		Self.Check;

	end Create;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Destroy (Self : in out T)
	is
		procedure Fn (Data : System.Address)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cbook_destroy";
	begin

		Fn (Self.Data);
		Self.Data := Placeholder'Address;

	end Destroy;

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Clear (Self : in out T)
	is
		procedure Fn (Data : System.Address)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cbook_clear";
	begin

		Fn (Self.Data);

	end Clear;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Pop_Group (Self : in out T)
	is
		procedure Fn (Data : System.Address)
			with Import        => True,
			     Convention    => C,
			     External_Name => "cbook_pop_group";
	begin

		Fn (Self.Data);

	end Pop_Group;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Pop_Word (Self : in out T)
	is
		procedure Fn (Data : System.Address)
			with Import        => True,
			     Convention    => C,
			     External_Name => "cbook_pop_word";
	begin

		Fn (Self.Data);

	end Pop_Word;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Prealloc (Self : in out T; Bytes : in Size; Words : in Size; Groups : in Size)
	is
		procedure Fn (Data : System.Address; B : size_t; W : size_t; G : size_t)
			with Import        => True,
			     Convention    => C,
			     External_Name => "cbook_prealloc";
	begin
		
		Fn (Self.Data, Bytes, Words, Groups);
		Self.Check;

	end Prealloc;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Repair (Self : in out T)
	is
		procedure Fn (Data : System.Address)
			with Import        => True,
			     Convention    => C,
			     External_Name => "cbook_repair";
	begin

		Fn (Self.Data);

	end Repair;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Write (Self : in out T; Str : in String; Group_Mode : in Mode)
	is
		procedure Fn (Data : System.Address; Str : C.Strings.chars_ptr; Group_Mode : Mode)
			with Import        => True,
			     Convention    => C,
			     External_Name => "cbook_write";
		
		S : C.Strings.chars_ptr := C.Strings.New_String (Str);
		
	begin

		Fn (Self.Data, S, Group_Mode);
		C.Strings.Free (S);
		Self.Check;

	end Write;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Zero (Self : in out T)
	is
		procedure Fn (Data : System.Address)
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cbook_zero";
	begin

		Fn (Self.Data);

	end Zero;

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function Error (Self : in T) return Cassette.Error.T
	is
		function Fn (Data : System.Address) return unsigned
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cbook_error";
	begin

		return Fn (Self.Data);

	end Error;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Group_Length (Self : in T; Group_Index : in Index) return Size
	is
		function Fn (Data : System.Address; Group_Index : size_t) return size_t
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cbook_group_length";
	begin

		return Fn (Self.Data, Group_Index);

	end Group_Length;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Groups_Number (Self : in T) return Size
	is
		function Fn (Data : System.Address) return size_t
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cbook_groups_number";
	begin

		return Fn (Self.Data);

	end Groups_Number;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Length (Self : in T) return Size
	is
		function Fn (Data : System.Address) return size_t
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cbook_length";
	begin

		return Fn (Self.Data);

	end Length;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Word (Self : in T; Word_Index : in Index) return String
	is
		function Fn (Data : System.Address; Group_Index : size_t) return C.Strings.chars_ptr
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cbook_word";
	begin

		return C.Strings.Value (Fn (Self.Data, Word_Index));

	end Word;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Word_In_Group (Self : in T; Group_Index : in Index; Word_Index : in Index) return String
	is
		function Fn (
			Data        : System.Address;
			Group_Index : size_t;
			Word_Index  : size_t)
				return C.Strings.chars_ptr
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cbook_word_in_group";
	begin

		return C.Strings.Value (Fn (Self.Data, Group_Index, Word_Index));

	end Word_In_Group;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Word_Index (Self : in T; Group_Index : in Index; Word_Index : in Index) return Index
	is
		function Fn (
			Data        : System.Address;
			Group_Index : size_t;
			Word_Index  : size_t)
				return size_t
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cbook_word_index";
	begin

		return Fn (Self.Data, Group_Index, Word_Index);

	end Word_Index;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Words_Number (Self : in T) return Size
	is
		function Fn (Data : System.Address) return size_t
			with Import        => True, 
			     Convention    => C, 
			     External_Name => "cbook_words_number";
	begin

		return Fn (Self.Data);

	end Words_Number;

	-------------------------------------------------------------------------------------------------
	-- PRIVATE -------------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Check (Self : in T) is
	begin

		if Self.Error > 0
		then
			raise E;
		end if;

	end Check;

end Cassette.Book;

