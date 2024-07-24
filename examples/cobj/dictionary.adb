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

with Ada.Text_IO;   use Ada.Text_IO;
with Cassette.Dict; use Cassette;
with Cassette.Str;
with Cassette.Error;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

procedure Dictionary
is

	D : Dict.T;
	S : Str.T;
	
	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
	
	procedure Print_Stats is
	begin

		S.Clear;
		S.Append (D.Load_Factor);

		Put ("Load:");
		Put (D.Load'Image & " used slots (");
		Put (S.Chars      & " load factor)");
		New_Line;

	end Print_Stats;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
	
	procedure Print_Value (Key : in String; Group : in Dict.Group_Value)
	is
		Value : Dict.Slot_Value;
	begin

		if D.Find (Key, Group, Value)
		then
			Put (Key         & ", ");
			Put (Group'Image & ", ");
			Put (Value'Image);
			New_Line;
		end if;

	end Print_Value;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

begin

	-- Setup

	D.Create;
	S.Create;

	S.Set_Precision (3);

	-- Operations

	D.Write ("test", 0, 12);
	D.Write ("test", 1, 32);
	D.Write ("test", 1, 44);
	D.Write ("AAAA", 0, 99);
	D.Write ("ASXC", 0, 56);

	D.Erase ("AAAA", 0);
	D.Clear_Group (0);

	Print_Stats;
	Print_Value ("test", 0);
	Print_Value ("test", 1);
	Print_Value ("AAAA", 0);
	Print_Value ("ASXC", 0);

	-- End

	D.Destroy;
	S.Destroy;

exception
	
	when E : Dict.E =>
		Put ("Dictionary errored during operation. Code :");
		Put (D.Error'Image);
		New_Line;

	when E : Str.E =>
		Put ("String errored during operation. Code :");
		Put (S.Error'Image);
		New_Line;
	
end Dictionary;
