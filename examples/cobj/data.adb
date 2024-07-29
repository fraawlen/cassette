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
with Cassette.Book;
with Cassette.Rand;
with Cassette.Str;
with Interfaces.C;  use Interfaces.C;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

-- This example demonstrates how to use a Cassette book and dictionary together to store data from some CSV
-- file. It's assumed that the first column stores a row title and will be used as a key to access the data in
-- the following columns. The book will store the data, and each row will be assigned a group. At the same
-- time, the dictionary will store the indexes that access the right data rows. For this example, the data
-- will be randomly generated and not read from an actual file.
--
-- Example data :
--
--	d_1,	345,	234,	2,
--	d_2,	0,	0,	111,	123,	99,
--	d_3,	34,
--	d_4,	094,	0944,

procedure Data
is

	B : Book.T;
	D : Dict.T;
	R : Rand.T;
	S : Str.T;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Generate_Sequence (Key : in String)
	is begin

		B.Prepare_New_Group;
		D.Write (Key, 0, B.Groups_Number);
		for I in 1 .. Integer (R.Next(1.0, 5.0)) -- Generate a random number of columns.
		loop

			S.Clear;
			S.Append (R.Next (0.0, 999.0));
			B.Write  (S.Chars);

		end loop;

	end Generate_Sequence;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
	
	procedure Print_Sequence (Key : in String)
	is
		I : Index;
	begin

		if not D.Find (Key, 0, I)
		then
			return;
		end if;

		Put (Key & ",");
		for J in 0 .. (B.Group_Length (I) - 1)
		loop

			S.Clear;
			S.Append (B.Word_In_Group(I, J));
			S.Pad    (" ", 0, 3);
			Put      (" " & S.Chars & ",");

		end loop;
		New_Line;

	end Print_Sequence;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

begin

	-- Setup

	B.Create;
	D.Create;
	S.Create;

	S.Set_Precision (0);
	R.Seed          (1);

	-- Generate data

	Generate_Sequence ("d_1");
	Generate_Sequence ("d_2");
	Generate_Sequence ("d_3");
	Generate_Sequence ("d_4");

	-- Retrieve data

	Print_Sequence ("d_1");
	Print_Sequence ("d_2");
	Print_Sequence ("d_3");
	Print_Sequence ("d_4");
	Print_Sequence ("d_5"); -- Doesn't exists, will be skipped.

	-- End

	B.Destroy;
	D.Destroy;
	S.Destroy;

exception

	when Book.E =>
		Put ("Book errored during operation. Code : ");
		Put (B.Error'Image);
		New_Line;

	when Dict.E =>
		Put ("Dictionary errored during operation. Code : ");
		Put (D.Error'Image);
		New_Line;

	when Str.E =>
		Put ("String errored during operation. Code : ");
		Put (S.Error'Image);
		New_Line;

end Data;
