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
with Cassette.Rand; use Cassette;
with Cassette.Str;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

procedure Random
is

	R : Rand.T;
	S : Str.T;
	
	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
	
	procedure Sequence (Seed : in Rand.Seed_Value; Min : in Float; Max : in Float) is
	begin

		R.Seed (Seed);

		Put ("Seed" & Seed'Image & ":");
		for I in 1 .. 6
		loop
			S.Clear;
			S.Append (R.Next(Min, Max));
			S.Pad    ("_", 0, 7);
			Put      (" " & S.Chars);
		end loop;
		New_Line;

	end Sequence;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

begin

	-- Setup

	S.Create;
	S.Set_Precision (2);

	-- Operations

	Sequence (0,  0.0,   10.0);
	Sequence (1,  0.0, 1000.0);
	Sequence (2,  0.0,   10.0);
	Sequence (3, -1.0,    1.0);

	-- End

	S.Destroy;

exception

	when Str.E =>
		Put ("String errored during operation. Code : ");
		Put (S.Error'Image);
		New_Line;
	
end Random;
