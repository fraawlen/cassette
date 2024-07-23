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

with Ada.Text_IO;     use Ada.Text_IO;
with Cassette;        use Cassette;
with Cassette.Error;
with Cassette.Inputs; 

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

procedure Tracker
is

	Tracker : Inputs.T;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Print (ID : Inputs.Identifier)
	is
		I : Inputs.Index;
	begin

		Put ("Input with ID" & ID'Image);
		if Tracker.Find (ID, I)
		then
			Put (" was found:");
			Put (": X =" & Tracker.X(I)'Image);
			Put (", Y =" & Tracker.Y(I)'Image);
			New_Line;
		else
			Put_Line (" was not found");
		end if;	

	end Print;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

begin

	Tracker.Create (10);

	Tracker.Push (1,    0, 1000);
	Tracker.Push (2,   34, 1450);
	Tracker.Push (3, 1032,  653);
	Tracker.Push (4,  327, 2459);

	Put_Line ("Tracker usage :" & Tracker.Load'Image);
	
	Tracker.Pull_ID    (1);
	Tracker.Pull_Index (2);

	Put_Line ("Tracker usage :" & Tracker.Load'Image);

	Print (2);
	Print (3);
	Print (4);
	Print (5);
	Print (8);

	Tracker.Clear;

	Put_Line ("Tracker usage :" & Tracker.Load'Image);

	Tracker.Destroy;

exception

	when E : Inputs.E =>
		Put ("Input tracker errored during operation. Code :");
		Put (Tracker.Error'Image);
		New_Line;

end Tracker;
