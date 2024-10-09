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
with Cassette.Inputs; use Cassette;
with Interfaces.C;    use Interfaces.C;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

procedure Tracker
is
	Tracker : Inputs.T;
begin

	Tracker.Create (10);

	Tracker.Push (1,    0, 1000);
	Tracker.Push (2,   34, 1450);
	Tracker.Push (3, 1032,  653);
	Tracker.Push (4,  327, 2459);
	Tracker.Push (8, 1200,  200);
	Tracker.Push (9,    0,   11);
	Tracker.Push (3,  999, 1450);

	Tracker.Pull_ID    (1);
	Tracker.Pull_Index (2);

	for I in 0 .. Tracker.Load - 1
	loop
		Put ("ID :"  & Tracker.ID(I)'Image);
		Put (", X =" & Tracker.X(I)'Image);
		Put (", Y =" & Tracker.Y(I)'Image);
		New_Line;
	end loop;

	Tracker.Destroy;

exception

	when Inputs.E =>
		Put ("Input tracker errored during operation. Code :");
		Put (Tracker.Error'Image);
		New_Line;

end Tracker;
