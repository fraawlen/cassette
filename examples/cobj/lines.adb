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

with Ada.Text_IO;      use Ada.Text_IO;
with Cassette.Segment; use Cassette;
with Interfaces;       use Interfaces;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

procedure Lines
is
	S : Segment.T := Segment.I_8;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Print (Seg : in Segment.T)
	is begin

		Put (Seg.Min'Image    & " <=");
		Put (Seg.Origin'Image & " + ");
		Put (Seg.Length'Image & " <=");
		Put (Seg.Max'Image);
		New_Line;

	end Print;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
begin

	S.Limit  (20, -20);
	S.Move   (7);
	S.Resize (-5);
	S.Grow   (-30);
	S.Offset (-7);

	Print (S);

end Lines;
