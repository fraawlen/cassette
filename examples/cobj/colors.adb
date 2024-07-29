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

with Ada.Text_IO;    use Ada.Text_IO;
with Cassette.Color; use Cassette;
with Cassette.Str;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

procedure Colors
is
	Cl_1 : Color.T;
	Cl_2 : Color.T;
	Cl_3 : Color.T;
	Cl_4 : Color.T;
	S    : Str.T;
	
	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
	
	function To_Str (Val : Color.Channel) return String is
	begin

		S.Clear;
		S.Append (Float(Val) * 255.0);
		S.Pad    (" ", 0, 4);

		return S.Chars;
	
	end To_Str;
	
	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Print (Cl : Color.T) is
	begin

		Put (  "R =" & To_Str(Cl.R));
		Put (", G =" & To_Str(Cl.G));
		Put (", B =" & To_Str(Cl.B));
		Put (", A =" & To_Str(Cl.A));
		New_Line;

	end Print;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

begin

	S.Create;
	S.Set_Precision (0);

	Cl_1 := Color.Blue;
	Cl_2 := Color.From_Str    ("#FF0000");
	Cl_3 := Color.From_RGBA   (128, 128, 128);
	Cl_4 := Color.Interpolate (Cl_1, Cl_2, 0.5);

	Print (Cl_1);
	Print (Cl_2);
	Print (Cl_3);
	Print (Cl_4);

	S.Destroy;

exception

	when Str.E =>
		Put ("String errored during operation. Code :");
		Put (S.Error'Image);
		New_Line;

	when Color.E =>
		Put ("Color conversion errored during operation.");
		New_Line;
	
end Colors;
