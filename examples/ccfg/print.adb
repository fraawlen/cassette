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

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Text_IO;            use Ada.Text_IO;
with Cassette.Config;        use Cassette;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

-- In this 1st example, a simple resource look-up is done after loading the configuration. If found, the 
-- values associated with the resource are printed to stdout.

procedure Print
is

	Conf : Config.T;
	Data : String :=
		       "LET ratio 0.5"
		& LF & "example a TIME"
		& LF & "example b 45.5"
		& LF & "example c 50 60 70"
		& LF & "example d TRUE FALSE"
		& LF & "example e new_value"
		& LF & "example f ($$ example_param)"
		& LF & "example g (CITRPL #000000 #ffff8000 ($ ratio))"
		& LF & "example h ($ ratio)";
	
	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Print_Resource (Namespace : String; Property : String) is
	begin

		Put (Namespace & " " & Property);
		Conf.Fetch (Namespace, Property);
		while Conf.Iterate
		loop
			Put (" " & Conf.Resource);
		end loop;
		New_Line;

	end Print_Resource;
	
	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

begin

	-- Setup

	Conf.Create;
	Conf.Push_Param ("example_param", "value from executable");

	-- Operations

	Conf.Load_Internal (Data);

	Print_Resource ("example", "a");
	Print_Resource ("example", "b");
	Print_Resource ("example", "c");
	Print_Resource ("example", "d");
	Print_Resource ("example", "e");
	Print_Resource ("example", "f");
	Print_Resource ("example", "g");
	Print_Resource ("example", "h");
	Print_Resource ("example", "i"); -- Expected to not be found

	-- End

	Conf.Destroy;

exception

	when Config.E =>
		Put ("Input tracker errored during operation. Code : ");
		Put (Conf.Error'Image);
		New_Line;

end Print;

