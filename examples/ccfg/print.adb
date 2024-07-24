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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;
with Cassette.Config;   use Cassette;
with Cassette.Error;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

-- In this 1st example, a simple resource look-up is done after loading the configuration. If found, the 
-- values associated with the resource are printed to stdout.

procedure Print
is

	Path : String := "/tmp/ccfg_sample";
	Conf : Config.T;
	
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

	Conf.Push_Source (Path);
	Conf.Push_Param  ("example_param", "value from executable");

	-- Operations

	Conf.Load;

	Print_Resource ("example-1", "a");
	Print_Resource ("example-1", "b");
	Print_Resource ("example-1", "c");
	Print_Resource ("example-1", "d");
	Print_Resource ("example-1", "e");
	Print_Resource ("example-1", "f");
	Print_Resource ("example-1", "g");
	Print_Resource ("example-1", "h");
	Print_Resource ("example-1", "i"); -- Expected to not be found

	-- End

	Conf.Destroy;

exception

	when E : Config.E =>
		Put ("Input tracker errored during operation. Code :");
		Put (Conf.Error'Image);
		New_Line;

end Print;

