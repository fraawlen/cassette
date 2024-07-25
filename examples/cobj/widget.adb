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
with Cassette.Str;   use Cassette;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

-- This example demonstrates the use of the cstr string objects in an interactable pseudo GUI widget that
-- displays text in monospace. The text is wrapped automatically to fit the width of the widget.
-- For this, two strings are needed. The first one, Str_Ref, will hold the text data in its original form,
-- as input by an end-user or set by the application developer without any extra newlines. The second string,
-- Str_Wrap, will be a wrapped version of the first string that can fit inside the widget. The first string
-- gets updated only when its content changes, while the second one updates when either the contents of the
-- first one change or if the widget gets resized.
--
--
--       original text                   widget text
--         (Str_Ref)                     (Str_Wrap)
--
--                                      ┏━━━━━━━━━━━┓
--                                      ┃ Hello Wor ┃
--       "Hello World!"       -->       ┃ ld!       ┃ 
--                                      ┃           ┃
--                                      ┗━━━━━━━━━━━┛

procedure Widget
is

	Str_Ref  : Str.T;
	Str_Wrap : Str.T;
	Columns  : Str.Size := 10;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	-- Because the wrapped string is shown to the end-user, it's also the one being interacted with.
	-- This function will first get the codepoint offset that matches the insertion point at the
	-- given coordinates of the wrapped string. Then, this offset gets converted into an 'unwrapped'
	-- offset that can be used with the reference string. New data can then be added to the reference
	-- string that will match the 2d coordinates of the wrapped string.

	procedure Insert_2d (Text : in String; Row : in Str.Index; Col : in Str.Index)
	is
	
		Offset_Ref  : Str.Index;
		Offset_Wrap : Str.Index;

	begin

		Offset_Wrap := Str_Wrap.Coords_Offset   (Row, Col);
		Offset_Ref  := Str_Ref.Unwrapped_Offset (Str_Wrap, Offset_Wrap);

		Str_Ref.Insert (Text, Offset_Ref);

	end Insert_2D;
	
	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Update_Wrap is
	begin

		Str_Wrap.Clear;
		Str_Wrap.Append (Str_Ref);
		Str_Wrap.Wrap   (Columns);

		Put_Line (Str_Wrap.Chars);
		Put ("-> ");
		Put (Str_Wrap.Height'Image      & " rows x ");
		Put (Str_Wrap.Width'Image       & " cols / ");
		Put (Str_Wrap.Length'Image      & " utf8-characters / ");
		Put (Str_Wrap.Byte_Length'Image & " bytes");
		New_Line;
		New_Line;

	end Update_Wrap;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

begin

	-- Setup

	Str_Ref.Create;
	Str_Wrap.Create;

	Str_Ref.Append ("This is a loooooooooooooooooooooooooooooooooooong line of text!");

	Update_Wrap;

	-- We assume the end-user wrote 'NEW' into the widget at the 3rd row and 6th column
	-- (index 2 and 5). Once the new text has been added to the reference string, the wrapped
	-- string is updated to display the changes.

	Insert_2D ("NEW", 2, 5);

	Update_Wrap;

	-- We now assume that the widget's width changes. The reference string is unchanged, but the
	-- wrapped string needs to be updated to use of all the new available space.

	Columns := 25;

	Update_Wrap;

	-- End

	Str_Ref.Destroy;
	Str_Wrap.Destroy;

exception

	when E : Str.E =>
		Put ("Strings errored during operation. Code :");
		Put (Str_Ref.Error'Image & "-");
		Put (Str_Wrap.Error'Image);
		New_Line;

end Widget;
