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

with Interfaces;
with Interfaces.C;
with Interfaces.C.Extensions;
with System;

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

package body Cassette.Segment is

	-------------------------------------------------------------------------------------------------
	-- CONSTRUCTORS / DESTRUCTORS ------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function I_64 return T
	is
		Seg : T;
	begin

		Seg.Data.Min := -(2 ** 63);
		Seg.Data.Max :=   2 ** 63 - 1;

		return Seg;

	end I_64;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function I_32 return T
	is
		Seg : T;
	begin

		Seg.Data.Min := -(2 ** 31);
		Seg.Data.Max :=   2 ** 31 - 1;

		return Seg;

	end I_32;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function I_16 return T
	is
		Seg : T;
	begin

		Seg.Data.Min := -(2 ** 15);
		Seg.Data.Max :=   2 ** 15 - 1;

		return Seg;

	end I_16;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function I_8 return T
	is
		Seg : T;
	begin

		Seg.Data.Min := -(2 ** 7);
		Seg.Data.Max :=   2 ** 7 - 1;

		return Seg;

	end I_8;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function U_64 return T
	is
		Seg : T;
	begin

		Seg.Data.Min := 0;
		Seg.Data.Max := 2 ** 63 - 1;

		return Seg;

	end U_64;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function U_32 return T
	is
		Seg : T;
	begin

		Seg.Data.Min := 0;
		Seg.Data.Max := 2 ** 31 - 1;

		return Seg;

	end U_32;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function U_16 return T
	is
		Seg : T;
	begin

		Seg.Data.Min := 0;
		Seg.Data.Max := 2 ** 15 - 1;

		return Seg;

	end U_16;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function U_8 return T
	is
		Seg : T;
	begin

		Seg.Data.Min := 0;
		Seg.Data.Max := 2 ** 7 - 1;

		return Seg;

	end U_8;

	-------------------------------------------------------------------------------------------------
	-- IMPURE METHODS ------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	procedure Bind (Segment : in out T)
	is begin

		C_Bind (Segment.Data'Access);

	end Bind;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Grow (Segment : in out T; Length : in Integer_64)
	is begin

		C_Grow (Segment.Data'Access, Length);

	end Grow;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Limit (Segment : in out T; Lim_1 : Integer_64; Lim_2 : Integer_64)
	is begin

		C_Limit (Segment.Data'Access, Lim_1, Lim_2);

	end Limit;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Move (Segment : in out T; Origin : in Integer_64)
	is begin

		C_Move (Segment.Data'Access, Origin);

	end Move;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Offset (Segment : in out T; Length : in Integer_64)
	is begin

		C_Offset (Segment.Data'Access, Length);

	end Offset;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Pad (Segment : in out T; Length : in Integer_64)
	is begin

		C_Pad (Segment.Data'Access, Length);

	end Pad;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Resize (Segment : in out T; Length : in Integer_64)
	is begin

		C_Resize (Segment.Data'Access, Length);

	end Resize;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Scale (Segment : in out T; Scale : in Float)
	is begin

		C_Scale (Segment.Data'Access, C.double(Scale));

	end Scale;

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function Is_In (Segment : in out T; Point : in Integer_64) return Boolean
	is begin

		return Boolean (C_Is_In (Segment.Data, Point));

	end Is_In;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Length (Segment : in T) return Integer_64
	is begin

		return Segment.Data.Length;

	end Length;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

 	function Max (Segment : in T) return Integer_64
	is begin

		return Segment.Data.Max;

	end Max;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Min (Segment : in T) return Integer_64
	is begin

		return Segment.Data.Min;

	end Min;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Origin (Segment : in T) return Integer_64
	is begin

		return Segment.Data.Origin;

	end Origin;

end Cassette.Segment;

