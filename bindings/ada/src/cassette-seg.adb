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

package body Cassette.Seg is

	-------------------------------------------------------------------------------------------------
	-- PRESETS --------------------------------------------------------------------------------------
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

	procedure Bind (Seg : in out T)
	is begin

		C_Bind (Seg.Data'Access);

	end Bind;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Grow (Seg : in out T; Length : in Integer_64)
	is begin

		C_Grow (Seg.Data'Access, Length);

	end Grow;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Limit (Seg : in out T; Lim_1 : Integer_64; Lim_2 : Integer_64)
	is begin

		C_Limit (Seg.Data'Access, Lim_1, Lim_2);

	end Limit;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Move (Seg : in out T; Origin : in Integer_64)
	is begin

		C_Move (Seg.Data'Access, Origin);

	end Move;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Offset (Seg : in out T; Length : in Integer_64)
	is begin

		C_Offset (Seg.Data'Access, Length);

	end Offset;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Pad (Seg : in out T; Length : in Integer_64)
	is begin

		C_Pad (Seg.Data'Access, Length);

	end Pad;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Resize (Seg : in out T; Length : in Integer_64)
	is begin

		C_Resize (Seg.Data'Access, Length);

	end Resize;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	procedure Scale (Seg : in out T; Scale : in Float)
	is begin

		C_Scale (Seg.Data'Access, C.double(Scale));

	end Scale;

	-------------------------------------------------------------------------------------------------
	-- PURE METHODS --------------------------------------------------------------------------------- 
	-------------------------------------------------------------------------------------------------

	function Is_In (Seg : in T; Point : in Integer_64) return Boolean
	is begin

		return Boolean (C_Is_In (Seg.Data, Point));

	end Is_In;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Length (Seg : in T) return Integer_64
	is begin

		return Seg.Data.Length;

	end Length;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

 	function Max (Seg : in T) return Integer_64
	is begin

		return Seg.Data.Max;

	end Max;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Min (Seg : in T) return Integer_64
	is begin

		return Seg.Data.Min;

	end Min;

	-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

	function Origin (Seg : in T) return Integer_64
	is begin

		return Seg.Data.Origin;

	end Origin;

end Cassette.Seg;

