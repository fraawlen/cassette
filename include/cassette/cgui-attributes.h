/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Configuration (CCFG) library.
 *
 * This library is free software; you can redistribute it and/or modify it either under the terms of the GNU
 * Lesser General Public License as published by the Free Software Foundation; either version 2.1 of the
 * License or (at your option) any later version.
 *
 * This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
 * See the LGPL for the specific language governing rights and limitations.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this program. If not,
 * see <http://www.gnu.org/licenses/>.
 */

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#pragma once

#if __GNUC__ > 4
	#define CGUI_NONNULL_RETURN __attribute__((returns_nonnull))
	#define CGUI_NONNULL(...)   __attribute__((nonnull (__VA_ARGS__)))
	#define CGUI_PURE           __attribute__((pure))
	#define CGUI_CONST          __attribute__((const))
#else
	#define CGUI_NONNULL_RETURN
	#define CGUI_NONNULL(...)
	#define CGUI_PURE
	#define CGUI_CONST
#endif


