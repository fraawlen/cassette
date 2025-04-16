/**
 * Copyright © 2024-2025 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette library.
 *
 * This library is free software; you can redistribute it and/or modify it either under the terms of the GNU
 * Lesser General Public License as published by the Free Software Foundation; either version 3.0 of the
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

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Keeper value that keeps track of the LCG (rand48-based) state.
 */
typedef unsigned long crand;

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Sets the initial value of the keeper.
 *
 * [Parameters]
 *
 * 	seed - Initial value to apply
 *
 * [Returns]
 *
 * 	Value
 */
[[nodiscard]] crand crand_seed(unsigned long seed);

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Gets the next random value bound between lim_1 and lim_2 for the given keeper.
 * 	Calling this function on a NULL rand has no effect.
 * 	lim_1 and lim_2 can be in any order.
 *
 * [Parameters]
 *
 * 	rand  - Keeper value to interact with
 * 	lim_1 - First bound 
 * 	lim_2 - Second bound
 *
 * [Returns]
 *
 * 	Generated random value.
 * 	If rand is NULL, this function always returns 0.0.
 */
double crand_next(crand *rand, double lim_1, double lim_2);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif
