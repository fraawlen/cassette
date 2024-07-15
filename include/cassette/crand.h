/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Objects (COBJ) library.
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

#ifdef __cplusplus
extern "C" {
#endif

#if __GNUC__ > 4
	#define CRAND_NONNULL(...) __attribute__((nonnull (__VA_ARGS__)))
#else
	#define CRAND_NONNULL(...)
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 * Keeper value that keeps track of the LCG (rand48-based) state.
 */
typedef unsigned long long int crand;

/************************************************************************************************************/
/* PROCEDURES ***********************************************************************************************/
/************************************************************************************************************/

/**
 * Gets the next random value bound between lim_1 and lim_2 for the given keeper.
 *
 * @param rand  : Keeper value to interact with
 * @param lim_1 : First bound 
 * @param lim_2 : Second bound
 *
 * @return : Generated random value
 */
double
crand_get(crand *rand, double lim_1, double lim_2)
CRAND_NONNULL(1);

/**
 * Sets the initial value of the keeper.
 *
 * @param rand : Keeper value to interact with
 * @param seed : Initial value to apply
 */
void
crand_seed(crand *rand, unsigned long long int seed)
CRAND_NONNULL(1);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif
