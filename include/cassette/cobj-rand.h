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

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * Keeper value that keeps track of the LCG (rand48-based) state.
 */
typedef unsigned long long cobj_rand_t;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Sets the initial value of the keeper.
 *
 * @param rand Keeper value to interact with
 * @param seed Initial value to apply
 */
void cobj_rand_seed(cobj_rand_t *rand, unsigned long long seed);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Gets the next random value bound between lim_1 and lim_2 for the given keeper.
 *
 * @param rand Keeper value to interact with
 * @param lim_1 First bound of the possible returned value
 * @param lim_2 Second bound of the posiible returned value
 *
 * @return Generated random value
 */
double cobj_rand_get(cobj_rand_t *rand, double lim_1, double lim_2);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif
