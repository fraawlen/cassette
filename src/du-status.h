/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Utilities (DU) library.
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

#ifndef DU_STATUS_H
#define DU_STATUS_H

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * Macro function to one-line a condition assertion that if it fails, the target status is set to
 * DU_STATUS_FAILURE and the given action is then executed.
 *
 * @param TARGET    : value to update in case of condition check failure, expected of type du_status_t
 * @param CONDITION : condition to check
 * @param ACTION    : code to exectute in code of condition check failure
 */
#define du_status_assert(TARGET, CONDITION, ACTION) {if (!CONDITION) {TARGET = DU_STATUS_FAILURE; ACTION;}};

/**
 * Macro function to one-line a status value check, in case of DU_STATUS_FAILURE value, execute the given
 * action.
 *
 * @param TARGET : status value to check, expected of type du_status_t
 * @param ACTION : code to exectute in case the status has a fail value
 */
#define du_status_test(TARGET, ACTION) {if (TARGET != DU_STATUS_SUCCESS) {ACTION;}};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Status generic values, there are only 2 values, success for no errors at all or failure otherwise, for
 * simplicity.
 */
typedef enum {
	DU_STATUS_SUCCESS = 0,
	DU_STATUS_FAILURE = 1,
} du_status_t;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DU_STATUS_H */
