/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Graphics (DG) GUI library.
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

#ifndef DG_BASE_CONFIG_H_PRIVATE
#define DG_BASE_CONFIG_H_PRIVATE

#include <stdbool.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * Initialise the module's config by pushing the associated resource group to the resource tracker.
 *
 * @return : inherited from dg_core_resource_push_group()
 *
 * @error DG_CORE_ERRNO_MEMORY    : inherited from dg_core_resource_push_group()
 * @error DG_CORE_ERRNO_STACK     : inherited from dg_core_resource_push_group()
 * @error DG_CORE_ERRNO_HASHTABLE : inherited from dg_core_resource_load_group()
 */
bool dg_base_config_init(void);

/**
 * Remove the module's config associated resource group from the resource tracker.
 */
void dg_base_config_reset(void);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#endif /* DG_BASE_CONFIG_H_PRIVATE */
