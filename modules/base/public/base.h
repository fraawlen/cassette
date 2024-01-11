/**
 * Copyright © 2024 Frawwlen <fraawlen@posteo.net>
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

#ifndef DG_BASE_H
#define DG_BASE_H

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/**
 * Initialises this module and enables the other functions of this header and sub-headers (unless explicitely
 * stated). Should only be called once until dg_base_reset(). Can be called again after, however all
 * components created with this header and sub-headers should only be used within the session they have been
 * instantiated in. This module and its initialisation is dependent on the core module and therefore requires
 * the core module to be initialised first to use it. If any error is raised when this function is called,
 * then the module failed to initialise and remain in an unitialised state.
 *
 * @error DG_CORE_ERRNO_DEPENDENCY : the core module has not been initialised before this function call.
 * @error DG_CORE_ERRNO_MEMORY     : internal configuration issues, the module can't initialize 
 * @error DG_CORE_ERRNO_STACK      : inherited from dg_core_resource_push_group()
 * @error DG_CORE_ERRNO_HASHTABLE  : inherited from dg_core_resource_load_group()
 */
void dg_base_init(void);

/**
 * Resets the module to its inital state pre dg_base_init() and frees all memory internally allocated.
 */
void dg_base_reset(void);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

 /**
 * Checks if the module has been initialized.
 * This function can be used when the module is not initialized.
 *
 * @return : self-explanatory
 */
bool dg_base_is_init(void);

/************************************************************************************************************/
/* CELLS (SUB-INCLUDES) *************************************************************************************/
/************************************************************************************************************/

/* passives */

#include "cells/gap.h"
#include "cells/gauge.h"
#include "cells/indicator.h"
#include "cells/label.h"
#include "cells/placeholder.h"
#include "cells/spinner.h"

/* interactables */

#include "cells/button.h"
#include "cells/switch.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif

#endif /* DG_BASE_H */
