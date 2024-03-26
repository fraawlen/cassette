/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Derelict Resources (DR) library.
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

#ifndef FILE_H
#define FILE_H

#include <stdbool.h>

#include "config.h"
#include "context.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

void dr_file_parse_child(dr_context_t *ctx_parent, const char *filename);

bool dr_file_parse_root(dr_config_t *cfg, const char *filename);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#endif /* FILE_H */
