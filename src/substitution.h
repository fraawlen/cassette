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

#ifndef SUBSTITUTION_H
#define SUBSTITUTION_H

#include "context.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

dr_token_kind_t dr_subtitution_apply(dr_context_t *ctx, char token[static DR_TOKEN_N], double *math_result);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#endif /* SUBSTITUTION_H */
