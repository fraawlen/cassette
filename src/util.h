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

#ifndef UTIL_H
#define UTIL_H

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

double dr_util_limit(double d, double lim_1, double lim_2);

double dr_util_interpolate(double d_1, double d_2, double ratio);

void dr_util_sort_pair(double *d_1, double *d_2);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#endif /* UTIL_H */

