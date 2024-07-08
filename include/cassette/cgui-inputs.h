/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Graphics (CGUI) library.
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

#include <stdint.h>
#include <stdlib.h>

#include "cgui-attributes.h"

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 *
 */
typedef struct cgui_inputs cgui_inputs;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
enum cgui_inputs_err
{
	CGUI_INPUTS_OK        = 0,
	CGUI_INPUTS_INVALID   = 1,
	CGUI_INPUTS_OVERFLOW  = 1 << 1,
	CGUI_INPUTS_MEMORY    = 1 << 2,
	CGUI_INPUTS_BAD_INPUT = 1 << 3,
};

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
struct cgui_input
{
	unsigned int id;
	const void *ref;
	int16_t x;
	int16_t y;
};

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

/**
 * A macro that gives uninitialized books a non-NULL value that is safe to use with the input tracker's
 * related functions. However, any function called with a handle set to this value will return early and
 * without any side effects.
 */
#define CGUI_INPUTS_PLACEHOLDER &cgui_inputs_placeholder_instance

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Global input tracker instance with the error state set to CGUI_INPUT_INVALID. This instance is only made
 * available to allow the static initialization of input tracker pointers with the macro
 * CGUI_INPUT_PLACEHOLDER.
 */
extern cgui_inputs cgui_inputs_placeholder_instance;

/************************************************************************************************************/
/* CONSTRUCTORS / DESTRUCTORS *******************************************************************************/
/************************************************************************************************************/

/** 
 *
 */
cgui_inputs *
cgui_inputs_clone(const cgui_inputs *inputs)
CGUI_NONNULL_RETURN
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/** 
 *
 */
cgui_inputs *
cgui_inputs_create(size_t max_inputs)
CGUI_NONNULL_RETURN;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/** 
 *
 */
void
cgui_inputs_destroy(cgui_inputs *inputs)
CGUI_NONNULL(1);

/************************************************************************************************************/
/* PROCEDURES ***********************************************************************************************/
/************************************************************************************************************/

/**
 * Convenience for-loop wrapper.
 */
#define CGUI_INPUTS_FOR_EACH(INPUTS, I) for(size_t I = 0; I < cgui_inputs_load(INPUTS); I++)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Convenience inverse for-loop wrapper.
 */
#define CGUI_INPUTS_FOR_EACH_REV(INPUTS, I) for(size_t I = cgui_inputs_load(INPUTS) - 1; I < SIZE_MAX; I--)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/** 
 *
 */
void
cgui_inputs_clear(cgui_inputs *inputs)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/** 
 *
 */
void
cgui_inputs_pull_id(cgui_inputs *inputs, unsigned int id)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/** 
 *
 */
void
cgui_inputs_pull_index(cgui_inputs *inputs, size_t index)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/** 
 *
 */
void
cgui_inputs_push(cgui_inputs *inputs, unsigned int id, int x, int y, void *ref)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void
cgui_inputs_repair(cgui_inputs *inputs)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 *
 */
void
cgui_inputs_resize(cgui_inputs *inputs, size_t max_inputs)
CGUI_NONNULL(1);

/************************************************************************************************************/
/* FUNCTIONS ************************************************************************************************/
/************************************************************************************************************/

/** 
 *
 */
enum cgui_inputs_err
cgui_inputs_error(const cgui_inputs *inputs)
CGUI_NONNULL(1)
CGUI_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/** 
 *
 */
bool
cgui_inputs_find(const cgui_inputs *inputs, unsigned int id, size_t *index)
CGUI_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/** 
 *
 */
struct cgui_input
cgui_inputs_get(const cgui_inputs *inputs, size_t index)
CGUI_NONNULL(1)
CGUI_PURE;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/** 
 *
 */
size_t
cgui_inputs_load(const cgui_inputs *inputs)
CGUI_NONNULL(1)
CGUI_PURE;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif
