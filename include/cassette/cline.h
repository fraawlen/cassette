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

#include <stdbool.h>
#include <stdint.h>

#if __GNUC__ > 4
	#define CLINE_NONNULL(...) __attribute__((nonnull (__VA_ARGS__)))
	#define CLINE_CONST        __attribute__((const))
#else
	#define CLINE_NONNULL(...)
	#define CLINE_CONST
#endif

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 * Representation of a 1-dimension line defined by a length and an origin. As long as the related procedures
 * are used to manipulate cline's values, the following equation is guaranteed to be true :
 * min <= origin + length <= max. Moreover, all procedures are overflow and underflow protected.
 */
struct cline
{
	int64_t origin;
	int64_t length;
	int64_t min;
	int64_t max;
};

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

/**
 * Macro to initialize a cline with default values.
 */
#define CLINE_DEFAULT (struct cline){.origin = 0, .length = 0, .min = INT64_MIN, .max = INT64_MAX}

/************************************************************************************************************/
/* PROCEDURES ***********************************************************************************************/
/************************************************************************************************************/

/**
 * Rechecks and corrects a cline so that its parameters respect this equation : min <= origin + length <= max.
 *
 * @param line : Line to interact with
 */
void
cline_bind(struct cline *line)
CLINE_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Adds a value to the cline's length.
 *
 * @param line   : Line to interact with
 * @param length : Distance value
 */
void
cline_grow(struct cline *line, int64_t length)
CLINE_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Sets new limits. The order of lim_1 or lim_2 does not matter. If necessary, origin and length values will
 * also be udpated to respect the new limits.
 *
 * @param line  : Line to interact with
 * @param lim_1 : First bound
 * @param lim_2 : Second bound
 */
void
cline_limit(struct cline *line, int64_t lim_1, int64_t lim_2)
CLINE_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Sets a new origin. If necessary, the length will also be udpated to respect the limits.
 *
 * @param line   : Line to interact with
 * @param origin : Position value
 */
void
cline_move(struct cline *line, int64_t origin)
CLINE_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Add a value to the cline's origin. If necessary, the length will also be udpated to respect the limits.
 *
 * @param line   : Line to interact with
 * @param length : Distance value
 */
void
cline_offset(struct cline *line, int64_t length)
CLINE_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Offsets the line by a length value, then decreases its length (stored by the cline) by 2 * length (the
 * funtion parameter).
 *
 * @param line   : Line to interact with
 * @param lenght : Distance value
 */
void
cline_pad(struct cline *line, int64_t length)
CLINE_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Sets a new length value.
 *
 * @param line   : Line to interact with
 * @param length : Distance value
 */
void
cline_resize(struct cline *line, int64_t length)
CLINE_NONNULL(1);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/**
 * Mutilplies the origin and length.
 *
 * @param line  : Line to interact with
 * @param scale : Multiplier value
 */
void
cline_scale(struct cline *line, double scale)
CLINE_NONNULL(1);

/************************************************************************************************************/
/* FUNCTIONS ************************************************************************************************/
/************************************************************************************************************/

/**
 * Checks whether a point coordinate is on the line.
 *
 * @param line  : Line to interact with
 * @param point : Position value
 *
 * @return : True if it is, false otherwise
 */
bool
cline_is_in(struct cline line, int64_t point)
CLINE_CONST;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif
