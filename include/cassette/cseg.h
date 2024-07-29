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
	#define CSEG_NONNULL(...) __attribute__((nonnull (__VA_ARGS__)))
	#define CSEG_CONST        __attribute__((const))
#else
	#define CSEG_NONNULL(...)
	#define CSEG_CONST
#endif

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 * Representation of a 1-dimension segment defined by a length and an origin. As long as segment's methods are
 * used to manipulate segment's values, no overflow or underflow can occur and the following equation is
 * guaranteed to be true: min <= origin + length <= max.
 */
struct cseg
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
 * Macros to initialize a cseg with default values.
 */
#define CSEG_I64 (struct cseg){ .origin = 0, .length = 0, .min = INT64_MIN, .max = INT64_MAX  }
#define CSEG_I32 (struct cseg){ .origin = 0, .length = 0, .min = INT32_MIN, .max = INT32_MAX  }
#define CSEG_I16 (struct cseg){ .origin = 0, .length = 0, .min = INT16_MIN, .max = INT16_MAX  }
#define CSEG_I8  (struct cseg){ .origin = 0, .length = 0, .min = INT8_MIN,  .max = INT8_MAX   }
#define CSEG_U64 (struct cseg){ .origin = 0, .length = 0, .min = 0,         .max = INT64_MAX  }
#define CSEG_U32 (struct cseg){ .origin = 0, .length = 0, .min = 0,         .max = UINT32_MAX }
#define CSEG_U16 (struct cseg){ .origin = 0, .length = 0, .min = 0,         .max = UINT16_MAX }
#define CSEG_U8  (struct cseg){ .origin = 0, .length = 0, .min = 0,         .max = UINT8_MAX  }

/************************************************************************************************************/
/* IMPURE METHODS *******************************************************************************************/
/************************************************************************************************************/

/**
 * Rechecks and corrects a segment so that its parameters respect this equation :
 * min <= origin + length <= max.
 *
 * @param seg : Segment to interact with
 */
void
cseg_bind(struct cseg *seg)
CSEG_NONNULL(1);

/**
 * Adds a value to the segment's length.
 *
 * @param seg    : Segment to interact with
 * @param length : Distance value
 */
void
cseg_grow(struct cseg *seg, int64_t length)
CSEG_NONNULL(1);

/**
 * Sets new limits. The order of lim_1 or lim_2 does not matter. If necessary, origin and length values will
 * also be udpated to respect the new limits.
 *
 * @param seg   : Segment to interact with
 * @param lim_1 : First bound
 * @param lim_2 : Second bound
 */
void
cseg_limit(struct cseg *seg, int64_t lim_1, int64_t lim_2)
CSEG_NONNULL(1);

/**
 * Sets a new origin. If necessary, the length will also be udpated to respect the limits.
 *
 * @param seg    : Segment to interact with
 * @param origin : Position value
 */
void
cseg_move(struct cseg *seg, int64_t origin)
CSEG_NONNULL(1);

/**
 * Add a value to the segmetn's origin. If necessary, the length will also be udpated to respect the limits.
 *
 * @param seg    : Segment to interact with
 * @param length : Distance value
 */
void
cseg_offset(struct cseg *seg, int64_t length)
CSEG_NONNULL(1);

/**
 * Offsets the segment by a length value, then decreases its length (stored by the cseg) by 2 * length (the
 * funtion parameter).
 *
 * @param seg    : Segment to interact with
 * @param lenght : Distance value
 */
void
cseg_pad(struct cseg *seg, int64_t length)
CSEG_NONNULL(1);

/**
 * Sets a new length value.
 *
 * @param seg    : Segment to interact with
 * @param length : Distance value
 */
void
cseg_resize(struct cseg *seg, int64_t length)
CSEG_NONNULL(1);

/**
 * Mutilplies the origin and length.
 *
 * @param seg   : Segment to interact with
 * @param scale : Multiplier value
 */
void
cseg_scale(struct cseg *seg, double scale)
CSEG_NONNULL(1);

/************************************************************************************************************/
/* PURE METHODS *********************************************************************************************/
/************************************************************************************************************/

/**
 * Checks whether a point coordinate is on the seg.
 *
 * @param seg   : Segment to interact with
 * @param point : Position value
 *
 * @return : True if it is, false otherwise
 */
bool
cseg_is_in(struct cseg seg, int64_t point)
CSEG_CONST;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif
