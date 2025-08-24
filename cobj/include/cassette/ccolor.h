/************************************************************************************************************/
/* PRELUDE **************************************************************************************************/
/************************************************************************************************************/

#pragma once

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/************************************************************************************************************/
/* TYPES ****************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Represention of a RGBA color. Double types bound inside the [0.0 and 1.0] range are used,
 * 	so that they could be passed to cairo's function without conversion.
 *
 * [Parameters]
 *
 * 	r - Red   color component
 * 	g - Green color component
 * 	b - Blue  color component
 * 	a - Alpha color component
 */
struct ccolor
{
	double r;
	double g;
	double b;
	double a;
};

/************************************************************************************************************/
/* GLOBALS **************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Named colors.
 */
constexpr struct ccolor ccolor_transparent = {0.000, 0.000, 0.000, 0.000};
constexpr struct ccolor ccolor_white       = {1.000, 1.000, 1.000, 1.000};
constexpr struct ccolor ccolor_black       = {0.000, 0.000, 0.000, 1.000};
constexpr struct ccolor ccolor_red         = {1.000, 0.000, 0.000, 1.000};
constexpr struct ccolor ccolor_green       = {0.000, 1.000, 0.000, 1.000};
constexpr struct ccolor ccolor_blue        = {0.000, 0.000, 1.000, 1.000};
constexpr struct ccolor ccolor_yellow      = {1.000, 1.000, 0.000, 1.000};
constexpr struct ccolor ccolor_magenta     = {1.000, 0.000, 1.000, 1.000};
constexpr struct ccolor ccolor_cyan        = {0.000, 1.000, 1.000, 1.000};

/************************************************************************************************************/
/* ACCESS ***************************************************************************************************/
/************************************************************************************************************/

/**
 * [Description]
 *
 * 	Converts a 32-bits ARGB color representation into a color object.
 *
 * [Parameters]
 *
 * 	argb - Color uint to convert
 *
 * [Returns]
 *
 * 	Color object
 */
[[gnu::const]] struct ccolor ccolor_from_argb_uint(uint32_t argb);

/**
 * [Description]
 *
 * 	Converts a RGBA color representation with channel values bounded between 0 and 255
 * 	into a color object.
 *
 * [Parameters]
 *
 * 	r - Red   color component
 * 	g - Green color component
 * 	b - Blue  color component
 * 	a - Alpha color component
 *
 * [Returns]
 *
 * 	Color object
 */
[[gnu::const]] struct ccolor ccolor_from_rgba(uint8_t r, uint8_t g, uint8_t b, uint8_t a);

/**
 * [Description]
 *
 * 	Converts a C string into a color object. The given string is interpreted as a char
 * 	representation of an unsigned 32-bit ARGB value (which will get converted with stroul();
 * 	see the documentation of this function for more information). But there is a leading '#
 * 	character, the string instead gets interpreted as a 6-8 digit "#rrggbbaa" hex. In this
 * 	representation, if the optional alpha parameter is omitted, a 0xFF value is assumed.
 *
 * 	If the optinal err parameter is provided, this function will set it to true if the string
 * 	to convert is invalid. Otherwise, it's set to false.
 *
 * [Parameters]
 *
 * 	str - Source string to convert
 * 	err - Optional conversion error check
 *
 * [Returns]
 *
 * 	Color object. Check *err to be certain of the return's validity.
 * 	If the string is NULL, this function always returns ccolor_black.
 */
struct ccolor ccolor_from_str(const char *str, bool *err);

/**
 * [Description]
 *
 * 	Interpolates a color between two given colors.
 *
 * [Parameters]
 *
 * 	color_1 - First  color
 * 	color_2 - Second color
 * 	ratio   - Second / first color ratio in the [0.0 1.0] range used for the interpolation.
 *
 * [Returns]
 *
 * 	interpolated color
 */
[[gnu::const]] struct ccolor ccolor_interpolate(struct ccolor color_1, struct ccolor color_2, double ratio);

/*
 * [Description]
 *
 * 	Converts a given color object to its equivalent ARGB representation within a single 32-bit
 * 	unsigned int. Useful when using colors directly with XCB.
 *
 * [Parameters]
 *
 * 	color - Color object to convert
 *
 * [Returns]
 *
 * 	32-bit argb color value
 */
[[gnu::const]] uint32_t ccolor_to_argb_uint(struct ccolor color);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#ifdef __cplusplus
}
#endif
