#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#include <derelict/du.h>

#include "dr.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static void _load  (dr_config_t *cfg);
static void _read  (dr_config_t *cfg, const char *namespace, const char *property);
static void _reset (dr_config_t *cfg);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(void)
{
	dr_config_t *cfg = dr_config_create(0);

	_load(cfg);

	_read(cfg, "font",       "face");
	_read(cfg, "font",       "size");
	_read(cfg, "gap",         "color_background");
	_read(cfg, "label",       "color_background");
	_read(cfg, "placeholder", "color_background");
	_read(cfg, "placeholder", "border_width");
	_read(cfg, "placeholder", "foreground");
	_read(cfg, "window",      "corner_styles");

	_reset(cfg);

}
/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/


static void
_load(dr_config_t *cfg)
{
	char home[PATH_MAX] = "";
	du_string_t str;
	
	du_misc_get_home_path(home, PATH_MAX);
	du_string_init(&str, home);
	du_string_append(&str, "/A");

	dr_config_push_source(cfg, str.chars);

	du_string_reset(&str);
	dr_config_load(cfg);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_read(dr_config_t *cfg,  const char *namespace, const char *property)
{
	char value[32];

	if (dr_config_find_resource(cfg, namespace, property, value, 1, 32)) {
		printf(">> %s\t%s\t%s\n", namespace, property, value);
	} else {
		printf("couln't find the following resource : %s - %s\n", namespace, property);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_reset(dr_config_t *cfg)
{
	dr_config_destroy(cfg);
}
