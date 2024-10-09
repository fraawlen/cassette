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

#include <assert.h>
#include <libgen.h>
#include <limits.h>
#include <pwd.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "color.h"
#include "errno.h"
#include "hashtable.h"
#include "resource.h"
#include "stack.h"
#include "util.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define _GROUP(I) ((const dg_core_resource_group_t*)_groups.ptr[I])

#define _HM_N_EXTRA     64
#define _FILE_MAX_DEPTH 64

#define _RUNE_COMMENT       '-'
#define _RUNE_INCLUDE       '@'
#define _RUNE_VAR           '$'
#define _RUNE_SECTION_ADD   '+'
#define _RUNE_SECTION_START '['
#define _RUNE_VAL_SPLIT     '~'

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

typedef enum {
	_HM_SECTION,
	_HM_VARIABLE,
	_HM_NAMESPACE,
	_HM_PROPERTY, /* always last because its used like "_HM_PROPERTY + i" to get a property on a specific */
} _hm_groups_t;   /* namespace                                                                            */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

typedef struct _parent_file_t _parent_file_t;
struct _parent_file_t {
	_parent_file_t *parent;
	ino_t inode;
	int depth;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static bool _get_source_file (char *buf);
static bool _load_resources  (const size_t *i_groups, size_t n_groups);
static void _preload_group   (const dg_core_resource_group_t *group, size_t index);

static void _parse_file      (const char *filename, _parent_file_t *parent);
static void _parse_resource  (char *str);
static void _parse_variable  (char *str);

static void _convert_bool    (bool *target, char *str);
static void _convert_color   (dg_core_color_t *target, char *str);
static void _convert_num     (void *target, char *str, dg_core_resource_kind_t kind);
static void _convert_str     (char *target, char *str);
static void _split_val       (char *str, char **str_left, char **str_right, double *factor);

static void   _save_str      (const char *key, const char *var, int group);
static char * _swap_str_val  (char *str);

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

/* persistent data */

static dg_core_stack_t _groups = {.ptr = NULL, .n = 0, .n_alloc = 0};
static void (*_fn_callback)(void) = NULL;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/* temp data */

static dg_core_hashtable_t _hm = {.slots = NULL, .n = 0, .n_alloc = 0};

static char  (*_vals)[DG_CORE_RESOURCE_STR_LEN] = NULL;
static size_t  _vals_n = 0;
static size_t  _vals_n_alloc = 0;

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

bool
dg_core_resource_load_all(void)
{
	return dg_core_resource_load_groups((const dg_core_resource_group_t* const*)_groups.ptr, _groups.n);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_core_resource_load_group(const dg_core_resource_group_t *group)
{
	assert(group);

	return dg_core_resource_load_groups(&group, 1);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_core_resource_load_groups(const dg_core_resource_group_t *const *groups, size_t n)
{
	assert(groups);

	if (n == 0) {
		return true;
	}

	bool success = false;

	/* prep index array */

	size_t *i_groups = malloc(n * sizeof(size_t));
	if (!i_groups) {
		dg_core_errno_set(DG_CORE_ERRNO_MEMORY);
		goto err;
	}

	for (size_t i = 0; i < n; i++) {
		i_groups[i] = i;
		if (!dg_core_stack_find(&_groups, groups[i], &i_groups[i])) {
			goto err;
		}
	}

	/* process resources, end & errors */

	success = _load_resources(i_groups, n);

err:

	free(i_groups);

	return success;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_resource_pull_group(const dg_core_resource_group_t *group)
{
	assert(group);

	dg_core_stack_pull(&_groups, group);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
dg_core_resource_push_group(const dg_core_resource_group_t *group)
{
	assert(group);
	if (group->kind == DG_CORE_RESOURCE_GROUP_CUSTOM) {
		assert(group->fn_parse);
	}

	return dg_core_stack_push(&_groups, group, NULL);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
dg_core_resource_set_callback(void (*fn)(void))
{
	_fn_callback = fn;
}

/************************************************************************************************************/
/* _ ********************************************************************************************************/
/************************************************************************************************************/

static void
_convert_bool(bool *target, char *str)
{
	if (!strcmp(str, "true")) {
		*target = true;
	} else if (!strcmp(str, "false")) {
		*target = false;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_convert_color(dg_core_color_t *target, char *str)
{
	char  *str_l  = NULL;
	char  *str_r  = NULL;
	double factor = 0.0;

	/* split values for conversion and interpolation */

	_split_val(str, &str_l, &str_r, &factor);
	if (!str_l) {
		return;
	}

	/* convert and interpolate colors */

	bool err_l = false;
	bool err_r = false;

	dg_core_color_t cl_l = dg_core_color_from_str(str_l, &err_l);
	dg_core_color_t cl_r = dg_core_color_from_str(str_r, &err_r);
	dg_core_color_t cl   = dg_core_color_interpolate(cl_l, cl_r, factor);

	if (!err_l && !err_r) {
		*target = cl;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_convert_num(void *target, char *str, dg_core_resource_kind_t kind)
{
	char *str_l   = NULL;
	char *str_r   = NULL;
	double factor = 0.0;

	char  *e_l = NULL;
	char  *e_r = NULL;
	long   l_l = 0;
	long   l_r = 0;
	double d_l = 0;
	double d_r = 0;

	bool fail = false;

	/* split values for interpolation */

	_split_val(str, &str_l, &str_r, &factor);
	if (!str_l) {
		return;
	}

	/* attempt conversion */

	switch (kind) {

		case DG_CORE_RESOURCE_INT:
		case DG_CORE_RESOURCE_UINT:
		case DG_CORE_RESOURCE_INT16:
		case DG_CORE_RESOURCE_UINT16:
			l_l = strtol(str_l, &e_l, 10);
			l_r = strtol(str_r, &e_r, 10);
			break;

		case DG_CORE_RESOURCE_DOUBLE:
		case DG_CORE_RESOURCE_UDOUBLE:
			d_l = strtod(str_l, &e_l);
			d_r = strtod(str_r, &e_r);
			break;

		default:
			return;
	}

	/* check for errors and bounds */

	switch (kind) {

		case DG_CORE_RESOURCE_INT:
			fail |= (*e_l != '\0' || l_l > INT_MAX || l_l < INT_MIN);
			fail |= (*e_r != '\0' || l_r > INT_MAX || l_r < INT_MIN);
			break;

		case DG_CORE_RESOURCE_UINT:
			fail |= (*e_l != '\0' || l_l > UINT_MAX || l_l < 0);
			fail |= (*e_r != '\0' || l_r > UINT_MAX || l_r < 0);
			break;

		case DG_CORE_RESOURCE_INT16:
			fail |= (*e_l != '\0' || l_l > INT16_MAX || l_l < INT16_MIN);
			fail |= (*e_r != '\0' || l_r > INT16_MAX || l_r < INT16_MIN);
			break;

		case DG_CORE_RESOURCE_UINT16:
			fail |= (*e_l != '\0' || l_l > INT16_MAX || l_l < 0);
			fail |= (*e_r != '\0' || l_r > INT16_MAX || l_r < 0);
			break;

		case DG_CORE_RESOURCE_DOUBLE:
			fail |= (d_l == 0.0 && e_l == str_l);
			fail |= (d_r == 0.0 && e_r == str_r);
			break;

		case DG_CORE_RESOURCE_UDOUBLE:
			fail |= ((d_l == 0.0 && e_l == str_l) || d_l < 0.0);
			fail |= ((d_r == 0.0 && e_r == str_r) || d_r < 0.0);
			break;

		default:
			return;
	}

	if (fail) {
		return;
	}

	/* apply converted value */

	long   l = (double)l_r * factor + (double)l_l * (1.0 - factor);
	double d = d_r * factor + d_l * (1 - factor);

	switch (kind) {

		case DG_CORE_RESOURCE_INT:
			*(int*)target = l;
			break;

		case DG_CORE_RESOURCE_UINT:
			*(unsigned int*)target = l;
			break;

		case DG_CORE_RESOURCE_INT16:
		case DG_CORE_RESOURCE_UINT16:
			*(int16_t*)target = l;
			break;

		case DG_CORE_RESOURCE_DOUBLE:
		case DG_CORE_RESOURCE_UDOUBLE:
			*(double*)target = d;
			break;

		default:
			return;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_convert_str(char *target, char *str)
{
	char *c;

	if (str[0] == '"') {
		c = strchr(++str, '"');
		if (c) {
			*c = '\0';
		}
	}

	strncpy(target, str, DG_CORE_RESOURCE_STR_LEN);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_get_source_file(char *buf)
{
	/* get conf filename in homedir */

	const char *home_dir = dg_core_util_test_env("HOME") ? getenv("HOME") : getpwuid(getuid())->pw_dir;
	char *home_conf = malloc(strlen(home_dir) + 17);
	if (home_conf) {
		strcpy(home_conf, home_dir);
		strcat(home_conf, "/.config/dg.conf");
	}

	/* possible filenames */

	const char *f_s[] = {
		dg_core_util_test_env("DG_CORE_RESOURCE_FILE") ? getenv("DG_CORE_RESOURCE_FILE") : NULL,
		home_conf,
		"/usr/share/dg/dg.conf",
		"/etc/dg.conf",
	};

	/* check all filenames */

	FILE *f = NULL;

	for (size_t i = 0; i < sizeof(f_s) / sizeof(char*); i++) {
		if (f_s[i] && (f = fopen(f_s[i], "r"))) {
			strncpy(buf, f_s[i], PATH_MAX);
			fclose(f);
			free(home_conf);
			return true;
		}
	}

	/* end */

	free(home_conf);

	return false;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static bool
_load_resources(const size_t *i_groups, size_t n_groups)
{
	bool success = false;

	/* init data structures */

	size_t n = _HM_N_EXTRA + n_groups;

	for (size_t i = 0; i < n_groups; i++) {
		n += _GROUP(i_groups[i])->n;
	}

	if (!dg_core_hashtable_init(&_hm, n)) {
		goto err;
	}

	_vals_n = 0;
	_vals_n_alloc = n;
	_vals = malloc(_HM_N_EXTRA * DG_CORE_RESOURCE_STR_LEN);
	if (!_vals) {
		goto err;
	}

	/* pre-fill hashmap with references to group's resources and apply their defaults */

	for (size_t i = 0; i < n_groups; i++) {
		_preload_group(_GROUP(i_groups[i]), i_groups[i]);
		if (_GROUP(i_groups[i])->fn_preprocess && !_GROUP(i_groups[i])->fn_preprocess()) {
			goto err;
		}
	}

	/* locate, select and parse resources source file */

	char path[PATH_MAX + 1];

	if (!dg_core_util_test_env("DG_CORE_RESOURCE_USE_BUILTIN") && _get_source_file(path)) {
		_parse_file(path, NULL);
	}

	/* activate post processing functions if any */

	for (size_t i = 0; i < n_groups; i++) {
		if (_GROUP(i_groups[i])->fn_postprocess) {
			_GROUP(i_groups[i])->fn_postprocess();
		}
	}

	/* end & errors */

	success = true;

err:

	free(_vals);
	dg_core_hashtable_reset(&_hm);

	if (_fn_callback) {
		_fn_callback();
	}

	return success;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_parse_file(const char *filename, _parent_file_t *parent)
{
	FILE *f;
	char dir[PATH_MAX + 1];
	char child[PATH_MAX * 2 + 1];
	_parent_file_t f_ref;

	/* prep file and paths */
	
	struct stat fs;

	if (parent && parent->depth >= _FILE_MAX_DEPTH) {
		return;
	}
	
	if (!(f = fopen(filename, "r"))) {
		return;
	}

	if (fstat(fileno(f), &fs) < 0) {
		goto end;
	} 

	f_ref.inode  = fs.st_ino;
	f_ref.parent = parent;
	f_ref.depth  = parent ? parent->depth + 1 : 0;

	strncpy(dir, filename, PATH_MAX);
	dirname(dir);

	/* check that the file to be parsed was not already opened to avoid circular dependencies */

	while (parent) {
		if (f_ref.inode == parent->inode) {
			goto end;
		} else {
			parent = parent->parent;
		}
	}

	/* read file */
	
	char *str;
	char c;
	char l[256];
	bool found;
	bool skipping = false;

	while (fgets(l, 256, f)) {

		str = dg_core_util_trim_str(l);

		/* empty line or comment */

		if (!str) {
			continue;
		}
		
		c = str[0];

		/* grouped action - check if the current section was added or not (and skip rows as needed) */
		/*                  or if the line is a comment                                             */

		if (c == _RUNE_COMMENT || (c != _RUNE_SECTION_START && skipping)) {
			continue;
		}

		/* grouped action - check if its a special definition that its not empty */

		if (c == _RUNE_SECTION_ADD || c == _RUNE_INCLUDE || c == _RUNE_VAR) {
			if (strlen(str) < 2) {
				continue;
			} else {
				str = dg_core_util_trim_str(str + 1);
			}
		}

		/* individual actions */

		switch (c) {

			/* start of section */
			case _RUNE_SECTION_START:
				if (strlen(str) < 2) {
					skipping = false;
				} else {
					str = dg_core_util_trim_str(str + 1);
					dg_core_hashtable_get_value(&_hm, str, _HM_SECTION, &found);
					skipping = !found;
				}
				break;

			/* section addition */
			case _RUNE_SECTION_ADD: 
				dg_core_hashtable_set_value(&_hm, str, _HM_SECTION, 0);
				break;

			/* child resource file */
			case _RUNE_INCLUDE: 
				if (str[0] == '/') {
					strncpy(child, str, PATH_MAX);
				} else {
					snprintf(child, PATH_MAX * 2 + 1, "%s/%s", dir, str);
				}
				_parse_file(child, &f_ref);
				break;

			/* variable */
			case _RUNE_VAR: 
				_parse_variable(str);
				break;

			/* resource */
			default: 
				_parse_resource(str);
				break;
		}
	}
	
	/* end */

end:
	fclose(f);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_parse_resource(char *str)
{
	bool found;

	/* split resource namespace, name and value */

	char *s_ctx   = NULL;
	char *s_group = dg_core_util_trim_str(strtok_r(str,  ".", &s_ctx));
	char *s_prop  = dg_core_util_trim_str(strtok_r(NULL, "=", &s_ctx));
	char *s_val   = dg_core_util_trim_str(strtok_r(NULL, "=", &s_ctx));
	if (!s_val || !s_prop || !s_group) {
		return;
	}

	/* swap value if its a variable name                                               */
	/* make it a copy to avoid messing with the saved values if the swap was effective */

	char *s_tmp = NULL;
	char *s_var = NULL;

	s_var = _swap_str_val(s_val);
	if (!s_var) {
		return;
	}

	s_tmp = strdup(s_var);
	if (!s_tmp) {
		return;
	}

	s_val = s_tmp;

	/* match the namespace */

	const size_t i_group = dg_core_hashtable_get_value(&_hm, s_group, _HM_NAMESPACE, &found);

	if (!found) {
		free(s_val);
		return;
	}

	/* if the group has a custom parser use that instead of continuing with the internal one */

	if (_GROUP(i_group)->kind == DG_CORE_RESOURCE_GROUP_CUSTOM) {
		_GROUP(i_group)->fn_parse(s_prop, s_val);
		free(s_val);
		return;
	}

	/* match the resource's property name */

	const size_t i_prop = dg_core_hashtable_get_value(&_hm, s_prop, _HM_PROPERTY + i_group, &found);

	if (!found) {
		free(s_val);
		return;
	}

	/* convert and apply the string value to the property */
	
	const dg_core_resource_t *res = &_GROUP(i_group)->resources[i_prop];

	if (!res->target) {
		free(s_val);
		return;
	}

	switch (res->kind) {
		
		case DG_CORE_RESOURCE_STR:
			_convert_str((char*)res->target, s_val);
			break;

		case DG_CORE_RESOURCE_INT:
		case DG_CORE_RESOURCE_UINT:
		case DG_CORE_RESOURCE_INT16:
		case DG_CORE_RESOURCE_UINT16:
		case DG_CORE_RESOURCE_DOUBLE:
		case DG_CORE_RESOURCE_UDOUBLE:
			_convert_num(res->target, s_val, res->kind);
			break;

		case DG_CORE_RESOURCE_COLOR:
			_convert_color((dg_core_color_t*)res->target, s_val);
			break;

		case DG_CORE_RESOURCE_BOOL:
			_convert_bool((bool*)res->target, s_val);
			break;
	}
	
	free(s_val);
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_parse_variable(char *str)
{
	/* split variable name and value */

	char *s_ctx  = NULL;
	char *s_name = dg_core_util_trim_str(strtok_r(str,  "=", &s_ctx));
	char *s_val  = dg_core_util_trim_str(strtok_r(NULL, "=", &s_ctx));
	if (!s_val || !s_name) {
		return;
	}

	/* swap value if its a variable name */

	s_val = _swap_str_val(s_val);
	if (!s_val) {
		return;
	}

	/* check if variable has been defined before, if it was, just update its value, */
	/* otherwhise add the new string to the hashmap and dynamic values array        */

	bool found;

	const size_t i = dg_core_hashtable_get_value(&_hm, s_name, _HM_VARIABLE, &found);

	if (found) {
		strncpy(_vals[i], s_val, DG_CORE_RESOURCE_STR_LEN);
	} else {
		_save_str(s_name, s_val, _HM_VARIABLE);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_preload_group(const dg_core_resource_group_t *group, size_t index)
{
	dg_core_hashtable_set_value(&_hm, group->namespace, _HM_NAMESPACE, index);

	if (group->kind == DG_CORE_RESOURCE_GROUP_CUSTOM) {
		return;
	}

	for (size_t i = 0; i < group->n; i++) {
		dg_core_hashtable_set_value(&_hm, group->resources[i].name, _HM_PROPERTY + index, i);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_save_str(const char *key, const char *val, int group)
{
	/* if the dynamic value array is not big enough, increase its size */

	if (_vals_n < _vals_n_alloc) {
		goto skip_resize;
	}

	const size_t n_alloc = _vals_n_alloc * 2;
	
	char (*tmp)[DG_CORE_RESOURCE_STR_LEN] = realloc(_vals, n_alloc * DG_CORE_RESOURCE_STR_LEN);
	if (!tmp) {
		dg_core_errno_set(DG_CORE_ERRNO_MEMORY);
		return;
	} 

	_vals = tmp;
	_vals_n_alloc = n_alloc;

skip_resize:

	/* add a reference to the new string in the dynamic values hashmap then copy it in the related array */

	if (dg_core_hashtable_set_value(&_hm, key, group, _vals_n)) {
		strncpy(_vals[_vals_n], val, DG_CORE_RESOURCE_STR_LEN);
		_vals_n++;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static void
_split_val(char *str, char **str_left, char **str_right, double *factor)
{
	/* check for rune */

	if (str[0] != _RUNE_VAL_SPLIT) {
		*str_left  = str;
		*str_right = str;
		*factor    = 0.0;
		return;
	}

	str++;

	/* split */

	char *e        = NULL;
	char *s_ctx    = NULL;
	char *s_left   = _swap_str_val(dg_core_util_trim_str(strtok_r(str,  ":", &s_ctx)));
	char *s_right  = _swap_str_val(dg_core_util_trim_str(strtok_r(NULL, ":", &s_ctx)));
	char *s_factor = _swap_str_val(dg_core_util_trim_str(strtok_r(NULL, ":", &s_ctx)));

	*str_left  = s_left;
	*str_right = s_right  ? s_right : s_left;
	*factor    = s_factor ? strtod(s_factor, &e) : 0.0;

	if ((*factor == 0.0 && e == s_factor) || *factor < 0.0) {
		*factor = 0.0;
	} else if (*factor > 1.0) {
		*factor = 1.0;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

static char *
_swap_str_val(char *str)
{
	if (!str || str[0] != _RUNE_VAR) {
		return str;
	}

	bool found;

	const size_t i = dg_core_hashtable_get_value(&_hm, dg_core_util_trim_str(str + 1), _HM_VARIABLE, &found);

	return found ? _vals[i] : NULL;
}
