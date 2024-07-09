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

#include <errno.h>
#include <pthread.h>
#include <stdbool.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static pthread_mutex_t _mutex;
static bool _init = false;

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

bool
mutex_init(void)
{
	pthread_mutexattr_t mut_attr;

	if (pthread_mutexattr_init(&mut_attr) != 0
	 || pthread_mutexattr_settype(&mut_attr, PTHREAD_MUTEX_ERRORCHECK) != 0)
	{
		return false;
	}
	
	_init = pthread_mutex_init(&_mutex, &mut_attr) == 0;

	pthread_mutexattr_destroy(&mut_attr);

	return _init;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
mutex_lock(void)
{
	if (!_init)
	{
		return false;
	}

	return pthread_mutex_lock(&_mutex) != EDEADLK;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
mutex_reset(void)
{
	if (_init)
	{
		pthread_mutex_destroy(&_mutex);
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
mutex_unlock(void)
{
	if (!_init)
	{
		return false;
	}

	return pthread_mutex_unlock(&_mutex) != EPERM;
}
