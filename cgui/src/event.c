/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cassette/cobj.h>
#include <stdbool.h>
#include <stdckdint.h>
#include <stddef.h>
#include <stdlib.h>

#include "event.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#define GUARD(OBJ, ...) if (!OBJ || cerr_critical(OBJ->err)) { return __VA_OPT__(__VA_ARGS__); }

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct event_stack
{
	struct cevent *slots;
	size_t n;
	size_t n_alloc;
	enum cerr err;
};

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

event_stack *
event_stack_create(void)
{
	event_stack *evs;

	if (!(evs = malloc(sizeof(event_stack))))
	{
		return nullptr;
	}

	if (!(evs->slots = malloc(sizeof(struct cevent))))
	{
		free(evs);
		return nullptr;
	}

	evs->n       = 0;
	evs->n_alloc = 1;
	evs->err     = CERR_NONE;

	return evs;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

nullptr_t 
event_stack_destroy(event_stack *evs)
{
	if (evs)
	{
		free(evs->slots);
		free(evs);
	}

	return nullptr;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum cerr
event_stack_error(event_stack *evs)
{
	return evs ? evs->err : CERR_INVALID;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

size_t
event_stack_length(event_stack *evs)
{
	GUARD(evs, 0);

	return evs->n;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cevent
event_stack_pop(event_stack *evs)
{
	GUARD(evs, cevent_blank);

	return evs->n > 0 ? evs->slots[--evs->n] : cevent_blank;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

#include <stdio.h>
void
event_stack_push(event_stack *evs, struct cevent ev)
{
	GUARD(evs);
	
	size_t n;

	if (evs->n == evs->n_alloc)
	{
		if (ckd_mul(&n, evs->n_alloc, 2))
		{
			cerr_set(&evs->err, CERR_OVERFLOW);
			return;
		}
		if (!CUTIL_REALLOC(evs->slots, evs->n_alloc, n, sizeof(struct cevent), evs->err))
		{
			return;
		}
	}

	evs->slots[evs->n++] = ev;
}
