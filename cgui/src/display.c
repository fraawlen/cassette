/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cassette/cgui.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#include "display.h"
#include "x11.h"

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

enum cdisplay_server
cdisplay_server(const cdisplay *dp)
{
	return dp ? dp->server : CDISPLAY_NONE;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

struct cx11
cdisplay_x11(const cdisplay *dp)
{
	return dp && dp->server == CDISPLAY_X11 ? dp->x : display_none.x;
}

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

struct cevent
display_event(cdisplay *dp)
{
	switch (dp->server)
	{
		case CDISPLAY_X11:
			return x11_event(&dp->x);

		default:
			return cevent_blank;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
display_init(cdisplay *dp, enum cdisplay_server server)
{
	if (server & CDISPLAY_X11 && x11_init(&dp->x, &dp->fd))
	{
		dp->server = CDISPLAY_X11;
	}
	else
	{
		return false;
	}
	
	return true;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
display_kill(cdisplay *dp)
{
	switch (dp->server)
	{
		case CDISPLAY_X11:
			x11_kill(&dp->x);
			break;

		default:
			break;
	}

	dp->server = CDISPLAY_NONE;
	dp->fd     = -1;
}
