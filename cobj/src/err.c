/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

#include <cassette/cobj.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>

/************************************************************************************************************/
/* PUBLIC ***************************************************************************************************/
/************************************************************************************************************/

void
cerr_clear_warnings(enum cerr *err)
{
	if (err && !cerr_critical(*err))
	{
		*err = CERR_NONE;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

bool
cerr_critical(enum cerr code)
{
	switch (code)
	{
		case CERR_NONE:
		case CERR_PARAM:
		case CERR_CALL:
			return false;

		default:
			return true;
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

const char *
cerr_name(enum cerr code)
{
	switch (code)
	{
		case CERR_NONE:
			return "CERR_NONE";

		case CERR_PARAM:
			return "CERR_PARAM";

		case CERR_CALL:
			return "CERR_CALL";

		case CERR_INVALID:
			return "CERR_INVALID";

		case CERR_OVERFLOW:
			return "CERR_OVERFLOW";

		case CERR_MEMORY:
			return "CERR_MEMORY";

		case CERR_THREAD:
			return "CERR_THREAD";

		case CERR_DISPLAY:
			return "CERR_DISPLAY";

		case CERR_CONFIG:
			return "CERR_CONFIG";

		case CERR_MENU:
			return "CERR_MENU";

		default:
			return "UNKNOWN";
	}
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

void
cerr_set(enum cerr *err, enum cerr code)
{
	if (err && (*err == CERR_NONE || (!cerr_critical(*err) && cerr_critical(code))))
	{
		*err = code;
	}
}
