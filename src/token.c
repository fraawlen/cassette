/**
 * Copyright © 2024 Fraawlen <fraawlen@posteo.net>
 *
 * This file is part of the Cassette Configuration (CCFG) library.
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

#include <cassette/cobj.h>
#include <stdlib.h>

#include "token.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

struct slot
{
	const char *key;
	enum token type;
};

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

static const struct slot map[] =
{
	/* substitution tokens */

	{ "EOF",           TOKEN_EOF              },
	{ "",              TOKEN_INVALID          },
	{ "//",            TOKEN_COMMENT          },
	{ "EOS",           TOKEN_COMMENT          },
	{ "=",             TOKEN_FILLER           },
	{ ":=",            TOKEN_FILLER           },
	{ "JOIN",          TOKEN_JOIN             },
	{ "\\",            TOKEN_ESCAPE           },
	{ "$",             TOKEN_VAR_INJECTION    },
	{ "%",             TOKEN_ITER_INJECTION   },
	{ "$$",            TOKEN_PARAM_INJECTION  },
	{ "VAR",           TOKEN_VAR_INJECTION    },
	{ "ITER",          TOKEN_ITER_INJECTION   },
	{ "PARAM",         TOKEN_PARAM_INJECTION  },

	{ "<",             TOKEN_IF_LESS          },
	{ "<=",            TOKEN_IF_LESS_EQ       },
	{ ">",             TOKEN_IF_MORE          },
	{ ">=",            TOKEN_IF_MORE_EQ       },
	{ "==",            TOKEN_IF_EQ            },
	{ "!=",            TOKEN_IF_EQ_NOT        },
	{ "STREQ",         TOKEN_IF_STR_EQ        },

	{ "TIME",          TOKEN_TIMESTAMP        },

	{ "PI",            TOKEN_CONST_PI         },
	{ "E",             TOKEN_CONST_EULER      },
	{ "TRUE",          TOKEN_CONST_TRUE       },
	{ "FALSE",         TOKEN_CONST_FALSE      },

	{ "SQRT",          TOKEN_OP_SQRT          },
	{ "CBRT",          TOKEN_OP_CBRT          },
	{ "ABS",           TOKEN_OP_ABS           },
	{ "CEIL",          TOKEN_OP_CEILING       },
	{ "FLOOR",         TOKEN_OP_FLOOR         },
	{ "ROUND",         TOKEN_OP_ROUND         },
	{ "COS",           TOKEN_OP_COS           },
	{ "SIN",           TOKEN_OP_SIN           },
	{ "TAN",           TOKEN_OP_TAN           },
	{ "ACOS",          TOKEN_OP_ACOS          },
	{ "ASIN",          TOKEN_OP_ASIN          },
	{ "ATAN",          TOKEN_OP_ATAN          },
	{ "COSH",          TOKEN_OP_COSH          },
	{ "SINH",          TOKEN_OP_SINH          },
	{ "LN",            TOKEN_OP_LN            },
	{ "LOG",           TOKEN_OP_LOG           },
	{ "+",             TOKEN_OP_ADD           },
	{ "-",             TOKEN_OP_SUBSTRACT     },
	{ "*",             TOKEN_OP_MULTIPLY      },
	{ "/",             TOKEN_OP_DIVIDE        },
	{ "MOD",           TOKEN_OP_MOD           },
	{ "POW",           TOKEN_OP_POW           },
	{ "BIG",           TOKEN_OP_BIGGEST       },
	{ "SMALL",         TOKEN_OP_SMALLEST      },
	{ "RAND",          TOKEN_OP_RANDOM        },
	{ "ITRPL",         TOKEN_OP_INTERPOLATE   },
	{ "LIMIT",         TOKEN_OP_LIMIT         },

	{ "CITRPL",        TOKEN_CL_INTERPOLATE   },
	{ "RGB",           TOKEN_CL_RGB           },
	{ "RGBA",          TOKEN_CL_RGBA          },

	/* lead tokens */

	{ "LET",           TOKEN_VAR_DECLARATION  },
	{ "LET_APPEND",    TOKEN_VAR_APPEND       },
	{ "LET_PREPEND",   TOKEN_VAR_PREPEND      },
	{ "LET_MERGE",     TOKEN_VAR_MERGE        },
	{ "LET_ENUM",      TOKEN_ENUM_DECLARATION },
	{ "SECTION",       TOKEN_SECTION_BEGIN    },
	{ "SECTION_ADD",   TOKEN_SECTION_ADD      },
	{ "SECTION_DEL",   TOKEN_SECTION_DEL      },
	{ "INCLUDE",       TOKEN_INCLUDE          },
	{ "SEED",          TOKEN_SEED             },
	{ "DEBUG_PRINT",   TOKEN_PRINT            },
	{ "FOR_EACH",      TOKEN_FOR_BEGIN        },
	{ "FOR_END",       TOKEN_FOR_END          },
};

/************************************************************************************************************/
/* PRIVATE **************************************************************************************************/
/************************************************************************************************************/

cdict *
token_dict_create(void)
{
	cdict *dict;

	dict = cdict_create();

	cdict_prealloc(dict,   sizeof(map) / sizeof(struct slot));
	for (size_t i = 0; i < sizeof(map) / sizeof(struct slot); i++)
	{
		cdict_write(dict, map[i].key, 0, map[i].type);
	}

	return dict;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

enum token
token_match(cdict *token_dict, const char *token)
{
	size_t id;

	if (cdict_find(token_dict, token, 0, &id))
	{
		return id;
	}

	return TOKEN_STRING;
}
