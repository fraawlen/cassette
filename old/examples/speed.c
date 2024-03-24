#include <stdio.h>
#include <stdlib.h>

#include "du.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(int argc, char **argv)
{
	/* variables */

	du_dictionary_t dict;

	size_t n_dict = 1000;

	/* setup */

	n_dict = argc > 1 ? strtoul(argv[1], NULL, 0) : n_dict;

	du_dictionary_init(&dict, n_dict, 0.6);

	/* add values to dict */

	for (size_t i = 0; i < n_dict; i++) {
		du_dictionary_set_value(&dict, "ABCDEFGHIJKLMOPQ", 0, i);
		du_status_test(dict.status, printf("dictionary error, aborting\n"); return 0);
	}

	/* end */
	
	du_dictionary_reset(&dict);


	return 0;
}
