#include <stdio.h>
#include <stdlib.h>

#include "du.h"

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(int argc, char **argv)
{
	du_dictionary_t dict;
	du_string_t str;

	size_t n_str  = 1000;
	size_t n_dict = 1000;

	/* setup */

	n_str  = argc > 1 ? strtoul(argv[1], NULL, 0) : n_str;
	n_dict = argc > 2 ? strtoul(argv[2], NULL, 0) : n_str;

	du_dictionary_init(&dict, n_dict, 0.8);

	/* add values to dict */

	for (size_t i = 0; i < n_str; i++) {
		str = du_string_from_double(i, 0);
		du_status_test(str.status, printf("string error, aborting\n"); return 0);
		du_dictionary_set_value(&dict, str.chars, 0, i);
		du_status_test(dict.status, printf("dictionary error, aborting\n"); return 0);
		du_string_reset(&str);
	}

//	du_dictionary_erase_value(&dict, "9", 0);
	du_dictionary_erase_value(&dict, "4", 0);
	du_dictionary_erase_value(&dict, "2", 0);
	du_dictionary_erase_value(&dict, "7", 0);
	du_dictionary_erase_value(&dict, "8", 0);
//	printf(">> %i / %i / %f\n\n", dict.n, dict.n_alloc, (double)dict.n / (double)dict.n_alloc);

	printf("\n");

	for (size_t i = 0; i < dict.n_alloc; i++) {
		printf("(%li)\t%i -> %li (%u)\n", i, dict.slots[i].usage, dict.slots[i].value, dict.slots[i].hash);
	}

	printf("\n");

	du_dictionary_set_value(&dict, "8", 0, 8);
	du_dictionary_set_value(&dict, "9", 1, 90);
	du_dictionary_set_value(&dict, "9", 0, 99);

	printf("\n");

	/* debug */

	for (size_t i = 0; i < dict.n_alloc; i++) {
		printf("(%li)\t%i -> %li\t(%u)\n", i, dict.slots[i].usage, dict.slots[i].value, dict.slots[i].hash);
	}

	/* end */
	
	du_dictionary_reset(&dict);

	return 0;
}
