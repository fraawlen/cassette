#include <stdio.h>
#include <stdlib.h>

#include <derelict/du.h>

/************************************************************************************************************/
/************************************************************************************************************/
/************************************************************************************************************/

int
main(int argc, char **argv)
{
	/* variables */

	du_dictionary_t dict;
	du_string_t str;

	size_t n_str  = 1000;
	size_t n_dict = 1000;

	/* setup */

	n_str  = argc > 1 && strtol(argv[1], NULL, 0) >  0 ? strtol(argv[1], NULL, 0) : n_str;
	n_dict = argc > 2 && strtol(argv[2], NULL, 0) >= 0 ? strtol(argv[2], NULL, 0) : n_str;

	du_dictionary_init(&dict, n_dict, 0.6);

	printf("amount of strings to process = %li\n", n_str);
	printf("dictionary initial size      = %li\n", n_dict);

	/* add values to dict */

	for (size_t i = 0; i < n_str; i++) {
		str = du_string_from_double(i, 0);
		du_string_prepend(&str, "ooooooooooooooooooooooooooooooooovalue-");
		du_status_test(str.status, printf("string error, aborting\n"); return 0);
		du_dictionary_set_value(&dict, str.chars, 0, i);
		du_status_test(dict.status, printf("dictionary error, aborting\n"); return 0);
		du_string_reset(&str);
	}

	/* end */
	
	du_dictionary_reset(&dict);

	return 0;
}
