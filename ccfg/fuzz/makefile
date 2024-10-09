all:
	afl-gcc-fast -g3 fuzz.c -o fuzz -lcobj
	afl-fuzz -isamples -ooutput fuzz
