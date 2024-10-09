all:
	afl-gcc-fast -g3 fuzzer.c -o fuzzer -lcobj
	afl-fuzz -isamples -ooutput fuzzer
