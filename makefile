#############################################################################################################
# DESTINATIONS ##############################################################################################
#############################################################################################################

DEST_HEADERS := /usr/include/cassette
DEST_LIBS    := /usr/lib
DEST_BUILD   := build

#############################################################################################################
# INTERNAL VARIABLES ########################################################################################
#############################################################################################################

DIR_DEMOS := examples
DIR_SRC   := src
DIR_INC   := include
DIR_TEST  := test
DIR_LIB   := $(DEST_BUILD)/lib
DIR_OBJ   := $(DEST_BUILD)/obj
DIR_BIN   := $(DEST_BUILD)/bin
DIR_FUZZ  := $(DEST_BUILD)/fuzzing

LIST_DEMOS := $(wildcard $(DIR_DEMOS)/*.c)
LIST_SRC   := $(wildcard $(DIR_SRC)/*.c)
LIST_HEAD  := $(wildcard $(DIR_SRC)/*.h) $(wildcard $(DIR_INC)/*.h)
LIST_OBJ   := $(patsubst $(DIR_SRC)/%.c, $(DIR_OBJ)/%.o, $(LIST_SRC))
LIST_BIN   := $(patsubst $(DIR_DEMOS)/%.c, $(DIR_BIN)/%, $(LIST_DEMOS))

OUTPUT := ccfg
LIBS   := -lcobj -lm -lpthread
FLAGS  := -std=c11 -O3 -D_POSIX_C_SOURCE=200809L -pedantic -pedantic-errors -Werror -Wall -Wextra          \
          -Wbad-function-cast -Wcast-align -Wcast-qual -Wdeclaration-after-statement -Wfloat-equal         \
          -Wformat=2 -Wlogical-op -Wmissing-declarations -Wmissing-include-dirs -Wmissing-prototypes       \
          -Wnested-externs -Wpointer-arith -Wredundant-decls -Wsequence-point -Wshadow -Wstrict-prototypes \
          -Wswitch -Wundef -Wunreachable-code -Wunused-but-set-parameter -Wwrite-strings

#############################################################################################################
# PUBLIC TARGETS ############################################################################################
#############################################################################################################

all: lib examples

lib: --prep $(LIST_OBJ)
	cc -shared $(DIR_OBJ)/*.o -o $(DIR_LIB)/lib$(OUTPUT).so $(DIR_LIBS)
	ar rcs $(DIR_LIB)/lib$(OUTPUT).a $(DIR_OBJ)/*.o

examples: lib $(LIST_BIN)

test: lib
	afl-gcc-fast -g3 $(DIR_TEST)/fuzz.c -o $(DIR_BIN)/fuzz -I$(DIR_INC) -I$(DIR_SRC) $(LIBS)
	afl-fuzz -i$(DIR_TEST)/samples -o$(DIR_FUZZ) $(DIR_BIN)/fuzz

install:
	mkdir -p $(DEST_HEADERS)
	cp $(DIR_INC)/*/* $(DEST_HEADERS)/
	cp $(DIR_LIB)/* $(DEST_LIBS)/

clean:
	rm -rf $(DEST_BUILD)

force: clean all

#############################################################################################################
# PRIVATE TARGETS ###########################################################################################
#############################################################################################################

--prep:
	mkdir -p $(DIR_LIB)
	mkdir -p $(DIR_OBJ)
	mkdir -p $(DIR_BIN)

$(DIR_OBJ)/%.o: $(DIR_SRC)/%.c $(LIST_HEAD)
	$(CC) -c -fPIC $(FLAGS) -c $< -o $@ -I$(DIR_INC) $(LIBS)

$(DIR_BIN)%: $(DIR_DEMOS)/%.c
	$(CC) -static $(FLAGS) $< -o $@ -I$(DIR_INC) -L$(DIR_LIB) -l$(OUTPUT) $(LIBS)
