#############################################################################################################
# DESTINATIONS ##############################################################################################
#############################################################################################################

DEST_HEADERS := /usr/include/derelict
DEST_LIBS    := /usr/lib
DEST_BUILD   := build

#############################################################################################################
# INTERNAL VARIABLES ########################################################################################
#############################################################################################################

OUTPUT_NAME := du

DIR_DEMOS := demos
DIR_SRC   := src
DIR_INC   := $(DEST_BUILD)/inc
DIR_LIB   := $(DEST_BUILD)/lib
DIR_OBJ   := $(DEST_BUILD)/obj
DIR_BIN   := $(DEST_BUILD)/bin

LIST_DEMOS := $(wildcard $(DIR_DEMOS)/*.c)
LIST_SRC   := $(wildcard $(DIR_SRC)/*.c)
LIST_INC   := $(wildcard $(DIR_SRC)/*.h)
LIST_OBJ   := $(patsubst $(DIR_SRC)/%.c,   $(DIR_OBJ)/%.o, $(LIST_SRC))
LIST_BIN   := $(patsubst $(DIR_DEMOS)/%.c, $(DIR_BIN)/%,   $(LIST_DEMOS))

FLAGS := -std=c99 -pedantic -Wall -Wextra -O3 -D_POSIX_C_SOURCE=200809L
LIBS  :=

#############################################################################################################
# PUBLIC TARGETS ############################################################################################
#############################################################################################################

build: --prep $(LIST_OBJ)
	cc -shared $(DIR_OBJ)/*.o -o $(DIR_LIB)/lib$(OUTPUT_NAME).so $(DIR_LIBS)
	ar rcs $(DIR_LIB)/lib$(OUTPUT_NAME).a $(DIR_OBJ)/*.o

demos: --prep $(LIST_BIN)

install:
	mkdir -p $(DEST_HEADERS)
	cp $(DIR_INC)/* $(DEST_HEADERS)/
	cp $(DIR_LIB)/* $(DEST_LIBS)/

clean:
	rm -rf $(DEST_BUILD)

force: clean build demos

#############################################################################################################
# PRIVATE TARGETS ###########################################################################################
#############################################################################################################

--prep:
	mkdir -p $(DIR_INC)
	mkdir -p $(DIR_LIB)
	mkdir -p $(DIR_OBJ)
	mkdir -p $(DIR_BIN)
	cp src/*.h $(DIR_INC)

$(DIR_OBJ)/%.o: $(DIR_SRC)/%.c $(LIST_INC)
	$(CC) -c -fPIC $(FLAGS) -c $< -o $@

$(DIR_BIN)%: $(DIR_DEMOS)/%.c
	$(CC) -static $(FLAGS) $< -o $@ -I$(DIR_INC) -L$(DIR_LIB) -l$(OUTPUT_NAME) $(LIBS)
