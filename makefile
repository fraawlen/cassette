#############################################################################################################
# INSTALLATION DESTINATIONS #################################################################################
#############################################################################################################

FAMILY          := cassette
PREFIX          := /usr
DIR_INSTALL_INC := $(PREFIX)/include/$(FAMILY)
DIR_INSTALL_LIB := $(PREFIX)/lib

#############################################################################################################
# DIRS ######################################################################################################
#############################################################################################################

DIR_BUILD := build
DIR_DEMOS := examples
DIR_SRC   := src
DIR_INC   := include
DIR_LIB   := $(DIR_BUILD)/lib
DIR_OBJ   := $(DIR_BUILD)/obj
DIR_BIN   := $(DIR_BUILD)/bin

#############################################################################################################
# FILE LISTS ################################################################################################
#############################################################################################################

SRC_DEMOS := $(wildcard $(DIR_DEMOS)/*.c)
SRC_LIB   := $(wildcard $(DIR_SRC)/*.c)
OBJ_LIB   := $(patsubst $(DIR_SRC)/%.c,   $(DIR_OBJ)/%.o, $(SRC_LIB))
BIN_DEMOS := $(patsubst $(DIR_DEMOS)/%.c, $(DIR_BIN)/%,   $(SRC_DEMOS))

#############################################################################################################
# PARAMS ####################################################################################################
#############################################################################################################

NAME    := cgui
DEPS    := -lcobj -lccfg -lm -lpthread -lcairo -lfontconfig -lxcb -lxcb-keysyms -lxcb-present -lxcb-randr \
           -lxcb-xinput -lxkbcommon
LDFLAGS := -shared
CFLAGS  := -std=c11 -O3 -D_POSIX_C_SOURCE=200809L -pedantic -pedantic-errors -Wall -Wextra -Wformat=2 \
           -Wbad-function-cast -Wcast-align -Wcast-qual -Wdeclaration-after-statement -Wfloat-equal \
           -Wlogical-op -Wmissing-declarations -Wmissing-include-dirs -Wmissing-prototypes -Wswitch \
           -Wnested-externs -Wpointer-arith -Wredundant-decls -Wsequence-point -Wshadow -Wwrite-strings \
           -Wstrict-prototypes -Wundef -Wunreachable-code -Wunused-but-set-parameter

#############################################################################################################
# PUBLIC TARGETS ############################################################################################
#############################################################################################################

all: --dirs lib demos

force: clean all

lib: $(OBJ_LIB)
	$(CC) $(LDFLAGS) -o $(DIR_LIB)/lib$(NAME).so $^ $(DEPS)

demos: $(BIN_DEMOS)

install:
	install -d $(DIR_INSTALL_INC)
	install -d $(DIR_INSTALL_LIB)
	install $(DIR_INC)/*/* $(DIR_INSTALL_INC)
	install $(DIR_LIB)/*   $(DIR_INSTALL_LIB)

clean:
	rm -rf $(DIR_BUILD)

#############################################################################################################
# PRIVATE TARGETS ###########################################################################################
#############################################################################################################

--dirs:
	mkdir -p $(DIR_LIB) $(DIR_OBJ) $(DIR_BIN)

$(DIR_OBJ)/%.o: $(DIR_SRC)/%.c
	$(CC) $(CFLAGS) -fPIC -c $< -o $@ -I$(DIR_INC)

$(DIR_BIN)%: $(DIR_DEMOS)/%.c
	$(CC) $(CFLAGS) $< -o $@ -I$(DIR_INC) -L$(DIR_LIB) -l$(NAME) $(DEPS) -Wl,-rpath='$$ORIGIN'/../lib
