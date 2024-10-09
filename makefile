#############################################################################################################
# INSTALLATION DESTINATIONS #################################################################################
#############################################################################################################

FAMILY          := cassette
PREFIX          := /usr
DIR_INSTALL_INC := $(PREFIX)/include/$(FAMILY)
DIR_INSTALL_LIB := $(PREFIX)/lib

#############################################################################################################
# SOURCE DIRS ###############################################################################################
#############################################################################################################

DIR_DEMOS := examples
DIR_SRC   := src
DIR_INC   := include

#############################################################################################################
# BUILD DIRS ################################################################################################
#############################################################################################################

DIR_BUILD := build
DIR_LIB   := $(DIR_BUILD)/lib
DIR_OBJ   := $(DIR_BUILD)/obj
DIR_BIN   := $(DIR_BUILD)/bin

#############################################################################################################
# PARAMS ####################################################################################################
#############################################################################################################

LDFLAGS := -shared
CFLAGS  := -std=c11 -O3 -D_POSIX_C_SOURCE=200809L -pedantic -pedantic-errors -Wall -Wextra -Wformat=2 \
           -Wbad-function-cast -Wcast-align -Wdeclaration-after-statement -Wfloat-equal \
           -Wlogical-op -Wmissing-declarations -Wmissing-include-dirs -Wmissing-prototypes -Wswitch \
           -Wnested-externs -Wpointer-arith -Wredundant-decls -Wsequence-point -Wshadow -Wwrite-strings \
           -Wstrict-prototypes -Wundef -Wunreachable-code -Wunused-but-set-parameter

#############################################################################################################
# EXPORTS ###################################################################################################
#############################################################################################################

export DIR_INSTALL_INC
export DIR_INSTALL_LIB
export DIR_BUILD
export DIR_DEMOS
export DIR_SRC
export DIR_INC
export DIR_LIB
export DIR_OBJ
export DIR_BIN
export LDFLAGS
export CFLAGS

#############################################################################################################
# PUBLIC TARGETS ############################################################################################
#############################################################################################################

build:
	$(MAKE) -C cobj build
	$(MAKE) -C ccfg build
	$(MAKE) -C cgui build
	
install:
	$(MAKE) -C cobj install
	$(MAKE) -C ccfg install
	$(MAKE) -C cgui install

clean:
	$(MAKE) -C cobj clean
	$(MAKE) -C ccfg clean
	$(MAKE) -C cgui clean

uninstall:
	-rm $(DIR_INSTALL_LIB)/libcobj.so
	-rm $(DIR_INSTALL_LIB)/libccfg.so
	-rm $(DIR_INSTALL_LIB)/libcgui.so
	-rm -r $(DIR_INSTALL_INC)

force: clean build
