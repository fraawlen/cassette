#############################################################################################################
# INSTALLATION DESTINATIONS #################################################################################
#############################################################################################################

FAMILY           := cassette
PREFIX           := /usr
DIR_INSTALL_INC  := $(PREFIX)/include/$(FAMILY)
DIR_INSTALL_LIB  := $(PREFIX)/lib
DIR_VIM_SYNTAX   := /usr/share/vim/vimfiles/syntax
DIR_VIM_FTDETECT := /usr/share/vim/vimfiles/ftdetect

#############################################################################################################
# SOURCE DIRS ###############################################################################################
#############################################################################################################

DIR_DEMOS := examples
DIR_SRC   := src
DIR_INC   := include
DIR_API   := docs/api

#############################################################################################################
# BUILD DIRS ################################################################################################
#############################################################################################################

DIR_BUILD := build
DIR_LIB   := $(DIR_BUILD)/lib
DIR_OBJ   := $(DIR_BUILD)/obj
DIR_BIN   := $(DIR_BUILD)/bin
DIR_DOC   := $(DIR_BUILD)/doc

#############################################################################################################
# PARAMS ####################################################################################################
#############################################################################################################

LDFLAGS := -shared
CFLAGS  := \
	-std=c23 \
	-O3 -D_POSIX_C_SOURCE=200809L \
	-pedantic \
	-Wall \
	-Wextra \
	-Wformat=2 \
	-Wbad-function-cast \
	-Wcast-align \
	-Wfloat-equal \
	-Wlogical-op \
	-Wmissing-declarations \
	-Wmissing-include-dirs \
	-Wmissing-prototypes \
	-Wnested-externs \
	-Wpointer-arith \
	-Wshadow \
	-Wwrite-strings \
	-Wstrict-prototypes \
	-Wundef \
	-Wunreachable-code \
	-Wno-attributes \
	-fanalyzer

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
export DIR_DOC
export DIR_API
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

install-syntax:
	mkdir -p $(DIR_VIM_SYNTAX)
	mkdir -p $(DIR_VIM_FTDETECT)
	cp ccfg/syntax/vim/syntax/*   $(DIR_VIM_SYNTAX)
	cp ccfg/syntax/vim/ftdetect/* $(DIR_VIM_FTDETECT)

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
