#############################################################################################################
# FILE LISTS ################################################################################################
#############################################################################################################

SRC_DEMOS := $(wildcard $(DIR_DEMOS)/*.c)
SRC_LIB   := $(wildcard $(DIR_SRC)/*.c)
OBJ_LIB   := $(patsubst $(DIR_SRC)/%.c,   $(DIR_OBJ)/%.o, $(SRC_LIB))
BIN_DEMOS := $(patsubst $(DIR_DEMOS)/%.c, $(DIR_BIN)/%,   $(SRC_DEMOS))
RPATH     := -Wl,-rpath='$$ORIGIN'/../lib \
             -Wl,-rpath='$$ORIGIN'/../../../ccfg/build/lib \
             -Wl,-rpath='$$ORIGIN'/../../../cobj/build/lib

#############################################################################################################
# WAYLAND PROTOCOL FILES ####################################################################################
#############################################################################################################

WL_DIR := ext
WL_OUT := $(DIR_BUILD)/wl
WL_XML := $(wildcard $(WL_DIR)/*.xml)
WL_LIB := $(patsubst $(WL_DIR)/%.xml, $(WL_OUT)/%.o, $(WL_XML))

#############################################################################################################
# PARAMS ####################################################################################################
#############################################################################################################

NAME := cgui
INCS := -I../cobj/$(DIR_INC) -I../ccfg/$(DIR_INC) -I$(DIR_INC) -I$(WL_OUT)
DEPS := -L../cobj/$(DIR_LIB) -L../ccfg/$(DIR_LIB) -lcobj -lccfg -lm -lpthread -lcairo -lfontconfig \
        -lxcb -lxcb-keysyms -lxcb-present -lxcb-randr -lxcb-xinput -lxkbcommon -lxcb-render \
	  -lxcb-render-util -lwayland-client

#############################################################################################################
# PUBLIC TARGETS ############################################################################################
#############################################################################################################

build: dir lib doc $(BIN_DEMOS)

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

dir:
	mkdir -p $(DIR_LIB) $(DIR_OBJ) $(DIR_BIN) $(DIR_DOC) $(WL_OUT)

doc:
	asciidoctor -b manpage -D $(DIR_DOC) $(DIR_API)/*.adoc

lib: $(WL_LIB) $(OBJ_LIB)
	$(CC) $(LDFLAGS) -o $(DIR_LIB)/lib$(NAME).so $^ $(DEPS)

$(DIR_OBJ)/%.o: $(DIR_SRC)/%.c
	$(CC) $(CFLAGS) -fPIC -c $< -o $@ $(INCS)

$(DIR_BIN)%: $(DIR_DEMOS)/%.c
	$(CC) $(CFLAGS) $< -o $@ $(INCS) -L$(DIR_LIB) -l$(NAME) $(DEPS) $(RPATH)

$(WL_OUT)/%.o: $(WL_DIR)/%.xml
	wayland-scanner client-header $< $(patsubst $(WL_DIR)/%.xml, $(WL_OUT)/%.h, $<)
	wayland-scanner private-code  $< $(patsubst $(WL_DIR)/%.xml, $(WL_OUT)/%.c, $<)
	$(CC) $(CFLAGS) -fPIC -o $@ -c   $(patsubst $(WL_DIR)/%.xml, $(WL_OUT)/%.c, $<)
