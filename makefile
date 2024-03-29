#############################################################################################################
# DESTINATIONS ##############################################################################################
#############################################################################################################

DEST_HEADERS = "/usr/include/dg"
DEST_LIBS    = "/usr/lib/"
DEST_BUILD   = "build"

#############################################################################################################
# INTERNAL VARIABLES ########################################################################################
#############################################################################################################

SRC_CORE = "modules/core"
SRC_BASE = "modules/base"
SRC_WM   = "modules/wm"
SRC_DEMO = "demos"

INC_CORE = -I${SRC_CORE}
INC_BASE = -I${DEST_BUILD}/include -I${SRC_BASE}
INC_WM   = -I${DEST_BUILD}/include -I${SRC_WM}
INC_DEMO = -I${DEST_BUILD}/include

OBJ_CORE = ${DEST_BUILD}/obj/core
OBJ_BASE = ${DEST_BUILD}/obj/base
OBJ_WM   = ${DEST_BUILD}/obj/wm

CFLAGS = \
	-std=c11                     \
	-ggdb3                       \
	-pedantic                    \
	-Wall                        \
	-O3                          \
	-Wno-deprecated-declarations \
	-D_POSIX_C_SOURCE=200809L

LIBS = \
	-lcairo       \
	-lfontconfig  \
	-lm           \
	-lpthread     \
	-lxcb         \
	-lxcb-keysyms \
	-lxcb-present \
	-lxcb-randr   \
	-lxcb-xinput  \
	-lxkbcommon

LIBS_DG = \
	-L${DEST_BUILD}/lib \
	-Wl,-rpath='$$ORIGIN'/../lib \
	-ldg \
	-ldg-base \
	-ldg-wm

#############################################################################################################
# PUBLIC TARGETS ############################################################################################
#############################################################################################################

build: --build_prep --build_core --build_base --build_wm

demos: --build_demos

install:
	mkdir -p ${DEST_HEADERS}
	mkdir -p ${DEST_LIBS}
	cp -r ${DEST_BUILD}/include/dg/* ${DEST_HEADERS}
	cp -r ${DEST_BUILD}/lib/*        ${DEST_LIBS}

clean:
	rm -rf ${DEST_BUILD}

#############################################################################################################
# PRIVATE TARGETS ###########################################################################################
#############################################################################################################

--build_prep:
	mkdir -p ${DEST_BUILD}/include/dg/core
	mkdir -p ${DEST_BUILD}/include/dg/base
	mkdir -p ${DEST_BUILD}/include/dg/wm
	mkdir -p ${DEST_BUILD}/obj/core
	mkdir -p ${DEST_BUILD}/obj/base
	mkdir -p ${DEST_BUILD}/obj/wm
	mkdir -p ${DEST_BUILD}/lib
	rsync -a --exclude "*-private*" --exclude "*.c" ${SRC_CORE}/* ${DEST_BUILD}/include/dg/core
	rsync -a --exclude "*-private*" --exclude "*.c" ${SRC_BASE}/* ${DEST_BUILD}/include/dg/base
	rsync -a --exclude "*-private*" --exclude "*.c" ${SRC_WM}/*   ${DEST_BUILD}/include/dg/wm

--build_core:
	cc -fPIC ${CFLAGS} ${INC_CORE} -c ${SRC_CORE}/core.c         -o ${OBJ_CORE}/core.o
	cc -fpic ${CFLAGS} ${INC_CORE} -c ${SRC_CORE}/config.c       -o ${OBJ_CORE}/config.o
	cc -fpic ${CFLAGS} ${INC_CORE} -c ${SRC_CORE}/errno.c        -o ${OBJ_CORE}/errno.o
	cc -fpic ${CFLAGS} ${INC_CORE} -c ${SRC_CORE}/resource.c     -o ${OBJ_CORE}/resource.o
	cc -fpic ${CFLAGS} ${INC_CORE} -c ${SRC_CORE}/util.c         -o ${OBJ_CORE}/util.o
	cc -fpic ${CFLAGS} ${INC_CORE} -c ${SRC_CORE}/stack.c        -o ${OBJ_CORE}/stack.o
	cc -fpic ${CFLAGS} ${INC_CORE} -c ${SRC_CORE}/input_buffer.c -o ${OBJ_CORE}/input_buffer.o
	cc -fpic ${CFLAGS} ${INC_CORE} -c ${SRC_CORE}/color.c        -o ${OBJ_CORE}/color.o
	cc -fpic ${CFLAGS} ${INC_CORE} -c ${SRC_CORE}/hashtable.c    -o ${OBJ_CORE}/hashtable.o
	cc -shared ${OBJ_CORE}/*.o -o ${DEST_BUILD}/lib/libdg.so ${LIBS}

--build_base:
	cc -fPIC ${CFLAGS} ${INC_BASE} -c ${SRC_BASE}/base.c              -o ${OBJ_BASE}/base.o
	cc -fPIC ${CFLAGS} ${INC_BASE} -c ${SRC_BASE}/config.c            -o ${OBJ_BASE}/config.o
	cc -fPIC ${CFLAGS} ${INC_BASE} -c ${SRC_BASE}/draw.c              -o ${OBJ_BASE}/draw.o
	cc -fPIC ${CFLAGS} ${INC_BASE} -c ${SRC_BASE}/origin.c            -o ${OBJ_BASE}/origin.o  
	cc -fPIC ${CFLAGS} ${INC_BASE} -c ${SRC_BASE}/rotation.c          -o ${OBJ_BASE}/rotation.o
	cc -fPIC ${CFLAGS} ${INC_BASE} -c ${SRC_BASE}/string.c            -o ${OBJ_BASE}/string.o
	cc -fPIC ${CFLAGS} ${INC_BASE} -c ${SRC_BASE}/zone.c              -o ${OBJ_BASE}/zone.o
	cc -fPIC ${CFLAGS} ${INC_BASE} -c ${SRC_BASE}/util.c              -o ${OBJ_BASE}/util.o
	cc -fPIC ${CFLAGS} ${INC_BASE} -c ${SRC_BASE}/cells/button.c      -o ${OBJ_BASE}/button.o
	cc -fPIC ${CFLAGS} ${INC_BASE} -c ${SRC_BASE}/cells/gap.c         -o ${OBJ_BASE}/gap.o
	cc -fPIC ${CFLAGS} ${INC_BASE} -c ${SRC_BASE}/cells/gauge.c       -o ${OBJ_BASE}/gauge.o
	cc -fPIC ${CFLAGS} ${INC_BASE} -c ${SRC_BASE}/cells/indicator.c   -o ${OBJ_BASE}/indicator.o
	cc -fPIC ${CFLAGS} ${INC_BASE} -c ${SRC_BASE}/cells/label.c       -o ${OBJ_BASE}/label.o
	cc -fPIC ${CFLAGS} ${INC_BASE} -c ${SRC_BASE}/cells/placeholder.c -o ${OBJ_BASE}/placeholder.o
	cc -fPIC ${CFLAGS} ${INC_BASE} -c ${SRC_BASE}/cells/spinner.c     -o ${OBJ_BASE}/spinner.o
	cc -fPIC ${CFLAGS} ${INC_BASE} -c ${SRC_BASE}/cells/switch.c      -o ${OBJ_BASE}/switch.o
	cc -shared ${OBJ_BASE}/*.o -o ${DEST_BUILD}/lib/libdg-base.so -L${DEST_BUILD}/lib ${LIBS} -ldg

--build_wm:
	cc -fPIC ${CFLAGS} ${INC_WM} -c ${SRC_WM}/wm.c -o ${OBJ_WM}/wm.o
	cc -shared ${OBJ_WM}/*.o -o ${DEST_BUILD}/lib/libdg-wm.so -L${DEST_BUILD}/lib ${LIBS} -ldg

--build_demos:
	mkdir -p ${DEST_BUILD}/bin
	cc -no-pie ${CFLAGS} ${INC_DEMO} ${SRC_DEMO}/bar.c        -o ${DEST_BUILD}/bin/bar        ${LIBS} ${LIBS_DG}
	cc -no-pie ${CFLAGS} ${INC_DEMO} ${SRC_DEMO}/dialog.c     -o ${DEST_BUILD}/bin/dialog     ${LIBS} ${LIBS_DG}
	cc -no-pie ${CFLAGS} ${INC_DEMO} ${SRC_DEMO}/game.c       -o ${DEST_BUILD}/bin/game       ${LIBS} ${LIBS_DG}
	cc -no-pie ${CFLAGS} ${INC_DEMO} ${SRC_DEMO}/hello.c      -o ${DEST_BUILD}/bin/hello      ${LIBS} ${LIBS_DG}
	cc -no-pie ${CFLAGS} ${INC_DEMO} ${SRC_DEMO}/layouts.c    -o ${DEST_BUILD}/bin/layouts    ${LIBS} ${LIBS_DG}
	cc -no-pie ${CFLAGS} ${INC_DEMO} ${SRC_DEMO}/navigation.c -o ${DEST_BUILD}/bin/navigation ${LIBS} ${LIBS_DG}
	cc -no-pie ${CFLAGS} ${INC_DEMO} ${SRC_DEMO}/reconfig.c   -o ${DEST_BUILD}/bin/reconfig   ${LIBS} ${LIBS_DG}
	cc -no-pie ${CFLAGS} ${INC_DEMO} ${SRC_DEMO}/showcase.c   -o ${DEST_BUILD}/bin/showcase   ${LIBS} ${LIBS_DG}
	cc -no-pie ${CFLAGS} ${INC_DEMO} ${SRC_DEMO}/windows.c    -o ${DEST_BUILD}/bin/windows    ${LIBS} ${LIBS_DG}
	cc -no-pie ${CFLAGS} ${INC_DEMO} ${SRC_DEMO}/wm.c         -o ${DEST_BUILD}/bin/wm         ${LIBS} ${LIBS_DG}
