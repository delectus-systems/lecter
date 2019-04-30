UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
  GAMBIT_HOME=/usr/local
endif
ifeq ($(UNAME_S),Linux)
  GAMBIT_HOME=/usr/local/Gambit
endif

GCC = gcc

GSC=${GAMBIT_HOME}/bin/gsc
GSC_BIN=${GAMBIT_HOME}/bin
GSC_INC=${GAMBIT_HOME}/include
GSC_LIB=${GAMBIT_HOME}/lib

SCHEME_SOURCES = src/constants.scm src/uuid.scm src/lists.scm		\
		 src/vectors.scm src/strings.scm src/Sort.scm		\
		 src/sort-keys.scm src/filter-keys.scm			\
		 src/functions.scm src/entries.scm src/rows.scm		\
		 src/columns.scm src/tables.scm src/views.scm		\
		 src/registry.scm src/csv.scm src/io-formats.scm	\
		 src/io.scm 

C_SOURCES = src/constants.c src/uuid.c src/lists.c src/vectors.c	\
	    src/strings.c src/Sort.c src/sort-keys.c			\
	    src/filter-keys.c src/functions.c src/entries.c		\
	    src/rows.c src/columns.c src/tables.c src/views.c		\
	    src/registry.c src/csv.c src/io-formats.c src/io.c		\
	    src/io_.c

OBJS = src/constants.o src/uuid.o src/lists.o src/vectors.o		\
       src/strings.o src/Sort.o src/sort-keys.o src/filter-keys.o	\
       src/functions.o src/entries.o src/rows.o src/columns.o		\
       src/tables.o src/views.o src/registry.o src/csv.o		\
       src/io-formats.o src/io.o src/io_.o


OBJ_FLAGS = -I${GSC_INC} -L${GSC_LIB} -Wno-unused -O1 -fno-math-errno -fno-trapping-math -fno-strict-aliasing -fwrapv -fomit-frame-pointer -fPIC -fno-common -mieee-fp 

EXE = lecter

LIB = libDelectus.a

lecter:
	${GSC} -f -o ${EXE} -exe ${SCHEME_SOURCES} src/lecter.scm

lib: static_obj
	ar rc ${LIB} ${LIB} && ranlib ${LIB}
	rm -f ${C_SOURCES}
	rm -f ${OBJECTS}

static_obj: compile_scheme
	${GCC} ${OBJ_FLAGS} -c ${C_SOURCES} ${GSC_LIB}/libgambit.a -D___LIBRARY

compile_scheme:
	${GSC} -link ${SCHEME_SOURCES}

clean:
	rm -f ${C_SOURCES}
	rm -f ${OBJS}
	rm -f src/*.o1
	rm -f src/*.o2
	rm -f *.o
	rm -f *.o1
	rm -f *.o2
	rm -f *~
