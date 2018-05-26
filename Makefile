UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
  GAMBIT_HOME=/usr/local
endif
ifeq ($(UNAME_S),Linux)
  GAMBIT_HOME=/usr/local/Gambit
endif

GSC=${GAMBIT_HOME}/bin/gsc
GSC_INC=${GAMBIT_HOME}/include
GSC_LIB=${GAMBIT_HOME}/lib

SCHEME_SOURCES = src/constants.scm src/uuid.scm \
		 src/lists.scm src/vectors.scm src/strings.scm \
                 src/Sort.scm src/sort-keys.scm src/filter-keys.scm \
		 src/functions.scm \
                 src/entries.scm src/rows.scm src/columns.scm \
                 src/tables.scm src/views.scm src/registry.scm \
                 src/csv.scm src/io-formats.scm src/io.scm  \
                 src/lecter.scm

PRODUCT = lecter

compile:
	${GSC} -f -o ${PRODUCT} -exe ${SCHEME_SOURCES}

clean:
	rm -f ${PRODUCT}
	rm -f src/*.o1
	rm -f src/*.o2
	rm -f *~
