#define ___VERSION 409003

#include "gambit.h"

#define SCHEME_LIBRARY_LINKER ___LNK_Delectus__

___BEGIN_C_LINKAGE
extern ___mod_or_lnk SCHEME_LIBRARY_LINKER (___global_state);
___END_C_LINKAGE

int initDelectus(void) {
  ___setup_params_struct setup_params;
	
  ___setup_params_reset (&setup_params);
	
  setup_params.version = ___VERSION;
  setup_params.linker  = SCHEME_LIBRARY_LINKER;
	
  ___setup (&setup_params);
  return 0;
}

int finalizeDelectus(void) {
  ___cleanup ();
  return 0;
}
