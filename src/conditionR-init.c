#include "internal.h"
#include <R_ext/Rdynload.h>

void R_init_conditionR(DllInfo *info) {
  static const R_CallMethodDef callMethods[]  = {
    {"getLocal0", (DL_FUNC) &getLocal0, 3},
    {NULL, NULL, 0}
  };

  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);

  R_RegisterCCallable("conditionR", "getLocal0", (DL_FUNC) &getLocal0);
}
