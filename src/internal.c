/*
 * Internal Low-Level C Functions
 *
 * The documentation of these functions is in R/internal_c.R.
 */

#include "internal.h"
#include <Rinternals.h>

SEXP getLocal0(SEXP varname, SEXP env, SEXP ifnotfound) {
  if (TYPEOF(varname) != STRSXP || length(varname) != 1) {
    error("'varname' is not a character string");
  }
  if (TYPEOF(env) != ENVSXP) {
    error("'env' is not an environment");
  }

  SEXP varsym = installChar(STRING_ELT(varname, 0));

  SEXP var;
  while (env != R_EmptyEnv) {
    // no need to protect here
    var = findVarInFrame3(env, varsym, TRUE);
    if (var != R_UnboundValue) {
      // force promise
      if (TYPEOF(var) == PROMSXP) {
        var = eval(var, env);
      }

      return var;
    }

    if (R_IsNamespaceEnv(env) || env == R_GlobalEnv) {
      break;
    }

    env = ENCLOS(env);
  }

  return ifnotfound;
}

SEXP getSEXPAddress(SEXP obj) {
   char addr[20];
   snprintf(addr, 20, "%p", (void*) obj);
   return mkString(addr);
}
