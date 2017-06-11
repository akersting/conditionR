# Return a Local R Object
#
# Look for an object up to the first namespace or the global environment and
# return it. Otherwise, return a default value.
#
# @param x a variable name (given as a character string).
# @param envir the environment in which to start the search. Enclosing
#   environments are searched up to (and including) the first namespace or the
#   global environment.
# @param ifnotfound the object to return if \code{x} is not found.
#
# @details Compared to base \code{\link[base]{get0}}, it does not touch the
#   \code{named} field and it does not fail if the variable is the missing
#   argument / empty name.
#
# @seealso \code{\link[base]{get0}}.
getLocal0 <- function(x, envir = parent.frame(), ifnotfound = NULL) {
  .Call(C_getLocal0, varname = x, env = envir, ifnotfound = ifnotfound)
}