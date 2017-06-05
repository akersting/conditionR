#' @useDynLib conditionR

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
# @seealso \code{\link[base]{get0}}.
getLocal0 <- function(x, envir, ifnotfound = NULL) {
  .Call("getLocal0", varname = x, env = envir, ifnotfound = ifnotfound,
        PACKAGE = "conditionR")
}