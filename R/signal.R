#' Signal a Condition
#'
#' \code{signal} is a generic which chooses the appropriate base function for
#' signaling a condition depending on whether it is an error, a warning, a
#' message or neither of these. Furthermore, if \code{cond} is a call to
#' \code{stack*}, which returns a condition object, the call to \code{signal} is
#' ignored when constructing the \code{call} element of \code{cond}.
#'
#' @param cond a condition object.
#'
#' @details Errors are signaled offering an "ignoreError"-restart. If invoked,
#'   execution will continue after the call to \code{signal}.
#'
#' @return these functions are invoked for their side effects and return
#'   \code{NULL} invisibly.
#'
#' @export
signal <- function(cond) {
  env <- environment()  # do not use 'environment()' as arg to attr() directly
  attr(env, "conditionR_skip_frame") <- TRUE

  UseMethod("signal")  # this evaluates cond here
}

#' @export
signal.error <- function(cond) {
  withRestarts(
    stop(cond),
    ignoreError = function() NULL
  )
  invisible(NULL)
}

#' @export
signal.warning <- function(cond) {
  warning(cond)
  invisible(NULL)
}

#' @export
signal.message <- function(cond) {
  message(cond)
  invisible(NULL)
}

#' @export
signal.condition <- function(cond) {
  signalCondition(cond)
  invisible(NULL)
}

#' @rdname signal
#'
#' @param e the condition, i.e. error, object (ignored).
#'
#' @export
ignoreError <- function(e) {
  invokeRestart("ignoreError")
}