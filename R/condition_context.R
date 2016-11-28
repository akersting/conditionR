#' Conditional Stack-Based Errors and Conditions
#'
#' \code{set*Context} is used to set condition contexts along the function call
#' stack and \code{stack*} to construct a condition object herefrom.
#' \code{unset*Context} removes a previously set condition context.
#'
#' @param class a character vector of classes the condition object to be
#'   constructed should inherit form (from general to specific). "error",
#'   "warning" or "message" and "condition" are automatically added. This can
#'   also be an empty character vector for code{stack*} or if \code{base_class}
#'   is given.
#' @param message an error message as a character string. For \code{set*Context}
#'   this can also be an empty character vector or a character vector of error
#'   messages with at most one unnamed element. The names of the elements
#'   indicate which message to use conditional on the lower level condition
#'   classes: these classes are considered from specific to general and as soon
#'   as a matching element in \code{message} is found, it is used. If this
#'   search is unsuccessful, the unnamed element of \code{message} is used. If
#'   no such element is present, then no message from this context will be used.
#' @param base_class a character string or \code{NULL}. If given, it acts as a
#'   filter, i.e. only contexts of the same base class as the one specified when
#'   calling \code{stack*} are considered for the construction of the condition
#'   object, which will then also inherit from \code{base_class}.
#' @param call a logical value or \code{NULL}. Should the parent call be used as
#'   the call element of the condition object to construct? The default
#'   \code{NULL} is equivalent to using \code{TRUE} on the highest level and
#'   \code{FALSE} on all other levels. \code{TRUE} on a lower level overwrites
#'   \code{TRUE} on the higher levels.
#' @param type either "error" (equivalent to calling
#'   \code{setErrorContext}/\code{stackError}), "warning" (equivalent to calling
#'   \code{setWarningContext}/\code{stackWarning}), "message" (equivalent to
#'   calling \code{setMessageContext}/\code{stackMessage}) or "none". In the
#'   former cases, the condition object will inherit from "error", "warning" or
#'   "message" and "condition", respectively. In the latter case only from
#'   "condition".
#' @param ... further \emph{named} elements to add to the condition object. If
#'   elements with the same name are specified on multiple levels only the
#'   lowest level one will be included.
#'
#' @seealso \code{\link{signal}} for the preferred way to signal a condition
#'   object constructed by \code{stack*}.
#'
#' @return \code{set*Context} and \code{unset*Context} are invoked for their
#'   side effects, \code{stack*} returns a condition object.
#'
#' @name stackedConditions
NULL

.setConditionContext <- function(class, message, base_class, call, type,
                                 ellipsis) {
  env <- parent.frame(2)  # where to set the context

  if (!is.null(base_class) && missing(class)) {
    class <- character()
  } else if (!is.character(class) || length(class) == 0 || any(is.na(class))) {
    stop("'class' must be a (non-empty) character vector (without NAs).")
  }

  if (!is.character(message) || any(is.na(message))) {
    stop("'message' must be a character vector (without NAs).")
  }
  if (length(message) >= 2 && (
    is.null(names(message)) ||
    any(is.na(names(message))) ||
    sum(nchar(message) == 0) >= 2 ||
    length(unique(names(message))) != length(message))
  ) {
    stop("If 'message' has two or more elements, only one of them may be ",
         "unnamed. All others must have a unique name.")
  }

  if (!is.null(base_class) && (!is.character(base_class) ||
                               length(base_class) != 1 ||
                               is.na(base_class) ||
                               nchar(base_class) == 0)) {
    stop("'base_class' must be a character string (or NULL).")
  }
  if (!is.null(base_class) &&
      base_class %in% c("error", "warning", "message", "default")) {
    stop("'base_class' must be not be 'error', 'warning', 'message' or ",
         "'default'.")
  }
  if (is.null(base_class)) {
    base_class <- "default"
  }

  if (!is.null(call) && (!is.logical(call) || length(call) != 1 ||
                         is.na(call))) {
    stop("'call' must be a logical value (or NULL).")
  }
  if (identical(env, .GlobalEnv) && !is.null(call) && call) {
    warning("'call = TRUE' will be ignored if a context is set in the global ",
            "environment.")
  }

  if (!is.character(type) || length(type) != 1 || is.na(type) ||
      !type %in% c("error", "warning", "message", "none")) {
    stop("'type' must be one of 'error', 'warning', 'message' or 'none'.")
  }

  if (length(ellipsis) >= 1 && (is.null(names(ellipsis)) ||
                                any(is.na(names(ellipsis))) ||
                                any(names(ellipsis) == ""))) {
    stop("All additional arguments (...) must be named.")
  }

  # add to list of condition contexts ------------------------------------------
  if (is.null(attr(env, "conditionR_contexts", exact = TRUE))) {
    attr(env, "conditionR_contexts") <- list()
  }

  condition_context <- list(
    class = class,
    message = message,
    call = call,
    "..." = ellipsis
  )

  attr(env, "conditionR_contexts")[[type]][[base_class]] <-
    condition_context

  invisible(NULL)
}

#' @rdname stackedConditions
#'
#' @export
setConditionContext <- function(class, message = character(), base_class = NULL,
                                call = NULL, type = "none", ...) {
  .setConditionContext(class = class, message = message,
                       base_class = base_class, call = call,
                       type = type, ellipsis = list(...))
}

#' @rdname stackedConditions
#'
#' @export
setErrorContext <- function(class, message = character(), base_class = NULL,
                            call = NULL, ...) {
  .setConditionContext(class = class, message = message,
                       base_class = base_class, call = call,
                       type = "error", ellipsis = list(...))
}

#' @rdname stackedConditions
#'
#' @export
setWarningContext <- function(class, message = character(), base_class = NULL,
                              call = NULL, ...) {
  .setConditionContext(class = class, message = message,
                       base_class = base_class, call = call,
                       type = "warning", ellipsis = list(...))
}

#' @rdname stackedConditions
#'
#' @export
setMessageContext <- function(class, message = character(), base_class = NULL,
                              call = NULL, ...) {
  .setConditionContext(class = class, message = message,
                       base_class = base_class, call = call,
                       type = "message", ellipsis = list(...))
}

.unsetConditionContext <- function(base_class, type) {
  if (!is.null(base_class) && (!is.character(base_class) ||
                               length(base_class) != 1 ||
                               is.na(base_class) ||
                               nchar(base_class) == 0)) {
    stop("'base_class' must be a character string (or NULL).")
  }
  if (!is.null(base_class) &&
      base_class %in% c("error", "warning", "message", "default")) {
    stop("'base_class' must be not be 'error', 'warning', 'message' or ",
         "'default'.")
  }
  if (is.null(base_class)) {
    base_class <- "default"
  }

  if (!is.character(type) || length(type) != 1 || is.na(type) ||
      !type %in% c("error", "warning", "message", "none")) {
    stop("'type' must be one of 'error', 'warning', 'message' or 'none'.")
  }

  env <- parent.frame(2)
  attr(env, "conditionR_contexts")[[type]][[base_class]] <- NULL

  invisible()
}

#' @rdname stackedConditions
#'
#' @export
unsetConditionContext <- function(base_class = NULL, type = "none") {
  .unsetConditionContext(base_class, type = type)
}

#' @rdname stackedConditions
#'
#' @export
unsetErrorContext <- function(base_class = NULL) {
  .unsetConditionContext(base_class, type = "error")
}

#' @rdname stackedConditions
#'
#' @export
unsetWarningContext <- function(base_class = NULL) {
  .unsetConditionContext(base_class, type = "warning")
}

#' @rdname stackedConditions
#'
#' @export
unsetMessageContext <- function(base_class = NULL) {
  .unsetConditionContext(base_class, type = "message")
}