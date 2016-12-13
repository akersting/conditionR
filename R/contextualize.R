#' Contextualize (Simple) Conditions
#'
#' \code{contextualize} catches (simple) conditions and instead signals a
#' stacked condition.
#'
#' @param expr an expression which potentially signals a condition.
#' @param ... (named) lists as named arguments. The elements of one of these
#'   lists are passed as arguments to \code{stackCondition}, i.e. an element
#'   "message" must be present if \code{include_original_message == FALSE}. If
#'   missing, "type" is automatically derived from the type of the caught
#'   condition. "call" is ignored. The names of these arguments refer to the
#'   class of the condition object.
#' @param include_original_message a logical value. Should the message of the
#'   caught condition be appended to the message specified in the chosen
#'   \code{...} list?
#'
#' @return \code{NULL} (invisibly) if a condition was caught, otherwise whatever
#'   \code{expr} returns.
#'
#' @export
contextualize <- function(expr, ..., include_original_message = TRUE) {
  contexts <- list(...)

  handlers <- list()
  for (handler in names(contexts)) {
    handlers[[handler]] <- getHandlerFun(contexts[[handler]],
                                         include_original_message)
  }

  res <- do.call(tryCatch, c(quote(expr), handlers))

  if (isTRUE(attr(res, ".conditionR_contextualized_condition"))) {
    attr(res, ".conditionR_contextualized_condition") <- NULL
    signal(res)
    invisible(NULL)
  } else {
    res
  }
}

getHandlerFun <- function(context, include_original_message) {
  # forces promises
  context
  include_original_message

  function(cond) {
    if (include_original_message) {
      context[["message"]] <- paste(context[["message"]],
                                    conditionMessage(cond), sep = " | ")
    }

    if (is.null(context[["type"]])) {
      context[["type"]] <-
        ifelse(inherits(cond, "error"), "error",
               ifelse(inherits(cond, "warning"), "warning",
                      ifelse(inherits(cond, "message"), "message",
                             "none")))
    }

    cond_new <- do.call(stackCondition, context)
    if (is.null(cond_new[["call"]])) {
      cond_new[["call"]] <- cond[["call"]]
    }

    # make this condition object distinguishable from a condition object being
    # returned by expr (which we must not signal)
    attr(cond_new, ".conditionR_contextualized_condition") <- TRUE

    # we must not signal cond_new here already since handlers which were
    # established in the same tryCatch-call after this handler have not been
    # disestablished by this point
    cond_new
  }
}
