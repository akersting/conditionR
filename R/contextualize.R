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
#'   condition. "call" is ignored.
#' @param regex a logical value. If \code{FALSE} (the default), then the names
#'   of the \code{...} arguments refer to the class of the condition object. If
#'   \code{TRUE}, the names of the \code{...} arguments are treated as regular
#'   expressions against which the condition message is matched.
#' @param include_original_message a logical value. Should the message of the
#'   caught condition be appended to the message specified in the chosen
#'   \code{...} list?
#'
#' @return whatever \code{expr} returns.
#'
#' @export
contextualize <- function(expr, ..., regex = FALSE,
                          include_original_message = TRUE) {
  contexts <- list(...)
  withCallingHandlers(
    expr,
    condition = function(cond) {
      msg <- conditionMessage(cond)
      context <- NULL
      if (regex) {
        for (rgx in names(contexts)) {
          if (length(grep(rgx, msg)) == 1) {
            context <- contexts[[rgx]]
            break
          }
        }
      } else {
        for (cl in names(contexts)) {
          if (inherits(cond, cl)) {
            context <- contexts[[cl]]
            break
          }
        }
      }

      if (!is.null(context)) {
        if (include_original_message) {
          context[["message"]] <- paste0(context[["message"]],
                                         " | Original message: ", msg)
        }

        if (is.null(context[["type"]])) {
          context[["type"]] <-
            ifelse(inherits(cond, "error"), "error",
                   ifelse(inherits(cond, "warning"), "warning",
                          ifelse(inherits(cond, "message"), "message",
                                 "none")))
        }

        context[["call"]] <- FALSE

        cond_new <- do.call(stackCondition, context)
        if (is.null(cond_new[["call"]])) {
          cond_new[["call"]] <- cond[["call"]]
        }

        signal(cond_new)
      }
    }
  )
}