.stackCondition  <- function(message, class, base_class,
                             call, type, ellipsis) {

  if (!is.character(message) || length(message) != 1 || is.na(message)) {
    stop("'message' must be a character string.")
  }

  if (!is.character(class) || any(is.na(class))) {
    stop("'class' must be a character vector (without NAs).")
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

  if (!is.character(type) || length(type) != 1 || is.na(type) ||
      !type %in% c("error", "warning", "message", "none")) {
    stop("'type' must be one of 'error', 'warning', 'message' or 'none'.")
  }

  if (length(ellipsis) >= 1 && (is.null(names(ellipsis)) ||
                                any(is.na(names(ellipsis))) ||
                                any(names(ellipsis) == ""))) {
    stop("All additional arguments (...) must be named.")
  }

  # add condition context implied by call to stack*() --------------------------
  messages <- list(message)
  classes <- list(rev(class))

  explicit_call <- FALSE
  if (sys.nframe() == 2 ||
      is.null(attr(sys.frame(-2), "conditionR_skip_frame",
                   exact = TRUE))) {
    frames2skip <- 2  # skip over call to stack*()
  } else {
    frames2skip <- 3  # skip over calls to signal() and stack*()
  }
  if (is.null(call)) {
    call <- sys.call(-frames2skip)
  } else if (call == TRUE) {
    call <- sys.call(-frames2skip)
    explicit_call <- TRUE
  } else {
    call <- NULL
  }

  # collect condition contexts along the function call stack -------------------
  # (incl. global envir)
  unique_frame_ids <- which(!duplicated(c(.GlobalEnv, sys.frames()))) - 1
  cntr <- 1
  for (i in rev(unique_frame_ids)[-seq_len(frames2skip)]) {
    env <- sys.frame(i)
    env_addr <- getAddress(env)
    condition_context <- contexts[[env_addr]][[type]][[base_class]]

    if (is.null(condition_context)) {
      next
    }

    if (!is.null(names(condition_context[["message"]]))) {
      # conditional message
      message_id <- integer()
      for (class in unlist(classes)) {
        message_id <- which(names(condition_context[["message"]]) == class)
        if (length(message_id) > 0) {
          cntr <- cntr + 1
          classes[[cntr]] <- character()
          messages[[cntr]] <- condition_context[["message"]][message_id]
          break
        }
      }

      # use default message
      if (length(message_id) == 0) {
        message_id <- which(names(condition_context[["message"]]) == "")
        if (length(message_id) > 0) {
          cntr <- cntr + 1
          classes[[cntr]] <- character()
          messages[[cntr]] <- condition_context[["message"]][message_id]
        }

      }
    } else if (length(condition_context[["message"]]) > 0) {
      # unconditional message
      cntr <- cntr + 1
      classes[[cntr]] <- character()
      messages[[cntr]] <- condition_context[["message"]]
    }

    classes[[cntr]] <- c(classes[[cntr]], rev(condition_context[["class"]]))

    # don't overwrite ellipsis-elements which were set later
    ellipsis_names <- names(condition_context[["..."]])
    new_ellipsis_names <- ellipsis_names[!ellipsis_names %in% names(ellipsis)]
    ellipsis[new_ellipsis_names] <-
      condition_context[["..."]][new_ellipsis_names]

    if (i > 0 && !explicit_call) {
      if (is.null(condition_context[["call"]])) {
        call <- sys.call(i)
      } else if (!is.null(condition_context[["call"]]) &&
                 condition_context[["call"]] == TRUE) {
        call <- sys.call(i)
        explicit_call <- TRUE
      }
    }
  }

  # create elements of condition object ----------------------------------------
  if (base_class != "default") {
    classes[[length(classes)]] <- c(classes[[length(classes)]], base_class)
  }

  message <- character()
  for (i in rev(seq_along(messages))) {
    if (i == length(messages)) {
      class <- paste0(rev(classes[[i]]), collapse = " -> ")
    } else {
      class <- paste0(paste0(c("...", rev(classes[[i]])), collapse = " -> "))
    }
    message <- paste(message, paste0("[", class, "] ", messages[[i]]),
                     sep = "\n")
  }

  classes <- unlist(classes)
  if (type != "none") {
    classes <- c(classes, type, "condition")
  } else {
    classes <- c(classes, "condition")
  }
  classes <- unique(classes, fromLast = TRUE)

  # create condition object ----------------------------------------------------
  structure(
    c(
      list(
        message = message,
        call = call
      ),
      ellipsis
    ),
    class = classes
  )
}

#' @rdname stackedConditions
#'
#' @export
stackCondition  <- function(message, class = character(), base_class = NULL,
                            call = NULL, type = "none", ...) {
  .stackCondition(message = message, class = class, base_class = base_class,
                  call = call, type = type, ellipsis = list(...))
}

#' @rdname stackedConditions
#'
#' @export
stackError  <- function(message, class = character(), base_class = NULL,
                        call = NULL, ...) {
  .stackCondition(message = message, class = class, base_class = base_class,
                  call = call, type = "error", ellipsis = list(...))
}

#' @rdname stackedConditions
#'
#' @export
stackWarning  <- function(message, class = character(), base_class = NULL,
                        call = NULL, ...) {
  .stackCondition(message = message, class = class, base_class = base_class,
                  call = call, type = "warning", ellipsis = list(...))
}

#' @rdname stackedConditions
#'
#' @export
stackMessage  <- function(message, class = character(), base_class = NULL,
                        call = NULL, ...) {
  .stackCondition(message = message, class = class, base_class = base_class,
                  call = call, type = "message", ellipsis = list(...))
}
