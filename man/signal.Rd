% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/signal.R
\name{signal}
\alias{signal}
\alias{ignoreError}
\title{Signal a Condition}
\usage{
signal(cond)

ignoreError(e)
}
\arguments{
\item{cond}{a condition object.}

\item{e}{the condition, i.e. error, object (ignored).}
}
\value{
these functions are invoked for their side effects and return
  \code{NULL} invisibly.
}
\description{
\code{signal} is a generic which chooses the appropriate base function for
signaling a condition depending on whether it is an error, a warning, a
message or neither of these. Furthermore, if \code{cond} is a call to
\code{stack*}, which returns a condition object, the call to \code{signal} is
ignored when constructing the \code{call} element of \code{cond}.
}
\details{
Errors are signaled offering an "ignoreError"-restart. If invoked,
  execution will continue after the call to \code{signal}.
}
