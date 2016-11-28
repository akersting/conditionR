
conditionR
==========

[![Build Status](https://travis-ci.org/akersting/conditionR.svg?branch=develop)](https://travis-ci.org/akersting/conditionR) [![Coverage Status](https://codecov.io/github/akersting/conditionR/coverage.svg?branch=develop)](https://codecov.io/github/akersting/conditionR)

Installation
------------

There is no stable release of conditionR yet. To install the latest development version run:

``` r
devtools::install_github("akersting/conditionR@develop")
```

Introductory Examples
---------------------

``` r
library(conditionR)

f <- function(x) {
  setErrorContext("someErrorClass", "There was an error in f().")
  if (is.na(x)) {
    signal(stackError("x must not be NA.", "NAError"))
  }
  
}

setErrorContext("globalErrorClass", "A global error message.", call = FALSE)

f(NA)
#> Error in f(NA): 
#> [globalErrorClass] A global error message.
#> [... -> someErrorClass] There was an error in f().
#> [... -> NAError] x must not be NA.
```

``` r
lowLevelFUN <- function() {
  signal(stackError("A low level error message.", 
                    c("lowLevelError", "evenLowerLevelError")))
}

highLevelFUN <- function() {
  setErrorContext("highLevelError", "A high level error message.")
  lowLevelFUN()
}

highLevelFUN()
#> Error in highLevelFUN(): 
#> [globalErrorClass] A global error message.
#> [... -> highLevelError] A high level error message.
#> [... -> lowLevelError -> evenLowerLevelError] A low level error message.
```

``` r
tryCatch(
  highLevelFUN(),
  error = function(e) class(e)
)
#> [1] "evenLowerLevelError" "lowLevelError"       "highLevelError"     
#> [4] "globalErrorClass"    "error"               "condition"
```
