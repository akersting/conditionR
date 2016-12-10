context("contextualize")

test_that("contextualize chooses the correct handler", {
  cl <- tryCatch(
    contextualize(stop("test error"),
                  warning = list(class = "context warning"),
                  error = list(class = "context error"),
                  condition = list(class = "context condition")),
    condition = function(cond) class(cond)[1]
  )
  expect_equal(cl, "context error")

  cl <- tryCatch(
    contextualize(warning("test warning"),
                  warning = list(class = "context warning"),
                  error = list(class = "context error"),
                  condition = list(class = "context condition")),
    condition = function(cond) class(cond)[1]
  )
  expect_equal(cl, "context warning")
})