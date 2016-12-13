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

test_that("contextualize works if there is nothing to signal", {
  expect_equal(contextualize(3 * 4,
                             warning = list(class = "context warning"),
                             error = list(class = "context error"),
                             condition = list(class = "context condition")),
               12)
})