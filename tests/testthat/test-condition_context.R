context("condition_context")

test_that("setting condition contexts fails for invalid arguments", {
  # class
  expect_error(setConditionContext(NULL))
  expect_error(setConditionContext(NA_character_))

  # message
  expect_error(setConditionContext("class", c("msg1", "msg2")))
  expect_error(setConditionContext("class", c("msg1", NA)))
  expect_error(setConditionContext("class", c(a = "msg1", "msg2", "msg3")))
  msg <- c("msg1", "msg2", "msg3")
  names(msg) <- c(1, NA, 2)
  expect_error(setConditionContext("class", msg))

  # base_class
  expect_error(setConditionContext("class", "msg", base_class = NA_character_))
  expect_error(setConditionContext("class", "msg", base_class = "default"))
  expect_error(setConditionContext("class", "msg", base_class = c("c1", "c2")))

  # type
  expect_error(setConditionContext("class", "msg", base_class = "bc",
                                   type = NA_character_))
  expect_error(setConditionContext("class", "msg", base_class = "bc",
                                   type = "type"))
  expect_error(setConditionContext("class", "msg", base_class = "bc",
                                   type = c("t1", "error")))

  # call
  expect_error(setConditionContext("class", "msg", base_class = "bc",
                                   type = "none", call = NA))
  expect_error(setConditionContext("class", "msg", base_class = "bc",
                                   type = "none", call = c(TRUE, FALSE)))

  # ellipsis
  expect_error(setConditionContext("class", "msg", base_class = "bc",
                                   type = "none", call = TRUE, 123))
})
