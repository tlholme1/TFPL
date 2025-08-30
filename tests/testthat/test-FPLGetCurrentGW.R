library(testthat)

# This test makes a live call to the FPL API. It verifies that the
# structure of the returned gameweek information is valid.
test_that("FPLGetCurrentGW retrieves gameweek numbers from the API", {
  res <- FPLGetCurrentGW()

  expect_named(res, c("current", "next", "finished"))

  expect_true(is.numeric(res$current) && length(res$current) == 1)
  expect_true(is.numeric(res$finished) && length(res$finished) == 1)

  expect_true(is.numeric(res$next))
  expect_true(length(res$next) <= 1)

  expect_true(res$finished <= res$current)
  if (length(res$next) == 1) {
    expect_true(res$next >= res$current)
  }
})
