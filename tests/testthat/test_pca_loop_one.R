library(RelimpPCR)
library(testthat)

context("Test PCA Loop (Process One)")

Y <- mtcars[, 1]
X <- mtcars[, 2:6]

test_that(
  "Smoke Test",
  {
    expect_equal(1, 1)
  }
)

test_that(
  "Smoke Test2",
  {
    expect_equal(2, 2)
  }
)

test_that(
  "Find Factor Subset Test 1",
  {
    expect_equal(
      find_factor_subset(1, X, Y)$ok,
      TRUE
    )
  }
)

test_that(
  "Find Factor Subset Test 2",
  {
    expect_equal(
      find_factor_subset(4, X, Y)$ok,
      TRUE
    )
  }
)
