library(RelimpPCR)
library(testthat)

context("Test PCA Loop (Process One)")

mtcars_y <- mtcars[, 1]
mtcars_x <- mtcars[, 2:6]


test_that(
  "Find Factor Subset Test 1",
  {
    expect_equal(
      find_factor_subset(1, mtcars_x, mtcars_y)$ok,
      TRUE
    )
  }
)

test_that(
  "Find Factor Subset Test 2",
  {
    expect_equal(
      find_factor_subset(4, mtcars_x, mtcars_y)$ok,
      TRUE
    )
  }
)

test_that(
  "Find All Factor Subset Test 1",
  {
    expect_equal(
      find_all_factor_subset(1, mtcars_x, mtcars_y)$ok,
      TRUE
    )
  }
)

test_that(
  "Find All Factor Subset Test 2",
  {
    expect_equal(
      find_all_factor_subset(4, mtcars_x, mtcars_y)$ok,
      TRUE
    )
  }
)
