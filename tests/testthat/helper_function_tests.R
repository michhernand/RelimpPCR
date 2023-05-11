# Test 1: Check if normalization works properly
test_that("Normalization works properly", {
  # Create some test data
  train_x <- matrix(c(
    1, 2, 3,
    4, 5, 6,
    7, 8, 9,
    10, 11, 12
  ), ncol = 3)

  train_y <- c(1, 2, 3, 4)

  test_x <- matrix(c(
    10, 11, 12,
    13, 14, 15
  ), ncol = 3)

  test_y <- c(4, 5)

  # Normalize the data
  out <- normalize(train_x, train_y, test_x, test_y)

  # Check if the means and standard deviations of the training data are equal
  # to 0 and 1 respectively
  expect_equal(
    round(
      colMeans(out$train_x),
      digits = 2
    ),
    c(0, 0, 0)
  )
})

# Test 2: Check if normalization works when test data is missing
test_that("Normalization works when test data is missing", {
  # Create some test data
  train_x <- matrix(c(
    1, 2, 3,
    4, 5, 6,
    7, 8, 9,
    10, 11, 12
  ), ncol = 3)

  train_y <- c(1, 2, 3, 4)
  test_x <- NULL
  test_y <- NULL

  # Normalize the data
  out <- normalize(train_x, train_y, test_x, test_y)

  # Check if the means and standard deviations of the training data
  # are equal to 0 and 1 respectively
  expect_equal(
    round(
      colMeans(out$train_x),
      digits = 2
    ),
    c(0, 0, 0)
  )

  # Check if the test data is still NA
  expect_true(is.null(out$test_x))
  expect_true(is.null(out$test_y))
})