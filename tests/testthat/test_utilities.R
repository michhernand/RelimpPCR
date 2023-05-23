# Test 1: Check if tts works properly
test_that("Normalization works properly", {
  # Create some test data
  x <- matrix(c(
    1, 2, 3,
    4, 5, 6,
    7, 8, 9,
    10, 11, 12
  ), ncol = 3)
  x <- data.frame(x)

  y <- c(1, 2, 3, 4)

    dfs <- train_test_split(x, y, validation_split = 0.5)
    expect_equal(dim(dfs$train_x)[[1]], 2)
})



# Test 2: Check if tts raises exc for neg val split
test_that("Negative val split handled", {
  # Create some test data
  x <- matrix(c(
    1, 2, 3,
    4, 5, 6,
    7, 8, 9,
    10, 11, 12
  ), ncol = 3)
  x <- data.frame(x)

  y <- c(1, 2, 3, 4)

  err <- tryCatch({
    train_test_split(x, y, validation_split = -0.5)
    return(FALSE)
  },
  error = function(e) {
    return(TRUE)
  })
  expect_true(err)
})

# Test 3: Check if tts raises exc for >1 val split
test_that("Greater than 1 val split handled", {
  # Create some test data
  x <- matrix(c(
    1, 2, 3,
    4, 5, 6,
    7, 8, 9,
    10, 11, 12
  ), ncol = 3)
  x <- data.frame(x)

  y <- c(1, 2, 3, 4)

  err <- tryCatch({
    train_test_split(x, y, validation_split = 1.5)
    return(FALSE)
  },
  error = function(e) {
    return(TRUE)
  })
  expect_true(err)
})