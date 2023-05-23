train_test_split <- function(X, Y, validation_split) {
  stopifnot(validation_split > 0)
  stopifnot(validation_split <= 1)
  if (validation_split == 1) {
    train_y <- X
    test_x <- X
    train_y <- Y
    test_y <- Y
  } else {
    logger::log_info("performing train/test split")
    ix <- sample(
        x = 1:dim(X)[1],
        size = round(dim(X)[1] * validation_split, 0)
    )
    train_x <- X[ix,]
    test_x <- X[-ix,]
    train_y <- Y[ix]
    test_y <- Y[-ix]
  }

  out <- list()
  out$train_x <- train_x
  out$train_y <- train_y
  out$test_x <- test_x
  out$test_y <- test_y
  return(out)
}
