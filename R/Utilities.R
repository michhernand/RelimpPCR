#' A Train/Test Split Function
#'
#' This function splits the data into training and testing sets.
#'
#' @param x (data frame): A data frame of predictors.
#' @param y (list/vector): A list or vector of responses.
#' @param validation_split (numeric): A number between 0 and 1
#' that represents the proportion of the data to be used for testing.
#' @return out (list): A list containing all of the below components...
#' @return $train_x (data frame): A data frame of predictors for the
#' training set.
#' @return $train_y (list/vector): A list or vector of responses for
#' the training set.
#' @return $test_x (data frame): A data frame of predictors for the testing set.
#' @return $test_y (list/vector): A list or vector of responses for
#' the testing set.
#' \donttest{
#' # Below performs a 50/50 train/test split
#' result <- train_test_split(x = mtcars[,2:6], y = mtcars[,1],
#' validation_split = 0.5)
#' @export
#' }
train_test_split <- function(x, y, validation_split) {
  stopifnot(validation_split > 0)
  stopifnot(validation_split <= 1)
  if (validation_split == 1) {
    train_x <- x
    test_x <- x
    train_y <- y
    test_y <- y
  } else {
    logger::log_info("performing train/test split")
    ix <- sample(
        x = seq_len(dim(x)[1]),
        size = round(dim(x)[1] * validation_split, 0)
    )
    train_x <- x[ix,]
    test_x <- x[-ix,]
    train_y <- y[ix]
    test_y <- y[-ix]
  }

  return(list(
    train_x = train_x,
    train_y = train_y,
    test_x = test_x,
    test_y = test_y
  ))
}
