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
  out[[train_x]] <- train_x
  out[[train_y]] <- train_y
  out[[test_x]] <- test_x
  out[[test_y]] <- test_y
  return(out)
}

normalize <- function(train_x, train_y, test_x = NULL, test_y = NULL) {
  logger::log_debug("normalizing data")

  # Pre-allocate mean and sd vectors
  train_means <- numeric(length = ncol(train_x))
  train_sds <- numeric(length = ncol(train_x))

  # Compute means and sds
  for (z in seq_along(train_means)) {
    this_mean <- mean(train_x[, z])
    this_sd <- sd(train_x[, z])
    train_means[z] <- this_mean
    train_sds[z] <- this_sd
  }

  # Normalize train_x
  train_x <- sweep(train_x, 2, train_means, FUN = "-")
  train_x <- sweep(train_x, 2, train_sds, FUN = "/")

  # Normalize test_x (if provided)
  if (!is.null(test_x)) {
    test_x <- sweep(test_x, 2, train_means, FUN = "-")
    test_x <- sweep(test_x, 2, train_sds, FUN = "/")
  }

  # Normalize train_y
  y_mean <- mean(train_y)
  y_sd <- sd(train_y)
  train_y <- (train_y - y_mean) / y_sd

  # Normalize test_y (if provided)
  if (!is.null(test_y)) {
    test_y <- (test_y - y_mean) / y_sd
  }

  # Return as a list
  list(train_x = train_x,
       train_y = train_y,
       test_x = test_x,
       test_y = test_y)
}

get_relimp_factor_rank <- function(relimp_factors, relimp_algorithm) {
    if (relimp_algorithm == "lmg") {
        return(relimp_factors@lmg.rank)
    } else if (relimp_algorithm == "last") {
        return(relimp_algorithm@last.rank)
    } else if (relimp_algorithm == "first") {
        return(relimp_algorithm@first.rank)
    } else if (relimp_algorithm == "betasq") {
        return(relimp_algorithm@betasq.rank)
    } else if (relimp_algorithm == "pratt") {
        return(relimp_algorithm@pratt.rank)
    } else if (relimp_algorithm == "genizi") {
        return(relimp_algorithm@genizi.rnak)
    } else if (relimp_algorithm == "car") {
        return(relimp_algorithm@car.rank)
    } else {
        stop("unknown relaimpo type")
    }
}