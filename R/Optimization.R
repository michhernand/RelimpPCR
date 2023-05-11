get_r2s <- function(
    n_factors_to_keep,
    dfs
) {
    if (n_factors_to_keep == 1) {
        train_x <- data.frame(dfs$train_x[, 1:n_factors_to_keep])
        test_x <- data.frame(dfs$test_x[, 1:n_factors_to_keep])
        colnames(train_x) <- "X1"
        colnames(test_x) <- "X1"
    } else {
        train_x <- data.frame(dfs$train_x[, 1:n_factors_to_keep])
        test_x <- data.frame(dfs$test_x[, 1:n_factors_to_keep])
    }

    fit <- caret::train(
        x = as.data.frame(train_x),
        y = as.vector(dfs$train_y),
        method = "lm")

    train_r2 <- cor(predict(fit, train_x), dfs$train_y)^2
    test_r2 <- cor(predict(fit, test_x), dfs$test_y)^2

    return(c(train_r2, test_r2))
}

get_r2s_batch_mp <- function(
    predictors_range,
    dfs,
    raw_ranked,
    pca_ranked,
    train_x_pca,
    test_x_pca,
    cores) {
    out <- list()

    log::log_info("original features")
    out$original_r2 <- parallel::mclapply(
      X = predictors_range,
      FUN = get_r2s,
      dfs = dfs,
      mc.cores = cores)

    if (raw_ranked$ok == TRUE) {
        log::log_info("ordered features")
      out$relimp_r2 <- parallel::mclapply(
        X = predictors_range,
        FUN = get_r2s,
        dfs = list(
          train_x = raw_ranked$train_x_ordered,
          train_y = dfs$train_y,
          test_x = raw_ranked$test_x_ordered,
          test_y = dfs$test_y
        ),
        mc.cores = cores)
    }

    log::log_info("pca factors")
    out$pca_r2 <- parallel::mclapply(
      X = predictors_range,
      FUN = get_r2s,
      dfs = list(
        train_x = train_x_pca,
        train_y = dfs$train_y,
        test_x = test_x_pca,
        test_y = dfs$test_y
      ),
      mc.cores = cores)

    log::log_info("ordered pca factors")
    out$pca_relimp_r2 <- parallel::mclapply(
      X = predictors_range,
      FUN = get_r2s,
      dfs = list(
        train_x = pca_ranked$train_x_ordered,
        train_y = dfs$train_y,
        test_x = pca_ranked$test_x_ordered,
        test_y = dfs$test_y
      ),
      mc.cores = cores
    )
    return(out)
}

get_r2s_batch <- function(
    predictors_range,
    dfs,
    raw_ranked,
    pca_ranked,
    train_x_pca,
    test_x_pca
) {
    out <- list()

    log::log_info("original features")
    out$original_r2 <- lapply(
        X = predictors_range,
        FUN = get_r2s,
        dfs = dfs,
    )
    if (raw_ranked$ok == TRUE) {
        log::log_info("ordered features")
        out$relimp_r2 <- lapply(
            X = predictors_range,
            FUN = get_r2s,
            dfs = list(
                train_x = raw_ranked$train_x_ordered,
                train_y = dfs$train_y,
                test_x = raw_ranked$test_x_ordered,
                test_y = dfs$test_y
            ),
        )
    }

    log::log_info("pca factors")
    out$pca_r2 <- lapply(
        X = predictors_range,
        FUN = get_r2s,
        dfs = list(
            train_x = train_x_pca,
            train_y = dfs$train_y,
            test_x = test_x_pca,
            test_y = dfs$test_y
        ),
    )

    log::log_info("ordered pca factors")
    out$pca_relimp_r2 <- lapply(
        X = predictors_range,
        FUN = get_r2s,
        dfs = list(
            train_x = pca_ranked$train_x_ordered,
            train_y = dfs$train_y,
            test_x = pca_ranked$test_x_ordered,
            test_y = dfs$test_y
        ),
    )
    return(out)
}


# TODO: Option for test instead of train?
get_best_model <- function(
    dfs,
    train_r2,
    test_r2
) {
    best_r2 <- which.max(train_r2)
    return(
        lm(
            Y ~ .,
            data = data.frame(
                Y = dfs$train_y,
                dfs$train_x[, 1:best_r2]
            )
        )
    )
}