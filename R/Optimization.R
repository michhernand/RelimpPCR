get_r2s <- function(
    factors_to_remove,
    dfs
) {
    if (z == 1) {
        train_x <- data.frame(dfs$train_x[, 1:factors_to_remove])
        test_x <- data.frame(dfs$test_x[, 1:factors_to_remove])
        colnames(train_x) <- "X1"
        colnames(test_x) <- "X1"
    } else {
        train_x <- data.frame(dfs$train_x[, 1:factors_to_remove])
        test_x <- data.frame(dfs$test_x[, 1:factors_to_remove])
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
    raw_ranked_features,
    pca_ranked,
    train_x_pca,
    test_x_pca,
    ranking_successful,
    cores,
    verbose) {
    log::log_info("original features")

    original_r2 <- parallel::mclapply(
      X = predictors_range,
      FUN = get_r2s,
      dfs = dfs,
      mc.cores = cores)

    if(ranking_successful==T){
        log::log_info("ordered features")
      relimp_r2 <- parallel::mclapply(
        X = predictors_range,
        FUN = get_r2s,
        dfs = list(
          train_x = raw_ranked_features$train_x_ordered,
          train_y = dfs$train_y,
          test_x = raw_ranked_features$test_x_ordered,
          test_y = dfs$test_y
        ),
        mc.cores = cores)
    }

    log::log_info("pca factors")
    pca_r2 <- parallel::mclapply(
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
    pca_relimp_r2 = parallel::mclapply(
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
}

get_best_model <- function(
    dfs,
    train_r2,
    test_rw
) {
    best_rq <- which.max(train_r2)
    return(
        lm(
            Y ~ .,
            data = data.frame(
                Y = dfs$train_y,
                dfs$train_x[, 1:best_rq]
            )
        )
    )
}