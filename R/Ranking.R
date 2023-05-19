rank_features_by_relimp <- function(data, relimp_algorithm = "lmg") {
  out <- tryCatch(
    {
        fit <- lm(
            Y ~ .,
            data = data.frame(
                Y = unlist(data$train_y),
                data$train_x
            )
        )

        relimp_factors <- relaimpo::calc.relimp(
            fit,
            type = relimp_algorithm
        )

        ranked_factors <- get_relimp_factor_rank(
            relimp_factors,
            relimp_algorithm
        )

        train_x_ordered <- data$train_x[, order(ranked_factors)]

        if (!is.null(data$test_x)) {
            test_x_ordered <- data$test_x[, order(ranked_factors)]
        }
        return(list(
            ok = TRUE,
            fit = fit,
            relimp_factors = relimp_factors,
            ranked_factors = ranked_factors,
            train_x_ordered = train_x_ordered
        ))
    },
    error = function(cond) {
        warning(
            paste(
                "WARN: Ranking predictors against Y using calc.relimp FAILED.",
                cond
            )
        )
        return(list(
            ok = FALSE,
            fit = NULL,
            relimp_factors = NULL,
            ranked_factors = NULL,
            train_x_ordered = NULL
        ))
    }
  )

    if (!is.null(data$test_x)) {
        out$test_x_ordered <- test_x_ordered
    }

    return(out)
}

iteratively_remove_features <- function(
    train_x_pca,
    dfs,
    max_factors_to_remove,
    relimp_algorithm
) {
    for (x in 0:max_factors_to_remove) {
        pca_factor_subset <- train_x_pca[, 1:(ncol(train_x_pca) - x)]
        pca_ranked_factors <- rank_features_by_relimp(
            data = list(
                train_x = pca_factor_subset,
                train_y = dfs$train_y,
                test_x = NULL,
                test_y = NULL
            ),
            relimp_algorithm = relimp_algorithm
        )
        if (pca_ranked_factors$ok) {
            break
        }
        if (x == max_factors_to_remove) {
            stop(
            "Could not create non-singular matrix. 
            Try increasing max_factors_to_remove.")
        }
    }
    return(pca_ranked_factors)
}

staticly_remove_features <- function(
    train_x_pca,
    dfs,
    factors_to_remove,
    relimp_algorithm
) {
    pca_factor_subset <- train_x_pca[, 1:(
        ncol(train_x_pca) - factors_to_remove)]
    pca_ranked_factors <- rank_features_by_relimp(
        data = list(
            train_x = pca_factor_subset,
            train_y = dfs$train_y,
            test_x = NULL,
            test_y = NULL
        ),
        relimp_algorithm = relimp_algorithm
    )
    if (!pca_ranked_factors$ok) {
        stop(
            "Could not create non-singular matrix. 
            Try increasing max_factors_to_remove.")
    }
    return(pca_ranked_factors)
}