# Testa Relimp Rank LMG
test_that("Rank Features by Relimp Test", {
    data <- list(
        train_y = mtcars[, 1],
        train_x = mtcars[, 2:ncol(mtcars)],
        test_y = mtcars[, 1],
        test_x = mtcars[, 2:ncol(mtcars)]
    )
    relimp_algorithm <- "lmg"

    ranked_features <- rank_features_by_relimp(data, relimp_algorithm)

    i <- 1
    for (expected_result in c(2, 3, 4, 6, 1, 10, 8, 5, 9, 7)) {
        expect_true(ranked_features$ranked_factors[i] == expected_result)
        i <- i + 1
    }
})

# Test Relimp Rank First
test_that("Rank Features by Relimp Test", {
    data <- list(
        train_y = mtcars[, 1],
        train_x = mtcars[, 2:ncol(mtcars)],
        test_y = mtcars[, 1],
        test_x = mtcars[, 2:ncol(mtcars)]
    )
    relimp_algorithm <- "first"

    ranked_features <- rank_features_by_relimp(data, relimp_algorithm)

    i <- 1
    for (expected_result in c(2, 3, 4, 5, 1, 10, 6, 7, 9, 8)) {
        expect_true(ranked_features$ranked_factors[i] == expected_result)
        i <- i + 1
    }
})

# Test Iteratively Remove Features LMG
test_that("Iteratively Remove Features LMG", {
    train_x_pca <- prcomp(mtcars[, 2:ncol(mtcars)])$x
    dfs <- list(
        train_y = mtcars[, 1],
        train_x = mtcars[, 2:ncol(mtcars)],
        test_y = mtcars[, 1],
        test_x = mtcars[, 2:ncol(mtcars)]
    )
    max_factors_to_remove <- 5
    relimp_algorithm <- "lmg"

    ranked_features <- iteratively_remove_features(
        train_x_pca,
        dfs,
        max_factors_to_remove,
        relimp_algorithm
    )

    i <- 1
    for (expected_result in c(1, 7, 6, 3, 2, 4, 8, 10, 5, 9)) {
        expect_true(ranked_features$ranked_factors[i] == expected_result)
        i <- i + 1
    }
})

# Test Iteratively Remove Features First
test_that("Iteratively Remove Features First", {
    train_x_pca <- prcomp(mtcars[, 2:ncol(mtcars)])$x
    dfs <- list(
        train_y = mtcars[, 1],
        train_x = mtcars[, 2:ncol(mtcars)],
        test_y = mtcars[, 1],
        test_x = mtcars[, 2:ncol(mtcars)]
    )
    max_factors_to_remove <- 5
    relimp_algorithm <- "first"

    ranked_features <- iteratively_remove_features(
        train_x_pca,
        dfs,
        max_factors_to_remove,
        relimp_algorithm
    )

    i <- 1
    for (expected_result in c(1, 7, 6, 3, 2, 4, 8, 10, 5, 9)) {
        expect_true(ranked_features$ranked_factors[i] == expected_result)
        i <- i + 1
    }
})

# Test Staticly Remove Features LMG
test_that("Staticly Remove Features LMG", {
    train_x_pca <- prcomp(mtcars[, 2:ncol(mtcars)])$x
    dfs <- list(
        train_y = mtcars[, 1],
        train_x = mtcars[, 2:ncol(mtcars)],
        test_y = mtcars[, 1],
        test_x = mtcars[, 2:ncol(mtcars)]
    )
    factors_to_remove <- 5
    relimp_algorithm <- "lmg"

    ranked_features <- staticly_remove_features(
        train_x_pca,
        dfs,
        factors_to_remove,
        relimp_algorithm
    )

    i <- 1
    for (expected_result in c(1, 5, 4, 3, 2)) {
        expect_true(ranked_features$ranked_factors[i] == expected_result)
        i <- i + 1
    }
})

# Test Staticly Remove Features First
test_that("Staticly Remove Features First", {
    train_x_pca <- prcomp(mtcars[, 2:ncol(mtcars)])$x
    dfs <- list(
        train_y = mtcars[, 1],
        train_x = mtcars[, 2:ncol(mtcars)],
        test_y = mtcars[, 1],
        test_x = mtcars[, 2:ncol(mtcars)]
    )
    factors_to_remove <- 5
    relimp_algorithm <- "first"

    ranked_features <- staticly_remove_features(
        train_x_pca,
        dfs,
        factors_to_remove,
        relimp_algorithm
    )

    i <- 1
    for (expected_result in c(1, 5, 4, 3, 2)) {
        expect_true(ranked_features$ranked_factors[i] == expected_result)
        i <- i + 1
    }
})