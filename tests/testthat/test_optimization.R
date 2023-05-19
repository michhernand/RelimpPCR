test_that("Get R2s", {
    dfs <- list(
        train_y = mtcars[, 1],
        train_x = mtcars[, 2:ncol(mtcars)],
        test_y = mtcars[, 1],
        test_x = mtcars[, 2:ncol(mtcars)]
    )

    r2s <- get_r2s(3, dfs)
    expect_equal(r2s[1], 0.7678877, tolerance = 1e-6)
    expect_equal(r2s[2], 0.7678877, tolerance = 1e-6)
})