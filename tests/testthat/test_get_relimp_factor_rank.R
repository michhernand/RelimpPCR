# Test LMG
test_that("LMG Relimp Factor Rank Works Properly", {
    err <- tryCatch({
        data <- mtcars
        fit <- lm(
            mpg ~ .,
            data = data
        )

        relimp_factors <- relaimpo::calc.relimp(
            fit,
            type = "lmg"
        )
        x <- get_relimp_factor_rank(relimp_factors, "lmg")
        expect_true(!is.null(x))
    })
})

# Test First
test_that("First Relimp Factor Rank Works Properly", {
    err <- tryCatch({
        data <- mtcars
        fit <- lm(
            mpg ~ .,
            data = data
        )

        relimp_factors <- relaimpo::calc.relimp(
            fit,
            type = "first"
        )
        x <- get_relimp_factor_rank(relimp_factors, "first")
        expect_true(!is.null(x))
    })
})

# Test Last
test_that("Last Relimp Factor Rank Works Properly", {
    err <- tryCatch({
        data <- mtcars
        fit <- lm(
            mpg ~ .,
            data = data
        )

        relimp_factors <- relaimpo::calc.relimp(
            fit,
            type = "last"
        )
        x <- get_relimp_factor_rank(relimp_factors, "last")
        expect_true(!is.null(x))
    })
})

# Test Betasq
test_that("Betasq Relimp Factor Rank Works Properly", {
    err <- tryCatch({
        data <- mtcars
        fit <- lm(
            mpg ~ .,
            data = data
        )

        relimp_factors <- relaimpo::calc.relimp(
            fit,
            type = "betasq"
        )
        x <- get_relimp_factor_rank(relimp_factors, "betasq")
        expect_true(!is.null(x))
    })
})

# Test Pratt
test_that("Pratt Relimp Factor Rank Works Properly", {
    err <- tryCatch({
        data <- mtcars
        fit <- lm(
            mpg ~ .,
            data = data
        )

        relimp_factors <- relaimpo::calc.relimp(
            fit,
            type = "pratt"
        )
        x <- get_relimp_factor_rank(relimp_factors, "pratt")
        expect_true(!is.null(x))
    })
})

# Test Genizi
test_that("Genizi Relimp Factor Rank Works Properly", {
    err <- tryCatch({
        data <- mtcars
        fit <- lm(
            mpg ~ .,
            data = data
        )

        relimp_factors <- relaimpo::calc.relimp(
            fit,
            type = "genizi"
        )
        x <- get_relimp_factor_rank(relimp_factors, "genizi")
        expect_true(!is.null(x))
    })
})

# Test Car
test_that("Car Relimp Factor Rank Works Properly", {
    err <- tryCatch({
        data <- mtcars
        fit <- lm(
            mpg ~ .,
            data = data
        )

        relimp_factors <- relaimpo::calc.relimp(
            fit,
            type = "car"
        )
        x <- get_relimp_factor_rank(relimp_factors, "car")
        expect_true(!is.null(x))
    })
})