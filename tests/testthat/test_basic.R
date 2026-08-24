testthat::test_that("semljsyn fits a regression-like model", {
    n <- 60
    x1 <- seq(-1, 1, length.out = n)
    x2 <- rep(c(-1, 1), length.out = n)
    y <- 1 + 2 * x1 - 0.75 * x2 + sin(seq_len(n))

    result <- semlj::semljsyn(
        data = data.frame(y = y, x1 = x1, x2 = x2),
        vars = c("y", "x1", "x2"),
        code = "y ~ x1 + x2",
        cluster = NULL,
        multigroup = NULL,
        donotrun = FALSE,
        sample_n = NULL,
        sample_mean = NULL,
        sample_std = NULL,
        meas_invariance = NULL)

    coefficients <- result$models$coefficients$asDF

    testthat::expect_equal(coefficients$lhs, c("y", "y"))
    testthat::expect_equal(coefficients$rhs, c("x1", "x2"))
    testthat::expect_equal(coefficients$est, c(2, -0.75), tolerance = 0.1)
})
