testthat::test_that("semljsyn fits a regression-like model", {
    data(pathjdata, package = "semlj")

    result <- testthat::expect_no_warning(semlj::semljsyn(
        data = pathjdata,
        vars = c("y4", "y1", "x1"),
        code = "y4 ~ y1 + x1",
        cluster = NULL,
        multigroup = NULL,
        donotrun = FALSE,
        sample_n = NULL,
        sample_mean = NULL,
        sample_std = NULL,
        meas_invariance = NULL))

    coefficients <- result$models$coefficients$asDF

    testthat::expect_equal(coefficients$lhs, c("y4", "y4"))
    testthat::expect_equal(coefficients$rhs, c("y1", "x1"))
    testthat::expect_true(all(is.finite(coefficients$est)))
    testthat::expect_true(all(is.finite(coefficients$se)))
})

testthat::test_that("rejects a numeric multigroup variable", {
    data(pathjdata, package = "semlj")
    pathjdata$groups_a <- as.numeric(pathjdata$groups_a)

    testthat::expect_error(
        semlj::semljsyn(
            data = pathjdata,
            .interface = "R",
            vars = c("y4", "y1", "x1", "groups_a"),
            code = "y4 ~ y1 + x1",
            multigroup = "groups_a",
            cluster = NULL,
            donotrun = FALSE,
            sample_n = NULL,
            sample_mean = NULL,
            sample_std = NULL,
            meas_invariance = NULL),
        "Multigroup variable groups_a should be a factor")
})


testthat::test_that("defined-parameter descriptions treat labels literally", {
    data(pathjdata, package = "semlj")

    result <- testthat::expect_no_warning(semlj::semljsyn(
        data = pathjdata,
        vars = c("y4", "y1", "x1"),
        code = "y4 ~ a.b*y1 + axb*x1\nind := a.b*axb",
        cluster = NULL,
        multigroup = NULL,
        donotrun = FALSE,
        sample_n = NULL,
        sample_mean = NULL,
        sample_std = NULL,
        meas_invariance = NULL))

    defined <- result$models$defined$asDF

    testthat::expect_equal(defined$desc, " (y4~y1) * (y4~x1) ")
})
