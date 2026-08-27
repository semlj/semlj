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



testthat::test_that("matrix multigroup metadata names are validated", {
  matrix_data <- data.frame(
    groups_a = factor(rep(c("A", "B"), each = 3)),
    y1 = c(1, 0, 0, 1, 0, 0),
    y2 = c(.2, 1, 0, .25, 1, 0),
    x1 = c(.1, .3, 1, .15, .2, 1),
    n = rep(100, 6),
    means = rep(c(.1, .2, .3), 2),
    std = rep(c(1, 1.1, .9), 2))

  common <- list(
    data = matrix_data,
    .interface = "R",
    vars = c("y1", "y2", "x1", "groups_a", "n", "means", "std"),
    code = "y1 ~ y2 + x1",
    multigroup = "groups_a",
    cluster = NULL,
    data_type = "cor",
    meanstructure = TRUE,
    donotrun = FALSE,
    sample_n = "n",
    sample_mean = "means",
    sample_std = "std",
    meas_invariance = NULL)

  testthat::expect_no_warning(do.call(semlj::semljsyn, common))

  common["sample_std"] <- list(NULL)
  testthat::expect_warning(
    do.call(semlj::semljsyn, common),
    "standard deviations")

  common$sample_std <- "std"
  common["sample_mean"] <- list(NULL)
  testthat::expect_warning(
    do.call(semlj::semljsyn, common),
    "sample means")

  common$sample_mean <- "means"
  common$sample_std <- "missing_std"
  testthat::expect_error(
    do.call(semlj::semljsyn, common),
    "Argument .sample_std. contains .missing_std.")

  common$sample_std <- "std"
  common$sample_n <- "missing_n"
  testthat::expect_error(
    do.call(semlj::semljsyn, common),
    "Argument .sample_n. contains .missing_n.")
})
