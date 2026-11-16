testthat::test_that("observed regression agrees with ordinary least squares", {
    result <- fit_syntax(mtcars, "mpg ~ wt + hp", c("mpg", "wt", "hp"), r2 = "endo")
    reference <- stats::lm(mpg ~ wt + hp, data = mtcars)
    coefficients <- result$models$coefficients$asDF

    testthat::expect_equal(coefficients$rhs, c("wt", "hp"))
    testthat::expect_equal(
        coefficients$est, unname(stats::coef(reference)[coefficients$rhs]),
        tolerance = 1e-5)
    testthat::expect_equal(
        result$fit$rsquared$asDF$est, summary(reference)$r.squared,
        tolerance = 1e-5)
})

testthat::test_that("CFA loadings and fit match lavaan and fixed benchmarks", {
    dataset <- lavaan::HolzingerSwineford1939
    result <- fit_syntax(dataset, cfa_syntax, paste0("x", 1:9))
    reference <- lavaan::cfa(cfa_syntax, data = dataset, meanstructure = TRUE)

    expect_parameter_table(result$models$loadings$asDF, reference, "=~")
    expect_fit_tables(result, reference)
    main <- result$fit$main$asDF
    testthat::expect_equal(main$chisq[1], 85.30552, tolerance = 1e-5)
    testthat::expect_equal(main$df[1], 24)
    testthat::expect_equal(result$fit$indices$asDF$rmsea[1], 0.09212148, tolerance = 1e-5)
    testthat::expect_equal(result$fit$indices$asDF$srmr[1], 0.05952380, tolerance = 1e-5)
    testthat::expect_equal(
        result$models$loadings$asDF$est,
        c(1, 0.5535003, 0.7293702, 1, 1.1130766, 0.9261462, 1, 1.1799508, 1.0815302),
        tolerance = 1e-5)
})

testthat::test_that("GUI endogenous latent paths agree with syntax and lavaan", {
    dataset <- lavaan::HolzingerSwineford1939
    model <- paste(cfa_syntax, "visual ~ textual + speed", sep = "\n")
    gui <- fit_gui(
        dataset,
        code = model,
        endogenous = list(list(label = "visual", vars = paste0("x", 1:3))),
        exogenous = list(
            list(label = "textual", vars = paste0("x", 4:6)),
            list(label = "speed", vars = paste0("x", 7:9))),
        endogenousTerms = list(list("textual", "speed")))
    syntax <- fit_syntax(dataset, model, paste0("x", 1:9))
    reference <- lavaan::sem(model, data = dataset, meanstructure = TRUE)

    expect_parameter_table(gui$models$coefficients$asDF, reference, "~")
    expect_parameter_table(gui$models$loadings$asDF, reference, "=~")
    expect_fit_tables(gui, reference)
    expect_parameter_table(syntax$models$coefficients$asDF, reference, "~")
    expect_fit_tables(syntax, reference)
})

testthat::test_that("equal multigroup loadings agree with lavaan", {
    dataset <- lavaan::HolzingerSwineford1939
    result <- fit_syntax(
        dataset, cfa_syntax, c(paste0("x", 1:9), "school"),
        multigroup = "school", eq_loadings = TRUE)
    reference <- lavaan::cfa(
        cfa_syntax, data = dataset, meanstructure = TRUE,
        group = "school", group.equal = "loadings")

    expect_parameter_table(result$models$loadings$asDF, reference, "=~")
    expect_fit_tables(result, reference)
})

testthat::test_that("FIML estimates and fit agree with lavaan", {
    dataset <- lavaan::HolzingerSwineford1939
    dataset$x2[seq(1, nrow(dataset), by = 7)] <- NA_real_
    dataset$x5[seq(3, nrow(dataset), by = 11)] <- NA_real_
    result <- fit_syntax(dataset, cfa_syntax, paste0("x", 1:9), missing = "ml")
    reference <- lavaan::cfa(
        cfa_syntax, data = dataset, meanstructure = TRUE, missing = "fiml")

    expect_parameter_table(result$models$loadings$asDF, reference, "=~")
    expect_fit_tables(result, reference)
})

testthat::test_that("defined effects and confidence intervals agree with lavaan", {
    dataset <- mtcars
    dataset$hp <- dataset$hp / 100
    model <- "mpg ~ direct*hp + mediator*wt\nwt ~ exposure*hp\nindirect := exposure*mediator\ntotal := direct + indirect"
    result <- fit_syntax(dataset, model, c("mpg", "wt", "hp"))
    reference <- lavaan::sem(model, data = dataset, meanstructure = TRUE, fixed.x = FALSE)

    expect_parameter_table(result$models$coefficients$asDF, reference, "~")
    expect_parameter_table(result$models$defined$asDF, reference, ":=")
})

testthat::test_that("robust standard errors and scaled tests agree with lavaan", {
    dataset <- lavaan::HolzingerSwineford1939
    result <- fit_syntax(dataset, cfa_syntax, paste0("x", 1:9), estimator = "MLR")
    reference <- lavaan::cfa(
        cfa_syntax, data = dataset, meanstructure = TRUE, estimator = "MLR")

    expect_parameter_table(result$models$loadings$asDF, reference, "=~")
    expect_fit_tables(result, reference)
    main <- result$fit$main$asDF
    scaled <- main[trimws(main$label) == "Scaled User", ]
    expected <- lavaan::fitMeasures(reference)
    testthat::expect_equal(nrow(scaled), 1L)
    for (column in c("chisq", "df", "pvalue")) {
        testthat::expect_equal(
            scaled[[column]], unname(expected[paste0(column, ".scaled")]),
            tolerance = 1e-5)
    }
})
