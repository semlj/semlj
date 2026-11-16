fit_syntax <- function(data, code, vars, ...) {
    arguments <- list(
        data = data,
        code = code,
        vars = vars,
        cluster = NULL,
        multigroup = NULL,
        donotrun = FALSE,
        sample_n = NULL,
        sample_mean = NULL,
        sample_std = NULL,
        meas_invariance = NULL)
    arguments <- utils::modifyList(arguments, list(...), keep.null = TRUE)
    do.call(semlj::semljsyn, arguments)
}

fit_gui <- function(data, ...) {
    arguments <- list(
        data = data,
        cluster = NULL,
        multigroup = NULL,
        varcov = NULL,
        donotrun = FALSE,
        sample_n = NULL,
        sample_mean = NULL,
        sample_std = NULL,
        meas_invariance = NULL)
    arguments <- utils::modifyList(arguments, list(...), keep.null = TRUE)
    do.call(semlj::semljgui, arguments)
}

cfa_syntax <- paste(
    "visual =~ x1 + x2 + x3",
    "textual =~ x4 + x5 + x6",
    "speed =~ x7 + x8 + x9",
    sep = "\n")

expect_parameter_table <- function(actual, reference, operator, tolerance = 1e-5) {
    expected <- lavaan::parameterEstimates(reference, standardized = TRUE)
    expected <- expected[expected$op == operator, ]
    actual_key <- paste(actual$lhs, actual$rhs)
    expected_key <- paste(expected$lhs, expected$rhs)
    if ("lgroup" %in% names(actual) && lavaan::lavInspect(reference, "ngroups") > 1) {
        groups <- lavaan::lavInspect(reference, "group.label")
        actual_key <- paste(actual$lgroup, actual_key)
        expected_key <- paste(groups[expected$group], expected_key)
    }
    testthat::expect_setequal(actual_key, expected_key)
    testthat::expect_equal(nrow(actual), nrow(expected))
    expected <- expected[match(actual_key, expected_key), ]
    for (column in c("est", "se", "ci.lower", "ci.upper", "std.all")) {
        testthat::expect_equal(
            actual[[column]], expected[[column]], tolerance = tolerance,
            info = paste("Parameter column:", column))
    }
}

expect_fit_tables <- function(result, reference, tolerance = 1e-5) {
    expected <- lavaan::fitMeasures(reference)
    main <- result$fit$main$asDF
    user <- main[trimws(main$label) == "User Model", ]
    testthat::expect_equal(nrow(user), 1L)
    for (column in c("chisq", "df", "pvalue")) {
        testthat::expect_equal(
            user[[column]], unname(expected[column]), tolerance = tolerance,
            info = paste("Model fit:", column))
    }
    indices <- result$fit$indices$asDF
    for (column in c("srmr", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "rmsea.pvalue")) {
        testthat::expect_equal(
            indices[[column]][1], unname(expected[column]), tolerance = tolerance,
            info = paste("Fit index:", column))
    }
}
