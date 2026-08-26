
testthat::test_that("semljsyn fits a multigroup regression-like model", {
  data(pathjdata, package = "semlj")
  pathjdata$groups_a <- factor(pathjdata$groups_a)
  
  result <- testthat::expect_no_warning(semlj::semljsyn(
    data = pathjdata,
    vars = c("y4", "y1", "x1", "groups_a"),
    code = "y4 ~ y1 + x1",
    multigroup = "groups_a",
    cluster = NULL,
    donotrun = FALSE,
    sample_n = NULL,
    sample_mean = NULL,
    sample_std = NULL,
    meas_invariance = NULL))
  
  coefficients <- result$models$coefficients$asDF
  groups <- levels(pathjdata$groups_a)
  
  testthat::expect_equal(sort(unique(as.character(coefficients$lgroup))), groups)
  testthat::expect_equal(as.integer(table(coefficients$lgroup)), rep(2, length(groups)))
  testthat::expect_equal(sort(unique(coefficients$rhs)), c("x1", "y1"))
  testthat::expect_true(all(is.finite(coefficients$est)))
  testthat::expect_true(all(is.finite(coefficients$se)))
})
