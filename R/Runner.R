## This class takes care of estimating the models and return the results. It inherit from Initer, and defines the same tables
## defined by Initer, but it fills them with the results. It also adds a few tables not defined in Initer
## Any function that produce a table goes here

Runner <- R6::R6Class("Runner",
    inherit = Initer,
    cloneable = FALSE,
    class = TRUE,
    public = list(
        model = NULL,
        tab_mardia = NULL,
        tab_htmt = NULL,
        rownames = NULL,
        estimate = function(data) {
            ## save rownames for predicted with missing
            self$rownames <- rownames(data)
            ## prepare the options based on Syntax definitions
            ## NOTE: for some reasons, when `<-` is present in the model fixed.x passed by lavaanify()
            ##       is not considered by lavaan(). We passed again and it works


            lavoptions <- list(
                model = private$.lav_structure,
                data = data,
                estimator = self$options$estimator,
                likelihood = self$options$likelihood,
                std.ov = self$options$std_ov,
                bootstrap = self$options$bootN,
                fixed.x = self$options$cov_x,
                missing = self$options$missing,
                rotation = self$options$rotation,
                rotation.args = list(
                    orthogonal = self$options$orthogonal,
                    geomin.epsilon = self$options$geomin.epsilon,
                    orthomax.gamma = self$options$orthomax.gamma,
                    oblimin.gamma = self$options$oblimin.gamma
                )
            )

            if (self$options$se != "auto") {
                lavoptions[["se"]] <- self$options$se
            }

            if (self$options$se == "boot" && self$option("parallel")) {
                method <- "multicore"
                if (.Platform$OS.type == "windows") {
                    method <- "snow"
                }
                lavoptions[["parallel"]] <- method
            }


            if (is.something(self$multigroup)) {
                lavoptions[["group"]] <- self$multigroup$var
                lavoptions[["group.label"]] <- self$multigroup$levels
                # TO-DO: test eq_-options
                # this is dealt with in syntax.R
            }

            if (is.something(self$cluster)) {
                lavoptions[["cluster"]] <- self$cluster
                lavoptions[["h1"]] <- TRUE
            }

            ## estimate the models
            jinfo("Estimating the model...")
            results <- try_hard({
                do.call(lavaan::lavaan, lavoptions)
            })
            jinfo("Estimating the model...done")

            ## check if warnings or errors are produced
            self$warning <- list(topic = "info", message = results$warning)
            ## if it fails here, we should stop
            error <- results$error
            if (length(grep("subscript out of bound", error, fixed = T)) > 0) {
                error <- "Model cannot be estimated. Please refine the model or choose different options."
            }
            self$error <- list(topic = "info", message = error, final = TRUE)

            self$model <- results$obj

            ### we need the data for mardia's, so we save them here

            if (self$option("outputMardiasCoefficients")) {
                vars <- setdiff(self$datamatic$observed, self$datamatic$ordered)

                if (length(vars) > 0) {
                    results <- try_hard({
                        s <- semTools::mardiaSkew(data[, vars], "pairwise.complete.obs")
                        k <- semTools::mardiaKurtosis(data[, vars], "pairwise.complete.obs")
                        self$tab_mardia <- list(
                            list(name = "Skewness", coeff = s[[1]], z = "", chi = s[[2]], df = s[[3]], p = s[[4]]),
                            list(name = "Kurtosis", coeff = k[[1]], z = k[[2]], chi = "", df = "", p = k[[3]])
                        )
                    })
                    if (!isFALSE(results$error)) {
                        self$warning <- list(topic = "additional_mardia", message = "Mardia's coefficients not available.")
                    }
                    if (!isFALSE(results$warning)) {
                        self$warning <- list(topic = "additional_mardia", message = results$earning)
                    }
                }
            }


            ### we need the data for htmt, so we save them here

            if (self$options$htmt) {
                results <- try_hard(semTools::htmt(
                    model = self$user_syntax,
                    data = data,
                    missing = "default", ordered = self$datamatic$ordered
                ))
                if (!isFALSE(results$error)) {
                    self$warning <- list(topic = "additional_htmt", message = "HTMT indices not available for this model.")
                }
                if (!isFALSE(results$warning)) {
                    self$warning <- list(topic = "additional_htmt", message = results$warning)
                }

                self$tab_htmt <- as.data.frame(results$obj)
            }
        },
        par_table = function() {
            if (is.null(private$.par_table)) {
                private$.get_par_table()
            }

            return(private$.par_table)
        },
        fit_measures = function() {
            if (is.null(private$.fit_measures)) {
                private$.get_fit_measures()
            }

            return(private$.fit_measures)
        },
        run_info = function() {
            alist <- list()
            alist[[length(alist) + 1]] <- c(info = "Estimation Method", value = self$model@Options$estimator)
            alist[[length(alist) + 1]] <- c(info = "Optimization Method", value = toupper(self$model@Options$optim.method))
            alist[[length(alist) + 1]] <- c(info = "Number of observations", value = lavaan::lavInspect(self$model, "ntotal"))
            alist[[length(alist) + 1]] <- c(info = "Free parameters", value = self$model@Fit@npar)
            alist[[length(alist) + 1]] <- c(info = "Standard errors", value = INFO_SE[[self$model@Options$se]])
            alist[[length(alist) + 1]] <- c(info = "Scaled test", value = private$.get_test_info())
            alist[[length(alist) + 1]] <- c(info = "Converged", value = self$model@Fit@converged)
            alist[[length(alist) + 1]] <- c(info = "Iterations", value = self$model@optim$iterations)
            alist[[length(alist) + 1]] <- c(info = "", value = "")

            return(alist)
        },
        run_fit_main = function() {
            fit <- self$fit_measures()
            if (hasName(fit, "logl")) logl <- fit[["logl"]] else logl <- ""
            if (hasName(fit, "logl.restricted")) logl <- fit[["logl"]] else logl <- ""

            tab <- list()
            tab[[1]] <- list(
                label = "User Model",
                chisq = fit[["chisq"]],
                df = fit[["df"]],
                pvalue = fit[["pvalue"]],
                logl = logl
            )
            if (hasName(fit, "baseline.chisq")) {
                tab[[length(tab) + 1]] <- list(
                    label = "Baseline Model",
                    chisq = fit[["baseline.chisq"]],
                    df = fit[["baseline.df"]],
                    pvalue = fit[["baseline.pvalue"]]
                )
            }
            if (hasName(fit, "chisq.scaled")) {
                tab[[length(tab) + 1]] <- list(
                    label = "Scaled User",
                    chisq = fit[["chisq.scaled"]],
                    df = fit[["df.scaled"]],
                    pvalue = fit[["pvalue.scaled"]]
                )
            }

            if (hasName(fit, "baseline.chisq.scaled")) {
                tab[[length(tab) + 1]] <- list(
                    label = "Scaled Baseline",
                    chisq = fit[["baseline.chisq.scaled"]],
                    df = fit[["baseline.df.scaled"]],
                    pvalue = fit[["baseline.pvalue.scaled"]]
                )
            }
            return(tab)
        },
        run_fit_constraints = function() {
            # checking constraints
            itab <- self$init_fit_constraints()
            if (is.null(itab)) {
                return()
            }

            op <- itab$op
            check <- sapply(op, function(con) length(grep("<|>", con)) > 0, simplify = T)
            if (any(check)) {
                warning(WARNS[["scoreineq"]])
                return()
            }
            tab <- NULL
            rtab <- lavaan::lavTestScore(self$model,
                univariate = self$options$scoretest,
                cumulative = self$options$cumscoretest
            )

            if (self$options$scoretest) {
                names(rtab$uni) <- c("lhs", "op", "rhs", "chisq", "df", "pvalue")
                tab <- rtab$uni
                tab$type <- "Univariate"
            }
            if (self$options$cumscoretest) {
                names(rtab$cumulative) <- c("lhs", "op", "rhs", "chisq", "df", "pvalue")
                rtab$cumulative$type <- "Cumulative"
                tab <- rbind(tab, rtab$cumulative)
            }
            tab$lhs <- sapply(tab$lhs, function(st) ifelse(grepl("^.p\\d+\\.$", st), gsub(".", "", st, fixed = T), st))
            tab$rhs <- sapply(tab$rhs, function(st) ifelse(grepl("^.p\\d+\\.$", st), gsub(".", "", st, fixed = T), st))
            ### here we add the total test ###
            ttab <- rtab$test
            ttab$test <- NULL
            names(ttab) <- c("chisq", "df", "pvalue")
            ttab$lhs <- ttab$op <- ttab$rhs <- ""
            ttab$type <- "Total"
            tab <- rbind(tab, ttab)

            return(tab)
        },
        run_fit_indices = function() {
            fi <- self$fit_measures()

            tab <- list(list(
                srmr = fi$srmr,
                rmsea = fi$rmsea,
                rmsea.ci.lower = fi$rmsea.ci.lower,
                rmsea.ci.upper = fi$rmsea.ci.upper,
                rmsea.pvalue = fi$rmsea.pvalue
            ))

            if (hasName(fi, "rmsea.robust")) {
                tab[[2]] <- list(
                    srmr = fi$srmr_bentler,
                    rmsea = fi$rmsea.robust,
                    rmsea.ci.lower = fi$rmsea.ci.lower.robust,
                    rmsea.ci.upper = fi$rmsea.ci.upper.robust,
                    rmsea.pvalue = fi$rmsea.pvalue.robust
                )
            }
            if (utils::hasName(fi, "rmsea.scaled")) {
                tab[[length(tab) + 1]] <- list(
                    srmr = fi$srmr_bentler,
                    rmsea = fi$rmsea.scaled,
                    rmsea.ci.lower = fi$rmsea.ci.lower.scaled,
                    rmsea.ci.upper = fi$rmsea.ci.upper.scaled,
                    rmsea.pvalue = fi$rmsea.pvalue.scaled
                )
            }
            return(tab)
        },
        run_fit_modelbaseline = function() {
            fun <- function(aname) {
                item <- list(statistics = fit[[aname]], scaled = NA, robust = NA)
                scaled <- paste0(aname, ".scaled")
                if (scaled %in% names(fit)) {
                    item[["scaled"]] <- fit[[scaled]]
                }
                robust <- paste0(aname, ".robust")
                if (robust %in% names(fit)) {
                    item[["robust"]] <- fit[[robust]]
                }
                return(item)
            }
            fit <- self$fit_measures()
            alist <- lapply(names(INFO_INDICES), function(x) {
                item <- fun(x)
                item$name <- INFO_INDICES[[x]]
                item
            })
            if (self$optionValue("estimator") %in% ROBUST_ESTIM) {
                test <- all(sapply(alist, function(x) is.na(x$robust)))
                if (test) {
                    warning("Robust indices cannot be computed for this model/estimator combination. The scaled stistics can be used instead.")
                }
            }
            alist
        },
        run_fit_moreindices = function() {
            fit <- self$fit_measures()
            alist <- lapply(names(INFO_MOREINDICES), function(x) {
                item <- list()
                if (hasName(fit, x)) {
                    item$statistics <- fit[[x]]
                }
                if (is.something(item)) {
                    item$name <- INFO_MOREINDICES[[x]]
                }
                item
            })
            return(alist)
        },
        run_fit_rsquared = function() {
            # R²
            if (self$option("r2", "none")) {
                return()
            }

            results <- try_hard(lavaan::parameterEstimates(self$model,
                se = FALSE,
                zstat = FALSE,
                pvalue = FALSE,
                ci = FALSE,
                rsquare = TRUE
            ))
            if (!isFALSE(results$error)) {
                warning("R-squared cannot be computed for this model")
                return()
            }
            ### here we can compute them
            RSqEst <- results$obj
            RSqEst <- RSqEst[RSqEst$op == "r2", ]
            if (self$option("r2", "endo")) {
                endo <- private$.lav_structure$lhs[private$.lav_structure$op == "~"]
                RSqEst <- RSqEst[RSqEst$lhs %in% endo, ]
            }

            ### for some reasons, multigroup r2 are identified by block and not group
            RSqEst$group <- RSqEst$block
            if (nrow(RSqEst) > 0) {
                RSqEst <- private$.fix_groups_labels(RSqEst)
                return(RSqEst)
            }
            warning("R-squared not completed")
            return(NULL)
        },
        run_fit_icc = function() {
            tab <- lavaan::lavInspect(self$model, "icc")
            tab <- private$.make_matrix_table(tab, transform)
            labs <- row.names(tab)
            names(tab) <- "est"

            if (is.something(self$multigroup)) {
                labs <- stringr::str_split(labs, "\\.")
                tab$lgroup <- sapply(labs, function(x) x[[1]])
                labs <- sapply(labs, function(x) paste0(x[-1], collapse = "."))
            }
            tab$rhs <- labs
            return(tab)
        },
        run_models_coefficients = function() {
            tab <- self$par_table()
            tab <- tab[tab$op == "~", ]
            if (nrow(tab) == 0) tab <- NULL
            return(tab)
        },
        run_models_loadings = function() {
            tab <- self$par_table()
            tab <- tab[tab$op == "=~", ]
            if (nrow(tab) == 0) tab <- NULL
            return(tab)
        },
        run_models_composites = function() {
            tab <- self$par_table()
            tab <- tab[tab$op == "<~", ]
            if (nrow(tab) == 0) tab <- NULL
            return(tab)
        },
        run_models_covariances = function() {
            tab <- self$par_table()
            tab <- tab[tab$op == "~~", ]
            if (nrow(tab) == 0) tab <- NULL
            return(tab)
        },
        run_models_intercepts = function() {
            tab <- self$par_table()
            tab <- tab[tab$op == "~1", ]
            if (nrow(tab) == 0) tab <- NULL
            return(tab)
        },
        run_models_thresholds = function() {
            tab <- self$par_table()
            tab <- tab[tab$op == "|", ]
            if (nrow(tab) == 0) tab <- NULL
            return(tab)
        },
        run_models_mlmeans = function() {
            if (!is.something(self$cluster)) {
                return(NULL)
            }

            tab <- lavaan::inspect(self$model, "fitted")
            tabs <- lapply(tab, function(atab) atab$mean)
            tab <- private$.make_matrix_table(tabs, transform)
            names(tab) <- "est"
            return(tab)
        },
        run_models_defined = function() {
            tab <- self$par_table()
            tab <- tab[tab$op == ":=", ]
            if (nrow(tab) == 0) tab <- NULL
            return(tab)
        },
        run_additional_reliability = function() {
            tab <- list()
            results <- try_hard(semTools::reliability(self$model))
            self$warning <- list(topic = "additional_reliability", message = results$warning)
            self$warning <- list(topic = "additional_reliability", message = results$error)
            if (isFALSE(results$error)) {
                tab <- private$.make_matrix_table(results$obj, fun = t)
            } else {
                self$warning <- list(topic = "additional_reliability", message = "Reliabilities not available for this model.")
            }

            return(tab)
        },
        run_additional_htmt = function() {
            self$tab_htmt
        },
        run_additional_mardia = function() {
            self$tab_mardia
        },
        run_covariances_observed = function() {
            mat <- lavaan::inspect(self$model, "observed")
            if (self$options$.caller == "gui") {
                tab <- private$.make_covcor_table(mat)
                tab$type <- "observed"
            } else {
                if ("cov" %in% names(mat)) mat <- list("1" = mat)

                tab <- lapply(mat, function(x) {
                    one <- private$.make_covcor_table(x)
                    one$variable <- names(one)
                    one
                })
            }
            return(tab)
        },
        run_covariances_implied = function() {
            mat <- lavaan::inspect(self$model, "fitted")
            if (self$options$.caller == "gui") {
                tab <- private$.make_covcor_table(mat)
                tab$type <- "observed"
            } else {
                if ("cov" %in% names(mat)) mat <- list("1" = mat)

                tab <- lapply(mat, function(x) {
                    one <- private$.make_covcor_table(x)
                    one$variable <- names(one)
                    one
                })
            }
            return(tab)
        },
        run_covariances_residual = function() {
            # calculates the difference between observed and fitted correlations since
            # using cov2cor(resCov) almost invariably ends in having 0 or NA entries in the
            # main diagonal (given the small size of the residuals)
            obj1 <- lavaan::inspect(self$model, "observed")
            obj2 <- lavaan::inspect(self$model, "fitted")

            if (self$options$.caller == "gui") {
                tab <- private$.make_covcor_diff(obj1, obj2)
                tab$type <- "residual"
                return(tab)
            } else {
                if ("cov" %in% names(obj1)) {
                    obj1 <- list("1" = obj1)
                    obj2 <- list("1" = obj2)
                }

                tab <- lapply(seq_along(obj1), function(i) {
                    one <- private$.make_covcor_diff(obj1[[i]], obj2[[i]])
                    one$variable <- names(one)
                    one
                })
            }
            return(tab)
        },
        run_covariances_combined = function() {
            tab1 <- tab2 <- tab3 <- NULL
            if (self$option("outputObservedCovariances")) {
                tab1 <- self$run_covariances_observed()
            }
            if (self$option("outputImpliedCovariances")) {
                tab2 <- self$run_covariances_implied()
            }
            if (self$option("outputResidualCovariances")) {
                tab3 <- self$run_covariances_residual()
            }

            rbind(tab1, tab2, tab3)
        },
        run_covariances_latent = function() {
            obj <- lavaan::lavInspect(self$model, "cov.lv")

            if (self$options$.caller == "gui") {
                tab <- private$.make_covcor_table(obj, "none")

                tab
            } else {
                if (!inherits(obj, "list")) obj <- list("1" = obj)
                lapply(obj, function(x) {
                    x[upper.tri(x, diag = FALSE)] <- x[lower.tri(x, diag = FALSE)]
                    x <- as.data.frame(x)
                    x$variable <- names(x)
                    x
                })
            }
        },
        run_modification_indices = function() {
            modRes <- lavaan::modificationIndices(self$model)
            if (self$options$miHideLow) {
                modRes <- modRes[modRes$mi > self$options$miThreshold, ]
            }
            if (nrow(modRes) > 0) {
                modRes <- modRes[order(modRes$mi, decreasing = TRUE), ]
                modRes <- private$.fix_groups_labels(modRes)
                tab <- modRes
            } else {
                warning("No modification indices above threshold")
                tab <- NULL
            }
            tab
        },
        run_lavaanoptions = function() {
            alist <- self$model@Options
            alist[26] <- NULL # to ugly to show
            results <- try_hard({
                alist[sapply(alist, is.null)] <- "NULL"
                alist[sapply(alist, function(x) length(x) == 0)] <- "EMPTY"
                alist <- sapply(alist, function(x) {
                    if (length(x) > 1) {
                        paste(x, collapse = ",")
                    } else {
                        x
                    }
                })
                blist <- names(alist)
                amat <- as.data.frame(matrix(unlist(alist), ncol = 3))
                names(amat) <- c("value1", "value2", "value3")
                bmat <- as.data.frame(matrix(blist, ncol = 3))
                names(bmat) <- c("opt1", "opt2", "opt3")
                tab <- as.data.frame(cbind(bmat, amat))
                tab
            })
            results$obj
        },
        savePredRes = function(results, data) {
            private$.saveDv(results, data)
            private$.saveLv(results)
            private$.saveOv(results)
        } ## end of savePredRes
    ), # end of public function estimate

    private = list(
        .fit_measures = NULL,
        .par_table = NULL,
        .get_fit_measures = function() {
            results <- try_hard(as.list(lavaan::fitmeasures(self$model)))

            if (!isFALSE(results$warning)) {
                warning(results$warning)
            }

            if (!isFALSE(results$error)) {
                err <- gsub("subscript out of bounds", "Results not available, please revise the model", results$error, fixed = T)
                warning(err)
                return()
            }

            private$.fit_measures <- results$obj
        },
        .get_par_table = function() {
            results <- try_hard(
                lavaan::parameterestimates(
                    self$model,
                    ci = self$options$est_ci,
                    standardized = T,
                    level = self$options$ci_width / 100,
                    boot.ci.type = self$options$bootci
                )
            )
            self$warning <- list(topic = "info", message = results$warning)
            self$error <- list(topic = "info", message = results$error)
            private$.par_table <- results$obj
            if (is.null(private$.par_table)) private$.par_table <- FALSE
            userlabel <- grep("^\\p\\d+$", private$.par_table$label, invert = T)
            if (length(userlabel) > 0) {
                ilabel <- paste("(", private$.lav_structure$plabel[userlabel], ")")
                private$.par_table$label[userlabel] <- paste(private$.par_table$label[userlabel], ilabel)
            }
            z <- lavaan::standardizedSolution(self$model,
                type = "std.all",
                se = TRUE,
                zstat = FALSE,
                pvalue = FALSE,
                ci = TRUE,
                level = self$options$ci_width / 100
            )

            private$.par_table$std.ci.lower <- z$ci.lower
            private$.par_table$std.ci.upper <- z$ci.upper
        },
        .get_test_info = function() {
            if (!"test" %in% methods::slotNames(self$model)) {
                return("")
            }

            tests <- names(self$model@test)

            if (length(tests) < 2) {
                return("None")
            }

            return(INFO_TEST[[tests[[2]]]])
        },
        .saveDv = function(results, data) {
            if (!self$option("preds_dv")) {
                return()
            }

            if (!(results$preds_dv$isNotFilled())) {
                return()
            }

            jinfo("Trying saving dv predicted")

            if (is.something(self$multigroup)) {
                self$warning <- list(topic = "info", message = "Dependent variables predicted values not implemented for multigroup analysis")
                return()
            }

            .compute <- function() {
                #### this comes from
                ###  https://github.com/mjderooij/SEMpredict/blob/main/predicty.lavaan.R
                try_hard({
                    Sxx <- lavaan::fitted(self$model)$cov[xnames, xnames]
                    Sxy <- lavaan::fitted(self$model)$cov[xnames, ynames]
                    mx <- lavaan::fitted(self$model)$mean[xnames]
                    my <- lavaan::fitted(self$model)$mean[ynames]

                    #
                    Xtest <- as.matrix(data[, xnames])
                    Xtest <- scale(Xtest, center = mx, scale = FALSE)
                    yhat <- matrix(my, nrow = nrow(Xtest), ncol = length(ynames), byrow = TRUE) + Xtest %*% solve(Sxx) %*% Sxy
                    colnames(yhat) <- paste0("PRDV_", ynames)
                    data.frame(yhat, row.names = rownames(data))
                })
            }

            jinfo("saving dv predicted")


            .names <- private$.observed_vars()
            xnames <- .names[[1]]
            ynames <- .names[[2]]

            predsobj <- .compute()
            predsdata <- predsobj$obj
            if (!isFALSE(predsobj$error)) {
                self$warning <- list(topic = "info", message = "Dependent variables predicted values cannot be computed for this  model")
                return()
            }

            results$preds_dv$set(
                1:ncol(predsdata),
                names(predsdata),
                rep("DV Predicted", ncol(predsdata)),
                rep("continuous", ncol(predsdata))
            )
            results$preds_dv$setRowNums(self$rownames)
            rownames(predsdata) <- self$rownames
            results$preds_dv$setValues(predsdata)
            self$warning <- list(topic = "info", message = paste("Dependent variables predicted values saved in the dataset. Varnames:", paste(names(predsdata), collapse = ", ")))
        },
        .saveLv = function(results) {
            if (!self$option("preds_lv")) {
                return()
            }
            if (!results$preds_lv$isNotFilled()) {
                return()
            }

            jinfo("Trying saving lv predicted")

            obj <- try_hard(lavaan::lavPredict(self$model, type = "lv", assemble = TRUE, append.data = T))
            if (!isFALSE(obj$error)) {
                self$warning <- list(topic = "info", message = "Factor scores cannot be computed for this model")
                return()
            }
            predsdata <- as.data.frame(obj$obj)

            if (ncol(predsdata) == 0) {
                self$warning <- list(topic = "info", message = "Factor scores cannot be computed for this model")
                return()
            }

            jinfo("saving lv predicted")

            colnames(predsdata) <- paste0("PRFS_", colnames(predsdata))
            results$preds_lv$set(
                1:ncol(predsdata),
                names(predsdata),
                rep("Factor scores ", ncol(predsdata)),
                rep("continuous", ncol(predsdata))
            )
            results$preds_lv$setRowNums(self$rownames)
            rownames(predsdata) <- self$rownames
            results$preds_lv$setValues(predsdata)
            self$warning <- list(topic = "info", message = paste("Factors scores (latent predicted values) saved in the dataset. Varnames:", paste(names(predsdata), collapse = ", ")))
        },
        .saveOv = function(results) {
            if (!self$option("preds_ov")) {
                return()
            }

            if (!results$preds_ov$isNotFilled()) {
                return()
            }

            jinfo("Trying saving ov predicted")

            obj <- try_hard(lavaan::lavPredict(self$model, type = "ov", assemble = TRUE))

            if (!isFALSE(obj$error)) {
                self$warning <- list(topic = "info", message = "Indicators predicted values cannot be computed for this model")
                return()
            }

            predsdata <- as.data.frame(obj$obj)
            if (ncol(predsdata) == 0) {
                self$warning <- list(topic = "info", message = "Indicators predicted values cannot be computed for this model")
                return()
            }

            if (ncol(predsdata) == 0) {
                self$warning <- list(topic = "info", message = "Indicators predicted values cannot be computed for this model")
                return()
            }

            jinfo("saving ov predicted")
            colnames(predsdata) <- paste0("PRIN_", colnames(predsdata))
            results$preds_ov$set(
                1:ncol(predsdata),
                names(predsdata),
                rep("Indicator predicted", ncol(predsdata)),
                rep("continuous", ncol(predsdata))
            )
            results$preds_ov$setRowNums(self$rownames)
            rownames(predsdata) <- self$rownames
            results$preds_ov$setRowNums(rownames(predsdata))
            results$preds_ov$setValues(predsdata)
            self$warning <- list(topic = "info", message = paste("Indicators predicted values saved in the dataset. Varnames:", paste(names(predsdata), collapse = ", ")))
        }
    ) # end of private
) # end of class
