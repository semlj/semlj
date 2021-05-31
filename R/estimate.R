## This class takes care of estimating the model and return the results. It inherit from Syntax, and defines the same tables
## defined by Syntax, but it fill them with the results. It also adds a few tables not defined in Syntax

## Any function that produce a table goes here

Estimate <- R6::R6Class("Estimate",
                        inherit = Syntax,
                        cloneable=FALSE,
                        class=FALSE,
                        list(
                          model=NULL,
                          tab_fit=NULL,
                          tab_fitindices=NULL,
                          tab_constfit=NULL,
                          tab_addfit=NULL,
                          tab_r2=NULL,
                          tab_mardia=NULL,
                          tab_covcorr=NULL,
                          tab_modInd=NULL,
                          ciwidth=NULL,
                          initialize=function(options,datamatic) {
                            super$initialize(options=options,datamatic=datamatic)
                            self$ciwidth <- options$ciWidth/100
                          },
                          estimate=function(data) {
                            
                            ## prepare the options based on Syntax definitions
                            
                            lavoptions <- list(model = private$.lav_structure, 
                                             data = data,
                                             se=self$options$se,
                                             bootstrap=self$options$bootN,
                                             estimator=self$options$estimator
                                             
                            )

                            if (is.something(self$multigroup)) {
                              lavoptions[["group"]] <- self$multigroup$var
                              lavoptions[["group.label"]] <- self$multigroup$levels
                              # TO-DO: test eq_-options
                              nmeOpt = names(self$options);
                              nmeEql = nmeOpt[grep("^eq_", nmeOpt)];
#                             lavoptions[["group.equal"]] <- gsub("eq_", "", nmeEql[unlist(mget(nmeEql, envir=self$options))]));
                            }

                            if (self$options$estimator == "ML") {
                              lavoptions[["likelihood"]] <- self$options$likelihood
                            }

                            # TO-DO: add further model options
                            # iterate through the names and check for matches with lavOpt, update lavOpt if matching
                            # should be the following variables: auto.cov.lv.x auto.cov.y auto.delta auto.efa auto.fix.single auto.th auto.var
                            #   bootstrap estimator fixed.x int.lv.fixed int.ov.fixed meanstructure mimic orthogonal se std.ov
                            
                            ## estimate the models
                            results <- try_hard({do.call(lavaan::lavaan, lavoptions) })
                            
                            ## check if warnings or errors are produced
                            self$warnings <- list(topic="info",message=results$warning)
                            self$errors <- results$error
                            
                            if (is.something(self$errors))
                                return(self$errors)
                            
                            ## ask for the paramters estimates
                            self$model <- results$obj
                            .lav_params <- lavaan::parameterestimates(
                              self$model,
                              ci = self$options$ci,
                              standardized = T,
                              level = self$ciwidth,
                              boot.ci.type = self$options$bootci
                            )
                            
                            ## we need some info initialized by Syntax regarding the parameters properties
                            .lav_structure <- private$.lav_structure
                             sel <- grep("==|<|>",.lav_structure$op,invert = T)
                            .lav_structure <- .lav_structure[sel,]
                            ## make some change to render the results
                            .lav_params$free <- (.lav_structure$free>0)
                            
                            ## collect regression coefficients table
                            self$tab_coefficients <- .lav_params[.lav_params$op == "~",]

                            ## collect loadings table
                            self$tab_loadings <- .lav_params[.lav_params$op == "=~",]
                            
                            ## collect variances and covariances table
                            self$tab_covariances <- .lav_params[.lav_params$op == "~~",]

                            ## collect defined parameters table
                            self$tab_defined <- .lav_params[.lav_params$op == ":=",]
                            if (nrow(self$tab_defined) == 0) self$tab_defined <- NULL
                            
                            tab <- self$tab_covariances
                            ### collect intercepts
                            self$tab_intercepts <- .lav_params[.lav_params$op == "~1",]
                            if (nrow(self$tab_intercepts) == 0) self$tab_intercepts <- NULL
                            
                            #### fit tests ###
                            alist <- list()
                            ff <- lavaan::fitmeasures(self$model)
                            alist <- list()
                            if (ff[["df"]] > 0)
                              alist[[1]] <- list(label="User Model",chisq=ff[["chisq"]],df=ff[["df"]],pvalue=ff[["pvalue"]])
                            try(alist[[length(alist) + 1]] <- list(label="Baseline Model",chisq=ff[["baseline.chisq"]],df=ff[["baseline.df"]],pvalue=ff[["baseline.pvalue"]]))
                            self$tab_fitindices <- as.list(ff)
                            
                            self$tab_fit <- alist
                            
                            # fit indices
                            alist <- list()
                            alist[[length(alist) + 1]] <- c(info="Estimation Method",value=self$model@Options$estimator)
                            alist[[length(alist) + 1]] <- c(info="Number of observations",value=lavaan::lavInspect(self$model,"ntotal")) 
                            alist[[length(alist) + 1]] <- c(info="Free parameters",value=self$model@Fit@npar)
                            alist[[length(alist) + 1]] <- c(info="Converged",value=self$model@Fit@converged) 
                            alist[[length(alist) + 1]] <- c(info="",value="")
                            try(alist[[length(alist) + 1]] <- c(info="Loglikelihood user model",value=round(ff[["logl"]],digits=3) ))
                            try(alist[[length(alist) + 1]] <- c(info="Loglikelihood unrestricted model",value=round(ff[["unrestricted.logl"]],digits=3)))
                            alist[[length(alist) + 1]] <- c(info="",value="")
                            
                            self$tab_info <- alist
                            
                            # checking constraints
                            if (is.something(self$tab_constfit)) {
                              check <- sapply(self$tab_constfit$op,function(con) length(grep("<|>",con))>0,simplify = T)
                              if (any(check)) {
                                self$warnings <- list(topic="constraints",message=WARNS[["scoreineq"]])
                              } else {
                                tab <- lavaan::lavTestScore(self$model,
                                                            univariate = self$options$scoretest,
                                                            cumulative = self$options$cumscoretest)
                                
                                if (self$options$scoretest) {
                                  names(tab$uni) <- c("lhs","op","rhs","chisq","df","pvalue")
                                  self$tab_constfit <- tab$uni
                                  self$tab_constfit$type="Univariate"
                                }
                                if (self$options$cumscoretest) {
                                  names(tab$cumulative) <- c("lhs","op","rhs","chisq","df","pvalue")
                                  tab$cumulative$type <- "Cumulative"
                                  self$tab_constfit <- rbind(self$tab_constfit,tab$cumulative)
                                }
                                self$tab_fit[[length(self$tab_fit)+1]] <- list(label="Constraints Score Test",
                                                                     chisq=tab$test$X2,
                                                                     df=tab$test$df,
                                                                     pvalue=tab$test$p.value)
                                
                              }
                            } # end of checking constraints

                            # additional fit measures
                            if (self$options$outputAdditionalFitMeasures) {
                              alist <- list()
                              # (1) Model test baseline model
                              alist[[length(alist) + 1]]  <- c(name = "Minimum Function Test Statistic",            statistics = ff[["fmin"]]);
#                             alist[[length(alist) + 1]]  <- c(name = "χ²",                                         statistics = ff[["chisq"]]);
#                             alist[[length(alist) + 1]]  <- c(name = "Degrees of freedom",                         statistics = ff[["df"]]);
#                             alist[[length(alist) + 1]]  <- c(name = "p",                                          statistics = ff[["pvalue"]]);

                              # (2) User model versus baseline model
                              alist[[length(alist) + 1]]  <- c(name = "Comparative Fit Index (CFI)",                statistics = ff[["cfi"]]);
                              alist[[length(alist) + 1]]  <- c(name = "Tucker-Lewis Index (TLI)",                   statistics = ff[["tli"]]);
                              alist[[length(alist) + 1]]  <- c(name = "Bentler-Bonett Non-normed Fit Index (NNFI)", statistics = ff[["nnfi"]]);
                              alist[[length(alist) + 1]]  <- c(name = "Bentler-Bonett Normed Fit Index (NFI)",      statistics = ff[["nfi"]]);
                              alist[[length(alist) + 1]]  <- c(name = "Parsimony Normed Fit Index (PNFI)",          statistics = ff[["pnfi"]]);
                              alist[[length(alist) + 1]]  <- c(name = "Bollen's Relative Fit Index (RFI)",          statistics = ff[["rfi"]]);
                              alist[[length(alist) + 1]]  <- c(name = "Bollen's Incremental Fit Index (IFI)",       statistics = ff[["ifi"]]);
                              alist[[length(alist) + 1]]  <- c(name = "Relative Noncentrality Index (RNI)",         statistics = ff[["rni"]]);

                              # (3) Loglikelihood and Information Criteria
                              alist[[length(alist) + 1]]  <- c(name = "Loglikelihood user model (H0)",              statistics = ff[["logl"]]);
                              alist[[length(alist) + 1]]  <- c(name = "Loglikelihood unrestricted model (H1)",      statistics = ff[["unrestricted.logl"]]);
                              alist[[length(alist) + 1]]  <- c(name = "Number of free parameters",                  statistics = ff[["npar"]]);
                              alist[[length(alist) + 1]]  <- c(name = "Akaike Information Criterion (AIC)",         statistics = ff[["aic"]]);
                              alist[[length(alist) + 1]]  <- c(name = "Bayesian Information Criterion (BIC)",       statistics = ff[["bic"]]);
                              alist[[length(alist) + 1]]  <- c(name = "Sample-size adjusted Bayesian (BIC)",        statistics = ff[["bic2"]]);
               
                              # (4) Root Mean Square Error of Approximation
                              alist[[length(alist) + 1]]  <- c(name = "Root Mean Square Error of Approximation",    statistics = ff[["rmsea"]]);
                              alist[[length(alist) + 1]]  <- c(name = "lower boundary of the 90% CI",               statistics = ff[["rmsea.ci.lower"]]);
                              alist[[length(alist) + 1]]  <- c(name = "upper boundary of the 90% CI",               statistics = ff[["rmsea.ci.upper"]]);
                              alist[[length(alist) + 1]]  <- c(name = "p-value RMSEA <= 0.05",                      statistics = ff[["rmsea.pvalue"]]);

                              # (5) Standardized Root Mean Square Residual
                              alist[[length(alist) + 1]]  <- c(name = "Root Mean Square Residual",                  statistics = ff[["rmr"]]);
                              alist[[length(alist) + 1]]  <- c(name = "Root Mean Square Residual (no mean)",        statistics = ff[["rmr_nomean"]]);
                              alist[[length(alist) + 1]]  <- c(name = "Standardized Root Mean Square Residual",     statistics = ff[["srmr"]]);
                 
                              # (6) Other Fit Indices
                              alist[[length(alist) + 1]]  <- c(name = "Hoelter Critical N (CN), α=0.05",            statistics = ff[["cn_05"]]);
                              alist[[length(alist) + 1]]  <- c(name = "Hoelter Critical N (CN), α=0.01",            statistics = ff[["cn_01"]]);
                              alist[[length(alist) + 1]]  <- c(name = "Goodness of Fit Index (GFI)",                statistics = ff[["gfi"]]);
                              alist[[length(alist) + 1]]  <- c(name = "Parsimony Goodness of Fit Index (GFI)",      statistics = ff[["pgfi"]]);
                              alist[[length(alist) + 1]]  <- c(name = "McDonald Fit Index (MFI)",                   statistics = ff[["mfi"]]);

                              alist[[length(alist) + 1]]  <- c(name = "",                                           statsitics = "");
                              # other measures that are not implemented yet: "srmr_bentler", "srmr_bentler_nomean", "crmr", "crmr_nomean", "srmr_mplus", "srmr_mplus_nomean"
                              #                                              "agfi", "ecvi"

                              self$tab_addFit <- alist
                            }

                            # R²
                            if (self$options$outputRSquared) {
                              RSqEst = lavaan::parameterEstimates(fit, se = FALSE, zstat = FALSE, pvalue = FALSE, ci = FALSE, rsquare=TRUE);
                              RSqEst = RSqEst[RSqEst$op == "r2", 3:4];
                              self$tab_r2 <- NULL
                              if (nrow(self$tab_r2) == 0) self$tab_r2 < -NULL
                            }

                            # Mardia's coefficients
                            if (self$options$outputMardiasCoefficients) {
                              mrdSkw = semTools::mardiaSkew(self$data[,lavaan::lavaanNames(fit)]);
                              mrdKrt = semTools::mardiaKurtosis(self$data[,lavaan::lavaanNames(fit)]);
                              alist = list();
                              alist[[length(alist) + 1]]  <- c(name = "Skewness", coeff=mrdSkw[[1]], as.list(mrdSkw[2:4]));
                              alist[[length(alist) + 1]]  <- c(name = "Kurtosis", coeff=mrdKrt[[1]], as.list(mrdKrt[2:3]));
                              self$tab_mardia <- alist;
                            }

                            # covariances and correlations
                            if (self$options$outputObservedCovariances || self$options$outputImpliedCovariances || self$options$outputResidualCovariances) {
                              self$tab_covcorr <- NULL
                              if (nrow(self$tab_covcorr) == 0) self$tab_covcorr <- NULL
                            }

                            # modification indices
                            if (self$options$outputModificationIndices) {
                              modRes = lavaan::modificationIndices(fit);
                              if (self$options$miHideLow) {
                                modRes = modRes[modRes$mi > self$options$miThreshold, ];
                              }
                              modRes = modRes[order(modRes$mi, decreasing=TRUE), ];
                              if (nrow(modRes) > 0) {
                                alist = list();
                                for (r in seq(nrow(modRes))) { alist[[r]] <- as.list(modRes[r, !is.na(modRes[r, ])]) }
                              } else {
                                alist[[1]] <- list(lhs='No modification indices above threshold.');
                              }
                              self$tab_modInd <- alist;
                            }
                            
                            ginfo("Estimation is done...")
                          } # end of private function estimate

              ) # end of private
)  # end of class

