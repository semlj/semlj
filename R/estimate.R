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
                          tab_testBslModel=NULL,
                          tab_compModelBsl=NULL,
                          tab_infCrit=NULL,
                          tab_Rsquared=NULL,
                          tab_mardia=NULL,
                          tab_modInd=NULL,
#                         tab_addFit=NULL,
#                         tab_covcorr=NULL,
#                         tab_modInd=NULL,
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

                            mark('before SJ');
                            # additional fit measures
                            if (self$options$outputAdditionalFitMeasures) {
                              alist <- list()
                              # (1) Model test baseline model
                             alist[[length(alist) + 1]]  <- list(name = "Minimum Function Test Statistic",            statistics = ff[["fmin"]]);
                             alist[[length(alist) + 1]]  <- list(name = "χ²",                                         statistics = ff[["chisq"]]);
                             alist[[length(alist) + 1]]  <- list(name = "Degrees of freedom",                         statistics = ff[["df"]]);
                             alist[[length(alist) + 1]]  <- list(name = "p",                                          statistics = ff[["pvalue"]]);
                             self$tab_testBslModel<-alist
                             
                              # (2) User model versus baseline model
                              alist<-list()
                              alist[[length(alist) + 1]]  <- list(name = "Comparative Fit Index (CFI)",                statistics = ff[["cfi"]]);
                              alist[[length(alist) + 1]]  <- list(name = "Tucker-Lewis Index (TLI)",                   statistics = ff[["tli"]]);
                              alist[[length(alist) + 1]]  <- list(name = "Bentler-Bonett Non-normed Fit Index (NNFI)", statistics = ff[["nnfi"]]);
                              alist[[length(alist) + 1]]  <- list(name = "Bentler-Bonett Normed Fit Index (NFI)",      statistics = ff[["nfi"]]);
                              alist[[length(alist) + 1]]  <- list(name = "Parsimony Normed Fit Index (PNFI)",          statistics = ff[["pnfi"]]);
                              alist[[length(alist) + 1]]  <- list(name = "Bollen's Relative Fit Index (RFI)",          statistics = ff[["rfi"]]);
                              alist[[length(alist) + 1]]  <- list(name = "Bollen's Incremental Fit Index (IFI)",       statistics = ff[["ifi"]]);
                              alist[[length(alist) + 1]]  <- list(name = "Relative Noncentrality Index (RNI)",         statistics = ff[["rni"]]);
                              self$tab_compModelBsl<-alist
                              
                              
                              # (3) Loglikelihood and Information Criteria
                              alist<-list()
                              alist[[length(alist) + 1]]  <- list(name = "Loglikelihood user model (H0)",              statistics = ff[["logl"]]);
                              alist[[length(alist) + 1]]  <- list(name = "Loglikelihood unrestricted model (H1)",      statistics = ff[["unrestricted.logl"]]);
                              alist[[length(alist) + 1]]  <- list(name = "Number of free parameters",                  statistics = ff[["npar"]]);
                              alist[[length(alist) + 1]]  <- list(name = "Akaike Information Criterion (AIC)",         statistics = ff[["aic"]]);
                              alist[[length(alist) + 1]]  <- list(name = "Bayesian Information Criterion (BIC)",       statistics = ff[["bic"]]);
                              alist[[length(alist) + 1]]  <- list(name = "Sample-size adjusted Bayesian (BIC)",        statistics = ff[["bic2"]]);
                              self$tab_infCrit<-alist
                              
                              # (4) Root Mean Square Error of Approximation
                              alist[[length(alist) + 1]]  <- list(name = "Root Mean Square Error of Approximation",    statistics = ff[["rmsea"]]);
                              alist[[length(alist) + 1]]  <- list(name = "lower boundary of the 90% CI",               statistics = ff[["rmsea.ci.lower"]]);
                              alist[[length(alist) + 1]]  <- list(name = "upper boundary of the 90% CI",               statistics = ff[["rmsea.ci.upper"]]);
                              alist[[length(alist) + 1]]  <- list(name = "p-value RMSEA <= 0.05",                      statistics = ff[["rmsea.pvalue"]]);

                              # (5) Standardized Root Mean Square Residual
                              alist[[length(alist) + 1]]  <- list(name = "Root Mean Square Residual",                  statistics = ff[["rmr"]]);
                              alist[[length(alist) + 1]]  <- list(name = "Root Mean Square Residual (no mean)",        statistics = ff[["rmr_nomean"]]);
                              alist[[length(alist) + 1]]  <- list(name = "Standardized Root Mean Square Residual",     statistics = ff[["srmr"]]);

                              # (6) Other Fit Indices
                              alist[[length(alist) + 1]]  <- list(name = "Hoelter Critical N (CN), α=0.05",            statistics = ff[["cn_05"]]);
                              alist[[length(alist) + 1]]  <- list(name = "Hoelter Critical N (CN), α=0.01",            statistics = ff[["cn_01"]]);
                              alist[[length(alist) + 1]]  <- list(name = "Goodness of Fit Index (GFI)",                statistics = ff[["gfi"]]);
                              alist[[length(alist) + 1]]  <- list(name = "Parsimony Goodness of Fit Index (GFI)",      statistics = ff[["pgfi"]]);
                              alist[[length(alist) + 1]]  <- list(name = "McDonald Fit Index (MFI)",                   statistics = ff[["mfi"]]);

                              alist[[length(alist) + 1]]  <- list(name = "",                                           statistics = "");
                              # other measures that are not implemented yet: "srmr_bentler", "srmr_bentler_nomean", "crmr", "crmr_nomean", "srmr_mplus", "srmr_mplus_nomean"
                              #                                              "agfi", "ecvi"
                              mark('finished addFit');
                            }
                            

                            # R²
                            if (self$options$outputRSquared) {
                              ginfo('begin tab_r2')
                              RSqEst = lavaan::parameterEstimates(self$model, se = FALSE, zstat = FALSE, pvalue = FALSE, ci = FALSE, rsquare=TRUE);
                              mark(RSqEst)
                              RSqEst = RSqEst[RSqEst$op == "r2",];
                              if (nrow(RSqEst) > 0) { 
                                self$tab_Rsquared<- RSqEst;
                              };
                              mark('finished tab_r2');
                              mark(self$tab_r2);
                            }
                            

                            # Mardia's coefficients
                            if (FALSE) {
                            if (self$options$outputMardiasCoefficients) {
                              mark('begin tab_mardia');
                              mrdSkw = semTools::mardiaSkew(self$data[,lavaan::lavaanNames(self$model)]);
                              mrdKrt = semTools::mardiaKurtosis(self$data[,lavaan::lavaanNames(self$model)]);
                              
                              mark('before adding to tab_mardia')
                              self$tab_mardia <- list(list(name = "Skewness", coeff=mrdSkw[[1]], as.list(mrdSkw[2:4])),
                                                      list(name = "Kurtosis", coeff=mrdKrt[[1]], as.list(mrdKrt[2:3])));
                              mark('finished tab_mardia');                            
                            }
                            }

                            # covariances and correlations
#                            if (self$options$outputObservedCovariances || self$options$outputImpliedCovariances || self$options$outputResidualCovariances) {
#                              self$tab_covcorr <- NULL
#                              if (nrow(self$tab_covcorr) == 0) self$tab_covcorr <- NULL
#                            }

                            # modification indices
                            if (self$options$outputModificationIndices) {
                              ginfo('begin tab_modInd');
                              modRes = lavaan::modificationIndices(self$model);
                              if (self$options$miHideLow) {
                                modRes = modRes[modRes$mi > self$options$miThreshold, ];
                              }
                              modRes = modRes[order(modRes$mi, decreasing=TRUE), ];
                              if (nrow(modRes) > 0) {
                                alist = list();
                                for (r in seq(nrow(modRes))) { alist[[r]] <- as.list(modRes[r, !is.na(modRes[r, ])]) }
                              } else {
                                self$warnings <- list(topic="mi",message='No modification indices above threshold.');
                              }
                              self$tab_modInd <- alist;
                              mark('finished tab_modInd');
                            }
                            
                            ginfo("Estimation is done...")
                          } # end of private function estimate

              ) # end of private
)  # end of class

