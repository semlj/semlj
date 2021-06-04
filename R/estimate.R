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
                          tab_compModelBsl=NULL,
                          tab_otherFit=NULL,
                          tab_Rsquared=NULL,
                          tab_mardia=NULL,
                          
                          tab_modInd=NULL,
                          ciwidth=NULL,
                          initialize=function(options,datamatic) {
                            super$initialize(options=options,datamatic=datamatic)
                            self$ciwidth <- options$ciWidth/100
                          },
                          estimate = function(data) {
                            
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
                            results <- try_hard({ do.call(lavaan::lavaan, lavoptions) })
                            
                            ## check if warnings or errors are produced
                            self$warnings <- list(topic="info", message=results$warning)
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
                              # (1) User model versus baseline model
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
                              
                              # (2) Other Fit Indices
                              alist<-list();
                              alist[[length(alist) + 1]]  <- list(name = "Hoelter Critical N (CN), α=0.05",            statistics = ff[["cn_05"]]);
                              alist[[length(alist) + 1]]  <- list(name = "Hoelter Critical N (CN), α=0.01",            statistics = ff[["cn_01"]]);
                              alist[[length(alist) + 1]]  <- list(name = "Goodness of Fit Index (GFI)",                statistics = ff[["gfi"]]);
                              alist[[length(alist) + 1]]  <- list(name = "Parsimony Goodness of Fit Index (GFI)",      statistics = ff[["pgfi"]]);
                              alist[[length(alist) + 1]]  <- list(name = "McDonald Fit Index (MFI)",                   statistics = ff[["mfi"]]);
                              self$tab_otherFit <- alist;
                              
                              # other measures that are not implemented yet: "rmr", "rmr_nomean", "srmr_bentler", "srmr_bentler_nomean", "crmr", "crmr_nomean",
                              #                                              "srmr_mplus", "srmr_mplus_nomean", "agfi", "ecvi"
                              # most of them are some form of Root Mean Squared Residual measures
                              mark('finished addFit');
                            }
                            

                            # R²
                            if (self$options$outputRSquared) {
                              ginfo('begin tab_r2')
                              RSqEst = lavaan::parameterEstimates(self$model, se = FALSE, zstat = FALSE, pvalue = FALSE, ci = FALSE, rsquare=TRUE);
                              RSqEst = RSqEst[RSqEst$op == "r2",];
                              if (nrow(RSqEst) > 0) { 
                                self$tab_Rsquared<- RSqEst;
                              };
                              mark('finished tab_r2');
                            }

                            # Mardia's coefficients
                            if (self$options$outputMardiasCoefficients) {
                              mark('begin tab_mardia');
                              mrdSkw = semTools::mardiaSkew(data[,lavaan::lavaanNames(self$model)]);
                              mrdKrt = semTools::mardiaKurtosis(data[,lavaan::lavaanNames(self$model)]);
                              
                              mark('before adding to tab_mardia')
                              abit<-unlist(mrdSkw[2:4])
                              bbit<-unlist(mrdKrt[2:3])
                              self$tab_mardia <- list(list(name = "Skewness", coeff=mrdSkw[[1]], z="", chi=abit[[1]],df=abit[[2]],p=abit[[3]]),
                                                      list(name = "Kurtosis", coeff=mrdKrt[[1]], z=bbit[[1]],chi="",df="",p=bbit[[2]])
                                                      );
                            }

                            # covariances and correlations
                            if (self$options$outputObservedCovariances || self$options$outputImpliedCovariances || self$options$outputResidualCovariances) {
                              nmeVar = lavaan::lavNames(self$model, 'ov');
                              numVar = length(nmeVar);
                              #self$tab_covcorr <- matrix(, nrow = 0, ncol=numVar + 1, dimnames=list(list(), c('type', nmeVar)));
                              
                              obsCov = lavaan::inspect(self$model, "observed")$cov;
                              fitCov = lavaan::inspect(self$model,   "fitted")$cov;
                              # TO-DO: Implement standardized residuals (cov.z instead of cov)
                              resCov = lavaan::lavResiduals(self$model, type="raw")$cov;
                              
                              if (self$options$outputObservedCovariances) {
                                obsCrr = cov2cor(obsCov);
                                obsCvC = matrix(NA, nrow=numVar, ncol=numVar, dimnames=list(nmeVar, nmeVar));
                                obsCvC[lower.tri(obsCvC, diag=TRUE)]  = obsCov[lower.tri(obsCov, diag=TRUE)];
                                obsCvC[upper.tri(obsCvC, diag=FALSE)] = obsCrr[upper.tri(obsCrr, diag=FALSE)];
                                ## The fill.table() functions accepts data.frames or named vector, not matrix 
                                ## the need names() to return something
                                df<-as.data.frame(obsCvC)
                                df$type="Observed"
                                self$tab_covcorr<-df

                              }
                              if (self$options$outputImpliedCovariances)  { 
                                fitCrr = cov2cor(fitCov);
                                fitCvC = matrix(NA, nrow=numVar, ncol=numVar, dimnames=list(nmeVar, nmeVar));
                                fitCvC[lower.tri(fitCvC, diag=TRUE)]  = fitCov[lower.tri(fitCov, diag=TRUE)];
                                fitCvC[upper.tri(fitCvC, diag=FALSE)] = fitCrr[upper.tri(fitCrr, diag=FALSE)];
                                df<-as.data.frame(fitCvC)
                                df$type="Implied"
                                self$tab_covcorrImplied<-df
                                

                              }
                              if (self$options$outputResidualCovariances) {
                                # calculates the difference between observed and fitted correlations since
                                # using cov2cor(resCov) almost invariably ends in having 0 or NA entries in the
                                # main diagonal (given the small size of the residuals)
                                # TO-DO: check whether the values have to be Fisher z-transformed before subtracting
                                resCrr = cov2cor(obsCov) - cov2cor(fitCov);
                                resCvC = matrix(NA, nrow=numVar, ncol=numVar, dimnames=list(nmeVar, nmeVar));
                                resCvC[lower.tri(resCvC, diag=TRUE)]  = resCov[lower.tri(resCov, diag=TRUE)];
                                resCvC[upper.tri(resCvC, diag=FALSE)] = resCrr[upper.tri(resCrr, diag=FALSE)];
                                df<-as.data.frame(resCvC)
                                df$type="Residual"
                                self$tab_covcorrResidual<-df
                                
                              }
                              if (self$options$outpuCombineCovariances) {
                                self$tab_covcorr<-rbind(self$tab_covcorr,self$tab_covcorrImplied)
                                self$tab_covcorr<-rbind(self$tab_covcorr,self$tab_covcorrResidual)
                                self$tab_covcorrImplied<-NULL
                                self$tab_covcorrResidual<-NULL
                              }


                              mark('finished tab_covcorr');
                            }

                            # modification indices
                            if (self$options$outputModificationIndices) {
                              mark('begin tab_modInd');
                              modRes = lavaan::modificationIndices(self$model);
                              if (self$options$miHideLow) {
                                modRes = modRes[modRes$mi > self$options$miThreshold, ];
                              }
                              if (nrow(modRes) > 0) {
                                self$tab_modInd = modRes[order(modRes$mi, decreasing=TRUE), ];
                              } else {
                                self$warnings = list(topic="tab_modInd", message='No modification indices above threshold.');
                                self$tab_modInd = NULL;
                              }
                              mark('finished tab_modInd');
                            }
                            
                            ginfo("Estimation is done...")
                          } # end of private function estimate

              ) # end of private
)  # end of class
