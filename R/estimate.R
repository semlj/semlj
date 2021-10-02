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
                          tab_covcorrObserved=NULL,
                          tab_covcorrImplied=NULL,
                          tab_covcorrResidual=NULL,
                          tab_covcorrCombined=NULL,
                          tab_modInd=NULL,
                          ciwidth=NULL,
                          initialize=function(options,datamatic) {
                            super$initialize(options=options,datamatic=datamatic)
                            self$ciwidth <- options$ciWidth/100
                          },
                          estimate = function(data) {
                            
                            ## prepare the options based on Syntax definitions
                            ## NOTE: for some reasons, when `<-` is present in the model fixed.x passed by lavaanify()
                            ##       is not considered by lavaan(). We passed again and it works
                            lavoptions <- list(model = private$.lav_structure, 
                                             data = data,
                                             estimator  = self$options$estimator,
                                             likelihood = self$options$likelihood,
                                             se         = self$options$se,
                                             bootstrap  = self$options$bootN,
                                             std.ov     = self$options$std_ov,
                                             fixed.x    = self$options$cov_x
                            )
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
                            ginfo("Estimating the model...")
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
                             ## we need to be sure to keep `<~` operator ##
                             op<-gsub("<~","&",.lav_structure$op, fixed = T)
                             sel <- grep("==|<|>",op,invert = T)
                             ## select coefficients
                            .lav_structure <- .lav_structure[sel,]
                            ## make some change to render the results
                            .lav_params$free <- (.lav_structure$free>0)
                            
                            ## collect regression coefficients table
                            self$tab_coefficients <- .lav_params[.lav_params$op == "~",]

                            ## collect loadings table
                            self$tab_loadings <- .lav_params[.lav_params$op == "=~",]

                            ## collect composites table
                            if (is.something(self$tab_composites))
                                  self$tab_composites <- .lav_params[.lav_params$op == "<~",]
                            
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
                              op<-self$tab_constfit$op
                              check <- sapply(op,function(con) length(grep("<|>",con))>0,simplify = T)
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
                                self$tab_constfit$lhs<-sapply(self$tab_constfit$lhs,function(st) ifelse(grepl("^.p\\d+\\.$",st),gsub(".","",st,fixed=T),st))
                                self$tab_constfit$rhs<-sapply(self$tab_constfit$rhs,function(st) ifelse(grepl("^.p\\d+\\.$",st),gsub(".","",st,fixed=T),st))
                                
                                self$tab_fit[[length(self$tab_fit)+1]] <- list(label="Constraints Score Test",
                                                                     chisq=tab$test$X2,
                                                                     df=tab$test$df,
                                                                     pvalue=tab$test$p.value)

                              }
                            } # end of checking constraints

                            # additional fit measures
                            if (self$options$outputAdditionalFitMeasures) {                            
                              ginfo('begin addFit');
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
                              ginfo('finished addFit');
                            }
                            

                            # R²
                            if (self$options$r2!="none") {
                              ginfo('begin tab_r2')
                              results<-try_hard(lavaan::parameterEstimates(self$model, se = FALSE, zstat = FALSE, pvalue = FALSE, ci = FALSE, rsquare=TRUE))
                              if (!isFALSE(results$error)) {
                                self$warnigs<-list(topic="tab_r2",message="Rsquared cannot be computed for this model")
                              } else {
                                      RSqEst = results$obj
                                      RSqEst = RSqEst[RSqEst$op == "r2",]
                                      if (self$options$r2=="endo") {
                                            endo<-private$.lav_structure$lhs[private$.lav_structure$op=="~"]
                                            RSqEst<-RSqEst[RSqEst$lhs %in% endo,]  
                                        }
                              ### for some reasons, multigroup r2 are identified by block and not group
                                      RSqEst$group<-RSqEst$block
                                      if (nrow(RSqEst) > 0) {
                                            RSqEst<-private$.fix_groups_labels(RSqEst)
                                            self$tab_Rsquared<- RSqEst
                                      }
                              ginfo('finished tab_r2')
                              }
                            }

                            # Mardia's coefficients
                            if (self$options$outputMardiasCoefficients) {
                              ginfo('begin tab_mardia');
                              ## for multilevel-multigroup it gives an error. For the moment we 
                              ## wrap it in try_hard() so it does not halt the estimation of other tables
                              
                              # re-implemented code from semTools → R/dataDiagnosis.R with the aim to re-use statistics. etc.
                              # that are already contained in the lavaan model-fit
                              results<-try_hard({
                                      nVar = length(self$model@Data@ov$name);
                                      nObs = self$model@Data@nobs[[1]];
                                      cntDta = as.list(data.frame(t(sweep(self$model@Data@X[[1]], 2, self$model@SampleStats@mean[[1]]))));
                                      invS = self$model@SampleStats@icov[[1]] / nObs * (nObs - 1);

                                      FUN_S1 <- function(vec1, vec2, invS)     { as.numeric(t(as.matrix(vec1)) %*% invS %*% as.matrix(vec2)) };
                                      FUN_S2 <- function(vec1, listVec2, invS) { sapply(listVec2, FUN_S1, vec1=vec1, invS=invS) };
                                      MS_Cf  <- sum(sapply(cntDta, FUN_S2, listVec2=cntDta, invS=invS) ^ 3) / (nObs ^ 2);
                                      MS_chi <- nObs * MS_Cf / 6;
                                      MS_df  <- nVar * (nVar + 1) * (nVar + 2) / 6;
                                      MS_p   <- pchisq(MS_chi, df = MS_df, lower.tail = FALSE);
                              
                                      FUNK1 <- function(vec, invS) { as.numeric(t(as.matrix(vec)) %*% invS %*% as.matrix(vec)) };
                                      MK_Cf <- sum(sapply(cntDta, FUNK1, invS=invS) ^ 2) / nObs;
                                      MK_z  <- (MK_Cf - nVar * (nVar + 2)) / sqrt(8 * nVar * (nVar + 2) / nObs);
                                      MK_p  <- pnorm(-abs(MK_z)) * 2;
                                      list(list(name = "Skewness", coeff=MS_Cf, z="",   chi=MS_chi, df=MS_df, p=MS_p),
                                                              list(name = "Kurtosis", coeff=MK_Cf, z=MK_z, chi="",     df="",    p=MK_p));
                              })
                              ginfo('before adding to tab_mardia')
                              self$warnings<-list(topic="tab_mardia",message=results$warning)
                              self$warnings<-list(topic="tab_mardia",message=results$error)
                              
                              self$tab_mardia <- results$obj 
                            }

                            # covariances and correlations
                            if (self$options$outputObservedCovariances || self$options$outputImpliedCovariances || self$options$outputResidualCovariances) {
                              nmeVar = lavaan::lavNames(self$model, 'ov');
                              numVar = length(nmeVar);
                              
                              all_obsCov = lavaan::inspect(self$model, "observed")
                              # we always want a list of matrices, so the results will be ok also
                              # for multigroup and multilevel 
                              if ("cov" %in% names(all_obsCov))
                                 all_obsCov<-list("1"=all_obsCov)
                              
                              all_fitCov = lavaan::inspect(self$model,   "fitted")
                              if ("cov" %in% names(all_fitCov))
                                  all_fitCov<-list("1"=all_fitCov)
                              
                              # TO-DO: Implement standardized residuals (cov.z instead of cov)
                              
                              ## for multilevel-multigroup (in the same model) model 
                              ## lav residuals are not available yet. So we wrap in try_hard()
                              ## do it does not stop the estimation of other tables
                              results<-try_hard(lavaan::lavResiduals(self$model, type="raw"))
                              all_resCov =results$obj 
                              if (is.something(all_resCov) & ("cov" %in% names(all_resCov)))
                                  all_resCov<-list("1"=all_resCov)
                              

                              if (self$options$outputObservedCovariances) {
                                obsCvClist<-list()
                                for (i in seq_along(all_obsCov)) {
                                      obsCov<-all_obsCov[[i]]$cov
                                      obsCrr = stats::cov2cor(obsCov);
                                      obsCvC = matrix(NA, nrow=numVar, ncol=numVar, dimnames=list(nmeVar, nmeVar));
                                      obsCvC[lower.tri(obsCvC, diag=TRUE)]  = obsCov[lower.tri(obsCov, diag=TRUE)];
                                      obsCvC[upper.tri(obsCvC, diag=FALSE)] = obsCrr[upper.tri(obsCrr, diag=FALSE)];
                                      obsCvClist[[length(obsCvClist)+1]]<-obsCvC
                                }
                                obsCvC<-do.call("rbind",obsCvClist)
                                self$tab_covcorrObserved <- cbind(variable=nmeVar, type="observed", as.data.frame(obsCvC));
                              }
                              
                              
                              if (self$options$outputImpliedCovariances)  { 
                                fitCvClist<-list()
                                for (i in seq_along(all_obsCov)) {
                                  fitCov<-all_fitCov[[i]]$cov
                                  fitCrr = cov2cor(fitCov);
                                  fitCvC = matrix(NA, nrow=numVar, ncol=numVar, dimnames=list(nmeVar, nmeVar));
                                  fitCvC[lower.tri(fitCvC, diag=TRUE)]  = fitCov[lower.tri(fitCov, diag=TRUE)];
                                  fitCvC[upper.tri(fitCvC, diag=FALSE)] = fitCrr[upper.tri(fitCrr, diag=FALSE)];
                                  fitCvClist[[length(fitCvClist)+1]]<-fitCvC
                                  
                                }
                                fitCvC<-do.call("rbind",fitCvClist)
                                self$tab_covcorrImplied <- cbind(variable=nmeVar, type="implied", as.data.frame(fitCvC));
                              }
                              
                              if (self$options$outputResidualCovariances) {
                                # calculates the difference between observed and fitted correlations since
                                # using cov2cor(resCov) almost invariably ends in having 0 or NA entries in the
                                # main diagonal (given the small size of the residuals)
                                # TO-DO: check whether the values have to be Fisher z-transformed before subtracting
                                resCvClist<-list()
                                for (i in seq_along(all_obsCov)) {
                                  resCrr = cov2cor(all_obsCov[[i]]$cov) - cov2cor(all_fitCov[[i]]$cov)
                                  resCvC = matrix(NA, nrow=numVar, ncol=numVar, dimnames=list(nmeVar, nmeVar));
                                  resCvC[upper.tri(resCvC, diag=FALSE)] = resCrr[upper.tri(resCrr, diag=FALSE)];
                                  ## if we have lavResiduals, we use them
                                  if (is.something(all_resCov)) {
                                    resCov<-all_resCov[[i]]$cov
                                    resCvC[lower.tri(resCvC, diag=TRUE)]  = resCov[lower.tri(resCov, diag=TRUE)];
                                  }
                                  
                                  resCvClist[[length(resCvClist)+1]]<-resCvC
                                }
                                resCvC<-do.call("rbind",resCvClist)
                                self$tab_covcorrResidual <- cbind(variable=nmeVar, type="residual", as.data.frame(resCvC));
                              }
                              if (self$options$outpuCombineCovariances) {
                                dfCombined <- rbind(self$tab_covcorrObserved, self$tab_covcorrImplied, self$tab_covcorrResidual);
                                self$tab_covcorrCombined <- dfCombined[order(dfCombined$variable), ];
                                self$tab_covcorrObserved <- NULL;
                                self$tab_covcorrImplied  <- NULL;
                                self$tab_covcorrResidual <- NULL;
                              }
                              ginfo('finished tab_covcorr');
                            }

                            # modification indices
                            if (self$options$outputModificationIndices) {
                              ginfo('begin tab_modInd');
                              modRes = lavaan::modificationIndices(self$model);
                              if (self$options$miHideLow) {
                                modRes = modRes[modRes$mi > self$options$miThreshold, ];
                              }
                              if (nrow(modRes) > 0) {
                                modRes<-modRes[order(modRes$mi, decreasing=TRUE), ]
                                modRes<-private$.fix_groups_labels(modRes)
                                self$tab_modInd = modRes 
                              } else {
                                self$warnings = list(topic="tab_modInd", message='No modification indices above threshold.');
                                self$tab_modInd = NULL;
                              }
                              ginfo('finished tab_modInd');
                            }
                            
                            ginfo("Estimation is done...")
                          } # end of private function estimate

              ) # end of private
)  # end of class
