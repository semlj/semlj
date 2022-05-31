## This class takes care of estimating the models and return the results. It inherit from Initer, and defines the same tables
## defined by Initer, but it fills them with the results. It also adds a few tables not defined in Initer
## Any function that produce a table goes here

Runner <- R6::R6Class("Runner",
                        inherit = Initer,
                        cloneable=FALSE,
                        class=TRUE,
                        public=list(
                          model=NULL,
                          initialize=function(options,dispatcher,datamatic) {
                            super$initialize(options,dispatcher,datamatic)
                          },
                          estimate = function(data) {
                            ## prepare the options based on Syntax definitions
                            ## NOTE: for some reasons, when `<-` is present in the model fixed.x passed by lavaanify()
                            ##       is not considered by lavaan(). We passed again and it works
                            lavoptions <- list(model = private$.lav_structure, 
                                               data = data,
                                               estimator  = self$options$estimator,
                                               likelihood = self$options$likelihood,
                                               std.ov     = self$options$std_ov,
                                               bootstrap  = self$options$bootN,
                                               fixed.x    = self$options$cov_x
                            )
                            
                            if (self$options$se!="auto") 
                                lavoptions[["se"]]<-self$options$se

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
                            ginfo("Estimating the model...done")
                            
                            ## check if warnings or errors are produced
                            self$dispatcher$warnings <-  list(topic="info", message=results$warning)
                            ## it it fails here, we should stop
                            error<-results$error
                            if (length(grep("subscript out of bound",error,fixed=T))>0) 
                                   error<-"Model cannot be estimated. Please refine the model or choose different options."
                            self$dispatcher$errors <-  list(topic="info", message=error,final=TRUE)
                            
                            self$model <- results$obj
                          },
                          
                          par_table=function() {
                             
                            if (is.null(private$.par_table))
                                private$.get_par_table()
                            
                            return(private$.par_table)

                          },
                          fit_measures=function() {
                            
                            if (is.null(private$.fit_measures))
                                private$.get_fit_measures()

                            return(private$.fit_measures)

                          },
                          run_info=function() {

                            alist <- list()
                            alist[[length(alist) + 1]]   <-  c(info="Estimation Method",value=self$model@Options$estimator)
                            alist[[length(alist) + 1]]   <-  c(info="Optimization Method",value=toupper(self$model@Options$optim.method))
                            alist[[length(alist) + 1]]   <-  c(info="Number of observations",value=lavaan::lavInspect(self$model,"ntotal")) 
                            alist[[length(alist) + 1]]   <-  c(info="Free parameters",value=self$model@Fit@npar)
                            alist[[length(alist) + 1]]   <-  c(info="Standard errors",value=INFO_SE[[self$model@Options$se]])
                            alist[[length(alist) + 1]]   <-  c(info="Scaled test",value=private$.get_test_info())
                            alist[[length(alist) + 1]]   <-  c(info="Converged",value=self$model@Fit@converged) 
                            alist[[length(alist) + 1]]   <-  c(info="Iterations",value=self$model@optim$iterations) 
                            alist[[length(alist) + 1]]   <-  c(info="",value="")

                            return(alist)
                          },
                          run_fit_main=function() {
                            
                            fit<-self$fit_measures()
                            if (hasName(fit,"logl")) logl=fit[["logl"]] else logl=""
                            if (hasName(fit,"logl.restricted")) logl=fit[["logl"]] else logl=""
                            
                            tab <- list()
                            tab[[1]] <- list(    label="User Model",
                                                 chisq=fit[["chisq"]],
                                                 df=fit[["df"]],
                                                 pvalue=fit[["pvalue"]],
                                                 logl=logl
                                                 )
                            if (hasName(fit,"baseline.chisq"))  
                                    tab[[length(tab) + 1]] <- list(  label="Baseline Model",
                                                                     chisq=fit[["baseline.chisq"]],
                                                                     df=fit[["baseline.df"]],
                                                                     pvalue=fit[["baseline.pvalue"]]
                                                                     )
                            if (hasName(fit,"chisq.scaled"))  
                                    tab[[length(tab) + 1]] <- list(  label="Scaled User",
                                                                     chisq=fit[["chisq.scaled"]],
                                                                     df=fit[["df.scaled"]],
                                                                     pvalue=fit[["pvalue.scaled"]]
                                                                     )
                            
                            if (hasName(fit,"baseline.chisq.scaled"))  
                                    tab[[length(tab) + 1]] <- list(  label="Scaled Baseline",
                                                                     chisq=fit[["baseline.chisq.scaled"]],
                                                                     df=fit[["baseline.df.scaled"]],
                                                                     pvalue=fit[["baseline.pvalue.scaled"]]
                                                                     )
                            return(tab)

                          },
                          run_fit_constraints=function() {
                            
                            # checking constraints
                            itab  <-  self$init_fit_constraints()
                            if (is.null(itab)) return()
                            
                            op<-itab$op
                              check <- sapply(op,function(con) length(grep("<|>",con))>0,simplify = T)
                              if (any(check)) {
                                 warning(WARNS[["scoreineq"]])
                                 return()
                              }
                              tab  <-  NULL
                              rtab <- lavaan::lavTestScore( self$model,
                                                            univariate = self$options$scoretest,
                                                            cumulative = self$options$cumscoretest
                                                          )

                              if (self$options$scoretest) {
                                  names(rtab$uni) <- c("lhs","op","rhs","chisq","df","pvalue")
                                  tab  <-  rtab$uni
                                  tab$type  <-  "Univariate"
                              }
                              if (self$options$cumscoretest) {
                                  names(rtab$cumulative) <- c("lhs","op","rhs","chisq","df","pvalue")
                                  rtab$cumulative$type   <- "Cumulative"
                                  tab <- rbind(tab,rtab$cumulative)
                              }
                              tab$lhs  <- sapply(tab$lhs,function(st) ifelse(grepl("^.p\\d+\\.$",st),gsub(".","",st,fixed=T),st))
                              tab$rhs  <- sapply(tab$rhs,function(st) ifelse(grepl("^.p\\d+\\.$",st),gsub(".","",st,fixed=T),st))
                              ### here we add the total test ###
                              ttab        <-  rtab$test
                              ttab$test   <-  NULL
                              names(ttab) <- c("chisq","df","pvalue")
                              ttab$lhs    <-  ttab$op<-ttab$rhs<-""
                              ttab$type   <-  "Total"
                              tab         <-  rbind(tab,ttab)
                              
                              return(tab)

                          },
                          run_fit_indices=function() {
                            
                            fi  <-  self$fit_measures()
                            
                            tab <-  list(list(srmr=fi$srmr,
                                              rmsea=fi$rmsea,
                                              rmsea.ci.lower=fi$rmsea.ci.lower,
                                              rmsea.ci.upper=fi$rmsea.ci.upper,
                                              rmsea.pvalue=fi$rmsea.pvalue
                                              )
                                         )
                            
                            if (hasName(fi,"rmsea.robust")) {
                              
                              tab[[2]]  <-  list(srmr=fi$srmr_bentler,
                                                 rmsea=fi$rmsea.robust,
                                                 rmsea.ci.lower=fi$rmsea.ci.lower.robust,
                                                 rmsea.ci.upper=fi$rmsea.ci.upper.robust,
                                                 rmsea.pvalue=fi$rmsea.pvalue.robust
                                                 )
                            }
                            if (hasName(fi,"rmsea.scaled") ) {
                              
                              tab[[length(tab)+1]]<-list(srmr=fi$srmr_bentler,
                                                         rmsea=fi$rmsea.scaled,
                                                         rmsea.ci.lower=fi$rmsea.ci.lower.scaled,
                                                         rmsea.ci.upper=fi$rmsea.ci.upper.scaled,
                                                         rmsea.pvalue=fi$rmsea.pvalue.scaled)

                            }
                            return(tab)

                          },
                          run_fit_modelbaseline=function() {
                            
                            fit<-self$fit_measures()
                            alist<-list()
                            alist[[length(alist) + 1]]  <- list(name = "Comparative Fit Index (CFI)",                statistics = fit[["cfi"]]);
                            alist[[length(alist) + 1]]  <- list(name = "Tucker-Lewis Index (TLI)",                   statistics = fit[["tli"]]);
                            alist[[length(alist) + 1]]  <- list(name = "Bentler-Bonett Non-normed Fit Index (NNFI)", statistics = fit[["nnfi"]]);
                            alist[[length(alist) + 1]]  <- list(name = "Bentler-Bonett Normed Fit Index (NFI)",      statistics = fit[["nfi"]]);
                            alist[[length(alist) + 1]]  <- list(name = "Parsimony Normed Fit Index (PNFI)",          statistics = fit[["pnfi"]]);
                            alist[[length(alist) + 1]]  <- list(name = "Bollen's Relative Fit Index (RFI)",          statistics = fit[["rfi"]]);
                            alist[[length(alist) + 1]]  <- list(name = "Bollen's Incremental Fit Index (IFI)",       statistics = fit[["ifi"]]);
                            alist[[length(alist) + 1]]  <- list(name = "Relative Noncentrality Index (RNI)",         statistics = fit[["rni"]]);
                            return(alist)
                            
                          },
                          run_fit_moreindices=function() {
                            
                            
                            fit<-self$fit_measures()
                            alist<-list()
                            if (hasName(fit,"logl"))
                               alist[[length(alist) + 1]]  <- list(name = "Log Likelihood",            statistics = fit[["logl"]]);

                            if (hasName(fit,"unrestricted.logl"))
                              alist[[length(alist) + 1]]  <- list(name = "Unrestricted Log Likelihood",            statistics = fit[["unrestricted.logl"]]);
                            
                            alist[[length(alist) + 1]]  <- list(name = "Hoelter Critical N (CN), α=0.05",            statistics = fit[["cn_05"]]);
                            alist[[length(alist) + 1]]  <- list(name = "Hoelter Critical N (CN), α=0.01",            statistics = fit[["cn_01"]]);
                            alist[[length(alist) + 1]]  <- list(name = "Goodness of Fit Index (GFI)",                statistics = fit[["gfi"]]);
                            alist[[length(alist) + 1]]  <- list(name = "Parsimony Goodness of Fit Index (GFI)",      statistics = fit[["pgfi"]]);
                            alist[[length(alist) + 1]]  <- list(name = "McDonald Fit Index (MFI)",                   statistics = fit[["mfi"]]);
                            return(alist)
                            
                            
                          },
                          run_fit_rsquared=function() {
                            
                            # R²
                            if (self$option("r2","none"))
                                return()
                                
                              results<-try_hard(lavaan::parameterEstimates(self$model,
                                                                           se = FALSE, 
                                                                           zstat = FALSE, 
                                                                           pvalue = FALSE, 
                                                                           ci = FALSE, 
                                                                           rsquare=TRUE))
                              if (!isFALSE(results$error)) {
                                warning("R-squared cannot be computed for this model")
                                return()
                              } 
                              ### here we can compute them
                                RSqEst = results$obj
                                RSqEst = RSqEst[RSqEst$op == "r2",]
                                if (self$option("r2","endo")) {
                                  endo<-private$.lav_structure$lhs[private$.lav_structure$op=="~"]
                                  RSqEst<-RSqEst[RSqEst$lhs %in% endo,]  
                                }
                                
                                ### for some reasons, multigroup r2 are identified by block and not group
                                RSqEst$group<-RSqEst$block
                                if (nrow(RSqEst) > 0) {
                                  RSqEst<-private$.fix_groups_labels(RSqEst)
                                  return(RSqEst)
                                }
                                warning("R-squared not completed")
                                return(NULL)
                          },
                          run_fit_icc=function() {
                            
                            tab<-lavaan::lavInspect(self$model,"icc")
                            tab<-private$.make_matrix_table(tab,transform)
                            labs<-row.names(tab)
                            names(tab)<-"est"
                            
                            if (is.something(self$multigroup)) {
                              labs<-stringr::str_split(labs,"\\.")
                              tab$lgroup<-sapply(labs, function(x) x[[1]])
                              labs<-sapply(labs, function(x) paste0(x[-1],collapse = "."))
                            }
                            tab$rhs<-labs
                            return(tab)
                          },
                          run_models_coefficients=function() {
                            
                            tab<-self$par_table()
                            tab<-tab[tab$op=="~",]
                            if (nrow(tab)==0) tab<-NULL
                            return(tab)
                            
                          },
                          run_models_loadings=function() {
                            
                            tab<-self$par_table()
                            tab<-tab[tab$op=="=~",]
                            if (nrow(tab)==0) tab<-NULL
                            return(tab)
                            
                          },
                          run_models_composites=function() {
                            
                            tab<-self$par_table()
                            tab<-tab[tab$op=="<~",]
                            if (nrow(tab)==0) tab<-NULL
                            return(tab)
                            
                          },
                          run_models_covariances=function() {
                            
                            tab<-self$par_table()
                            tab<-tab[tab$op=="~~",]
                            if (nrow(tab)==0) tab<-NULL
                            return(tab)
                            
                          },
                          run_models_intercepts=function() {
                            
                            tab<-self$par_table()
                            tab<-tab[tab$op=="~1",]
                            if (nrow(tab)==0) tab<-NULL
                            return(tab)
                            
                          },
                          run_models_thresholds=function() {
                            
                            tab<-self$par_table()
                            tab<-tab[tab$op=="|",]
                            if (nrow(tab)==0) tab<-NULL
                            return(tab)
                            
                          },
                          run_models_mlmeans=function() {
                            
                            if (!is.something(self$cluster))
                               return(NULL)
                            
                            tab = lavaan::inspect(self$model, "fitted")
                            tabs<-lapply(tab, function(atab) atab$mean)
                            tab<-private$.make_matrix_table(tabs,transform)
                            names(tab)<-"est"
                            return(tab)
                          },
                          
                          run_models_defined=function() {
                            
                            tab<-self$par_table()
                            tab<-tab[tab$op==":=",]
                            if (nrow(tab)==0) tab<-NULL
                            return(tab)
                            
                          },
                          run_additional_reliability=function() {

                            tab<-NULL
                            results<-try_hard(semTools::reliability(self$model))
                            self$dispatcher$warnings<-list(topic="additional_reliability",results$warning)
                            self$dispatcher$warnings<-list(topic="additional_reliability",results$error)
                            if (isFALSE(results$error))
                              tab<-private$.make_matrix_table(results$obj,fun=t)
                            return(tab)
                            
                          },
                          run_additional_mardia=function() {
                            
                              ## for multilevel-multigroup it gives an error. For the moment we 
                              ## we leave it fail in that case
                            
                              # re-implemented code from semTools → R/dataDiagnosis.R with the aim to re-use statistics. etc.
                              # that are already contained in the lavaan model-fit
                            
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
                                
                                tab<-list(list(name = "Skewness", coeff=MS_Cf, z="",   chi=MS_chi, df=MS_df, p=MS_p),
                                     list(name = "Kurtosis", coeff=MK_Cf, z=MK_z, chi="",     df="",    p=MK_p));
                                
                                return(tab)

                          },
                          run_covariances_observed=function() {
                            
                            tab = lavaan::inspect(self$model, "observed")
                            tab<-private$.make_covcor_table(tab)
                            tab$type<-"observed"
                            return(tab)
                          },
                          run_covariances_implied=function() {
                            
                            tab = lavaan::inspect(self$model, "fitted")
                            tab<-private$.make_covcor_table(tab)
                            tab$type<-"implied"
                            return(tab)
                            
                          },
                          run_covariances_residual=function() {
                            
                            # calculates the difference between observed and fitted correlations since
                            # using cov2cor(resCov) almost invariably ends in having 0 or NA entries in the
                            # main diagonal (given the small size of the residuals)

                            obj1 = lavaan::inspect(self$model, "observed")
                            obj2 = lavaan::inspect(self$model, "fitted")
                            tab<-private$.make_covcor_diff(obj1,obj2)
                            tab$type<-"residual"
                            return(tab)
                            
                          },
                          run_covariances_combined=function() {
                            
                            tab1<-tab2<-tab3<-NULL
                            if (self$option("outputObservedCovariances"))
                              tab1<-self$run_covariances_observed()
                            if (self$option("outputImpliedCovariances"))
                              tab2<-self$run_covariances_implied()
                            if (self$option("outputResidualCovariances"))
                              tab2<-self$run_covariances_residual()
                            
                            rbind(tab1,tab2,tab3)
                            
                          },
                          run_covariances_latent=function() {
                            
                            covs<-lavaan::lavInspect(self$model,"cov.lv")
                            private$.make_matrix_table(covs)

                          },
                          run_modification_indices=function() {
                            
                            modRes = lavaan::modificationIndices(self$model);
                            if (self$options$miHideLow) {
                              modRes = modRes[modRes$mi > self$options$miThreshold, ];
                            }
                            if (nrow(modRes) > 0) {
                              modRes<-modRes[order(modRes$mi, decreasing=TRUE), ]
                              modRes<-private$.fix_groups_labels(modRes)
                              tab = modRes 
                            } else {
                              warning('No modification indices above threshold');
                              tab = NULL;
                            }
                            tab
                          },
                          savePredRes=function(results,data) {

                            .compute<-function() {
                            #### this comes from
                            ###  https://github.com/mjderooij/SEMpredict/blob/main/predicty.lavaan.R
                            try_hard({

                              Sxx = lavaan::fitted(self$model)$cov[xnames , xnames]
                              Sxy = lavaan::fitted(self$model)$cov[xnames , ynames]
                              mx = lavaan::fitted(self$model)$mean[xnames]
                              my = lavaan::fitted(self$model)$mean[ynames]
                              
                              #
                              Xtest = as.matrix(data[, xnames])
                              Xtest = scale(Xtest, center = mx, scale = FALSE)
                              yhat = matrix(my, nrow = nrow(Xtest), ncol = length(ynames), byrow = TRUE) + Xtest %*% solve(Sxx) %*% Sxy
                              colnames(yhat)<-paste0("PRDV_",ynames)
                              data.frame(yhat, row.names=rownames(data))
                            })
                            }
                            
                              
                              if (self$option("preds_dv") & results$preds_dv$isNotFilled()) {
                                 ginfo("saving dv predicted")
                                 .names<-private$.observed_vars()
                                 xnames<-.names[[1]]
                                 ynames<-.names[[2]]
                           
                                 predsobj<-.compute()
                                 predsdata<-predsobj$obj
                                 if (!isFALSE(predsobj$error)) {
                                   self$dispatcher$warnings<-list(topic="info",message="Dependent variables predicted values cannot be computed for this  model")
                                 } else {
                                   results$preds_dv$set(1:ncol(predsdata),
                                                       names(predsdata),
                                                       rep("DV Predicted",ncol(predsdata)),
                                                       rep("continuous",ncol(predsdata)))
                                   results$preds_dv$setValues(predsdata)
                                   self$dispatcher$warnings<-list(topic="info",message=paste("Dependent variables predicted values saved in the dataset. Varnames:",paste(names(predsdata),collapse = ", ")))
                                 }                       
                                 }
                              
                              if (self$option("preds_lv") & results$preds_lv$isNotFilled()) {
                                 ginfo("saving lv predicted")
                                 predsdata<-as.data.frame(lavaan::lavPredict(self$model,type="lv"))
                                 if (ncol(predsdata)>0) {
                                   
                                   colnames(predsdata)<-paste0("PRFS_",colnames(predsdata))
                                   results$preds_lv$set(1:ncol(predsdata),
                                                        names(predsdata),
                                                        rep("Factor scores ",ncol(predsdata)),
                                                        rep("continuous",ncol(predsdata)))
                                   results$preds_lv$setValues(predsdata)
                                   self$dispatcher$warnings<-list(topic="info",message=paste("Factors scores (latent predicted values) saved in the dataset. Varnames:",paste(names(predsdata),collapse = ", ")))
                                   
                                 } else {
                                   self$dispatcher$warnings<-list(topic="info",message="Factor scores cannot be computed for this model")
                                   
                                 }
                                 
                              }
                              if (self$option("preds_ov") && results$preds_ov$isNotFilled()) {
                                ginfo("saving ov predicted")
                                predsdata<-as.data.frame(lavaan::lavPredict(self$model,type="ov"))
                                if (ncol(predsdata)>0) {
                                  colnames(predsdata)<-paste0("PRIN_",colnames(predsdata))
                                  results$preds_ov$set(1:ncol(predsdata),
                                                     names(predsdata),
                                                     rep("Indicator predicted",ncol(predsdata)),
                                                     rep("continuous",ncol(predsdata)))
                                  results$preds_ov$setValues(predsdata)
                                  self$dispatcher$warnings<-list(topic="info",message=paste("Indicators predicted values saved in the dataset. Varnames:",paste(names(predsdata),collapse = ", ")))
                                } else {
                                  self$dispatcher$warnings<-list(topic="info",message="Indicators predicted values cannot be computed for this model")
                                }
                              }

                  }  ## end of savePredRes

                          
                          
                          ), # end of public function estimate

                        private=list(
                          .fit_measures=NULL,
                          .par_table=NULL,

                          .get_fit_measures=function() {
                            
                              results<-try_hard(as.list(lavaan::fitmeasures(self$model)))

                              if (!isFALSE(results$warning))
                                       warning(results$warning)
                            
                              if (!isFALSE(results$error)) {
                                       err<-gsub("subscript out of bounds","Results not available, please revise the model",results$error,fixed=T)
                                       warning(err)
                                      return()
                               }
                             
                              private$.fit_measures<-results$obj

                            
                          },  

                          .get_par_table=function() {
                            
                            results<-try_hard(
                              lavaan::parameterestimates(
                                self$model,
                                ci = self$options$ci,
                                standardized = T,
                                level = self$options$ci_width/100,
                                boot.ci.type = self$options$bootci
                              )
                            )
                            self$dispatcher$warnings <- list(topic="info", message=results$warning)
                            self$dispatcher$errors <- list(topic="info", message=results$error)
                            private$.par_table<-results$obj
                            if (is.null(private$.par_table)) private$.par_table<-FALSE
                            userlabel<-grep("^\\p\\d+$",private$.par_table$label,invert = T)
                            if (length(userlabel)>0) {
                              ilabel<-paste("(",private$.lav_structure$plabel[userlabel],")")
                              private$.par_table$label[userlabel]<-paste(private$.par_table$label[userlabel],ilabel)
                            }
                          },
                          .get_test_info=function() {
                            
                            if (! "test" %in% methods::slotNames(self$model))
                                return("")

                            tests<-names(self$model@test)

                            if (length(tests)<2)
                              return("None")
                            
                            return(INFO_TEST[[tests[[2]]]])
                            
                          }
                          

                        ) #end of private
)  # end of class


