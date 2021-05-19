## This class takes care of estimating the model and return the results. It inherit from Syntax, and define the same tables
## defined by Syntax, but it fill them with the results.

Estimate <- R6::R6Class("Estimate",
                        inherit = Syntax,
                        cloneable=FALSE,
                        class=FALSE,
                        list(
                          model=NULL,
                          tab_fit=NULL,
                          tab_fitindices=NULL,
                          ciwidth=NULL,
                          tab_constfit=NULL,
                          initialize=function(options,datamatic) {
                            super$initialize(options=options,datamatic=datamatic)
                            self$ciwidth<-options$ciWidth/100
                          },
                          estimate=function(data) {
                            
                            
                            ## prepare the options based on Syntax definitions
                            lavoptions<-list(model = private$.lav_structure, 
                                             data = data,
                                             se=self$options$se,
                                             bootstrap=self$options$bootN,
                                             estimator=self$options$estimator
                                             
                            )
                            if (is.something(self$multigroup)) {
                              lavoptions[["group"]]<-self$multigroup$var
                              lavoptions[["group.label"]]<-self$multigroup$levels
                            }
                            if (self$options$estimator=="ML") {
                              lavoptions[["likelihood"]]<-self$options$likelihood
                            }
                            
                            
                            ## estimate the models
                            results<-try_hard({do.call(lavaan::lavaan,lavoptions)  })
                            
                            

                            self$warnings<-list(topic="main",message=results$warning)
                            self$errors<-results$error
                            
                            if (is.something(self$errors))
                                return(self$errors)
                            
                            ## ask for the paramters estimates
                            self$model<-results$obj
                            .lav_params<-lavaan::parameterestimates(
                              self$model,
                              ci=self$options$ci,
                              standardized = T,
                              level = self$ciwidth,
                              boot.ci.type = self$options$bootci
                            )
                            
                            ## we need some info initialized by Syntax regarding the parameters properties
                            .lav_structure<-private$.lav_structure
                             sel<-grep("==|<|>",.lav_structure$op,invert = T)
                            .lav_structure<-.lav_structure[sel,]
                            ## make some change to render the results
                            mark(.lav_params)
                            .lav_params$rhs<-fromb64(.lav_params$rhs,self$vars)
                            .lav_params$lhs<-fromb64(.lav_params$lhs,self$vars)
                            .lav_params$free<-(.lav_structure$free>0)
                            
                            ## collect regression coefficients table
                            self$tab_coefficients<-.lav_params[.lav_params$op=="~",]

                            ## collect loadings table
                            self$tab_loadings<-.lav_params[.lav_params$op=="=~",]
                            
                            ## collect variances and covariances table
                            self$tab_covariances<-.lav_params[.lav_params$op=="~~",]

                            ## collect defined parameters table
                            self$tab_defined<-.lav_params[.lav_params$op==":=",]
                            if (nrow(self$tab_defined)==0) self$tab_defined<-NULL
                            
                            tab<-self$tab_covariances
                            ### collect intercepts
                            self$tab_intercepts<-.lav_params[.lav_params$op=="~1",]
                            if (nrow(self$tab_intercepts)==0) self$tab_intercepts<-NULL
                            
                            
                            #### fit tests ###
                            alist<-list()
                            ff<-lavaan::fitmeasures(self$model)
                            alist<-list()
                            if (ff[["df"]]>0)
                              alist[[1]]<-list(label="User Model",chisq=ff[["chisq"]],df=ff[["df"]],pvalue=ff[["pvalue"]])
                            try(alist[[length(alist)+1]]<-list(label="Baseline Model",chisq=ff[["baseline.chisq"]],df=ff[["baseline.df"]],pvalue=ff[["baseline.pvalue"]]))
                            self$tab_fitindices<-as.list(ff)
                            
                            self$tab_fit<-alist
                            
                            # fit indices
                            alist<-list()
                            alist[[length(alist)+1]]<-c(info="Estimation Method",value=self$model@Options$estimator)
                            alist[[length(alist)+1]]<-c(info="Number of observations",value=lavaan::lavInspect(self$model,"ntotal")) 
                            alist[[length(alist)+1]]<-c(info="Free parameters",value=self$model@Fit@npar)
                            alist[[length(alist)+1]]<-c(info="Converged",value=self$model@Fit@converged) 
                            alist[[length(alist)+1]]<-c(info="",value="")
                            try(alist[[length(alist)+1]]<-c(info="Loglikelihood user model",value=round(ff[["logl"]],digits=3) ))
                            try(alist[[length(alist)+1]]<-c(info="Loglikelihood unrestricted model",value=round(ff[["unrestricted.logl"]],digits=3)))
                            alist[[length(alist)+1]]<-c(info="",value="")
                            

                            self$tab_info<-alist
                            if (is.something(self$constraints)) {
                              check<-sapply(self$constraints,function(con) length(grep("<|>",con$value))>0,simplify = T)
                              if (any(check)) {
                                self$warnings<-list(topic="main",message=WARNS[["scoreineq"]])
                              } else {
                                tab<-lavaan::lavTestScore(self$model,
                                                          univariate = self$options$scoretest,
                                                          cumulative = self$options$cumscoretest)
                                
                                if (self$options$scoretest) {
                                  names(tab$uni)<-c("lhs","op","rhs","chisq","df","pvalue")
                                  self$tab_constfit<-tab$uni
                                  self$tab_constfit$type="Univariate"
                                }
                                if (self$options$cumscoretest) {
                                  names(tab$cumulative)<-c("lhs","op","rhs","chisq","df","pvalue")
                                  tab$cumulative$type<-"Cumulative"
                                  self$constfit<-rbind(self$constfit,tab$cumulative)
                                }
                                self$tab_fit[[length(self$tab_fit)+1]]<-list(label="Constraints Score Test",
                                                                     chisq=tab$test$X2,
                                                                     df=tab$test$df,
                                                                     pvalue=tab$test$p.value)
                                
                                
                              }
                            } # end of checking constraints
                            
                            ginfo("Estimation is done...")
                          } # end of private function estimate
                          


              ) # end of private
)  # end of class

