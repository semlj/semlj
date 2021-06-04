### This class takes care of passing all information from lavaan tables definitions and estimations from jamovi input
### to jamovi results tables. It works using Datamatic R6 class, Syntax R6 class, Estimate R6 class, and Plotter R6 class.
### Datamatic R6 cleans the data and makes some checking
### Syntax R6 class gets all input options and defines the tables required for showing the results using lavaan::lavaanify(). 
### Estimate R6 class inherits from Syntax all properties of the tables and fill them with the actual 
### results estimated with lavaan::lavaan() function.
### We do not need to initiate Syntax class because Estimate inherits from Syntax, 
### so only one instance of Estimate is defined. Here is called lav_machine.
### Filling the results tables is handled by function in jamovi.R (functions starting with j.)


semljsynClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "semljsynClass",
    inherit = semljsynBase,
    private = list(
        .lav_machine=NULL,
        .data_machine=NULL,
        .plot_machine=NULL,
        .model=NULL,
        .ready=NULL,
        .init = function() {
            ginfo("init")
            
            ### output some syntax examples if required by the user####
            
            if (self$options$constraints_examples) {
                j.init_table(self$results$contraintsnotes,LAT_EXAMPLES,indent=-1)
                j.init_table_append(self$results$contraintsnotes,CONT_EXAMPLES,indent=-1)
                j.init_table_append(self$results$contraintsnotes,DP_EXAMPLES,indent=-1)
                j.init_table_append(self$results$contraintsnotes,SY_EXAMPLES,indent=-1)
                self$results$contraintsnotes$setNote(1,CONT_NOTE)
            }
            

            ### check that we have enough information to run using readiness.R function####
            private$.ready<-readiness(self$options)
            if (!private$.ready$ready) {
                  if(private$.ready$report)
                      self$results$info$addRow("info",list(info="Setup",specs=private$.ready$reason))
                return()
            }
            ### prepare R6 classes that do the work ####
            data_machine<-Datamatic$new(self$options,self$data)
            lav_machine<-Estimate$new(self$options,data_machine)
            plot_machine<-Plotter$new(self$options,data_machine,lav_machine,self$results$pathgroup)
            
            ### fill the info table ###
            j.init_table(self$results$info,lav_machine$tab_info)

            #### parameter fit indices table ####
            j.init_table(self$results$fit$indices,"",ci=T,ciroot="rmsea.",ciformat='RMSEA {}% CI',ciwidth=self$options$ciWidth)

            #### parameter estimates table ####
            j.init_table(self$results$models$coefficients,lav_machine$tab_coefficients,ci=T,ciwidth=self$options$ciWidth)

            #### loadings table ####
            j.init_table(self$results$models$loadings,lav_machine$tab_loadings,ci=T,ciwidth=self$options$ciWidth)
            
            ### prepare defined params ###
            j.init_table(self$results$models$defined,lav_machine$tab_defined,ci=T,ciwidth=self$options$ciWidth)

            ### prepare intercepts ###
            if (self$options$showintercepts & !is.null(lav_machine$tab_intercepts))
                j.init_table(self$results$models$intercepts,lav_machine$tab_intercepts,ci=T,ciwidth=self$options$ciWidth)


            ### additional output ###
            if (is.something(lav_machine$tab_covcorrObserved)) {
                j.expand_table(self$results$group_covariances$covcorrObserved,lav_machine$tab_covcorrObserved)
                j.init_table(self$results$group_covariances$covcorrObserved,lav_machine$tab_covcorrObserved)
            }

            if (is.something(lav_machine$tab_covcorrImplied)) {
                j.expand_table(self$results$group_covariances$covcorrImplied,lav_machine$tab_covcorrImplied)
                j.init_table(self$results$group_covariances$covcorrImplied,lav_machine$tab_covcorrImplied)
            }
            if (is.something(lav_machine$tab_covcorrResidual)) {
                j.expand_table(self$results$group_covariances$covcorrResidual,lav_machine$tab_covcorrResidual)
                j.init_table(self$results$group_covariances$covcorrResidual,lav_machine$tab_covcorrResidual)
            }
            
            if (is.something(lav_machine$tab_covcorrCombined)) {
                j.expand_table(self$results$group_covariances$covcorrCombined,lav_machine$tab_covcorrCombined,append=T)
                j.init_table(self$results$group_covariances$covcorrCombined,lav_machine$tab_covcorrCombined)
                
            }
                
            ################
            
            private$.lav_machine<-lav_machine
            private$.data_machine<-data_machine
            private$.plot_machine<-plot_machine 

            ### init the diagram
            plot_machine$initPlots()
            
        },
    
        .run = function() {
            ginfo("run")
            ### check that we have enough information to run ####
            if (!private$.ready$ready)
                return()

            ### clean the data and prepare things ###
            lav_machine<-private$.lav_machine
            data<-private$.data_machine$cleandata(self$data)

            ## estimate the model running lavaan in lav_machine (Estimate R6 class)
            lav_machine$estimate(data)

            warns<-lav_machine$warnings

            ## fit info
            j.fill_table(self$results$info,lav_machine$tab_info)
            j.add_warnings(self$results$info,lav_machine)

            ## stop if error
             
            if (is.something(lav_machine$errors)) {
              stop(paste(lav_machine$errors,collapse = "; "))
            } 
            
            ## fit indices
            self$results$fit$indices$setRow(rowNo=1,lav_machine$tab_fitindices)
             
            ## fit test
            j.fill_table(self$results$fit$main,lav_machine$tab_fit,append=T)

            ## constraints fit test
            j.fill_table(self$results$fit$constraints,lav_machine$tab_constfit,append=T, spaceby="type")
            j.add_warnings(self$results$fit$constraints,lav_machine)

            ### parameters estimates ####
            j.fill_table(self$results$models$coefficients,lav_machine$tab_coefficients)

            ### loadings table ####
            j.fill_table(self$results$models$loadings,lav_machine$tab_loadings)
             
            j.fill_table(self$results$models$defined,lav_machine$tab_defined)
            j.add_warnings(self$results$models$defined,lav_machine,"defined")
            
            
            if (self$options$showintercepts & !is.null(lav_machine$tab_intercepts)) {
               j.fill_table(self$results$models$intercepts,lav_machine$tab_intercepts)
            }


            if (self$options$showintercepts & !is.null(lav_machine$tab_intercepts)) {
                j.fill_table(self$results$models$intercepts,lav_machine$tab_intercepts)
            }

            # Additional fit measures (1): User model versus baseline model
            if (self$options$outputAdditionalFitMeasures) {
                j.fill_table(self$results$add_outputs$compModelBsl,lav_machine$tab_compModelBsl)
            }

            # Additional fit measures (2): Other Fit Indices
            if (self$options$outputAdditionalFitMeasures) {
                j.fill_table(self$results$add_outputs$otherFit,lav_machine$tab_otherFit)
            }
            
            # R² measures
            if (self$options$outputRSquared) {
                j.fill_table(self$results$add_outputs$Rsquared,lav_machine$tab_Rsquared,append=T)
            }
            
            # Mardia's coefficients
            if (self$options$outputMardiasCoefficients) {
                j.fill_table(self$results$add_outputs$mardia,lav_machine$tab_mardia)
            }


            # Modification indices
            if (self$options$outputModificationIndices) {
                j.fill_table(self$results$modgroup$modInd,lav_machine$tab_modInd,append=T)
                j.add_warnings(self$results$modgroup$modInd,lav_machine,"tab_modInd")
            }
    
            ### loadings vars and covars ####
            if (is.something(lav_machine$tab_covcorrObserved)) {
                
                table<-self$results$group_covariances$covcorrObserved
                obj<-lav_machine$tab_covcorrObserved
                j.fill_table(table,obj)

            }
            
            if (is.something(lav_machine$tab_covcorrImplied)) {
                
                table<-self$results$group_covariances$covcorrImplied
                obj<-lav_machine$tab_covcorrImplied
                j.fill_table(table,obj) 
                
            }
            
            if (is.something(lav_machine$tab_covcorrResidual)) {
                
                table<-self$results$group_covariances$covcorrResidual
                obj<-lav_machine$tab_covcorrResidual
                j.fill_table(table,obj) 
                
            }
            if (is.something(lav_machine$tab_covcorrCombined)) {
                
                table<-self$results$group_covariances$covcorrCombined
                obj<-lav_machine$tab_covcorrCombined
                j.fill_table(table,obj,spaceby="type") 
                
            }
            
            ## diagrams
            private$.plot_machine$preparePlots()   
            if (is.something(private$.plot_machine$warnings$diagram)) {
                 for (i in seq_along(private$.plot_machine$warnings$diagram))
                        self$results$pathgroup$notes$addRow(i,list(message=private$.plot_machine$warnings$diagram[[i]]))
                  self$results$pathgroup$notes$setVisible(TRUE)
            }
            
            self$results$.setModel(lav_machine$model)
            ginfo("run ends")
            
        },
 
        .showDiagram=function(image,ggtheme, theme, ...) {
            if (self$options$diagram==FALSE) 
                return()
            if (!is.something(image$state$semModel))
                 return()
            options<-private$.plot_machine$semPathsOptions
            note<-FALSE
            # we cannot do a do.call() because semPaths will fail
            # so we need to pass the options directly to semPaths
            res<-try_hard(
                semPlot::semPaths(object = image$state$semModel
                                  ,layout =options$layout
                                  ,whatLabels=options$whatLabels
                                  ,rotation = options$rotation
                                  ,nCharNodes=options$nCharNodes
                                  ,sizeLat = options$sizeLat
                                  ,sizeLat2 = options$sizeLat2
                                  ,sizeMan=options$sizeMan
                                  ,sizeMan2=options$sizeMan2
                                  ,residuals=options$residuals
                                  ,intercepts=options$intercepts
                                  ,shapeLat=options$shapeLat
                                  ,shapeMan=options$shapeMan
                                  
                        )
            )
            
            
            if (!isFALSE(res$error)) {
                # translate some lavaan error so they make sense for the user interface
                if  (length(grep("Circle layout only supported",res$error,fixed = T))>0) {
                    res$error<-PLOT_WARNS[["nocircle"]]
                    note<-TRUE
                } 
                if  (length(grep("graph_from_edgelist",res$error,fixed = T))>0) {
                    res$error<-PLOT_WARNS[["nocircle"]]
                    note<-TRUE
                } 
                if  (length(grep("subscript out of",res$error,fixed = T))>0) {
                    res$error<-PLOT_WARNS[["fail"]]
                    note<-TRUE
                }
            }
            
            
            
            if (!isFALSE(res$error)) {
                 self$results$pathgroup$notes$addRow("err",list(message=res$error))
                 note<-TRUE
            }
            if (!isFALSE(res$warning)) {
                self$results$pathgroup$notes$addRow("war",list(message=res$warning))
                note<-TRUE
            }

            if (note)
                self$results$pathgroup$notes$setVisible(TRUE)

            return(TRUE)

        },

## at the moment no "syntax mode" is produced. I do not think that this module will be 
## useful in R, so the there's no need to output it's R syntax
        .sourcifyOption = function(option) {
            return("")
        }
        
        )
)
