### This class takes care of passing all information from lavaan tables definitions and estimations  from jamovi input
### to jamovi results tables. It wokrs using Syntax R6 class, Estimate R6 class, and Plotter R6 class.
### Syntax R6 class gets all input options and defines the tables required for showing the results. Estimate R6 class inherit from Syntax
### all properties of the tables and fill them with the actual results estimated with lavaan() function.
### Estimate inherit from Syntax, so only one instance of Estimate is defined. Here is called lav_machine
### Filling the results tables is handle by function in jamovi.R (functions starting with j.)
### Data are handled by a Datamatic R6 class, which does all transformations and checking required.


semljguiClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "semljguiClass",
    inherit = semljguiBase,
    private = list(
        .lav_machine=NULL,
        .data_machine=NULL,
        .plot_machine=NULL,
        .model=NULL,
        .ready=NULL,
        .init = function() {
            ginfo("init")
            
            ### output some syntax examples ####
            
            if (self$options$constraints_examples) {
                j.init_table(self$results$contraintsnotes,CONT_EXAMPLES,indent=-1)
                j.init_table_append(self$results$contraintsnotes,DP_EXAMPLES,indent=-1)
                j.init_table_append(self$results$contraintsnotes,SY_EXAMPLES,indent=-1)
                self$results$contraintsnotes$setNote(1,CONT_NOTE)
            }
            

            ### check that we have enough information to run ####
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
            ### prepare r2 table
#            j.init_table(self$results$models$r2,lav_machine$tab_r2,ci=T,ciwidth=self$options$ciWidth)
            
            #### parameter estimates table ####
            j.init_table(self$results$models$coefficients,lav_machine$tab_coefficients,ci=T,ciwidth=self$options$ciWidth)

            #### loadings table ####
            j.init_table(self$results$models$loadings,lav_machine$tab_loadings,ci=T,ciwidth=self$options$ciWidth)
            
            ### prepare var cov table ###
            j.init_table(self$results$models$correlations,lav_machine$tab_covariances,ci=T,ciwidth=self$options$ciWidth)
            

            ### prepare defined params ###
            j.init_table(self$results$models$defined,lav_machine$tab_defined,ci=T,ciwidth=self$options$ciWidth)

            ### prepare intercepts ###
            if (self$options$showintercepts & !is.null(lav_machine$tab_intercepts))
                 j.init_table(self$results$models$intercepts,lav_machine$tab_intercepts,ci=T,ciwidth=self$options$ciWidth)

            
            private$.lav_machine<-lav_machine
            private$.data_machine<-data_machine
            plot_machine$initPlots()
            private$.plot_machine<-plot_machine 
            
            
        },
    
        .run = function() {
            ginfo("run")
            ### check that we have enough information to run ####
            if (!private$.ready$ready)
                return()

            ### clean the data and prepare things ###
            lav_machine<-private$.lav_machine
            data<-private$.data_machine$cleandata(self$data)

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
             
             ### loadings vars and covars ####
            j.fill_table(self$results$models$correlations,lav_machine$tab_covariances)
            
#            j.fill_table(self$results$models$r2,lav_machine$tab_r2)
#            j.add_warnings(self$results$models$r2,lav_machine,"r2")
            
            j.fill_table(self$results$models$defined,lav_machine$tab_defined)
            j.add_warnings(self$results$models$defined,lav_machine,"defined")
            
            if (self$options$showintercepts & !is.null(lav_machine$tab_intercepts))
                   j.fill_table(self$results$models$intercepts,lav_machine$tab_intercepts)
            

            ## diagrams
            private$.plot_machine$preparePlots()   
            if (is.something(private$.plot_machine$warnings$diagram)) {
                 for (i in seq_along(private$.plot_machine$warnings$diagram))
                        self$results$pathgroup$notes$addRow(i,list(message=private$.plot_machine$warnings$diagram[[i]]))
                  self$results$pathgroup$notes$setVisible(TRUE)
            }
            
            self$results$.setModel(lav_machine$model)
        },
 
        .showDiagram=function(image,ggtheme, theme, ...) {
            if (self$options$diagram==FALSE) 
                return()
            if (!is.something(image$state$semModel))
                 return()
            options<-private$.plot_machine$semPathsOptions
            # semPlot::semPaths(object = image$state$semModel,
            #                   layout =options$layout,
            #                   residuals = options$residuals,
            #                   rotation = options$rotation,
            #                   intercepts = options$intercepts,
            #                   nodeLabels= options$nodeLabels,
            #                   whatLabels=options$whatLabels,
            #                   sizeMan = options$sizeMan,
            #                   sizeMan2=options$sizeMan2,
            #                   curve=options$curve,
            #                   shapeMan=options$shapeMan,
            #                   edge.label.cex =options$edge.label.cex)
            note<-FALSE
            
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


        .sourcifyOption = function(option) {
            return("")
        }
        
        
        
        
        
        )
)
