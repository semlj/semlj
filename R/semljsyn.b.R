
semljsynClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "semljsynClass",
    inherit = semljsynBase,
    private = list(
      .data_machine = NULL,
      .runner_machine = NULL,
      .plotter_machine = NULL,
      .ready = NULL,
      .time = NULL,
      .smartObjs = list(),
      .init = function() {
        jinfo(paste("MODULE: SEMLj SYNTAX  #### phase init  ####"))

        private$.time <- Sys.time()
        private$.ready <- readiness(self$options)
        if (!private$.ready$ready) {
          if (private$.ready$report) {
            self$results$info$addRow("info", list(info = "Setup", specs = private$.ready$reason))
          }
          return()
        }

        ### set up the R6 workhorse class

        data_machine             <- Datamatic$new(self)
        runner_machine           <- Runner$new(self,data_machine)

        ### info table ###
        aSmartObj                <- SmartTable$new(self$results$info, runner_machine)
        ladd(private$.smartObjs) <- aSmartObj

        ## syntax examples table ###
        EXAMPLES                  <- c(CONT_EXAMPLES, DP_EXAMPLES)
        aSmartObj                 <- SmartTable$new(self$results$synexamples)
        aSmartObj$initSource      <- EXAMPLES
        aSmartObj$indent          <- c(-1, -11)
        ladd(private$.smartObjs)  <- aSmartObj


        ## main fit table ###
        aSmartObj                 <- SmartTable$new(self$results$fit$main, runner_machine)
        ladd(private$.smartObjs)  <- aSmartObj

        ## constraints test table ###
        aSmartObj                 <- SmartTable$new(self$results$fit$constraints, runner_machine)
        aSmartObj$activateOnData  <-TRUE
        aSmartObj$spaceBy         <- "type"
        ladd(private$.smartObjs)  <- aSmartObj


        ## fit basic indices table ###
        aSmartObj                 <- SmartTable$new(self$results$fit$indices, runner_machine)
        aSmartObj$ci("rmsea", self$options$ci_width)
        if (runner_machine$moretests)
            aSmartObj$setColumnVisible<-"type"     
        ladd(private$.smartObjs)    <- aSmartObj

        ## more fit indices table ###
        aSmartObj                 <- SmartTable$new(self$results$fit$moreindices, runner_machine)
        ladd(private$.smartObjs)  <- aSmartObj

        ## even more fit indices table ###

        aSmartObj                 <- SmartTable$new(self$results$fit$modelbaseline, runner_machine)
        ladd(private$.smartObjs)  <- aSmartObj

        ## R2 table ###

        aSmartObj                 <- SmartTable$new(self$results$fit$rsquared, runner_machine)
        aSmartObj$activated       <- (self$options$r2 != "none")
        aSmartObj$spaceBy         <- "lgroup"
        ladd(private$.smartObjs)  <- aSmartObj

        ## icc table ###
        
        aSmartObj                  <- SmartTable$new(self$results$fit$icc, runner_machine)
        aSmartObj$spaceBy          <- "lgroup"
        aSmartObj$activated        <-(self$options$icc & is.something(data_machine$cluster))
        ladd(private$.smartObjs)   <- aSmartObj
        
        ## regression coefficients ###

        aSmartObj                 <- SmartTable$new(self$results$models$coefficients, runner_machine)
        aSmartObj$spaceBy         <- "lgroup"
        aSmartObj$ci(NULL, self$options$ci_width)
        aSmartObj$ci("std", self$options$ci_width, label="β")
        aSmartObj$activateOnData  <-  TRUE
        ladd(private$.smartObjs)  <- aSmartObj

        ## factor loadings table ###

        aSmartObj                  <- SmartTable$new(self$results$models$loadings, runner_machine)
        aSmartObj$spaceBy          <- "lgroup"
        aSmartObj$ci(NULL, self$options$ci_width)
        aSmartObj$ci("std", self$options$ci_width, label="β")
        aSmartObj$activateOnData   <-TRUE
        ladd(private$.smartObjs)   <- aSmartObj

        ## factor composite table ###

        aSmartObj                  <- SmartTable$new(self$results$models$composites, runner_machine)
        aSmartObj$spaceBy          <- "lgroup"
        aSmartObj$ci(NULL, self$options$ci_width)
        aSmartObj$ci("std", self$options$ci_width, label="β")
        aSmartObj$activateOnData   <- TRUE
        ladd(private$.smartObjs)   <- aSmartObj

        ## factor composites table ###

        aSmartObj                  <- SmartTable$new(self$results$models$covariances, runner_machine)
        aSmartObj$spaceBy          <- "lgroup"
        aSmartObj$ci(NULL, self$options$ci_width)
        aSmartObj$ci("std", self$options$ci_width, label="β")
        ladd(private$.smartObjs)   <- aSmartObj

        ## intercepts table ###

        aSmartObj                  <- SmartTable$new(self$results$models$intercepts, runner_machine)
        aSmartObj$spaceBy          <- "lgroup"
        aSmartObj$ci(NULL, self$options$ci_width)
        aSmartObj$activateOnData   <-TRUE
        ladd(private$.smartObjs)   <-  aSmartObj

        ## thresholds table ###
        
        aSmartObj                  <- SmartTable$new(self$results$models$thresholds, runner_machine)
        aSmartObj$spaceBy          <- c("lgroup","level","lhs")
        aSmartObj$ci(NULL, self$options$ci_width)
        aSmartObj$activated        <- is.something(data_machine$ordered)
        ladd(private$.smartObjs)   <- aSmartObj
        

        ## mlmeans table ###
        
        aSmartObj                  <- SmartTable$new(self$results$models$mlmeans, runner_machine)
        aSmartObj$spaceBy          <- c("lgroup","level")
        aSmartObj$activated        <- (self$options$mlmeans & is.something(runner_machine$cluster))
        ladd(private$.smartObjs)   <- aSmartObj
        
        ## defined parameters table ###

        aSmartObj                  <- SmartTable$new(self$results$models$defined, runner_machine)
        aSmartObj$ci(NULL, self$options$ci_width)
        aSmartObj$ci("std", self$options$ci_width, label="β")
        aSmartObj$activateOnData   <-TRUE
        ladd(private$.smartObjs)   <- aSmartObj

        
        ## raliability matrix table ###
        
        aSmartObj                  <- SmartTable$new(self$results$additional$reliability, runner_machine)
        aSmartObj$spaceBy          <- "lgroup"
        aSmartObj$activated        <-(self$options$reliability & is.something(runner_machine$latent))
        if (is.something(runner_machine$ordered))
          aSmartObj$setColumnVisible <- "alpha.ord"
        ladd(private$.smartObjs)     <- aSmartObj

        ## htmt matrix table ###
        
        aSmartObj                  <- SmartTable$new(self$results$additional$htmt, runner_machine)
        aSmartObj$expandOnInit     <- TRUE
        aSmartObj$expandFrom       <- 2
        ladd(private$.smartObjs)   <- aSmartObj
        
        
        
        
        ## mardia matrix table ###
        
        aSmartObj                  <- SmartTable$new(self$results$additional$mardia, runner_machine)
        aSmartObj$spaceBy          <- "lgroup"
        ladd(private$.smartObjs)   <- aSmartObj

        ## observed covariances table ###
        
        aSmartObj                  <- SmartArray$new(self$results$covariances$observed, runner_machine)
        aSmartObj$expandOnInit     <- TRUE
        aSmartObj$expandOnRun      <- TRUE
        aSmartObj$expandFrom       <- 2
        ladd(private$.smartObjs)   <- aSmartObj

        ## implied covariances table ###
        
        aSmartObj                  <- SmartArray$new(self$results$covariances$implied, runner_machine)
        aSmartObj$expandOnInit     <- TRUE
        aSmartObj$expandOnRun      <- TRUE
        aSmartObj$expandFrom       <- 2
        ladd(private$.smartObjs)   <- aSmartObj
        
        ## residuals covariances table ###
        
        aSmartObj                  <- SmartArray$new(self$results$covariances$residual, runner_machine)
        aSmartObj$expandOnInit     <- TRUE
        aSmartObj$expandOnRun      <- TRUE
        aSmartObj$expandFrom       <- 2
        ladd(private$.smartObjs)   <- aSmartObj
        

        ## latent covariances table ###
        aSmartObj                  <- SmartArray$new(self$results$covariances$latent, runner_machine)
        aSmartObj$expandOnInit     <- TRUE
        aSmartObj$expandOnRun      <- TRUE
        aSmartObj$expandFrom       <- 2
        ladd(private$.smartObjs)   <- aSmartObj
        
        ## modification indices table ###
        aSmartObj                  <- SmartTable$new(self$results$modification$indices, runner_machine)
        aSmartObj$spaceBy          <- "lgroup"
        ladd(private$.smartObjs)   <- aSmartObj



        for (tab in private$.smartObjs) {
          tab$initTable()
        }


        private$.data_machine <- data_machine
        private$.runner_machine <- runner_machine

        ######## plotting class #######
        private$.plotter_machine<-Plotter$new(self,runner_machine,self$results$pathgroup)
        private$.plotter_machine$initPlots()

        now <- Sys.time()
        jinfo("MEM: (end init)",pryr::mem_used()/10e5)
        jinfo("INIT TIME:", now - private$.time, " secs")
        
      },
      .run = function() {
        jinfo("MODULE:  #### phase run ####")

        if (is.something(self$options$donotrun)) {
          if (self$options$donotrun) 
            return()
        }
        
        private$.ready <- readiness(self$options)
        if (!private$.ready$ready) {
          return()
        }
        runnow <- Sys.time()
        data <- private$.data_machine$cleandata()
        jinfo("MEM: (before estimate)",pryr::mem_used()/10e5)
        private$.runner_machine$estimate(data)
        jinfo("MEM: (after estimate)",pryr::mem_used()/10e5)
        
        ### save predicted if needed
        private$.runner_machine$savePredRes(self$results,data)
        
        
        ### run tables ###

        for (smarttab in private$.smartObjs) {
          smarttab$runTable()
        }
        jinfo("MEM: (after table run)",pryr::mem_used()/10e5)
        
        private$.plotter_machine$preparePlots()   
        if (is.something(private$.plotter_machine$warnings$diagram)) {
          for (i in seq_along(private$.plotter_machine$warnings$diagram))
            self$results$pathgroup$notes$addRow(i,list(message=private$.plotter_machine$warnings$diagram[[i]]))
          self$results$pathgroup$notes$setVisible(TRUE)
        }
        

      
        jinfo("MODULE:  #### phase end ####")

        jinfo("RUN TIME:", Sys.time() - runnow, " secs")

        jinfo("TIME:", Sys.time() - private$.time, " secs")
        
        jinfo("MEM:", pryr::mem_used()/10e5)
        

        return()
      },

      .showDiagram=function(image,ggtheme, theme, ...) {
        
          
          if (self$options$diagram==FALSE) 
              return()
          if (!is.something(image$state$semModel))
              return()
          options<-private$.plotter_machine$semPathsOptions
        
              # leave this here for future reference
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
                                        ,thresholds = FALSE
                      
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
              private$.plotter_machine$dispatcher$warnings<-list(topic="pathgroup_notes",message=res$error)
          }
          if (!isFALSE(res$warning)) {
            private$.plotter_machine$dispatcher$warnings<-list(topic="pathgroup_notes",message=res$warning)
          }
  
          return(TRUE)
  
      },

      .sourcifyOption = function(option) {
        return("")
        # skip<-c("modelTerms","factors","covs","dep")
        # defaults<-c(scaling="centered",contrasts="simple")
        #
        # if (option$name %in% skip)
        #     return('')
        # sourcifyOption(option,defaults)
      }
    )
  )
}
