
semljguiClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
  R6::R6Class(
    "semljguiClass",
    inherit = semljguiBase,
    private = list(
      .dispatcher = NULL,
      .data_machine = NULL,
      .runner_machine = NULL,
      .plotter_machine = NULL,
      .ready = NULL,
      .time = NULL,
      .smartObjs = list(),
      .init = function() {
        ginfo(paste("MODULE: SEMLj GUI  #### phase init  ####"))

        private$.time <- Sys.time()
        private$.ready <- readiness(self$options)
        if (!private$.ready$ready) {
          if (private$.ready$report) {
            self$results$info$addRow("info", list(info = "Setup", specs = private$.ready$reason))
          }
          return()
        }

        ### set up the R6 workhorse class
        dispatcher <- Dispatch$new(self$results)
        data_machine <- Datamatic$new(self$options, dispatcher, self$data)
        runner_machine <- Runner$new(self$options, dispatcher, data_machine)


        ### info table ###
        aSmartObj <- SmartTable$new(self$results$info, runner_machine)
        private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

        ## syntax examples table ###
        EXAMPLES <- c(CONT_EXAMPLES, DP_EXAMPLES)
        aSmartObj <- SmartTable$new(self$results$synexamples)
        aSmartObj$initSource(EXAMPLES)
        aSmartObj$indent <- c(-1, -11)
        private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)


        ## main fit table ###
        aSmartObj <- SmartTable$new(self$results$fit$main, runner_machine)
        private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

        ## constraints test table ###
        aSmartObj <- SmartTable$new(self$results$fit$constraints, runner_machine)
        aSmartObj$activateOnData<-TRUE
        aSmartObj$spaceBy <- "type"
        private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)


        ## fit basic indices table ###
        aSmartObj <- SmartTable$new(self$results$fit$indices, runner_machine)
        aSmartObj$ci("rmsea", self$options$ci_width)
        private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

        ## more fit indices table ###
        aSmartObj <- SmartTable$new(self$results$fit$moreindices, runner_machine)
        private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

        ## even more fit indices table ###

        aSmartObj <- SmartTable$new(self$results$fit$modelbaseline, runner_machine)
        private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

        ## R2 table ###

        aSmartObj <- SmartTable$new(self$results$fit$rsquared, runner_machine)
        aSmartObj$activated <- (self$options$r2 != "none")
        aSmartObj$spaceBy <- "lgroup"
        private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

        ## regression coefficients ###

        aSmartObj <- SmartTable$new(self$results$models$coefficients, runner_machine)
        aSmartObj$spaceBy <- "lgroup"
        aSmartObj$ci(NULL, self$options$ci_width)
        aSmartObj$activateOnData<-TRUE
        private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

        ## factor loadings table ###

        aSmartObj <- SmartTable$new(self$results$models$loadings, runner_machine)
        aSmartObj$spaceBy <- "lgroup"
        aSmartObj$ci(NULL, self$options$ci_width)
        aSmartObj$activateOnData<-TRUE
        private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

        ## factor composite table ###

        aSmartObj <- SmartTable$new(self$results$models$composites, runner_machine)
        aSmartObj$spaceBy <- "lgroup"
        aSmartObj$ci(NULL, self$options$ci_width)
        aSmartObj$activateOnData<-TRUE
        private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

        ## factor composites table ###

        aSmartObj <- SmartTable$new(self$results$models$covariances, runner_machine)
        aSmartObj$spaceBy <- "lgroup"
        aSmartObj$ci(NULL, self$options$ci_width)
        private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

        ## intercepts table ###

        aSmartObj <- SmartTable$new(self$results$models$intercepts, runner_machine)
        aSmartObj$spaceBy <- "lgroup"
        aSmartObj$ci(NULL, self$options$ci_width)
        aSmartObj$activateOnData<-TRUE
        private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

        ## defined parameters table ###

        aSmartObj <- SmartTable$new(self$results$models$defined, runner_machine)
        aSmartObj$ci(NULL, self$options$ci_width)
        aSmartObj$activateOnData<-TRUE
        private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

        ## raliability matrix table ###
        
        aSmartObj <- SmartTable$new(self$results$additional$reliability, runner_machine)
        aSmartObj$spaceBy <- "lgroup"
        aSmartObj$activated<-(self$options$reliability & is.something(runner_machine$latent))
        private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

        ## mardia matrix table ###
        
        aSmartObj <- SmartTable$new(self$results$additional$mardia, runner_machine)
        aSmartObj$spaceBy <- "lgroup"
        private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

        ## observed covariances table ###
        
        aSmartObj <- SmartTable$new(self$results$covariances$observed, runner_machine)
        aSmartObj$spaceBy <- "lgroup"
        aSmartObj$expandable <- TRUE
        aSmartObj$expandFrom <- 3
        private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

        ## implied covariances table ###
        
        aSmartObj <- SmartTable$new(self$results$covariances$implied, runner_machine)
        aSmartObj$spaceBy <- "lgroup"
        aSmartObj$expandable <- TRUE
        aSmartObj$expandFrom <- 3
        private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

        ## residuals covariances table ###
        
        aSmartObj <- SmartTable$new(self$results$covariances$residual, runner_machine)
        aSmartObj$spaceBy <- "lgroup"
        aSmartObj$expandable <- TRUE
        aSmartObj$expandFrom <- 3
        private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

        ## combined covariances table ###
        
        aSmartObj <- SmartTable$new(self$results$covariances$combined, runner_machine)
        aSmartObj$spaceBy <- "lgroup"
        aSmartObj$expandable <- TRUE
        aSmartObj$expandFrom <- 4
        private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

        ## latent covariances table ###
        aSmartObj <- SmartTable$new(self$results$covariances$latent, runner_machine)
        aSmartObj$spaceBy      <- "lgroup"
        aSmartObj$activated    <- ((self$options$cov.lv) & is.something(runner_machine$latent))
        aSmartObj$expandable   <- TRUE
        aSmartObj$expandFrom   <- 3
        private$.smartObjs     <- append_list(private$.smartObjs, aSmartObj)

        ## modification indices table ###
        aSmartObj <- SmartTable$new(self$results$modification$indices, runner_machine)
        aSmartObj$spaceBy      <- "lgroup"
        private$.smartObjs     <- append_list(private$.smartObjs, aSmartObj)
        
        if (1 == 0) {


          ### anova table ###
          aSmartObj <- SmartTable$new(self$results$main$anova, runner_machine)
          aSmartObj$spaceAt <- c(1, -2)
          private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

          ### estimates table ###
          aSmartObj <- SmartTable$new(self$results$main$coefficients, runner_machine)
          aSmartObj$ci("est", self$options$ci_width)
          aSmartObj$ci("beta", self$options$ci_width, label = greek_vector[["beta"]])
          private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

          ### contrasts code tables
          aSmartObj <- SmartArray$new(self$results$main$contrastCodeTables, runner_machine)
          aSmartObj$expandable <- TRUE
          private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

          ### intercept more info table ###

          aSmartObj <- SmartTable$new(self$results$main$intercept, runner_machine)
          private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

          ### effectsizes table ###

          aSmartObj <- SmartTable$new(self$results$main$effectsizes, runner_machine)
          aSmartObj$ci("est", self$options$ci_width)
          aSmartObj$spaceBy <- "effect"
          private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

          ### vcov table ###

          aSmartObj <- SmartTable$new(self$results$main$vcov, runner_machine)
          aSmartObj$expandable <- TRUE
          aSmartObj$expandFrom <- 2
          private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

          ## post hoc #####

          aSmartObj <- SmartArray$new(self$results$posthoc, runner_machine)
          aSmartObj$expandable <- TRUE
          aSmartObj$expandSuperTitle <- "Comparison"
          aSmartObj$ci("est", self$options$ci_width)
          private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

          aSmartObj <- SmartArray$new(self$results$posthocEffectSize, runner_machine)
          aSmartObj$activated <- (is.something(self$options$posthoc) & is.something(self$options$posthoces))
          aSmartObj$expandable <- TRUE
          aSmartObj$expandSuperTitle <- "Comparison"
          aSmartObj$ci("dm", self$options$ci_width)
          aSmartObj$ci("ds", self$options$ci_width)
          aSmartObj$ci("g", self$options$ci_width)
          #      aSmartObj$restNotes(self$options$dci==FALSE)
          private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

          ### estimate marginal means

          aSmartObj <- SmartArray$new(self$results$emmeans, runner_machine)
          aSmartObj$activated <- is.something(self$options$emmeans)
          aSmartObj$expandable <- TRUE
          aSmartObj$combineBelow <- "new!"
          aSmartObj$spaceBy <- "new!"
          aSmartObj$ci("est", self$options$ci_width)
          private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

          ### simple effects
          ##### anova
          aSmartObj <- SmartTable$new(self$results$simpleEffects$anova, runner_machine)
          aSmartObj$activated <- (is.something(self$options$simple_effects) & is.something(self$options$simple_moderators))
          aSmartObj$expandable <- TRUE
          aSmartObj$expandSuperTitle <- "Moderator"
          aSmartObj$key <- self$options$simple_effects
          aSmartObj$combineBelow <- 1:(length(self$options$simple_moderators) - 1)
          aSmartObj$spaceBy <- (length(self$options$simple_moderators) - 1)

          private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

          ##### coefficients
          aSmartObj <- SmartTable$new(self$results$simpleEffects$coefficients, runner_machine)
          aSmartObj$activated <- (is.something(self$options$simple_effects) & is.something(self$options$simple_moderators))
          aSmartObj$expandable <- TRUE
          aSmartObj$expandSuperTitle <- "Moderator"
          aSmartObj$key <- self$options$simple_effects
          aSmartObj$ci("est", self$options$ci_width)
          aSmartObj$combineBelow <- 1:(length(self$options$simple_moderators) - 1)
          aSmartObj$spaceBy <- (length(self$options$simple_moderators) - 1)
          private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

          ### simple interaction
          aSmartObj <- SmartArray$new(self$results$simpleInteractions, runner_machine)
          aSmartObj$activated <- (self$options$simple_interactions & is.something(self$options$simple_effects) & length(self$options$simple_moderators) > 1)
          aSmartObj$expandable <- TRUE
          aSmartObj$expandSuperTitle <- "Moderator"
          aSmartObj$ci("est", self$options$ci_width)
          aSmartObj$combineBelow <- "new!"
          aSmartObj$spaceBy <- "new!"
          private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

          ### assumptions hometest
          aSmartObj <- SmartTable$new(self$results$assumptions$homotest, runner_machine)
          private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)
          ### assumptions nromtest
          aSmartObj <- SmartTable$new(self$results$assumptions$normtest, runner_machine)
          private$.smartObjs <- append_list(private$.smartObjs, aSmartObj)

          ### init all ####
        }

        for (tab in private$.smartObjs) {
          tab$initTable()
        }


        private$.data_machine <- data_machine
        private$.runner_machine <- runner_machine

        ######## plotting class #######
        #    plotter_machine<-Plotter$new(self$options,runner_machine,self$results)
        #    plotter_machine$initPlots()
        #    private$.plotter_machine<-plotter_machine

        now <- Sys.time()
        ginfo("INIT TIME:", now - private$.time, " secs")
      },
      .run = function() {
        ginfo("MODULE:  #### phase run ####")

        if (self$options$donotrun) {
          return()
        }

        private$.ready <- readiness(self$options)
        if (!private$.ready$ready) {
          return()
        }
        runnow <- Sys.time()
        data <- private$.data_machine$cleandata(self$data)
        private$.runner_machine$estimate(data)

        ### run tables ###

        for (smarttab in private$.smartObjs) {
          smarttab$runTable()
        }



        #      private$.checkpoint()

        # #save model preds and resids
        # private$.runner_machine$savePredRes(self$results)
        #
        # private$.plotter_machine$preparePlots()
        #
        # if ("plot" %in% private$.plotter_machine$dispatcher$warnings_topics) {
        #     self$results$plotnotes$setContent(paste(private$.plotter_machine$dispatcher$warnings[["plot"]],collapse = "; "))
        #     self$results$plotnotes$setVisible(TRUE)
        # }


        ginfo("MODULE:  #### phase end ####")

        ginfo("RUN TIME:", Sys.time() - runnow, " secs")

        ginfo("TIME:", Sys.time() - private$.time, " secs")

        return()
      },
      .sourcifyOption = function(option) {

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
