## This class prepares the pathdiagram. It does not produce the actual diagram to avoid
## issues in Windows with semPaths(). The actual diagram is produced in the .rederFun " in .b.R file.

Plotter <- R6::R6Class(
  "Plotter",
  cloneable=FALSE,
  class=FALSE,
  inherit = Dispatch,
  public=list(
      semPathsOptions=NULL,
      initialize=function(options,datamatic,operator,resultsplots) {
            astring<-options$code
            reg<-"[=~:+\n]"
            avec<-stringr::str_split(astring,reg)[[1]]
            avec<-avec[sapply(avec, function(a) a!="")]
            vars<-sapply(avec, function(a) stringr::str_remove(a,'.?[*]'))
        
            super$initialize(options=options,vars=vars)
            private$.plotgroup<-resultsplots
            private$.operator<-operator
            private$.datamatic<-datamatic
      },

      initPlots=function() {
           if (!self$options$diagram)
               return()

           if (is.something(private$.datamatic$multigroup))  {
                for (level in private$.datamatic$multigroup$levels) {
                     title <- paste(private$.datamatic$multigroup$var, "=", level)
                     private$.plotgroup$diagrams$addItem(level)
                     private$.plotgroup$diagrams$get(key = level)$setTitle(title)
                }
           } else {
                private$.plotgroup$diagrams$addItem("0")
                private$.plotgroup$diagrams$get(key = "0")$setTitle("")
      }
      
      },
      preparePlots=function(image, ggtheme, theme, ...) {
        
        if (!self$options$diagram)
          return()
        
        model<-private$.operator$model
        ### We need to clean the model because semPaths seems to fail when an inequality constraints is in ###
        ### check for more lavaanian solution ###
        check<-grep(">|<",model@ParTable$op,invert = T)
        par<-model@ParTable
        model@ParTable<-sapply(par, function(x) x[check],simplify = F)
        ### end ###
      
        
        ## prepare the semPlotModel object to be passed to .rendefun where
        ## semPaths will do the actual diagrams
        
        ## At the moment (mid-2021) semPaths invokes the graphical device even when DoNotPlot is TRUE,
        ## which causes a window to pop up in windows and mac when run in jamovi. So, we need to prepare a semPlot object 
        ## here and make the actual plot in the .renderfun of .b.R with semPaths. 
        ## for multigroup, we trick the .renderfun semPaths() to believe that there's always only one group
        ## per call, because the .renderfun is called for each group.
        
        
        sm<-semPlot::semPlotModel(model)
        groups<-unique(sm@Pars$group)
        images<-private$.plotgroup$diagrams

        if (all(groups=="")) {
          image<-images$get(key = "0")
          image$setState(list(semModel = sm))
          } else {
          for (i in seq_along(images$itemKeys)) {
            image<-images$get(key = images$itemKeys[[i]])
            .sm<-sm
            .sm@Pars<-.sm@Pars[.sm@Pars$group==groups[i],]
             image$setState(list(semModel = .sm))
          }

          }

        ### images are set ####
        
        ## now we prepare the options for semPaths

        labs<-self$options$diag_paths
        nodeLabels<-model@pta$vnames$ov.num[[1]]

        size<-8
        if (self$options$diag_labsize=="small") size<-6
        if (self$options$diag_labsize=="large") size<-14
        if (self$options$diag_labsize=="vlarge") size<-18
        
        nNodes<-length(nodeLabels)
        size<-size*exp(-nNodes/80)+1
        
        layout<-self$options$diag_type
        rotation<-as.numeric(self$options$diag_rotate)
        if (layout %in% c("circle") & rotation %in% c(2,4)) {
            rotation<-1
            self$warnings<-list(topic="diagram",message=PLOT_WARNS[["rotation"]])
        }


        self$semPathsOptions<-list(
                      layout = layout
                      ,whatLabels=labs
                      ,residuals = self$options$diag_resid
                      ,intercepts= self$options$diag_intercepts
                      ,rotation = rotation 
                      ,intercepts = F
                      ,sizeLat = size
                      ,sizeLat2 = size*.50
                      ,sizeMan=size*.70
                      ,sizeMan2=size*.35
                      , curve=2
                      , nCharNodes=as.numeric(self$options$diag_abbrev)
                      , shapeMan=self$options$diag_shape_man
                      , shapeLat=self$options$diag_shape_lat
                      
                      ,edge.label.cex =1.3)
        

        
        return()
      }
      
      
  ), # end of public
  private = list(
    .diagrams=NULL,
    .datamatic=FALSE,
    .plotgroup=NULL,
    .operator=NULL
  ) # end of private
) # end of class
    