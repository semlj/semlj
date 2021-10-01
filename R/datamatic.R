Datamatic <- R6::R6Class(
  "Datamatic",
  cloneable=FALSE,
  class=FALSE,
  inherit = Dispatch,
  public=list(
    multigroup=NULL,
    observed=NULL,
    cluster=NULL,
    initialize=function(options,data) {
      astring<-options$code
      reg<-"[=~:+\n]"
      avec<-stringr::str_split(astring,reg)[[1]]
      avec<-avec[sapply(avec, function(a) a!="")]
      vars<-sapply(avec, function(a) trimws(stringr::str_remove(a,'.*[\\*]')))
      vars<-vars[grep("#",vars,fixed=T,invert = T)]
      super$initialize(options=options,vars=vars)
      mg<-options$multigroup
      if (is.character(mg))
             if(trimws(mg)=="")
                   mg<-NULL
      self$multigroup=mg
      ml<-options$cluster
      if (is.character(ml))
        if(trimws(ml)=="")
          ml<-NULL
      self$cluster=ml
      
      private$.inspect_data(data)
      

    },
    cleandata=function(data) {
      
      for (var in self$vars) {
        if (is.factor(data[[var]]))
            if (length(levels(data[[var]]))>2) {
                self$errors<-paste("Variable",var, "is a factor with more than two levels, it cannot be analyzed")
                return()
            }
            if (length(levels(data[[var]]))==2) {
              self$warnings<-list(topic="info",message=paste("Variable",var, "is a factor with two levels, it has been converted to continuous type"))
              avar<-data[[var]]
              levels(avar)<-1:nlevels(avar)
              data[[var]]<-as.numeric(a)-1
            } else
                 data[[var]]<-jmvcore::toNumeric(data[[var]])
      }
      
      return(data)
      
      
      
    }      
  
    ), ### end of public
   private=list(
      .inspect_data=function(data) {
        if (is.something(self$multigroup)) {
          var<-trimws(self$multigroup)
          levels<-levels(data[,var])
          self$multigroup<-list(var=var,levels=levels,nlevels=length(levels))
        }
        self$observed<-intersect(self$vars,names(data))
      }
     
   ) #end of private
)