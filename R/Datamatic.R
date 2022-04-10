Datamatic <- R6::R6Class(
  "Datamatic",
  cloneable=FALSE,
  class=TRUE,
  inherit = Scaffold,
  public=list(
    vars=NULL,
    multigroup=NULL,
    observed=NULL,
    cluster=NULL,
    ordered=NULL,
    varTable=NULL,
    
    initialize=function(options,dispatcher,data) {
      
      super$initialize(options,dispatcher)
      mark("Datamatic")
      astring<-options$code
      reg<-"[=~:+\n]"
      avec<-stringr::str_split(astring,reg)[[1]]
      avec<-avec[sapply(avec, function(a) a!="")]
      vars<-sapply(avec, function(a) trimws(stringr::str_remove(a,'.*[\\*]')))
      vars<-vars[grep("#",vars,fixed=T,invert = T)]
      self$vars<-vars
      
      mg<-options$multigroup
      if (is.character(mg))
        if(trimws(mg)=="")
          mg<-NULL
      self$multigroup=mg
      
      ml<-options$cluster
      if (is.character(ml))
        if(trimws(ml)=="")
          ml<-NULL
      self$cluster<-ml
      mark(self$cluster,is.something(self$cluster),class(self$cluster))
      private$.inspect_data(data)
      
      
    },
    
    cleandata=function(data) {
      
      trans<-c()
      for (var in self$vars) {
        
        if (is.factor(data[[var]])) { 
          data[[var]]<-ordered(data[[var]])
          trans<-c(trans,var)
        }
      }
      if (is.something(trans))
        self$dispatcher$warnings<-list(topic="info",
                            message=glue::glue(DATA_WARNS[["fac_to_ord"]],x=paste(unique(trans),collapse = ",")))
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
        observed<-self$observed[(!(self$observed %in% c(self$multigroup$var,self$cluster)))]
        self$ordered<-observed[sapply(observed, function(a) any(class(data[[a]]) %in% c("factor","ordered")))]
        if (is.something(self$ordered))
            mark(paste("ordered vars:" ,self$ordered))
        ### if ordered variables are present, we need to prepare the varTable to give information
        ### about the variables. Since we are in init, we do not have the full dataset, so
        ### varTable() will assign obs=0 and the variable will be ignored by lavaanify().
        ### We trick it to consider the variables anyway by setting obs=100. This does not
        ### influence the estimation, because lavaan() function will operate on the full
        ### dataset and so the number of observations will be correct at the end
        ### do not specify "ordered" in varTable() because it needs the data. Without
        ### the option "ordered" it takes the class of variable that it finds and work just fine
        ### for the factor to be ordered, we will make them so later on in cleandata()        
        
        if (is.something(self$ordered)) {
          self$varTable<-lavaan::varTable(data)
          self$varTable$type[self$varTable$type=="factor"]<-"ordered"
          self$varTable$nobs<-100
        }
        
      }

  ) #end of private
)

