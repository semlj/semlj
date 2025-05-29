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
    missing=NULL,
    
    initialize=function(jmvobj) {
      
      super$initialize(jmvobj)
      
      astring<-self$options$code
      reg<-"[=~:+\n]"
      ## split by syntax operators
      avec<-stringr::str_split(astring,reg)[[1]]
      ## remove empty lines
      avec<-avec[sapply(avec, function(a) a!="")]
      ## remove product operator
      vars<-sapply(avec, function(a) trimws(stringr::str_remove(a,'.*[\\*]')))
      ## remove comments
      vars<-vars[grep("#",vars,fixed=T,invert = T)]
      vars<-vars[sapply(vars,function(x) x!="")]
      ## remove constraints numeric values
      vars<-vars[sapply(vars,function(x) is.na(as.numeric(x)))]

      self$vars<-vars
      
      mg<-self$options$multigroup
      if (is.character(mg))
        if(trimws(mg)=="")
          mg<-NULL
      self$multigroup=mg
      
      ml<-self$options$cluster
      if (is.character(ml))
        if(trimws(ml)=="")
          ml<-NULL
      self$cluster<-ml
      
      self$missing<-self$options$missing
      
      private$.inspect_data()
    },
    
    cleandata=function() {
      
      data<-self$analysis$data
      trans<-c()
      facts<-c(self$cluster,self$multigroup$var)
      vars<-setdiff(self$vars,facts)
      
      for (var in vars) {
        if (is.factor(data[[var]])) { 
          data[[var]]<-ordered(data[[var]])

          trans<-c(trans,var)
        }
      }
      if (is.something(trans))
        self$warning<-list(topic="info",
                            message=glue::glue(DATA_WARNS[["fac_to_ord"]],x=paste(unique(trans),collapse = ",")))

      
      trans<-NULL
      for (var in facts) {
      if (!is.factor(data[[var]])) { 
          data[[var]]<-factor(data[[var]])
          trans<-c(trans,var)
      }
      }
      if (is.something(trans))
        self$warning<-list(topic="info",
                                       message=glue::glue(DATA_WARNS[["num_to_fac"]],x=paste(unique(trans),collapse = ",")))
      
      if (self$missing=="listwise") {
        cdata<-jmvcore::naOmit(data)
      
        if (dim(cdata)[1] != dim(data)[1]) 
                        self$warning<-list(topic="info",
                                       message=DATA_WARNS[["missing"]])
     
        return(cdata)
      }

      return(data)
      
    }

  ), ### end of public
  private=list(
    .inspect_data=function() {
       
        data<-self$analysis$data
        
        test<-(make.names(self$vars) %in% self$vars)

        if (!all(test)) {
          msg<-paste(self$vars[!test],collapse = ",")
          self$error <-  list(topic="info", message=paste0(
                        "Variable name not allowed for variables: ",
                        msg,
                        ". Please remove characters that are not letters, numbers, dot or underline. Letters may be defined differently in different locales."),
                        final=TRUE)
        }
        
        
        if (is.something(self$multigroup)) {
          var<-trimws(self$multigroup)
          levels<-levels(data[,var])
          self$multigroup<-list(var=var,levels=levels,nlevels=length(levels))
        }
        self$observed<-intersect(self$vars,names(data))
        if (length(self$observed)==0)
          self$error <-  list(topic="info", message="No observed variable in the dataset",final=TRUE)
        
        observed<-self$observed[(!(self$observed %in% c(self$multigroup$var,self$cluster)))]
        self$ordered<-observed[sapply(observed, function(a) any(class(data[[a]]) %in% c("factor","ordered")))]

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

