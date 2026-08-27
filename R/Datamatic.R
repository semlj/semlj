Datamatic <- R6::R6Class(
  "Datamatic",
  cloneable=FALSE,
  class=TRUE,
  inherit = Scaffold,
  public=list(
    vars=NULL,
    multigroup=list(),
    observed=NULL,
    cluster=NULL,
    ordered=NULL,
    varTable=NULL,
    missing=NULL,
    sample_n = list(),
    sample_mean = list(),
    sample_std = list(),
    initialize=function(jmvobj) {
      
      super$initialize(jmvobj)
      
      astring<-self$options$code
      reg<-"[<=~:+\n]"
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
      vars<-vars[sapply(vars,function(x) is.na(suppressWarnings(as.numeric(x))))]

      self$vars<-vars
      
      mg<-self$options$multigroup
      if (is.character(mg))
        if(trimws(mg)=="")
          mg<-NULL
      self$multigroup$var=mg
      
      ml<-self$options$cluster
      if (is.character(ml))
        if(trimws(ml)=="")
          ml<-NULL
      self$cluster<-ml
      
      self$missing<-self$options$missing
      
      private$.inspect_data()
    },
    
    cleandata=function() {
      
      jinfo("Cleaning the data")
      if (!self$ok) return()

      data<-self$analysis$data
      facts<-c(self$cluster,self$multigroup$var)
      vars<-setdiff(self$vars,facts)
      
      ### here handle standard data
      if (self$options$data_type == "data") {
        trans<-c()
        
        for (var in vars) {
          if (is.factor(data[[var]])) { 
            data[[var]]<-ordered(data[[var]])
            trans<-c(trans,var)
          }
        }
        if (is.something(trans))
          self$warning<-list(topic="info",
                            message=DATA_WARNS[["fac_to_ord"]] %<+% paste(unique(trans),collapse = ","))

      
        trans<-NULL
        for (var in facts) {
          if (!is.factor(data[[var]])) { 
            data[[var]]<-factor(data[[var]])
            trans<-c(trans,var)
          }
        }
        if (is.something(trans))
          self$warning<-list(topic="info",
                                       message=DATA_WARNS[["num_to_fac"]] %<+% paste(unique(trans),collapse = ","))
        

        if (self$missing=="listwise") {
          cdata<-jmvcore::naOmit(data)
          if (dim(data)[1] != dim(data)[1]) 
                        self$warning<-list(topic="info",
                                       message=DATA_WARNS[["missing"]])
     
          data<-cdata
        }
      } else { # end of standard dataset handling, if we get here, input is covs or cors
      
        sample_n <- self$options$sample_n
        sample_mean <- self$options$sample_mean
        sample_std <- self$options$sample_std

        sample_n_ok <- is.something(sample_n) && sample_n %in% names(data)
        sample_mean_ok <- is.something(sample_mean) && sample_mean %in% names(data)
        sample_std_ok <- is.something(sample_std) && sample_std %in% names(data)

        if (!sample_n_ok)
          self$stop("Estimation requires a valid column with sample sizes for covariance or correlation input.")

        matrix_warnings <- character()
        if (self$options$data_type == "cor" && !sample_std_ok)
          matrix_warnings <- c(matrix_warnings,
                               "Estimation requires covariances as input, please define a valid column with standard deviations to rescale correlations into covariances.")
        if (isTRUE(self$options$meanstructure) && !sample_mean_ok)
          matrix_warnings <- c(matrix_warnings,
                               "Mean structure requires a valid column with sample means when covariance or correlation matrices are supplied.")
        if (length(matrix_warnings) > 0)
          self$warning <- list(topic="issues", message=matrix_warnings, head="info")

        if (is.something(self$multigroup)) {
          cdata<-list()

          for ( x in self$multigroup$levels) {

              ldata<-data[data[[self$multigroup$var]]==x,]
              ## covariances
              xdata<-as.matrix(ldata[,self$observed])
              if (nrow(xdata) != length(self$observed))
                self$stop("Group " %+% x %+% ": Number of rows containing covariances is not equal to the number of observed variables.")

              L <- xdata * lower.tri(xdata, diag = TRUE)
              xdata <- L + t(L) - diag(diag(L))
              ## std
              if (self$options$data_type=="cor" && sample_std_ok) {
                    D<-diag(ldata[,sample_std])
                    xdata<-D %*% xdata %*% D
              }

              colnames(xdata)<-rownames(xdata)<-self$observed
              ladd(cdata)<-xdata
              ## N
              xdata<-ldata[,sample_n]
              ladd(self$sample_n)<-min(as.numeric(as.character(xdata)))
              ## means
              if (sample_mean_ok) {
                  xdata<-ldata[,sample_mean]
                  ladd(self$sample_mean)<-xdata
              }

          }

           names(cdata)<-self$multigroup$levels
           data<-cdata

      } else {
        ## here we do not have multigroup
        cdata<-as.matrix(data[,self$observed])
        if (nrow(cdata) != length(self$observed))
                self$stop("Number of rows containing covariances is not equal to the number of observed variables.")

        ## we accept also lower triangular
        L <- cdata * lower.tri(cdata, diag = TRUE)
        cdata <- L + t(L) - diag(diag(L))

        self$sample_n<-min(as.numeric(as.character(data[,sample_n])))

        if (sample_mean_ok)
                self$sample_mean<-as.vector(data[,sample_mean])

        if (self$options$data_type=="cor" && sample_std_ok) {
                   D<-diag(data[,sample_std])
                   cdata<-D %*% cdata %*% D
        }
         rownames(cdata)<-colnames(cdata)<-self$observed
      }
         data<-cdata

      } ### end of cov inputs
      # be sure there are not NaN 
      for ( x in names(data)) data[[x]][is.nan(data[[x]])]<-NA
      ## return data, whatever transformation has been done
    
      return(data)
    }

  ), ### end of public
  private=list(
    .inspect_data=function() {
       
        data<-self$analysis$data
        test<-(make.names(self$vars) %in% self$vars)

        if (!all(test)) {
          msg<-paste(self$vars[!test],collapse = ",")
          self$stop("Variable name not allowed for variables: " %+%
                        msg %+%
                        ". Please remove characters that are not letters, numbers, dot or underline. Letters may be defined differently in different locales.")
        }
        
        
        if (is.something(self$multigroup)) {
          var<-trimws(self$multigroup$var)
          if (!is.factor(data[[var]]))
            self$stop("Multigroup variable " %+% var %+% " should be a factor (nominal)")
          self$multigroup$levels<-levels(data[[var]])
          self$multigroup$nlevels<-length(levels(data[[var]]))
        }
        self$observed<-intersect(self$vars,names(data))
        if (length(self$observed)==0)
          self$stop("No observed variable in the dataset")
        
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

