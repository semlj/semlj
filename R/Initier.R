

Initer <- R6::R6Class(
  "Initer",
  class=TRUE, 
  cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
  inherit = Scaffold,
  public=list(
    datamatic=NULL,
    endogenous=NULL,
    observed=NULL,
    latent=NULL,
    ordered=NULL,
    multigroup=NULL,
    customgroups=FALSE,
    cluster=NULL,
    varTable=NULL,
    user_syntax=NULL,
    
    initialize=function(options,dispatcher,datamatic) {
      
      super$initialize(options,dispatcher)
      self$datamatic<-datamatic

      self$observed    <- datamatic$observed
      self$multigroup  <- datamatic$multigroup
      self$cluster     <- datamatic$cluster
      self$ordered     <- datamatic$ordered
      self$varTable    <- datamatic$varTable
      # check_* check the input options and produces tables and list with names
      ### prepare list of models for lavaan
      private$.check_models()
      
      ## check and build indirect effect (if requires)
      private$.indirect()
      
      ## build the full structure from where all tables will be inited
      private$.make_structure()

    }, # here initialize ends
    #### init functions #####
    
    init_info=function() {
      
      ### info contains the info table, with some loose information about the model
      alist<-list()
      alist[[length(alist)+1]]<-c(info="Estimation Method",value=self$options$estimator)
      alist[[length(alist)+1]]<-c(info="Optimization Method",value="")
      alist[[length(alist)+1]]<-c(info="Number of observations",value="") 
      alist[[length(alist)+1]]<-c(info="Free parameters",value=max(private$.lav_structure$free))
      alist[[length(alist)+1]]<-c(info="Standard errors",value="")
      alist[[length(alist)+1]]<-c(info="Scaled Test",value="")
      alist[[length(alist)+1]]<-c(info="Converged", value="") 
      alist[[length(alist)+1]]<-c(info="Iterations", value="") 
      alist[[length(alist)+1]]<-c(info="",value="")
      
      for (m in self$user_syntax)
        alist[[length(alist)+1]]<-c(info="Model",value=m)

      alist[[length(alist)+1]]<-c(info="",value="")
      
      if (is.something(self$options$cluster)) {
         alist[[length(alist)+1]]<-c(info="",value="")
         alist[[length(alist)+1]]<-c(info="Cluster variable",value=self$options$cluster)
      }
      
      if (is.something(self$options$multigroup)) {
        alist[[length(alist)+1]]<-c(info="Multi-group variable",value=self$options$multigroup)
      }
      
      
      if (self$option("se","boot"))
          self$dispatcher$warnings<-list(topic="info",message="Bootstrap confidence intervals may take a while. Please be patient.",init=TRUE)
      
      return(alist)
    },
    init_fit_main=function() {

      tab<-list(list(label="User Model"),list(label="Baseline Model"))
      
      if (self$options$estimator %in% ROBUST_ESTIM) {
           tab[[3]]<-list(label="Scaled User")
           tab[[4]]<-list(label="Scaled Baseline")
      }
      
      return(tab)      
    },
    
    init_fit_modelbaseline=function() {
      
    alist<-list()
    alist[[length(alist) + 1]]  <- list(name = "Comparative Fit Index (CFI)");
    alist[[length(alist) + 1]]  <- list(name = "Tucker-Lewis Index (TLI)");
    alist[[length(alist) + 1]]  <- list(name = "Bentler-Bonett Non-normed Fit Index (NNFI)");
    alist[[length(alist) + 1]]  <- list(name = "Bentler-Bonett Normed Fit Index (NFI)");
    alist[[length(alist) + 1]]  <- list(name = "Parsimony Normed Fit Index (PNFI)");
    alist[[length(alist) + 1]]  <- list(name = "Bollen's Relative Fit Index (RFI)");
    alist[[length(alist) + 1]]  <- list(name = "Bollen's Incremental Fit Index (IFI)");
    alist[[length(alist) + 1]]  <- list(name = "Relative Noncentrality Index (RNI)");
    
    return(alist)
    },
    init_fit_moreindices=function() {
      
      
      tab<-lapply(1:5,function(a) list(name="."))
      if (self$options$estimator %in% INFO_ML) {
          tab[[6]]<-list(name=".")
          tab[[7]]<-list(name=".")
      }
      

      return(tab)      
    },
    
    init_fit_constraints=function() {
      
      what<-c("==","<",">")
      tab<-private$.lav_structure[private$.lav_structure$op %in% what,]
      
      if (nrow(tab)==0) return(NULL)
      
      tab1<-tab2<-tab3<-NULL
      if (self$option("scoretest")) {
              tab1<-tab
              tab1$type="Univariate"
      }
      if (self$option("cumscoretest")) {
        tab2<-tab
        tab2$type="Cumulative"
      }
      
      tab3<-tab[1,]
      tab3$type<-"Total"
      tab3$lhs<-tab3$op<-tab3$rhs<-""
      tab<-rbind(tab1,tab2,tab3)      
      return(tab)
      
    },
    init_fit_indices=function() {
      
      tab<-list(list(type="Classical"))
      
      if (self$options$estimator %in% ROBUST_ESTIM) {
        tab[[2]]<-list(type="Robust")
        tab[[3]]<-list(type="Scaled")
      }      
      return(tab)    
    },
    init_fit_rsquared=function() {
      
      if (self$option("r2","endo")) 
         vars<-private$.lav_structure$lhs[private$.lav_structure$op=="~"]
      else
         vars<-self$observed
      
      if (is.something(vars)) {
        amat<-as.data.frame(matrix(vars,nrow = length(vars),ncol=1))
        names(amat)<-"rhs"
        amat<-private$.make_empty_table(amat)
        return(amat)
      }
      ### return a row anyway to make the warning appear as a note
      return(list(list(rhs=" ")))

    },
    init_models_coefficients=function() {
      
      tab<-private$.lav_structure[private$.lav_structure$op=="~",]
      if (nrow(tab)==0) tab<-NULL
      return(tab)
      
    },
    init_models_loadings=function() {
      
      tab<-private$.lav_structure[private$.lav_structure$op=="=~",]
      if (nrow(tab)==0) tab<-NULL
      return(tab)
      
    },
    init_models_composites=function() {
      
      tab<-private$.lav_structure[private$.lav_structure$op=="<~",]
      if (nrow(tab)==0) tab<-NULL
      return(tab)
      
    },
    init_models_covariances=function() {
      
      tab<-private$.lav_structure[private$.lav_structure$op=="~~",]
      if (nrow(tab)==0) tab<-NULL
      return(tab)
      
    },
    init_models_intercepts=function() {
      
      tab<-private$.lav_structure[private$.lav_structure$op=="~1",]
      if (nrow(tab)==0) tab<-NULL
      return(tab)
      
    },
    init_models_thresholds=function() {
      
      tab<-private$.lav_structure[private$.lav_structure$op=="|",]
      if (nrow(tab)==0) tab<-NULL
      return(tab)
      
    },
    init_models_defined=function() {

      tab<-private$.lav_structure[private$.lav_structure$op==":=",]
### here we give a human description of the defined parameters      
      if (nrow(tab)>0) {
        
        tab$desc<-""
        .structure<-private$.lav_structure[private$.lav_structure$op %in% c("=~", "~","~~","~1","|","<~"),]
        
        for (i in seq_along(tab$rhs)) {
          r<-tab$rhs[i]
          if (r %in% names(self$indirect_names))
            r<-self$indirect_names[[r]]
          else {
            for (j in 1:nrow(.structure)) {
              arow<-.structure[j,]
              group<-arow$group
              if (is.something(self$multigroup))  groupsub<-SUB[[group]] else groupsub<-""
              target<-paste0(" (",arow$lhs,arow$op,arow$rhs,") ",groupsub)
              reg<-paste0(arow$label,"(?![0-9])")
              r<-stringr::str_replace(r,reg,target)
            }
          }
          tab$desc[i]<-r
        }
        if (is.something(self$multigroup)) {
          msg<-paste(1:self$multigroup$nlevels,self$multigroup$levels,sep="= group ",collapse = ", ")
          msg<-paste("Description subscripts refer to groups, with",msg)
          self$dispatcher$warnings<-list(topic="models_defined",message=msg) 
        }
      }
      if (nrow(tab)==0) tab<-NULL
      return(tab)
      
    },
    init_additional_reliability=function() {

       tab <- as.data.frame(matrix(self$latent,ncol=1))
       names(tab)<-c("variable")
       private$.make_empty_table(tab) 

    },
    init_covariances_observed=function() {
     
      .length <- length(self$observed)
       tab <- cbind(variable=self$observed, as.data.frame(matrix(".", ncol=.length, nrow=.length, dimnames=list(NULL, self$observed))));
       names(tab)<-c("variable",self$observed)
       tab<-private$.make_empty_table(tab)
       return(tab)
    },
    init_covariances_implied=function() {
  
      .length <- length(self$observed)
       tab <- cbind(variable=self$observed, as.data.frame(matrix(".", ncol=.length, nrow=.length, dimnames=list(NULL, self$observed))));
       names(tab)<-c("variable",self$observed)
       private$.make_empty_table(tab)
       
    },
    init_covariances_residual=function() {
  
      .length <- length(self$observed)
       tab <- cbind(variable=self$observed, as.data.frame(matrix(".", ncol=.length, nrow=.length, dimnames=list(NULL, self$observed))));
       names(tab)<-c("variable",self$observed)
       private$.make_empty_table(tab)
  
     },
     init_covariances_combined=function() {
       
          tab1<-tab2<-tab3<-NULL
          if (self$option("outputObservedCovariances"))
                tab1<-self$init_covariances_observed()
          if (self$option("outputImpliedCovariances"))
                tab2<-self$init_covariances_implied()
          if (self$option("outputResidualCovariances"))
                tab2<-self$init_covariances_residual()
          
          rbind(tab1,tab2,tab3)
      },

      init_covariances_latent=function() {
  
         .length <- length(self$latent)
          tab <- cbind(variable=self$latent, as.data.frame(matrix(".", ncol=.length, nrow=.length, dimnames=list(NULL, self$latent))));
          names(tab)<-c("variable",self$latent)
          private$.make_empty_table(tab) 
          
      }




  ),   # End public
  
  private=list(
        
    .lav_terms=NULL,
    .lav_structure=NULL,
    .lav_indirect=NULL,
    
    .check_models=function() {
      
      synt<-self$options$code
      synt<-stringr::str_replace_all(synt, "[\r]" , "")
      avec<-stringr::str_split(synt,"\n")[[1]]
      avec<-avec[sapply(avec, function(a) a!="")]
      avec<-avec[grep("#",avec,fixed = T,invert = T)]
      
      # here we check if the user used .pN. labels and warn if that is the case 
      check<-stringr::str_extract(avec, ".p\\d+\\.")
      check<-check[!unlist(sapply(check,is.na))]
      if (length(check)>0)
        for (st in check) {
          msg<-glue::glue(DP_WARNS[[".p."]],x=st)
          self$dispatcher$warnings<-list(topic="info",message=msg)
        }
      
      # here we check if the user used custom group names 
      check<-any(stringr::str_detect(tolower(gsub(" ","",avec)), "^group:(?!\\=)"))
      if (check)
        self$customgroups<-TRUE
      # retrieve latent variables if any 
      lat<-sapply(avec,function(a) stringr::str_extract(gsub(" ","",a),"^.+=~"))
      lat<-lat[!is.na(lat)]
      lat<-gsub("=~","",lat)       
      if (length(lat)>0)
        self$latent<-lat
      
      self$user_syntax<-avec
      
    },
    ### collapse the informations in the private lists of terms and constraints and produce a lavaan syntax string
    .lavaan_syntax=function() {
      f <- glue::glue_collapse(unlist(self$user_syntax), sep = "; ")
      i <- glue::glue_collapse(unlist(private$.lav_indirect), sep = "; ")
      paste(f,i, sep="; ")
    },
    ## lavaanify the information available to obtain a info table representing the parameters structure.
    ## That is, the parameter names, labels, etc
    
    .make_structure = function() {
      
      lavoptions <- list(
        model = private$.lavaan_syntax(),
        meanstructure = self$options$meanstructure,           # 'default' for sem + cfa, TRUE for growth (default: FALSE; 'default' is FALSE in most cases)
        int.ov.free = self$options$intercepts,                # TRUE for sem + cfa, FALSE for growth (default: TRUE)
        int.lv.free = FALSE,                                  # TRUE for grwoth, FALSE for sem + cfa - to be implemented as option
        # auto.fix.first - see below                          # TRUE (unless std.lv = TRUE) for sem + cfa + growth
        # std.lv - see below                                  # 
        auto.fix.single = TRUE,                               # TRUE for sem + cfa + growth - to be implemented as option
        auto.var = TRUE,                                      # TRUE for sem + cfa + growth - to be implemented as option
        auto.cov.lv.x =TRUE,                                  # TRUE for sem + cfa + growth - to be implemented as option
        auto.efa = TRUE,                                      # TRUE for sem + cfa + growth - to be implemented as option
        auto.th = TRUE,                                       # TRUE for sem + cfa + growth - to be implemented as option
        auto.delta = TRUE,                                    # TRUE for sem + cfa + growth - to be implemented as option
        auto.cov.y = self$options$cov_y,                      # TRUE for sem + cfa + growth (default: TRUE)
        fixed.x = self$options$cov_x,
        orthogonal=!self$options$cov_lv
        # TO-DO (1): implemented as option
        # int.lv.free, auto.fix.single, auto.var, auto.cov.lv.x, auto.efa, auto.th, auto.delta
        # TO-DO (2): further arguments included in JASP-SEM
        # int.lv.fixed int.ov.fixed mimic orthogonal
      )
      
      # std_lv - standardize latent variables
      # (setting both to FALSE leads to the SE's not being estimated, setting both to TRUE 
      if (self$options$std_lv == "fix_first") {
        lavoptions[["auto.fix.first"]] <- TRUE
        lavoptions[["std.lv"]]         <- FALSE
      } else if (self$options$std_lv == "std_res") {
        lavoptions[["auto.fix.first"]] <- FALSE
        lavoptions[["std.lv"]]         <- TRUE
      }
      
      ### semPaths has trouble with some multigroups diagram when meanstructure is FALSE                
      if (is.something(self$multigroup)) {
        lavoptions[["ngroups"]] <- self$multigroup$nlevels
        lavoptions[["meanstructure"]] <- TRUE
        group.equal<-c()
        if (self$options$eq_loadings) group.equal<-c(group.equal,"loadings")
        if (self$options$eq_intercepts) group.equal<-c(group.equal,"intercepts")
        if (self$options$eq_means) group.equal<-c(group.equal,"means")
        if (self$options$eq_thresholds) group.equal<-c(group.equal,"thresholds")
        if (self$options$eq_regressions) group.equal<-c(group.equal,"regressions")
        if (self$options$eq_residuals) group.equal<-c(group.equal,"residuals")
        if (self$options$eq_residual.covariances) group.equal<-c(group.equal,"residual.covariances")
        if (self$options$eq_lv.variances) group.equal<-c(group.equal,"lv.variances")
        if (self$options$eq_lv.variances) group.equal<-c(group.equal,"lv.covariances")
        if (length(group.equal)>0)
          lavoptions[["group.equal"]]<-group.equal
        
      }
      
      ### if ordered variables are present, datamatic prepared a varTable containing
      ### the information about the variables. we need to pass it to lavaanify()
      
      if (is.something(self$ordered)) 
        lavoptions[["varTable"]]<-self$varTable
      
      
      results <- try_hard({ do.call(lavaan::lavaanify, lavoptions) })
      self$dispatcher$warnings <- list(topic="info", message = results$warning)
      self$dispatcher$errors   <- list(topic="info", message = results$error,final=TRUE)
      
      private$.lav_structure <- results$obj
      ## if not user defined, create easy labels to be used by the user in constraints and defined parameters  
      ## we want to be sure that we nicify only lavaan plabels and not user defined
      ulabels <- private$.lav_structure$label
      tvec<-gsub(" ","",ulabels)
      check<-grep("^\\p\\d+$",tvec)
      if (length(check)>0)
        for (i in check) {
          st<-tvec[[i]]
          msg<-glue::glue(DP_WARNS[["p"]],x=st)
          self$dispatcher$warnings<-list(topic="info",message=msg)
        }
      
      def <- ulabels!=""
      labels<-private$.lav_structure$plabel
      labels[def]<-ulabels[def]
      # nicify lavaan .pN. labels
      whichp<-grepl("^.p\\d+\\.$",labels)
      labels[whichp]<-gsub(".","",labels[whichp],fixed=T)
      private$.lav_structure$label <- labels
      
      private$.lav_structure<-private$.fix_groups_labels(private$.lav_structure)
      
    },
    .fix_groups_labels=function(table) {
      
      ## for multigroup analysis, add a description label with the level of each group (all for general parameter)
      if (is.something(self$multigroup)) {
        if (self$customgroups) {
          table$lgroup=table$group
        } else {
          levs<-c(self$multigroup$levels,"All")
          table$group<-ifelse(table$group==0,length(levs)+1,table$group)
          table$lgroup<-levs[table$group]
          
        }
      } else
        table$lgroup<-"1"
      table
    },
    
    

    ### compute indirect effects if required by the user
    .indirect=function() {
      
      if (!self$options$indirect)
        return()
      
      ## first, we update the lavaanified structure table
      private$.make_structure()
      tab<-private$.lav_structure
      termslist<-list()
      labslist<-list()
      groupslist<-list()
      sel<-grep(":",tab$rhs,fixed = T,invert=T) 
      tab<-tab[sel,]
      sel<-tab$op=="~" 
      tab<-tab[sel,]
      tab<-tab[tab$group>0,]
      
      ## recursive function to extract indirect effects.
      .doit<-function(tab,term,alist=list(),blist=list(),lab=NULL) {
        alist<-c(alist,term)
        blist<-c(blist,lab)
        if (term %in% deps) {
          final<-unlist(alist)
          if (length(final)>2) {
            termslist[[length(termslist)+1]]<<-final
            labslist[[length(labslist)+1]]<<-unlist(blist)
            groupslist[[length(groupslist)+1]]<<-tab$group[1]
            
          }
          return()
        }
        a<-tab[tab$rhs==term,]
        if (length(a$lhs)==0)
          return()
        for (i in 1:nrow(a)) {
          x<-a$lhs[i]
          lab<-a$label[i]
          .doit(tab,x,alist,blist,lab)
        }
        
      }
      
      terms<-unique(tab$rhs)
      deps<-unique(setdiff(tab$lhs,tab$rhs))
      
      
      if (length(deps)==0) {
        self$dispatcher$warnings<-list(topic="models_defined",message=WARNS[["noindirect"]])
        return()
      }
      tabs<-list()
      for (i in tab$group) 
        tabs[[i]]<-tab[tab$group==i,]
      
      results<-list()
      ### we run the recursive function for each group and for each rhs term
      for (i in seq_along(tabs)) {
        .results<-try_hard({
          for (tt in terms)
            .doit(tabs[[i]],term=tt)
        })
        if (.results$error!=FALSE)
          self$dispatcher$warnings<-list(topic="models_defined",message=WARNS[["noindirect"]])
        results[[i]]<-.results
      }
      pars<-sapply(labslist,paste,collapse="*")
      if (!is.something(pars))
        return()
      labs<-sapply(termslist,paste,collapse=" \U21d2 ")
      plabs<-paste0("IE",1:length(pars))
      synt<-paste(plabs,pars,sep=":=",collapse = " ; ")
      private$.lav_indirect<-synt
      self$indirect_names<-labs
      if (is.something(self$options$multigroup))
        self$indirect_names<-paste0("(",self$indirect_names,")",SUB[unlist(groupslist)])
      names(self$indirect_names)<-pars
    },
    
    .make_empty_table=function(atable) {
      
      .len<-dim(as.data.frame(atable))[1]
      
      if (is.something(self$cluster)) {
        atable<-as.data.frame(rbind(atable,atable))
        atable$level<-rep(c("within","between"),each=.len)
      } else
        atable$level<-""
      
      ### in case we have multigroup, we expect the matrix to be replicated
      ### for each group. This should go after multilevel, because in case of both 
      ### multilevel and multigroup, the matrices will be within-between for each group
      if (is.something(self$multigroup)) {
          .len<-dim(atable)[1]
           k<-self$multigroup$nlevels
           atable<-as.data.frame(do.call(rbind,lapply(1:k ,function(a) atable)))
           atable$lgroup<-rep(self$multigroup$levels,each=.len)
      } else
           atable$lgroup<-0
      
      return(atable)
    },
    
    .make_matrix_table=function(obj,fun=identity) {
      
      if (is.list(obj)) {
         obj<-lapply(obj,fun)
         return(as.data.frame(do.call("rbind",obj)))
      }
      else
         return(as.data.frame(fun(obj)))
      
    },
    .make_covcor_table=function(obj,field="cov") {
      
      if (field %in% names(obj))   all_obj<-list("1"=obj) else all_obj<-obj
      alist<-list()
      for (i in seq_along(all_obj)) {
        covTab<-all_obj[[i]][[field]]
        corTab<- stats::cov2cor(covTab)
        .names<-colnames(covTab)
        .numVar<-length(.names)
        tab = matrix(NA, nrow=.numVar, ncol=.numVar, dimnames=list(.names, .names))
        tab[lower.tri(tab, diag=TRUE)]  = covTab[lower.tri(covTab, diag=TRUE)]
        tab[upper.tri(tab, diag=FALSE)] = corTab[upper.tri(corTab, diag=FALSE)]
        alist[[length(alist)+1]]<-tab
      }
      tabs<-do.call("ebind_square",alist)
      return(as.data.frame(tabs))
  
    },
    
    .make_covcor_diff=function(obj1,obj2) {
      
      if ("cov" %in% names(obj1))   all_obj1<-list("1"=obj1) else all_obj1<-obj1
      if ("cov" %in% names(obj2))   all_obj2<-list("1"=obj2) else all_obj2<-obj2
      
      alist<-list()
      for (i in seq_along(all_obj1)) {
        covTab1<-all_obj1[[i]][["cov"]]
        covTab2<-all_obj2[[i]][["cov"]]
        covTab<- covTab1-covTab2
        corTab<- stats::cov2cor(covTab1)-stats::cov2cor(covTab2)
        .names<-colnames(covTab)
        .numVar<-length(.names)
        tab = matrix(NA, nrow=.numVar, ncol=.numVar, dimnames=list(.names, .names))
        tab[lower.tri(tab, diag=TRUE)]  = covTab[lower.tri(covTab, diag=TRUE)]
        tab[upper.tri(tab, diag=FALSE)] = corTab[upper.tri(corTab, diag=FALSE)]
        alist[[length(alist)+1]]<-tab
      }
      tabs<-do.call("ebind_square",alist)
      return(as.data.frame(tabs))
    }
    
    
    

  ) # end of private
) # End Rclass


