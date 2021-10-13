### This class takes care of digesting lavaan syntax and it makes all tables with information about the estimation

### tables are `parameter table` in Yves Rosseel terminology (see lavaan manual)
### Inherit from Dispatch, which provides $warnings and $errors mechanisms to store info. 
### It requires the try_hard() function to catch errors (see functions.R)
##  naming convention: all objects containing lavaan syntax, objects that are needed to build objects 
###                   that can be passed directly  to lavaan are named $lav_*.    
##                     All object containing tables to be passed to the results init tables are called $tab_*. 
##  Tables produced here are assumed to have the columns names matching the jamovi results tables

## The Syntax class does all the work on the model produced by lavaanify(). The estimation is done in the "Estimate" class,
## which is a offspring of this class. The version of the tables produced here are used by the ".init()" function in .b.R
## to prepare the tables (so the output is nicely shown during estimation)

Syntax <- R6::R6Class(
         "Syntax",
          class=FALSE, ## this and the next 
          cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
          inherit = Dispatch,
          public=list(
              endogenous=NULL,
              observed=NULL,
              latent=NULL,
              lav_terms=NULL,
              lav_structure=NULL,
              tab_coefficients=NULL,
              tab_loadings=NULL,
              tab_composites=NULL,
              tab_covariances=NULL,
              tab_intercepts=NULL,
              tab_defined=NULL,
              tab_info=NULL,
              tab_constfit=NULL,
              tab_compModelBsl=NULL,
              tab_otherFit=NULL,
              tab_Rsquared=NULL,
              tab_mardia=NULL,
              tab_covcorrObserved=NULL,
              tab_covcorrImplied=NULL,                          
              tab_covcorrResidual=NULL,
              tab_covcorrCombined=NULL,
              tab_covcorrLatent=NULL,
              tab_modInd=NULL,
              structure=NULL,
              options=NULL,
              multigroup=NULL,
              cluster=NULL,
              indirect_names=NULL,
              models=NULL,
              customgroups=FALSE,
              initialize=function(options,datamatic) {
                astring<-options$code
                super$initialize(options=options,vars=datamatic$vars)

                self$observed    <- datamatic$observed
                self$multigroup  <- datamatic$multigroup
                self$cluster     <- datamatic$cluster
                # check_* check the input options and produces tables and list with names
                ### prepare list of models for lavaan
                private$.check_models()

                ## check and build indirect effect (if requires)
                private$.indirect()
                
                ### here we update to build a lavaanify structure
                private$.update()  
                
                } # here initialize ends

          ),   # End public
          active=list(
           ### inherited warnings and errors are overriden to check specific message to change, and then passed to super 
           warnings=function(obj) {
             if (missing(obj))
               return(private$.warnings)
             if (is.null(obj))
               return()
             if (is.null(obj$message))
                 return()
             check<-length(grep("fixed.x=FALSE",obj$message,fixed = T)>0) 
             if (check) 
               obj$message<-WARNS[["usercov"]]
             
             check<-length(grep("cov.lv",obj$message,fixed = T)>0) 
             if (check) 
               obj$message<-WARNS[["cov.lv"]]
             
                            
             super$warnings<-obj
           },
           errors=function(obj) {
             
             if (missing(obj))
               return(private$.errors)
             if (is.null(obj))
               return()
             check<-length(grep("infinite or missing",obj,fixed = T)>0) 
             if (check) 
               super$errors<-ERRS[["noluck"]]
             
             super$errors<-obj
           }
           
          ),
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
                  self$warnings<-list(topic="info",message=msg)
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



              self$models<-avec

            },
            ### collapse the informations in the private lists of terms and constraints and produce a lavaan syntax string
            .lavaan_syntax=function() {
                  f <- glue::glue_collapse(unlist(self$models), sep = "; ")
                  i <- glue::glue_collapse(unlist(private$.lav_indirect), sep = "; ")
                  paste(f,i, sep="; ")
            },
            ## lavaanify the information available to obtain a info table representing the parameters structure.
            ## That is, the parameter names, labels, 

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
                  fixed.x = self$options$cov_x

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
                results <- try_hard({ do.call(lavaan::lavaanify, lavoptions) })
                self$warnings <- list(topic="info", message = results$warning)
                self$errors <- results$error
                if (is.something(self$errors))
                  stop(paste(self$errors, collapse = "\n"))

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
                    self$warnings<-list(topic="info",message=msg)
                  }
                
                def <- ulabels!=""
                labels<-private$.lav_structure$plabel
                labels[def]<-ulabels[def]
                # nicify lavaan .pN. labels
                whichp<-grepl("^.p\\d+\\.$",labels)
                labels[whichp]<-gsub(".","",labels[whichp],fixed=T)
                private$.lav_structure$label <- labels
            },

            ### here we create the tables we need for init results. Those tables contains the information needed to init the results tables
            ### ideally, there should be one publich table for each results table, but a few table cannot be defined before
            ### estimation, so here are missing
            
            .update=function() {
              
              ## first, we update the lavaanified structure table
              private$.make_structure()
              

              ## now we start the filling of the tables to be used in the init of results
              .lav_structure<-private$.lav_structure
              ## fill some info to make nicer tables
                  .lav_structure$type<-ifelse(.lav_structure$free>0,"Free","Fixed")
              ## for multigroup analysis, add a description label with the level of each group (all for general parameter)
                  .lav_structure<-private$.fix_groups_labels(.lav_structure)

              ### self$structure containts all parameters with. Useful for children to refer to parameters properties
              ### .lav_structure is not a tab_* which will be displayed in results
                  
              sel<-grep("==|<|>",.lav_structure$op,invert = T)
              self$structure<-.lav_structure[sel,]

              ### tab_coefficients contains all regression coefficients
              
              self$tab_coefficients<-.lav_structure[.lav_structure$op=="~",]
              
              ### tab_loadings contains loadings from observed to latent vars
              
              self$tab_loadings<-.lav_structure[.lav_structure$op=="=~",]
              
              ### tab_composites contains loadings from observed to formative vars
              
              .tab_composites<-.lav_structure[.lav_structure$op=="<~",]
              if (nrow(.tab_composites)>0)
                    self$tab_composites<-.tab_composites
              
              ### tab_covariances contains variances and covariances
              self$tab_covariances<-.lav_structure[.lav_structure$op=="~~",]
              ### intercepts table
              self$tab_intercepts<-.lav_structure[.lav_structure$op=="~1",]
              if (nrow(self$tab_intercepts)==0) self$tab_intercepts<-NULL
              
              ### info contains the info table, with some loose information about the model
              alist<-list()
              alist[[length(alist)+1]]<-c(info="Estimation Method",value=self$options$estimator)
              alist[[length(alist)+1]]<-c(info="Number of observations",value="") 
              alist[[length(alist)+1]]<-c(info="Free parameters",value=max(.lav_structure$free))
              alist[[length(alist)+1]]<-c(info="Converged", value="") 
              alist[[length(alist)+1]]<-c(info="",value="")
              alist[[length(alist)+1]]<-c(info="Loglikelihood user model",value="" )
              alist[[length(alist)+1]]<-c(info="Loglikelihood unrestricted model",value="")
              alist[[length(alist)+1]]<-c(info="",value="")
              for (m in self$models)
                  alist[[length(alist)+1]]<-c(info="Model",value=m)
              self$tab_info<-alist
              
              # tab constraints
              op<-gsub("<~","&",.lav_structure$op,fixed=TRUE)
              sel<-grep("==|<|>",op)
              const<-.lav_structure[sel,]
              if (nrow(const)>0)
                 self$tab_constfit<-const

              
              ###  tab_defined contains defined parameters

              dp<-.lav_structure[.lav_structure$op==":=",]
              if (nrow(dp)>0) {
                      dp$desc<-""
                      .structure<-.lav_structure[.lav_structure$op %in% c("~","~~","~1"),]
                      for (i in seq_along(dp$rhs)) {
                            r<-dp$rhs[i]
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
                      dp$desc[i]<-r
                      }
              }
              if (nrow(dp)>0) {
                self$tab_defined=dp
                if (is.something(self$multigroup)) {
                     msg<-paste(1:self$multigroup$nlevels,self$multigroup$levels,sep="= group ",collapse = ", ")
                     msg<-paste("Description subscripts refer to groups, with",msg)
                     self$warnings<-list(topic="defined",message=msg) 
                     
                }
              }
              
              #### additional output ####
              .length <- length(self$observed)
              tab <- cbind(variable=self$observed, as.data.frame(matrix(0, ncol=.length, nrow=.length, dimnames=list(NULL, self$observed))));
              names(tab)<-c("variable",self$observed)
              
              
              ### in case we have multilevel, we expect the matrix to be replicated
              ### for within and between
              
              if (is.something(self$cluster)) {
                tab<-as.data.frame(rbind(tab,tab))
                tab$level<-rep(c("within","between"),each=length(self$observed))
              } else
                tab$level<-""
              
              ### in case we have multigroup, we expect the matrix to be replicated
              ### for each group. This should go after multilevel, because in case of both 
              ### multilevel and multigroup, the matrices will be within-between for each group
              if (is.something(self$multigroup)) {
                len<-dim(tab)[1]
                k<-self$multigroup$nlevels
                tab<-as.data.frame(do.call(rbind,lapply(1:k ,function(a) tab)))
                tab$lgroup<-rep(self$multigroup$levels,each=len)
              } else
                tab$lgroup<-0
              
              if (self$options$outputObservedCovariances) { self$tab_covcorrObserved <- tab };
              if (self$options$outputImpliedCovariances)  { self$tab_covcorrImplied  <- tab };
              if (self$options$outputResidualCovariances) { self$tab_covcorrResidual <- tab };

              if (self$options$outpuCombineCovariances) {
                tab <- rbind(self$tab_covcorrObserved, self$tab_covcorrImplied, self$tab_covcorrResidual);
                self$tab_covcorrCombined <- tab[order(tab$variable), ];
                self$tab_covcorrObserved <- NULL;
                self$tab_covcorrImplied  <- NULL;
                self$tab_covcorrResidual <- NULL;
              }

              #### additional output ####
              if (self$options$cov.lv & is.something(self$latent)) {
                  .length <- length(self$latent)
              tab <- cbind(variable=self$latent, as.data.frame(matrix(0, ncol=.length, nrow=.length, dimnames=list(NULL, self$latent))));
              names(tab)<-c("variable",self$latent)
              
              
              ### in case we have multilevel, we expect the matrix to be replicated
              ### for within and between
              
              if (is.something(self$cluster)) {
                tab<-as.data.frame(rbind(tab,tab))
                tab$level<-rep(c("within","between"),each=length(self$observed))
              } else
                tab$level<-""
              
              ### in case we have multigroup, we expect the matrix to be replicated
              ### for each group. This should go after multilevel, because in case of both 
              ### multilevel and multigroup, the matrices will be within-between for each group
              if (is.something(self$multigroup)) {
                len<-dim(tab)[1]
                k<-self$multigroup$nlevels
                tab<-as.data.frame(do.call(rbind,lapply(1:k ,function(a) tab)))
                tab$lgroup<-rep(self$multigroup$levels,each=len)
              } else
                tab$lgroup<-0
              
              self$tab_covcorrLatent <- tab 
              }
              
              
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
                self$warnings<-list(topic="defined",message=WARNS[["noindirect"]])
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
                    self$warnings<-list(topic="defined",message=WARNS[["noindirect"]])
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
            }

          ) # end of private
) # End Rclass
