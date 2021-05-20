### This class takes care of producing lavaan syntax with B64 and normal names and all tables with information about the estimation
### tables are `parameter table` in Yves Rosseel terminology (see lavaan manual)
### It takes care also of producing and checking constraints
### free parameters, indirect effects and the like. 
### it assumes that the estimation will be done on B64 names, and the results reported in plain names
### It assumes that all interactions term are computed as new variables with INTERACTION_SYMBOL as separator (in place of ":") in the 
### variable name. 
### It assumes that all factors are present in the data as K-1 new variables with appropriated constrast value as numeric variables. 
### each "dummy" variable in named `VAR{FACTOR_SYMBOL}k`` 
### Inherit from Dispatch, which provides $warnings and $errors mechanisms to store info. Requires the try_hard() function to catch errors
##  naming convention: all objects containing lavaan syntax, objects that are needed to build objects that can be passed directly
##                     to lavaan are named $lav_*.   $lav_* objects contains B64 variable names 
##                     All object containing tables to be passed to the results init tables are called $tab_*. 


Syntax <- R6::R6Class(
         "Syntax",
          class=FALSE, ## this and the next 
          cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
          inherit = Dispatch,
          public=list(
              endogenous=NULL,
              lav_terms=NULL,
              lav_structure=NULL,
              tab_coefficients=NULL,
              tab_loadings=NULL,
              tab_covariances=NULL,
              tab_intercepts=NULL,
              tab_defined=NULL,
              tab_info=NULL,
              structure=NULL,
              options=NULL,
              constraints=NULL,
              defined=NULL,
              contrasts_names=NULL,
              multigroup=NULL,
              indirect_names=NULL,
              models=NULL,
              initialize=function(options,datamatic) {
                
                astring<-options$code
                reg<-"[=~:+\n]"
                avec<-stringr::str_split(astring,reg)[[1]]
                avec<-avec[sapply(avec, function(a) a!="")]
                vars<-sapply(avec, function(a) stringr::str_remove(a,'.?[*]'))
                
                super$initialize(options=options,vars=vars)
                self$multigroup=datamatic$multigroup

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
            .lav_constraints=NULL,
            .lav_defined=NULL,
            .lav_indirect=NULL,
        
            .check_models=function() {
              synt<-self$options$code
              avec<-stringr::str_split(synt,"\n")[[1]]
              avec<-avec[sapply(avec, function(a) a!="")]
              self$models<-avec

            },
            ### collapse the informations in the private lists of terms and constraints and produce a lavaan syntax string
            .lavaan_syntax=function() {
                  f<-glue::glue_collapse(unlist(self$models),sep = " ; ")
                  i<-glue::glue_collapse(unlist(private$.lav_indirect),sep = " ; ")
                  paste(f,i, sep=";")
            },
            ## lavaanify the information available to obtain a raw (B64) table representing the parameters structure
            ## parameter structure means their names, labels, 

            .make_structure=function() {
  
                lavoptions<-list(
                model=private$.lavaan_syntax(),
                int.ov.free = self$options$intercepts, 
                auto.cov.y = self$options$cov_y,
                fixed.x=self$options$cov_x,
                int.lv.free=FALSE,
                std.lv = FALSE,
                auto.fix.first = TRUE, 
                auto.fix.single = TRUE, 
                auto.var = TRUE, 
                auto.cov.lv.x = TRUE, 
                auto.efa = TRUE, 
                auto.th = TRUE, 
                auto.delta = TRUE, 
                meanstructure = FALSE  ### this is needed to be TRUE for semPaths to work also with multigroups
              )
              if (is.something(self$multigroup)) {
                    lavoptions[["ngroups"]]<-self$multigroup$nlevels
                    lavoptions[["meanstructure"]]<-TRUE
                    
                }
              
              results<-try_hard({
                do.call(lavaan::lavaanify, lavoptions)
              })
              self$warnings<-list(topic="main",message=results$warning)
              self$errors<-results$error
              if (is.something(self$errors))
                stop(paste(self$errors,collapse = "\n"))
              
              ## raw (B64ed) structure of results gos in .lav_structure. Here are all parameters to be estimated, with info regarding
              ## their nature (exogenous, endogenous, coefficient vs variances, etc), labels and names 
              
              private$.lav_structure<-results$obj
              ## create easy labels to be used by the user in constraints and defined parameters  
              private$.lav_structure$label<-ifelse(
                                private$.lav_structure$label=="",
                                gsub(".","",private$.lav_structure$plabel,fixed=T),
                                private$.lav_structure$label)

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
                  if (is.something(self$multigroup)) {
                        levs<-c(self$multigroup$levels,"All")
                       .lav_structure$group<-ifelse(.lav_structure$group==0,length(levs)+1,.lav_structure$group)
                       .lav_structure$lgroup<-levs[.lav_structure$group]
                   } else
                        .lav_structure$lgroup<-"1"
              
              ### self$structure containts all parameters with plain names. Useful for children to refer to parameters properties
              ### .lav_structure is not a tab_* which will be displayed in results
                  
              sel<-grep("==|<|>",.lav_structure$op,invert = T)
              self$structure<-.lav_structure[sel,]

              ### tab_coefficients contains all regression coefficients
              
              self$tab_coefficients<-.lav_structure[.lav_structure$op=="~",]
              
              ### tab_loadings contains loadings from observed to latent vars
              
              self$tab_loadings<-.lav_structure[.lav_structure$op=="=~",]
              
              
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
              alist[[length(alist)+1]]<-c(info="Converged","") 
              alist[[length(alist)+1]]<-c(info="",value="")
              alist[[length(alist)+1]]<-c(info="Loglikelihood user model",value="" )
              alist[[length(alist)+1]]<-c(info="Loglikelihood unrestricted model",value="")
              alist[[length(alist)+1]]<-c(info="",value="")
              for (m in self$models)
                  alist[[length(alist)+1]]<-c(info="Model",value=m)
              self$tab_info<-alist
              
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

            },

            .check_constraints=function() {
              
              consts<-self$options$constraints
              realconsts<-list()
              realestims<-list()
              for (con in consts) {
                ## error if user passes a latent variable definition. Not the right module :-)
                check<-(length(grep("=~",con,fixed=T))>0)
                if (check) {
                      self$errors<-ERRS[["nolatent"]]
                      return()
                }
                ### divide constraints from defined parameters
                check<-(length(grep("==|>|<",con,fixed=F))>0) 
                if (check)
                  realconsts[[length(realconsts)+1]]<-con
                else
                  realestims[[length(realestims)+1]]<-con
              }
              ### handle defined parameters
              realestims<-lapply(seq_along(realestims), function(j) {
                    estim<-realestims[[j]]
                    estim<-trimws(estim)
                    
                    if (estim=="")
                           return("")

                    check<-grep(":=|~",estim)
                    if (length(check)==0 ) {
                          estim<-paste0("dp",j,":=",estim)
                      }
                    else {
                       check<-grep("^IE",estim)
                       if (length(check)>0)
                         self$warnings<-list(topic="defined",message=glue::glue(WARNS[["noreserved"]],var="IE"))
                       check<-grep("^dp",estim)
                       if (length(check)>0)
                         self$warnings<-list(topic="defined",message=glue::glue(WARNS[["noreserved"]],var="dp"))
                    }
                    estim
              })
              ### this are used in the info results table to list the user parameter
              self$constraints<-lapply(realconsts, function(x) list(info="Constraint",value=x))
              self$defined<-lapply(realestims, function(x) list(info="Defined parameter",value=x))
              
              ## now we translate constraints to be passed to estimation with correct names, B64ed and interaction/factor aware
              for (i in seq_along(realconsts)) {
                     
                      for (term in self$interactions) {
                        realconsts[[i]]<-gsub(fromb64(term),term,realconsts[[i]],fixed=TRUE)
                      }
                     
                      realconsts[[i]]<-gsub(":",INTERACTION_SYMBOL,realconsts[[i]],fixed = T)
                
                
                     for (name in names(self$contrasts_names))
                       realconsts[[i]]<-gsub(name,self$contrasts_names[[name]],realconsts[[i]],fixed=TRUE)
              }
              
              ## now we translate realestims to be passed to estimation with correct names, B64ed and interaction/factor aware
              for (i in seq_along(realestims)) {
                  estim<-realestims[[i]]
                  estim<-gsub(":="," $ ",estim,fixed = T)
                for (term in self$interactions) {
                  estim<-gsub(fromb64(term),term,estim)
                }
                estim<-gsub(":",INTERACTION_SYMBOL,estim)
                
                for (name in names(self$contrasts_names))
                  estim<-gsub(name,self$contrasts_names[[name]],estim,fixed=TRUE)
                estim<-gsub("$",":=",estim,fixed = T)
                realestims[[i]]<-estim
              }

              
              private$.lav_constraints<-tob64(realconsts,self$vars)
              private$.lav_defined<-tob64(realestims,self$vars)

            },
            .check_varcov=function() {
              
              
              varcov64<-tob64(self$options$varcov)
              ## we need to check for single variable pair, when a term is null
              sel<-unlist(sapply(varcov64, function(vc) !any(unlist(sapply(vc,is.null)))))
              varcov64<-varcov64[sel]
              factorinfo64<-self$factorinfo
              names(factorinfo64)<-tob64(names(factorinfo64))
              varcov64<-private$.factorlist(varcov64,factorinfo64)
              res<-lapply(varcov64, function(vc) {
                if (length(vc)==2) {
                        private$.lav_defined[[length(private$..lav_defined)+1]]<-paste(vc[[1]],vc[[2]],sep = "~~")
                }
              })

            },
            .factorlist=function(terms,factorslen) {
              .terms<-list()
              for (f in names(factorslen)) {
                for (term in terms) {
                  ind<-which(term==f)
                  for (i in ind) {
                    for (j in seq_len(factorslen[[f]])) {
                      .term<-term
                      .term[[i]]<-paste0(trimws(.term[[i]]),FACTOR_SYMBOL,j)
                      .terms[[length(.terms)+1]]<-.term
                    }
                  }
                  if (length(ind)==0)
                    .terms[[length(.terms)+1]]<-trimws(term)
                }
                terms<-.terms
              }
              terms  
            },
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
              
              ## recursive function to extact indirect effects.
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

