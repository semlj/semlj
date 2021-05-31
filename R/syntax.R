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
              lav_terms=NULL,
              lav_structure=NULL,
              tab_coefficients=NULL,
              tab_loadings=NULL,
              tab_covariances=NULL,
              tab_intercepts=NULL,
              tab_defined=NULL,
              tab_info=NULL,
              tab_constfits=NULL,
              structure=NULL,
              options=NULL,
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
            .lav_indirect=NULL,
        
            .check_models=function() {
              synt<-self$options$code
              synt<-stringr::str_replace_all(synt, "[\r]" , "")
              avec<-stringr::str_split(synt,"\n")[[1]]
              avec<-avec[sapply(avec, function(a) a!="")]
              self$models<-avec
              wmark(self$models)

            },
            ### collapse the informations in the private lists of terms and constraints and produce a lavaan syntax string
            .lavaan_syntax=function() {
                  f<-glue::glue_collapse(unlist(self$models),sep = " ; ")
                  i<-glue::glue_collapse(unlist(private$.lav_indirect),sep = " ; ")

                  paste(f,i, sep=";")
            },
            ## lavaanify the information available to obtain a info table representing the parameters structure.
            ## That is, the parameter names, labels, 

            .make_structure=function() {
  
                lavoptions<-list(
                model=private$.lavaan_syntax(),
                int.ov.free = self$options$intercepts, 
                auto.cov.y = self$options$cov_y,
                fixed.x=self$options$cov_x,
                int.lv.free=FALSE,
                std.lv = self$options$std.lv,
                auto.fix.first = self$options$auto.fix.first, 
                auto.fix.single = TRUE, 
                auto.var = TRUE, 
                auto.cov.lv.x = TRUE, 
                auto.efa = TRUE, 
                auto.th = TRUE, 
                auto.delta = TRUE, 
                meanstructure = FALSE  
              )
                ### semPaths has trouble with some multigroups diagram when meanstructure is FALSE
              if (is.something(self$multigroup)) {
                    lavoptions[["ngroups"]]<-self$multigroup$nlevels
                    lavoptions[["meanstructure"]]<-TRUE
                    
                }
              
              results<-try_hard({
                do.call(lavaan::lavaanify, lavoptions)
              })
              self$warnings<-list(topic="info",message=results$warning)
              self$errors<-results$error
              if (is.something(self$errors))
                stop(paste(self$errors,collapse = "\n"))
              

              private$.lav_structure<-results$obj
              ## if not user defined, create easy labels to be used by the user in constraints and defined parameters  
              ## we want to be sure that we do not interfere with user ability to use p* as custom label
              ulabels<-private$.lav_structure$label
              def<-ulabels!=""
              plabels<-setdiff(paste0("p",1:(length(ulabels)*2)),ulabels[def])
              plabels[which(def)]<-ulabels[def]
              labels<-plabels[1:length(ulabels)]
              private$.lav_structure$label<-labels

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
  #                      levs<-c(self$multigroup$levels,"All")
                       .lav_structure$group<-ifelse(.lav_structure$group==0,length(levs)+1,.lav_structure$group)
                       .lav_structure$lgroup<-levs[.lav_structure$group]
                   } else
                        .lav_structure$lgroup<-"1"
              
              ### self$structure containts all parameters with. Useful for children to refer to parameters properties
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
              
              # tab constraints
              sel<-grep("==|<|>",.lav_structure$op)
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

