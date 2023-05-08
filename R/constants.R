j_DEBUG=F
j_INFO=F
t_INFO=F


NOTES<-list()

NOTES[["ci"]]<-list("standard"="Standard (Delta method)",
                    "bca"="Bias corrected bootstrap",
                    "perc"="Bootstrap percentiles",
                    "norm"="Parametric bootstrap")


LAT_EXAMPLES<-list()
LAT_EXAMPLES[[1]]<-list("info"="Latent measurement",example="",com="")
LAT_EXAMPLES[[2]]<-list("info"="indicators",example=" eta =~ x1 + x2 + x3",com="Eta is measured by x1,x2, x3")
LAT_EXAMPLES[[3]]<-list("info"="indicators fixed",example=" eta =~ x1 + 1*x2 + x3",com="Fix the scale of eta to x2 scale")
LAT_EXAMPLES[[4]]<-list("info"="Latent Regression",example="eta~beta+gamma",com="Eta is predicted by beta and gamma")
LAT_EXAMPLES[[5]]<-list("info"="Latent Regression",example="eta~beta+x1",com="Latenent Eta is predicted by beta and observed x1")


CONT_EXAMPLES<-list()
CONT_EXAMPLES[[1]]<-list("info"="Constraints",example="",com="")
CONT_EXAMPLES[[2]]<-list("info"="Equality constraint",example="p1==p2",com="Constrain the estimates of p1 and p2 to be equal")
CONT_EXAMPLES[[3]]<-list("info"="Linear constraint",example="p1+p2==2",com="Constrain the estimates of p1 and p2 to be equal to 2")
CONT_EXAMPLES[[4]]<-list("info"="Linear constraint",example="p1+p2+p3==2", com="Constrain the estimates for p1,p2, and p3")
CONT_EXAMPLES[[5]]<-list("info"="Linear constraint",example="p1+2*p2==0", com="Constrain the estimates of p1 plus twice p2 to be equal to 2")
CONT_EXAMPLES[[5]]<-list("info"="Constrain coefficients",example="p1==0", com="Fix the coefficient p1 to 0")
CONT_EXAMPLES[[6]]<-list("info"="Inequality Constraint",example="p1>0", com="Estimate the coefficient p1 as larger than 0")
CONT_EXAMPLES[[7]]<-list("info"="Inequality Constraint",example="p1<3", com="Estimate the coefficient p1 as smaller than 3")
CONT_EXAMPLES[[8]]<-list("info"="Constrain intercepts",example="y1~0", com="Fix the y1 intercept to 0")
CONT_EXAMPLES[[9]]<-list("info"="Constrain intercepts",example="y1~1*0", com="Fix the y1 intercept to 1")
CONT_EXAMPLES[[10]]<-list("info"="Non linear constraint",example="p1*p2=0", com="Constrain the estimates such that p1*p2 equals 0")

DP_EXAMPLES<-list()
DP_EXAMPLES[[1]]<-list("info"="Defined Parameters",example="",com="")
DP_EXAMPLES[[2]]<-list("info"="Linear estimates",example="dp:=p1+p2",com="p1 and p2 are free, and their sum is estimated and tested")
DP_EXAMPLES[[3]]<-list("info"="Linear estimates",example="dp:=(p1+p2)-p3",com="p1,p2, and p3 are free, and the specified function is estimated and tested")
DP_EXAMPLES[[4]]<-list("info"="Non linear estimates",example="aname:=p1^2", com="Estimate and test the square of p1")


SY_EXAMPLES<-list()
SY_EXAMPLES[[1]]<-list("info"="Free structural parameters",example="",com="")
SY_EXAMPLES[[2]]<-list("info"="Estimate residual coovariances",example="y1~~y2",com="Variables y1 and y2 covariance is set free")
SY_EXAMPLES[[3]]<-list("info"="Estimate exogenous variables covariances",example="x1~~x2",com="Variables x1 and x2 covariance is set free")
SY_EXAMPLES[[4]]<-list("info"="Estimate exogenous variables variances",example="x1~~x1",com="Variable x1 variance is set free")
SY_EXAMPLES[[5]]<-list("info"="Estimate  variables covariances",example="y1~~x1",com="Variables y1 and x1 covariance is set free. Direct path should not be set")
SY_EXAMPLES[[6]]<-list("info"="Estimate covariances involving interactions",example="x1:x2~~x3",com="The interaction term x1:x2 and x3 variable covariance is set free. Direct path should not be set")


CONT_NOTE<-"Automatic parameters labels are in the form `pN`, where `N` is a number. 
The parameter labels can be found in the results tables. Please be sure to have the options `Show parameters labels` selected."

WARNS<-list()
WARNS[["usercov"]]<-"Variances/Covariances specified by the user. The option  `Free Parameter - Exogenous Correlations` is overridden for these parameters"
WARNS[["nocenterd"]]<-"Variables {vars} are not centered Consider using `Continuous Variables Scaling options` for easier interpretation of lower order effects"
WARNS[["scoreineq"]]<-"Score Tests not available with inequality constraints"
WARNS[["noindirect"]]<-"Indirect effects cannot be computed for this model"
WARNS[["noreserved"]]<-"`{var}` label is reserved for indirect effects. Using it in defined parameters may create confusion and unexpected results."
WARNS[["cov.lv"]]<-"The covariance matrix of latent variables is not positive defenite. Please use 'Model-implied latent' option in 'Output options' panel to inspect it."

DATA_WARNS<-list()
DATA_WARNS[["fac_to_ord"]]<-"Variable ({x}) has been coerced to ordered type."
DATA_WARNS[["num_to_fac"]]<-"Variable ({x}) has been coerced to nominal type."
DATA_WARNS[["missing"]]<-"There are missing values in the data and they are removed listwise. Please consider other methods of handling missing values available in Model Options. "

## defined parameters warnings
DP_WARNS<-list()
DP_WARNS[[".p."]]<-"Warming: {x} is a reserved word (.pN.) for parameters labels. Please change it to avoid confusion with automatic parameters labels."
DP_WARNS[["p"]]<-"Warming: {x} is a reserved word (pN) for parameters labels. Please change it to avoid confusion with automatic parameters labels."


ERRS<-list()
ERRS[["noluck"]]<-"The model cannot be estimated. Please refine the model."

PLOT_WARNS<-list()
PLOT_WARNS[["nocircle"]]<-"Circle layout requires rotation to be `Exogenous Top` or `Exogenous Bottom`"
PLOT_WARNS[["rotation"]]<-"Circle layout requires rotation to be `Exogenous Top` or `Exogenous Bottom`. Rotation has been set to `Exogenous Top` "
PLOT_WARNS[["circlelayout"]]<-"Layout has been set to Circle"
PLOT_WARNS[["fail"]]<-"The diagram cannot be displayed. Please try a different layout type"

SUB<-list("\u2081","\u2082","\u2083","\u2084","\u2085","\u2086","\u2087","\u2088","\u2089","\u20810",
          "\u20811","\u20812","\u20813","\u20814","\u20815","\u20816","\u20817","\u20818","\u20819","\u20820")


TRANS_WARNS<-list()
TRANS_WARNS[[1]]<-list(original="subscript out of bounds",new="Computation cannot be done") 
TRANS_WARNS[[2]]<-list(original="lavaan ERROR",new="The model cannot be estimated, please refine it. Reason: {}") 
TRANS_WARNS[[3]]<-list(original="covariance matrix of latent",new="Covariance matrix of latent variables is not positive definite.") 


##### some info ####

ROBUST_ESTIM<-c("MLR", "MLM","MLMV","MLMVS","WLSM","WLSMV","WLSMVS")

INFO_TEST<-list()
INFO_TEST[["satorra.bentler"]]   <- "Satorra-Bentler mean adjusted"
INFO_TEST[["scaled.shifted"]]    <- "Mean adjusted scaled and shifted"
INFO_TEST[["mean.var.adjusted"]] <- "Mean and variance adjusted"
INFO_TEST[["yuan.bentler.mplus"]]<- " Yuan-Bentler T2*"
INFO_ML<-c("ML","MLM","MLMV","MLMVS")


INFO_SE<-list()
INFO_SE[["standard"]]<-"Standard"
INFO_SE[["robust.sem"]]<-"Robust"
INFO_SE[["boot"]]<-"Bootstrap"
INFO_SE[["robust.huber.white"]]<-"Robust Huber and White"

INFO_INDICES=list(
                cfi   =  "Comparative Fit Index (CFI)" , 
                tli   = "Tucker-Lewis Index (TLI)",
                nnfi  = "Bentler-Bonett Non-normed Fit Index (NNFI)",
                rni   = "Relative Noncentrality Index (RNI)",
                nfi   = "Bentler-Bonett Normed Fit Index (NFI)",
                rfi   = "Bollen's Relative Fit Index (RFI)",
                ifi   = "Bollen's Incremental Fit Index (IFI)",
                pnfi   = "Parsimony Normed Fit Index (PNFI)"
                
                )
INFO_MOREINDICES=list(
  
  cn_05 = "Hoelter Critical N (CN), α=0.05",  
  cn_01 = "Hoelter Critical N (CN), α=0.01", 
  gfi   = "Goodness of Fit Index (GFI)",
  agfi  = "Adjusted Goodness of Fit Index (AGFI)",
  pgfi  =  "Parsimony Goodness of Fit Index (PGFI)",
  mfi   =  "McDonald Fit Index (MFI)",
  ecvi  =  "Expected Cross-Validation Index (ECVI)",
  logl  =  "Loglikelihood user model (H0)"  ,
  unrestricted.logl = "Loglikelihood unrestricted model (H1)",
  aic   =  "Akaike (AIC)",
  bic   =  "Bayesian (BIC)",
  bic2  =   "Sample-size adjusted Bayesian (SABIC)"
)                


