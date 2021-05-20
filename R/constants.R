j_DEBUG=T
j_INFO=T


INTERACTION_SYMBOL="__XX__XX__"
FACTOR_SYMBOL="._._._."

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


CONT_NOTE<-"Not user defined labels are in the form `pN`, where `N` is a number. 
The parameter labels can be found in the results tables. Please be sure to have the options `Show parameters labels` selected."

WARNS<-list()
WARNS[["usercov"]]<-"Variances/Covariances specified by the user. The option  `Free Parameter - Exogenous Correlations` is overridden for these parameters"
WARNS[["nocenterd"]]<-"Variables {vars} are not centered Consider using `Continuous Variables Scaling options` for easier interpretation of lower order effects"
WARNS[["scoreineq"]]<-"Score Tests not available with inequality constraints"
WARNS[["noindirect"]]<-"Indirect effects cannot be computed for this model"
WARNS[["noreserved"]]<-"`{var}` label is reserved for indirect effects. Using it in defined parameters may create confusion and unexpected results."

DATA_WARNS<-list()
DATA_WARNS[["fac_to_cont"]]<-"Warming: continuous variable are defined as factor. Please make sure that each is a continuous variable."
DATA_WARNS[["cont_to_fac"]]<-"Warning: variable coerced to factor"

ERRS<-list()
ERRS[["nolatent"]]<-"Latent variables are not allowed in pathj. Please use another  SEM module"
ERRS[["noluck"]]<-"The model cannot be estimated. Please refine the model"

PLOT_WARNS<-list()
PLOT_WARNS[["nocircle"]]<-"Circle layout requires rotation to be `Exogenous Top` or `Exogenous Bottom`"
PLOT_WARNS[["rotation"]]<-"Circle layout requires rotation to be `Exogenous Top` or `Exogenous Bottom`. Rotation has been set to `Exogenous Top` "
PLOT_WARNS[["circlelayout"]]<-"Layout has been set to Circle"
PLOT_WARNS[["fail"]]<-"The diagram cannot be displayed. Please try a different layout type"

SUB<-list("\u2081","\u2082","\u2083","\u2084","\u2085","\u2086","\u2087","\u2088","\u2089","\u20810",
          "\u20811","\u20812","\u20813","\u20814","\u20815","\u20816","\u20817","\u20818","\u20819","\u20820")

