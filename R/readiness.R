
readiness <- function(options) {
  result <- list(reason = NULL, ready = TRUE, report = FALSE)


  if(length(options$endogenous) == 0) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- glue::glue("we need at least 1 endogenous variable")
    return(result)
  } 

  if((length(options$factors) == 0) & (length(options$covs) == 0)) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- glue::glue("we need at least 1 exogenous variable")
    return(result)
  } 
  
  check<-sum(unlist(sapply(options$endogenousTerms, function(l) as.numeric(length(l)>0))))
  if (check< length(options$endogenous)) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- glue::glue("Predictors not specified for {length(options$endogenous)-check} endogenous variable")
    return(result)
  }

  check<-length(unlist(options$varcov))

  return(result)
}
