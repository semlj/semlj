
readiness <- function(options) {
  result <- list(reason = NULL, ready = TRUE, report = FALSE)


if(length(options$code) == 0 | options$code=="") {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- glue::glue("Please define a model to begin")
    return(result)
  } 

  if ("endogenousTerms" %in% names(options)) {
     if (length(unlist(options$endogenousTerms))==0) {
       result$ready <- FALSE
       result$report <- TRUE
       result$reason <- glue::glue("Please define a structural relationship")
       return(result)
       
     }
  }
     
    
  return(result)
}
