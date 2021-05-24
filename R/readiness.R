
readiness <- function(options) {
  result <- list(reason = NULL, ready = TRUE, report = FALSE)


if(length(options$code) == 0 | options$code=="") {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- glue::glue("Please define a model to begin")
    return(result)
  } 

  
  return(result)
}
