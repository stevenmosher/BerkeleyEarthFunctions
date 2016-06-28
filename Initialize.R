Initialize <- function(){
  source("Libraries.R")
  source("Constants.R")
  functions <- list.files(FUNCTIONS,full.names=T, pattern = "\\.R")
  if(length(functions)>0)lapply(functions, FUN=source)
  return(getwd())
}