  
  source("Constants.R")
  functions <- list.files("Functions", full.name=TRUE,pattern = "\\.R")
  for(s in functions)source(s)
   