###
 source("Initialize.R")
 Initialize()
 
 selectors <- c("global","monthly","tavg")
 FILES     <- download_Berkeley(files=selectors,Directory = BERKELEY_DATA,log=DOWNLOG)
 
 
 selectors <- c("station","monthly","tavg","breakpoint","quality")
 FILES2     <- download_Berkeley(files=selectors,Directory = BERKELEY_DATA,overwrite=F,log=DOWNLOG)
 
 
 
 
  
 
 DB <-dbConnect(SQLite(),dbname=DOWNLOG,flags=SQLITE_RW)
 Log <-dbReadTable(DB,"Log")