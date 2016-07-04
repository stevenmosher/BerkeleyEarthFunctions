currentCopy <- function(filename,filter=c("TAVG","Single-Value")){
   
    filter <- tolower(filter)
    foundfiles <- list.files(path=getwd(),full.names=TRUE, recursive = TRUE, pattern = basename(filename))
    if(length(foundfiles)==0)stop(cat(filename, " not found","\n"))
    founddirs  <- dirname(foundfiles)
    if(length(foundfiles)==1){
      fname <-foundfiles
    }else
    {
      
      lastdir    <-  str_split(founddirs,pattern = "/")  
      dirlen    <-   sapply(lastdir,FUN= function(x){x[length(x)]})
      dirlen    <-  str_replace(dirlen,"_"," ")
      dirdate   <- str_replace(as.yearmon(max(as.Date(as.yearmon(dirlen))))," ","_")
      fname    <-foundfiles[grepl(dirdate,foundfiles)]
    }
    
   print(fname)
   return(fname)
  
}