download_Berkeley<- function(files,Directory=BERKELEY_DATA,overwrite=TRUE,log=TRUE){
  require(R.utils)
  ##  file can be a url
  ##  file can be an index to berkely data
  ##  file can be a collection of specifiers
  ##  return list of downloaded files
  
  filesValid <- FALSE
   
  if(class(Directory)[1]!="tbl_df"){
    stop("You must pass in a table dataframe like ",BERKELEY_DATA, "\n")
  }else
  {
   Qualifiers <- tolower(unique(unlist(select(Directory,Coverage,Area,TimePeriod,Type,Metric,Processing))))
   
   
  }
  if(length(files)==1){
   if(isUrl(files)){
      Found <- sum(grepl(files,Directory$Url))
      if(Found==1)filematches <- Directory$Index[grepl(files,Directory$Url)]
      if(Found!=1)stop("Url is not found in Table")
      Directory <- Directory[filematches,]
      filesValid<- TRUE
   }
  }
  
  if(class(files)=="integer" | class(files)=="character"){
    
    if(class(files)=="integer"){
      filematches <- files[files %in% Directory$Index]
      if(length(filematches)==0)stop(cat("Enter File indexes from ","\n",Directory$Index))
      Directory <- Directory[filematches,]
    }
    if(class(files)=="character"){
      files <- tolower(files)
      #  check for any matches
      if(sum(files %in% Qualifiers)>0){
        if(sum(files %in% Qualifiers)!=length(files))stop(cat("invalid selector ",files, "\n", "choose ",Qualifiers))
        CoverageRows <-which( tolower(Directory$Coverage) %in% files)
        if(length(CoverageRows)>0) Directory <- Directory[CoverageRows,]
        AreaRows     <-which( tolower(Directory$Area) %in% files)
        if(length(AreaRows)>0) Directory <- Directory[AreaRows,]
        TimeRows     <-which( tolower(Directory$TimePeriod) %in% files)
        if(length(TimeRows)>0) Directory <- Directory[TimeRows,]
        TypeRows     <-which( tolower(Directory$Type) %in% files)
        if(length(TypeRows)>0) Directory <- Directory[TypeRows,]
        MetricRows     <-which( tolower(Directory$Metric) %in% files)
        if(length(MetricRows)>0) Directory <- Directory[MetricRows,]
        ProcessRows     <-which( tolower(Directory$Processing) %in% files)
        if(length(ProcessRows)>0) Directory <- Directory[ProcessRows,]
      
      }else
      {
        stop(cat( files, "No  valid selectors","\n", Qualifiers,"\n"))
      }
       
      
    }
    filesValid<- TRUE
    
  }  
  
  #########################   Directory  Contains all the files to be Processed
  
  # Now step through the directory
  # create the Dir
  # Get the month
  # check if the month has been created
  # check for Overwriting
  # download and unzip if necessary.
  # collect the file names, full path
  #return a tbl_df?
  if(!filesValid)stop(cat("files has to a be a vector of integers, a single url or charcter strings","\n",
                       files))
  Year_Month <- str_replace(as.yearmon(as.character(Sys.Date()))," ","_")
   
  
  
  D<- Directory
  Destinations <-vector()
  for(i in 1:nrow(D)){
    
    currenturl <- D$Url[i]
    cat("Downloading ", currenturl, "\n")
    destinationname <- basename(currenturl)
    Dirs <- file.path(D$Directory[i],D$Area[i],D$Metric[i],D$Processing[i],D$Coverage[i],
                              D$TimePeriod[i],D$Type[i],Year_Month)
    if(!dir.exists(Dirs))dir.create(Dirs,recursive = TRUE)
    destinationname<-file.path(Dirs,destinationname)
    if(!file.exists(destinationname) | (file.exists(destinationname) & overwrite)){
       download.file(url=currenturl,destfile=destinationname,mode="wb")
       if(D$Extension[i]!="zip")Destinations <-c(Destinations,destinationname)
       if(D$Extension[i]=="zip"){
           zipped <- unzip(destinationname,list=T)
           cat("Unzipping ", zipped, "\n")
           Destinations<-c(Destinations,file.path(Dirs,zipped$Name))
           unzip(zipfile=destinationname,exdir=Dirs,junkpaths=T)
      
        }
    
    }
    
    
    
  }
  
   
  
 return(Destinations) 
  
}

 