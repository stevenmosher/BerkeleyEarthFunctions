readBerkeley  <- function(filename, Extent = NULL,Directory= "Current"){
  
  if(tolower(Directory) == "current"){
    
         foundfiles <- list.files(full.names=TRUE, recursive = TRUE, pattern = basename(filename))
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
    
  }else{
    fname <- file.path(Directory,filename)
    if(!file.exists(fname))stop(cat(fname,"not found","\n"))
  }
  
  NetCDF <- grepl(pattern = "\\.nc",fname)
  if(NetCDF) return(readBerkeleyNetCDF(filename = fname, Extent=Extent,start=start,end=end))
  
  ###   Not a netcdf 
  switch(filename,
         
          data.txt=                      { print("hi")},
          site.summary.txt =             {print("by")},
          data_characterization.txt   = {},
          data_flag_definitions.txt   = {},
          flags.txt                   = {},
          site_complete_detail.txt    = {},
          site_detail.txt             = {},
          site_flag_definitions.txt   = {},
          site_flags.txt              = {},
          source_flag_definitions.txt  = {},
          sources.txt                  = {},
          station_change.txt            = {},
          Land_and_Ocean_complete.txt  = {return(readBerkeleySummary(fname))},
          Complete_TAVG_complete.txt   = {return(readBerkeleySummary(fname))},
          Complete_TMAX_complete.txt   = {return(readBerkeleySummary(fname))},
          Complete_TMIN_complete.txt   = {return(readBerkeleySummary(fname))},
          Complete_TAVG_daily.txt      = {return(readBerkeleySummary(fname))},
          Complete_TMAX_daily.txt      = {return(readBerkeleySummary(fname))},
          Complete_TMIN_daily.txt      = {return(readBerkeleySummary(fname))},
          Land_and_Ocean_summary.txt   = {return(readBerkeleySummary(fname))},
          Complete_TAVG_summary.txt    = {return(readBerkeleySummary(fname))},
          Complete_TMAX_summary.txt    = {return(readBerkeleySummary(fname))},
          Complete_TMIN_summary.txt    = {return(readBerkeleySummary(fname))},
       
          stop(cat(filename, "not found"))
         
  )
  
  
  
  
}

 