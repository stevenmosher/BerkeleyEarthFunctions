readBerkeley  <- function(filename, Extent = NULL){
  
  if(!file.exists(filename))stop(cat(filename, " Does not exist","\n"))
  
  NetCDF <- grepl(pattern = "\\.nc",filename)
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

 