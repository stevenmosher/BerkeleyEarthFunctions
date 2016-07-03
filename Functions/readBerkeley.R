readBerkeley  <- function(filename, Extent = NULL){
  
  if(!file.exists(filename))stop(cat(filename, " Does not exist","\n"))
  
  NetCDF <- grepl(pattern = "\\.nc",filename)
  if(NetCDF) return(readBerkeleyNetCDF(filename = fname, Extent=Extent))
  
  ###   Not a netcdf 
  switch(basename(filename),
         
          data.txt                     = {return(readBerkeleyStation(filename))},
          site.summary.txt             = {return(readBerkeleyStation(filename))},
          data_characterization.txt    = {return(readBerkeleyStation(filename))},
          data_flag_definitions.txt    = {return(readBerkeleyStation(filename))},
          flags.txt                    = {return(readBerkeleyStation(filename))},
          site_complete_detail.txt     = {return(readBerkeleyStation(filename))},
          site_detail.txt              = {return(readBerkeleyStation(filename))},
          site_flag_definitions.txt    = {return(readBerkeleyStation(filename))},
          site_flags.txt               = {return(readBerkeleyStation(filename))},
          source_flag_definitions.txt  = {return(readBerkeleyStation(filename))},
          sources.txt                  = {return(readBerkeleyStation(filename))},
          station_change.txt           = {return(readBerkeleyStation(filename))},
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

 