readBerkeley  <- function(filename, Extent = NULL){
  
  if(!file.exists(filename))stop(cat(filename, " Does not exist","\n"))
  
  NetCDF <- grepl(pattern = "\\.nc",filename)
  if(NetCDF) return(readBerkeleyNetCDF(filename = filename, Extent=Extent))
  
  ###   Not a netcdf 
  switch(basename(filename),
         
          data.txt                     = {return(readBerkeleyStation(filename=filename))},
          site_summary.txt             = {return(readBerkeleyStation(filename=filename))},
          data_characterization.txt    = {return(readBerkeleyStation(filename=filename))},
          data_flag_definitions.txt    = {return(readBerkeleyStation(filename=filename))},
          flags.txt                    = {return(readBerkeleyStation(filename=filename))},
          site_complete_detail.txt     = {return(readBerkeleyStation(filename=filename))},
          site_detail.txt              = {return(readBerkeleyStation(filename=filename))},
          site_flag_definitions.txt    = {return(readBerkeleyStation(filename=filename))},
          site_flags.txt               = {return(readBerkeleyStation(filename=filename))},
          source_flag_definitions.txt  = {return(readBerkeleyStation(filename=filename))},
          sources.txt                  = {return(readBerkeleyStation(filename=filename))},
          station_change.txt           = {return(readBerkeleyStation(filename=filename))},
          Land_and_Ocean_complete.txt  = {return(readBerkeleySummary(filename=filename))},
          Complete_TAVG_complete.txt   = {return(readBerkeleySummary(filename=filename))},
          Complete_TMAX_complete.txt   = {return(readBerkeleySummary(filename=filename))},
          Complete_TMIN_complete.txt   = {return(readBerkeleySummary(filename=filename))},
          Complete_TAVG_daily.txt      = {return(readBerkeleySummary(filename=filename))},
          Complete_TMAX_daily.txt      = {return(readBerkeleySummary(filename=filename))},
          Complete_TMIN_daily.txt      = {return(readBerkeleySummary(filename=filename))},
          Land_and_Ocean_summary.txt   = {return(readBerkeleySummary(filename=filename))},
          Complete_TAVG_summary.txt    = {return(readBerkeleySummary(filename=filename))},
          Complete_TMAX_summary.txt    = {return(readBerkeleySummary(filename=filename))},
          Complete_TMIN_summary.txt    = {return(readBerkeleySummary(filename=filename))},
       
          stop(cat(filename, "not found"))
         
  )
  
  
  
  
}

 