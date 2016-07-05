readBerkeleyStation <- function(filename) {
  
   if(!file.exists(filename))stop(cat(filename, " not found", "\n"))
  
   
  
  switch(basename(filename),
         
         data.txt                     = { X<- tbl_df(read_delim(filename,col_names=c("Id","Series","Date","Temperature","Uncertainty", "Obs", "Tob"),
                                                    comment = "%",delim = "\t")) %>%
                                                    mutate(Year = as.integer(trunc(Date)), Temperature=as.integer(Temperature*10),
                                                           Month = as.integer(round(((Date %% 1)*12)+.5,0)),Date=NULL) %>% 
                                                    mutate(Obs = na_if(Obs,-99),Tob=na_if(Tob,-99)) %>%
                                                    arrange(Id, Year,Month) %>%
                                                    dplyr::select(Id,Year,Month,Temperature,Uncertainty,Obs,Tob,Series) %>%
                                                    return
                                        
                                         },
         site_summary.txt             = {X <- tbl_df(read.delim(filename,comment.char="%",header=FALSE,
                                                  col.names=c("Id","Latitude","Longitude","Elevation"))) %>%
                                                  mutate(Latitude = ifelse(Latitude < -99 ,NA,Latitude)) %>%
                                                  mutate(Longitude = ifelse(Longitude < -199 ,NA,Longitude)) %>%
                                                  mutate(Elevation = ifelse(Elevation < -998 ,NA,Elevation))  
                                                   
                                          return(X)
                                        },
         data_characterization.txt    = { },
         data_flag_definitions.txt    = { },
         flags.txt                    = { },
         site_complete_detail.txt     = { },
         site_detail.txt              = {X <- tbl_df(read.delim(filename,comment.char="%",quote="",
                                                       header=FALSE,stringsAsFactors = FALSE,
                                                       col.names=c("Id","Name", "Latitude","Longitude","Elevation" ,
                                                          "LatUnc","LonUnc","ElvUnc","Country", "State","County","Tzone", 
                                                          "WMO","COOP","WBAN","ICOA","Relo","SuggestedRelo",
                                                           "Sources","Hash")))
         
                                                   X <- X %>% map_if(is.character,str_trim) %>% tbl_df %>%
                                                    mutate(Country  =  na_if(Country,"[Missing]"))
                                                    mutate(Latitude =  ifelse(Latitude < -99 ,NA,Latitude)) %>%
                                                    mutate(Longitude = ifelse(Longitude < -199 ,NA,Longitude)) %>%
                                                    mutate(Elevation = ifelse(Elevation < -998 ,NA,Elevation)) %>%
                                                    mutate(LatUnc    = ifelse(LatUnc < -1 ,NA,LatUnc)) %>%
                                                    mutate(LonUnc    = ifelse(LonUnc < -1 ,NA,LonUnc)) %>%
                                                    mutate(ElvUnc    = ifelse(ElvUnc < -1 ,NA,ElvUnc)) %>%
                                                    mutate(State    =  na_if(State,"")) %>%
                                                    mutate(County    =  na_if(County,"")) %>%
                                                    mutate(ICOA    =  na_if(ICOA,"")) %>%
                                                    mutate(Tzone    =  na_if(Tzone,-99)) %>%
                                                    mutate(WMO    = ifelse(WMO < -990 ,NA,WMO)) %>%
                                                    mutate(COOP    = ifelse(COOP < -990 ,NA,COOP)) %>%
                                                    mutate(WBAN    = ifelse(WBAN < -990 ,NA,WBAN)) %>%
                                                    return
                                               
                                         },
         site_flag_definitions.txt    = { },
         site_flags.txt               = { },
         source_flag_definitions.txt  = { },
         sources.txt                  = { },
         station_change.txt           = { },
         
         
         stop(cat(filename, "not found"))
         
  )
  
  
  
  
 
  
}