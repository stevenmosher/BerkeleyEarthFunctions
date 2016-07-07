readBerkeleyStation <- function(filename) {
  
   if(!file.exists(filename))stop(cat(filename, " not found", "\n"))
  
  notallna <- function(x){ any(!is.na(x))}
  
  switch(basename(filename),
         
         data.txt                     = { X<- tbl_df(read_delim(filename,col_names=c("Id","Series","Date","Temperature","Uncertainty", "Obs", "Tob"),
                                                    comment = "%",delim = "\t")) %>%
                                                    mutate(Year = as.integer(trunc(Date)), Temperature=as.integer(Temperature*10),
                                                           Month = as.integer(round(((Date %% 1)*12)+.5,0)),Date=NULL) %>% 
                                                    mutate(Obs = na_if(Obs,-99),Tob=na_if(Tob,-99)) %>%
                                                    arrange(Id, Year,Month) %>%
                                                    dplyr::select(Id,Year,Month,Temperature,Uncertainty,Obs,Tob,Series) %>%  return
                                         },
         site_summary.txt             = {X <- tbl_df(read.delim(filename,comment.char="%",header=FALSE,
                                                  col.names=c("Id","Latitude","Longitude","Elevation"))) %>%
                                                  mutate(Latitude = ifelse(Latitude < -99 ,NA,Latitude)) %>%
                                                  mutate(Longitude = ifelse(Longitude < -199 ,NA,Longitude)) %>%
                                                  mutate(Elevation = ifelse(Elevation < -998 ,NA,Elevation)) %>% return 
                                        },
         data_characterization.txt    = {X<- tbl_df(read_delim(filename,comment = "%",
                                                      col_names=c("Id","Series","DataValues","EarlyDate","LateDate","UniqueDate",
                                                                  "Missing","TobKnown","UnderlyingKnown","MaxValue","MinValue",
                                                                  "MeanValue","MedianValue","FirstQuartile","LastQuartile","StdDev",
                                                                  "BadFlags","dummy"),delim="\t")) %>% 
                                         mutate(EarlyYear = as.integer(trunc(EarlyDate)),EarlyMonth = as.integer(round(((EarlyDate %% 1)*12)+.5,0))) %>%  
                                         mutate(LateYear = as.integer(trunc(LateDate)),LateMonth = as.integer(round(((LateDate %% 1)*12)+.5,0))) %>% 
                                         select(Id,Series,EarlyYear,EarlyMonth,LateYear,LateMonth,DataValues, UniqueDate,
                                                 Missing,TobKnown,UnderlyingKnown,MaxValue,MinValue,
                                                   MeanValue,MedianValue,FirstQuartile,LastQuartile,StdDev,BadFlags) %>% return
           
           
           
           return
         },
         data_flag_definitions.txt    = {X <- tbl_df(read.delim(filename,   comment.char = "%",quote ="", header = FALSE, 
                                                    col.names=c("Flag","Description"), stringsAsFactors= FALSE)) %>% return
                                         },
         flags.txt                    = {X <- tbl_df(read.delim(filename, comment.char = "%",quote ="",header = FALSE,
                                                                 stringsAsFactors= FALSE))%>%
                                         select_if(function(col) notallna(col)) %>% 
                                         rename(Id = V1,Series=V2,Date=V3) %>%
                                         gather(Flags,Code,starts_with("V")) %>%
                                         filter(Code !=0) %>%
                                         mutate(Year = as.integer(trunc(Date)),Month=as.integer(round(((Date %% 1)*12)+.5,0)),Date=NULL) %>%
                                         select(Id,Series,Year,Month,Code)  %>% return 
                                         },
         site_complete_detail.txt     = { X <- tbl_df(read.delim(filename ,comment.char = "%", quote ="",header = FALSE, stringsAsFactors= FALSE,
                                                      col.names= c("Id","ReportNo","Name","Country","StartDate","EndDate","Source","Latitude","Longitude",
                                                      "Elevation" ,"AltElevation","LatUnc","LonUnc","ElvUnc", "ReloFlag","State","County","Tzone",
                                                      "WMO","COOP","WBAN","ICOA", "USAF","NCDC","Instrument","OtherId","ArchiveKey","Hash"))) %>%                                  
                                          map_if(is.character,str_trim) %>% tbl_df %>%
                                          mutate(Country   = na_if(Country,"[Missing]")) %>%
                                          mutate(Latitude  = ifelse(Latitude < -99 ,NA,Latitude)) %>%
                                          mutate(Longitude = ifelse(Longitude < -199 ,NA,Longitude)) %>%
                                          mutate(Elevation = ifelse(Elevation < -998 ,NA,Elevation)) %>%
                                          mutate(LatUnc    = ifelse(LatUnc < -1 ,NA,LatUnc)) %>%
                                          mutate(LonUnc    = ifelse(LonUnc < -1 ,NA,LonUnc)) %>%
                                          mutate(ElvUnc    = ifelse(ElvUnc < -1 ,NA,ElvUnc)) %>%
                                          mutate(State     = na_if(State,"")) %>%
                                          mutate(County    = na_if(County,"")) %>%
                                          mutate(ICOA      = na_if(ICOA,"")) %>%
                                          mutate(OtherId   = na_if(OtherId,"")) %>%
                                          mutate(Tzone     = na_if(Tzone,-99)) %>%
                                          mutate(WMO       = ifelse(WMO < -990 ,NA,WMO)) %>%
                                          mutate(COOP      = ifelse(COOP < -990 ,NA,COOP)) %>%
                                          mutate(WBAN      = ifelse(WBAN < -990 ,NA,WBAN)) %>%
                                          mutate(StartYear = as.integer(trunc(StartDate)),StartMonth = as.integer(round(((StartDate %% 1)*12)+.5,0)),StartDate=NULL) %>%  
                                          mutate(EndYear   = as.integer(trunc(EndDate)),EndMonth = as.integer(round(((EndDate %% 1)*12)+.5,0)),EndDate=NULL) %>% return
                                         },
         site_detail.txt              = {X <- tbl_df(read.delim(filename,comment.char="%",quote="",header=FALSE,stringsAsFactors = FALSE,
                                                                col.names=c("Id","Name", "Latitude","Longitude","Elevation" ,"LatUnc","LonUnc","ElvUnc",
                                                                            "Country", "State","County","Tzone","WMO","COOP","WBAN","ICOA","Relo", 
                                                                            "SuggestedRelo","Sources","Hash"))) %>%
                                          map_if(is.character,str_trim) %>% tbl_df %>%
                                          mutate(Country  =  na_if(Country,"[Missing]")) %>%
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
                                          mutate(WBAN    = ifelse(WBAN < -990 ,NA,WBAN)) %>% return
                                         },
         site_flag_definitions.txt    = { X <- tbl_df(read.delim(filename,comment.char = "%", 
                                                                 quote ="",col.names=c("Flag","Description"),
                                                                 header = FALSE, 
                                                                 stringsAsFactors= FALSE)) %>% return
                                         },
         site_flags.txt               = {X <- tbl_df(read.delim(filename, comment.char = "%",
                                                          quote ="",header = FALSE,stringsAsFactors= FALSE))%>%
                                         select_if(function(col) notallna(col)) %>% 
                                         rename(Id = V1) %>%
                                         gather(Flags,Code,starts_with("V")) %>%
                                         filter(Code !=0) %>%
                                         select(Id,Code) %>% return
                                         },
         source_flag_definitions.txt  = { X <- tbl_df(read.delim(filename,  comment.char = "%",
                                                          col.names=c("Flag","Description"),
                                                          quote ="",header = FALSE,stringsAsFactors= FALSE)) %>% return
                                        },
         sources.txt                  = { X <- tbl_df(x<-read.delim(filename,   comment.char = "%",quote ="", header = FALSE, stringsAsFactors= FALSE, 
                                                      col.names = c("Id","Series","Date","F1","F2","F3","F4","F5","F6","F7","F8","F9"))) %>%
                                          gather(Flags,Code,starts_with("F")) %>% 
                                          filter(Code !=0) %>%  
                                          select(Id,Series,Date,Code) %>%
                                          mutate(Year = as.integer(trunc(Date)),Month=as.integer(round(((Date %% 1)*12)+.5,0)),Date=NULL) %>%
                                          select(Id,Series,Year,Month,Code) %>% return 
         
         
                                         },
         station_change.txt           = {X <- tbl_df(read.delim(filename,comment.char = "%",
                                                         quote ="",header = FALSE,stringsAsFactors= FALSE,
                                                         col.names=c("Id","EventCount","Date","ChangeType"))) %>%
                                              mutate(Year = as.integer(trunc(Date)),Month = as.integer(round(((Date %% 1)*12)+.5,0)),
                                                     Date=NULL) %>% return
                                        },
         
         
         stop(cat(filename, "not found"))
         
  )
  
  
  
  
 
  
}