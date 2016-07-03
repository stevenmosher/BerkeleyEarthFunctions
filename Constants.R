#######  Constants and  global data
#############   URLS 
u1 <- "http://berkeleyearth.lbl.gov/auto/Global/Complete_TAVG_complete.txt"
u2 <- "http://berkeleyearth.lbl.gov/auto/Global/Complete_TMAX_complete.txt"
u3 <- "http://berkeleyearth.lbl.gov/auto/Global/Complete_TMIN_complete.txt"
u4 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TAVG_LatLong1.nc"
u5 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMAX_LatLong1.nc"
u6 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMIN_LatLong1.nc"
u7 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Land_and_Ocean_LatLong1.nc"
u8 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Land_and_Ocean_Alternate_LatLong1.nc"
u9 <- "http://berkeleyearth.lbl.gov/downloads/TAVG/LATEST%20-%20Breakpoint%20Corrected.zip"
u10<- "http://berkeleyearth.lbl.gov/downloads/TMIN/LATEST%20-%20Breakpoint%20Corrected.zip"
u11<- "http://berkeleyearth.lbl.gov/downloads/TMAX/LATEST%20-%20Breakpoint%20Corrected.zip"
u12<- "http://berkeleyearth.lbl.gov/downloads/TAVG/LATEST%20-%20Multi-valued.zip"
u13<- "http://berkeleyearth.lbl.gov/downloads/TMAX/LATEST%20-%20Multi-valued.zip"
u14<- "http://berkeleyearth.lbl.gov/downloads/TMIN/LATEST%20-%20Multi-valued.zip"
u15<- "http://berkeleyearth.lbl.gov/downloads/TAVG/LATEST%20-%20Single-valued.zip"
u16<- "http://berkeleyearth.lbl.gov/downloads/TMAX/LATEST%20-%20Single-valued.zip"
u17<- "http://berkeleyearth.lbl.gov/downloads/TMIN/LATEST%20-%20Single-valued.zip"
u18<- "http://berkeleyearth.lbl.gov/downloads/TAVG/LATEST%20-%20Quality%20Controlled.zip"
u19<- "http://berkeleyearth.lbl.gov/downloads/TMAX/LATEST%20-%20Quality%20Controlled.zip"
u20<- "http://berkeleyearth.lbl.gov/downloads/TMIN/LATEST%20-%20Quality%20Controlled.zip"
u21<- "http://berkeleyearth.lbl.gov/auto/Global/Complete_TAVG_daily.txt"
u22<- "http://berkeleyearth.lbl.gov/auto/Global/Complete_TMAX_daily.txt"
u23<- "http://berkeleyearth.lbl.gov/auto/Global/Complete_TMIN_daily.txt"
u24<-  "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TAVG_Daily_LatLong1_1880.nc"
u25<-  "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TAVG_Daily_LatLong1_1890.nc"
u26 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TAVG_Daily_LatLong1_1900.nc"
u27 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TAVG_Daily_LatLong1_1910.nc"
u28 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TAVG_Daily_LatLong1_1920.nc"
u29 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TAVG_Daily_LatLong1_1930.nc"
u30 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TAVG_Daily_LatLong1_1940.nc"
u31 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TAVG_Daily_LatLong1_1950.nc"
u32 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TAVG_Daily_LatLong1_1960.nc"
u33 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TAVG_Daily_LatLong1_1970.nc"
u34 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TAVG_Daily_LatLong1_1980.nc"
u35 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TAVG_Daily_LatLong1_1990.nc"
u36 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TAVG_Daily_LatLong1_2000.nc"
u37 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TAVG_Daily_LatLong1_2010.nc"
u38<-  "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMAX_Daily_LatLong1_1880.nc"
u39<-  "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMAX_Daily_LatLong1_1890.nc"
u40 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMAX_Daily_LatLong1_1900.nc"
u41 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMAX_Daily_LatLong1_1910.nc"
u42 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMAX_Daily_LatLong1_1920.nc"
u43 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMAX_Daily_LatLong1_1930.nc"
u44 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMAX_Daily_LatLong1_1940.nc"
u45 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMAX_Daily_LatLong1_1950.nc"
u46 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMAX_Daily_LatLong1_1960.nc"
u47 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMAX_Daily_LatLong1_1970.nc"
u48 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMAX_Daily_LatLong1_1980.nc"
u49 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMAX_Daily_LatLong1_1990.nc"
u50 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMAX_Daily_LatLong1_2000.nc"
u51 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMAX_Daily_LatLong1_2010.nc"
u52<-  "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMIN_Daily_LatLong1_1880.nc"
u53<-  "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMIN_Daily_LatLong1_1890.nc"
u54 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMIN_Daily_LatLong1_1900.nc"
u55 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMIN_Daily_LatLong1_1910.nc"
u56 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMIN_Daily_LatLong1_1920.nc"
u57 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMIN_Daily_LatLong1_1930.nc"
u58 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMIN_Daily_LatLong1_1940.nc"
u59 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMIN_Daily_LatLong1_1950.nc"
u60 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMIN_Daily_LatLong1_1960.nc"
u61 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMIN_Daily_LatLong1_1970.nc"
u62 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMIN_Daily_LatLong1_1980.nc"
u63 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMIN_Daily_LatLong1_1990.nc"
u64 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMIN_Daily_LatLong1_2000.nc"
u65 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMIN_Daily_LatLong1_2010.nc"
u66 <- "http://berkeleyearth.lbl.gov/auto/Global/Land_and_Ocean_complete.txt"
u67 <- "http://berkeleyearth.lbl.gov/auto/Global/Complete_TAVG_summary.txt"
u68 <- "http://berkeleyearth.lbl.gov/auto/Global/Complete_TMAX_summary.txt"
u69 <- "http://berkeleyearth.lbl.gov/auto/Global/Complete_TMIN_summary.txt"
u70 <- "http://berkeleyearth.lbl.gov/auto/Global/Land_and_Ocean_summary.txt"

DOWNLOADS <- "Downloads"
FUNCTIONS <- "Functions"
SCRIPTS   <- "Scripts"
DATABASE  <- "Databases"
METADATA  <- "Metadata"

if(!dir.exists(DOWNLOADS))dir.create(DOWNLOADS)
if(!dir.exists(FUNCTIONS))dir.create(FUNCTIONS)
if(!dir.exists(SCRIPTS))dir.create(SCRIPTS)
if(!dir.exists(DATABASE))dir.create(DATABASE)
if(!dir.exists(METADATA))dir.create(METADATA)
 


#############################  END URLS
# Global, Regional, Country,State,City,Station
# Gridded, TimeSeries
# monthly, Daily
# Land, LandOcean
# Adjusted, Raw, QC, Source,
# TAVG, TMAX, TMin


##############################  URLS  as a table

BERKELEY_DATA <- tbl_df(data.frame(Url = c(u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15,u16,u17,u18,u19,u20,
                                           u21,u22,u23,u24,u25,u26,u27,u28,u29,u30,u31,u32,u33,u34,u35,u36,u37,u38,u39,u40,
                                           u41,u42,u43,u44,u45,u46,u47,u48,u49,u50,
                                           u51,u52,u53,u54,u55,u56,u57,u58,u59,u60,
                                           u61,u62,u63,u64,u65,u66,u67,u68,u69,u70),
                                   stringsAsFactors=FALSE)) %>%
                 mutate(Extension = str_sub(Url, unlist(lapply(str_locate_all(Url,pattern="\\.") , FUN=max))+1,
                                            nchar(Url)))    %>%
                 mutate(Directory = DOWNLOADS, Area= "Global", TimePeriod  ="Monthly") %>%
                 mutate(Processing ="Averaged",Metric =  "TAVG") %>%
                 mutate(Index=1:n())
                 
                
                 


BERKELEY_DATA$Metric[grepl("TMAX",BERKELEY_DATA$Url)] <-"TMAX"
BERKELEY_DATA$Metric[grepl("TMIN",BERKELEY_DATA$Url)] <-"TMIN"
BERKELEY_DATA$Processing[grepl("Multi-valued",BERKELEY_DATA$Url)]  <-"Multi-valued"
BERKELEY_DATA$Processing[grepl("Breakpoint",BERKELEY_DATA$Url)]    <-"Breakpoint"
BERKELEY_DATA$Processing[grepl("Single-valued",BERKELEY_DATA$Url)] <-"Single-valued"
BERKELEY_DATA$Processing[grepl("Quality",BERKELEY_DATA$Url)]       <-"Quality"
BERKELEY_DATA$Processing[BERKELEY_DATA$Extension=="nc"]            <-"Gridded"
BERKELEY_DATA$TimePeriod[grepl("daily",BERKELEY_DATA$Url,ignore.case=T)]<-"Daily"
BERKELEY_DATA$TimePeriod[grepl("summary",BERKELEY_DATA$Url,ignore.case=T)]<-"Annual"
BERKELEY_DATA$Area[grepl("Breakpoint",BERKELEY_DATA$Url)]   <-"Station"
BERKELEY_DATA$Area[grepl("Multi-valued",BERKELEY_DATA$Url)]  <-"Station"
BERKELEY_DATA$Area[grepl("Single-valued",BERKELEY_DATA$Url)] <-"Station"
BERKELEY_DATA$Area[grepl("Quality",BERKELEY_DATA$Url)]       <-"Station"





                            