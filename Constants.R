#######  Constants and  global data

u1 <-"http://berkeleyearth.lbl.gov/auto/Global/Complete_TAVG_complete.txt"
u2 <- "http://berkeleyearth.lbl.gov/auto/Global/Complete_TMAX_complete.txt"
u3 <- "http://berkeleyearth.lbl.gov/auto/Global/Complete_TMIN_complete.txt"
u4 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TAVG_LatLong1.nc"
u5 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMAX_LatLong1.nc"
u6 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMIN_LatLong1.nc"
u7 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Complete_TMAX_LatLong1.nc"
u8 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Land_and_Ocean_LatLong1.nc"
u9 <- "http://berkeleyearth.lbl.gov/auto/Global/Gridded/Land_and_Ocean_Alternate_LatLong1.nc"
u10<- "http://berkeleyearth.lbl.gov/downloads/TAVG/LATEST%20-%20Breakpoint%20Corrected.zip"
u11<- "http://berkeleyearth.lbl.gov/downloads/TMIN/LATEST%20-%20Breakpoint%20Corrected.zip"
u12<- "http://berkeleyearth.lbl.gov/downloads/TMAX/LATEST%20-%20Breakpoint%20Corrected.zip"
u13<- "http://berkeleyearth.lbl.gov/downloads/TAVG/LATEST%20-%20Multi-valued.zip"
u14<- "http://berkeleyearth.lbl.gov/downloads/TMAX/LATEST%20-%20Multi-valued.zip"
u15<- "http://berkeleyearth.lbl.gov/downloads/TMIN/LATEST%20-%20Multi-valued.zip"
u16<- "http://berkeleyearth.lbl.gov/downloads/TAVG/LATEST%20-%20Single-valued.zip"
u17<- "http://berkeleyearth.lbl.gov/downloads/TMAX/LATEST%20-%20Single-valued.zip"
u18<- "http://berkeleyearth.lbl.gov/downloads/TMIN/LATEST%20-%20Single-valued.zip"
u19<- "http://berkeleyearth.lbl.gov/downloads/TAVG/LATEST%20-%20Quality%20Controlled.zip"
u20<- "http://berkeleyearth.lbl.gov/downloads/TAVG/LATEST%20-%20Quality%20Controlled.zip"
u21<- "http://berkeleyearth.lbl.gov/downloads/TAVG/LATEST%20-%20Quality%20Controlled.zip"




BERKELEY_DATA <- data.frame(url = c(u1,u2,u2,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15,u16,u17,u18,u19,u20,u21),
                            format =c(),)