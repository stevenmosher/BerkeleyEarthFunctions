
TMAX<-readBerkeley(filename = "Complete_TMAX_summary.txt")
TAVG<-readBerkeley(filename = "Complete_TAVG_summary.txt")
TMIN<-readBerkeley(filename = "Complete_TMIN_summary.txt")
TAVG<-readBerkeley(filename = "Complete_TAVG_complete.txt")
TMAX<-readBerkeley(filename = "Complete_TMAX_complete.txt")
TMIN<-readBerkeley(filename = "Complete_TMIN_complete.txt")
TAVG<-readBerkeley(filename = "Complete_TAVG_daily.txt")
TMAX<-readBerkeley(filename = "Complete_TMAX_daily.txt")
TMIN<-readBerkeley(filename = "Complete_TMIN_daily.txt")

LO<-readBerkeley(filename = "Land_and_Ocean_complete.txt")
LO2<-readBerkeley(filename = "Land_and_Ocean_summary.txt")

S1 <-readBerkeley(filename = "data.txt")
S2 <-readBerkeley(filename = "site.summary.txt")
S3 <-readBerkeley(filename = "data_characterization.txt")
S4 <-readBerkeley(filename = "data_flag_definitions.txt")
S5 <-readBerkeley(filename = "flags.txt")
S6 <-readBerkeley(filename = "site_complete_detail.txt")
S7 <-readBerkeley(filename = "site_detail.txt")
S8 <-readBerkeley(filename = "site_flag_definitions.txt")
S9 <-readBerkeley(filename = "site_flags.txt")
S10 <-readBerkeley(filename = "source_flag_definitions.txt")
S11 <-readBerkeley(filename = "sources.txt")
S12 <-readBerkeley(filename = "station_change.txt")

SS <- list.files(path=getwd(),recursive = TRUE,full.names=T, pattern="site_summary.txt")

for(i in 1:length(SS)){
  print(i)
  G <-readBerkeley(filename=SS[i])
  print(max(G$Latitude))
  print(min(G$Latitude))
  print(max(G$Longitude))
  print(min(G$Longitude))
  print(max(G$Elevation))
  print(min(G$Elevation))
  print(max(G$Latitude,na.rm=T))
  print(min(G$Latitude,na.rm=T))
  print(max(G$Longitude,na.rm=T))
  print(min(G$Longitude,na.rm=T))
  print(max(G$Elevation,na.rm=T))
  print(min(G$Elevation,na.rm=T))
  print(SS[i])
  
}
 
SS <- list.files(path=getwd(),recursive = TRUE,full.names=T, pattern="site_detail.txt")

for(i in 1:length(SS)){
  print(i)
  G <-readBerkeley(filename=SS[i])
  print(max(G$Latitude))
  print(min(G$Latitude))
  print(max(G$Longitude))
  print(min(G$Longitude))
  print(max(G$Elevation))
  print(min(G$Elevation))
  print(max(G$Latitude,na.rm=T))
  print(min(G$Latitude,na.rm=T))
  print(max(G$Longitude,na.rm=T))
  print(min(G$Longitude,na.rm=T))
  print(max(G$Elevation,na.rm=T))
  print(min(G$Elevation,na.rm=T))
  print(SS[i])
  
}


SS <- list.files(path=getwd(),recursive = TRUE,full.names=T, pattern="site_flag_definitions.txt")
for(i in 1:length(SS)){
  print(i)
  G <-readBerkeley(filename=SS[i])
   
  print(SS[i])
  
}

SS <- list.files(path=getwd(),recursive = TRUE,full.names=T, pattern="source_flag_definitions.txt")
for(i in 1:length(SS)){
  print(i)
  G <-readBerkeley(filename=SS[i])
  print(nrow(G))
  print(SS[i])
  
}

SS <- list.files(path=getwd(),recursive = TRUE,full.names=T, pattern="station_change.txt")
for(i in 1:length(SS)){
  print(i)
  G <-readBerkeley(filename=SS[i])
  print(nrow(G))
  print(SS[i])
  
}

SS <- list.files(path=getwd(),recursive = TRUE,full.names=T, pattern="data_flag_definitions.txt")
for(i in 1:length(SS)){
  print(i)
  G <-readBerkeley(filename=SS[i])
  print(nrow(G))
  print(SS[i])
  
}

SS <- list.files(path=getwd(),recursive = TRUE,full.names=T, pattern="site_flags.txt")
for(i in 1:length(SS)){
  print(i)
  G <-readBerkeley(filename=SS[i])
  print(ncol(G))
  
  print(SS[i])
  
}


SS <- list.files(path=getwd(),recursive = TRUE,full.names=T, pattern="data_characterization.txt")
for(i in 1:length(SS)){
  print(i)
  G <-readBerkeley(filename=SS[i])
  print(ncol(G))
  
  print(SS[i])
  
}


SS <- list.files(path=getwd(),recursive = TRUE,full.names=T, pattern="site_complete_detail.txt")

for(i in 1:length(SS)){
  print(i)
  G <-readBerkeley(filename=SS[i])
  print(max(G$Latitude))
  print(min(G$Latitude))
  print(max(G$Longitude))
  print(min(G$Longitude))
  print(max(G$Elevation))
  print(min(G$Elevation))
  print(max(G$Latitude,na.rm=T))
  print(min(G$Latitude,na.rm=T))
  print(max(G$Longitude,na.rm=T))
  print(min(G$Longitude,na.rm=T))
  print(max(G$Elevation,na.rm=T))
  print(min(G$Elevation,na.rm=T))
  print(SS[i])
  
}


SS <- list.files(path=getwd(),recursive = TRUE,full.names=T, pattern="sources.txt")
for(i in 1:length(SS)){
  print(i)
  G <-readBerkeley(filename=SS[i])
  print(ncol(G))
  
  print(SS[i])
  
}

X <- X %>% map_df( all(is.na(x)))


d <-tbl_df(data.frame(a=1:10,b=10:19))

d<- d %>% mutate( b=na_if(b,15) )
