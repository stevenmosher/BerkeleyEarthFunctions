source("Initialize.R")


getExtent <- function(lon,lat){
  
  LatDistance <- 111
  dTr <- pi/180
  Buffer <- 20
  LatBuffer <- Buffer/LatDistance 
  LonDistance <- LatDistance* cos(lat*dTr)
  Mult        <- LatDistance/LonDistance
  LonBuffer   <- LatBuffer*Mult
  
  E10         <-extent(lon-LonBuffer,lon+LonBuffer,lat-LatBuffer,lat+LatBuffer)
  return(E10)
                        
}

extentCrop <- function(e1,ecrop){
  
  #  -180,-170, 30,40
  #  -175,-171, 31,32
  lonmin <-ifelse(ecrop[1]> e1[1],ecrop[1],e1[1])
  lonmax <-ifelse(ecrop[2]< e1[2],ecrop[2],e1[2])
  latmin <-ifelse(ecrop[3]> e1[3],ecrop[3],e1[3])
  latmax <-ifelse(ecrop[4]< e1[4],ecrop[4],e1[4])
  
  ec <- extent(lonmin,lonmax,latmin,latmax)
  ec = cbind(c(ec[1],ec[2]),c(ec[3],ec[4]))
  ec = SpatialPoints(ec)
  proj4string(ec)<-WGS
  return(ec)
}

urbanCount <- function(File,extent2,lon,lat) {
  
  WGS <-  "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  extent1 <- extent(File$LonMin,File$LonMax,File$LatMin,File$LatMax)
  A1 <- raster(File$Files)
  e1 <- extentCrop(e1=extent1,ecrop=extent2)
  e1t <- spTransform(e1,CRSobj=projection(A1))
  A1  <- crop(A1,e1t)
  AF  <-tbl_df(raster::extract(A1,y=1:ncell(A1), df=T))
  colnames(AF)[2]<-"Value"
  AFxy <-xyFromCell(A1,AF$ID)
  AFxy = cbind(AFxy[,1],AFxy[,2])
  AFxy = SpatialPoints(AFxy)
  proj4string(AFxy)<-projection(A1)
  AFxy <- spTransform(AFxy,CRSobj=WGS)
  AF <- AF %>% mutate(Longitude=coordinates(AFxy)[,1] ,Latitude=coordinates(AFxy)[,2] ) %>%
        filter(Value == 80)
  if(nrow(AF)>0){
    AF <- AF %>% mutate(CenterLon=lon,CenterLat=lat) %>% mutate(Distance= pointDistance(p1=cbind(CenterLon,CenterLat),
                                                                              p2= cbind(Longitude,Latitude),
                                                                              lonlat=TRUE)) %>%
                filter(Distance <= 20000) %>% mutate(Distance = round(Distance/1000,0))
    
    
    
  }
  
  return(AF)
  
  
}

WGS <-  "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"



geonames <- "http://download.geonames.org/export/dump/cities1000.zip"
geomshap <- "http://download.geonames.org/export/dump/shapes_simplified_low.json.zip"
nearth   <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_urban_areas.zip"
cpoint   <- "G:\\BerkeleyEarthFunctions\\Data\\cities1000.txt"
fn4 <-"G:\\BerkeleyEarthFunctions\\Data\\gl_grumpv1_ppoints_csv\\gl_grumpv1_ppoints_csv\\glpv1.csv"
fn2 <-"G:\\BerkeleyEarthFunctions\\Downloads\\Station\\Monthly\\Breakpoint\\TAVG\\Jul_2016\\site_summary.txt"
fn3 <-"G:\\BerkeleyEarthFunctions\\Downloads\\Station\\Monthly\\Breakpoint\\TAVG\\Jul_2016\\site_detail.txt"
crn <-  "G:\\BerkeleyEarthFunctions\\Data\\stations.tsv"
esa <-"G:\\BerkeleyEarthFunctions\\Data\\ESACCI-LC-L4-LCCS-Map-300m-P5Y-2010-v1.6.1.nc"
etif <-"G:\\BerkeleyEarthFunctions\\Data\\ESACCI-LC-L4-LCCS-Map-300m-P5Y-2010-v1.6.1.tif"
ecity <-"G:\\BerkeleyEarthFunctions\\Data\\ne_10m_populated_places\\ne_10m_populated_places.dbf"
lscn  <-"G:\\BerkeleyEarthFunctions\\Data\\ne_10m_urban_areas_landscan\\ne_10m_urban_areas_landscan.dbf"

lc <-"G:\\BerkeleyEarthFunctions\\Data\\ESACCI-LC-Legend.csv"

ud <-  "G:\\BerkeleyEarthFunctions\\UrbanFileMetaData.csv"
p  <- "G:\\BerkeleyEarthFunctions\\Data\\gpw-v4-population-count-2015\\gpw-v4-population-count_2015.tif"
ushcn <- "G:\\BerkeleyEarthFunctions\\Data\\fall_etal_2011_SI\\FinalList.csv"

US <- readBerkeley(fn3)

US <- US %>% select(Id,Name,Latitude,Longitude,Country) %>% rename(Bid=Id)%>%mutate(Id=1:nrow(US))


  



 

Ufiles <- tbl_df(read.csv(ud,stringsAsFactors = F))

 


 
count <- 0
DF<-NULL

for( i in 1:nrow(US)) {
  
  AF <- NULL
  AF2 <- NULL
  AF3 <- NULL
  AF4 <- NULL
  lat <- US$Latitude[i]
  lon <- US$Longitude[i]
  e <-getExtent(lon=lon,lat=lat)
  
  
  c1 <- which(Ufiles$LatMin <= lat & Ufiles$LatMax >= lat & Ufiles$LonMin <= lon & Ufiles$LonMax >= lon)
  t1 <- which(Ufiles$LatMin <= e[3] & Ufiles$LatMax >= e[3] & Ufiles$LonMin <= e[1] & Ufiles$LonMax >= e[1])
  t2 <- which(Ufiles$LatMin <= e[3] & Ufiles$LatMax >= e[3] & Ufiles$LonMin <= e[2] & Ufiles$LonMax >= e[2])
  t3 <- which(Ufiles$LatMin <= e[4] & Ufiles$LatMax >= e[4] & Ufiles$LonMin <= e[1] & Ufiles$LonMax >= e[1])
  t4 <- which(Ufiles$LatMin <= e[4] & Ufiles$LatMax >= e[4] & Ufiles$LonMin <= e[2] & Ufiles$LonMax >= e[2])
  
  Tiles <- unique(c(t1,t2,t3,t4))
 
  if(length(Tiles)==1){
    coords = cbind(c(e[1],e[2]),c(e[3],e[4]))
    e = SpatialPoints(coords)
    proj4string(e)<-WGS
    
     CenterTile <- raster(Ufiles$Files[Tiles])
     transformed <- spTransform(e,CRSobj=projection(CenterTile))
     A1  <- crop(CenterTile,transformed)
      
     AF  <-tbl_df(raster::extract(A1,y=1:ncell(A1), df=T))
    
     colnames(AF)[2]<-"Value"
     AFxy <-xyFromCell(A1,AF$ID)
     AFxy = cbind(AFxy[,1],AFxy[,2])
     AFxy = SpatialPoints(AFxy)
     proj4string(AFxy)<-projection(A1)
     AFxy <- spTransform(AFxy,CRSobj=WGS)
     AF <- AF %>% mutate(Longitude=coordinates(AFxy)[,1] ,Latitude=coordinates(AFxy)[,2] ) %>%
       filter(Value == 80)
     if(nrow(AF)>0){
       AF <- AF %>% mutate(CenterLon=lon,CenterLat=lat) %>% mutate(Distance= pointDistance(p1=cbind(CenterLon,CenterLat),
                                                                                           p2= cbind(Longitude,Latitude),
                                                                                           lonlat=TRUE)) %>%
         filter(Distance <= 20000) %>% mutate(Distance = round(Distance/1000,0))
     }
    if(nrow(AF)==0)AF<-NULL 
  }
  if(length(Tiles)==2 | length(Tiles)==4){
    print(i)
    AF <- urbanCount(File=Ufiles[Tiles[1],],extent2=e,lon=lon,lat=lat)
    if(nrow(AF)==0)AF<-NULL 
    AF2 <-urbanCount(File=Ufiles[Tiles[2],],extent2=e,lon=lon,lat=lat)
    if(nrow(AF2)==0)AF2<-NULL 
    if(length(Tiles)==4){
      AF3 <- urbanCount(File=Ufiles[Tiles[3],],extent2=e,lon=lon,lat=lat)
      if(nrow(AF3)==0)AF3<-NULL 
      AF4 <-urbanCount(File=Ufiles[Tiles[4],],extent2=e,lon=lon,lat=lat)
      if(nrow(AF4)==0)AF4<-NULL 
      
    }
  }
  
  AF <- bind_rows(AF,AF2,AF3,AF4)
  
  if(nrow(AF)>0) AF <- AF %>% select(ID,Value,Longitude,Latitude,Distance) %>% 
    group_by(Distance) %>% summarize(Count = n()) %>% mutate(ID=i)
  
  
  if(count == 0){
    
    if(nrow(AF)>0){
      DF<- AF
      DF$ID <- i
      count <- 1
    }
    
  }else
    {
    if(nrow(AF)>0){
      AF$ID <- i
      DF<- bind_rows(DF,AF)
      
    }
    
    
  }
   
  
  
 print(i)
 print(Tiles)
 print(nrow(DF))
     
}

 S <- DF %>% group_by(ID,Distance)%>%summarize(Area = (30^2*Count)/1000000,
                                               ScaleLength= Area^.5 ) %>% rename(Id=ID)
 
 S <- left_join(S,US, by =  "Id")
                                               
  
 write.csv(S, "Berk30mDistance.csv")
 
 S2 <- S  %>% group_by(Id) %>% summarize(Area=sum(Area),
                                         ScaleLength= Area^.5)
 
 S2 <- left_join(S2,US,by="Id")
 
 write.csv(S2, "Berk30mSummary.csv")
 
 DFin <- read.csv("Berkurban300.csv",stringsAsFactors = F)
 
 S2 <- S2 %>% select(Id,Area,ScaleLength) %>%
     rename(Area30m = Area, ScaleLength30m= ScaleLength) 
 
 DFin <- left_join(DFin,S2, by="Id")
 
 DFin <- DFin %>% mutate(Area30m = ifelse(is.na(Area30m),0,Area30m),
                         ScaleLength30m = ifelse(is.na(ScaleLength30m),0,ScaleLength30m))
 

 ggplot(DFin, aes(x=Area30m,y=PopCount))+geom_point()+stat_smooth(method="lm") 
 
 write.csv(DFin, "BerkPopAndArea.csv")
 
 
 ggplot(DFin, aes(x=Area30m,y=Area))+geom_point()+stat_smooth(method="lm") 
 
 ggplot(DFin, aes(x=Area30m,y=Tmin.trend))+geom_point()+stat_smooth(method="lm")
 ggplot(DFin, aes(x=Area30m,y=Tmax.trend))+geom_point()+stat_smooth(method="lm")
 ggplot(DFin, aes(x=Area30m,y=Tmean.trend))+geom_point()+stat_smooth(method="lm")
 
 ggplot(DFin, aes(x=PopCount,y=Tmin.trend))+geom_point()+stat_smooth(method="lm")
 ggplot(DFin, aes(x=PopCount,y=Tmax.trend))+geom_point()+stat_smooth(method="lm")
 ggplot(DFin, aes(x=PopCount,y=Tmean.trend))+geom_point()+stat_smooth(method="lm")
 
 
 
 
 S <- DF %>% group_by(ID) %>%summarize(Total = sum(Count))
 S2 <- DF %>% filter(Distance <2) %>% group_by(ID) %>%summarize(Total = sum(Count))
 S2 <- S2 %>% mutate(Area = (Total*900)/1000000, ScaleLength = Area^.5)
 