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
crn <-  "G:\\BerkeleyEarthFunctions\\Data\\stations.tsv"
esa <-"G:\\BerkeleyEarthFunctions\\Data\\ESACCI-LC-L4-LCCS-Map-300m-P5Y-2010-v1.6.1.nc"
etif <-"G:\\BerkeleyEarthFunctions\\Data\\ESACCI-LC-L4-LCCS-Map-300m-P5Y-2010-v1.6.1.tif"
ecity <-"G:\\BerkeleyEarthFunctions\\Data\\ne_10m_populated_places\\ne_10m_populated_places.dbf"
lscn  <-"G:\\BerkeleyEarthFunctions\\Data\\ne_10m_urban_areas_landscan\\ne_10m_urban_areas_landscan.dbf"

lc <-"G:\\BerkeleyEarthFunctions\\Data\\ESACCI-LC-Legend.csv"

ud <-  "G:\\BerkeleyEarthFunctions\\UrbanFileMetaData.csv"
p  <- "G:\\BerkeleyEarthFunctions\\Data\\gpw-v4-population-count-2015\\gpw-v4-population-count_2015.tif"

db<- cbind(5.173,52.088)
lon=db[1]
lat=db[2]

Pop <- raster(p)
raster::extract(Pop,db,buffer=20000, fun =sum)
CRNmeta <- tbl_df(read.delim(crn,stringsAsFactors=FALSE))
CRNmeta$WBAN[is.na(CRNmeta$WBAN)]<-"MISSING"

Index <- CRNmeta %>% select(WBAN,LONGITUDE,LATITUDE,NETWORK,OPERATION,STATUS) 

 

CRN  <- Index %>% filter(NETWORK=="USCRN")%>% filter(OPERATION=="Operational" & STATUS=="Commissioned")%>% 
  mutate(Id=1:nrow(CRN)) 

E <- raster(etif)

 

count <- 0
DF<-NULL

for( i in 1:nrow(CRN)) {
  
   
  lat <- CRN$LATITUDE[i]
  lon <- CRN$LONGITUDE[i]
  
  e <-getExtent(lon=lon,lat=lat)
  AF <- tbl_df(raster::extract(E, y = cbind(lon,lat),buffer= 20000, df=TRUE))
  colnames(AF)[2]<-"LandClass"
  Total <- nrow(AF)
  AF <- AF %>% filter(LandClass == 190) %>% summarize(Count=n(),
                                                      Area=(Count*300^2)/1000000,
                                                      ScaleLength = Area^.5,
                                                      Total=Total) %>% mutate(ID=i) %>%mutate(CRNid=CRN$Id[i])
  
  Pcount <- raster::extract(Pop,cbind(lon,lat),buffer=20000, fun=sum)
  
  AF <- AF %>% mutate(PopCount = Pcount)
  
  if(count==0){
    DF<-AF
    count <-1
  }else
  {
    DF <- bind_rows(DF,AF)
  }
  
  print(i) 
  
}

DF <- DF %>% rename(Id=ID) 
DF <- left_join(DF,CRN, by = "Id")
write.csv(DF, "CRNurban300.csv")
S <- DF %>% group_by(ID) %>%summarize(Total = sum(Count))
S2 <- DF %>% filter(Distance <2) %>% group_by(ID) %>%summarize(Total = sum(Count))
S2 <- S2 %>% mutate(Area = (Total*900)/1000000, ScaleLength = Area^.5)
