source("Initialize.R")

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
 

CRNmeta <- tbl_df(read.delim(crn,stringsAsFactors=FALSE))
CRNmeta$WBAN[is.na(CRNmeta$WBAN)]<-"MISSING"

Index <- CRNmeta %>% select(WBAN,LONGITUDE,LATITUDE,NETWORK) %>% mutate(Id=1:nrow(CRNmeta))

 



P <- P %>% mutate(CellLon=xy[,"x"], CellLat= xy[,"y"])


P <- P %>% mutate(Distance= pointDistance(cbind(LONGITUDE,LATITUDE),cbind(CellLon,CellLat),lonlat=TRUE)/1000)

 
 


Lcode <- read.csv(lc,sep=";")





X<-raster(etif)

 

LC3 <- raster::extract(X, y=cbind(CRNmeta$LONGITUDE,CRNmeta$LATITUDE),buffer=20000   , cellnumbers=TRUE,df=TRUE)

LC3 <- tbl_df(LC3)

colnames(LC3)[3]<-"LandClass"


xy <- xyFromCell(X,LC3$cells)

LC3 <- LC3 %>% mutate(CellLon=xy[,"x"], CellLat= xy[,"y"]) %>% rename(Id=ID)

LC3 <- left_join(LC3,Index, by="Id")

LC3 <- LC3 %>% mutate(Distance= pointDistance(cbind(LONGITUDE,LATITUDE),cbind(CellLon,CellLat),lonlat=TRUE)/1000)

LC3 <-LC3 %>% mutate(LandClass =as.factor(LandClass), WBAN=NULL,LONGITUDE=NULL,LATITUDE=NULL,CellLon=NULL,CellLat=NULL)

S <- group_by(LC3, Id) %>% summarize(Total=n()) 
S2 <- group_by(LC3,Id,LandClass) %>% summarize(Count = n()) 
S2 <- left_join(S2,S, by= "Id")
S2 <-S2 %>% mutate(Area = (Count/Total)*pi*20000^2) %>% mutate(Area=round(Area/1000000,2))


Urban <- LC3 %>% filter(LandClass=='190')

Urban <- left_join(Urban,Index, by= "Id")


ggplot(Urban,aes(Distance))+geom_histogram()+facet_grid(~NETWORK,scales="free_y")


ggplot(filter(Urban, Id %in% 80:100),aes(Distance))+geom_histogram()+facet_wrap(~Id,scales="free_y")



W04 <-   cbind(−1.92,52.37)

db<- cbind(5.173,52.088)


BUCL <- raster::extract(X, y=W04 ,buffer=20000   , cellnumbers=TRUE,df=TRUE)

BUCL <- tbl_df(BUCL)

colnames(BUCL)[3]<-"LandClass"

xy <- xyFromCell(X,BUCL$cells)

BUCL <- BUCL %>% mutate(CellLon=xy[,"x"], CellLat= xy[,"y"]) %>% mutate(LONGITUDE=W04[1],LATITUDE=W04[2])


BUCL <- BUCL %>% mutate(Distance= pointDistance(cbind(LONGITUDE,LATITUDE),cbind(CellLon,CellLat),lonlat=TRUE)/1000) %>%
                mutate(LandClass =as.factor(LandClass))

ggplot(BUCL, aes(LandClass)) +geom_bar()

ggplot(filter(BUCL, LandClass==190),aes(Distance))+geom_histogram() 


Debilt <- raster::extract(X, y=db ,buffer=20000   , cellnumbers=TRUE,df=TRUE)

Debilt <- tbl_df(Debilt)

colnames(Debilt)[3]<-"LandClass"

xy <- xyFromCell(X,Debilt$cells)

Debilt <- Debilt %>% mutate(CellLon=xy[,"x"], CellLat= xy[,"y"]) %>% mutate(LONGITUDE=db[1],LATITUDE=db[2])


Debilt <- Debilt %>% mutate(Distance= pointDistance(cbind(LONGITUDE,LATITUDE),cbind(CellLon,CellLat),lonlat=TRUE)/1000) %>%
  mutate(LandClass =as.factor(LandClass))

ggplot(Debilt, aes(LandClass)) +geom_bar()

ggplot(filter(Debilt, LandClass==190),aes(Distance))+geom_histogram() 


ggplot(filter(Debilt, LandClass==190),aes(Distance))+stat_ecdf()

ggplot(filter(BUCL, LandClass==190),aes(Distance))+stat_ecdf()

 
