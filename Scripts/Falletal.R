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
hyde <- "F:\\HydeALL\\Density.grd"

lc <-"G:\\BerkeleyEarthFunctions\\Data\\ESACCI-LC-Legend.csv"

ushcn <- "G:\\BerkeleyEarthFunctions\\Data\\fall_etal_2011_SI\\FinalList.csv"

US <- tbl_df(read.csv(ushcn,stringsAsFactors=F))
US <- US %>% mutate(Id=1:nrow(US))

Density <- brick(hyde)

 

Hydelayer2005=which(getZ(Density)==2005)
 

Density2005 <- raster(Density, Hydelayer2005)

PD <- raster::extract(Density2005, y=cbind(US$Lon ,US$Lat))

US[,"PopD"]<-PD
rm(PD)
US <- US %>% select(Id,Lon,Lat,CRN,Station,Environs,Tmean.trend,Tmax.trend,Tmin.trend,PopD)  



ggplot(US,aes(PopD))+geom_histogram()+facet_wrap(~ Environs, scale="free_y")  

ggplot(US,aes(PopD))+geom_histogram()+facet_wrap(~ CRN, scale="free_y")


X<-raster(etif)

U <- raster::extract(X, y=cbind(US$Lon ,US$Lat))

US[, "landclass"]<-U
Index <- US[,c("Id","Lon","Lat","CRN")]
 
for( i in 1:nrow(US)){
  U  <-  raster::extract(X, y=cbind(US$Lon[i],US$Lat[i]),buffer=20000 , cellnumbers=TRUE,df=TRUE)
  U  <-  tbl_df(U )
  colnames(U)[3]<-"LandClass"
  U[,"Count"]<-nrow(U)
  xy <- xyFromCell(X,U$cells)
  Dist <- spDistsN1(pts=xy,pt=c(US$Lon[i],US$Lat[i]),longlat = TRUE)
  U  <- U  %>% mutate(Distance=Dist)  %>% filter(LandClass==190)
  if(nrow(U)>0)U$ID<-i
  if(i==1){
    if(nrow(U)>0)UF <-U
    
  }
  if(i != 1){
    if(nrow(U)>0)UF <-bind_rows(UF,U)
  }
  print(i)
}

UF <- UF %>% mutate(LandClass= as.factor(LandClass)) %>% rename(Id=ID)

UF <- left_join(UF,Index, by ="Id")
UF <- UF %>% mutate(CellArea = (pi*20000^2)/Count)

S <- UF %>%  mutate(Rbin = Distance %/% 2) %>% group_by(Id,Rbin) %>%
     summarize(Count = n(),
               BinArea =sum(CellArea))

CX <- US[,c("Id","CRN","Tmean.trend","Tmax.trend","Tmin.trend")]

CX <- left_join(CX,S, by = "Id")
                
                

ggplot(UF, aes(Distance))+geom_histogram()+facet_wrap(~CRN, scales="free_y")
 

 

 


U200 <- left_join(U200,Index, by="Id")
U200 <- U200 %>% mutate(Distance= pointDistance(cbind(Lon,Lat),cbind(CellLon,CellLat),lonlat=TRUE)/1000)
U200 <- U200 %>% mutate(LandClass =as.factor(LandClass), Lon=NULL,Lat=NULL,CellLon=NULL,CellLat=NULL)
U200 <- U200 %>% filter(LandClass==190)

U400 <-  raster::extract(X, y=cbind(US$Lon[201:400],US$Lat[201:400]),buffer=20000   , cellnumbers=TRUE,df=TRUE)
U400 <-  tbl_df(U400)   
colnames(U400)[3]<-"LandClass"
U400$ID <- U400$ID+200
U4 <- U400 %>% group_by(ID) %>%summarise(Count = n())
U400 <- U400 %>% filter(LandClass==190)
U600 <-  raster::extract(X, y=cbind(US$Lon[401:600],US$Lat[401:600]),buffer=20000   , cellnumbers=TRUE,df=TRUE)
U600 <-  tbl_df(U600)   
colnames(U600)[3]<-"LandClass"
U600$ID <- U600$ID+200
U6   <- U600 %>% group_by(ID) %>%summarise(Count = n())
U600 <- U600 %>% filter(LandClass==190)
U800 <-  raster::extract(X, y=cbind(US$Lon[601:800],US$Lat[601:800]),buffer=20000   , cellnumbers=TRUE,df=TRUE)
U800 <-  tbl_df(U800)   
colnames(U800)[3]<-"LandClass"
U800$ID <- U800$ID+200
U8 <- U800 %>% group_by(ID) %>% summarise(Count = n())
U800 <- U800 %>%  filter(LandClass==190)
   
U1000 <-  raster::extract(X, y=cbind(US$Lon[801:1000],US$Lat[801:1000]),buffer=20000   , cellnumbers=TRUE,df=TRUE)
U1000 <-  tbl_df(U1000)   
colnames(U1000)[3]<-"LandClass"
U1000$ID <- U1000$ID+200
U10 <- U1000 %>% group_by(ID) %>%summarise(Count = n())
U1000 <- U1000 %>% filter(LandClass==190)

U1200 <-  raster::extract(X, y=cbind(US$Lon[1001:1221],US$Lat[1001:1221]),buffer=20000   , cellnumbers=TRUE,df=TRUE)
U1200 <-  tbl_df(U1200)   
colnames(U1200)[3]<-"LandClass"
U1200$ID <- U1200$ID+200
U12 <- U1200 %>% group_by(ID) %>%summarise(Count = n())
U1200 <- U1200 %>%  filter(LandClass==190)

U200 <- bind_rows(U200,U400,U600,U800,U1000,U1200)
U400 <-NULL;U600<-NULL;U800<-NULL;U1000<-NULL;U1200<-NULL
UT <- bind_rows(U2 ,U4 ,U6 ,U8 ,U10 ,U12 )
U2  <-NULL;U4  <-NULL;U6 <-NULL;U8 <-NULL;U10 <-NULL;U12 <-NULL

U200 <- left_join(U200,UT, by= "ID")

xy <- xyFromCell(X,U200$cells)

U200 <- U200 %>% mutate(CellLon=xy[,"x"], CellLat= xy[,"y"]) %>% rename(Id=ID)

U200 <- left_join(U200,Index, by="Id")

U200 <- U200 %>% mutate(Distance= pointDistance(cbind(Lon,Lat),cbind(CellLon,CellLat),lonlat=TRUE)/1000)

U200 <-U200 %>% mutate(LandClass =as.factor(LandClass), Lon=NULL,Lat=NULL,CellLon=NULL,CellLat=NULL)


 
S <- group_by(LC3, Id) %>% summarize(Total=n()) 
S2 <- group_by(LC3,Id,LandClass) %>% summarize(Count = n()) 
S2 <- left_join(S2,S, by= "Id")
S2 <-S2 %>% mutate(Area = (Count/Total)*pi*20000^2) %>% mutate(Area=round(Area/1000000,2))


Urban <- LC3 %>% filter(LandClass=='190')

Urban <- left_join(Urban,Index, by= "Id")


ggplot(U200,aes(Distance))+geom_histogram()+facet_grid(~NETWORK,scales="free_y")


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