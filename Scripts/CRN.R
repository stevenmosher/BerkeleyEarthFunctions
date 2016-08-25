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


hyde <- "F:\\HydeALL\\Density.grd"

download.file(geonames, destfile = file.path("Data",basename(geonames)),mode="wb")
#download.file(geonames, destfile = file.path("Data",basename(geomshap)),mode="wb")
download.file(geonames, destfile = file.path("Data",basename(nearth)),mode="wb")

unzip(file.path("Data",basename(geonames)),exdir="Data")
unzip(file.path("Data",basename(nearth)),exdir="Data") 

 

# PPPL, PPPLF

G <- tbl_df(read.table(cpoint, header=F, sep= "\t",quote="",stringsAsFactors = F)) %>%
     select(V1,V3,V5,V6,V8,V15,V17,V19) %>% filter(V15 !=0) %>% 
      rename(Id=V1,Name=V3,Latitude=V5,Longitude=V6,Code=V8,Pop=V15,Elv=V17,Date=V19) 
 
#GrumpCity <- read.csv(fn4)
#Locations <- readBerkeley(filename=fn2)

CRNmeta <- tbl_df(read.delim(crn,stringsAsFactors=FALSE))
CRNmeta$WBAN[is.na(CRNmeta$WBAN)]<-"MISSING"

Index <- CRNmeta %>% select(WBAN,LONGITUDE,LATITUDE) %>% mutate(Id=1:nrow(CRNmeta))

Density <- brick(hyde)

Hydelayer1850=which(getZ(Density)==1850)

Hydelayer2005=which(getZ(Density)==2005)

Dates  <- getZ(Density)
 

Pop <- raster::extract(Density, y=cbind(CRNmeta$LONGITUDE,CRNmeta$LATITUDE),layer=1, nl =nlayers(Density))
P  <- tbl_df(raster::extract(raster(Density,layer=Hydelayer2005), y=cbind(CRNmeta$LONGITUDE,CRNmeta$LATITUDE),buffer=20000, cellnumbers=TRUE,df=TRUE)) %>%
      rename(Id=ID) %>% rename(PopulationDensity2005= X2005)

P <- left_join(P,Index, by="Id")

xy <- xyFromCell(raster(Density,layer=Hydelayer2005),P$cells)

P <- P %>% mutate(CellLon=xy[,"x"], CellLat= xy[,"y"])

 
P <- P %>% mutate(Distance= pointDistance(cbind(LONGITUDE,LATITUDE),cbind(CellLon,CellLat),lonlat=TRUE)/1000)


Summary <- group_by(P,Id) %>% summarize(cellCount= n(),
                                          Min      = round(min(PopulationDensity2005,na.rm=T),2),
                                          Max      = round(max(PopulationDensity2005,na.rm=T),2),
                                          Mean     = round(mean(PopulationDensity2005,na.rm=T),2),
                                          Station  = round(PopulationDensity2005[which(Distance==min(Distance))],2))

Pop <- as.data.frame(Pop)

colnames(Pop)<- c(seq(from=1700, to = 2000, by=10),2005)

Pop[,"WBAN"]<- CRNmeta$WBAN

CRNmeta <- CRNmeta %>% mutate(Pop2005 = Pop[ ,"2005"])


Pop <- Pop %>% gather(Year,Population, 1:32) %>% tbl_df %>% filter(Year >= 1850)

P2 <-  raster::extract(Density, y=cbind(CRNmeta$LONGITUDE,CRNmeta$LATITUDE),
                          layer= 1 , nl =32 ,cellnumbers=TRUE, buffer=20000,df=TRUE)

P2 <- P2 %>% tbl_df() %>% gather(Year,Population, starts_with("X")) %>% 
      mutate(Year = as.numeric(str_replace(Year,"X",""))) %>%filter(Year > 1849)
                           

###  Save population

ggplot(Pop[Pop$WBAN %in% W[1:240],], aes(x=Year, y=Population,group=WBAN)) + geom_line()


Lcode <- read.csv(lc,sep=";")




 
X<-raster(etif)

LC2 <- raster::extract(X, y=cbind(CRNmeta$LONGITUDE,CRNmeta$LATITUDE)   , cellnumbers=TRUE,df=TRUE)

LC3 <- raster::extract(X, y=cbind(CRNmeta$LONGITUDE,CRNmeta$LATITUDE),buffer=20000   , cellnumbers=TRUE,df=TRUE)

LC3 <- tbl_df(LC3)

colnames(LC3)[3]<-"LandClass"

LC3 <- LC3 %>% filter(LandClass==190)


group_by(LC3, ID) %>% summarize(j = n()) ->S

