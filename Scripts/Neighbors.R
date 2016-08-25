
source("Initialize.R")

milesTokm <- 2.58999

X <- c(-Inf,25,150,500,2500,10000,Inf) * milesTokm
L <- c("Natural","Rural","ExUrban","Sprawl","DenseSuburb","UrbanCore")

hyde <- "F:\\HydeALL\\Density.grd"
fn <-"G:\\BerkeleyEarthFunctions\\Downloads\\Station\\Monthly\\Breakpoint\\TAVG\\Jul_2016\\data_characterization.txt"
fn2 <-"G:\\BerkeleyEarthFunctions\\Downloads\\Station\\Monthly\\Breakpoint\\TAVG\\Jul_2016\\site_summary.txt"

Stations <-readBerkeley(filename=fn)
Locations <- readBerkeley(filename=fn2)
Locations <- left_join(Locations,Stations, by = "Id")
Birmingham <- extent(-2.2,-1.5,52.3,52.7)
Paradise <- c( −1.90,52.48)
W04 <-   c(−1.92,52.37)	
W07 <- c(−1.90,52.49)
DF <- data.frame(x=c(Paradise[1],W04[1],W07[1]),y=c(Paradise[2],W04[2],W07[2]))
 

Density <- brick(hyde)

Dates  <- getZ(Density)
Hydelayer1800=which(getZ(Density)==1800)
Hydelayer1850=which(getZ(Density)==1850)
Hydelayer1900=which(getZ(Density)==1900)
Hydelayer1950=which(getZ(Density)==1950)
Hydelayer2000=which(getZ(Density)==2000)
Hydelayer2005=which(getZ(Density)==2005)

Density2005 <- raster(Density, Hydelayer2005)
RPA         <- cut(Density2005,breaks=X)

B <- crop(Density2005, Birmingham) 


POP_All <- raster::extract(x=Density2005, y = cbind(Locations$Longitude,Locations$Latitude))
Zone_Max <- raster::extract(x=Density2005, y = cbind(Locations$Longitude,Locations$Latitude),
                           buffer=20000, fun=max)
Region_Max <- raster::extract(x=Density2005, y = cbind(Locations$Longitude,Locations$Latitude),
                           buffer=40000, fun=max)
rpaClass <- raster::extract(x=RPA, y = cbind(Locations$Longitude,Locations$Latitude))


Locations <- Locations %>% mutate(Pop2005 = POP_All) %>% 
                           mutate(Active= ifelse(LateYear >2004,TRUE,FALSE)) %>%
                           mutate(NeighborPOP=Zone_Max) %>%
                           mutate(RegionPOP=Region_Max) %>%
                           mutate(LogPop = ifelse(Pop2005< 1,0,log10(Pop2005))) %>% 
                           mutate(Rpa =  rpaClass) %>% 
                           filter(!is.na(Pop2005)) %>% 
                           mutate(LogPopNeighbor = ifelse(NeighborPOP< 1,0,log10(NeighborPOP)))%>%
                           mutate(LogRegion = ifelse(RegionPOP< 1,0,log10(RegionPOP))) 

Locations <- Locations %>% mutate(NoHighPOPNeighbor =ifelse(NeighborPOP<=5000 ,TRUE,FALSE)) %>%
                           mutate(Homogeneous =ifelse(NeighborPOP<=1000 ,TRUE,FALSE))

Locations[,"RpaClass"]<-L[Locations$Rpa]


Bplot <-gplot(B)+geom_tile(aes(fill=value))+coord_equal()

Bplot + geom_point(data=DF,aes(x=x,y=y,color="red"))+ggtitle("Birmingham")


ggplot(data=Locations, aes(Rpa, fill=Active)) +geom_histogram(position="dodge")+ggtitle("Active Versus Non Active")

ggplot(data=Locations, aes(LogPop, fill=NoHighPOPNeighbor)) +geom_histogram(position="dodge")+ggtitle("Isolated/Not Isolated")

ggplot(data=Locations, aes(LogPop, fill=Homogeneous)) +geom_histogram(position="dodge")+ggtitle("Isolated/Not Isolated")

ggplot(data=Locations, aes(LogRegion, fill=Active)) +geom_histogram(position="dodge")+ggtitle("Isolated/Not Isolated")


ggplot(filter(Locations,Active==T & NoHighPOPNeighbor), aes(LogPop)) +geom_histogram()+ggtitle("Isolated Active")

ggplot(filter(Locations,Active==T & NoHighPOPNeighbor), aes(LogPop)) + stat_ecdf(geom = "step")+ggtitle("Isolated Active")


group_by(Locations, Active) %>% summarize(TotalNoHIGHpop=sum(NoHighPOPNeighbor),
                                          AvePOP = mean(Pop2005))

summarize(Locations, A = sum(Active))