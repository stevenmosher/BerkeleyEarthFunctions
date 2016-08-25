source("Initialize.R")

milesTokm <- 2.58999

X <- c(-Inf,25,150,500,2500,5000,Inf) * milesTokm
L <- c("Natural","Rural","ExUrban","Sprawl","DenseSuburb","UrbanCore")

D <- c(-Inf,1850,1880,1910,1940,1970,2000,2016)
DL <-c("Pre","1850-1880","1880-1910","1910-1940","1940-1970","1970-2000","2000-2016")
 

hyde <- "F:\\HydeALL\\Density.grd"
fn <-"G:\\BerkeleyEarthFunctions\\Downloads\\Station\\Monthly\\Breakpoint\\TAVG\\Jul_2016\\data_characterization.txt"
fn2 <-"G:\\BerkeleyEarthFunctions\\Downloads\\Station\\Monthly\\Breakpoint\\TAVG\\Jul_2016\\site_summary.txt"
fn3 <-"G:\\BerkeleyEarthFunctions\\Downloads\\Station\\Monthly\\Breakpoint\\TAVG\\Jul_2016\\data.txt"
fn4 <-"G:\\BerkeleyEarthFunctions\\Data\\gl_grumpv1_ppoints_csv\\gl_grumpv1_ppoints_csv\\glpv1.csv"

Data <-readBerkeley(filename=fn3)

Data <- Data %>% select(Id, Year) %>% distinct(Id,Year)
Data <- Data %>% mutate(Decade= Year %/% 10)
Data <- Data %>% mutate(Period= cut(x=Year,breaks=D,labels=DL))
Data <- Data %>% distinct(Id,Decade,Period)
 

ggplot(data=distinct(Data,Id,Period), aes(Period)) +geom_bar(position="dodge")+ggtitle("Stations Per Period")




Stations <-readBerkeley(filename=fn)
Locations <- readBerkeley(filename=fn2)
Locations <- left_join(Locations,Stations, by = "Id")
Locations <- Locations %>% select(Id,Latitude,Longitude,EarlyYear,EarlyMonth,LateYear,LateMonth)

mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

#Now Layer the cities on top
 
Period  <- Locations %>% filter(Id %in% Data$Id[Data$Period=="1850-1880"])

mp1880 <- mp+ geom_point(aes(x=Period$Longitude, 
                          y= Period$Latitude) ,
                          color="blue", size=1)
mp1880+ggtitle("1850-1880")
 
Period  <- Locations %>% filter(Id %in% Data$Id[Data$Period=="1880-1910"])

mp1910 <- mp+ geom_point(aes(x=Period$Longitude, 
                             y= Period$Latitude) ,
                         color="blue", size=1)
mp1910 +ggtitle("1880-1910")

Period  <- Locations %>% filter(Id %in% Data$Id[Data$Period=="1910-1940"])

mp1940 <- mp+ geom_point(aes(x=Period$Longitude, 
                             y= Period$Latitude) ,
                         color="blue", size=1)
mp1940+ggtitle("1910-1940")

Period  <- Locations %>% filter(Id %in% Data$Id[Data$Period=="1940-1970"])

mp1970 <- mp+ geom_point(aes(x=Period$Longitude, 
                             y= Period$Latitude) ,
                         color="blue", size=1)
mp1970+ggtitle("1940-1970")

Period  <- Locations %>% filter(Id %in% Data$Id[Data$Period=="1970-2000"])

mp2000 <- mp+ geom_point(aes(x=Period$Longitude, 
                             y= Period$Latitude) ,
                         color="blue", size=1)
mp2000 +ggtitle("1970-2000")

Period  <- Locations %>% filter(Id %in% Data$Id[Data$Period=="2000-2016"])

mp2016 <- mp+ geom_point(aes(x=Period$Longitude, 
                             y= Period$Latitude) ,
                         color="blue", size=1)
mp2016 +ggtitle("2000-2016")

 

Density <- brick(hyde)

Dates  <- getZ(Density)
 
Hydelayer1850=which(getZ(Density)==1850)
 
Density <- dropLayer(Density,1:(Hydelayer1850-1))

Pvalues <-raster::extract(x=raster(Density, y = cbind(Locations$Longitude,Locations$Latitude),layer=1,nl=17)
Pvalues <- tbl_df(Pvalues) 
Pvalues <- Pvalues %>% mutate(Id=Locations$Id)
BeginEnd <- Pvalues %>% select(Id, X1850,X2005)
BeginEnd <- BeginEnd %>% mutate(PopClass1850= cut(X1850,breaks=X,labels=L),
                                PopClass2005= cut(X2005,breaks=X,labels=L),X1850=NULL,X2005=NULL) %>%
                         gather(TimeSlice,PopClass,2:3) %>% filter(!is.na(PopClass))


#ggplot(data=BeginEnd, aes(PopClass, fill=TimeSlice)) +geom_bar(position="stack")+ggtitle("Stations Per Period")
ggplot(data=BeginEnd, aes(PopClass, fill=TimeSlice)) +geom_bar(position="dodge")+ggtitle("Grid Population Class")


PMaxvalues <-raster::extract(x=raster(Density,layer=17), y = cbind(Locations$Longitude,Locations$Latitude),
                             buffer = 20000, fun = max)
Pvalues <- Pvalues %>% mutate(Max2005 = PMaxvalues)
Adjacent <- Pvalues %>% select(Id, X2005,Max2005) %>% filter(!is.na(X2005)) %>% 
                      mutate(Adjacent = ifelse(Max2005>5000,T,F)) %>%
                      mutate(PopClass2005 =cut(X2005,breaks=X,labels=L)) %>%
                      mutate(PopClass2005 = as.character(PopClass2005)) %>%
                      mutate(PopClass2005 = ifelse(Adjacent,paste(PopClass2005,"U",sep="_"),PopClass2005)) %>%
                      mutate(PopClass2005= as.factor(PopClass2005))

g<- ggplot(data=Adjacent, aes(PopClass2005,fill=PopClass2005)) +geom_bar( position = "dodge")
g<- g+theme(axis.text.x=element_text(angle=90, size=8)) + ggtitle("Grid Population Class 2005")

table(Adjacent$PopClass2005)  

nrow(Adjacent)

CityPoints <- read.csv(fn4)
Large <- CityPoints[CityPoints$ES00POP >=50000, c("OBJECTID","LONGITUDE","LATITUDE", "ES00POP")]

 pts <- cbind(Large$LONGITUDE,Large$LATITUDE)

for(i in 1:nrow(Locations)){
  
  pt <- c(Locations$Longitude[i],Locations$Latitude[i])
  dist = spDistsN1(pts=pts,pt=pt,longlat=TRUE)
  o <-order(dist)
  Locations[i,"distanceToLarge"]<- min(dist,na.rm=T)
  Locations[i,"CityId"]<-Large$OBJECTID[o[1]]
  print(i)
  
}

D2 <- c(-Inf,20,50,100,Inf)
D2L <-c("0-20","20-50","50-100","100+")

Locations[,"DistClass"]<- cut(Locations$distanceToLarge,breaks=D2,labels=D2L)


ggplot(data=Locations , aes(DistClass)) +geom_bar(position="dodge")+ggtitle("Distance to Large Period")


A2 <- merge(Adjacent,Locations[, c("Id","distanceToLarge","DistClass")], by.x="Id",by.y="Id",all.x=T)

A2 <- A2 %>% mutate(PopClass2005= cut(X2005, breaks=X,labels=L))

ggplot(data=A2 , aes(PopClass2005,fill=PopClass2005)) +geom_bar(position="dodge")+
  facet_wrap(~DistClass,scales="free")+
  theme(axis.text.x=element_text(angle=90, size=8))+
  ggtitle("Population class by Distance to 50K city")

db<- c(5.173,52.088)
z <- c(5.23,52.1)

u<-c(5.110000,52.10000)
DF2 <- data.frame(x=c(db[1],z[1],u[1]),y=c(db[2],z[2],u[2]))

 
DeBilt <- extent(4.6,5.6,51.5,52.5)


MZ <- GetMap(center=c( lat=52.088,lon=5.178 ),maptype="satellite",zoom = 13,SCALE=2)
PlotOnStaticMap(MZ)




Birmingham <- extent(-2.2,-1.5,52.0,53.0)
 

#52.37 52.57
# 52.48 −1.90 

Paradise <- c( −1.90,52.48)
W04 <-   c(−1.92,52.37)	
W07 <- c(−1.90,52.49)
DF <- data.frame(x=c(Paradise[1],W04[1],W07[1]),y=c(Paradise[2],W04[2],W07[2]))



L2005 <- raster(Density, layer=17)

B <-crop(L2005,Birmingham)
DE <-crop(L2005,DeBilt)
 
Bplot <-gplot(B)+geom_tile(aes(fill=value))+coord_equal()

Bplot + geom_point(data=DF,aes(x=x,y=y,color="red"))+ggtitle("Birmingham")

Bplot <-gplot(DE)+geom_tile(aes(fill=value))+coord_equal()

Bplot + geom_point(data=DF2,aes(x=x,y=y,color="red"))+ggtitle("DeBilt")



