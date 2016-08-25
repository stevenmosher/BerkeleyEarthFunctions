

milesTokm <- 2.58999

X <- c(-Inf,25,150,500,2500,10000,Inf) * milesTokm
L <- c("Natural","Rural","ExUrban","Sprawl","DenseSuburb","UrbanCore")


hyde <- "F:\\HydeALL\\Density.grd"
fn <-"G:\\BerkeleyEarthFunctions\\Downloads\\Station\\Monthly\\Breakpoint\\TAVG\\Jul_2016\\data_characterization.txt"
fn2 <-"G:\\BerkeleyEarthFunctions\\Downloads\\Station\\Monthly\\Breakpoint\\TAVG\\Jul_2016\\site_summary.txt"

Stations <-readBerkeley(filename=fn)
Locations <- readBerkeley(filename=fn2)
Locations <- left_join(Locations,Stations, by = "Id")

conusExt <- extent(-135,-50,20,50)
chicago  <- extent(-90,-70,35,50)
africa   <- extent(-10,50,10,30)
London   <- extent(-2,2,48,53)


Density <- brick(hyde)

Dates  <- getZ(Density)
Hydelayer1800=which(getZ(Density)==1800)
Hydelayer1850=which(getZ(Density)==1850)
Hydelayer1900=which(getZ(Density)==1900)
Hydelayer1950=which(getZ(Density)==1950)
Hydelayer2000=which(getZ(Density)==2000)
Hydelayer2005=which(getZ(Density)==2005)

A <- crop(raster(Density,layer=Hydelayer2005),africa)
A2 <-cut(A,breaks=X)
RPA <- cut(Density, breaks=X)

Density2005 <- raster(RPA, Hydelayer2005)

LND <-crop(Density2005,London)

mat=matrix(c(-Inf,0,0,0,10,1,10,100,2,100,1000,3,
             1000,10000,4,10000,100000,5),ncol=3,byrow=T)

Density2005RC <- raster::reclassify(Density2005, rcl=mat)
gplot(crop(Density2005,chicago))+geom_tile(aes(fill=value))+coord_equal()


POP_All <- raster::extract(x=Density2005, y = cbind(Locations$Longitude,Locations$Latitude))
Locations <- Locations %>% mutate(Pop2005 = POP_All) %>% mutate(Active= ifelse(LateYear >2004,TRUE,FALSE)) %>%
  filter(!is.na(Pop2005)) %>% mutate(LogPop = ifelse(Pop2005< 1,0,log10(Pop2005)))

ggplot(data=Locations, aes(LogPop, fill=Active)) +geom_histogram(position="dodge") +ggtitle("Active Versus Non Active")

ggplot(filter(Locations,Active==T), aes(LogPop)) + stat_ecdf(geom = "step")
ggplot( Locations , aes(LogPop,color=Active)) + stat_ecdf(geom = "step")

ggplot( Locations , aes(EarlyYear,color=Active)) + stat_ecdf(geom = "step")

Conus2005 <- crop(raster(Density,Hydelayer2005),conusExt)

Zero <-Which(Conus2005 == 0, cells=TRUE)
One <-Which(Conus2005 > 9 & Conus2005 < 11, cells=TRUE)
Two <-Which(Conus2005 > 90 & Conus2005 < 110, cells=TRUE)
Three <-Which(Conus2005 > 900 & Conus2005 < 1100, cells=TRUE)
Four <-Which(Conus2005 > 9000 & Conus2005 < 11000, cells=TRUE)
F1 <-Which(Conus2005 > 4000 & Conus2005 < 6000, cells=TRUE)
F2 <-Which(Conus2005 > 1900 & Conus2005 < 2100, cells=TRUE)
F3 <-Which(Conus2005 > 2900 & Conus2005 < 3100, cells=TRUE)
Cmax <-Which(Conus2005 > 19000  , cells=TRUE)

ZeroXY <-xyFromCell(Conus2005, Zero[10000])
OneXY <-xyFromCell(Conus2005, One[1080])
TwoXY <-xyFromCell(Conus2005, Two[800])
ThreeXY <-xyFromCell(Conus2005,Three[120])
FourXY <-xyFromCell(Conus2005, Four[6])
F1XY <-xyFromCell(Conus2005, F1[2])
F2XY <-xyFromCell(Conus2005, F1[15])
F3XY <-xyFromCell(Conus2005, F1[10])
maxXY <-xyFromCell(Conus2005, Cmax)



MZ <- GetMap(center=c( lat=ZeroXY[1,2],lon=ZeroXY[1,1] ),maptype="satellite",zoom = 13,SCALE=2)
PlotOnStaticMap(MZ)
MO <- GetMap(center=c( lat=OneXY[1,2],lon=OneXY[1,1] ),maptype="satellite",zoom = 13,SCALE=2)
PlotOnStaticMap(MO)
MO1 <- GetMap(center=c( lat=OneXY[1,2],lon=OneXY[1,1] ),maptype="satellite",zoom = 16,SCALE=2)
PlotOnStaticMap(MO1)
MT <- GetMap(center=c( lat=TwoXY[1,2],lon=TwoXY[1,1] ),maptype="satellite",zoom = 13,SCALE=2)
PlotOnStaticMap(MT)
MT1 <- GetMap(center=c( lat=TwoXY[1,2],lon=TwoXY[1,1] ),maptype="satellite",zoom = 16,SCALE=2)
PlotOnStaticMap(MT1)
MTH <- GetMap(center=c( lat=ThreeXY[1,2],lon=ThreeXY[1,1] ),maptype="satellite",zoom = 13,SCALE=2)
MTH1 <- GetMap(center=c( lat=ThreeXY[1,2],lon=ThreeXY[1,1] ),maptype="satellite",zoom = 16,SCALE=2)
PlotOnStaticMap(MTH)
PlotOnStaticMap(MTH1)
MF <- GetMap(center=c( lat=FourXY[1,2],lon=FourXY[1,1] ),maptype="satellite",zoom = 16,SCALE=2)
PlotOnStaticMap(MF)
MX <- GetMap(center=c( lat=maxXY[1,2],lon=maxXY[1,1] ),maptype="satellite",zoom = 13,SCALE=2)
MX1 <- GetMap(center=c( lat=maxXY[1,2],lon=maxXY[1,1] ),maptype="satellite",zoom = 17,SCALE=2)
PlotOnStaticMap(MX)
PlotOnStaticMap(MX1)
MF1 <- GetMap(center=c( lat=F1XY[1,2],lon=F1XY[1,1] ),maptype="satellite",zoom = 13,SCALE=2)
PlotOnStaticMap(MF1)
MF2 <- GetMap(center=c( lat=F2XY[1,2],lon=F2XY[1,1] ),maptype="satellite",zoom = 13,SCALE=2)
PlotOnStaticMap(MF2)
MF3 <- GetMap(center=c( lat=F3XY[1,2],lon=F3XY[1,1] ),maptype="satellite",zoom = 13,SCALE=2)
PlotOnStaticMap(MF3)
