

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
Birmingham <- extent(-2.2,-1.5,52.3,52.7)

#52.37 52.57
# 52.48 −1.90 

Paradise <- c( −1.90,52.48)
W04 <-   c(−1.92,52.37)	
W07 <- c(−1.90,52.49)

Density <- brick(hyde)

Dates  <- getZ(Density)
Hydelayer1800=which(getZ(Density)==1800)
Hydelayer1850=which(getZ(Density)==1850)
Hydelayer1900=which(getZ(Density)==1900)
Hydelayer1950=which(getZ(Density)==1950)
Hydelayer2000=which(getZ(Density)==2000)
Hydelayer2005=which(getZ(Density)==2005)

B <- crop(raster(Density,layer=Hydelayer2005),Birmingham)
B2 <-cut(B,breaks=X)

 
MZ <- GetMap(center=c( lat=52.5,lon=-1.9),maptype="satellite",zoom = 13,SCALE=2)
PlotOnStaticMap(MZ,   FUN=points)
points(x=Paradise[1],y=Paradise[2],col="red")

MZ <- GetMap(center=c( lat=52.5,lon=-1.9),maptype="satellite",zoom = 16,SCALE=2)
PlotOnStaticMap(MZ,   FUN=points)
points(x=Paradise[1],y=Paradise[2],col="red")

MZ <- GetMap(center=c( lat=52.5,lon=-1.9),maptype="satellite",zoom = 17,SCALE=2)
PlotOnStaticMap(MZ,   FUN=points)
points(x=Paradise[1],y=Paradise[2],col="red")

MZ <- GetMap(center=c( lat=W04[2],lon=W04[1]),maptype="satellite",zoom = 12,SCALE=2)
PlotOnStaticMap(MZ,   FUN=points)
points(x=W04[1],y=W04[2],col="blue")

MZ <- GetMap(center=c( lat=W04[2],lon=W04[1]),maptype="satellite",zoom = 15,SCALE=2)
PlotOnStaticMap(MZ,   FUN=points)
points(x=W04[1],y=W04[2],col="blue")

MZ <- GetMap(center=c( lat=W07[2],lon=W07[1]),maptype="satellite",zoom = 15,SCALE=2)
PlotOnStaticMap(MZ,   FUN=points)
points(x=W07[1],y=W07[2],col="blue")

MZ <- GetMap(center=c( lat=W07[2],lon=W07[1]),maptype="satellite",zoom = 17,SCALE=2)
PlotOnStaticMap(MZ,   FUN=points)
points(x=W07[1],y=W07[2],col="blue")
 


MZ2 <- GetMap(center=c( lat=52.5,lon=-1.9),maptype="satellite",zoom = 12,SCALE=2)
PlotOnStaticMap(MZ2)

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
