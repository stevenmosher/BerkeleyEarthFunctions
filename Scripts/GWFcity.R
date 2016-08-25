


cpoint   <- "G:\\BerkeleyEarthFunctions\\Data\\cities1000.txt"

fn4 <-"G:\\BerkeleyEarthFunctions\\Data\\gl_grumpv1_ppoints_csv\\gl_grumpv1_ppoints_csv\\glpv1.csv"
p  <- "G:\\BerkeleyEarthFunctions\\Data\\gpw-v4-population-count-2015\\gpw-v4-population-count_2015.tif"

City <- tbl_df(read.csv(fn4)) %>% select(OBJECTID,LONGITUDE,LATITUDE,ES00POP) %>% filter(ES00POP <2000 &ES00POP >500)
Pop  <- raster(p)

lonlat <- cbind(City$LONGITUDE,City$LATITUDE)

P <-raster::extract(Pop,y=lonlat)
PA <- raster::extract(Pop,y=lonlat,buffer=5000, fun =sum)

City <- City %>% mutate(Gwf4=P,km5=PA)