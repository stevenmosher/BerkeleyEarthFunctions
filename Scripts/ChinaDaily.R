
library(raster)
library(ncdf4)
library(ggplot2)
library(dplyr)
library(magrittr)
library(zoo)


is.leapyear=function(year){
  #http://en.wikipedia.org/wiki/Leap_year
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

####   My file names.. you should change these

city <- "G:\\BerkeleyEarthFunctions\\Data\\gl_grumpv1_ppoints_csv\\gl_grumpv1_ppoints_csv\\glpv1.csv"
tavg1950 <-"G:\\BerkeleyEarthFunctions\\Downloads\\Global\\Daily\\Gridded\\TAVG\\Jul_2016\\Complete_TAVG_Daily_LatLong1_1950.nc"

##  Using  dplyr

Locations <- tbl_df(read.csv(city,stringsAsFactors = FALSE))

Locations <- Locations %>% select(OBJECTID,LONGITUDE,LATITUDE,NAME1, COUNTRY) %>% filter(COUNTRY=="China")

##  Just to make it fast I pick 10 cities
Locations <- Locations[1:10,]

 

getTimeSeries <- function(rasterFileName=tavg1950, L=Locations,FirstYear=1950){
  
  is.leapyear=function(year){
    #http://en.wikipedia.org/wiki/Leap_year
    return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
  }
  
  ##  us the ID for column names
  ID          <- unique(L$OBJECTID)
  lonlat      <- cbind(select(L,LONGITUDE,LATITUDE))
  #  load the  data
  Temps       <- brick(rasterFileName, varname='temperature')
  Climatology <- brick(rasterFileName, varname='climatology')
  #  extract the values at locations
  ChinaCities <- raster::extract(Temps,lonlat,layer=1,nl=nlayers(Temps))
  CityClimate <- raster::extract(Climatology,lonlat,layer=1,nl=nlayers(Climatology))
  
  ##   start of the date series
  FY <- paste(FirstYear,"-01-01", sep ="")
  
  date1 <- seq(from= as.Date(FY), length.out=nlayers(Temps), by = "day")
  monthday <- format(date1, "%m%d")
  #  an index of the first days of the year
  dex  <- which(monthday=="0101")
  
  #  transform row and column
  ChinaCities <- t(as.matrix(ChinaCities))
  #  name the columns
  dimnames(ChinaCities)[2]<- list(ID)
  # make a time series  with leap days
  ChinaCities <- zoo(ChinaCities, order.by=date1)
  
  #  make a climatology for each city, flip row and column
  CityClimate <- t(as.matrix(CityClimate))
  
  #  make a  leap year climatology.. interpolate for feb 29
  CityClimateLeapYear <- matrix(NA, ncol=ncol(CityClimate), nrow=366)
  Feb28 <- 59
  Feb29 <- 60
  Mar1  <- 61
  CityClimateLeapYear[1:Feb28,1:ncol(CityClimate)]<-CityClimate[1:Feb28,1:ncol(CityClimate)] 
  CityClimateLeapYear[Mar1:366,1:ncol(CityClimate)]<-CityClimate[Feb29:365,1:ncol(CityClimate)]
  CityClimateLeapYear[Feb29,1:ncol(CityClimate)]<- (CityClimateLeapYear[Feb28,1:ncol(CityClimate)]+CityClimateLeapYear[Mar1,1:ncol(CityClimate)])/2 
  
  ###  Now loop through the first days of the years.. and add the correct climatology
  for(y in dex){
    
    # y is the index of rows
    # get the full date and get the Year
    FirstDay <- time(ChinaCities)[y]
    Year    <-  as.numeric(format(FirstDay,"%Y"))
    
    # if we have a leap year  add the correct climatology
    if(is.leapyear(Year)){
      ChinaCities[y:(y+365),1:ncol(ChinaCities)] <-ChinaCities[y:(y+365),1:ncol(ChinaCities)]+
                                                       CityClimateLeapYear
      
    }else
    {
      ChinaCities[y:(y+364),1:ncol(ChinaCities)] <-ChinaCities[y:(y+364),1:ncol(ChinaCities)]+
        CityClimate 
    }
    print(FirstDay)
  }
    
  
 return(ChinaCities) 
}




