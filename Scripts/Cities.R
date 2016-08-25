source("Initialize.R")

geonames <- "http://download.geonames.org/export/dump/cities1000.zip"
geomshap <- "http://download.geonames.org/export/dump/shapes_simplified_low.json.zip"
nearth   <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_urban_areas.zip"
cpoint   <- "G:\\BerkeleyEarthFunctions\\Data\\cities1000.txt"
fn4 <-"G:\\BerkeleyEarthFunctions\\Data\\gl_grumpv1_ppoints_csv\\gl_grumpv1_ppoints_csv\\glpv1.csv"
fn2 <-"G:\\BerkeleyEarthFunctions\\Downloads\\Station\\Monthly\\Breakpoint\\TAVG\\Jul_2016\\site_summary.txt"


download.file(geonames, destfile = file.path("Data",basename(geonames)),mode="wb")
#download.file(geonames, destfile = file.path("Data",basename(geomshap)),mode="wb")
download.file(geonames, destfile = file.path("Data",basename(nearth)),mode="wb")

unzip(file.path("Data",basename(geonames)),exdir="Data")
unzip(file.path("Data",basename(nearth)),exdir="Data") 

GeoCity <- read.delim(cpoint,header=F)
colnames(GeoCity)[c(1,)]
GrumpCity <- read.csv(fn4)
Locations <- readBerkeley(filename=fn2)