source("Initialize.R")

d1 <- "G:\\BerkeleyEarthFunctions\\Data\\GlobeLand30_ATS2010_1"
d2 <- "G:\\BerkeleyEarthFunctions\\Data\\GlobeLand30_ATS2010_2"
d3 <- "G:\\BerkeleyEarthFunctions\\Data\\GlobeLand30_ATS2010_3"

F1 <- list.files(d1, full.names=T,recursive = T, pattern= 'TIF')
F2 <- list.files(d2, full.names=T,recursive = T, pattern= 'TIF')
F3 <- list.files(d3, full.names=T,recursive = T, pattern= 'TIF')
Files <- c(F1,F2,F3)
fn <- basename(Files)
length(unique(fn))

DF <- tbl_df(data.frame(Files=Files, Name=fn,stringsAsFactors = FALSE))
DF <- DF %>% mutate(LLcode= str_sub(Name,1,6)) %>% mutate(Lat=as.numeric(str_sub(Name,5,6))) %>%
             mutate(NS=str_sub(Name,1,1)) %>% mutate(dex=as.numeric(str_sub(Name,2,3))) %>% 
             mutate(Lat =ifelse(NS=="N",Lat*1,Lat*-1)) %>%
             mutate(LatMin = ifelse(NS=="N",Lat,Lat-5)) %>% mutate(LatMax = ifelse(NS=="N",Lat+5,Lat)) %>%
             mutate(LonMin =-180 +((dex-1)*6)) %>% mutate(LonMax =-180 +  dex*6)


s<- seq(from=1,to=59,by=2)

idex <- which(abs(DF$LatMax)>60 & DF$dex %in% s)
 

DF$LonMin[idex] <- -180 +  (DF$dex[idex]%/%2)*12
DF$LonMax[idex] <- -180 +  ((DF$dex[idex]%/%2)+1)*12

write.csv(DF, "UrbanFileMetaData.csv")

