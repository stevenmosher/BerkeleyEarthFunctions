
source("Initialize.R")


hyde <- "F:\\HydeALL\\Density.grd"
fn <-"G:\\BerkeleyEarthFunctions\\Downloads\\Station\\Monthly\\Breakpoint\\TAVG\\Jul_2016\\data_characterization.txt"
fn2 <-"G:\\BerkeleyEarthFunctions\\Downloads\\Station\\Monthly\\Breakpoint\\TAVG\\Jul_2016\\site_summary.txt"
fn3 <- "G:\\BerkeleyEarthFunctions\\Downloads\\Station\\Monthly\\Quality\\TAVG\\Jul_2016\\data.txt"
fn4 <- "G:\\BerkeleyEarthFunctions\\Downloads\\Station\\Monthly\\Breakpoint\\TAVG\\Jul_2016\\data.txt"

fn2 <-"G:\\BerkeleyEarthFunctions\\Downloads\\Station\\Monthly\\Breakpoint\\TAVG\\Jul_2016\\site_detail.txt"

 
Locations <- readBerkeley(filename=fn2)
Data      <- readBerkeley(filename=fn3)
Data      <- Data %>% select(Id,Year) %>% unique()
Density <- brick(hyde)

Pop      <- tbl_df(raster::extract(Density, y=cbind(Locations$Longitude,Locations$Latitude),df=T))
Pop      <- Pop %>% mutate(ID = Locations$Id) %>% rename(Id=ID) %>% gather(Year,Pop,2:33) %>%
            mutate(Year = as.numeric(str_replace(Year,"X",""))) %>% filter(!is.na(Pop)) %>% filter(Year> 1849)

Pid <- unique(Pop$Id)

Pop2 <- data.frame( Id= rep(Pid,each= length(1850:2005)),Year=1850:2005,Pop=NA)

for(i in 1:length(Pid)){
  
  x= Pop$Year[Pop1$Id==Pid[i]]
  y= Pop$Pop[Pop1$Id==Pid[i]]
  p2 <- approx(x=x,y=y,xout=1850:2005)
  
  dex <- which(Pop2$Id==Pid[i] & Pop2$Year==p2$x)
  Pop2$Pop[dex]<-p2$y
   
  print(i)
  
}

Pop2 <- tbl_df(Pop2)
Data <- left_join(Data,Pop2, by =c("Id","Year")) %>% filter(!is.na(Pop))
D2   <- Data %>% group_by(Id)%>%summarize(MinYear= min(Year),MaxYear=max(Year))

D2   <- merge(D2,Data, by.x=c("Id","MinYear"),by.y=c("Id","Year"),all.x=T)
colnames(D2)[4]<-"MinPop"
D2   <- merge(D2,Data, by.x=c("Id","MaxYear"),by.y=c("Id","Year"),all.x=T)
colnames(D2)[5]<-"MaxPop"

D2 <- tbl_df(D2)

rm(Pop)

ggplot(D2, aes(x=MinPop,y=MaxPop))+geom_point() 


P <- Pop2 %>% group_by(Id)%>% do(tidy(lm(Pop~Year,.)))

P <- P %>% select(Id, term, estimate) %>% filter(term=="Year")

D2 <- left_join(D2,P, by="Id")

D2 <- mutate(D2,Urban = ifelse(MinPop<50,"Rural","Urban"))

rm(Pop2)
rm(P)
rm(Data)
gc()

ggplot(D2, aes(x=Urban, y= estimate ))+geom_boxplot(notch=T,outlier.shape = NA) +
  scale_y_continuous(limits = c(-5, 5))

ggplot(D2, aes(x=MinPop,y=MaxPop))+geom_point() + facet_grid(~Urban)
 

Raw      <- readBerkeley(filename=fn3)

Raw <- Raw %>% filter(Id %in% D2$Id) %>% select(Id,Year,Month, Temperature) %>%
   mutate(Date = Year + (Month-1)/12, Year=NULL,Month=NULL)

S <- Raw %>% group_by(Id)%>% do(tidy(lm(Temperature~Date,.)))

rm(Raw)

S <- S %>% select(Id, term, estimate) %>% filter(term=="Date") %>% select(Id,estimate)%>%
     rename(RawSlope= estimate)

D2 <- left_join(D2,S, by= "Id")

rm(S)


D2 <- mutate(D2,Urban2 = ifelse(MaxPop<50,"Rural","Urban"))

Adj      <- readBerkeley(filename=fn4)

Adj <- Adj %>% filter(Id %in% D2$Id) %>% select(Id,Year,Month, Temperature) %>%
  mutate(Date = Year + (Month-1)/12, Year=NULL,Month=NULL)

S2 <- Adj %>% group_by(Id)%>% do(tidy(lm(Temperature~Date,.)))

rm(Adj)

S2 <- S2 %>% select(Id, term, estimate) %>% filter(term=="Date") %>% select(Id,estimate)%>%
  rename(AdjSlope= estimate)

D2 <- left_join(D2,S2, by= "Id")

D2 <- D2 %>% mutate(Adjustment = AdjSlope-RawSlope, Years= MaxYear-MinYear+1)


ggplot(filter(D2, Adjustment ==0 & Urban2=="Rural"),aes(AdjSlope))+geom_histogram()

ggplot(filter(D2, Adjustment ==0 & Urban2=="Rural" & Years <10),aes(AdjSlope))+geom_histogram()

ggplot(filter(D2,  Urban2=="Rural" & Years >100),aes(AdjSlope))+geom_histogram()
ggplot(filter(D2,  Urban2=="Rural" & Years >100),aes(RawSlope))+geom_histogram()
 

ggplot(filter(D2,  Urban2=="Urban" & Years >100),aes(AdjSlope))+geom_histogram()
ggplot(filter(D2,  Urban2=="Urban" & Years >100),aes(RawSlope))+geom_histogram()
ggplot(filter(D2,  Urban=="Rural" & Urban2=="Urban" & Years >50),aes(x=Years,y=Adjustment))+geom_point() 


LongD2 <- D2 %>% filter(Years > 4 & Adjustment==0)

LongD2adj <- D2 %>% filter(Years > 4 & Adjustment!=0)


ggplot(LongD2adj,aes(Adjustment))+geom_histogram() + facet_wrap(~Urban2)


ggplot(filter(LongD2adj,Urban2=="Urban" & Urban=="Rural"),aes(Adjustment))+geom_histogram()+  
  geom_vline(aes(xintercept=mean(Adjustment)),
             color="blue", linetype="dashed", size=1)

Neg <- D2 %>% filter(Adjustment < 0)