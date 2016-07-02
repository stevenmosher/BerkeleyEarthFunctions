readBerkeleySummary <-function(filename){
  
  Header <- readLines(filename,n=200)
  D      <- read.table(filename,comment.char = "%")
   
  if(basename(filename) %in% c("Complete_TAVG_summary.txt","Complete_TMAX_summary.txt",
                              "Complete_TMIN_summary.txt")){
    colnames(D)<-c("Year","AnomalyAnnual","AnnualUncertainty",
                   "AnomalyFiveYear","FiveYearUncertainty")
    Aline <- grep(" Estimated Jan 1951-Dec 1980 absolute temperature",Header,ignore.case=T)
    Absolute <- str_sub(Header[Aline],str_locate(pattern=fixed(":"),Header[Aline])[2]+1)
    Absolute <- str_sub(Absolute,1,str_locate(pattern=fixed("+"),Absolute)[2]-1)
    Absolute <- as.numeric(str_trim(Absolute))
    if(grepl("TMAX",basename(filename))) D[,"TMAX"]<-Absolute+D$AnomalyAnnual
    if(grepl("TMIN",basename(filename))) D[,"TMIN"]<-Absolute+D$AnomalyAnnual
    if(grepl("TAVG",basename(filename))) D[,"TAVG"]<-Absolute+D$AnomalyAnnual
    D <- tbl_df(D[,c(1,6,2,3,4,5)])
    return(D)
  } 
  if(basename(filename) %in% c("Complete_TAVG_complete.txt","Complete_TMAX_complete.txt",
                               "Complete_TMIN_complete.txt")){
    colnames(D)<-c("Year","Month","AnomalyMonthly","MonthlyUncertainty","AnomalyAnnual","AnnualUncertainty",
                   "AnomalyFiveYear","FiveYearUncertainty","AnomalyTenYear","TenYearUncertainty",
                   "AnomalyTwentyYear","TwentyYearUncertainty")
    Aline <- grep(" Feb   Mar   Apr",Header,ignore.case=T)+1
    Absolute <- str_trim(str_sub(Header[Aline],str_locate(pattern=fixed("%"),Header[Aline])[2]+1))
    BasePeriod <-as.numeric(unlist(str_split(Absolute, pattern = "\\s+")))
    if(grepl("TMAX",basename(filename))) D[,"TMAX"]<- D$AnomalyMonthly+rep(BasePeriod, length.out =nrow(D))
    if(grepl("TMIN",basename(filename))) D[,"TMIN"]<-D$AnomalyMonthly+rep(BasePeriod, length.out =nrow(D))
    if(grepl("TAVG",basename(filename))) D[,"TAVG"]<-D$AnomalyMonthly+rep(BasePeriod, length.out =nrow(D))
    D <- tbl_df(D[,c(1,2,13,3:12)])
    return(D)
  } 
  if(basename(filename) %in% c("Complete_TAVG_daily.txt","Complete_TMAX_daily.txt",
                               "Complete_TMIN_daily.txt")){
    colnames(D)<-c("Date","Year","Month","Day",
                   "DayofYear","Anomaly")
    Aline <- grep(" Estimated Jan 1951-Dec 1980 land-average temperature",Header,ignore.case=T)
    Absolute <- str_sub(Header[Aline],str_locate(pattern=fixed(":"),Header[Aline])[2]+1)
    Absolute <- str_sub(Absolute,1,str_locate(pattern=fixed("+"),Absolute)[2]-1)
    Absolute <- as.numeric(str_trim(Absolute))
    if(grepl("TMAX",basename(filename))) D[,"TMAX"]<-Absolute+D$Anomaly
    if(grepl("TMIN",basename(filename))) D[,"TMIN"]<-Absolute+D$Anomaly
    if(grepl("TAVG",basename(filename))) D[,"TAVG"]<-Absolute+D$Anomaly
    D <- tbl_df(D[,c(1:5,7,6)])
    return(D)
  } 
  if(basename(filename) =="Land_and_Ocean_summary.txt"){
    colnames(D)<-c("Year","AnomalyAnnualIceSAT","AnnualUncertaintyIceSAT",
                   "AnomalyFiveYearIceSAT","FiveYearUncertaintyIceSAT","AnomalyAnnualIceSST","AnnualUncertaintyIceSST",
                   "AnomalyFiveYearIceSST","FiveYearUncertaintyIceSST")
    satline <- grep("%   Using air temperature above sea ice",Header,ignore.case=T)
    Absolute <- str_sub(Header[satline],str_locate(pattern=fixed(":"),Header[satline])[2]+1)
    Absolute <- str_sub(Absolute,1,str_locate(pattern=fixed("+"),Absolute)[2]-1)
    Absolute <- as.numeric(str_trim(Absolute))
    D[,"TAVG_ICESAT"]<-Absolute+D$AnomalyAnnualIceSAT
    
    sstline <- grep("%   Using water temperature below sea ice",Header,ignore.case=T)
    Absolute <- str_sub(Header[sstline],str_locate(pattern=fixed(":"),Header[sstline])[2]+1)
    Absolute <- str_sub(Absolute,1,str_locate(pattern=fixed("+"),Absolute)[2]-1)
    Absolute <- as.numeric(str_trim(Absolute))
    D[,"TAVG_ICESST"]<-Absolute+D$AnomalyAnnualIceSST
     
    D <- tbl_df(D[,c(1,10,2,3,4,5,11,6:9)])
    return(D)
  }
  if(basename(filename)  =="Land_and_Ocean_complete.txt"){
    colnames(D)<-c("Year","Month","AnomalyMonthly","MonthlyUncertainty","AnomalyAnnual","AnnualUncertainty",
                   "AnomalyFiveYear","FiveYearUncertainty","AnomalyTenYear","TenYearUncertainty",
                   "AnomalyTwentyYear","TwentyYearUncertainty")
    Aline <- grep(" Feb   Mar   Apr",Header,ignore.case=T)+1
    Absolute <- str_trim(str_sub(Header[Aline],str_locate(pattern=fixed("%"),Header[Aline])[2]+1))
    BasePeriod <-as.numeric(unlist(str_split(Absolute, pattern = "\\s+")))
    if(grepl("TMAX",basename(filename))) D[,"TMAX"]<- D$AnomalyMonthly+rep(BasePeriod, length.out =nrow(D))
    if(grepl("TMIN",basename(filename))) D[,"TMIN"]<-D$AnomalyMonthly+rep(BasePeriod, length.out =nrow(D))
    if(grepl("TAVG",basename(filename))) D[,"TAVG"]<-D$AnomalyMonthly+rep(BasePeriod, length.out =nrow(D))
    D <- tbl_df(D[,c(1,2,13,3:12)])
    return(D)
  } 
  
  
  
}