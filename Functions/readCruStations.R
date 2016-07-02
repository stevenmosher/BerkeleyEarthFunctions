

readCruStations <- function(inFile, output=c( "Data","Inventory")){
      require(dplyr)
      
     if(length(output)>2)stop(cat("output selection wrong", output, "\n"))
     output <- tolower(output)
     matchedO <- sum(output %in% c("data","inventory"))
     if(matchedO==0)stop(cat("output selection wrong", output, "\n"))
  
      X <- readLines(inFile)
      headerLength <- nchar(X[1])
      if(headerLength == 83){
        CRU4 = TRUE
        CRU3 = FALSE
      }else{
        CRU3 = TRUE
        CRU4 = FALSE
      }
      
      
      dex <- which(nchar(X[]) == headerLength)
      headers  <- X[dex]
      
      
      if(CRU3){ 

         Inv <- data.frame(Id  =  as.numeric(substr(headers,1,6)),
                           Lat = as.numeric(substr(headers,7,10)),
                           Lon = as.numeric(substr(headers,11,15)),
                           Altitude = as.numeric(substr(headers,16,20)),
                           Name = substr(headers,22,42),
                           Country = substr(headers,43,56),
                           StartYear = as.numeric(substr(headers,57,60)),
                           EndYear  =  as.numeric(substr(headers,61,64)),
                           Source   =  as.numeric(substr(headers,67,68)),
                           GoodFirst = as.numeric(substr(headers,69,72)),
                           stringsAsFactors = FALSE)

        Inv$Lat[Inv$Lat == -999]    <- NA
        Inv$Lon[Inv$Lon == -1999]   <- NA
        Inv$Lat <- Inv$Lat / 10
        Inv$Lon <- (-1)*Inv$Lon / 10
            
        Inv$Altitude[Inv$Altitude == -999] <- NA
      
      } else {
        Inv <- data.frame(Id  =  as.numeric(substr(headers,1,7)),
                          Lat = as.numeric(substr(headers,8,11)),
                          Lon = as.numeric(substr(headers,12,16)),
                          Altitude = as.numeric(substr(headers,17,21)),
                          Name = substr(headers,23,43),
                          Country = substr(headers,44,57),
                          StartYear = as.numeric(substr(headers,58,61)),
                          EndYear  =  as.numeric(substr(headers,62,65)),
                          Source   =  as.numeric(substr(headers,66,69)),
                          GoodFirst = as.numeric(substr(headers,70,73)),
                          Index    = as.numeric(substr(headers,74,78)),
                          GridNumber    = as.numeric(substr(headers,79,83)),
                          stringsAsFactors = FALSE)
        
        Inv$Lat[Inv$Lat == -999]    <- NA
        Inv$Lon[Inv$Lon == -1999]   <- NA
        Inv$Lat <- Inv$Lat / 10
        Inv$Lon <- (-1)*Inv$Lon / 10
        
        Inv$Altitude[Inv$Altitude == -999] <- NA
        Inv <- tbl_df(Inv)
        
      }
      
      if(matchedO ==1 & output == "inventory")return(Inv)

      begin <- min(Inv$StartYear)
      end   <- max(Inv$EndYear)
      Years <- begin:end
      stations <- nrow(Inv)
      DATA <- array(NA,dim=c(stations,12,length(Years)))
      dimnames(DATA)<-list(Inv$Id,month.abb,Years)
     
      stationCount <- 0
      for(line in 1:length(X)){
        if (nchar(X[line]) == headerLength){
          # we have a header
          stationCount <- stationCount + 1
        } else {
          # we have a data record
          thisLine <- as.numeric(unlist(strsplit(X[line]," +")))
          thisLine[thisLine == -999] <-NA
          thisYear <- thisLine[1] - begin + 1
          DATA[stationCount,,thisYear] <- thisLine[2:13]/10
          
        }
      }
      
      DATA <- apply(DATA,MARGIN = 1, FUN = c)
      DATA <- ts(DATA, start = begin, frequency = 12)
   if(matchedO == 2)return(list(Data = DATA,Inventory=Inv))
   if(matchedO ==1 & output == "data")return( DATA )
           
       
}

 
 