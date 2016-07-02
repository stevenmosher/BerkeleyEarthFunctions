readBerkeleyNetCDF<- function(filename = fname, Extent=NULL,start=NULL,end=NULL){
  
  require(raster)
  X<- brick(filename, varname = 'temperature')
  CL <- brick(filename,varname= 'climatology')
  LM <- raster(filename, varname="land_mask")
  Z <- getZ(X)
   
  if(!is.null(start) & !is.null(end)){
    if(end < start)stop("end is less than start")
    
    
  }
  if(!is.null(Extent)){
    X<-crop(X,Extent)
    CL<- crop(CL,Extent)
    Z <- crop(LM,Extent)
    
  }
  print( "Returning  3 rasters")
  
  return(list(Temperature=X,Climatology=CL,LandMask=LM))  
}