
# create function to deal with NetCDF files
ncdf2rast <- function(i,nc){
  
  # load packages
  require(ncdf4) # package for netcdf manipulation
  require(raster) # package for raster manipulation
  require(rgdal) # package for geospatial analysis
  
  # open NetCDF file
  nc_data <- nc_open(paste0(nc,".nc"))
  
  # Save the print(nc) dump to a text file
  {
    sink(paste0(nc,".txt"))
    print(nc_data)
    sink()
  }
  
  # retrieving the longitude
  lon <- ncvar_get(nc_data, "longitude")
  lon[lon > 180] <- lon[lon > 180] - 360
  
  # retrieving the latitude
  lat <- ncvar_get(nc_data, "latitude", verbose = F)
  
  # retrieving variables
  var <- attributes(nc_data$var)
  var <- var$names
  
  # create array for variable nº i
  array <- ncvar_get(nc_data, var[i]) # store the data in a 3-dimensional array
  
  # retrieve fill value of variable nº i
  fillvalue <- ncatt_get(nc_data, var[i], "_FillValue")
  
  # change fill value to NA
  array[array == fillvalue$value] <- NA
  
  # turn array into raster object
  r <- raster(array, xmn=min(lon), 
              xmx=max(lon),
              ymn=min(lat),
              ymx=max(lat),
              crs=CRS(SRS_string="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  
  # free unused memory
  gc()
  
  # load package
  require("terra")
  
  # creat SpatRaster object
  r <- rast(r)
  
  # change name of layers to name of variables
  names(r) <- var[i]
  
  # close NetCDF file
  nc_close(nc_data) 
  
  return(r)
}

# generalising the process 

# load package
library("terra")

# create object with the "i" number of variables
n <- c(1:33)

# create stacked raster
LU2015 <- do.call(c, lapply(n,ncdf2rast,nc = "LU/585/GCAM_Demeter_LU_ssp5_rcp85_modelmean_2015"))
gc()
rm(n);gc()

writeRaster(LU2015,"2015.tif", overwrite = T)
rm(LU2015)
gc()

# create object with the "i" number of variables
n <- c(1:33)

# create stacked raster
LU2040 <- do.call(c, lapply(n,ncdf2rast,nc = "LU/585/GCAM_Demeter_LU_ssp5_rcp85_modelmean_2040"))
gc()
rm(n);gc()

writeRaster(LU2040,"2040.tif", overwrite = T)
rm(LU2040)
gc()

# create object with the "i" number of variables
n <- c(1:33)

# create stacked raster
LU2070 <- do.call(c, lapply(n,ncdf2rast,nc = "LU/585/GCAM_Demeter_LU_ssp5_rcp85_modelmean_2070"))
gc()
rm(n);gc()

writeRaster(LU2070,"2070.tif", overwrite = T)
rm(LU2070)
gc()

# create object with the "i" number of variables
n <- c(1:33)

# create stacked raster
LU2100 <- do.call(c, lapply(n,ncdf2rast,nc = "LU/585/GCAM_Demeter_LU_ssp5_rcp85_modelmean_2100"))
gc()
rm(n);gc()

writeRaster(LU2100,"2100.tif", overwrite = T)
rm(LU2100)
gc()

# remove function
rm(ncdf2rast)

# import rasters
n2015 <- rast("2015.tif")
n2040 <- rast("2040.tif")
n2070 <- rast("2070.tif")
n2100 <- rast("2100.tif")

# formatting the layout
par(mfrow = c(2,2))

# making a loop
for(i in seq_along(names(n2015))){
  
  # load required package
  require("stringr")
  
  # making title for plots
  t2015 <- str_sub(sources(n2015),nchar(getwd())+2,nchar(sources(n2015))-4)
  t2040 <- str_sub(sources(n2040),nchar(getwd())+2,nchar(sources(n2040))-4)
  t2070 <- str_sub(sources(n2070),nchar(getwd())+2,nchar(sources(n2070))-4)
  t2100 <- str_sub(sources(n2100),nchar(getwd())+2,nchar(sources(n2100))-4)
  
  # making plots
  plot(n2015[[i]]/100, main = t2015, range = c(0,1))
  plot(n2040[[i]]/100, main = t2040, range = c(0,1))
  plot(n2070[[i]]/100, main = t2070, range = c(0,1))
  plot(n2100[[i]]/100, main = t2100, range = c(0,1))
  
  # adding overall title
  mtext(names(n2015[[i]]), side=3, outer=T, line=-1.5)
  
  # removing unnecessary objects
  rm(i,t2015,t2040,t2070,t2100)
  
  # garbage collection - free unused memory
  gc()
}

######################################################
### checking if the species timesteps are the same ###
######################################################

# import data
AllRast <- rast(list.files("LU/Future/tif/",
                           pattern = "tif",
                           full.names=T))

# get sources
sources <- sources(AllRast)

# processing the sources
# Here and in the line below 69 is the length of this string: nchar(getwd()) + location of rasters + 2
usources <- unique(substr(sources,69,nchar(sources)-4)) 

# looping the process for all the species
for(i in seq_along(usources)){
  
  # choosing species
  a <- c(which(substr(sources(AllRast),69,nchar(sources(AllRast))-4) == usources[i]))
  
  # giving names to layers
  names(AllRast[[a]]) <- c("2015","2040","2070","2100",
                           "2015","2040","2070","2100")
  
  # extracting target species
  RastA <- AllRast[[a]]
  
  # process remove layers from the SSP1 - 2.6 scenario
  RastB <- RastA[[5:8]]
  
  # remove unnecessary objects
  rm(a,RastA)
  
  # make layout
  par(mfrow = c(3,3))
  
  # compare layers
  map <- RastB[[1]] == RastB[[2]] 
  plot(map, main = "2015 vs 2040");gc() 
  map <- RastB[[1]] == RastB[[3]]
  plot(map, main = "2015 vs 2070");gc() 
  map <- RastB[[1]] == RastB[[4]]
  plot(map, main = "2015 vs 2100");gc()
  plot(1,xaxt = "n", yaxt = "n",
       axes=FALSE, frame.plot=F,
       type = "n", xlab = "", 
       ylab = "", xlim = c(0, 2),  
       ylim = c(0, 2))
  map <- RastB[[2]] == RastB[[3]]
  plot(map, main = "2040 vs 2070");gc()
  map <- RastB[[2]] == RastB[[4]]
  plot(map, main = "2040 vs 2100");gc()
  plot(1,xaxt = "n", yaxt = "n",
       axes=FALSE, frame.plot=F,
       type = "n", xlab = "", 
       ylab = "", xlim = c(0, 2),  
       ylim = c(0, 2))
  plot(1,xaxt = "n", yaxt = "n",
       axes=FALSE, frame.plot=F,
       type = "n", xlab = "", 
       ylab = "", xlim = c(0, 2),  
       ylim = c(0, 2))
  map <- RastB[[3]] == RastB[[4]]
  plot(map, main = "2070 vs 2100");gc()
  mtext(paste("       ",usources[i]), side=1, adj = 0,
        padj = 1, outer=T, line=-5)
  # removing unnecessary objects
  rm(RastB,map,i);gc()
}
rm(AllRast,sources,usources)