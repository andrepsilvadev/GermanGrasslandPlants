
###########################################################
####                                                   ####
####   Script 0 - Installing and loading packages      ####
####   Date: August 7th, 2025                          ####
####   Author: Afonso Barrocal                         ####
####                                                   ####
###########################################################

#### Step 1 - install/load "easypackages" package ####

# create logical condition
condition <- "easypackages" %in% installed.packages()[,"Package"]

# install/load package
if(!(condition)){
  install.packages("easypackages",
                   quiet = TRUE)
  library("easypackages",
          quietly = TRUE)
} else {
  library("easypackages",
          quietly = TRUE)
}

# remove unnecessary objects
rm(condition)
invisible(gc())

#### Step 2 - install all necessary packages ####

# load packages
easypackages::packages(
  "tidyverse",
  "readr",
  "data.table",
  "tidyr",
  "fuzzySim",
  "ncdf4",
  "raster",
  "rgdal",
  "terra",
  "stringr",
  "sf",
  "colorspace",
  "rnaturalearth",
  "svMisc",
  "dplyr",
  "predicts",
  "biomod2",
  prompt = TRUE
)

#### Step 3 - install packages from GitHub ####

# create vector with wanted packages
pkg <- "ecoregions"

# create dummy vector
condition <- rep(NA,length(pkg))

# create loop to create logical condition
for(i in seq_along(pkg)){
  # create logical condition
  condition[i] <- pkg[i] %in% installed.packages()[,"Package"]
  # process vector (turn logical value into numeric value)
  condition[i] <- as.numeric(condition[i])
  # remove i-value
  rm(i)
  invisible(gc())
}

# process data (turn condition and package vectors into numeric values)
condition <- sum(condition, na.rm = TRUE)

# install/load package
if(condition == length(pkg)){
  library("ecoregions", quietly = TRUE)
} else {
  devtools::install_github("tomroh/ecoregions", quiet = TRUE)
  1
  library("ecoregions", quietly = TRUE)
}

# remove unnecessary objects
rm(condition,pkg)
invisible(gc())

#### Step 4 - Create functions ####

# create function to extract historical monthly data from CHELSA
chelsa4r <- function(var,year,method = "mean"){
  # extract wanted URLs
  urls <- c(
    paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/",var,"/CHELSA_",var,"_0",1:9,"_",year,"_V.2.1.tif"),
    paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/",var,"/CHELSA_",var,"_",10:12,"_",year,"_V.2.1.tif")
  )
  # load required package
  require("terra")
  # download wanted layers
  if(method == "mean"){
    layers <- mean(rast(urls), na.rm = TRUE)
    invisible(gc())
  } else {
    if(method == "max"){
      layers <- max(rast(urls), na.rm = TRUE)
      invisible(gc())
    } else {
      if(method == "min"){
        layers <- min(rast(urls), na.rm = TRUE)
        invisible(gc())
      } else {
        if(method == "full"){
          layers <- rast(urls)
        } else {
          cat("Error: 'method' not valid.")
          stop()
        }
      }
    }
  }
  # process raster (names of layers)
  names(layers) <- paste0(var,"_",year,"_",method)
  longnames(layers) <- paste0(var,"_",year,"_",method)
  # process rasters (correct variable units)
  if(var == "pr"){
    # correct precipitation units
    layers <- layers/100
  } else {
    # correct temperature units
    layers <- (layers/10)-273.15
  }
  # return raster
  return(layers)
  ####
  ## var can have the values "pr", "tas", "tasmax", and "tasmin", as well, as the other values available in the CHELSA database ##
  ## values available from 1979 until 2019 ##
  ####
}

# create function to extract average across 30-year CHELSA
chelsa4r30 <- function(var,period,scenario){
  if(period == "1981-2010"){
    # create vector with URLs
    urls <- c(paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_",var,"_1981-2010_V.2.1.tif"))
    # load required package
    require("terra")
    # download raster files
    rasters <- rast(urls)
    invisible(gc())
    # process raster
    names(rasters) <- paste0(var,"_",period)
    longnames(rasters) <- paste0(var,"_",period)
    # return raster
    return(rasters)
  } else {
    # forbidden arguments
    if(!(var %in% c(paste0("bio",1:19)))){
      stop("Error: Chosen variable not available in the CHELSA database.")
    } else {
      if(!(period %in% c("2011-2040","2041-2070","2071-2100"))){
        stop("Error: Time period unavailable.")
      } else {
        if(!(scenario %in% c("126","370","585"))){
          stop("Error: Chosen scenario not available in the CHELSA database.")
        }
      }
    }
  }
  # create vector with all models
  models <- c("gfdl-esm4","ipsl-cm6a-lr","mpi-esm1-2-hr","mri-esm2-0","ukesm1-0-ll")
  # create vector with all URLs needed
  urls <- c(paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/",period,"/",toupper(models),"/ssp",scenario,"/bio/CHELSA_",var,"_",period,"_",models,"_ssp",scenario,"_V.2.1.tif"))
  # load required package
  require("terra")
  # download raster files
  rasters <- mean(rast(urls), na.rm = TRUE)
  invisible(gc())
  # process raster
  names(rasters) <- paste0(var,"_",period,"_",scenario)
  longnames(rasters) <- paste0(var,"_",period,"_",scenario)
  # return raster
  return(rasters)
}

# create function to extract NetCDF files (adapted for this project)
nc2tif <- function(path){
  # create function to extract NetCDF files
  ncdf2rast <- function(i,nc){
    # load packages
    require(ncdf4) # package for netcdf manipulation
    require(raster) # package for raster manipulation
    require(rgdal) # package for geospatial analysis
    # open NetCDF file
    nc_data <- nc_open(paste0(nc,".nc"))
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
    # return raster
    return(r)
  }
  # load package
  require(stringr)
  # process string
  path <- str_sub(path, start = 1, end = str_length(path)-3)
  # convert NetCDF to raster format
  GEOTIF <- do.call(c, lapply(1:33,ncdf2rast,nc = path))
  invisible(gc())
  # return raster file
  return(GEOTIF)
}

# mode function
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# alarm clock function
clockifyR <- function(...){
  # take starting time
  start <- Sys.time()
  # run arguments
  obj <- list(...)
  obj
  # load required packages
  require("beepr")
  # make "beep" to signal conclusion
  beep()
  # taking end time
  end <- Sys.time()
  # print final message
  print(end-start)
}
