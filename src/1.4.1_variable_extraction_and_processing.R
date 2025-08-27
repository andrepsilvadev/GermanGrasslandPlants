
################################################################
####                                                        ####
####   Script 1 - Extracting and processing variables       ####
####   Date: June 26th, 2025                                ####
####   Author: Afonso Barrocal                              ####
####                                                        ####
################################################################

#### Step 0 - Run previous scripts ####

# run library script
source("./1.4.0_libraries.R")

#### Step 1 - Unzip Occurrence Data ####

## Step 1a: unzip all data ##

# create vector
zipped_files <- list.files(path = "./../data/zipped_data",
                           pattern = ".zip$",
                           full.names = TRUE)

# create new directory
dir.create("./../data/unzipped_data")

# unzip files
walk(zipped_files, unzip, exdir = "./../data/unzipped_data")

# remove unnecessary objects
rm(zipped_files)
invisible(gc())

## Step 1b: unzip occurrence data ##

# create vector
zipped_files <- list.files(path = "./../data/unzipped_data/Occurrences-GBIF-2025-08",
                           pattern = ".zip$",
                           full.names = TRUE)

# create new directory
dir.create("./../data/Occurrences")

# unzip files
walk(zipped_files, unzip, exdir = "./../data/Occurrences")

# remove unnecessary objects
rm(zipped_files)
invisible(gc())

#### Step 2 - Create directories ####

# create vector with species
species <- c("achillea_millefolium","plantago_lanceolata",
             "rumex_acetosa","veronica_chamaedrys",
             "poa_pratensis","poa_trivialis",
             "ranunculus_acris","ranunculus_bulbosus")

# create vector with scenarios
scenarios <- c("126","585")

# create vector with model types
models <- c("climate","climate + suitability","suitability")

# create vector with input files folders
input <- c("species","environment","suitability")

# create loop to create folders
for(i in seq_along(species)){
  # create species directory
  dir.create(paste0("./../input/",species[i]))
  # create another loop to create folders
  for(z in seq_along(scenarios)){
    # create scenario directory
    dir.create(paste0("./../input/",species[i],"/",scenarios[z]))
    # create another loop to create folders
    for(a in seq_along(models)){
      # create different models/experiments folders
      dir.create(paste0("./../input/",species[i],"/",scenarios[z],"/",models[a]))
      # create another loop to create folders
      for(b in seq_along(input)){
        # create different input directories
        dir.create(paste0("./../input/",species[i],"/",scenarios[z],"/",models[a],"/",input[b]))
        # create folders conditionally
        if(input[b] == "environment"){
          # create different input directories
          dir.create(paste0("./../input/",species[i],"/",scenarios[z],"/",models[a],"/",input[b],"/temperature"))
          dir.create(paste0("./../input/",species[i],"/",scenarios[z],"/",models[a],"/",input[b],"/precipitation"))
          # remove b-value
          rm(b)
          invisible(gc())
        } else {
          # remove b-value
          rm(b)
          invisible(gc())
        }
      }
      # remove a-value
      rm(a)
      invisible(gc())
    }
    # remove z-value
    rm(z)
    invisible(gc())
  }
  # remove i-value
  rm(i)
  invisible(gc())
}

# remove unnecessary objects
rm(species,scenarios,models,input)
invisible(gc())

#### Step 3 - Process land-use data ####

## Step 3a: Import data

# import paths
landuse_list <- append(list.files("./../data/landuse/ssp1_rcp26", pattern = "nc", full.names=T),
                       list.files("./../data/landuse/ssp5_rcp85", pattern = "nc", full.names=T))

# import study area's shapefile
shape <- read_sf("./../data/unzipped_data/Spatial/Bav&BW.shp")

# process study area's shape file (turn sf to SpatVector)
shape <- vect(st_union(shape))

# import world map
world <- ne_countries(scale = 10)

# process data (take Antarctica's out)
world <- world[world$sovereignt != "Antarctica",]

# process world's shapefile (turn sf to SpatVector)
world <- vect(world)

## Step 3b: Process year-one data ##

# process paths (only year-one data)
year_one <- landuse_list[c(1,19)]

# create loop for year-one land use data
for(i in seq_along(year_one)){
  # import land use file
  luFile <- nc2tif(year_one[i])
  # process land use file (create 0s)
  luFile[is.na(luFile)] <- 0
  # process land-use file (make CRS compatible)
  crs(luFile) <- crs(world)
  # process land use file (crop and mask by world shape)
  luFile <- mask(crop(luFile, ext(world)),world)
  # export data
  writeRaster(x = luFile,
              filename = paste0(substr(year_one[i],1,nchar(year_one[i])-3),"_full.tif"),
              overwrite = TRUE)
  # process land-use file (make CRS compatible)
  crs(luFile) <- crs(shape)
  # process land use file (crop and mask by study area's shape)
  luFile <- mask(crop(luFile, ext(shape)),shape)
  # export data
  writeRaster(x = luFile,
              filename = paste0(substr(year_one[i],1,nchar(year_one[i])-3),".tif"),
              overwrite = TRUE)
}

# remove unnecessary objects
rm(world,luFile,i,year_one)
invisible(gc())

## Step 3c: Process all other years ##

# process paths (all but the year-one data)
landuse_list <- landuse_list[-c(1,19)]

# create loop for year-one land use data
for(i in seq_along(landuse_list)){
  # import land use file
  luFile <- nc2tif(landuse_list[i])
  # process land use file (create 0s)
  luFile[is.na(luFile)] <- 0
  # process land-use file (make CRS compatible)
  crs(luFile) <- crs(shape)
  # process land use file (crop and mask by study area's shape)
  luFile <- mask(crop(luFile, ext(shape)),shape)
  # export data
  writeRaster(x = luFile,
              filename = paste0(substr(landuse_list[i],1,nchar(landuse_list[i])-3),".tif"),
              overwrite = TRUE)
}

# remove unnecessary objects
rm(luFile,i,landuse_list)
invisible(gc())

#### Step 4 - Download climate data ####

## Step 4a: Import sample file

# import data
sample <- nc2tif("./../data/landuse/ssp1_rcp26/GCAM_Demeter_LU_ssp1_rcp26_modelmean_2015.nc")

# process land-use data (make CRS compatible)
crs(sample) <- crs(shape)

# process land-use data (make extent compatible)
sample <- crop(sample, ext(shape))

# process land-use data (make extent compatible)
sample <- mask(sample, shape, touches = TRUE)

## Step 4b: Year-One data

# download year-one climate data (2015 data)
year_one <- c(chelsa4r(var = "tas", year = 2015),
              chelsa4r(var = "pr", year = 2015))

# process year-one climate data (make CRS compatible)
crs(year_one) <- crs(sample)

# process lyear-one climate data (make extent compatible)
year_one <- crop(year_one, ext(shape))

# process year-one climate data (make extent compatible)
year_one <- mask(year_one, shape, touches = TRUE)

# process year-one climate data (resampling to land-use scale)
year_one <- resample(year_one, sample, method = "average")

# create vector with species
species <- c("achillea_millefolium","plantago_lanceolata",
             "rumex_acetosa","veronica_chamaedrys",
             "poa_pratensis","poa_trivialis",
             "ranunculus_acris","ranunculus_bulbosus")

# create vector with scenarios
scenarios <- c("126","585")

# create vector with model types
models <- c("climate","climate + suitability","suitability")

# export climate data (year-one)
for(i in seq_along(species)){
  for(z in seq_along(scenarios)){
    for(a in seq_along(models)){
      if(models[a] != "suitability"){
        # now save as CSV in MetaRange format (temperature)
        metarange <- round(as.array(year_one[[1]]))
        metarange[which(is.na(metarange))] <- "NaN"
        # Julia needs to have grid cells without values filled with "NaN"
        write.table(metarange,
                    paste0("./../input/",species[i],"/",scenarios[z],"/",models[a],"/environment/temperature/temperature_1.csv"),
                    col.names = F, row.names = F)
        # now save as CSV in MetaRange format (precipitation)
        metarange <- round(as.array(year_one[[2]]))
        metarange[which(is.na(metarange))] <- "NaN"
        # Julia needs to have grid cells without values filled with "NaN"
        write.table(metarange,
                    paste0("./../input/",species[i],"/",scenarios[z],"/",models[a],"/environment/precipitation/precipitation_1.csv"),
                    col.names = F, row.names = F)
        # remove unnecessary objects
        rm(metarange)
        invisible(gc())
      } else {
        for(b in 1:87){
          # now save as CSV in MetaRange format (temperature)
          metarange <- round(as.array(year_one[[1]]))
          metarange[which(is.na(metarange))] <- "NaN"
          # Julia needs to have grid cells without values filled with "NaN"
          write.table(metarange,
                      paste0("./../input/",species[i],"/",scenarios[z],"/",models[a],"/environment/temperature/temperature_",b,".csv"),
                      col.names = F, row.names = F)
          # now save as CSV in MetaRange format (precipitation)
          metarange <- round(as.array(year_one[[2]]))
          metarange[which(is.na(metarange))] <- "NaN"
          # Julia needs to have grid cells without values filled with "NaN"
          write.table(metarange,
                      paste0("./../input/",species[i],"/",scenarios[z],"/",models[a],"/environment/precipitation/precipitation_",b,".csv"),
                      col.names = F, row.names = F)
          # remove unnecessary objects
          rm(metarange,b)
          invisible(gc())
        }
      }
      # remove a-value
      rm(a)
      invisible(gc())
    }
    # remove z-value
    rm(z)
    invisible(gc())
  }
  # remove i-value
  rm(i)
  invisible(gc())
}

# remove unnecessary objects
rm(species,scenarios,
   models,year_one)
invisible(gc())

## Step 4c: Time intervals data

# create dummy rasters
dummy <- rast(nrow = 20880,
              ncol = 43200,
              xmin = -180.0001,
              xmax = 179.9999,
              ymin = -90.00014,
              ymax = 83.99986)
crs(dummy)<- "epsg:4326"

# transfer 
pr_126 <- c(dummy,dummy,dummy)
pr_585 <- c(dummy,dummy,dummy)
tas_126 <- c(dummy,dummy,dummy)
tas_585 <- c(dummy,dummy,dummy)

# remove unnecessary objects
rm(dummy)
invisible(gc())

# create vector
intervals <- c("2011-2040","2041-2070","2071-2100")

# download data
for(i in seq_along(intervals)){
  # download data
  tas_126[[i]] <- chelsa4r30(var = "bio1", period = intervals[i],
                             scenario = 126)
  tas_585[[i]] <- chelsa4r30(var = "bio1", period = intervals[i],
                             scenario = 585)
  pr_126[[i]] <- chelsa4r30(var = "bio12", period = intervals[i],
                            scenario = 126)
  pr_585[[i]] <- chelsa4r30(var = "bio12", period = intervals[i],
                            scenario = 585)
  # process data (change layers names)
  names(tas_126[[i]]) <- paste0("tas_",intervals[i],"_126")
  names(tas_585[[i]]) <- paste0("tas_",intervals[i],"_585")
  names(pr_126[[i]]) <- paste0("pr_",intervals[i],"_126")
  names(pr_585[[i]]) <- paste0("pr_",intervals[i],"_585")
  # process data (change layers "longnames")
  longnames(tas_126[[i]]) <- paste0("tas_",intervals[i],"_126")
  longnames(tas_585[[i]]) <- paste0("tas_",intervals[i],"_585")
  longnames(pr_126[[i]]) <- paste0("pr_",intervals[i],"_126")
  longnames(pr_585[[i]]) <- paste0("pr_",intervals[i],"_585")
  # remove i-value
  rm(i)
}

# remove unnecessary objects
rm(intervals)
invisible(gc())

# process 30-year time climate data (make CRS compatible)
crs(tas_126) <- crs(sample)
crs(tas_585) <- crs(sample)
crs(pr_126) <- crs(sample)
crs(pr_585) <- crs(sample)

# process 30-year time climate data (make extent compatible)
tas_126 <- crop(tas_126, ext(shape))
tas_585 <- crop(tas_585, ext(shape))
pr_126 <- crop(pr_126, ext(shape))
pr_585 <- crop(pr_585, ext(shape))

# process 30-year time climate data (make extent compatible)
tas_126 <- mask(tas_126, shape, touches = TRUE)
tas_585 <- mask(tas_585, shape, touches = TRUE)
pr_126 <- mask(pr_126, shape, touches = TRUE)
pr_585 <- mask(pr_585, shape, touches = TRUE)

# process 30-year time climate data (resampling to land-use scale)
tas_126 <- resample(tas_126, sample, method = "average")
tas_585 <- resample(tas_585, sample, method = "average")
pr_126 <- resample(pr_126, sample, method = "average")
pr_585 <- resample(pr_585, sample, method = "average")

# create function to speed up the data exportation process
fedexR <- function(obj,scenario){
  # turn it into MetaRange.jl format
  metarange1 <- round(as.array(obj[[1]]))
  metarange2 <- round(as.array(obj[[2]]))
  metarange3 <- round(as.array(obj[[3]]))
  # change NAs
  metarange1[which(is.na(metarange1))] <- "NaN"
  metarange2[which(is.na(metarange2))] <- "NaN"
  metarange3[which(is.na(metarange3))] <- "NaN"
  # create vector with nicknames
  nick <- substr(names(obj),1,nchar(names(obj))-nchar("_2011-2040_126"))
  # process nickname vector
  if(nick[1] == "tas"){
    nick[1] <- "temperature"
  } else {
    nick[1] <- "precipitation"
  }
  # create vector with species
  species <- c("achillea_millefolium","plantago_lanceolata",
               "rumex_acetosa","veronica_chamaedrys",
               "poa_pratensis","poa_trivialis",
               "ranunculus_acris","ranunculus_bulbosus")
  # create vector with model types
  models <- c("climate","climate + suitability")
  # Julia needs to have grid cells without values filled with "NaN"
  for(z in seq_along(species)){
    for(y in seq_along(models)){
      # save files
      for(i in 2:27){
        write.table(metarange1,
                    paste0("./../input/",species[z],"/",scenario,"/",models[y],"/environment/",nick[1],"/",nick[1],"_",i,".csv"),
                    col.names = F, row.names = F)
      }
      for(i in 1:30){
        write.table(metarange2,
                    paste0("./../input/",species[z],"/",scenario,"/",models[y],"/environment/",nick[1],"/",nick[1],"_",i+27,".csv"),
                    col.names = F, row.names = F)
        write.table(metarange3,
                    paste0("./../input/",species[z],"/",scenario,"/",models[y],"/environment/",nick[1],"/",nick[1],"_",i+57,".csv"),
                    col.names = F, row.names = F)
      }
      # remove y-value
      rm(y)
      invisible(gc())
    }
    # remove z-value
    rm(z)
    invisible(gc())
  }
  # remove unnecessary objects
  rm(metarange1,metarange2,metarange3,nick,i,
     species,models)
  invisible(gc())
}

# export data
fedexR(tas_126,126)
fedexR(tas_585,585)
fedexR(pr_126,126)
fedexR(pr_585,585)

# remove unnecessary objects
rm(tas_126,tas_585,pr_126,pr_585,
   sample,shape,fedexR)
invisible(gc())

#### Step 5 - Make supplementary tables and plots ####

# run script
source("./1.4.1.1_supplementary_tables_and_plots.R")
