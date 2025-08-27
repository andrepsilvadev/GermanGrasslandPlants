
#################################################
####                                         ####
####   Script 3 - Create background data     ####
####   Date: July 8th, 2025                  ####
####   Author: Afonso Barrocal               ####
####                                         ####
#################################################

#### Step 0 - Run previous scripts ####

# run library script and load data
source("./1.4.0_libraries.R")
load("./1.4.2_occurrence_processing.RData")

#### Step 1 - Import and extract ecoregion data ####

## Step 1a: Import ecoregion data ##

# load world's ecorregions (Olson et al., 2001; Dinerstein et al., 2017)
data("worldecoregions")

# process data (extract geometry and ecoregion names)
ecoregions <- unique(as.data.frame(worldecoregions)[,c("eco_name","geometry")])

# remove unnecessary objects
rm(worldecoregions)
invisible(gc())

# process data (turn data frame object into sf object)
ecoregions <- st_as_sf(ecoregions)

# process data (turn sf object into SpatVector object)
ecoregions2 <- vect(ecoregions)

## Step 1b: Extract ecoregion data ##

# extract data (species names)
species_regions <- data.frame(`species` = ocr$species)

# extract data (ecoregion data)
species_regions$ecoregions <- terra::extract(ecoregions2, ocr[,c("x","y")])[,2]

# remove unnecessary objects
rm(ecoregions2)
invisible(gc())

# process data (check for unique combinations)
species_regions <- unique(species_regions) %>% drop_na()

#### Step 3 - Import and process (year-one) land use data ####

# create path to year one land use files
paths <- paste0("./../data/landuse/ssp",c(1,5),"_rcp",c(26,85),"/GCAM_Demeter_LU_ssp",c(1,5),"_rcp",c(26,85),"_modelmean_2015_full.tif")

# create dummy list
landuse_data <- list()

# create loop to process data (extract land use data)
for(i in seq_along(paths)){
  # import raster
  landuse_data[[i]] <- rast(paths[i])
  # process data (change layers' names)
  names(landuse_data[[i]]) <- c("Water","NET_tem","NET_bor","NDT_bor","BET_tro",
                                "BET_tem","BDT_tro","BDT_tem","BDT_bor",
                                "BES_tem","BDS_tem","BDS_bor","C3_gra_arc",
                                "C3_gra","C4_gra","Corn_rf","Corn_irr",
                                "Wheat_rf","Wheat_irr","Soy_rf","Soy_irr",
                                "Cotton_rf","Cotton_irr","Rice_rf","Rice_irr",
                                "Sugarcrop_rf","Sugarcrop_irr","OtherCrop_rf",
                                "OtherCrop_irr","Bioenergy_rf","Bioenergy_irr",
                                "Urban","Barren")
  # print progress
  svMisc::progress(i,2, progress.bar = TRUE, char = "=")
  # remove unnecessary objects
  rm(i)
  invisible(gc())
}

# remove unnecessary objects
rm(paths)
invisible(gc())

# process data (calculate mean year-one land use)
landuse_data <- mean(landuse_data[[1]],landuse_data[[2]])

#### Step 4 - Generate background data for all species ####

# create dummy list
bgSpecies <- list(landuse_data,landuse_data,landuse_data,
                  landuse_data,landuse_data,landuse_data,
                  landuse_data,landuse_data)

# create species vector
species <- unique(species_regions$species)

# create loop to create background for each species
for(i in 1:length(species)){
  # extract species' ecoregions
  regions <- species_regions[species_regions$species == species[i],][,2]
  # select species' ecoregions
  ecoSp <- ecoregions[ecoregions$eco_name %in% regions,]
  # process data (unite all features and make features valid)
  ecoSp <- st_union(st_make_valid(ecoSp))
  # process data (turn sf object into SpatVector object)
  ecoSp <- vect(ecoSp)
  # crop and mask by ecoregions
  bgSpecies[[i]] <- mask(crop(landuse_data,ext(ecoSp)),ecoSp)
  # plot
  plot(bgSpecies[[i]][[1]], main = species[i])
  plot(vect(ne_countries()), add = TRUE, lwd = 0.45)
  # make progress bar
  svMisc::progress(i,length(species))
  # remove unnecessary objects
  rm(regions,ecoSp,i)
  invisible(gc())
}

# remove unnecessary objects
rm(species,species_regions,ecoregions,landuse_data)
invisible(gc())
