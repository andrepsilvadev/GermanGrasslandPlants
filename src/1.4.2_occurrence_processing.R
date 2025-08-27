
#################################################
####                                         ####
####   Script 2 - Processing occurrences     ####
####   Date: August 6th, 2025                ####
####   Author: Afonso Barrocal               ####
####                                         ####
#################################################

#### Step 0 - Run previous scripts ####

# run library script
source("./1.4.0_libraries.R")

#### Step 1 - Import occurrence data ####

# create list
ocr <- list.files(path = "./../data/Occurrences",
                  pattern = ".csv$",
                  full.names = TRUE)

# import data
ocr <- lapply(ocr,fread,quote="")
invisible(gc())

#### Step 2 - Process data ####

# create dummy list
clim <- list()
lu <- list()
all <- list()

# process data
for(i in seq_along(ocr)){
  # the land use baseline is set to the year 2015
  lu[[i]] <- ocr[[i]] %>%
    tidyr::drop_na(year) %>%
    filter(year >= 2014 & year <= 2016)
  # create presence column (necessary for grdding)
  lu[[i]]$presence <- 1
  # climate data only available from 1980 to 2010
  clim[[i]] <- ocr[[i]] %>%
    tidyr::drop_na(year) %>%
    filter(year >= 1981 & year <= 2010)
  # create presence column (necessary for grdding)
  clim[[i]]$presence <- 1
  # all the data to map
  all[[i]] <- ocr[[i]] 
  # process data (add presence column)
  all[[i]]$presence <- 1
  # process data (extract needed columns)
  all[[i]] <- all[[i]] %>% dplyr::select(species,decimalLongitude,decimalLatitude,presence) %>% drop_na() %>% distinct()
  # print progress
  svMisc::progress(i,8, progress.bar = TRUE, char = "=")
  # remove i-value
  rm(i)
  invisible(gc())
}

# remove unnecessary objects
rm(ocr)
invisible(gc())

#### Step 3 - Extract climate data ####

# download climate data
clim_data <- c(chelsa4r30(var = "bio1", period = "1981-2010"),
               chelsa4r30(var = "bio12", period = "1981-2010"))

# process data (change layers' names)
names(clim_data) <- c("tas","pr")

# create loop to grid records
for(i in seq_along(clim)){
  # extract species names
  species <- unique(clim[[i]]$species)
  # grid records
  clim[[i]] <- gridRecords(clim_data,
                           clim[[i]][clim[[i]]$presence == 1, c(22,23)],
                           clim[[i]][clim[[i]]$presence == 0, c(22,23)])
  # create species column
  clim[[i]]$species <- species
  # print progress
  svMisc::progress(i,8, progress.bar = TRUE, char = "=")
  # remove unnecessary objects
  rm(species,i)
  invisible(gc())
}

# remove unnecessary objects
rm(clim_data)
invisible(gc())

# create dummy list
climate <- list()

# create loop to process data (extract climate data)
for(i in seq_along(clim)){
  # extract climate statistics
  climate[[i]] <- data.frame(`species` = unique(clim[[i]]$species),
                             `upper_limit_temperature` = max(clim[[i]]$tas, na.rm = T),
                             `upper_limit_precipitation` = max(clim[[i]]$pr, na.rm = T),
                             `lower_limit_temperature` = min(clim[[i]]$tas, na.rm = T),
                             `lower_limit_precipitation` = min(clim[[i]]$pr, na.rm = T),
                             `sd_temperature` = sd(clim[[i]]$tas, na.rm = T),
                             `sd_precipitation` = sd(clim[[i]]$pr, na.rm = T),
                             `mode_temperature` = mode(clim[[i]]$tas),
                             `mode_precipitation` = mode(clim[[i]]$pr),
                             `mean_temperature` = mean(clim[[i]]$tas, na.rm = T),
                             `mean_precipitation` = mean(clim[[i]]$pr, na.rm = T))
  # print progress
  svMisc::progress(i,8, progress.bar = TRUE, char = "=")
  # remove unnecessary objects
  rm(i)
  invisible(gc())
}

# remove unnecessary objects
rm(clim)
invisible(gc())

# combine all into a data frame
climate <- do.call(rbind,climate)

#### Step 4 - Create species input files ####

# import trait data
traits <- fread("./../data/trait_data.csv")

# process data (select target species)
traits <- traits[traits$species %in% climate$species,]

# process data (select target traits)
traits <- traits %>%
  dplyr::select("species","density_sqMeter_mean","wetmass_g_mean",
                "mortality_mean","mean_dist_m_mean","max_dist_m_mean",
                "lambdaVals_mean","density_sqMeter_std","wetmass_g_std",
                "mortality_std","lambdaVals_std")

# process data (change column names)
colnames(traits) <- c("species_name","carry","mass","bevmort",
                      "mean_dispersal_dist","max_dispersal_dist",
                      "growrate","sd_carry","sd_mass","sd_bevmort",
                      "sd_growrate")

# process climate data
climate <- climate %>%
  dplyr::select("species","upper_limit_temperature",
                "upper_limit_precipitation","lower_limit_temperature",
                "lower_limit_precipitation","mean_temperature",
                "mean_precipitation")

# process climate data (change first column name)
colnames(climate)[c(1,6,7)] <- c("species_name",
                                 "optimum_temperature",
                                 "optimum_precipitation")

# combine the two datasets
climate_traits <- traits %>%
  left_join(climate, by = "species_name")

# remove unnecessary objects
rm(climate,traits)
invisible(gc())

# process data (turn into data frame object)
climate_traits <- as.data.frame(climate_traits)

# process data (turn into desirable format)
climate_traits$species_name <- sub(" ", "_", climate_traits$species_name)
climate_traits$species_name <- tolower(as.character(climate_traits$species_name))
climate_traits$species_name <- as.factor(climate_traits$species_name)

# create new columns ("cut-off value" and "allee effect")
climate_traits$habitat_cutoff_suitability <- rep(0.01,nrow(climate_traits))
climate_traits$allee <- rep(0,nrow(climate_traits))
climate_traits$sd_allee <- rep(0,nrow(climate_traits))

# process data (order columns)
climate_traits <- climate_traits %>%
  dplyr::select(species_name,mass,sd_mass,growrate,
                sd_growrate,max_dispersal_dist,
                mean_dispersal_dist,allee,sd_allee,
                bevmort,sd_bevmort,carry,sd_carry,
                upper_limit_temperature,
                lower_limit_temperature,
                upper_limit_precipitation,
                lower_limit_precipitation,
                habitat_cutoff_suitability,
                optimum_temperature,
                optimum_precipitation)

# process data (turn columns' classes to character)
climate_traits <- sapply(climate_traits,FUN = as.character)

# process data (make columns' names the first row)
climate_traits <- rbind(colnames(climate_traits),
                        climate_traits)

# process data (transpose data)
climate_traits <- t(climate_traits)

# process data (turn data frame into data table)
climate_traits <- as.data.table(climate_traits)

# process data (turn data table into data frame)
climate_traits <- as.data.frame(climate_traits)

# create loop to export species input files
for(i in seq_along(climate_traits[,-1])){
  # extract subset "i"
  df <- climate_traits[,c(1,1+i)]
  # extract species name
  species <- df[1,2]
  # process data (change first and second column's name)
  colnames(df) <- c("Argument","Value")
  # export data
  write.table(df,paste0("./../input/",species,"/126/climate + suitability/species/",species,".csv"),
              col.names = T, row.names = F, sep = " ",
              quote = FALSE)
  write.table(df,paste0("./../input/",species,"/585/climate + suitability/species/",species,".csv"),
              col.names = T, row.names = F, sep = " ",
              quote = FALSE)
  write.table(df,paste0("./../input/",species,"/126/climate/species/",species,".csv"),
              col.names = T, row.names = F, sep = " ",
              quote = FALSE)
  write.table(df,paste0("./../input/",species,"/585/climate/species/",species,".csv"),
              col.names = T, row.names = F, sep = " ",
              quote = FALSE)
  write.table(df,paste0("./../input/",species,"/126/suitability/species/",species,".csv"),
              col.names = T, row.names = F, sep = " ",
              quote = FALSE)
  write.table(df,paste0("./../input/",species,"/585/suitability/species/",species,".csv"),
              col.names = T, row.names = F, sep = " ",
              quote = FALSE)
  # check progress
  svMisc::progress(i,9, progress.bar = TRUE, char = "=")
  # remove unnecessary objects
  rm(species,df,i)
  invisible(gc())
}

# remove unnecessary objects
rm(climate_traits)
invisible(gc())

#### Step 5 - Grid records ####

## Step 5a: Grid records ##

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

# create loop to process data (grid records)
for(i in seq_along(lu)){
  # extract species names
  species <- unique(lu[[i]]$species)
  # grid records
  lu[[i]] <- gridRecords(landuse_data[[1]],
                           lu[[i]][lu[[i]]$presence == 1, c("decimalLongitude","decimalLatitude")],
                           lu[[i]][lu[[i]]$presence == 0, c("decimalLongitude","decimalLatitude")])
  # remove landuse columns
  lu[[i]] <- lu[[i]][,-c(4:37)]
  # create species column
  lu[[i]]$species <- species
  # create time range column
  lu[[i]]$time <- "2014-2016"
  # process data (change columns name)
  colnames(lu[[i]])[ncol(lu[[i]])] <- "Time Range"
  # grid records
  all[[i]] <- gridRecords(landuse_data[[1]],
                         all[[i]][all[[i]]$presence == 1, c("decimalLongitude","decimalLatitude")],
                         all[[i]][all[[i]]$presence == 0, c("decimalLongitude","decimalLatitude")])
  # remove landuse columns
  all[[i]] <- all[[i]][,-c(4:37)]
  # create species column
  all[[i]]$species <- species
  # create time range column
  all[[i]]$time <- "1981-2020"
  # process data (change columns name)
  colnames(all[[i]])[ncol(all[[i]])] <- "Time Range"
  # print progress
  svMisc::progress(i,8, progress.bar = TRUE, char = "=")
  # remove i-value
  rm(species,i)
  invisible(gc())
}

# remove unnecessary objects
rm(chelsa4r30,nc2tif,
   mode,chelsa4r)

# process data (combine into a data frame)
lu <- do.call(rbind,lu)
all <- do.call(rbind,all)

# process data
ocr <- rbind(lu,all)

# remove unnecessary objects
rm(lu,all)
invisible(gc())

## Step 5b: Plot gridded records ##

# load world's shapefile
world <- ne_countries(scale = 10)

# plot occurrences for all species
ocr_plot <- ggplot() +
  geom_sf(data = world,
          fill = "white",
          color = "black",
          linewidth = .1) +
  geom_point(data = ocr[ocr$`Time Range` == "1981-2020",],
             aes(x = x, y = y,
                 col = `Time Range`),
             size = .00001,
             alpha = .15) +
  geom_point(data = ocr[ocr$`Time Range` == "2014-2016",],
             aes(x = x, y = y,
                 col = `Time Range`),
             size = .00001) +
  facet_wrap(species~.) +
  scale_color_manual(values = c("1981-2020" = "maroon1",
                                "2014-2016" = "#E9002D")) +
  labs(x = "Longitude", y = "Latitude", caption = "CRS: WGS84") +
  coord_sf(expand = FALSE) +
  theme_bw()
ocr_plot <- ocr_plot + theme(strip.text = element_text(face = "italic"))

# export plot
ggsave(plot = ocr_plot, file = "./../output/supplementary/occurrences_gridded_map.png",
       bg = "white", height = 240-24*3, width = 480-48*3, units = "mm", dpi = 1200)

# create species vector
species <- unique(ocr$species)

# loop to create plots for each species
for(i in seq_along(species)){
  # subsample by species
  spSub <- ocr[ocr$species == species[i],]
  # plot occurrences
  ocr_plot <- ggplot() +
    geom_sf(data = world,
            fill = "white",
            color = "black",
            linewidth = .1) +
    geom_point(data = spSub[spSub$`Time Range` == "1981-2020",],
               aes(x = x, y = y,
                   col = `Time Range`),
               size = .00001) +
    geom_point(data = spSub[spSub$`Time Range` == "2014-2016",],
               aes(x = x, y = y,
                   col = `Time Range`),
               size = .00001) +
    facet_wrap(species~.) +
    scale_color_manual(values = c("1981-2020" = "salmon",
                                  "2014-2016" = "#E9002D")) +
    coord_sf(expand = FALSE) +
    labs(x = "Longitude", y = "Latitude", caption = "CRS: WGS84") +
    theme_bw()
  ocr_plot <- ocr_plot + theme(strip.text = element_text(face = "italic"))
  # export plot
  ggsave(plot = ocr_plot, file = paste0("./../output/supplementary/",sub(pattern = " ", replacement = "_", x = species[i]),"_map.png"),
         bg = "white", height = 240-24*3, width = 480-48*3, units = "mm", dpi = 1200)
  # remove unnecessary objects
  rm(spSub,i)
  invisible(gc())
}

#### Step 6 - Save objects ####

# process data
ocr <- ocr[ocr$`Time Range` == "2014-2016",] %>% dplyr::select(species,presence,x,y)

# save environment
save(ocr, file = "./1.4.2_occurrence_processing.RData")

# remove unnecessary objects
rm(ocr,ocr_plot,species,world,clockifyR)
invisible(gc())

#### Step 7 - Check number of gridded occurrences per year ####

# create list
ocr <- list.files(path = "./../data/Occurrences",
                  pattern = ".csv$",
                  full.names = TRUE)

# import data
ocr <- lapply(ocr,fread,quote="")
invisible(gc())

# create dummy list
dt <- list()

# create loop to process data
for(i in seq_along(ocr)){
  # process data
  ocr[[i]] <- ocr[[i]] %>% dplyr::select(species,year,decimalLongitude,decimalLatitude) %>% distinct()
  # subset data
  case <- ocr[[i]]
  # extract species data
  sp <- unique(case$species)
  # extract year data
  year <- unique(case$year)
  # process data (add presence column)
  case$presence <- 1
  # create dummy list
  spFile <- list()
  # create loop to process data
  for(x in seq_along(year)){
    # process data (filter by year)
    spCase <- case[case$year == year[x],]
    # process data (grid records)
    spFile[[x]] <- gridRecords(landuse_data[[1]],
                               spCase[spCase$presence == 1, c("decimalLongitude","decimalLatitude")],
                               spCase[spCase$presence == 0, c("decimalLongitude","decimalLatitude")])
    # process data (add species and year columns)
    spFile[[x]]$species <- sp
    spFile[[x]]$year <- year[x]
    # create dummy vector
    size <- as.numeric(length(year))
    # progress bar
    svMisc::progress(size*(i-1)+x,8*size)
    # remove unnecessary objects
    rm(spCase,size,x)
    invisible(gc())
  }
  # process data (turn list into data frame)
  case <- do.call(rbind,spFile)
  # process data (count presences by year)
  dt[[i]] <- case %>% group_by(species,year) %>% summarise(count = sum(presence, na.rm = TRUE))
  # remove unnecessary objects
  rm(case,spFile,sp,year,i)
  invisible(gc())
}

# process data (turn list into single data frame)
dt <- do.call(rbind,dt)

# remove unnecessary objects
rm(ocr,landuse_data)
invisible(gc())

# plot gridded occurrences across time
counts_plot <- ggplot(data = dt) +
  geom_col(aes(x = year, y = count),
           fill = "green4", color = NA) +
  labs(x = "Year", y = "No. of Gridded Occurrences") +
  facet_wrap(species~., scale = "free_y") +
  theme_bw()
counts_plot <- counts_plot + theme(strip.text = element_text(face = "italic"))

# export plot
ggsave(plot = counts_plot, file = "./../output/supplementary/occurrences_gridded_counts.png",
       bg = "white", height = 240-24*3, width = 480-48*3, units = "mm", dpi = 1200)

# remove unnecessary objects
rm(counts_plot,dt)
invisible(gc())
