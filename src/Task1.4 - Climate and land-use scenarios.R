
##### Step 0 - Set Working Directory and Start Clock #####

# start clock
start_f <- Sys.time()

# create vector with directory path
wd <- "path"

# set working directory
setwd(wd)

# remove unnecessary objects
rm(wd)

##### Step 1 - CLIMATIC DATA #####

### import data

# load package
library("data.table")

# import data
Amillefolium <- fread("occ/Achillea_millefolium_occurrences/0032263-231120084113126.csv",
                      stringsAsFactors=TRUE)
invisible(gc()) # to empty the memory
Evaginatum <- fread("occ/Eriophorum_vaginatum_occurrences/0203268-240321170329656.csv",
                    stringsAsFactors=TRUE)
invisible(gc()) # to empty the memory
Fsylvatica <- fread("occ/Fagus_sylvatica_occurrences/0203271-240321170329656.csv",
                    stringsAsFactors=TRUE)
invisible(gc()) # to empty the memory
Planceolata <- fread("occ/Plantago_lanceolata_occurrences/0038643-231120084113126.csv",
                     stringsAsFactors=TRUE)
invisible(gc()) # to empty the memory
Ppratensis <- fread("occ/Poa_pratensis_occurrences/0203436-240321170329656.csv",
                    stringsAsFactors=TRUE)
invisible(gc()) # to empty the memory
Ptrivialis <- fread("occ/Poa_trivialis_occurrences/0203438-240321170329656.csv",
                    stringsAsFactors=TRUE)
invisible(gc()) # to empty the memory
Racris <- fread("occ/Ranunculus_acris_occurrences/0203502-240321170329656.csv",
                stringsAsFactors=TRUE)
invisible(gc()) # to empty the memory
Rbulbosus <- fread("occ/Ranunculus_bulbosus_occurrences/0203503-240321170329656.csv",
                   stringsAsFactors=TRUE)
invisible(gc()) # to empty the memory
Racetosa <- fread("occ/Rumex_acetosa_occurrences/0038676-231120084113126.csv",
                  stringsAsFactors=TRUE)
invisible(gc()) # to empty the memory
Vchamaedrys <- fread("occ/Veronica_chamaedrys_occurrences/0038695-231120084113126.csv",
                     stringsAsFactors=TRUE)
invisible(gc()) # to empty the memory
Varvensis <- fread("occ/Viola_arvensis_occurrences/0203505-240321170329656.csv",
                   stringsAsFactors=TRUE)
invisible(gc()) # to empty the memory

# creating list of species (los)
los <- list(Amillefolium,Evaginatum,Fsylvatica,Planceolata,Ppratensis,Ptrivialis,Racris,Rbulbosus,Racetosa,Vchamaedrys,Varvensis)

# removing heavy datasets
rm(Amillefolium,Evaginatum,Fsylvatica,Planceolata,Ppratensis,Ptrivialis,Racris,Rbulbosus,Racetosa,Vchamaedrys,Varvensis)
invisible(gc()) # to empty the memory

### extract climatic data

# environmental variable values function 
getEValues <- function(vector){
  
  # processing dataset
  points <- subset(vector, select = c("decimalLongitude","decimalLatitude"))
  colnames(points) <- c("x","y")
  
  # load required package
  require("terra", quietly = T)
  
  # import environmental variables raster (from the CHELSA database)
  pr <- rast("clim/1981_2010/bio12.tif") # annual precipitation
  invisible(gc()) # to empty the memory
  tc <- rast("clim/1981_2010/bio01.tif") # annual mean air temperature
  invisible(gc()) # to empty the memory
  
  # stacking raster
  EV <- c(tc,pr)
  invisible(gc()) # to empty the memory
  
  # extract values
  points <- as.data.frame(points)
  valuesatpoints <- extract(EV, points)
  invisible(gc()) # to empty the memory
  points <- cbind(points,valuesatpoints)
  colnames(points) <- c("lon","lat","ID","bio01","bio12")
  
  # mode function
  mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  # creating dataframe
  climate <- data.frame(`species` = unique(vector$species),
                        `upper_limit_temperature` = max(points$bio01, na.rm = T),
                        `upper_limit_precipitation` = max(points$bio12, na.rm = T),
                        `lower_limit_temperature` = min(points$bio01, na.rm = T),
                        `lower_limit_precipitation` = min(points$bio12, na.rm = T),
                        `sd_temperature` = sd(points$bio01, na.rm = T),
                        `sd_precipitation` = sd(points$bio12, na.rm = T),
                        `optimum_temperature` = mode(points$bio01),
                        `optimum_precipitation` = mode(points$bio12),
                        `mean_temperature` = mean(points$bio01, na.rm = T),
                        `mean_precipitation` = mean(points$bio12, na.rm = T))
  invisible(gc())
  return(climate)
}

# run the list of species through the function and combine the result into a single dataframe
dataset_climate <- as.data.frame(do.call(rbind, lapply(los,getEValues)))
invisible(gc()) # to empty the memory

# see resulting dataframe
rmarkdown::paged_table(dataset_climate)

# export climatic information
write.csv(dataset_climate, "climate.csv",
          row.names = FALSE)

# remove unnecessary objects
rm(dataset_climate,getEValues)
invisible(gc())

### create species files

# load package
library("data.table")

# import trait data
trait_data <- fread("trait_data.csv")
invisible(gc())

# remove rownames
trait_data <- trait_data[,-c(1,3,4,11,12,16,17)]
invisible(gc())

# creating vector with correct argument names
names <- c("species_name","carry","mass","bevmort",
           "mean_dispersal_dist","max_dispersal_dist",
           "growrate","sd_carry","sd_mass","sd_bevmort",
           "sd_growrate")

# processing trait data (changing names)
colnames(trait_data) <- names

# remove unnecessary objects
rm(names)
invisible(gc())

# import climatic data
climate <- read.csv("climate.csv", stringsAsFactors=TRUE)
invisible(gc())

# processing climate data
climate <- climate[,-c(6,7,10,11)]

# processing climate data (change first collumn name)
colnames(climate) <- c("species_name",colnames(climate)[-1])

# merge trait and climatic data
data <- merge(trait_data,climate)
invisible(gc())

# remove unnecessary objects
rm(trait_data)
invisible(gc())

# change format to data frame
data <- as.data.frame(data)

# process merged data to the desirable format
data$species_name <- sub(" ", "_", data$species_name)

data$species_name <- tolower(as.character(data$species_name))

data$species_name <- as.factor(data$species_name)

# create new collumn (cut-off value)
data$habitat_cutoff_suitability <- rep(0.01,11)

# create new collumn (allee effect)
data$allee <- rep(0,11)
data$sd_allee <- rep(0,11)

# (re-)order data table
data <- data[,c(1,3,9,7,11,6,5,19,20,4,10,2,8,12,14,13,15,18,16,17)]

# create vector with collumn names
a <- colnames(data)

# load package
library("data.table")

# change format to data table
data <- data.table(data)

# transpose data table
dataT <- transpose(data)

# remove unnecessary objects
rm(data)
invisible(gc())

# create "Argument" collumn
dataT$Argument <- a

# (re-)order data table
dataT <- dataT[,c(12,1,2,3,4,5,6,7,8,9,10,11)]

# export a csv file with all the species
write.csv(dataT,"species_files.csv",row.names = F)

# import data for all the species
dataT <- read.csv("species_files.csv", stringsAsFactors=TRUE)

# create a loop to export input file for each species
for(i in seq_along(climate$species_name)){
  
  df <- dataT[,c(1,i+1)]
  
  colnames(df) <- c("Argument","Value")
  
  library("stringr")
  
  b <- sub(" ", "", str_to_title(as.character(climate$species[i])))
  
  write.table(df,paste0("outputs/",b,"/SSP1 - climate + land use/species/",b,".csv"),
              col.names = T, row.names = F, sep = " ",
              quote = FALSE)
  
  write.table(df,paste0("outputs/",b,"/SSP5 - climate + land use/species/",b,".csv"),
              col.names = T, row.names = F, sep = " ",
              quote = FALSE)
  
  write.table(df,paste0("outputs/",b,"/SSP1 - climate/species/",b,".csv"),
              col.names = T, row.names = F, sep = " ",
              quote = FALSE)
  
  write.table(df,paste0("outputs/",b,"/SSP5 - climate/species/",b,".csv"),
              col.names = T, row.names = F, sep = " ",
              quote = FALSE)
  
  write.table(df,paste0("outputs/",b,"/SSP1 - land use/species/",b,".csv"),
              col.names = T, row.names = F, sep = " ",
              quote = FALSE)
  
  write.table(df,paste0("outputs/",b,"/SSP5 - land use/species/",b,".csv"),
              col.names = T, row.names = F, sep = " ",
              quote = FALSE)
  
  invisible(gc())
  
}

# remove unnecessary objects
rm(climate,a,i,b,df,dataT)
invisible(gc())

### clipping and exporting climatic data (this operation is quite time demanding)

# load package
library("terra")

# creating necessary function
state <- function(x) {
  print(substitute(x))
}

# creating lists for precipitation raster (2011-2040 SSP126)
l1 <- list("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2011-2040/GFDL-ESM4/ssp126/bio/CHELSA_bio12_2011-2040_gfdl-esm4_ssp126_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2011-2040/IPSL-CM6A-LR/ssp126/bio/CHELSA_bio12_2011-2040_ipsl-cm6a-lr_ssp126_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2011-2040/MPI-ESM1-2-HR/ssp126/bio/CHELSA_bio12_2011-2040_mpi-esm1-2-hr_ssp126_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2011-2040/MRI-ESM2-0/ssp126/bio/CHELSA_bio12_2011-2040_mri-esm2-0_ssp126_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2011-2040/UKESM1-0-LL/ssp126/bio/CHELSA_bio12_2011-2040_ukesm1-0-ll_ssp126_V.2.1.tif")

# calculating the mean of all the GCMs
start <- Sys.time()
bio12_126_2011_2040 <- mean(do.call(c,lapply(l1,rast)))
names(bio12_126_2011_2040) <- "bio12"
varnames(bio12_126_2011_2040) <- "bio12_126_2011_2040"
longnames(bio12_126_2011_2040) <- "bio12_126_2011_2040"
writeRaster(bio12_126_2011_2040, 
            filename = paste0("clim/Future/",state(bio12_126_2011_2040),".tif"), overwrite=TRUE)
rm(bio12_126_2011_2040)
end <- Sys.time()
end-start

# creating lists for temperature raster (2011-2040 SSP126)
l2 <- list("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2011-2040/GFDL-ESM4/ssp126/bio/CHELSA_bio1_2011-2040_gfdl-esm4_ssp126_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2011-2040/IPSL-CM6A-LR/ssp126/bio/CHELSA_bio1_2011-2040_ipsl-cm6a-lr_ssp126_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2011-2040/MPI-ESM1-2-HR/ssp126/bio/CHELSA_bio1_2011-2040_mpi-esm1-2-hr_ssp126_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2011-2040/MRI-ESM2-0/ssp126/bio/CHELSA_bio1_2011-2040_mri-esm2-0_ssp126_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2011-2040/UKESM1-0-LL/ssp126/bio/CHELSA_bio1_2011-2040_ukesm1-0-ll_ssp126_V.2.1.tif")

# calculating the mean of all the GCMs
start <- Sys.time()
bio01_126_2011_2040 <- mean(do.call(c,lapply(l2,rast)))
names(bio01_126_2011_2040) <- "bio1"
varnames(bio01_126_2011_2040) <- "bio01_126_2011_2040"
longnames(bio01_126_2011_2040) <- "bio01_126_2011_2040"
writeRaster(bio01_126_2011_2040, 
            filename = paste0("clim/Future/",state(bio01_126_2011_2040),".tif"), overwrite=TRUE)
rm(bio01_126_2011_2040)
end <- Sys.time()
end-start

# creating lists for precipitation raster (2011-2040 SSP585)
l3 <- list("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2011-2040/GFDL-ESM4/ssp585/bio/CHELSA_bio12_2011-2040_gfdl-esm4_ssp585_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2011-2040/IPSL-CM6A-LR/ssp585/bio/CHELSA_bio12_2011-2040_ipsl-cm6a-lr_ssp585_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2011-2040/MPI-ESM1-2-HR/ssp585/bio/CHELSA_bio12_2011-2040_mpi-esm1-2-hr_ssp585_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2011-2040/MRI-ESM2-0/ssp585/bio/CHELSA_bio12_2011-2040_mri-esm2-0_ssp585_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2011-2040/UKESM1-0-LL/ssp585/bio/CHELSA_bio12_2011-2040_ukesm1-0-ll_ssp585_V.2.1.tif")

# calculating the mean of all the GCMs
start <- Sys.time()
bio12_585_2011_2040 <- mean(do.call(c,lapply(l3,rast)))
names(bio12_585_2011_2040) <- "bio12"
varnames(bio12_585_2011_2040) <- "bio12_585_2011_2040"
longnames(bio12_585_2011_2040) <- "bio12_585_2011_2040"
writeRaster(bio12_585_2011_2040, 
            filename = paste0("clim/Future/",state(bio12_585_2011_2040),".tif"), overwrite=TRUE)
rm(bio12_585_2011_2040)
end <- Sys.time()
end-start

# creating lists for temperature raster (2011-2040 SSP585)
l4 <- list("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2011-2040/GFDL-ESM4/ssp585/bio/CHELSA_bio1_2011-2040_gfdl-esm4_ssp585_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2011-2040/IPSL-CM6A-LR/ssp585/bio/CHELSA_bio1_2011-2040_ipsl-cm6a-lr_ssp585_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2011-2040/MPI-ESM1-2-HR/ssp585/bio/CHELSA_bio1_2011-2040_mpi-esm1-2-hr_ssp585_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2011-2040/MRI-ESM2-0/ssp585/bio/CHELSA_bio1_2011-2040_mri-esm2-0_ssp585_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2011-2040/UKESM1-0-LL/ssp585/bio/CHELSA_bio1_2011-2040_ukesm1-0-ll_ssp585_V.2.1.tif")

# calculating the mean of all the GCMs
start <- Sys.time()
bio01_585_2011_2040 <- mean(do.call(c,lapply(l4,rast)))
names(bio01_585_2011_2040) <- "bio1"
varnames(bio01_585_2011_2040) <- "bio01_585_2011_2040"
longnames(bio01_585_2011_2040) <- "bio01_585_2011_2040"
writeRaster(bio01_585_2011_2040, 
            filename = paste0("clim/Future/",state(bio01_585_2011_2040),".tif"), overwrite=TRUE)
rm(bio01_585_2011_2040)
end <- Sys.time()
end-start

# creating lists for precipitation raster (2041-2070 SSP126)
l5 <- list("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/GFDL-ESM4/ssp126/bio/CHELSA_bio12_2041-2070_gfdl-esm4_ssp126_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/IPSL-CM6A-LR/ssp126/bio/CHELSA_bio12_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/MPI-ESM1-2-HR/ssp126/bio/CHELSA_bio12_2041-2070_mpi-esm1-2-hr_ssp126_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/MRI-ESM2-0/ssp126/bio/CHELSA_bio12_2041-2070_mri-esm2-0_ssp126_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/UKESM1-0-LL/ssp126/bio/CHELSA_bio12_2041-2070_ukesm1-0-ll_ssp126_V.2.1.tif")

# calculating the mean of all the GCMs
start <- Sys.time()
bio12_126_2041_2070 <- mean(do.call(c,lapply(l5,rast)))
names(bio12_126_2041_2070) <- "bio12"
varnames(bio12_126_2041_2070) <- "bio12_126_2041_2070"
longnames(bio12_126_2041_2070) <- "bio12_126_2041_2070"
writeRaster(bio12_126_2041_2070, 
            filename = paste0("clim/Future/",state(bio12_126_2041_2070),".tif"), overwrite=TRUE)
rm(bio12_126_2041_2070)
end <- Sys.time()
end-start

# creating lists for temperature raster (2041-2070 SSP126)
l6 <- list("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/GFDL-ESM4/ssp126/bio/CHELSA_bio1_2041-2070_gfdl-esm4_ssp126_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/IPSL-CM6A-LR/ssp126/bio/CHELSA_bio1_2041-2070_ipsl-cm6a-lr_ssp126_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/MPI-ESM1-2-HR/ssp126/bio/CHELSA_bio1_2041-2070_mpi-esm1-2-hr_ssp126_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/MRI-ESM2-0/ssp126/bio/CHELSA_bio1_2041-2070_mri-esm2-0_ssp126_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/UKESM1-0-LL/ssp126/bio/CHELSA_bio1_2041-2070_ukesm1-0-ll_ssp126_V.2.1.tif")

# calculating the mean of all the GCMs
start <- Sys.time()
bio01_126_2041_2070 <- mean(do.call(c,lapply(l6,rast)))
names(bio01_126_2041_2070) <- "bio1"
varnames(bio01_126_2041_2070) <- "bio1"
longnames(bio01_126_2041_2070) <- "bio1"
writeRaster(bio01_126_2041_2070, 
            filename = paste0("clim/Future/",state(bio01_126_2041_2070),".tif"), overwrite=TRUE)
rm(bio01_126_2041_2070)
end <- Sys.time()
end-start

# creating lists for precipitation raster (2041-2070 SSP585)
l7 <- list("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/GFDL-ESM4/ssp585/bio/CHELSA_bio12_2041-2070_gfdl-esm4_ssp585_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/IPSL-CM6A-LR/ssp585/bio/CHELSA_bio12_2041-2070_ipsl-cm6a-lr_ssp585_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/MPI-ESM1-2-HR/ssp585/bio/CHELSA_bio12_2041-2070_mpi-esm1-2-hr_ssp585_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/MRI-ESM2-0/ssp585/bio/CHELSA_bio12_2041-2070_mri-esm2-0_ssp585_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/UKESM1-0-LL/ssp585/bio/CHELSA_bio12_2041-2070_ukesm1-0-ll_ssp585_V.2.1.tif")

# calculating the mean of all the GCMs
start <- Sys.time()
bio12_585_2041_2070 <- mean(do.call(c,lapply(l7,rast)))
names(bio12_585_2041_2070) <- "bio12"
writeRaster(bio12_585_2041_2070, 
            filename = paste0("clim/Future/",state(bio12_585_2041_2070),".tif"), overwrite=TRUE)
rm(bio12_585_2041_2070)
end <- Sys.time()
end-start

# creating lists for temperature raster (2041-2070 SSP585)
l8 <- list("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/GFDL-ESM4/ssp585/bio/CHELSA_bio1_2041-2070_gfdl-esm4_ssp585_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/IPSL-CM6A-LR/ssp585/bio/CHELSA_bio1_2041-2070_ipsl-cm6a-lr_ssp585_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/MPI-ESM1-2-HR/ssp585/bio/CHELSA_bio1_2041-2070_mpi-esm1-2-hr_ssp585_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/MRI-ESM2-0/ssp585/bio/CHELSA_bio1_2041-2070_mri-esm2-0_ssp585_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2041-2070/UKESM1-0-LL/ssp585/bio/CHELSA_bio1_2041-2070_ukesm1-0-ll_ssp585_V.2.1.tif")

# calculating the mean of all the GCMs
start <- Sys.time()
bio01_585_2041_2070 <- mean(do.call(c,lapply(l8,rast)))
names(bio01_585_2041_2070) <- "bio1"
writeRaster(bio01_585_2041_2070, 
            filename = paste0("clim/Future/",state(bio01_585_2041_2070),".tif"), overwrite=TRUE)
rm(bio01_585_2041_2070)
end <- Sys.time()
end-start

# creating lists for precipitation raster (2071-2100 SSP126)
l9 <- list("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2071-2100/GFDL-ESM4/ssp126/bio/CHELSA_bio12_2071-2100_gfdl-esm4_ssp126_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2071-2100/IPSL-CM6A-LR/ssp126/bio/CHELSA_bio12_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2071-2100/MPI-ESM1-2-HR/ssp126/bio/CHELSA_bio12_2071-2100_mpi-esm1-2-hr_ssp126_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2071-2100/MRI-ESM2-0/ssp126/bio/CHELSA_bio12_2071-2100_mri-esm2-0_ssp126_V.2.1.tif",
           "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2071-2100/UKESM1-0-LL/ssp126/bio/CHELSA_bio12_2071-2100_ukesm1-0-ll_ssp126_V.2.1.tif")

# calculating the mean of all the GCMs
start <- Sys.time()
bio12_126_2071_2100 <- mean(do.call(c,lapply(l9,rast)))
names(bio12_126_2071_2100) <- "bio12"
writeRaster(bio12_126_2071_2100, 
            filename = paste0("clim/Future/",state(bio12_126_2071_2100),".tif"), overwrite=TRUE)
rm(bio12_126_2071_2100)
end <- Sys.time()
end-start

# creating lists for temperature raster (2071-2100 SSP126)
l10 <- list("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2071-2100/GFDL-ESM4/ssp126/bio/CHELSA_bio1_2071-2100_gfdl-esm4_ssp126_V.2.1.tif",
            "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2071-2100/IPSL-CM6A-LR/ssp126/bio/CHELSA_bio1_2071-2100_ipsl-cm6a-lr_ssp126_V.2.1.tif",
            "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2071-2100/MPI-ESM1-2-HR/ssp126/bio/CHELSA_bio1_2071-2100_mpi-esm1-2-hr_ssp126_V.2.1.tif",
            "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2071-2100/MRI-ESM2-0/ssp126/bio/CHELSA_bio1_2071-2100_mri-esm2-0_ssp126_V.2.1.tif",
            "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2071-2100/UKESM1-0-LL/ssp126/bio/CHELSA_bio1_2071-2100_ukesm1-0-ll_ssp126_V.2.1.tif")

# calculating the mean of all the GCMs
start <- Sys.time()
bio01_126_2071_2100 <- mean(do.call(c,lapply(l10,rast)))
names(bio01_126_2071_2100) <- "bio1"
writeRaster(bio01_126_2071_2100, 
            filename = paste0("clim/Future/",state(bio01_126_2071_2100),".tif"), overwrite=TRUE)
rm(bio01_126_2071_2100)
end <- Sys.time()
end-start

# creating lists for precipitation raster (2071-2100 SSP585)
l11 <- list("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2071-2100/GFDL-ESM4/ssp585/bio/CHELSA_bio12_2071-2100_gfdl-esm4_ssp585_V.2.1.tif",
            "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2071-2100/IPSL-CM6A-LR/ssp585/bio/CHELSA_bio12_2071-2100_ipsl-cm6a-lr_ssp585_V.2.1.tif",
            "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2071-2100/MPI-ESM1-2-HR/ssp585/bio/CHELSA_bio12_2071-2100_mpi-esm1-2-hr_ssp585_V.2.1.tif",
            "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2071-2100/MRI-ESM2-0/ssp585/bio/CHELSA_bio12_2071-2100_mri-esm2-0_ssp585_V.2.1.tif",
            "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2071-2100/UKESM1-0-LL/ssp585/bio/CHELSA_bio12_2071-2100_ukesm1-0-ll_ssp585_V.2.1.tif")

# calculating the mean of all the GCMs
start <- Sys.time()
bio12_585_2071_2100 <- mean(do.call(c,lapply(l11,rast)))
names(bio12_585_2071_2100) <- "bio12"
writeRaster(bio12_585_2071_2100, 
            filename = paste0("clim/Future/",state(bio12_585_2071_2100),".tif"), overwrite=TRUE)
rm(bio12_585_2071_2100)
end <- Sys.time()
end-start

# creating lists for temperature raster (2071-2100 SSP585)
l12 <- list("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2071-2100/GFDL-ESM4/ssp585/bio/CHELSA_bio1_2071-2100_gfdl-esm4_ssp585_V.2.1.tif",
            "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2071-2100/IPSL-CM6A-LR/ssp585/bio/CHELSA_bio1_2071-2100_ipsl-cm6a-lr_ssp585_V.2.1.tif",
            "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2071-2100/MPI-ESM1-2-HR/ssp585/bio/CHELSA_bio1_2071-2100_mpi-esm1-2-hr_ssp585_V.2.1.tif",
            "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2071-2100/MRI-ESM2-0/ssp585/bio/CHELSA_bio1_2071-2100_mri-esm2-0_ssp585_V.2.1.tif",
            "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2071-2100/UKESM1-0-LL/ssp585/bio/CHELSA_bio1_2071-2100_ukesm1-0-ll_ssp585_V.2.1.tif")

# calculating the mean of all the GCMs
start <- Sys.time()
bio01_585_2071_2100 <- mean(do.call(c,lapply(l12,rast)))
names(bio01_585_2071_2100) <- "bio1"
writeRaster(bio01_585_2071_2100, 
            filename = paste0("clim/Future/",state(bio01_585_2071_2100),".tif"), overwrite=TRUE)
rm(bio01_585_2071_2100)
end <- Sys.time()
end-start

# removing unnecessary objects
rm(l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12)

# importing present values (precipitation; 1981-2010)
bio12_1981_2010 <- rast("clim/1981_2010/bio12.tif")
names(bio12_1981_2010) <- "bio12"
varnames(bio12_1981_2010) <- "bio12_1981_2010"
longnames(bio12_1981_2010) <- "bio12_1981_2010"

# importing present values (temperature; 1981-2010)
bio01_1981_2010 <- rast("clim/1981_2010/bio01.tif")
names(bio01_1981_2010) <- "bio1"
varnames(bio01_1981_2010) <- "bio01_1981_2010"
longnames(bio01_1981_2010) <- "bio01_1981_2010"
rm(bio01_1981_2010,bio12_1981_2010)

###### croping and exporting ####

# load package
library("terra")

# import rasters
EVs <- rast(list.files("clim/Future/",
                       pattern = "tif", full.names=T))

# remove unnecessary objects
rm(bio01_1981_2010,bio12_1981_2010)

# load package
library("sf")

# import shapefile
shape <- read_sf("shapefiles/Bav_BW.shp")

# turn shapefile into SpatVector object
tShape <- vect(shape);rm(shape)

# crs
crs(EVs) <- crs(tShape)

# extent
BavBW <- crop(EVs, ext(tShape))

# masking time
# BavBW <- mask(EVs,tShape)
BavBW <- mask(BavBW,tShape)
a <- varnames(EVs)
names(BavBW) <- a

# aggregate data
xBavBW <- aggregate(BavBW, fact = 6, fun = "mean", na.rm = T)

# create dummy raster
s <- rast(nrow = 66,
          ncol = 127,
          xmin = 7.49895858764648,
          xmax = 13.8480768585205,
          ymin = 47.286860309177,
          ymax = 50.5859435865614)
crs(s)<- "epsg:4326"

# resampling to match the 
nBavBW <- resample(xBavBW, s, method = "bilinear")

# remove unnecessary objects
rm(xBavBW,s)
invisible(gc())

# structuring
par(mfrow = c(4,3))

# plotting the clipped rasters
for(i in seq_along(names(BavBW))){
  if(i < 7){
    plot(nBavBW[[i]],
         range = c(-3.41,16.27),
         main = names(BavBW[[i]]))
  } else {
    plot(nBavBW[[i]],
         range = c(606.64,3283.36),
         main = names(BavBW[[i]]))
  }
}

# remove unnecessary objects
rm(i,EVs,tShape)
invisible(gc())

# cropping rasters
for(i in seq_along(names(nBavBW))){
  
  # now save as CSV in MetaRange format
  metarange <- round(as.array(nBavBW[[i]]))
  metarange[which(is.na(metarange))] <- "NaN"
  invisible(gc()) # garbage collection
  
  # Julia needs to have grid cells without values filled with "NaN"
  write.table(metarange,
              paste0("clim/Future/Cropped/",names(nBavBW)[i],".csv"),
              col.names = F, row.names = F)
  
  invisible(gc()) # garbage collection
  
  rm(metarange)
  
}

# removing unnecessary objects
rm(i,a,BavBW,nBavBW,state,end,start)
invisible(gc())

##### Step 2 - LAND USE/HABITAT SUITABILITY DATA #####

### selecting time for the baseline

# creating plotting function (from the oldest record to the present)
plantHist <- function(x){
  
  hist(x$year, probability = T, 
       main = paste(unique(x$species), "(All records)"),
       xlab = "Year", col = "green4")
  lines(density(x$year, na.rm = T),
        col = "gold", lwd = 2)
  
}

# creating plotting function (from 1900 to the present)
plantHist19 <- function(x){
  
  hist(x[x$year >= 1900,]$year, probability = T, 
       main = paste(unique(x$species), "\n (From 1900 to Present)"),
       xlab = "Year", col = "green4")
  lines(density(x[x$year >= 1900,]$year, na.rm = T),
        col = "gold", lwd = 2)
  
}

# creating plotting function (from 2000 to the present)
plantHist20 <- function(x){
  
  hist(x[x$year >= 2000,]$year, probability = T, 
       main = paste(unique(x$species), "\n (From 2000 to Present)"),
       xlab = "Year", col = "green4")
  lines(density(x[x$year >= 2000,]$year, na.rm = T),
        col = "gold", lwd = 2)
  
}

# structuring
par(mfrow = c(2, 2))

# plotting from the oldest record to the present
lapply(los,plantHist)

# plotting from 1900 to the present
lapply(los,plantHist19)

# plotting from 2000 to the present
lapply(los,plantHist20)

# remove unnecessary objects
rm(plantHist,plantHist19,plantHist20)
invisible(gc()) # to empty the memory

### processing land use

# creating function to convert NetCDF files into Stacked Rasters
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
  invisible(gc())
  
  # load package
  require("terra")
  
  # creat SpatRaster object
  r <- rast(r)
  
  # change name of layers to name of variables
  names(r) <- var[i]
  
  # close NetCDF file
  nc_close(nc_data) 
  
  # return values
  return(r)
}

### rotating land use file for 2020 (SSP 1 - RCP 2.6 scenario)

# load package
library("terra")

# create object with the "i" number of variables
n <- c(1:33)

# create stacked raster
LU2020 <- do.call(c, lapply(n,ncdf2rast,nc = "LU/126/GCAM_Demeter_LU_ssp1_rcp26_modelmean_2020"))
invisible(gc())

# removing unnecessary objects
rm(n)
invisible(gc())

# exporting rotated stacked raster
writeRaster(LU2020,"LU2020.tif", overwrite = TRUE)

# removing unnecessary objects
rm(LU2020)
invisible(gc())

# (re-)importing land use data
LU2020 <- rast("LU2020.tif")
invisible(gc())

### calculating suitability

# creating function to extract land use values from stacked raster
getLUalues <- function(vector,NetCDF){
  
  # import package
  require("terra", quietly = T)
  
  # processing dataset
  points <- subset(vector, select = c("decimalLongitude","decimalLatitude"))
  colnames(points) <- c("x","y")
  
  # extract values
  points <- as.data.frame(points)
  valuesatpoints <- extract(NetCDF, points)
  invisible(gc()) # to empty the memory
  points <- cbind(points,valuesatpoints)
  
  # These are the acronyms that are in the article
  colnames(points) <- c("lon","lat","ID","Water",
                        "NET_tem","NET_bor","NDT_bor","BET_tro",
                        "BET_tem","BDT_tro","BDT_tem","BDT_bor",
                        "BES_tem","BDS_tem","BDS_bor","C3_gra_arc",
                        "C3_gra","C4_gra","Corn_rf","Corn_irr",
                        "Wheat_rf","Wheat_irr","Soy_rf","Soy_irr",
                        "Cotton_rf","Cotton_irr","Rice_rf","Rice_irr",
                        "Sugarcrop_rf","Sugarcrop_irr","OtherCrop_rf","OtherCrop_irr",
                        "Bioenergy_rf","Bioenergy_irr","Urban","Barren")
  
  # These are the names that are in the .nc file
  # colnames(points) <- c("lat","lon","ID","PFT0",
  #                       "PFT1","PFT2","PFT3","PFT4",
  #                       "PFT5","PFT6","PFT7","PFT8",
  #                       "PFT9","PFT10","PFT11","PFT12",
  #                       "PFT13","PFT14","PFT15","PFT16",
  #                       "PFT17","PFT18","PFT19","PFT20",
  #                       "PFT21","PFT22","PFT23","PFT24",
  #                       "PFT25","PFT26","PFT27","PFT28",
  #                       "PFT29","PFT30","PFT31","PFT32")
  
  # These are the code names that are in the article
  # colnames(points) <- c("lat","lon","ID","FLT0",
  #                       "FLT1","FLT2","FLT3","FLT4",
  #                       "FLT5","FLT6","FLT7","FLT8",
  #                       "FLT9","FLT10","FLT11","FLT12",
  #                       "FLT13","FLT14","FLT15","FLT16",
  #                       "FLT17","FLT18","FLT19","FLT20",
  #                       "FLT21","FLT22","FLT23","FLT24",
  #                       "FLT25","FLT26","FLT27","FLT28",
  #                       "FLT29","FLT30","FLT31","FLT32")
  
  # calculating suitability
  SI <- (rowSums(t(points[,4:36]), na.rm = T)/max(rowSums(t(points[,4:36]), na.rm = T)));invisible(gc())
  
  # making the landuse class columns
  SI <- t(SI)
  
  # creating species column
  empty <- data.frame(`species` = unique(vector$species))
  
  # bringing it all up together
  df_SI <- cbind(empty,SI)
  
  # free unused memory
  invisible(gc())
  
  # return values
  return(df_SI)
}

# run the list of species through the function and combine the result into a single dataframe
SI <- do.call(rbind,lapply(los,getLUalues, NetCDF = LU2020))
invisible(gc())

# remove unnecessary objects
rm(LU2020)
invisible(gc())

### calculating and clipping habitaty suitability

# import paths
netlist <- append(list.files("LU/126", pattern = "nc", full.names=T),
                  list.files("LU/585", pattern = "nc", full.names=T))

# process paths
for(i in seq_along(netlist)){
  
  netlist[i] <- substr(netlist[i],1,nchar(netlist[i])-3)
  
  rm(i)
}
invisible(gc())

# load package
library("sf")

# import shapefile
shape <- read_sf("shapefiles/Bav_BW.shp")

# load package
library("terra")

# turn shapefile into SpatVector object
tShape <- vect(shape)

# remove unnecessary object
rm(shape)

# create suitability maps for all species
for(x in seq_along(netlist)){
  
  # create stacked raster
  n <- do.call(c, lapply(1:33,ncdf2rast,nc = netlist[x]))
  invisible(gc())
  
  # extent
  BavBW <- crop(n, ext(tShape))
  
  # masking time
  BavBW <- mask(BavBW,tShape)
  a <- varnames(n)
  rm(n)
  names(BavBW) <- a
  
  # creating list
  los <- SI$species
  
  for(i in seq_along(los)){
    
    r <- rast(ncol = 127, nrow = 66, xmin = 7.498959,
              xmax = 13.84808, ymin = 47.28686,
              ymax = 50.58594)
    r <- rep(r,33)
    
    SIn <- SI[,-1]
    
    for(z in seq_along(names(SIn[i,]))){
      
      r[[z]] <- BavBW[[z]]*SIn[i,z]
      invisible(gc())
      
    }
    
    r <- sum(r, na.rm = T)
    invisible(gc())
    
    #scenario name
    scenario <- substr(netlist[x],24,nchar(netlist[x]))
    
    # load package
    library("stringr")
    
    # species name
    species <- sub(" ", "", str_to_title(as.character(unique(SI[i,]$species))))
    
    # now save as CSV in MetaRange format
    metarange <- round(as.array(r))
    metarange[which(is.na(metarange))] <- "NaN"
    invisible(gc()) # garbage collection
    
    # Julia needs to have grid cells without values filled with "NaN"
    write.table(metarange,
                paste0("LU/Future/",scenario,"_",species,".csv"),
                col.names = F, row.names = F)
    
    # write as raster as well
    writeRaster(r,
                paste0("LU/Future/tif/",scenario,"_",species,".tif"),
                overwrite = T)
    
    invisible(gc()) # garbage collection
    
    rm(r,SIn,i,z,metarange)
  }
  rm(x,a,los,species,scenario)
}

# remove unnecessary objects
rm(los)

# make list 
HS_List <- list.files("LU/Future/tif", pattern = "tif", full.names=T)

# processing list (plotting result for the species F. sylvatica as an example - part 1)
Fsylvatica_list <- HS_List[which(substr(HS_List,41,54) == "FagusSylvatica")]
Fsylvatica_list <- Fsylvatica_list[c(1:18)]

# remove unnecessary objects
rm(HS_List)

# load package
library("terra")

# import data
Fsylvatica <- rast(Fsylvatica_list)

# structuring
par(mfrow = c(3,6))

# making the plots
for(i in seq_along(Fsylvatica_list)){
  
  plot(Fsylvatica[[i]],
       main = substr(Fsylvatica_list[i],36,39),
       range = c(0,100))
  
}
mtext(substitute(paste(italic('F. sylvatica')," (SSP1 - RCP 2.6)")), side = 3, line = - 2.75, outer = TRUE)

# remove unnecessary objects
rm(i,Fsylvatica,Fsylvatica_list)

##### Step 3 - COMBINING CLIMATIC FILES WITH HABITAT SUITABILITY FILES #####

### CLIMATIC FILES ###

# load package
library("data.table")

# create list
clim_list <- list.files("clim/Future/Cropped", pattern = "csv", full.names=T)

# processing list (taking unwanted files)
clim_list <- clim_list[-c(4,11)]

# creating species vector
species <- c("AchilleaMillefolium","EriophorumVaginatum","FagusSylvatica",
             "PlantagoLanceolata","PoaPratensis","PoaTrivialis","RanunculusAcris",
             "RanunculusBulbosus","RumexAcetosa","VeronicaChamaedrys","ViolaArvensis" )

# creating files - part 1
for(j in seq_along(species)){
  for(i in seq_along(clim_list[c(1,4,7,10)])){
    k <- fread(clim_list[c(1,4,7,10)][i])
    for(c in seq_along(2011:2100)){
      if(c < 10){
        if(i == 1){
          write.table(k,paste0("outputs/",species[j],"/SSP1 - land use/environment/temperature/Bio01_0",c,".csv"),
                      col.names = T, row.names = F, sep = " ",
                      quote = FALSE)
        } else {
          if(i == 2){
            write.table(k,paste0("outputs/",species[j],"/SSP5 - land use/environment/temperature/Bio01_0",c,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
          } else {
            if(i == 3){
              write.table(k,paste0("outputs/",species[j],"/SSP1 - land use/environment/precipitation/Bio12_0",c,".csv"),
                          col.names = T, row.names = F, sep = " ",
                          quote = FALSE)
            } else {
              write.table(k,paste0("outputs/",species[j],"/SSP5 - land use/environment/precipitation/Bio12_0",c,".csv"),
                          col.names = T, row.names = F, sep = " ",
                          quote = FALSE)
            }
          }
        }
      } else {
        if(i == 1){
          write.table(k,paste0("outputs/",species[j],"/SSP1 - land use/environment/temperature/Bio01_",c,".csv"),
                      col.names = T, row.names = F, sep = " ",
                      quote = FALSE)
        } else {
          if(i == 2){
            write.table(k,paste0("outputs/",species[j],"/SSP5 - land use/environment/temperature/Bio01_",c,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
          } else {
            if(i == 3){
              write.table(k,paste0("outputs/",species[j],"/SSP1 - land use/environment/precipitation/Bio12_",c,".csv"),
                          col.names = T, row.names = F, sep = " ",
                          quote = FALSE)
            } else {
              write.table(k,paste0("outputs/",species[j],"/SSP5 - land use/environment/precipitation/Bio12_",c,".csv"),
                          col.names = T, row.names = F, sep = " ",
                          quote = FALSE)
            }
          }
        }
      }
    }
  }
}

# remove unnecessary objects
rm(i,j,k,c)

# creating files - part 2
for(z in seq_along(species)){
  for(i in seq_along(clim_list)){
    
    # import file
    a <- fread(clim_list[i])
    
    # if it is a temperature file for scenario 126
    if(substr(clim_list[i],21,29) == "bio01_126"){
      
      # if it is from the interval 2011-2040
      if(substr(clim_list[i],31,39) == "2011_2040"){
        
        for(x in seq_along(2011:2040)){
          
          # if the value of x is lower than 10
          if(x < 10){
            write.table(a,paste0("outputs/",species[z],"/SSP1 - climate/environment/temperature/Bio01_0",x,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
            write.table(a,paste0("outputs/",species[z],"/SSP1 - climate + land use/environment/temperature/Bio01_0",x,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
          } else {
            write.table(a,paste0("outputs/",species[z],"/SSP1 - climate/environment/temperature/Bio01_",x,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
            write.table(a,paste0("outputs/",species[z],"/SSP1 - climate + land use/environment/temperature/Bio01_",x,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
          }}
        
        # remove unnecessary objects
        rm(x)
      } else {
        # if it is from the interval 2041-2070
        if(substr(clim_list[i],31,39) == "2041_2070"){
          for(x in seq_along(2041:2070)+30){
            write.table(a,paste0("outputs/",species[z],"/SSP1 - climate/environment/temperature/Bio01_",x,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
            write.table(a,paste0("outputs/",species[z],"/SSP1 - climate + land use/environment/temperature/Bio01_",x,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
          }
          
          # remove unnecessary objects
          rm(x)
        } else {
          # if it is from the interval 2071-2100
          for(x in seq_along(2071:2100)+60){
            write.table(a,paste0("outputs/",species[z],"/SSP1 - climate/environment/temperature/Bio01_",x,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
            write.table(a,paste0("outputs/",species[z],"/SSP1 - climate + land use/environment/temperature/Bio01_",x,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
          }
          
          # remove unnecessary objects
          rm(x)
        }
      }
    } else {
      if(substr(clim_list[i],21,29) == "bio01_585"){
        # if it is from the interval 2011-2040
        if(substr(clim_list[i],31,39) == "2011_2040"){
          
          for(x in seq_along(2011:2040)){
            
            # if the value of x is lower than 10
            if(x < 10){
              write.table(a,paste0("outputs/",species[z],"/SSP5 - climate/environment/temperature/Bio01_0",x,".csv"),
                          col.names = T, row.names = F, sep = " ",
                          quote = FALSE)
              write.table(a,paste0("outputs/",species[z],"/SSP5 - climate + land use/environment/temperature/Bio01_0",x,".csv"),
                          col.names = T, row.names = F, sep = " ",
                          quote = FALSE)
            } else {
              write.table(a,paste0("outputs/",species[z],"/SSP5 - climate/environment/temperature/Bio01_",x,".csv"),
                          col.names = T, row.names = F, sep = " ",
                          quote = FALSE)
              write.table(a,paste0("outputs/",species[z],"/SSP5 - climate + land use/environment/temperature/Bio01_",x,".csv"),
                          col.names = T, row.names = F, sep = " ",
                          quote = FALSE)
            }}
          
          # remove unnecessary objects
          rm(x)
        } else {
          # if it is from the interval 2041-2070
          if(substr(clim_list[i],31,39) == "2041_2070"){
            for(x in seq_along(2041:2070)+30){
              write.table(a,paste0("outputs/",species[z],"/SSP5 - climate/environment/temperature/Bio01_",x,".csv"),
                          col.names = T, row.names = F, sep = " ",
                          quote = FALSE)
              write.table(a,paste0("outputs/",species[z],"/SSP5 - climate + land use/environment/temperature/Bio01_",x,".csv"),
                          col.names = T, row.names = F, sep = " ",
                          quote = FALSE)
            }
            
            # remove unnecessary objects
            rm(x)
          } else {
            # if it is from the interval 2071-2100
            for(x in seq_along(2071:2100)+60){
              write.table(a,paste0("outputs/",species[z],"/SSP5 - climate/environment/temperature/Bio01_",x,".csv"),
                          col.names = T, row.names = F, sep = " ",
                          quote = FALSE)
              write.table(a,paste0("outputs/",species[z],"/SSP5 - climate + land use/environment/temperature/Bio01_",x,".csv"),
                          col.names = T, row.names = F, sep = " ",
                          quote = FALSE)
            }
            
            # remove unnecessary objects
            rm(x)
          }
        }
      } else {
        if(substr(clim_list[i],21,29) == "bio12_126"){
          # if it is from the interval 2011-2040
          if(substr(clim_list[i],31,39) == "2011_2040"){
            
            for(x in seq_along(2011:2040)){
              
              # if the value of x is lower than 10
              if(x < 10){
                write.table(a,paste0("outputs/",species[z],"/SSP1 - climate/environment/precipitation/Bio12_0",x,".csv"),
                            col.names = T, row.names = F, sep = " ",
                            quote = FALSE)
                write.table(a,paste0("outputs/",species[z],"/SSP1 - climate + land use/environment/precipitation/Bio12_0",x,".csv"),
                            col.names = T, row.names = F, sep = " ",
                            quote = FALSE)
              } else {
                write.table(a,paste0("outputs/",species[z],"/SSP1 - climate/environment/precipitation/Bio12_",x,".csv"),
                            col.names = T, row.names = F, sep = " ",
                            quote = FALSE)
                write.table(a,paste0("outputs/",species[z],"/SSP1 - climate + land use/environment/precipitation/Bio12_",x,".csv"),
                            col.names = T, row.names = F, sep = " ",
                            quote = FALSE)
              }}
            
            # remove unnecessary objects
            rm(x)
          } else {
            # if it is from the interval 2041-2070
            if(substr(clim_list[i],31,39) == "2041_2070"){
              for(x in seq_along(2041:2070)+30){
                write.table(a,paste0("outputs/",species[z],"/SSP1 - climate/environment/precipitation/Bio12_",x,".csv"),
                            col.names = T, row.names = F, sep = " ",
                            quote = FALSE)
                write.table(a,paste0("outputs/",species[z],"/SSP1 - climate + land use/environment/precipitation/Bio12_",x,".csv"),
                            col.names = T, row.names = F, sep = " ",
                            quote = FALSE)
              }
              
              # remove unnecessary objects
              rm(x)
            } else {
              # if it is from the interval 2071-2100
              for(x in seq_along(2071:2100)+60){
                write.table(a,paste0("outputs/",species[z],"/SSP1 - climate/environment/precipitation/Bio12_",x,".csv"),
                            col.names = T, row.names = F, sep = " ",
                            quote = FALSE)
                write.table(a,paste0("outputs/",species[z],"/SSP1 - climate + land use/environment/precipitation/Bio12_",x,".csv"),
                            col.names = T, row.names = F, sep = " ",
                            quote = FALSE)
              }
              
              # remove unnecessary objects
              rm(x)
            }
          }
        } else {
          # if it is from the interval 2011-2040
          if(substr(clim_list[i],31,39) == "2011_2040"){
            
            for(x in seq_along(2011:2040)){
              
              # if the value of x is lower than 10
              if(x < 10){
                write.table(a,paste0("outputs/",species[z],"/SSP5 - climate/environment/precipitation/Bio12_0",x,".csv"),
                            col.names = T, row.names = F, sep = " ",
                            quote = FALSE)
                write.table(a,paste0("outputs/",species[z],"/SSP5 - climate + land use/environment/precipitation/Bio12_0",x,".csv"),
                            col.names = T, row.names = F, sep = " ",
                            quote = FALSE)
              } else {
                write.table(a,paste0("outputs/",species[z],"/SSP5 - climate/environment/precipitation/Bio12_",x,".csv"),
                            col.names = T, row.names = F, sep = " ",
                            quote = FALSE)
                write.table(a,paste0("outputs/",species[z],"/SSP5 - climate + land use/environment/precipitation/Bio12_",x,".csv"),
                            col.names = T, row.names = F, sep = " ",
                            quote = FALSE)
              }}
            
            # remove unnecessary objects
            rm(x)
          } else {
            # if it is from the interval 2041-2070
            if(substr(clim_list[i],31,39) == "2041_2070"){
              for(x in seq_along(2041:2070)+30){
                write.table(a,paste0("outputs/",species[z],"/SSP5 - climate/environment/precipitation/Bio12_",x,".csv"),
                            col.names = T, row.names = F, sep = " ",
                            quote = FALSE)
                write.table(a,paste0("outputs/",species[z],"/SSP5 - climate + land use/environment/precipitation/Bio12_",x,".csv"),
                            col.names = T, row.names = F, sep = " ",
                            quote = FALSE)
              }
              
              # remove unnecessary objects
              rm(x)
            } else {
              # if it is from the interval 2071-2100
              for(x in seq_along(2071:2100)+60){
                write.table(a,paste0("outputs/",species[z],"/SSP5 - climate/environment/precipitation/Bio12_",x,".csv"),
                            col.names = T, row.names = F, sep = " ",
                            quote = FALSE)
                write.table(a,paste0("outputs/",species[z],"/SSP5 - climate + land use/environment/precipitation/Bio12_",x,".csv"),
                            col.names = T, row.names = F, sep = " ",
                            quote = FALSE)
              }
              
              # remove unnecessary objects
              rm(x)
            }
          }
        }
      } 
    }
    
    # remove unnecessary objects
    rm(a)
  }
}

# remove unnecessary objects
rm(clim_list,i,species)
invisible(gc())

### SUITABILITY FILES ###

# create list
species_list <- list.files("LU/Future", pattern = "csv", full.names=T)

# creating files
for(i in seq_along(species_list)){
  
  # import file
  a <- fread(species_list[i])
  
  # if it is from scenario 1.26
  if(substr(species_list[i],14,14) == 1){
    if(substr(species_list[i],32,35) == 2015){
      for(x in seq_along(2011:2015)){
        write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - land use/environment/suitability/HS_0",x,".csv"),
                    col.names = T, row.names = F, sep = " ",
                    quote = FALSE)
        write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - climate + land use/environment/suitability/HS_0",x,".csv"),
                    col.names = T, row.names = F, sep = " ",
                    quote = FALSE)
      }
      
      # remove unnecessary objects
      rm(x)
    } else {
      if(substr(species_list[i],32,35) == 2020){
        for(x in seq_along(2016:2020)+5){
          if(x < 10){
            write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - land use/environment/suitability/HS_0",x,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
            write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - climate + land use/environment/suitability/HS_0",x,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
          } else {
            write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - land use/environment/suitability/HS_",x,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
            write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - climate + land use/environment/suitability/HS_",x,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
          }
        }
        
        # remove unnecessary objects
        rm(x)
      } else {
        if(substr(species_list[i],32,35) == 2025){
          for(x in seq_along(2021:2025)+10){
            write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - land use/environment/suitability/HS_",x,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
            write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - climate + land use/environment/suitability/HS_",x,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
          }
          
          # remove unnecessary objects
          rm(x)
        } else {
          if(substr(species_list[i],32,35) == 2030){
            for(x in seq_along(2026:2030)+15){
              write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - land use/environment/suitability/HS_",x,".csv"),
                          col.names = T, row.names = F, sep = " ",
                          quote = FALSE)
              write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - climate + land use/environment/suitability/HS_",x,".csv"),
                          col.names = T, row.names = F, sep = " ",
                          quote = FALSE)
            }
            
            # remove unnecessary objects
            rm(x)
          } else {
            if(substr(species_list[i],32,35) == 2035){
              for(x in seq_along(2031:2035)+20){
                write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - land use/environment/suitability/HS_",x,".csv"),
                            col.names = T, row.names = F, sep = " ",
                            quote = FALSE)
                write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - climate + land use/environment/suitability/HS_",x,".csv"),
                            col.names = T, row.names = F, sep = " ",
                            quote = FALSE)
              }
              
              # remove unnecessary objects
              rm(x)
            } else {
              if(substr(species_list[i],32,35) == 2040){
                for(x in seq_along(2036:2040)+25){
                  write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - land use/environment/suitability/HS_",x,".csv"),
                              col.names = T, row.names = F, sep = " ",
                              quote = FALSE)
                  write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - climate + land use/environment/suitability/HS_",x,".csv"),
                              col.names = T, row.names = F, sep = " ",
                              quote = FALSE)
                }
                
                # remove unnecessary objects
                rm(x)
              } else {
                if(substr(species_list[i],32,35) == 2045){
                  for(x in seq_along(2041:2045)+30){
                    write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - land use/environment/suitability/HS_",x,".csv"),
                                col.names = T, row.names = F, sep = " ",
                                quote = FALSE)
                    write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - climate + land use/environment/suitability/HS_",x,".csv"),
                                col.names = T, row.names = F, sep = " ",
                                quote = FALSE)
                  }
                  
                  # remove unnecessary objects
                  rm(x)
                } else {
                  if(substr(species_list[i],32,35) == 2050){
                    for(x in seq_along(2046:2050)+35){
                      write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - land use/environment/suitability/HS_",x,".csv"),
                                  col.names = T, row.names = F, sep = " ",
                                  quote = FALSE)
                      write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - climate + land use/environment/suitability/HS_",x,".csv"),
                                  col.names = T, row.names = F, sep = " ",
                                  quote = FALSE)
                    }
                    
                    # remove unnecessary objects
                    rm(x)
                  } else {
                    if(substr(species_list[i],32,35) == 2055){
                      for(x in seq_along(2051:2055)+40){
                        write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - land use/environment/suitability/HS_",x,".csv"),
                                    col.names = T, row.names = F, sep = " ",
                                    quote = FALSE)
                        write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - climate + land use/environment/suitability/HS_",x,".csv"),
                                    col.names = T, row.names = F, sep = " ",
                                    quote = FALSE)
                      }
                      
                      # remove unnecessary objects
                      rm(x)
                    } else {
                      if(substr(species_list[i],32,35) == 2060){
                        for(x in seq_along(2056:2060)+45){
                          write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - land use/environment/suitability/HS_",x,".csv"),
                                      col.names = T, row.names = F, sep = " ",
                                      quote = FALSE)
                          write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - climate + land use/environment/suitability/HS_",x,".csv"),
                                      col.names = T, row.names = F, sep = " ",
                                      quote = FALSE)
                        }
                        
                        # remove unnecessary objects
                        rm(x)
                      } else {
                        if(substr(species_list[i],32,35) == 2065){
                          for(x in seq_along(2061:2065)+50){
                            write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - land use/environment/suitability/HS_",x,".csv"),
                                        col.names = T, row.names = F, sep = " ",
                                        quote = FALSE)
                            write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - climate + land use/environment/suitability/HS_",x,".csv"),
                                        col.names = T, row.names = F, sep = " ",
                                        quote = FALSE)
                          }
                          
                          # remove unnecessary objects
                          rm(x)
                        } else {
                          if(substr(species_list[i],32,35) == 2070){
                            for(x in seq_along(2066:2070)+55){
                              write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - land use/environment/suitability/HS_",x,".csv"),
                                          col.names = T, row.names = F, sep = " ",
                                          quote = FALSE)
                              write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - climate + land use/environment/suitability/HS_",x,".csv"),
                                          col.names = T, row.names = F, sep = " ",
                                          quote = FALSE)
                            }
                            
                            # remove unnecessary objects
                            rm(x)
                          } else {
                            if(substr(species_list[i],32,35) == 2075){
                              for(x in seq_along(2071:2075)+60){
                                write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - land use/environment/suitability/HS_",x,".csv"),
                                            col.names = T, row.names = F, sep = " ",
                                            quote = FALSE)
                                write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - climate + land use/environment/suitability/HS_",x,".csv"),
                                            col.names = T, row.names = F, sep = " ",
                                            quote = FALSE)
                              }
                              
                              # remove unnecessary objects
                              rm(x)
                            } else {
                              if(substr(species_list[i],32,35) == 2080){
                                for(x in seq_along(2076:2080)+65){
                                  write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - land use/environment/suitability/HS_",x,".csv"),
                                              col.names = T, row.names = F, sep = " ",
                                              quote = FALSE)
                                  write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - climate + land use/environment/suitability/HS_",x,".csv"),
                                              col.names = T, row.names = F, sep = " ",
                                              quote = FALSE)
                                }
                                
                                # remove unnecessary objects
                                rm(x)
                              } else {
                                if(substr(species_list[i],32,35) == 2085){
                                  for(x in seq_along(2081:2085)+70){
                                    write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - land use/environment/suitability/HS_",x,".csv"),
                                                col.names = T, row.names = F, sep = " ",
                                                quote = FALSE)
                                    write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - climate + land use/environment/suitability/HS_",x,".csv"),
                                                col.names = T, row.names = F, sep = " ",
                                                quote = FALSE)
                                  }
                                  
                                  # remove unnecessary objects
                                  rm(x)
                                } else {
                                  if(substr(species_list[i],32,35) == 2090){
                                    for(x in seq_along(2086:2090)+75){
                                      write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - land use/environment/suitability/HS_",x,".csv"),
                                                  col.names = T, row.names = F, sep = " ",
                                                  quote = FALSE)
                                      write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - climate + land use/environment/suitability/HS_",x,".csv"),
                                                  col.names = T, row.names = F, sep = " ",
                                                  quote = FALSE)
                                    }
                                    
                                    # remove unnecessary objects
                                    rm(x)
                                  } else {
                                    if(substr(species_list[i],32,35) == 2095){
                                      for(x in seq_along(2091:2095)+80){
                                        write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - land use/environment/suitability/HS_",x,".csv"),
                                                    col.names = T, row.names = F, sep = " ",
                                                    quote = FALSE)
                                        write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - climate + land use/environment/suitability/HS_",x,".csv"),
                                                    col.names = T, row.names = F, sep = " ",
                                                    quote = FALSE)
                                      }
                                      
                                      # remove unnecessary objects
                                      rm(x)
                                    } else {
                                      for(x in seq_along(2096:2100)+85){
                                        write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - land use/environment/suitability/HS_",x,".csv"),
                                                    col.names = T, row.names = F, sep = " ",
                                                    quote = FALSE)
                                        write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP1 - climate + land use/environment/suitability/HS_",x,".csv"),
                                                    col.names = T, row.names = F, sep = " ",
                                                    quote = FALSE)
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  } else {
    if(substr(species_list[i],32,35) == 2015){
      for(x in seq_along(2011:2015)){
        write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - land use/environment/suitability/HS_0",x,".csv"),
                    col.names = T, row.names = F, sep = " ",
                    quote = FALSE)
        write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - climate + land use/environment/suitability/HS_0",x,".csv"),
                    col.names = T, row.names = F, sep = " ",
                    quote = FALSE)
      }
      
      # remove unnecessary objects
      rm(x)
    } else {
      if(substr(species_list[i],32,35) == 2020){
        for(x in seq_along(2016:2020)+5){
          if(x < 10){
            write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - land use/environment/suitability/HS_0",x,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
            write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - climate + land use/environment/suitability/HS_0",x,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
          } else {
            write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - land use/environment/suitability/HS_",x,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
            write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - climate + land use/environment/suitability/HS_",x,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
          }
        }
        
        # remove unnecessary objects
        rm(x)
      } else {
        if(substr(species_list[i],32,35) == 2025){
          for(x in seq_along(2021:2025)+10){
            write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - land use/environment/suitability/HS_",x,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
            write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - climate + land use/environment/suitability/HS_",x,".csv"),
                        col.names = T, row.names = F, sep = " ",
                        quote = FALSE)
          }
          
          # remove unnecessary objects
          rm(x)
        } else {
          if(substr(species_list[i],32,35) == 2030){
            for(x in seq_along(2026:2030)+15){
              write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - land use/environment/suitability/HS_",x,".csv"),
                          col.names = T, row.names = F, sep = " ",
                          quote = FALSE)
              write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - climate + land use/environment/suitability/HS_",x,".csv"),
                          col.names = T, row.names = F, sep = " ",
                          quote = FALSE)
            }
            
            # remove unnecessary objects
            rm(x)
          } else {
            if(substr(species_list[i],32,35) == 2035){
              for(x in seq_along(2031:2035)+20){
                write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - land use/environment/suitability/HS_",x,".csv"),
                            col.names = T, row.names = F, sep = " ",
                            quote = FALSE)
                write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - climate + land use/environment/suitability/HS_",x,".csv"),
                            col.names = T, row.names = F, sep = " ",
                            quote = FALSE)
              }
              
              # remove unnecessary objects
              rm(x)
            } else {
              if(substr(species_list[i],32,35) == 2040){
                for(x in seq_along(2036:2040)+25){
                  write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - land use/environment/suitability/HS_",x,".csv"),
                              col.names = T, row.names = F, sep = " ",
                              quote = FALSE)
                  write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - climate + land use/environment/suitability/HS_",x,".csv"),
                              col.names = T, row.names = F, sep = " ",
                              quote = FALSE)
                }
                
                # remove unnecessary objects
                rm(x)
              } else {
                if(substr(species_list[i],32,35) == 2045){
                  for(x in seq_along(2041:2045)+30){
                    write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - land use/environment/suitability/HS_",x,".csv"),
                                col.names = T, row.names = F, sep = " ",
                                quote = FALSE)
                    write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - climate + land use/environment/suitability/HS_",x,".csv"),
                                col.names = T, row.names = F, sep = " ",
                                quote = FALSE)
                  }
                  
                  # remove unnecessary objects
                  rm(x)
                } else {
                  if(substr(species_list[i],32,35) == 2050){
                    for(x in seq_along(2046:2050)+35){
                      write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - land use/environment/suitability/HS_",x,".csv"),
                                  col.names = T, row.names = F, sep = " ",
                                  quote = FALSE)
                      write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - climate + land use/environment/suitability/HS_",x,".csv"),
                                  col.names = T, row.names = F, sep = " ",
                                  quote = FALSE)
                    }
                    
                    # remove unnecessary objects
                    rm(x)
                  } else {
                    if(substr(species_list[i],32,35) == 2055){
                      for(x in seq_along(2051:2055)+40){
                        write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - land use/environment/suitability/HS_",x,".csv"),
                                    col.names = T, row.names = F, sep = " ",
                                    quote = FALSE)
                        write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - climate + land use/environment/suitability/HS_",x,".csv"),
                                    col.names = T, row.names = F, sep = " ",
                                    quote = FALSE)
                      }
                      
                      # remove unnecessary objects
                      rm(x)
                    } else {
                      if(substr(species_list[i],32,35) == 2060){
                        for(x in seq_along(2056:2060)+45){
                          write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - land use/environment/suitability/HS_",x,".csv"),
                                      col.names = T, row.names = F, sep = " ",
                                      quote = FALSE)
                          write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - climate + land use/environment/suitability/HS_",x,".csv"),
                                      col.names = T, row.names = F, sep = " ",
                                      quote = FALSE)
                        }
                        
                        # remove unnecessary objects
                        rm(x)
                      } else {
                        if(substr(species_list[i],32,35) == 2065){
                          for(x in seq_along(2061:2065)+50){
                            write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - land use/environment/suitability/HS_",x,".csv"),
                                        col.names = T, row.names = F, sep = " ",
                                        quote = FALSE)
                            write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - climate + land use/environment/suitability/HS_",x,".csv"),
                                        col.names = T, row.names = F, sep = " ",
                                        quote = FALSE)
                          }
                          
                          # remove unnecessary objects
                          rm(x)
                        } else {
                          if(substr(species_list[i],32,35) == 2070){
                            for(x in seq_along(2066:2070)+55){
                              write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - land use/environment/suitability/HS_",x,".csv"),
                                          col.names = T, row.names = F, sep = " ",
                                          quote = FALSE)
                              write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - climate + land use/environment/suitability/HS_",x,".csv"),
                                          col.names = T, row.names = F, sep = " ",
                                          quote = FALSE)
                            }
                            
                            # remove unnecessary objects
                            rm(x)
                          } else {
                            if(substr(species_list[i],32,35) == 2075){
                              for(x in seq_along(2071:2075)+60){
                                write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - land use/environment/suitability/HS_",x,".csv"),
                                            col.names = T, row.names = F, sep = " ",
                                            quote = FALSE)
                                write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - climate + land use/environment/suitability/HS_",x,".csv"),
                                            col.names = T, row.names = F, sep = " ",
                                            quote = FALSE)
                              }
                              
                              # remove unnecessary objects
                              rm(x)
                            } else {
                              if(substr(species_list[i],32,35) == 2080){
                                for(x in seq_along(2076:2080)+65){
                                  write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - land use/environment/suitability/HS_",x,".csv"),
                                              col.names = T, row.names = F, sep = " ",
                                              quote = FALSE)
                                  write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - climate + land use/environment/suitability/HS_",x,".csv"),
                                              col.names = T, row.names = F, sep = " ",
                                              quote = FALSE)
                                }
                                
                                # remove unnecessary objects
                                rm(x)
                              } else {
                                if(substr(species_list[i],32,35) == 2085){
                                  for(x in seq_along(2081:2085)+70){
                                    write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - land use/environment/suitability/HS_",x,".csv"),
                                                col.names = T, row.names = F, sep = " ",
                                                quote = FALSE)
                                    write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - climate + land use/environment/suitability/HS_",x,".csv"),
                                                col.names = T, row.names = F, sep = " ",
                                                quote = FALSE)
                                  }
                                  
                                  # remove unnecessary objects
                                  rm(x)
                                } else {
                                  if(substr(species_list[i],32,35) == 2090){
                                    for(x in seq_along(2086:2090)+75){
                                      write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - land use/environment/suitability/HS_",x,".csv"),
                                                  col.names = T, row.names = F, sep = " ",
                                                  quote = FALSE)
                                      write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - climate + land use/environment/suitability/HS_",x,".csv"),
                                                  col.names = T, row.names = F, sep = " ",
                                                  quote = FALSE)
                                    }
                                    
                                    # remove unnecessary objects
                                    rm(x)
                                  } else {
                                    if(substr(species_list[i],32,35) == 2095){
                                      for(x in seq_along(2091:2095)+80){
                                        write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - land use/environment/suitability/HS_",x,".csv"),
                                                    col.names = T, row.names = F, sep = " ",
                                                    quote = FALSE)
                                        write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - climate + land use/environment/suitability/HS_",x,".csv"),
                                                    col.names = T, row.names = F, sep = " ",
                                                    quote = FALSE)
                                      }
                                      
                                      # remove unnecessary objects
                                      rm(x)
                                    } else {
                                      for(x in seq_along(2096:2100)+85){
                                        write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - land use/environment/suitability/HS_",x,".csv"),
                                                    col.names = T, row.names = F, sep = " ",
                                                    quote = FALSE)
                                        write.table(a/100,paste0("outputs/",substr(species_list[i],37,nchar(species_list[i])-4),"/SSP5 - climate + land use/environment/suitability/HS_",x,".csv"),
                                                    col.names = T, row.names = F, sep = " ",
                                                    quote = FALSE)
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    
  }
  
  # remove unnecessary objects
  rm(a)
}

# remove unnecessary objects
rm(i,x,species_list,a,z)
invisible(gc())

##### End - Stop Time #####

# start clock
end_f <- Sys.time()

# duration
print("CONCLUDED IN...")
end_f-start_f
