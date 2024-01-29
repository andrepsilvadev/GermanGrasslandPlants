---
title: "codeBLIZ"
subtitle: "Task1.4 - Climate and land-use scenarios (Part 1)"
author: "Afonso Barrocal"
date: "`r Sys.Date()`"
output: 
  html_document:
    toc: true
    toc_float: true
    code_folding: hide
editor_options: 
  chunk_output_type: inline
---
---
references:
- id: Chen2020
  title: Global land use for 2015–2100 at 0.05° resolution under diverse socioeconomic and climate scenarios
  author:
  - family: Chen
    given: Min
  - family: Vernon
    given: Chris R.
  - family: Graham
    given: Neal T.
  - family: Hejazi
    given: Mohamad
  - family: Huang
    given: Maoyi
  - family: Cheng
    given: Yanyan
  - family: Calvin
    given: Katherine
  container-title: Scientific Data
  volume: 7
  URL: 'http://dx.doi.org/10.1038/s41597-020-00669-x'
  DOI: 10.1038/s41597-020-00669-x
  issue: 1
  publisher: Springer Science and Business Media LLC
  type: article-journal
  issued:
    year: 2020
    month: 10
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

colorize <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color,
      x)
  } else x
}
```

# Summary of Task Progress

In **Step 1**, I extracted the maximum and minimum mean temperature and the mean precipitation^[variables ```bio01``` and ```bio12```, respectively.] based on the spatial occurrences of the selected species. The raster layers of environmental variables were downloaded from the [CHELSA Database _Version 2.1_](https://chelsa-climate.org/downloads/), which has bioclimatic variables for the present (1981-2010) and the future (2011-2040, 2041-2070, and 2071-2100, for each of the 3 scenarios^[Scenarios: SSP126, SSP370, and SSP585.]).

In the **Step 2**, I extracted the landuse classes for each of the selected species based on the spatial occurrences. After extracting land use data, the suitability of each species for each of the land use classes was calculated. The landuse data was downloaded from @Chen2020. The **Step 2** can be divided into 3 subtasks:

* Selection of the Year for Extracting Land use Data;
* Land use Data Processing;
* Calculating Suitability.

In **Part 1** of this task, the species selected were only those that had a value other than <b style="font-family: monospace;">`r colorize("NA", "red")`</b> in the ```density_sqMeter_mean``` trait.

`r colorize("All data files necessary can be found", "darkorange")` [here](https://drive.google.com/drive/folders/1QCVFMm3nQ60RlQPFXPCUdth6jKWRl_Fq?usp=drive_link).

## 1 - Extracting Bioclimatic Variables

First, I imported the occurrence data of the selected species.

```{r import_presence_data_and_list, warning=F, results='hide'}
# import data
Amillefolium <- read.delim("occ/Achillea_millefolium_occurrences/0032263-231120084113126.csv",
                           stringsAsFactors=TRUE)
gc() # to empty the memory
Cdiffusa <- read.delim("occ/Centaurea_diffusa_occurrences/0038615-231120084113126.csv",
                       stringsAsFactors=TRUE)
gc() # to empty the memory
Gmollugo <- read.delim("occ/Galium_mollugo_occurrences/0038621-231120084113126.csv",
                       stringsAsFactors=TRUE)
gc() # to empty the memory
Sreticulata <- read.delim("occ/Salix_reticulata_occurrences/0020030-231120084113126.csv",
                          stringsAsFactors=TRUE)
gc() # to empty the memory
Snigrum <- read.delim("occ/Solanum_nigrum_occurrences/0038680-231120084113126.csv",
                      stringsAsFactors=TRUE)
gc() # to empty the memory
Scanadensis <- read.delim("occ/Solidago_canadensis_occurrences/0032266-231120084113126.csv",
                          stringsAsFactors=TRUE)
gc() # to empty the memory

# creating list of species (los)
los <- list(Amillefolium,Cdiffusa,Gmollugo,Sreticulata,Snigrum,Scanadensis)

# removing heavy datasets
rm(Amillefolium,Cdiffusa,Gmollugo,Sreticulata,Snigrum,Scanadensis)
gc() # to empty the memory
```

The environmental variable layers were downloaded from [CHELSA Database _Version 2.1_](https://chelsa-climate.org/downloads/). In order to extract the requested environmental variables, I created the following R function^[In addition to the _Annual Mean Air Temperature_ (```bio01```) and the _Annual Precipitation_ (```bio12```), all bioclimatic variables (variables ```bio01``` to ```bio19```) were included in this function. This has pros and cons. As a plus, we have the easy adaptability of the function to include other bioclimatic variables. As a disadvantage, we have an increase in the time the function takes to run.]:

```{r environmental_variable_values_function, warning=F, results='hide', echo = TRUE}
# environmental variable values function 
getEValues <- function(vector){
  
  # processing dataset
  points <- subset(vector, select = c("decimalLongitude","decimalLatitude"))
  colnames(points) <- c("x","y")
  
  # load required package
  require("terra", quietly = T)
  
  # import environmental variables raster (from the CHELSA database)
  pr <- rast("clim/1981_2010/bio12.tif") # annual precipitation
  gc() # to empty the memory
  tc <- rast("clim/1981_2010/bio01.tif") # annual mean air temperature
  gc() # to empty the memory
  bio02 <- rast("clim/1981_2010/bio02.tif") # mean diurnal air temperature range
  gc() # to empty the memory
  bio03 <- rast("clim/1981_2010/bio03.tif") # isothermality
  gc() # to empty the memory
  bio04 <- rast("clim/1981_2010/bio04.tif") # temperature seasonality
  gc() # to empty the memory
  bio05 <- rast("clim/1981_2010/bio05.tif") # mean daily maximum air temperature of the warmest month
  gc() # to empty the memory
  bio06 <- rast("clim/1981_2010/bio06.tif") # mean daily minimum air temperature of the coldest month
  gc() # to empty the memory
  bio07 <- rast("clim/1981_2010/bio07.tif") # annual range of air temperature
  gc() # to empty the memory
  bio08 <- rast("clim/1981_2010/bio08.tif") # mean daily mean air temperatures of the wettest quarter
  gc() # to empty the memory
  bio09 <- rast("clim/1981_2010/bio09.tif") # mean daily mean air temperatures of the driest quarter
  gc() # to empty the memory
  bio10 <- rast("clim/1981_2010/bio10.tif") # mean daily mean air temperatures of the warmest quarter
  gc() # to empty the memory
  bio11 <- rast("clim/1981_2010/bio11.tif") # mean daily mean air temperatures of the coldest quarter
  gc() # to empty the memory
  bio13 <- rast("clim/1981_2010/bio13.tif") # precipitation amount of the wettest month
  gc() # to empty the memory
  bio14 <- rast("clim/1981_2010/bio14.tif") # precipitation amount of the driest month
  gc() # to empty the memory
  bio15 <- rast("clim/1981_2010/bio15.tif") # precipitation seasonality
  gc() # to empty the memory
  bio16 <- rast("clim/1981_2010/bio16.tif") # mean monthly precipitation amount of the wettest quarter
  gc() # to empty the memory
  bio17 <- rast("clim/1981_2010/bio17.tif") # mean monthly precipitation amount of the driest quarter
  gc() # to empty the memory
  bio18 <- rast("clim/1981_2010/bio18.tif") # mean monthly precipitation amount of the warmest quarter
  gc() # to empty the memory
  bio19 <- rast("clim/1981_2010/bio19.tif") # mean monthly precipitation amount of the coldest quarter
  gc() # to empty the memory
  
  # stacking raster
  EV <- c(tc,pr,bio02,bio03,bio04,bio05,bio06,bio07,bio08,
          bio09,bio10,bio11,bio13,bio14,bio16,bio17,bio18,
          bio19)
  gc() # to empty the memory
  
  # extract values
  points <- as.data.frame(points)
  valuesatpoints <- extract(EV, points)
  gc() # to empty the memory
  points <- cbind(points,valuesatpoints)
  colnames(points) <- c("lon","lat","ID","bio01","bio12",
                        "bio02","bio03","bio04","bio05","bio06","bio07",
                        "bio08","bio09","bio10","bio11","bio13","bio14",
                        "bio16","bio17","bio18","bio19")
  
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
  gc()
  return(climate)
}
```

This is the result for all the selected species:
```{r climatic_variables, warning=F, message = FALSE}
# run the list of species through the function and combine the result into a single dataframe
dataset_climate <- as.data.frame(do.call(rbind, lapply(los,getEValues)));gc()

# see resulting dataframe
rmarkdown::paged_table(dataset_climate)
```

```{r saving_memory_1, include = F, warning=F}
# removing objects to save RAM space
rm(dataset_climate,getEValues)
```

## 2 - Extracting Landuse Classes values and Calculating Suitability

### 2.1 - Selection of the Year for Extracting Land use Data

@Chen2020 provides land use data from 2015 to 2100 at 5-year intervals. Therefore, there are two years that we can use as a baseline for the present (2015 and 2020). In order to understand which year should be used as a baseline, I made histograms of the species' occurrences over the years.

```{r landuse_year, results = 'hide', warning = FALSE, message=FALSE, fig.fullwidth=TRUE, fig.align='center'}
# choosing the year of the model

plantHist <- function(x){
  
  hist(x$year, probability = T, 
       main = paste(unique(x$species), "(All records)"),
       xlab = "Year", col = "green4")
  lines(density(x$year, na.rm = T),
        col = "gold", lwd = 2)
  
}

plantHist19 <- function(x){
  
  hist(x[x$year >= 1900,]$year, probability = T, 
       main = paste(unique(x$species), "\n (From 1900 to Present)"),
       xlab = "Year", col = "green4")
  lines(density(x[x$year >= 1900,]$year, na.rm = T),
        col = "gold", lwd = 2)
  
}

plantHist20 <- function(x){
  
  hist(x[x$year >= 2000,]$year, probability = T, 
       main = paste(unique(x$species), "\n (From 2000 to Present)"),
       xlab = "Year", col = "green4")
  lines(density(x[x$year >= 2000,]$year, na.rm = T),
        col = "gold", lwd = 2)
  
}

par(mfrow = c(2, 3))

lapply(los,plantHist) # from the oldest record to the present
lapply(los,plantHist19) # from 1900 to the present
lapply(los,plantHist20) # from 2000 to the present

par(mfrow = c(1, 1))

rm(plantHist,plantHist19,plantHist20)
gc() # to empty the memory
```

The year that seems to correspond to the baseline for most occurrences is 2020 and, therefore, this is the year that I will use to extract landuse data and calculate suitability.

### 2.2 - Land use Data Processing

The data from @Chen2020 proved challenging to work with as the raster layers contained in the NetCDF files were rotated 90º counterclockwise, as shown in the figure below.

```{r the_problem_with_the_landuse_files_1, results = 'hide', warning = FALSE, message=FALSE, fig.align='center', fig.fullwidth=TRUE}
# load package
library("terra")

# import NetCDF file as Stacked Raster
r <- rast("LU/126/GCAM_Demeter_LU_ssp1_rcp26_modelmean_2020.nc",
            drivers="NETCDF")

# plotting layer
par(mfrow = c(1,1))
plot(r$PFT32, main = "Untransformed Raster Layer",
     xlab = "Latitude", ylab = "Longitude")
```

This problem can be solved in two ways: 

***1. Rotating the raster layer 90º clockwise.*** This is possible by rotating and flipping the layer (as shown in the figure below). However, it is a very computationally demanding process.

```{r the_problem_with_the_landuse_files_2, results = 'hide', warning = FALSE, message=FALSE, fig.align='center', fig.fullwidth=TRUE}
# take longitude and latitude from one species (in this case C. diffusa)
lon <- los[[2]]$decimalLongitude
lat <- los[[2]]$decimalLatitude

# plotting the map
par(mfrow = c(1,1))
plot(flip(t(r$PFT32), direction = "horizontal"), main = "Layer Flipped and Transposed \n with Points Untransformed",
     ylab = "Latitude", xlab = "Longitude")
points(lon,lat)
```

***2. Rotating the coordinates of occurrences.*** This is easily achieved, as shown in the figure below, by using the symmetric of latitude, and exchanging latitude for longitude and vice versa. This process is much faster and is computationally very undemanding.

```{r the_problem_with_the_landuse_files_3, results = 'hide', warning = FALSE, message=FALSE, fig.align='center', fig.fullwidth=TRUE}
# plotting the map
par(mfrow = c(1,1))
plot(r$PFT32, main = "Untransformed Raster Layer with Points Flipped and Transposed",
     xlab = "Latitude", ylab = "Longitude")
points(-lat,lon)
```

For the purposes of extracting land use values, I used the second option.

```{r saving_memory_2, include = F, warning=F}
# removing objects to save RAM space
rm(r,lat,lon)
```

### 2.3 - Calculating Suitability

In order to extract land use values and calculate suitability, I created the following R function:

```{r landuse_values_function, warning=F, results='hide', echo=TRUE}
getLUalues <- function(vector,nc){
  
  # import package
  require("terra", quietly = T)
  
  # import NetCDF as Stacked Raster
  NetCDF <- rast(nc,
          drivers="NETCDF")
  
  # processing dataset
  points <- subset(vector, select = c("decimalLatitude","decimalLongitude"))
  points$decimalLatitude <- -points$decimalLatitude
  colnames(points) <- c("x","y")
  
  # extract values
  points <- as.data.frame(points)
  valuesatpoints <- extract(NetCDF, points)
  gc() # to empty the memory
  points <- cbind(points,valuesatpoints)
  
  # These are the acronyms that are in the article
  colnames(points) <- c("lat","lon","ID","Water",
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
  SI <- (rowSums(t(points[,4:36]), na.rm = T)/max(rowSums(t(points[,4:36]), na.rm = T)));gc()
  
  # making the landuse class columns
  SI <- t(SI)
  
  # creating species column
  empty <- data.frame(`species` = unique(vector$species))
  
  # bringing it all up together
  df_SI <- cbind(empty,SI)
  
  gc()
  return(df_SI)
}
```

This is the result for all the selected species:
```{r landuse_variables, warning=F}
# run the list of species through the function and combine the result into a single dataframe
dataset_landuse <- as.data.frame(do.call(rbind, 
                                         lapply(los,
                                                getLUalues, nc = "LU/126/GCAM_Demeter_LU_ssp1_rcp26_modelmean_2020.nc")));gc()

# see resulting dataframe
rmarkdown::paged_table(dataset_landuse)
```

# References
