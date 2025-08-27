
################################################################
####                                                        ####
####   Script 1.5 - Supplementary tables and plots          ####
####   Date: June 25th, 2025                                ####
####   Author: Afonso Barrocal                              ####
####                                                        ####
################################################################

#### Step 0 - Run previous scripts ####

# run library script
source("./1.4.0_libraries.R")

#### Step 1 - Land-use's tables and plots ####

## Step 1a: Make table ##

# import paths
landuse_list <- append(list.files("./../data/landuse/ssp1_rcp26", pattern = "nc", full.names=T),
                       list.files("./../data/landuse/ssp5_rcp85", pattern = "nc", full.names=T))

# processing paths
landuse_list <- landuse_list[c(1,6,12,18,19,24,30,36)]

# load package
library("sf")

# import shapefile
shape <- read_sf("./../data/unzipped_data/Spatial/Bav&BW.shp")

# load package
library("terra")

# turn shapefile into SpatVector object
shape <- vect(shape)

# creating the datasets
S1_15 <- rep(NA,33)
S1_40 <- rep(NA,33)
S1_70 <- rep(NA,33)
S1_100 <- rep(NA,33)
S2_15 <- rep(NA,33)
S2_40 <- rep(NA,33)
S2_70 <- rep(NA,33)
S2_100 <- rep(NA,33)

# creating for loop
for(x in seq_along(landuse_list)){
  
  # create stacked raster
  n <- nc2tif(landuse_list[x])
  invisible(gc())
  
  # extent
  BavBW <- crop(n, ext(shape))
  
  # masking time
  BavBW <- mask(BavBW,shape)
  a <- c("Water","NET_tem","NET_bor","NDT_bor","BET_tro","BET_tem","BDT_tro",
         "BDT_tem","BDT_bor","BES_tem","BDS_tem","BDS_bor","C3_gra_arc","C3_gra",
         "C4_gra","Corn_rf","Corn_irr","Wheat_rf","Wheat_irr","Soy_rf","Soy_irr",
         "Cotton_rf","Cotton_irr","Rice_rf","Rice_irr","Sugarcrop_rf","Sugarcrop_irr",
         "OtherCrop_rf","OtherCrop_irr","Bioenergy_rf","Bioenergy_irr","Urban","Barren")
  rm(n)
  names(BavBW) <- a
  
  if(x == 1){
    for(i in 1:33){
      S1_15[i] <- sum(values(BavBW[[i]], na.rm = TRUE), na.rm = T)/(nrow(values(BavBW[[i]], na.rm = TRUE))*100)
    }
    rm(i)
    invisible(gc())
  } else {
    if(x == 2){
      for(i in 1:33){
        S1_40[i] <- sum(values(BavBW[[i]], na.rm = TRUE), na.rm = T)/(nrow(values(BavBW[[i]], na.rm = TRUE))*100)
      }
      rm(i)
      invisible(gc())
    } else {
      if(x == 3){
        for(i in 1:33){
          S1_70[i] <- sum(values(BavBW[[i]], na.rm = TRUE), na.rm = T)/(nrow(values(BavBW[[i]], na.rm = TRUE))*100)
        }
        rm(i)
        invisible(gc())
      } else {
        if(x == 4){
          for(i in 1:33){
            S1_100[i] <- sum(values(BavBW[[i]], na.rm = TRUE), na.rm = T)/(nrow(values(BavBW[[i]], na.rm = TRUE))*100)
          }
          rm(i)
          invisible(gc())
        } else {
          if(x == 5){
            for(i in 1:33){
              S2_15[i] <- sum(values(BavBW[[i]], na.rm = TRUE), na.rm = T)/(nrow(values(BavBW[[i]], na.rm = TRUE))*100)
            }
            rm(i)
            invisible(gc())
          } else {
            if(x == 6){
              for(i in 1:33){
                S2_40[i] <- sum(values(BavBW[[i]], na.rm = TRUE), na.rm = T)/(nrow(values(BavBW[[i]], na.rm = TRUE))*100)
              }
              rm(i)
              invisible(gc())
            } else {
              if(x == 7){
                for(i in 1:33){
                  S2_70[i] <- sum(values(BavBW[[i]], na.rm = TRUE), na.rm = T)/(nrow(values(BavBW[[i]], na.rm = TRUE))*100)
                }
                rm(i)
                invisible(gc())
              } else {
                for(i in 1:33){
                  S2_100[i] <- sum(values(BavBW[[i]], na.rm = TRUE), na.rm = T)/(nrow(values(BavBW[[i]], na.rm = TRUE))*100)
                }
                rm(i)
                invisible(gc())
              }
            }
          }
        }
      }
    }
  }
}

# remove unnecessary objects
rm(x)
invisible(gc())

# create dataframe
landuse <- data.frame(`Type` = rep("Land use",66),
                      `Variable` = rep(a,2),
                      `Scenario` = c(rep("SSP1 - RCP 2.6",33),
                                     rep("SSP5 - RCP 8.5",33)),
                      `2015` = c(S1_15,S2_15),
                      `2040` = c(S1_40,S2_40),
                      `2070` = c(S1_70,S2_70),
                      `2100` = c(S1_100,S2_100),
                      `Change1` = rep(NA,66),
                      `Change2` = rep(NA,66),
                      `Change3` = rep(NA,66))

# process dataframe
landuse["X2015"][landuse["X2015"] == "NaN"] <- 0
landuse["X2040"][landuse["X2040"] == "NaN"] <- 0
landuse["X2070"][landuse["X2070"] == "NaN"] <- 0
landuse["X2100"][landuse["X2100"] == "NaN"] <- 0
landuse$Change1 <- round((100*(landuse$X2040-landuse$X2015)/landuse$X2015),3)
landuse$Change2 <- round((100*(landuse$X2070-landuse$X2015)/landuse$X2015),3)
landuse$Change3 <- round((100*(landuse$X2100-landuse$X2015)/landuse$X2015),3)
landuse$X2015 <- round(landuse$X2015*100,3)
landuse$X2040 <- round(landuse$X2040*100,3)
landuse$X2070 <- round(landuse$X2070*100,3)
landuse$X2100 <- round(landuse$X2100*100,3)
landuse["Change1"][landuse["Change1"] == "NaN"] <- 0
landuse["Change2"][landuse["Change2"] == "NaN"] <- 0
landuse["Change3"][landuse["Change3"] == "NaN"] <- 0
colnames(landuse)[c(8,9,10)] <- c("%Change15_40","%Change15_70","%Change15_100")

# remove unnecessary objects
rm(a,S1_15,S1_40,S1_70,S1_100,S2_15,S2_40,S2_70,S2_100,BavBW)
invisible(gc())

## Step 1b: Make plots ##

# processing landuse list
landuse_list <- landuse_list[c(1,4,5,8)]

# import and process data (2015 - 126)
S1_15 <- nc2tif(landuse_list[1])
S1_15 <- S1_15[[c(2,3,9,14,30)]]
S1_15[is.na(S1_15)] <- 0 
S1_15 <- mask(crop(S1_15, ext(shape)),shape)

# import and process data (2100 - 126)
S1_100 <- nc2tif(landuse_list[2])
S1_100 <- S1_100[[c(2,3,9,14,30)]]
S1_100[is.na(S1_100)] <- 0 
S1_100 <- mask(crop(S1_100, ext(shape)),shape)

# create difference raster (126)
S1 <- (S1_100-S1_15)

# process raster (change layers names)
names(S1) <- c("NET_tem","NET_bor","BDT_bor","C3_gra","Bioenergy_rf")

# remove unnecessary objects
rm(S1_15,S1_100)
invisible(gc())

# import and process data (2015 - 585)
S2_15 <- nc2tif(landuse_list[3])
S2_15 <- S2_15[[c(2,3,9,14,30)]]
S2_15[is.na(S2_15)] <- 0 
S2_15 <- mask(crop(S2_15, ext(shape)),shape)

# import and process data (2100 - 585)
S2_100 <- nc2tif(landuse_list[4])
S2_100 <- S2_100[[c(2,3,9,14,30)]]
S2_100[is.na(S2_100)] <- 0 
S2_100 <- mask(crop(S2_100, ext(shape)),shape)

# create difference raster (585)
S2 <- (S2_100-S2_15)

# process raster (change layers names)
names(S2) <- c("NET_tem","NET_bor","BDT_bor","C3_gra","Bioenergy_rf")

# remove unnecessary objects
rm(S2_15,S2_100)
invisible(gc())

# create directory to save files
dir.create("./../output/supplementary")

# plotting
clockifyR(
  png("./../output/supplementary/lu_126.png",
      height = 240-24*3, width = 480-48*3,
      res = 300, units = "mm"),
  plot(S1, main = names(S1),
       mar = c(2, 1.5, 5, 4.5),
       range = c(-100,100),
       col = colorspace::diverging_hcl(300, h = c(250, 10), c = 100, l = c(37, 88), power = c(0.7, 1.7))),
  par(mfrow = c(1,1)),
  title(main = "Changes in main land use classes (%)\n(SSP1 - RCP 2.6: 2015 to 2100)",
        cex.main = 1, line = 2),
  dev.off(),
  png("./../output/supplementary/lu_585.png",
      height = 240-24*3, width = 480-48*3,
      res = 300, units = "mm"),
  plot(S2, main = names(S2),
       mar = c(2, 1.5, 5, 4.5),
       range = c(-100,100),
       col = colorspace::diverging_hcl(300, h = c(250, 10), c = 100, l = c(37, 88), power = c(0.7, 1.7))),
  par(mfrow = c(1,1)),
  title(main = "Changes in main land use classes (%)\n(SSP5 - RCP 8.5: 2015 to 2100)",
        cex.main = 1, line = 2),
  dev.off()
  )

#### Step 2 - Climate's tables and plots ####

## Step 2a: Make table ##

# create sample raster
sample <- sum(S1, na.rm = TRUE)/sum(S1, na.rm = TRUE)

# remove unnecessary objects
rm(S1,S2,landuse_list)
invisible(gc())

# download and process climate data (Year-one: 2015)
climate15 <- c(chelsa4r("tas",2015),
               chelsa4r("pr",2015))
climate15 <- crop(climate15, ext(shape))
climate15 <- mask(climate15,shape)

# download and process climate data (Year-one: 2015)
S1_40 <- c(chelsa4r30("bio1","2011-2040",126),
           chelsa4r30("bio12","2011-2040",126))
S1_40 <- crop(S1_40, ext(shape))
S1_40 <- mask(S1_40,shape)

# download and process climate data (Year-one: 2015)
S1_70 <- c(chelsa4r30("bio1","2041-2070",126),
           chelsa4r30("bio12","2041-2070",126))
S1_70 <- crop(S1_70, ext(shape))
S1_70 <- mask(S1_70,shape)

# download and process climate data (Year-one: 2015)
S1_100 <- c(chelsa4r30("bio1","2071-2100",126),
            chelsa4r30("bio12","2071-2100",126))
S1_100 <- crop(S1_100, ext(shape))
S1_100 <- mask(S1_100,shape)

# download and process climate data (Year-one: 2015)
S2_40 <- c(chelsa4r30("bio1","2011-2040",585),
           chelsa4r30("bio12","2011-2040",585))
S2_40 <- crop(S2_40, ext(shape))
S2_40 <- mask(S2_40,shape)

# download and process climate data (Year-one: 2015)
S2_70 <- c(chelsa4r30("bio1","2041-2070",585),
           chelsa4r30("bio12","2041-2070",585))
S2_70 <- crop(S2_70, ext(shape))
S2_70 <- mask(S2_70,shape)

# download and process climate data (Year-one: 2015)
S2_100 <- c(chelsa4r30("bio1","2071-2100",585),
            chelsa4r30("bio12","2071-2100",585))
S2_100 <- crop(S2_100, ext(shape))
S2_100 <- mask(S2_100,shape)

# create dataframe
bioclim <- data.frame(`Type` = rep("Climate",4),
                      `Variable` = c(rep("tas",2),
                                     rep("pr",2)),
                      `Scenario` = rep(c("SSP1 - RCP 2.6",
                                         "SSP5 - RCP 8.5"),
                                       2),
                      `X2015` = rep(NA,4),
                      `X2040` = rep(NA,4),
                      `X2070` = rep(NA,4),
                      `X2100` = rep(NA,4),
                      `Change1` = rep(NA,4),
                      `Change2` = rep(NA,4),
                      `Change3` = rep(NA,4))

# giving values
bioclim$X2015[1] <- round(mean(values(climate15[[1]], na.rm = TRUE), na.rm = T),3)
bioclim$X2040[1] <- round(mean(values(S1_40[[1]], na.rm = TRUE), na.rm = T),3)
bioclim$X2070[1] <- round(mean(values(S1_70[[1]], na.rm = TRUE), na.rm = T),3)
bioclim$X2100[1] <- round(mean(values(S1_100[[1]], na.rm = TRUE), na.rm = T),3)
bioclim$X2015[3] <- round(mean(values(climate15[[2]], na.rm = TRUE), na.rm = T),3)
bioclim$X2040[3] <- round(mean(values(S1_40[[2]], na.rm = TRUE), na.rm = T),3)
bioclim$X2070[3] <- round(mean(values(S1_70[[2]], na.rm = TRUE), na.rm = T),3)
bioclim$X2100[3] <- round(mean(values(S1_100[[2]], na.rm = TRUE), na.rm = T),3)
bioclim$X2015[2] <- round(mean(values(climate15[[1]], na.rm = TRUE), na.rm = T),3)
bioclim$X2040[2] <- round(mean(values(S2_40[[1]], na.rm = TRUE), na.rm = T),3)
bioclim$X2070[2] <- round(mean(values(S2_70[[1]], na.rm = TRUE), na.rm = T),3)
bioclim$X2100[2] <- round(mean(values(S2_100[[1]], na.rm = TRUE), na.rm = T),3)
bioclim$X2015[4] <- round(mean(values(climate15[[2]], na.rm = TRUE), na.rm = T),3)
bioclim$X2040[4] <- round(mean(values(S2_40[[2]], na.rm = TRUE), na.rm = T),3)
bioclim$X2070[4] <- round(mean(values(S2_70[[2]], na.rm = TRUE), na.rm = T),3)
bioclim$X2100[4] <- round(mean(values(S2_100[[2]], na.rm = TRUE), na.rm = T),3)

# remove unnecessary objects
rm(S1_40,S1_70,S2_40,S2_70)
invisible(gc())

# process dataframe
bioclim["X2015"][bioclim["X2015"] == "NaN"] <- 0
bioclim["X2040"][bioclim["X2040"] == "NaN"] <- 0
bioclim["X2070"][bioclim["X2070"] == "NaN"] <- 0
bioclim["X2100"][bioclim["X2100"] == "NaN"] <- 0
bioclim$Change1 <- round(((100*(bioclim$X2040-bioclim$X2015))/bioclim$X2015),3)
bioclim$Change2 <- round(((100*(bioclim$X2070-bioclim$X2015))/bioclim$X2015),3)
bioclim$Change3 <- round(((100*(bioclim$X2100-bioclim$X2015))/bioclim$X2015),3)
bioclim["Change1"][bioclim["Change1"] == "NaN"] <- 0
bioclim["Change2"][bioclim["Change2"] == "NaN"] <- 0
bioclim["Change3"][bioclim["Change3"] == "NaN"] <- 0
colnames(bioclim)[c(8,9,10)] <- c("%Change15_40","%Change15_70","%Change15_100")

# create final dataset
dataset <- rbind(bioclim,landuse)

# remove unnecessary objects
rm(bioclim,landuse)
invisible(gc())

# load package
library("writexl")

# export final dataset (excel)
write_xlsx(dataset,"./../output/supplementary/table.xlsx")

# export final dataset (csv)
write.csv(dataset,"./../output/supplementary/table.csv")

## Step 2b: Make plots ##

# calculate change
S1 <- (100*(S1_100-climate15))/climate15
S2 <- (100*(S2_100-climate15))/climate15

# remove unnecessary objects
rm(S1_100,S2_100,climate15)
invisible(gc())

# process data (change resolution)
S1 <- resample(S1,sample, method = "average")
S2 <- resample(S2,sample, method = "average")

# remove unnecessary objects
rm(sample)
invisible(gc())

# process data (change names)
names(S1) <- c("tas","pr")
names(S2) <- c("tas","pr")

# process data (crop and mask)
S1 <- crop(S1, ext(shape))
S1 <- mask(S1,shape)
S2 <- crop(S2, ext(shape))
S2 <- mask(S2,shape)

# plotting
clockifyR(
  png("./../output/supplementary/clim_126.png",
      height = 240-24*3, width = 480-48*3,
      res = 300, units = "mm"),
  par(mfrow = c(1,2)),
  plot(S1[[1]], main = names(S1)[1],
       mar = c(2, 1.5, 5, 4.5),
       range = c(-100,100),
       col = colorspace::diverging_hcl(300, h = c(250, 10), c = 100, l = c(37, 88), power = c(0.7, 1.7))),
  plot(S1[[2]], main = names(S1)[2],
       mar = c(2, 1.5, 5, 4.5),
       range = c(1000,1800),
       col = colorspace::diverging_hcl(300, h = c(250, 10), c = 100, l = c(37, 88), power = c(0.7, 1.7))),
  par(mfrow = c(1,1)),
  title(main = "Changes in climate (%)\n(SSP1 - RCP 2.6: 2015 to 2100)",
        cex.main = 1, line = 2),
  dev.off(),
  png("./../output/supplementary/clim_585.png",
      height = 240-24*3, width = 480-48*3,
      res = 300, units = "mm"),
  par(mfrow = c(1,2)),
  plot(S2[[1]], main = names(S2)[1],
       mar = c(2, 1.5, 5, 4.5),
       range = c(-100,100),
       col = colorspace::diverging_hcl(300, h = c(250, 10), c = 100, l = c(37, 88), power = c(0.7, 1.7))),
  plot(S2[[2]], main = names(S2)[2],
       mar = c(2, 1.5, 5, 4.5),
       range = c(1000,1800),
       col = colorspace::diverging_hcl(300, h = c(250, 10), c = 100, l = c(37, 88), power = c(0.7, 1.7))),
  par(mfrow = c(1,1)),
  title(main = "Changes in climate (%)\n(SSP5 - RCP 8.5: 2015 to 2100)",
        cex.main = 1, line = 2),
  dev.off()
)

# remove unnecessary objects
rm(S1,S2,shape,dataset)
invisible(gc())
