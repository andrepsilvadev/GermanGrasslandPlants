## ----echo=false----------------------------------------------------------------------------------------------------------------

plants_bavaria <- readLines("Data/Rote_List_Bayern.csv") #read csv as a text object

simplified_plants = paste(plants_bavaria, collapse = " ") #simplify data by collapsing everything with a space as delimiter

#run a for loop where we are looking for every "space + capital letter" and want to replace with a "new line + capital letter" - this was necessary because we had multiple species listed in one row
for (x in LETTERS) {
simplified_plants = gsub(paste0(" ",x), paste0("\n",x), simplified_plants)
}

#formatting data
sp_bavaria_red = 
  simplified_plants |>
  strsplit(split = "\n") |> #split lines in substrings
  unlist() |>
  gsub(paste0(" ","´"), paste0(""), x = _) |> #fix text (eliminate "´")
  gsub(paste0(" ","x"), paste0(""), x = _) |> #fix text (eliminate "x")
  unique() #remove duplicates

sp_bavaria_red <- data.frame(sp_bavaria_red) #transform into dataframe
colnames(sp_bavaria_red) <- c('species') #change name of id column


## ----echo=false----------------------------------------------------------------------------------------------------------------

GBIF_bav <- read.csv("Data/SpeciesList_Bavaria.csv", sep = "\t") #read file

sp_bav <- GBIF_bav[c("species")] #select column with species names
sp_bav <- sp_bav[sp_bav$species != "", , drop=FALSE] #eliminate empty rows


## ----echo=false----------------------------------------------------------------------------------------------------------------

GBIF_baden <- read.csv("Data/SpeciesList_Baden.csv", sep = "\t") #read file

sp_baden <- GBIF_baden[c("species")] #select column with species names
sp_baden <- sp_baden[sp_baden$species != "", , drop=FALSE]#eliminate empty rows


## ----echo=false----------------------------------------------------------------------------------------------------------------

GBIF_Species <- merge(sp_bav,sp_baden, by = "species", all = TRUE) #merge both lists from GBIF
BB_Species <- merge(GBIF_Species,sp_bavaria_red, by = "species", all = TRUE) #merge GBIF data with Red List data

length(unique(BB_Species$species)) #calculate number of candidate species


## ----echo=false----------------------------------------------------------------------------------------------------------------

seed_number <- read.csv2("Data/seed_number.txt", quote = "", dec = ".") #read file

unique(seed_number$reproduction.unit.measured) #check units measured
unique(seed_number$general.method) #check methods used

SeedNumber <- subset(seed_number, reproduction.unit.measured == 'per ramet/tussock or individual plant ') #filter rows with reproduction.unit.measured = 'per ramet/tussock or individual plant '
SeedNumber <- SeedNumber[c("SBS.name","single.value","general.method","reproduction.unit.measured")] #select columns with species, values, methods and units
names(SeedNumber)[names(SeedNumber) == 'SBS.name'] <- 'species' #change column name
names(SeedNumber)[names(SeedNumber) == 'single.value'] <- 'seed_number' #change column name

str(SeedNumber) #check if values are all numeric

merge_seeds <- merge(BB_Species, SeedNumber, by = "species") #merge data with candidate species list

length(unique(merge_seeds$species)) #check the number of species with data for this trait


## ----echo=false----------------------------------------------------------------------------------------------------------------

germination_rate <- read.delim("Data/SeedGermination.txt") #read file

SeedGermination <- subset(germination_rate, TraitID == 95) # filter rows with germination rate values
SeedGermination <- SeedGermination[SeedGermination$StdValue != "", , drop=FALSE] #eliminate empty rows

unique(SeedGermination$UnitName) #check units measured
unique(SeedGermination$OriglName) #check methods used

SeedGermination <- SeedGermination[c("AccSpeciesName","OriglName","StdValue","UnitName")] #select columns with species, values, methods and units
names(SeedGermination)[names(SeedGermination) == 'AccSpeciesName'] <- 'species' #change name
names(SeedGermination)[names(SeedGermination) == 'OriglName'] <- 'germination_method' #change name
names(SeedGermination)[names(SeedGermination) == 'StdValue'] <- 'germination_perc' #change name
names(SeedGermination)[names(SeedGermination) == 'UnitName'] <- 'germination_unit' #change name

str(SeedGermination) #check if values are all numeric

merge_germination_rate <- merge(BB_Species, SeedGermination, by = "species") #merge data with candidate species list


## ----echo=false----------------------------------------------------------------------------------------------------------------
genus_list_ger <- read.csv("Data/genus_list_Germination.csv") #read file

mean_germination_sp <- aggregate(germination_perc~species, data=SeedGermination, mean) #calculate the mean germination rate for each species, because there are multiple values

for (row in seq_len(nrow(genus_list_ger))) { #for each row in the genus list
  curr_genus = genus_list_ger[row, "genus"] #get each genus
  curr_sp = genus_list_ger[row, "sp"] #get each species
  
  curr_genus_subset = mean_germination_sp[grep(curr_genus, mean_germination_sp$species), ] #create a subset of data for the current genus
  curr_genus_nrows = nrow(curr_genus_subset)
  
  curr_df = data.frame(species = rep(curr_sp, curr_genus_nrows), germination_method = rep("%Germination", curr_genus_nrows), germinaton_perc = curr_genus_subset$germination_perc, germination_unit = rep("%", curr_genus_nrows)) #create dataframe with new data
  names(curr_df) <- names(SeedGermination) #make sure column names are the same as the original dataframe
  
  SeedGermination <- rbind(SeedGermination, curr_df) #merge new data with original dataframe
}



## ----echo=false----------------------------------------------------------------------------------------------------------------

density <- read.delim("Data/planting_density.txt") #read file

PDensity <- subset(density, DataID == 1320) # filter rows with maximum planting density - the database has other information

unique(PDensity$OrigUnitStr) #check units measured

PDensity <- PDensity[c("AccSpeciesName","OriglName","OrigValueStr","OrigUnitStr")] #select columns with species, values, methods and units
names(PDensity)[names(PDensity) == 'AccSpeciesName'] <- 'species' #change name
names(PDensity)[names(PDensity) == 'OriglName'] <- 'density_method' #change name
names(PDensity)[names(PDensity) == 'OrigValueStr'] <- 'density_acre' #change name
names(PDensity)[names(PDensity) == 'OrigUnitStr'] <- 'density_unit' #change name

str(PDensity) #check if values are all numeric

merge_density <- merge(BB_Species, PDensity, by = "species") #merge data with candidate species list


## ----echo=false----------------------------------------------------------------------------------------------------------------
genus_list_den <- read.csv2("Data/genus_list_PDensity.csv") #read file

for (row in seq_len(nrow(genus_list_den))) { #for each row in the genus list
  curr_genus = genus_list_den[row, "genus"] #get each genus
  curr_sp = genus_list_den[row, "sp"] #get each species
  
  curr_genus_subset = PDensity[grep(curr_genus, PDensity$species), ] #create a subset of data for the current genus
  curr_genus_nrows = nrow(curr_genus_subset)
  
  curr_df = data.frame(species = rep(curr_sp, curr_genus_nrows), density_method = rep("Planting Density per Acre, Maximum", curr_genus_nrows), density_acre = curr_genus_subset$density_acre, density_unit = rep("1/acre", curr_genus_nrows)) #create dataframe with new data
  names(curr_df) <- names(PDensity) #make sure column names are the same as the original dataframe
  
  PDensity <- rbind(PDensity, curr_df) #merge new data with original dataframe
}


#convert plants/acre to plants/m2
# 1 acre = 4046.86 m2
PDensity$density_sqMeter = PDensity$density_acre/4046.86


## ----echo=false----------------------------------------------------------------------------------------------------------------

plant_fresh_mass <- read.delim("Data/PlantFreshMass.txt") #read file

PlantFreshMass <- subset(plant_fresh_mass, TraitID == 3407) #filter rows with fresh mass values - the database has other information

unique(PlantFreshMass$OrigUnitStr) #check units measured
unique(PlantFreshMass$DataName) #check methods used

PlantFreshMass <- PlantFreshMass[c("AccSpeciesName","DataName","OrigValueStr","OrigUnitStr")] #select columns with species, values, methods and units
names(PlantFreshMass)[names(PlantFreshMass) == 'AccSpeciesName'] <- 'species' #change name
names(PlantFreshMass)[names(PlantFreshMass) == 'DataName'] <- 'P_part' #change name
names(PlantFreshMass)[names(PlantFreshMass) == 'OrigValueStr'] <- 'wetmass_g' #change name
names(PlantFreshMass)[names(PlantFreshMass) == 'OrigUnitStr'] <- 'Unit' #change name

str(PlantFreshMass) #check if values are all numeric
#if not numeric, go check if any values are wrong (no problems)
PlantFreshMass$wetmass_g <- as.numeric(PlantFreshMass$wetmass_g) #convert column with values to numeric instead of character

merge_fresh_mass <- merge(BB_Species, PlantFreshMass, by = "species") #merge data with candidate species list


## ----echo=false----------------------------------------------------------------------------------------------------------------

plantNE_data <- read.csv("Data/PlantNE_Biomass.csv", dec = ".") #read file

unique(plantNE_data$Unit) #check units measured
unique(plantNE_data$P_part) #check part of the plant measured

plantNE <- subset(plantNE_data, Unit %in% c('g','g ') & P_part == 'WP') #filter rows with unit = "g" and plant part = "whole plant"
plantNE <- plantNE[c("Species","CK_M","Unit","P_part")] #select columns with species, values, methods and units
names(plantNE)[names(plantNE) == 'Species'] <- 'species' #change name
names(plantNE)[names(plantNE) == 'CK_M'] <- 'wetmass_g' #change name

str(plantNE) #check if values are all numeric

merge_plantNE <- merge(BB_Species, plantNE, by = "species") #merge data with candidate species list


## ----echo=false----------------------------------------------------------------------------------------------------------------

Wetmass <- rbind(PlantFreshMass, plantNE)

length(unique(Wetmass$species)) #check the number of species with data for this trait


## ----echo=false----------------------------------------------------------------------------------------------------------------

Wetmass$mortality <- Wetmass$wetmass_g^-0.25 #mortality equation by Marba et al. (2007)

Wetmass <- subset(Wetmass, !wetmass_g<1) #exclude species with wetmass < 1g


## ----echo=false----------------------------------------------------------------------------------------------------------------

dispersal <- read.csv2("Data/Lososova_et_al_2023_Dispersal.csv") #read file

Dispersal <- dispersal[c("Taxon","Dispersal.distance.class..1.6.")] #select columns with species and dispersal class
names(Dispersal)[names(Dispersal) == 'Taxon'] <- 'species' #change name
names(Dispersal)[names(Dispersal) == 'Dispersal.distance.class..1.6.'] <- 'dispersal_class_1-6' #change name

##convert class data in mean distance and max distance
dispersal_key = read.csv("Data/dispersal_classes.csv") #read file with dispersal classes and respective mean and max values

#assign max and mean values for each row
for (row in seq_len(nrow(dispersal_key))) {
    curr_class = dispersal_key[row, "class"]
    curr_mean = dispersal_key[row, "mean_dist_m"]
    curr_max = dispersal_key[row, "max_dist_m"]
    
    Dispersal$mean_dist_m[Dispersal$'dispersal_class_1-6' == curr_class] = curr_mean
    Dispersal$max_dist_m[Dispersal$'dispersal_class_1-6' == curr_class] = curr_max
}

str(Dispersal) #check if values are all numeric

merge_dispersal <- merge(BB_Species, Dispersal, by = "species") #merge data with candidate species list

length(unique(merge_dispersal$species)) #check the number of species with data for this trait



## ----echo=false----------------------------------------------------------------------------------------------------------------
library(Rcompadre)

load("Data/COMPADRE_v.6.23.5.0.RData") #load compadre database
compadre <- as_cdb(compadre) #ensure that the database is of the correct class

genus_list_Lambda <- read.csv("Data/genus_list_Lambda.csv") #list with species and genus to look for

sub_genus <- subset(compadre, Genus %in% genus_list_Lambda$genus) #subset database for our genus
unique(sub_genus$MatrixTreatment) #check methods

sub_genus2 <- subset(sub_genus, MatrixTreatment == "Unmanipulated") #filter rows with only unmanipulated values
unique(sub_genus2$Genus) #check genus with information
unique(sub_genus2$SpeciesAccepted) #check species with information

lambdaVals <- sapply(matA(sub_genus2), popdemo::eigs, what = "lambda") #calculate lambda values

Lambda <- data.frame(sub_genus2$SpeciesAccepted, lambdaVals) #create dataframe with lambda values

names(Lambda)[names(Lambda) == 'sub_genus2.SpeciesAccepted'] <- 'species' #change name


## ----echo=false----------------------------------------------------------------------------------------------------------------

for (row in seq_len(nrow(genus_list_Lambda))) { #for each row in the genus list
  curr_genus = genus_list_Lambda[row, "genus"] #get each genus
  curr_sp = genus_list_Lambda[row, "sp"] #get each species
  
  curr_genus_subset = Lambda[grep(curr_genus, Lambda$species), ] #create a subset of data for the current genus
  curr_genus_nrows = nrow(curr_genus_subset)
  
  curr_df = data.frame(species = rep(curr_sp, curr_genus_nrows), lambdaVals = curr_genus_subset$lambdaVals) #create dataframe with new data
  
  Lambda <- rbind(Lambda, curr_df) #merge new data with original dataframe
}


## ----echo=false----------------------------------------------------------------------------------------------------------------

traits_all <- 
  BB_Species |> 
  merge(SeedNumber, by = "species") |>
  merge(SeedGermination, by = "species") |>
  merge(PDensity, by = "species") |>
  merge(Wetmass, by = "species") |>
  merge(Dispersal, by = "species") |>
  merge(Lambda, by = "species")
  
unique(traits_all$species)  #check the number of species with data for all traits

print(traits_all)


## ----echo=false----------------------------------------------------------------------------------------------------------------

#select only columns with the values for final table
simplified_traits <- traits_all[,c("species","seed_number","germination_perc","density_sqMeter","wetmass_g","mortality","mean_dist_m","max_dist_m","lambdaVals")]


#create final table
final <- simplified_traits[seq_along(unique(simplified_traits$species)),]

#delete data from table
final[TRUE, TRUE] = NA

#add species to column
final$species = unique(simplified_traits$species)

#duplicate trait columns
final <- cbind(final, final[, c(2:9)])

#change names
colnames(final)[2:9] = paste0(colnames(final)[2:9], "_mean")
colnames(final)[10:17] = paste0(colnames(final)[10:17], "_std")


#add traits

for (row in seq_len(nrow(final))) { #for each row (aka species) in the final table
  sp = final$species[row] #get the species name
  sp_rows = simplified_traits[simplified_traits$species == sp,] #get the rows for that species from the simplified_traits dataframe

  for (col in colnames(sp_rows)) { #for each column (aka trait)
    if (col == "species") next #skip species column

    if (length(unique(sp_rows[,col])) == 1) { #if there is only one unique value for that trait
      final[row, paste0(col, "_mean")] = sp_rows[1,col] #the mean is that number
final[row, paste0(col, "_std")] = 0 #the std is 0
    } else { #if there are multiple values
      if (is.character(sp_rows[,col])) { #must be numeric, else stop function
        stop("You have a character value in this column!")
      } else { #if is numeric
        final[row, paste0(col, "_mean")] = mean(sp_rows[,col]) #calculate mean
final[row, paste0(col, "_std")] = sd(sp_rows[,col]) #calculate std
      }
    }
  }
}

print(final)

write.csv(final, "Results/trait_data.csv")


## ----echo=false----------------------------------------------------------------------------------------------------------------
sp_list <- data.frame(unique(traits_all$species)) #get species names included in the complete data
names(sp_list) <- 'species' #change column name


## ----echo=false----------------------------------------------------------------------------------------------------------------

gbif_bav <- merge(sp_list, GBIF_bav, by = "species") #get occurrence data from Bavaria for the species included in the complete data

library(stats) #for aggregate function

occur_sp_bav <- aggregate(numberOfOccurrences ~ species, data=gbif_bav, sum) #calculate number of occurrences for each species

names(occur_sp_bav)[names(occur_sp_bav) == 'numberOfOccurrences'] <- 'numberOfOccurrences_bav' #change column name


## ----echo=false----------------------------------------------------------------------------------------------------------------

gbif_bw <- merge(sp_list, GBIF_baden, by = "species") #get occurrence data from BW for the species included in the complete data

occur_sp_bw <- aggregate(numberOfOccurrences ~ species, data=gbif_bw, sum) #calculate number of occurrences for each species

names(occur_sp_bw)[names(occur_sp_bw) == 'numberOfOccurrences'] <- 'numberOfOccurrences_bw' #change column name


## ----echo=false----------------------------------------------------------------------------------------------------------------

occur <- merge(occur_sp_bav, occur_sp_bw, by = "species", all=TRUE) #merge occurrences from Bavaria and BW

occur$numberOfOccurrences <- rowSums(cbind(occur$numberOfOccurrences_bav,occur$numberOfOccurrences_bw),na.rm=TRUE) #sum occurrences

print(occur)

write.csv(occur, "Results/Occurrences_bav&bw.csv") #download occurrence file


## ----echo=false----------------------------------------------------------------------------------------------------------------

GBIF_world <- read.csv(unzip("Data/SpeciesList_Tracheophyta_worldwide.zip"), sep = "\t", quote = "") #read file

gbif_world <- merge(sp_list, GBIF_world, by = "species") #get occurrence data worldwide for the species included in the complete data

occur_sp_ww <- aggregate(numberOfOccurrences ~ species, data=gbif_world, sum) #calculate number of occurrences for each species

print(occur_sp_ww)

write.csv(occur_sp_ww, "Results/Occurrences_worldwide.csv") #download occurrence file


## ----setup, include=FALSE------------------------------------------------------------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)

