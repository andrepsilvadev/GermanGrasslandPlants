# BLIZ TASK 1.6 #
## DATA FROM THE 24TH JULY#
### ONLY THE SENSITIVITY ANALYSIS PORTION OF THE SCRIPT ###

# !!! NAVIGATION WARNING !!! #

# do not use rbase or tidyr functions to import or transform these data
# datasets are too big even for the servers and r will show Fatal errors
# use data.table package's functions to perform any transformation to dataframes and the fread function to import that dataset

remove.packages("lifecycle")
install.packages("lifecycle")
remove.packages("tidyr")
install.packages("tidyr")
remove.packages("rlang")
install.packages("rlang")

# Packages
library(rlang)
library(readr)
library(dplyr)
library(collapse)
library(fs)
library(tidyverse) 
library(tidyr)
library(readxl)
library(writexl)
library(ggplot2)
library(raster)
library(gridExtra)
library(stringr)
library(data.table)
library(bit64) #for the integer64 columns
library(purrr)

start.time <- Sys.time() #start the clock

#set the path to the folder with sensitivity runs subfolders
main_folder <- "~/Desktop/sensitivity_runs"

#get the list of all subfolders
subfolders <- list.dirs(main_folder, full.names = T, recursive = T)

#initiallize lists to store data frames
data_105 <- list()
data_095 <- list()
data_control <- list()

#loop through each subfolder
for (subfolder in subfolders) {
  #get the name of the subfolder
  folder_name <- basename(subfolder)
  
  #get the path to the tsv files
  tsv_file <- list.files(subfolder, pattern = "\\.tsv$", full.names = TRUE)
  
  if(length(tsv_file) == 1 ){
    data <- fread(tsv_file) #avoid using the read.delim function because it takes too loog and uses too much space fread is a much better option
    data$source_folder <- folder_name
    
    #separate the data based on the folder name that contains either 105 or 95 or nothing meaning that we changed the variables by +5%, or -5% or it is the control
    if(grepl("control_sensitivity_105", folder_name)) {
      data_105[[folder_name]] <- data 
    } else if(grepl("control_sensitivity_095", folder_name)) {
      data_095[[folder_name]] <- data
    } else {
      data_control[[folder_name]] <- data
    }
    
  }
}
gc()

#bind all tables from each list together for both the +5%, -5% and control sensitivity runs
combined_data_105 <- data.table::rbindlist(data_105)
combined_data_095 <- data.table::rbindlist(data_095)
datacontrol <- data_control[grep("control", names(data_control))]
combined_data_control <- data.table::rbindlist(datacontrol)


#separate the source folder column into multiple one and exclude the not necessary ones for 105 sensitivity
combined_data_105 <- combined_data_105 %>%
  separate(source_folder, c("date", "time", "species", "scenario", "word", "SR", "VAR_CHANGED"), sep = "_", remove = FALSE) %>% #takes a VERY long time ^(more than 40 minutes)
  dplyr::select(-c("source_folder", "date", "time", "scenario", "word"))
gc() #remove unecessary memory

#separate the source folder column into multiple one and exclude the not necessary ones for 095 sensitivity
combined_data_095 <- combined_data_095 %>%
  separate(source_folder, c("date", "time", "species", "scenario", "word", "SR", "VAR_CHANGED"), sep = "_", remove = FALSE) %>% #takes a VERY long time ^(more than 40 minutes)
  dplyr::select(-c("source_folder", "date", "time", "scenario", "word"))
gc() #remove unecessary memory

#separate the source folder column into multiple one and exclude the not necessary ones for the control
combined_data_control <- combined_data_control %>%
  separate(source_folder, c("date", "time", "species", "scenario"), sep = "_", remove = FALSE) %>% #takes a VERY long time ^(more than 40 minutes)
  dplyr::select(-c("source_folder", "date", "time", "scenario"))
gc() #remove unecessary memory

#remove previous dataframes to save space
rm(data_control)
rm(datacontrol) 


## FOR THE 105 DATA ##

#split all +5% sensitivity runs into 6 separate dataframes (one for each of the parameters changed)
split_combined_105 <- split(combined_data_105, f = combined_data_105$VAR_CHANGED)
rm(combined_data_105) #remove previous dataframe to save space

#create a function to add the original column name + the name of the dataframe (which corresponds to the paramters changed by x%) + "_105" to each column name
add_suffix_105 <- function(df, name) {
  col_indices <-4:8
  if(max(col_indices) <= ncol(df)){
    colnames(df)[col_indices] <- paste0(colnames(df)[col_indices], "_", name, "_105")
  } else {
    warning(paste("Data frame", name, "does not have enough collumns"))
  }
  return(df)
}

#apply the function to each dataframe inside the large list
split_combined_105 <-lapply(names(split_combined_105), function(name){
  add_suffix_105(split_combined_105[[name]], name)
})
#restore names of each dataframe and add "_105"
names_105 <- c("bevmort_105", "carry_105", "growrate_105", "mass_105", "max_dispersal_dis_105", "mean_dispersal_dis_105")
names(split_combined_105) <- names_105
#remove VAR_CHANGED variable
split_combined_105 <- purrr::map(split_combined_105, dplyr::select, -"VAR_CHANGED") 
split_combined_105 <- purrr::map(split_combined_105, dplyr::select, -"SR") 
#common columns between dataframes
common_columns <- c("t", "x", "y", "species")

#combine all dataframes with correct column names that state the variable_"ChangedParameter"_"ByHowMuch"
combined_105 <- Reduce(function(x, y) merge(x, y, by = common_columns, all =TRUE), split_combined_105)
gc()

###### FOR THE 095 DATA ####

#split all -5% sensitivity runs into 6 separate dataframes (one for each of the parameters changed)
split_combined_095 <- split(combined_data_095, f = combined_data_095$VAR_CHANGED)
rm(combined_data_095) #remove previous dataframe for space release

#create a function to add the original column name + the name of the dataframe (which corresponds to the paramters changed by x%) + "_095" to each column name
add_suffix_095 <- function(df, name) {
  col_indices <- 4:8
  if(max(col_indices) <= ncol(df)){
    colnames(df)[col_indices] <- paste0(colnames(df)[col_indices], "_", name, "_095")
  } else {
    warning(paste("Data frame", name, "does not have enough collumns"))
  }
  return(df)
}

#apply the function to each dataframe inside the large list
split_combined_095 <- lapply(names(split_combined_095), function(name){
  add_suffix_095(split_combined_095[[name]], name)
})

#restore names of each dataframe and add "_105"
names_095 <- c("bevmort_095", "carry_095", "growrate_095", "mass_095", "max_dispersal_dis_095", "mean_dispersal_dis_095")
names(split_combined_095) <- names_095

#remove VAR_CHANGED and SR variable
split_combined_095 <- purrr::map(split_combined_095, dplyr::select, -"VAR_CHANGED") 
split_combined_095 <- purrr::map(split_combined_095, dplyr::select, -"SR") 
#common columns between dataframes
common_columns <- c("t", "x", "y", "species")

#combine all dataframes with correct column names that state the variable_"ChangedParameter"_"ByHowMuch"
combined_095 <- Reduce(function(x, y) merge(x, y, by = common_columns, all =TRUE), split_combined_095)
gc() #remove unecessary memory

#combine both the control, sensitivity run 105 and sensitivity run 095 unto a giant dataframe
sens_run_control <- combined_data_control %>% 
  left_join(combined_105, by = c("t", "x", "y", "species")) %>% 
  left_join(combined_095, by = c("t", "x", "y", "species"))

#convert integer64 columns into numeric
sens_run_control[] <- lapply(sens_run_control, function(col) {
  if (bit64::is.integer64(col)) {
    as.numeric(col)
  } else {
    col
  }
}) 

#calculate the proportion between each sensitivity run and the control runs for each variable for each parameter change option
sens_run_control <- sens_run_control %>% 
  mutate(#abundance
    abund_propgrowrate105 = abundance_growrate_105/abundance,
    abund_propgrowrate105 = abundance_growrate_095/abundance, 
    abund_propbevmort105 = abundance_bevmort_105/abundance,
    abund_propbevmort095 = abundance_bevmort_095/abundance,
    abund_propmax105 = abundance_max_105/abundance, 
    abund_propmax095 = abundance_max_095/abundance, 
    abund_propmass105 = abundance_mass_105/abundance,
    abund_propmass095 = abundance_mass_095/abundance,
    abund_propcarry105 = abundance_carry_105/abundance, 
    abund_propcarry095 = abundance_carry_095/abundance,
    abund_propmean105 = abundance_mean_105/abundance,
    abund_propmean095 = abundance_mean_095/abundance, 
    #reproduction
    reprod_propgrowrate105 = reproduction_growrate_105/reproduction,
    reprod_propgrowrate105 = reproduction_growrate_095/reproduction, 
    reprod_propbevmort105 = reproduction_bevmort_105/reproduction,
    reprod_propbevmort095 = reproduction_bevmort_095/reproduction,
    reprod_propmax105 = reproduction_max_105/reproduction, 
    reprod_propmax095 = reproduction_max_095/reproduction, 
    reprod_propmass105 = reproduction_mass_105/reproduction,
    reprod_propmass095 = reproduction_mass_095/reproduction,
    reprod_propcarry105 = reproduction_carry_105/reproduction, 
    reprod_propcarry095 = reproduction_carry_095/reproduction,
    reprod_propmean105 = reproduction_mean_105/reproduction,
    reprod_propmean095 = reproduction_mean_095/reproduction,
    #habitat
    hab_propgrowrate105 = habitat_growrate_105/habitat,
    hab_propgrowrate105 = habitat_growrate_095/habitat, 
    hab_propbevmort105 = habitat_bevmort_105/habitat,
    hab_propbevmort095 = habitat_bevmort_095/habitat,
    hab_propmax105 = habitat_max_105/habitat, 
    hab_propmax095 = habitat_max_095/habitat, 
    hab_propmass105 = habitat_mass_105/habitat,
    hab_propmass095 = habitat_mass_095/habitat,
    hab_propcarry105 = habitat_carry_105/habitat, 
    hab_propcarry095 = habitat_carry_095/habitat,
    hab_propmean105 = habitat_mean_105/habitat,
    hab_propmean095 = habitat_mean_095/habitat,
    #carrying capacity
    carry_propgrowrate105 = carry_growrate_105/carry,
    carry_propgrowrate105 = carry_growrate_095/carry, 
    carry_propbevmort105 = carry_bevmort_105/carry,
    carry_propbevmort095 = carry_bevmort_095/carry,
    carry_propmax105 = carry_max_105/carry, 
    carry_propmax095 = carry_max_095/carry, 
    carry_propmass105 = carry_mass_105/carry,
    carry_propmass095 = carry_mass_095/carry,
    carry_propcarry105 = carry_carry_105/carry, 
    carry_propcarry095 = carry_carry_095/carry,
    carry_propmean105 = carry_mean_105/carry,
    carry_propmean095 = carry_mean_095/carry,
    #bevmort
    bevmort_propgrowrate105 = bevmort_growrate_105/bevmort,
    bevmort_propgrowrate105 = bevmort_growrate_095/bevmort, 
    bevmort_propbevmort105 = bevmort_bevmort_105/bevmort,
    bevmort_propbevmort095 = bevmort_bevmort_095/bevmort,
    bevmort_propmax105 = bevmort_max_105/bevmort, 
    bevmort_propmax095 = bevmort_max_095/bevmort, 
    bevmort_propmass105 = bevmort_mass_105/bevmort,
    bevmort_propmass095 = bevmort_mass_095/bevmort,
    bevmort_propcarry105 = bevmort_carry_105/bevmort, 
    bevmort_propcarry095 = bevmort_carry_095/bevmort,
    bevmort_propmean105 = bevmort_mean_105/bevmort,
    bevmort_propmean095 = bevmort_mean_095/bevmort,
  )
#write a tsv file with all the data
#write_tsv(sens_run_control, "~/Desktop/sensitivity_runs_control_scenario.tsv")
#AFTER WRITTING THS TSV EITHER CLEAN THE GLOBAL ENVIRONMENT OR SHUT DOWN R ENTIERLY AND IMPORT THE WRITTEN TSV WITH FREAD, THE PREVIOUS ANALYSIS USE TOO MUCH MEMORY AND FROM HERE ON IT CAN NOT DO ANYTHING WITHOUT CRASHING! (FATAL FLAW OR ERROR MESSAGE SHOWS)

#import complete dataframe for the sensitivity analysis in the control scenario
sens_run_control<- fread("~/Desktop/sensitivity_runs_control_scenario.tsv", integer64 = "numeric")
gc()  #remove unecessary memory

#subset the necessary columns (the timestep, x, y, species and proportion for each variable)
sens_run_control_subset <- sens_run_control[, c(1:3, 9, 70:124)]
#transform all necessary column into rows (pivot longer operation)
simulations_long <- data.table::melt(sens_run_control_subset, id.vars = c(1:4), variable.name = "Var_sim", value.name = "Value")
gc()

#split the Var_sim column into two to have the Simulation and Variable names in the correct format for the boxplots
simulations_long[, c("Variable", "Simulation") := data.table::tstrsplit(Var_sim, "_")]



variables.labs <- c("abund" = "Abundance",
                    "bevmort" = "Mortality",
                    "carry" = "Carrying capacity",
                    "hab" = "Habitat suitability",
                    "reprod" = "Reproduction")


#plot the results into a boxplot 
#this running and saving this plot takes 40 min 
sens_analysis <- ggplot(simulations_long, aes(x = as.factor(Simulation), y = Value)) +
  geom_boxplot(outlier.shape = NA) +
  ylim(0,1.6) +
  facet_wrap(.~Variable, labeller = as_labeller(variables.labs), as.table = F, ncol = 2) +
  stat_summary(fun = mean, geom = "point", aes(group = interaction(species, Variable, Simulation), color = species), shape = 16, size = 1.5, position = position_jitter(width = 0.5, height = 0)) +
  scale_color_viridis_d(option = "D") +
  labs(x = "Simulation", y = "value", color = "species") +
  geom_hline(yintercept = 1.2, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1.0, hjust = 1.0),
        panel.background = element_blank (), 
        panel.grid.major = element_line(color = "gray90", size = 0.25),
        panel.grid.minor = element_line(color ="gray90", size = 0.25),
        strip.text = element_text(face = "bold", size = rel(0.8)),
        legend.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.key = element_rect(fill = "white", colour = NA))
ggsave(plot = sens_analysis, file = "~/Desktop/sens_analysis2.tiff", bg = "white", width = 170, height = 200, units = "mm", dpi = 600, compression ="lzw")
gc()

end.time <- Sys.time() #end the clock
time.taken <- round(end.time - start.time) #calculate time taken to run the complete script
time.taken


