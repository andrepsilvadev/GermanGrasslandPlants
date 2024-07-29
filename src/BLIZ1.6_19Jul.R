# BLIZ ##
## Task 1.6 ##

# This script uses data for "normal" analysis from the 12th July 2024
# The control run was only added on the 17th July 2024
# It also has the code for sensitivity analysis using data from the 24th July 2024
# The sensitivity analysis ran only on the wuerzburg server for ONLY ONE SCENARIO

# Packages
library(readr)
library(dplyr)
library(collapse)
library(fs)
library(tidyverse) 
library(readxl)
library(writexl)
library(ggplot2)
library(raster)
library(gridExtra)
library(DT) # functions to produce nicely formatted tables
library(stringr)
library(data.table)
library(gridExtra) #for spatial maps to have different fill scales
library(bit64) #for the integer64 columns

##############################################
# STEP 1 - IMPORT AND MERGE ALL OUTPUT FILES #
##############################################

setwd("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/BLIZ_1.6_ANDRE/19Jul/output_19Jul")

#all_data <- list.files(recursive = T) %>% #get a list of the paths to datasets files
 # map_dfr(function(path) {
  #retrieve any characters from the beginning of the path until /
  #foldername <- str_extract(path, "^.+(?=/)")
  #fread(path) %>% #import the files  
  #mutate(folder = foldername) # add a new collumn with the folder name
#})
#gc()

#all_data <- all_data %>%
 # separate(folder, c("date", "time", "species", "scenario"), sep = "_", remove = FALSE) %>% #takes a VERY long time ^(more than 40 minutes)
  #dplyr::select(-c("folder", "date", "time"))
#gc()

#check for negative values in abundance
#summary(all_data)
#rows with negative abundances
#bb <- as.data.frame(which(all_data < 0 , arr.ind=TRUE))
#bb <- bb[bb$col == 4,]

#values <- as.vector(bb$row)
#aa <- all_data[values,]



#write tsv file with all species in all scenarios
#write_tsv(all_data, "full_dataset.tsv")

#Import the complete dataset 
all_data <- fread("./full_dataset.tsv", integer64 = "numeric")
gc()

#################################
# STEP 2 - CREATE NEW VARIABLES #
#################################

#occupancy
all_data <- all_data %>%
  mutate(occupancy = ifelse(all_data$abundance > 0, 1, 0)) 
gc()

#abundance change
all_data <- all_data%>%
  group_by(species, scenario) %>% 
  mutate(abund_change <- abundance - abundance[t == 25]) %>%
  ungroup()
gc()

all_data <- all_data %>%
  mutate(occupancy = ifelse(all_data$abundance > 0, 1, 0)) 
gc()

#abundance change
all_data <- all_data %>%
  group_by(species, scenario) %>% 
  mutate(abund_change <- abundance - abundance[t == 25]) %>%
  ungroup()
gc()

#occupancy change
all_data <- all_data %>%
  group_by(species, scenario) %>% 
  mutate(occup_change <- occupancy - occupancy[t == 25]) %>%
  ungroup()
gc()

#abundance mismatch
all_data <- all_data %>%
  group_by(species, scenario) %>% 
  mutate(abund_mismatch <- carry - abundance) %>%
  ungroup()
gc()

#habitat change
all_data <- all_data %>%
  group_by(species, scenario) %>% 
  mutate(habitat_change <- habitat - habitat[t == 25]) %>%
  ungroup()

#remove unused memory
gc()

#change column names
colnames(all_data) <- c("t", "x", "y",  "abundance", "reproduction", "habitat", "carry",  "bevmort", "species", "scenario", "occupancy", "abund_change", "occup_change", "abund_mismatch", "habitat_change" )

##########################################
# STEP 3 - CREATE POPULATION LEVEL PLOTS #
##########################################

#calculate mean values
data_all_mean_stdev<-all_data[, c(1,4:10)]

#Mean and standard deviation for ABUNDANCE, REPRODUCTION, CARRYING CAPACITY, HABITAT SUITABILITY and MORTALITY variables
mean_stDEV <- data_all_mean_stdev %>%
  dplyr::group_by(species, scenario, t) %>%
  dplyr::summarise(across(c(abundance, reproduction, carry, habitat, bevmort), .fns = list(mean = mean, sd = sd), na.rm = TRUE), .groups = 'drop') %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) 
write.csv(mean_stDEV, "mean_values_17Jul.csv", row.names=FALSE)
gc()

summary(mean_stDEV)
#DT TABLE CUSTOM CONTAINERS
# design custom table containers for the DT table
sketch2 = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Time step'), # the species, scenario and Time step headers span 2 rows
      th(rowspan = 2, 'Species'),
      th(colspan = 2, 'Abundance'), # the headers Abundance, Reproduction, Habitat suitability, Carrying capacity and Mortality span 2 columns each (1 for the mean value and other for the standard deviation)
      th(colspan = 2, 'Reproduction'),
      th(colspan = 2, 'Habitat suitability'),
      th(colspan = 2, 'Carrying capacity'),
      th(colspan = 2, 'Mortality')
    ),
    tr(
      lapply(rep(c('Mean', 'St. Dev.'), 5), th) # repeat "Mean" and "St. Dev." 5 times (5 headers)
    ))))


#population plots' theme
line_plots_theme <- theme_minimal() +
  theme(# Customise the background grids
    panel.grid.major =  element_line(color = "gray90", size = 0.25),
    panel.grid.minor = element_blank(),
    # Add Bold legend titles
    legend.title = element_text(face = "bold"),
    # Italic, slightly larger facet titles 
    strip.text = element_text(face = "italic", size = rel(0.8)),
    # Bold axis titles
    axis.title = element_text(face = "bold"),
    # Set tick label color, margin, and size
    axis.text.y=element_text(colour="black" ,margin=margin(t=0,r=1,b=0,l=0), size = 9),
    axis.text.x=element_text(colour="black", margin=margin(t=1,r=0,b=0,l=0), size = 9))

#set the working directory again to save plots in tha correct folder
setwd("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/BLIZ_1.6_ANDRE/19Jul")

## ABUNDANCE THROUGH TIME

abund_plot <- ggplot(mean_stDEV, aes(x = t, y = abundance_mean, group = scenario, color = scenario)) + 
  geom_line() + # line plot
  #geom_ribbon(aes(x = t, ymin = abundance_mean - abundance_sd, ymax = abundance_mean + abundance_sd, fill = scenario), alpha = 0.3, colour = NA) + # add ribbon with standard deviation values
  facet_wrap(. ~ species, ncol =3, scales = "free_y") +  # one vertical panel for each species
  ggh4x::facetted_pos_scales(
    y = list(scale_y_continuous(limits = c(0, 1.2e+08)),
             scale_y_continuous(limits = c(0, 1.2e+07)), 
             scale_y_continuous(limits = c(0, 4.5e+6)), 
             scale_y_continuous(limits = c(0, 7.5e+08)),
             scale_y_continuous(limits = c(0, 1.0e+08)),
             scale_y_continuous(limits = c(0, 7.5e+07)),
             scale_y_continuous(limits = c(0, 1.0e+08)),
             scale_y_continuous(limits = c(0, 1.0e+08)),
             scale_y_continuous(limits = c(0, 5.5e+07)),
             scale_y_continuous(limits = c(0, 4.5e+07)),
             scale_y_continuous(limits = c(0, 9e+07)))) +  
  scale_fill_viridis_d(option = "D") + # colour palette
  scale_color_viridis_d(option = "D") + 
  labs(x ="Time" , y = "Abundance", color = "Scenarios", fill = "Scenarios") +  
  line_plots_theme + 
  theme(legend.position="bottom")
#abund_plot
#save ABUNDANCE THROUGH TIME plot
ggsave(plot = abund_plot, file = "./plots_22Jul/1.Abundance_through_time19Jul.tiff", bg = 'white', width = 200, height = 180, units = "mm", dpi = 1200, compression = "lzw")


### OPTION WITH SAME SCALE IN ALL AXIS ###
abund_plot2 <- ggplot(mean_stDEV, aes(x = t, y = abundance_mean, group = scenario, color = scenario)) + 
  geom_line() + # line plot
  #geom_ribbon(aes(x = t, ymin = abundance_mean - abundance_sd, ymax = abundance_mean + abundance_sd, fill = scenario), alpha = 0.3, colour = NA) + # add ribbon with standard deviation values
  facet_wrap(. ~ species, ncol =3) +  # one vertical panel for each species
  scale_fill_viridis_d(option = "D") + # colour palette
  scale_color_viridis_d(option = "D") + 
  labs(x ="Time" , y = "Abundance", color = "Scenarios", fill = "Scenarios") +  
  line_plots_theme + 
  theme(legend.position="bottom")
abund_plot2
#save ABUNDANCE THROUGH TIME plot
ggsave(plot = abund_plot2, file = "./plots_22Jul/1.1.Abundance_through19Jul.tiff", bg = 'white', width = 200, height = 180, units = "mm", dpi = 1200, compression = "lzw")

###############################
## REPRODUCTION THROUGH TIME ##
###############################

reprod_plot <- ggplot(mean_stDEV, aes(x = t, y = reproduction_mean, group = scenario, color = scenario)) +
  geom_line() + # line plot
  #geom_ribbon(aes(x = t, ymin = reproduction_mean - reproduction_sd, ymax = reproduction_mean + reproduction_sd, fill = scenario), alpha = 0.3, color =NA) + # add ribbon with standard deviation values
  facet_wrap(. ~ species,  scales = "free_y") + # one vertical panel for each species
  ggh4x::facetted_pos_scales(
    y = list(scale_y_continuous(limits = c(100, 220)),
             scale_y_continuous(limits = c(1.0, 2.5)), 
             scale_y_continuous(limits = c(0.5, 2.0)), 
             scale_y_continuous(limits = c(0.5, 2.0)),
             scale_y_continuous(limits = c(0.4, 1.5)),
             scale_y_continuous(limits = c(0.5, 1.7)),
             scale_y_continuous(limits = c(0.5, 2.0)),
             scale_y_continuous(limits = c(0.5, 2.0)),
             scale_y_continuous(limits = c(0.5, 2.0)),
             scale_y_continuous(limits = c(0.5, 2.0)),
             scale_y_continuous(limits = c(0.5, 2.0)))) +  
  scale_fill_viridis_d(option = "D") + # colour palette
  scale_color_viridis_d(option = "D") + 
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +  # set xx axis to start at zero and more space at the top of the yy axis
  labs(x ="Time", y = "Reproduction") + # axis labels (note: fill and color must have the same name to have only one legend)
  line_plots_theme +
  theme(legend.position = "bottom")
#reprod_plot

#Save reproduction through time plot
ggsave(plot = reprod_plot, file = "./plots_22Jul/2.Reproduction_through_time19Jul.tiff",  bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200, compression = "lzw")

### OPTION WITH SAME SCALE IN ALL AXIS ###
reprod_plot2 <- ggplot(mean_stDEV, aes(x = t, y = reproduction_mean, group = scenario, color = scenario)) +
  geom_line() + # line plot
  #geom_ribbon(aes(x = t, ymin = reproduction_mean - reproduction_sd, ymax = reproduction_mean + reproduction_sd, fill = scenario), alpha = 0.3, color =NA) + # add ribbon with standard deviation values
  facet_wrap(. ~ species, ncol = 3) +
  scale_fill_viridis_d(option = "D") + # colour palette
  scale_color_viridis_d(option = "D") + 
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +  # set xx axis to start at zero and more space at the top of the yy axis
  labs(x ="Time", y = "Reproduction") + # axis labels (note: fill and color must have the same name to have only one legend)
  line_plots_theme +
  theme(legend.position = "bottom")
#reprod_plot

#Save reproduction through time plot
ggsave(plot = reprod_plot2, file = "./plots_22Jul/2.1.Reproduction_through_time19Jul.tiff",  bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200, compression = "lzw")

######################################
## HABITAT SUITABILITY THROUGH TIME ##
######################################

habitat_plot <- ggplot(mean_stDEV, aes(x = t, y = habitat_mean, group = scenario, color = scenario)) +
  geom_line() + # line plot
  #geom_ribbon(aes(x = t, ymin = habitat_mean - habitat_sd, ymax = habitat_mean + habitat_sd, fill = scenario), alpha = 0.3, color = NA) + # add ribbon with standard deviation values
  facet_wrap(. ~ species, ncol = 3, scales = "free_y") + # one vertical panel for each species
  ggh4x::facetted_pos_scales(
    y = list(scale_y_continuous(limits = c(0.2,0.7)),
             scale_y_continuous(limits = c(0, 0.3)), 
             scale_y_continuous(limits = c(0.2,0.8)), 
             scale_y_continuous(limits = c(0.2,0.7)),
             scale_y_continuous(limits = c(0.2,0.7)),
             scale_y_continuous(limits = c(0.2,0.7)),
             scale_y_continuous(limits = c(0.2,0.7)),
             scale_y_continuous(limits = c(0.2,0.7)),
             scale_y_continuous(limits = c(0.2,0.7)),
             scale_y_continuous(limits = c(0.2,0.7)),
             scale_y_continuous(limits = c(0.2,0.7)))) + 
  scale_fill_viridis_d(option = "D") + # colour palette
  scale_color_viridis_d(option = "D") + 
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.1)), breaks = seq(0, 1, 0.2)) +  # set xx axis to start at zero and more space at the top of the yy axis
  labs( x ="Time" , y = "Habitat suitability", color = "Scenarios", fill = "Scenarios") + # axis labels (note: fill and color must have the same name to have only one legend)
  line_plots_theme + 
  theme(legend.position = "bottom")
#habitat_plot0.2

#Save habitat suitability through time plot
ggsave(plot = habitat_plot, file = "./plots_22Jul/3.Habitat_suitability_through_time19Jul.tiff",  bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200, compression = "lzw")


### OPTION WITH SAME SCALE IN ALL AXIS ###
habitat_plot2 <- ggplot(mean_stDEV, aes(x = t, y = habitat_mean, group = scenario, color = scenario)) +
  geom_line() + # line plot
  #geom_ribbon(aes(x = t, ymin = habitat_mean - habitat_sd, ymax = habitat_mean + habitat_sd, fill = scenario), alpha = 0.3, color = NA) + # add ribbon with standard deviation values
  facet_wrap(. ~ species, ncol = 3) + # one vertical panel for each species
  scale_fill_viridis_d(option = "D") + # colour palette
  scale_color_viridis_d(option = "D") + 
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.1)), breaks = seq(0, 1, 0.2)) +  # set xx axis to start at zero and more space at the top of the yy axis
  labs( x ="Time" , y = "Habitat suitability", color = "Scenarios", fill = "Scenarios") + # axis labels (note: fill and color must have the same name to have only one legend)
  line_plots_theme + 
  theme(legend.position = "bottom")
#habitat_plot0.2

#Save habitat suitability through time plot
ggsave(plot = habitat_plot2, file = "./plots_22Jul/3.1.Habitat_suitability_through_time19Jul.tiff",  bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200, compression = "lzw")

####################################
## CARRYING CAPACITY THROUGH TIME ##
####################################

carry_cap_plot <- ggplot(mean_stDEV, aes(x = t, y = carry_mean, group = scenario, color = scenario)) +
  geom_line() + # line plot
  #geom_ribbon(aes(x = t, ymin = carry_mean - carry_sd, ymax = carry_mean + carry_sd, fill = scenario), alpha = 0.3, color = NA) +  # add ribbon with standard deviation values
  facet_wrap(. ~ species, ncol = 3, scales = "free_y") + # one vertical panel for each species
  ggh4x::facetted_pos_scales(
    y = list(scale_y_continuous(limits = c(4.0e+07, 1.0e+08)),
             scale_y_continuous(limits = c(0, 1.5e+07)), 
             scale_y_continuous(limits = c(2.0e+06, 4.5e+6)), 
             scale_y_continuous(limits = c(2.0e+07, 7e+07)),
             scale_y_continuous(limits = c(2.5e+07, 1.0e+08)),
             scale_y_continuous(limits = c(2.0e+07, 8e+07)),
             scale_y_continuous(limits = c(4.0e+07, 1.5e+08)),
             scale_y_continuous(limits = c(4.0e+07, 1.2e+08)),
             scale_y_continuous(limits = c(2.0e+7, 5e+07)),
             scale_y_continuous(limits = c(2.0e+07, 5e+07)),
             scale_y_continuous(limits = c(4.0e+07, 1.0e+08)))) +
  scale_fill_viridis_d(option = "D") + # colour palette
  scale_color_viridis_d(option = "D") + 
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +  # set xx axis to start at zero and more space at the top of the yy axis
  labs( x ="Time" , y = "Carrying capacity", color = "Scenarios", fill = "Scenarios") + # axis labels (note: fill and color must have the same name to have only one legend)
  line_plots_theme + 
  theme(legend.position = "bottom")
#carry_cap_plot

#Save carrying capacity through time plot
ggsave(plot = carry_cap_plot, file = "./plots_22Jul/4.Carrying_capacity_through_time19Jul.tiff",  bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200, compression = "lzw")

### OPTION WITH SAME SCALE IN ALL AXIS ###
carry_cap_plot2 <- ggplot(mean_stDEV, aes(x = t, y = carry_mean, group = scenario, color = scenario)) +
  geom_line() + # line plot
  #geom_ribbon(aes(x = t, ymin = carry_mean - carry_sd, ymax = carry_mean + carry_sd, fill = scenario), alpha = 0.3, color = NA) +  # add ribbon with standard deviation values
  facet_wrap(. ~ species, ncol = 3) + # one vertical panel for each species
  scale_fill_viridis_d(option = "D") + # colour palette
  scale_color_viridis_d(option = "D") + 
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +  # set xx axis to start at zero and more space at the top of the yy axis
  labs( x ="Time" , y = "Carrying capacity", color = "Scenarios", fill = "Scenarios") + # axis labels (note: fill and color must have the same name to have only one legend)
  line_plots_theme + 
  theme(legend.position = "bottom")
#carry_cap_plot

#Save carrying capacity through time plot
ggsave(plot = carry_cap_plot2, file = "./plots_22Jul/4.1.Carrying_capacity_through_time19Jul.tiff",  bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200, compression = "lzw")

############################
## MORTALITY THROUGH TIME ##
############################

mort_plot <- ggplot(mean_stDEV, aes(x = t, y = bevmort_mean, group = scenario, color = scenario)) +
  geom_line() + # line plot
  #geom_ribbon(aes(x = t, ymin = bevmort_mean - bevmort_sd, ymax = bevmort_mean + bevmort_sd, fill = scenario), alpha = 0.3, color = NA) + # add ribbon with standard deviation values
  facet_wrap(. ~ species, ncol = 3, scales = "free_y") + # vertical panels for each species distributed in e columns
  ggh4x::facetted_pos_scales(
    y = list(scale_y_continuous(limits = c(0.5, 1.1)),
             scale_y_continuous(limits = c(0.3, 0.7)), 
             scale_y_continuous(limits = c(0.2, 0.75)), 
             scale_y_continuous(limits = c(0.6, 1.1)),
             scale_y_continuous(limits = c(0.7, 1.1)),
             scale_y_continuous(limits = c(0.7, 1.1)),
             scale_y_continuous(limits = c(0.6, 1.1)),
             scale_y_continuous(limits = c(0.6, 1.1)),
             scale_y_continuous(limits = c(0.6, 1.1)),
             scale_y_continuous(limits = c(0.6, 1.1)),
             scale_y_continuous(limits = c(0.4, 0.8)))) + 
  scale_fill_viridis_d(option = "D") + # colour palette
  scale_color_viridis_d(option = "D") + 
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +  # set xx axis to start at zero and more space at the top of the yy axis
  labs( x ="Time" , y = "Mortality", color = "Scenarios", fill = "Scenarios") + # axis labels (note: fill and color must have the same name to have only one legend)
  line_plots_theme + 
  theme(legend.position = "bottom")
#mort_plot

#Save mortality through time plot
ggsave(plot = mort_plot, file = "./plots_22Jul/5.Mortality_through_time_19Jul.tiff",  bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200, compression = "lzw")

### OPTION WITH SAME SCALE IN ALL AXIS ###
mort_plot2 <- ggplot(mean_stDEV, aes(x = t, y = bevmort_mean, group = scenario, color = scenario)) +
  geom_line() + # line plot
  #geom_ribbon(aes(x = t, ymin = bevmort_mean - bevmort_sd, ymax = bevmort_mean + bevmort_sd, fill = scenario), alpha = 0.3, color = NA) + # add ribbon with standard deviation values
  facet_wrap(. ~ species, ncol = 3) + # vertical panels for each species distributed in e columns
  scale_fill_viridis_d(option = "D") + # colour palette
  scale_color_viridis_d(option = "D") + 
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +  # set xx axis to start at zero and more space at the top of the yy axis
  labs( x ="Time" , y = "Mortality", color = "Scenarios", fill = "Scenarios") + # axis labels (note: fill and color must have the same name to have only one legend)
  line_plots_theme + 
  theme(legend.position = "bottom")
#mort_plot

#Save mortality through time plot
ggsave(plot = mort_plot2, file = "./plots_22Jul/5.1Mortality_through_time_19Jul.tiff",  bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200, compression = "lzw")


#################################
## ABUNDANCE MISMATCH BOXPLOTS ##
#################################

abund_mismatch_plot <- ggplot(all_data, aes(x=as.factor(scenario), y = abund_mismatch, fill = scenario)) +
  geom_boxplot() + 
  labs(x = "SSP scenarios", y = "Abundance Mismatch", fill ="Scenarios") + #axis labels
  facet_wrap(. ~ species, ncol = 3, scales = "free_y") + #one vertical panel per species "SO UM EIXO FREE"
  scale_fill_viridis_d(option = "D") +
  line_plots_theme + 
  theme(panel.background =  element_blank(),
        panel.grid.major =  element_line(color = "gray90", size = 0.25),
        panel.grid.minor = element_line(color = "gray90", size = 0.25),
        axis.text.x = element_text(angle = 60, vjust = 1.0, hjust = 1.0),
        legend.position = "bottom")
#abund_mismatch_plot

#Save abundance mismatch per scenario plot
ggsave(plot = abund_mismatch_plot, file = "./plots_22Jul/6.Abundance_mismatch19Jul.tiff",  bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200, compression = "lzw")
#this plot takes a lot more time than the others including saving it to an image

## Option with bigger figure size ##
ggsave(plot = abund_mismatch_plot, file = "./plots_22Jul/6.2Abundance_mismatch22Jul_expanded.tiff",  bg = 'white', width = 250, height = 280, units = "mm", dpi = 1200, compression = "lzw")
#this plot takes a lot more time than the others including saving it to an image

## OPTION WITHOUT OUTLIERS ##
abund_mismatch_plot2 <- ggplot(all_data, aes(x=as.factor(scenario), y = abund_mismatch, fill = scenario)) +
  geom_boxplot(outliers = FALSE) + 
  labs(x = "SSP scenarios", y = "Abundance Mismatch", fill ="Scenarios") + #axis labels
  facet_wrap(. ~ species, ncol = 3, scales = "free_y") + #one vertical panel per species "SO UM EIXO FREE"
  stat_summary(geom = "point", fun = \(x) quantile(x, 0.1, na.rm = T), shape = 3, size = 2, color = "red")+
  stat_summary(geom = "point", fun = \(x) quantile(x, 0.9, na.rm = T), shape = 3, size = 2, color = "blue") +
  scale_fill_viridis_d(option = "D") +
  line_plots_theme + 
  theme(panel.background =  element_blank(),
        panel.grid.major =  element_line(color = "gray90", size = 0.25),
        panel.grid.minor = element_line(color = "gray90", size = 0.25),
        axis.text.x = element_text(angle = 60, vjust = 1.0, hjust = 1.0),
        legend.position = "bottom")
ggsave(plot = abund_mismatch_plot2, file = "./plots_22Jul/6.3Abundance_mismatch22Jul_WITHOUT_OUTLIERS.tiff",  bg = 'white', width = 250, height = 280, units = "mm", dpi = 1200, compression = "lzw")
#this plot takes a lot more time than the others including saving it to an image

gc()

## OPTION USING VIOLOIN PLOTS ##
## ABUNDANCE MISMATCH WITH VIOLOIN PLOTS
abund_mismatch_violin <- ggplot(all_data, aes(x = as.factor(scenario), y = abund_mismatch, fill = scenario)) +
  geom_violin() +
  facet_wrap(.~species, ncol = 3, scales = "free_y") + #one vertical panel per species "SO UM EIXO FREE"
  scale_fill_viridis_d(option = "D") +
  line_plots_theme + 
  theme(panel.background =  element_blank(),
        panel.grid.major =  element_line(color = "gray90", size = 0.25),
        panel.grid.minor = element_line(color = "gray90", size = 0.25),
        axis.text.x = element_text(angle = 60, vjust = 1.0, hjust = 1.0),
        legend.position = "bottom")

ggsave(plot = abund_mismatch_violin, file = "Abundance_mismatch_violin19Jul.tiff",  bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200, compression = "lzw")

##################################################
# STEP 4 - CREATE CELL LEVEL PLOTS (per species) #
##################################################

# subset data for the last time step (t = 90)
abund_t90<-all_data[all_data$t == 90,]

#new way of having the spatial maps to have a different fill scale for each species
abundance_change_map <- abund_t90 %>%
  group_by(species) %>% 
  do(gg = {ggplot(., aes(x, -y, fill = abund_change)) + 
      geom_raster() +
      facet_wrap(.~species) + 
      #guides(fill = guide_colourbar(title.position = "bottom")) +
      scale_fill_distiller(palette = "RdBu", na.value = "transparent", direction = -1) + 
      #scale_fill_viridis_c("Abundance \nchange", option = "H", na.value = "transparent") +
      labs(x = "Cell longitude ", y ="Cell latitude ") +
        theme(legend.position = "bottom", panel.background =  element_blank(), # no background grids
            # facets identification customization
            strip.background =  element_rect(fill = "grey94", color = "black"), 
            strip.text = element_text(face = "italic", size = rel(0.8)), 
            panel.border =element_rect(color = "black", fill = NA),
            # axis and legend font type
            legend.title = element_text(face = "bold"),
            legend.text=element_text(size = 6),
            axis.title = element_text(face = "bold"))}) %>% 
  .$gg %>% arrangeGrob(grobs = ., nrow = 4) %>%
  grid.arrange()

#Save abundance mismatch per scenario plot
ggsave(plot = abundance_change_map, file = "./1.plots_22Jul/7.abundance_change_map.tiff",  bg = 'white', width = 230, height = 280, units = "mm", dpi = 1200, compression = "lzw")


## HABITAT SUITABILITY CHANGE SPATIAL MAP ##
habitat_change_map <- abund_t90 %>%
  group_by(species) %>% 
  do(gg = {ggplot(., aes(x, -y, fill = habitat_change)) + 
      geom_raster() +
      facet_wrap(.~species) + 
      #guides(fill = guide_colourbar(title.position = "bottom")) +
      scale_fill_distiller(palette = "RdBu", na.value = "transparent", name = "Habitat suitability \nchange", direction = -1) + 
      labs(x = "Cell longitude ", y ="Cell latitude ") +
      theme(legend.position = "bottom", panel.background =  element_blank(), # no background grids
            # facets identification customization
            strip.background =  element_rect(fill = "grey94", color = "black"), 
            strip.text = element_text(face = "italic", size = rel(0.8)), 
            panel.border =element_rect(color = "black", fill = NA),
            # axis and legend font type
            legend.title = element_text(face = "bold"), 
            axis.title = element_text(face = "bold"))}) %>% 
  .$gg %>% arrangeGrob(grobs = ., nrow = 4) %>%
  grid.arrange()

#Save habitat suitability per scenario plot
ggsave(plot = habitat_change_map, file = "./1.plots_22Jul/8.habitat_change_map.tiff",  bg = 'white', width = 230, height = 280, units = "mm", dpi = 1200, compression = "lzw")

########## HABITAT SUITABILITY MAP WITH DIFFERENT COLOUR PALLETE #############

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_fill_gradientn(colours = myPalette(25), limits=c(-1, 1))


habitat_change_map2 <- abund_t90 %>%
  group_by(species) %>% 
  do(gg = {ggplot(., aes(x, -y, fill = habitat_change)) + 
      geom_raster() +
      facet_wrap(.~species) + 
      #guides(fill = guide_colourbar(title.position = "bottom")) +
      sc + 
      labs(x = "Cell longitude ", y ="Cell latitude ") +
      theme(legend.position = "bottom", panel.background =  element_blank(), # no background grids
            # facets identification customization
            strip.background =  element_rect(fill = "grey94", color = "black"), 
            strip.text = element_text(face = "italic", size = rel(0.8)), 
            panel.border =element_rect(color = "black", fill = NA),
            # axis and legend font type
            legend.title = element_text(face = "bold"), 
            axis.title = element_text(face = "bold"))}) %>% 
  .$gg %>% arrangeGrob(grobs = ., nrow = 4) %>%
  grid.arrange()

#Save habitat suitability per scenario plot
ggsave(plot = habitat_change_map2, file = "habitat_change_map2_19Jul.tiff",  bg = 'white', width = 230, height = 280, units = "mm", dpi = 1200, compression = "lzw")
###################################################

corr_data_two <- all_data[all_data$scenario == "SSP1-climate", c(9,12,15)]
corr_data_two$abund_change <- as.numeric(corr_data_two$abund_change) #the lm and the labels functions do not work with the integer64 format thta this column had previously

# function to write the model equation and the r2 
lm_labels <- function(df) {
  mod <- lm(abund_change ~ habitat_change, data = df, na.action = "na.exclude")
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1], 2), round(coef(mod)[2], 2)) # write the equation formula
  r2 <- sprintf("italic(R^2) == %.2f", summary(mod)$r.squared) # write th r2 for each group
  data.frame(formula = formula, r2 = r2, stringsAsFactors = FALSE)
}
# SPRINTF function returns a character vector containing a formatted combination of text and variable values

# create dataframe with the equations and r2
labels <- corr_data_two %>%
  group_by(species) %>% # group data by species
  do(lm_labels(.)) %>%  
  ungroup() %>% # use the function above to create the dataframe with equation and r2 for each species
  mutate(xlabel = rep(-0.05, 11),
         ylabel = c(1.75e+08, -4.75e+07, 8.75e+05, 4e+08, 1.75e+08, 6.75e+07, -8e+07, -1.25e+08, -1.25e+08, 2.75e+07, -1.75e+08),
         xr2 = rep(-0.05, 11),
         yr2 = c(1.5e+08, -6e+07, 0.5e+06, 3.5e+08, 1.5e+08, 5e+07, -1.0e+08, -1.5e+08, -1.5e+08, 2.0e+07, -2.0e+08))
#labels # visualize lables created

sp_corr_plots <- ggplot(corr_data_two, aes(x = habitat_change, y = abund_change)) +
  geom_point(alpha = 0.3) +
  facet_wrap(species ~ .,ncol = 3, scales = "free_y") + # one pannel for each species
  labs(x = "Habitat suitability change", y = "Abundance change") + # label axis
  #geom_smooth(method = "loess", span = 0.1, size = 1) +
  stat_smooth(method = "lm", formula = y~x) + # plot a linear regression model for each species
  geom_text(data = labels, mapping = aes(x = xlabel, y = ylabel, label = formula), parse = TRUE, hjust = 0, size = 7/.pt) +
  geom_text(data = labels, mapping = aes(x = xr2, y = yr2, label = r2), parse = TRUE, hjust = 0, size = 7/.pt) +
  #geom_text(data = labels, aes(label = formula), x = 0, y = 150000, parse = TRUE, hjust = 0, size = 7/.pt) + # label for the equation
  #geom_text(x = 0, y = -350000, aes(label = r2), data = labels, parse = TRUE, hjust = 0, size = 7/.pt) + # label for the r2
  theme_minimal() + # main theme
  theme(panel.background =  element_blank(), # no background colour
        # background grids customization
        panel.grid.major =  element_blank(), 
        panel.grid.minor = element_line(color = "gray90", size = 0.25),
        # facets identification customization
        strip.text = element_text(face = "italic", size = rel(0.8)), 
        # axis and legend font type
        legend.title = element_text(face = "bold"), 
        axis.title = element_text(face = "bold"))
#sp_corr_plots

ggsave(plot = sp_corr_plots, file = "correlation_plots_species19Jul.tiff",  bg = 'white', width = 210, height = 270, units = "mm", dpi = 300, compression = "lzw")
gc()

######### new idea ############

corr_data_scenarios <- all_data[, c(9,10,12,15)]
corr_data_scenarios$abund_change <- as.numeric(corr_data_scenarios$abund_change) #the lm and the labels functions do not work with the integer64 format thta this column had previously

# function to write the model equation and the r2 
lm_labels <- function(df) {
  mod <- lm(abund_change ~ habitat_change, data = df, na.action = "na.exclude")
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1], 2), round(coef(mod)[2], 2)) # write the equation formula
  r2 <- sprintf("italic(R^2) == %.2f", summary(mod)$r.squared) # write th r2 for each group
  data.frame(formula = formula, r2 = r2, stringsAsFactors = FALSE)
}
# SPRINTF function returns a character vector containing a formatted combination of text and variable values

# create dataframe with the equations and r2
labels_scenarios <- corr_data_scenarios %>%
  group_by(species, scenario) %>% # group data by species
  do(lm_labels(.)) # use the function above to create the dataframe with equation and r2 for each species
#labels # visualize lables created


aa <- ggplot(corr_data_scenarios, aes(x = habitat_change, y = abund_change, color = scenario)) +
  #geom_point(alpha = 0.3) +
facet_wrap(species ~ ., ncol = 3, scales = "free_y") + # one pannel for each species
  labs(x = "Habitat suitability change", y = "Abundance change") + # label axis
  stat_smooth(method = "lm", formula = y~x) + # plot a linear regression model for each specieS
  scale_fill_viridis_c("Scenarios", option = "D") +
  theme_minimal() + # main theme
  theme(panel.background =  element_blank(), # no background colour
  #background grids customization
    panel.grid.major =  element_blank(),
    panel.grid.minor = element_line(color = "gray90", size = 0.25),
  # facets identification customization
    strip.text = element_text(face = "italic", size = rel(0.8)), 
  # axis and legend font type
    legend.title = element_text(face = "bold"), 
    axis.title = element_text(face = "bold"),
    legend.position = "bottom")

ggsave(plot = aa, file = "correlation_plots_scenarios_test19Jul.tiff",  bg = 'white', width = 210, height = 270, units = "mm", dpi = 1200, compression = "lzw")

#############################################

#sp_corr_plots_scenario <- ggplot(corr_data_scenarios, aes(x = habitat_change, y = abund_change, color = scenario, shape = scenario)) +
  #geom_point(alpha = 0.3) +
 # facet_wrap(species ~ .,ncol = 3, scales = "free_y") + # one pannel for each species
#  labs(x = "Habitat suitability change", y = "Abundance change") + # label axis
 # geom_smooth( aes(group = scenario ), method = "loess", span = 0.1, size = 1) +
  #stat_smooth(method = "lm", formula = y~x) + # plot a linear regression model for each species
  #geom_text(data = labels, aes(label = formula), x = 0, y = 150000, parse = TRUE, hjust = 0, size = 7/.pt) + # label for the equation
  #geom_text(x = 0, y = -350000, aes(label = r2), data = labels, parse = TRUE, hjust = 0, size = 7/.pt) + # label for the r2
  #theme_minimal() + # main theme
  #theme(panel.background =  element_blank(), # no background colour
        # background grids customization
    #    panel.grid.major =  element_blank(), 
   #     panel.grid.minor = element_line(color = "gray90", size = 0.25),
        # facets identification customization
     #   strip.text = element_text(face = "italic", size = rel(0.8)), 
        # axis and legend font type
      #  legend.title = element_text(face = "bold"), 
       # axis.title = element_text(face = "bold"))

#ggsave(plot = sp_corr_plots_scenario, file = "correlation_plots_scenarios_test2_17Jul.tiff",  bg = 'white', width = 210, height = 270, units = "mm", dpi = 1200, compression = "lzw")
gc()

#https://nilsreimer.com/post/gwtp-facets-and-curves/

########################
# SENSITIVITY ANALYSIS #
########################

#Sensistivity Analysis was moved to another file since the data had more that 18GB
