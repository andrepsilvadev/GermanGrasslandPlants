##### BLIZ #####
### Task 1.6 ###
# 11 Sept 2024 #
###### MIS #####
################

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
library(RColorBrewer) # palletes for the maps on step 4
library(openxlsx) # to write excel workbooks/sheets

##############################################
# STEP 1 - IMPORT AND MERGE ALL OUTPUT FILES #
##############################################

# merge model output into one complete dataset

# all_data <- list.files(recursive = T) %>% # get a list of the paths to datasets files
#   map_dfr(function(path) {
#   #retrieve any characters from the beginning of the path until /
#   foldername <- str_extract(path, "^.+(?=/)")
#   fread(path) %>% # import the files  
#   mutate(folder = foldername) # add a new collumn with the folder name
# })
# invisible(gc())
# 
# all_data <- all_data %>%
#  separate(folder, c("date", "time", "species", "scenario"), sep = "_", remove = FALSE) %>% # takes a VERY long time ^(more than 40 minutes)
#   dplyr::select(-c("folder", "date", "time"))
# invisible(gc())

# write tsv file with all species in all scenarios
#write_tsv(all_data, "full_dataset.tsv")



# Import the complete dataset 
all_data <- fread("./output_september/full_dataset.tsv", integer64 = "numeric")
invisible(gc())

#################################
# STEP 2 - CREATE NEW VARIABLES #
#################################

# occupancy
all_data <- all_data %>%
  mutate(occupancy = ifelse(all_data$abundance > 0, 1, 0)) 
invisible(gc())

# abundance change
all_data <- all_data %>%
  group_by(species, scenario) %>% 
  mutate(abund_change <- abundance - abundance[t == 25]) %>%
  ungroup()
invisible(gc())

# occupancy change
all_data <- all_data %>%
  group_by(species, scenario) %>% 
  mutate(occup_change <- occupancy - occupancy[t == 25]) %>%
  ungroup()
invisible(gc())

# habitat suitability change
all_data <- all_data %>%
  group_by(species, scenario) %>% 
  mutate(habitat_change <- habitat - habitat[t == 25]) %>%
  ungroup()
invisible(gc())

# change column names
colnames(all_data) <- c("t", "x", "y",  "abundance", "reproduction", "habitat", "carry",  "bevmort", "species", "scenario", "occupancy", "abund_change", "occup_change", "habitat_change" )

# chnage species names to have a space in between and lower case letter in the species
all_data <- all_data %>%
  mutate(species = recode(species,
                          "AchilleaMillefolium" ="Achillea millefolium",
                          "EriophorumVaginatum" = "Eriophorum vaginatum",
                          "FagusSylvatica" = "Fagus sylvatica",
                          "PlantagoLanceolata" = "Plantago lanceolata",
                          "PoaPratensis" = "Poa pratensis",
                          "PoaTrivialis" = "Poa trivialis",      
                          "RanunculusAcris" = "Ranunculus acris",
                          "RanunculusBulbosus" = "Ranunculus bulbosus",
                          "RumexAcetosa" = "Rumex acetosa",
                          "VeronicaChamaedrys" = "Veronica chamaedrys",
                          "ViolaArvensis" = "Viola arvensis"),
         scenario = recode(scenario,
                           "SSP1-landuse" = "SSP1",
                           "SSP1-climate+landuse" = "SSP1-RCP2.6",
                           "SSP1-climate" = "RCP2.6",
                           "SSP5-landuse" = "SSP5",
                           "SSP5-climate+landuse" = "SSP5-RCP8.5",
                           "SSP5-climate" = "RCP8.5")) %>% 
  dplyr::filter(! species %in% c("Viola arvensis","Eriophorum vaginatum" )) # remove 2 sps 
invisible(gc())

# # check if species were removed
# unique(all_data$species)
# unique(all_data$scenario)
# 
# #cehck why abundance mismatch plots dont show teh control scenario
# checks <- all_data[all_data$scenario == "control" & all_data$t >=25, c(1,2,3,6,14), ]
# summary(checks$habitat_change)

##########################################
# STEP 3 - CREATE POPULATION LEVEL PLOTS #
##########################################

# calculate mean values
data_all_mean_stdev <- all_data[, c(1,4:10)]

# mean and standard deviation for ABUNDANCE, REPRODUCTION, CARRYING CAPACITY, HABITAT SUITABILITY and MORTALITY variables
mean_stDEV <- data_all_mean_stdev %>%
  dplyr::group_by(species, scenario, t) %>%
  dplyr::summarise(across(c(abundance, reproduction, carry, habitat, bevmort), .fns = list(mean = mean, sd = sd), na.rm = TRUE), .groups = 'drop') %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>% 
  filter(t>=25)

# write table into .csv file
write.csv(mean_stDEV, "mean_values_25Sept_without_2sps.csv", row.names=FALSE)
invisible(gc())

# population plots' theme
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


############################
## ABUNDANCE THROUGH TIME ##
############################

#specify labels for plot
years <- c(2011, 2040, 2070, 2100)

# R4 palette
paletteR4 <- c("#000000", "#DF536B", "#61D04F" ,"#2297E6", "#CD0BBC", "#F5C710", "#9E9E9E")

# color blind palette with black
#cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#CC79A7")

# safe colour blind palette with 12 colours
#safe_colorblind_palette<- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")


abund_plot <- ggplot(mean_stDEV, aes(x = t, y = abundance_mean, group = scenario, color = scenario)) + 
  geom_line() + # line plot
  #geom_ribbon(aes(x = t, ymin = abundance_mean - abundance_sd, ymax = abundance_mean + abundance_sd, fill = scenario), alpha = 0.3, colour = NA) + # add ribbon with standard deviation values
  facet_wrap(. ~ species, ncol =3, scales = "free_y") +  # one vertical panel for each species
  ggh4x::facetted_pos_scales(
    y = list(scale_y_continuous(limits = c(2.5e+07, 1.2e+08)),
             scale_y_continuous(limits = c(2.0e+06, 4.5e+6)), 
             scale_y_continuous(limits = c(2.5e+07, 7.5e+07)),
             scale_y_continuous(limits = c(2.5e+07, 1.0e+08)),
             scale_y_continuous(limits = c(2.5e+07, 7.5e+07)),
             scale_y_continuous(limits = c(2.5e+07, 1.0e+08)),
             scale_y_continuous(limits = c(2.5e+07, 1.0e+08)),
             scale_y_continuous(limits = c(2.0e+07, 5.5e+07)),
             scale_y_continuous(limits = c(2.0e+07, 4.5e+07))))+
  #scale_colour_manual(values = safe_colorblind_palette) +
  #scale_fill_manual(values = safe_colorblind_palette) +
  scale_colour_manual(values = paletteR4) +
  scale_fill_manual(values = paletteR4) +
  #scale_colour_manual(values = paletteR4) +
  #scale_fill_manual(values = paletteR4) +
  scale_x_continuous(breaks = c(25, 54, 84, 115), labels = years) +
  labs(x = "Year" , y = "Abundance", color = "Scenarios", fill = "Scenarios") +  
  line_plots_theme + 
  theme(legend.position="bottom")
#abund_plot

# save ABUNDANCE THROUGH TIME plot
#ggsave(plot = abund_plot, file = "Abundance_through_time_Sept_without2sps_paletteR4.tiff", bg = 'white', width = 200, height = 180, units = "mm", dpi = 1200, compression = "lzw")


# quantification of abundance
abund_quant <- mean_stDEV %>% 
  dplyr::select(species, scenario, t, abundance_mean) %>% 
  dplyr::filter(t %in% c(25, 115)) %>% 
  pivot_wider(names_from = t, values_from = abundance_mean) %>% 
  group_by(species, scenario) %>% 
  mutate(change_2100_2011 = `115`-`25`)

# write table into .csv file
write.csv(abund_quant, "abundance_quantification_25Sept_without_2sps.csv", row.names=FALSE)
invisible(gc())


###############################
## REPRODUCTION THROUGH TIME ##
###############################

reprod_plot <- ggplot(mean_stDEV, aes(x = t, y = reproduction_mean, group = scenario, color = scenario)) +
  geom_line() + # line plot
  #geom_ribbon(aes(x = t, ymin = reproduction_mean - reproduction_sd, ymax = reproduction_mean + reproduction_sd, fill = scenario), alpha = 0.3, color =NA) + # add ribbon with standard deviation values
  facet_wrap(. ~ species,  scales = "free_y") + # one vertical panel for each species
  ggh4x::facetted_pos_scales(
    y = list(scale_y_continuous(limits = c(100, 220)),
             scale_y_continuous(limits = c(0.8, 1.5)),
             scale_y_continuous(limits = c(0.8, 2.0)),
             scale_y_continuous(limits = c(0.8, 2.0)),
             scale_y_continuous(limits = c(0.7, 1.5)),
             scale_y_continuous(limits = c(1.0, 1.7)),
             scale_y_continuous(limits = c(0.8, 2.0)),
             scale_y_continuous(limits = c(0.8, 2.0)),
             scale_y_continuous(limits = c(0.8, 2.0)))) +
  scale_colour_manual(values = paletteR4) +
  scale_fill_manual(values = paletteR4) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +  # set xx axis to start at zero and more space at the top of the yy axis
  labs(x ="Year", y = "Reproduction") + # axis labels (note: fill and color must have the same name to have only one legend)
  line_plots_theme +
  theme(legend.position = "bottom")
#reprod_plot

#Save reproduction through time plot
#ggsave(plot = reprod_plot, file = "Reproduction_through_time_25Sept_without2sps_paletteR4.tiff",  bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200, compression = "lzw")

######################################
## HABITAT SUITABILITY THROUGH TIME ##
######################################

habitat_plot <- ggplot(mean_stDEV, aes(x = t, y = habitat_mean, group = scenario, color = scenario)) +
  geom_line() + # line plot
  #geom_ribbon(aes(x = t, ymin = habitat_mean - habitat_sd, ymax = habitat_mean + habitat_sd, fill = scenario), alpha = 0.3, color = NA) + # add ribbon with standard deviation values
  facet_wrap(. ~ species, ncol = 3, scales = "free_y") + # one vertical panel for each species
  ggh4x::facetted_pos_scales(
    y = list(scale_y_continuous(limits = c(0.2,0.7)),
             scale_y_continuous(limits = c(0.4, 0.7)),
             scale_y_continuous(limits = c(0.2,0.6)),
             scale_y_continuous(limits = c(0.3,0.7)),
             scale_y_continuous(limits = c(0.2,0.6)),
             scale_y_continuous(limits = c(0.3,0.6)),
             scale_y_continuous(limits = c(0.3,0.7)),
             scale_y_continuous(limits = c(0.3,0.7)),
             scale_y_continuous(limits = c(0.3,0.7)))) +
  scale_colour_manual(values = paletteR4) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.1)), breaks = seq(0, 1, 0.2)) +  # set xx axis to start at zero and more space at the top of the yy axis
  labs( x ="Year" , y = "Habitat suitability", color = "Scenarios", fill = "Scenarios") + # axis labels (note: fill and color must have the same name to have only one legend)
  line_plots_theme +
  theme(legend.position = "bottom")
#habitat_plot

#Save habitat suitability through time plot
#ggsave(plot = habitat_plot, file = "Habitat_suitability_through_time_25Sept_without2sps_paletteR4.tiff",  bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200, compression = "lzw")

####################################
## CARRYING CAPACITY THROUGH TIME ##
####################################

carry_cap_plot <- ggplot(mean_stDEV, aes(x = t, y = carry_mean, group = scenario, color = scenario)) +
  geom_line() + # line plot
  #geom_ribbon(aes(x = t, ymin = carry_mean - carry_sd, ymax = carry_mean + carry_sd, fill = scenario), alpha = 0.3, color = NA) +  # add ribbon with standard deviation values
  facet_wrap(. ~ species, ncol = 3, scales = "free_y") + # one vertical panel for each species
  ggh4x::facetted_pos_scales(
    y = list(scale_y_continuous(limits = c(4.0e+07, 1.0e+08)),
             scale_y_continuous(limits = c(2.0e+06, 5.0e+06)),
             scale_y_continuous(limits = c(2.0e+07, 6.5e+07)),
             scale_y_continuous(limits = c(2.0e+07, 8e+07)),
             scale_y_continuous(limits = c(2.5e+07, 8e+07)),
             scale_y_continuous(limits = c(4e+07, 9e+07)),
             scale_y_continuous(limits = c(4.0e+07, 1.2e+08)),
             scale_y_continuous(limits = c(2.0e+07, 6.0e+07)),
             scale_y_continuous(limits = c(2.0e+7, 5e+07)))) +
  scale_colour_manual(values = paletteR4) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +  # set xx axis to start at zero and more space at the top of the yy axis
  labs( x ="Year" , y = "Carrying capacity", color = "Scenarios", fill = "Scenarios") + # axis labels (note: fill and color must have the same name to have only one legend)
  line_plots_theme +
  theme(legend.position = "bottom")
#carry_cap_plot

#Save carrying capacity through time plot
#ggsave(plot = carry_cap_plot, file = "Carrying_capacity_through_time_25Sept_without2sps_paletteR4.tiff",  bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200, compression = "lzw")

############################
## MORTALITY THROUGH TIME ##
############################

mort_plot <- ggplot(mean_stDEV, aes(x = t, y = bevmort_mean, group = scenario, color = scenario)) +
  geom_line() + # line plot
  #geom_ribbon(aes(x = t, ymin = bevmort_mean - bevmort_sd, ymax = bevmort_mean + bevmort_sd, fill = scenario), alpha = 0.3, color = NA) + # add ribbon with standard deviation values
  facet_wrap(. ~ species, ncol = 3, scales = "free_y") + # vertical panels for each species distributed in e columns
  ggh4x::facetted_pos_scales(
    y = list(scale_y_continuous(limits = c(0.7, 1.1)),
             scale_y_continuous(limits = c(0.3, 0.6)),
             scale_y_continuous(limits = c(0.6, 1.1)),
             scale_y_continuous(limits = c(0.8, 1.1)),
             scale_y_continuous(limits = c(0.7, 1.1)),
             scale_y_continuous(limits = c(0.7, 1.1)),
             scale_y_continuous(limits = c(0.7, 1.1)),
             scale_y_continuous(limits = c(0.7, 1.1)),
             scale_y_continuous(limits = c(0.7, 1.1)))) +
  scale_colour_manual(values = paletteR4) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +  # set xx axis to start at zero and more space at the top of the yy axis
  labs( x ="Year" , y = "Mortality", color = "Scenarios", fill = "Scenarios") + # axis labels (note: fill and color must have the same name to have only one legend)
  line_plots_theme +
  theme(legend.position = "bottom")
#mort_plot

#Save mortality through time plot
#ggsave(plot = mort_plot, file = "Mortality_through_time_25Sept_without2sps_paletteR4.tiff",  bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200, compression = "lzw")


#################################
## ABUNDANCE MISMATCH BOXPLOTS ##
#################################

abund_mismatch_df <- all_data[which(all_data$habitat_change >0), ]
invisible(gc())

abund_mismatch_df <- abund_mismatch_df[abund_mismatch_df$t==115,]
invisible(gc())
#summary(abund_mismatch_df)

# abundance mismatch
abund_mismatch_df <- abund_mismatch_df %>%
  group_by(species, scenario) %>% 
  mutate(abund_mismatch <- carry - abundance) %>%
  ungroup() %>% 
  rename(abund_mismatch = 15)
invisible(gc())

# # abundance mismatch boxplots with outliers
# abund_mismatch_plot <- ggplot(abund_mismatch_df, aes(x = as.factor(scenario), y = abund_mismatch, fill = scenario)) +
#   geom_boxplot() + 
#   labs(x = "SSP scenarios", y = "Abundance Mismatch", fill ="Scenarios") + #axis labels
#   facet_wrap(. ~ species, ncol = 3, scales = "free_y") + #one vertical panel per species "SO UM EIXO FREE"
#   line_plots_theme + 
#   theme(panel.background =  element_blank(),
#         panel.grid.major =  element_line(color = "gray90", size = 0.25),
#         panel.grid.minor = element_line(color = "gray90", size = 0.25),
#         axis.text.x = element_text(angle = 60, vjust = 1.0, hjust = 1.0),
#         legend.position = "bottom")
# abund_mismatch_plot
# not saving this plot


###########################################
# ORIGINIAL PLOT WITH VIRIDIS COLOR SCALE #
###########################################

# abundance mismatch without outliers and the 10% an 90% quantiles
abund_mismatch_plot2 <- ggplot(abund_mismatch_df, aes(x=as.factor(scenario), y = abund_mismatch, fill = scenario)) +
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
        legend.position = "none")
#abund_mismatch_plot2

# saving the plot
#ggsave(plot = abund_mismatch_plot2, file = "Abundance_mismatch_SeptWITHOUT_OUTLIERS_without2sps_original.tiff",  bg = 'white', width = 250, height = 280, units = "mm", dpi = 1200, compression = "lzw")
invisible(gc())

##############################################################################
# NEW IDEA TO COLOR CODE WITH RED AND BLUE BASED ON MEAN VALUES "TENDENCIES" #
##############################################################################


# with red for negative and blue for positive values ---------------------------

# calculate the mean abund_mismatch for each combination of scenario and species
abund_mismatch_df <- abund_mismatch_df %>%
  group_by(scenario, species) %>%
  mutate(mean_abund_mismatch = mean(abund_mismatch, na.rm = TRUE)) %>%
  ungroup()

# create a new column to color (based on whether the mean is positive or negative)
abund_mismatch_df$color_group <- ifelse(abund_mismatch_df$mean_abund_mismatch < 0, "negative", "positive")

# code to color the boxplots based on the mean
abund_mismatch_plot_red_blue <- ggplot(abund_mismatch_df, aes(x = as.factor(scenario), y = abund_mismatch, fill = color_group)) +
  geom_boxplot(outliers = FALSE) + 
  labs(x = "SSP scenarios", y = "Abundance Mismatch", fill = "Mean Mismatch") + # axis labels
  facet_wrap(. ~ species, ncol = 3, scales = "free_y") + # one vertical panel per species
  stat_summary(geom = "point", fun = \(x) quantile(x, 0.1, na.rm = T), shape = 3, size = 2, color = "red") +
  stat_summary(geom = "point", fun = \(x) quantile(x, 0.9, na.rm = T), shape = 3, size = 2, color = "blue") +
  scale_fill_manual(values = c("negative" = "red", "positive" = "blue")) + # red for negative, blue for positive
  line_plots_theme + 
  theme(panel.background =  element_blank(),
        panel.grid.major =  element_line(color = "gray90", size = 0.25),
        panel.grid.minor = element_line(color = "gray90", size = 0.25),
        axis.text.x = element_text(angle = 60, vjust = 1.0, hjust = 1.0),
        legend.position = "none")
#abund_mismatch_plot_red_blue

# saving the plot
#ggsave(plot = abund_mismatch_plot_red_blue, file = "Abundance_mismatch_SeptWITHOUT_OUTLIERS_without2sps_red_blu.tiff",  bg = 'white', width = 250, height = 280, units = "mm", dpi = 1200, compression = "lzw")
invisible(gc())




# with a gradient across all species -------------------------------------------

# the gradient fill here is based on the mean_abund_mismatch
abund_mismatch_plot_gradient <- ggplot(abund_mismatch_df, aes(x = as.factor(scenario), y = abund_mismatch, fill = mean_abund_mismatch)) +
  geom_boxplot(outliers = FALSE) + 
  labs(x = "SSP scenarios", y = "Abundance Mismatch", fill = "Mean Abundance Mismatch") + # axis labels
  facet_wrap(. ~ species, ncol = 3, scales = "free_y") + # one vertical panel per species
  stat_summary(geom = "point", fun = \(x) quantile(x, 0.1, na.rm = T), shape = 3, size = 2, color = "red") +
  stat_summary(geom = "point", fun = \(x) quantile(x, 0.9, na.rm = T), shape = 3, size = 2, color = "blue") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, 
                       name = "Mean Mismatch")  + # gradient color scale
  line_plots_theme + 
  theme(panel.background =  element_blank(),
        panel.grid.major =  element_line(color = "gray90", size = 0.25),
        panel.grid.minor = element_line(color = "gray90", size = 0.25),
        axis.text.x = element_text(angle = 60, vjust = 1.0, hjust = 1.0))
#abund_mismatch_plot_gradient

# saving the plot
#ggsave(plot = abund_mismatch_plot_gradient, file = "Abundance_mismatch_SeptWITHOUT_OUTLIERS_without2sps_gradient.tiff",  bg = 'white', width = 250, height = 280, units = "mm", dpi = 1200, compression = "lzw")
invisible(gc())




# with a gradient within each facet (within each species) ----------------------

# normalize the mean_abund_mismatch within each species
abund_mismatch_df <- abund_mismatch_df %>%
  group_by(species) %>%
  mutate(mean_abund_mismatch_scaled = scale(mean_abund_mismatch)) %>%
  ungroup()

# use the scaled mean for the gradient fill
abund_mismatch_plot_GWF <- ggplot(abund_mismatch_df, aes(x = as.factor(scenario), y = abund_mismatch, fill = mean_abund_mismatch_scaled)) +
  geom_boxplot(outliers = FALSE) + 
  labs(x = "SSP scenarios", y = "Abundance Mismatch", fill = "Mean Mismatch (Scaled)") + # axis labels
  facet_wrap(. ~ species, ncol = 3, scales = "free_y") + # one vertical panel per species
  stat_summary(geom = "point", fun = \(x) quantile(x, 0.1, na.rm = T), shape = 3, size = 2, color = "red") +
  stat_summary(geom = "point", fun = \(x) quantile(x, 0.9, na.rm = T), shape = 3, size = 2, color = "blue") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, 
                       name = "Mean Mismatch") +  # gradient color scale
  line_plots_theme + 
  theme(panel.background =  element_blank(),
        panel.grid.major =  element_line(color = "gray90", size = 0.25),
        panel.grid.minor = element_line(color = "gray90", size = 0.25),
        axis.text.x = element_text(angle = 60, vjust = 1.0, hjust = 1.0),
        legend.position = "bottom")
#abund_mismatch_plot_GWF


# saving the plot
#ggsave(plot = abund_mismatch_plot_GWF, file = "Abundance_mismatch_SeptWITHOUT_OUTLIERS_without2sps_GWF.tiff",  bg = 'white', width = 250, height = 280, units = "mm", dpi = 1200, compression = "lzw")
invisible(gc())




# with a gradient within each facet (within each species) and with outliers ----

abund_mismatch_plot_GWF_withoutliers <- ggplot(abund_mismatch_df, aes(x = as.factor(scenario), y = abund_mismatch, fill = mean_abund_mismatch_scaled)) +
  geom_boxplot() + 
  labs(x = "SSP scenarios", y = "Abundance Mismatch", fill = "Mean Mismatch (Scaled)") + # axis labels
  facet_wrap(. ~ species, ncol = 3, scales = "free_y") + # one vertical panel per species
  stat_summary(geom = "point", fun = \(x) quantile(x, 0.1, na.rm = T), shape = 3, size = 2, color = "red") +
  stat_summary(geom = "point", fun = \(x) quantile(x, 0.9, na.rm = T), shape = 3, size = 2, color = "blue") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, 
                       name = "Mean Mismatch") +  # gradient color scale
  line_plots_theme + 
  theme(panel.background =  element_blank(),
        panel.grid.major =  element_line(color = "gray90", size = 0.25),
        panel.grid.minor = element_line(color = "gray90", size = 0.25),
        axis.text.x = element_text(angle = 60, vjust = 1.0, hjust = 1.0),
        legend.position = "bottom")
# abund_mismatch_plot_GWF_withoutliers

# saving the plot
# ggsave(plot = abund_mismatch_plot_GWF_withoutliers, file = "Abundance_mismatch_SeptWITH_OUTLIERS_without2sps_GWF.tiff",  bg = 'white', width = 250, height = 280, units = "mm", dpi = 1200, compression = "lzw")
invisible(gc())


##################################################
# STEP 4 - CREATE CELL LEVEL PLOTS (per species) #
##################################################

# abundance change proportion
all_data <- all_data %>%
  group_by(species, scenario) %>% 
  mutate(abund_change_prop <- (abundance - abundance[t == 25])/abundance[t == 25]) %>%
  rename(abund_change_prop = 15)  %>% 
  ungroup()
invisible(gc())


# subset data for the last time step (t = 90)
abund_t115<-all_data[all_data$t == 115, c(2,3,9,10,4,15)]

#get the names of each scenario
scenarios <- unique(abund_t115$scenario)

# Define the RdBu palette with a midpoint at 0
palette <- brewer.pal(11, "RdBu")

#########################################
## PROPORTION OF ABUNDANCE CHANGE MAPS ##
#########################################

# create an empty list to store the plots
plot_list <- list()

# Loop through each scenario
for (scenario in scenarios) {
  # Filter data for the current scenario
  abund_scenario <- abund_t115[abund_t115$scenario == scenario,]
  
  # new way of having the spatial maps to have a different fill scale for each species
  abundance_change_prop_map <- ggplot() +
    geom_raster(data = abund_scenario, aes(x = x, y = -y, fill = abund_change_prop)) +
    facet_wrap(species ~ . ) + # vertical panels for each species in 3 columns
    scale_fill_gradientn(colors = palette, na.value = "transparent",
                         limits = c(-2, 2)) +
    labs(x = "Cell longitude", y = "Cell latitude", fill = "Proportion of \nabundance change") +
    theme(legend.position = "bottom", panel.background =  element_blank(), # no background grids
          # facets identification customization
          strip.background =  element_rect(fill = "grey94", color = "black"), 
          strip.text = element_text(face = "italic", size = rel(0.8)), 
          panel.border =element_rect(color = "black", fill = NA),
          # axis and legend font type
          legend.title = element_text(face = "bold"),
          legend.text=element_text(size = 6),
          axis.title = element_text(face = "bold")) 
  
  # save each plot in the list
  plot_list[[scenario]] <- abundance_change_prop_map
  
  # save each scenario map as a separate image
  ggsave(paste0("abundance_change_prop_map_scenario_without2sps", scenario, ".tiff"), abundance_change_prop_map, bg = 'white', width = 230, height = 210, units = "mm", dpi = 1200, compression = "lzw")
}


# quantification of the abundance change 
abund_chnage_prop_quant <- all_data %>% 
  dplyr::select(species, scenario, t, x, y, abund_change) %>% 
  dplyr::filter(t %in% c(115)) %>% 
  group_by(species, scenario) %>% 
  dplyr::summarise(abund_change_prop_2100_2011 = mean(abund_change, na.rm = TRUE))

# write table into .csv file
write.csv(abund_chnage_prop_quant, "proportion_of_abundance_quantification_25Sept_without_2sps.csv", row.names=FALSE)
invisible(gc())


#####################################
## SPATIAL HOTSPOTS OF CHANGE MAPS ##
#####################################

new_data <- abund_t115 %>% 
  group_by(scenario, x, y) %>%
  summarise_at(vars(abund_change_prop), list(mean_proportion_of_change_across_species = mean))

spatial_hotspots_map <- ggplot() +
  geom_raster(data = new_data, aes(x = x, y = -y, fill = mean_proportion_of_change_across_species)) +
  facet_wrap(scenario ~ . ) + # vertical panels for each species in 3 columns
  scale_fill_gradientn(colors = palette, na.value = "transparent",
                       limits = c(min(new_data$mean_proportion_of_change_across_species, na.rm = TRUE), 
                                  max(new_data$mean_proportion_of_change_across_species, na.rm = TRUE)),
                       values = scales::rescale(c(min(new_data$mean_proportion_of_change_across_species, na.rm = TRUE), 0, 
                                                  max(new_data$mean_proportion_of_change_across_species, na.rm = TRUE)))) +
  labs(x = "Cell longitude ", y ="Cell latitude", fill = "MPCAS (Mean proportion \nof change \nacross species)") +
  theme(legend.position = c(0.8,0.1), panel.background =  element_blank(), # no background grids
        # facets identification customization
        strip.background =  element_rect(fill = "grey94", color = "black"), 
        strip.text = element_text(face = "italic", size = rel(0.8)), 
        panel.border =element_rect(color = "black", fill = NA),
        # axis and legend font type
        legend.title = element_text(face = "bold"),
        legend.text=element_text(size = 6),
        axis.title = element_text(face = "bold"))
spatial_hotspots_map
ggsave(plot = spatial_hotspots_map, file = "Spatial_hotspots_map_without2sps.tiff",  bg = 'white', width = 210, height = 210, units = "mm", dpi = 1200, compression = "lzw")

################################################
# WRITE TABLE WITH VALUES OF PROP ABUND CHANGE #
################################################
# 
# # Split dataframe by scenario
# split_abund_t115 <- split(abund_t115, abund_t115$scenario)
# 
# # create a workbook
# wb <- createWorkbook()
# 
# # add the full dataframe as the first sheet
# addWorksheet(wb, sheetName = "Full_Data_per_species")
# writeData(wb, sheet = "Full_Data_per_species", abund_t115)
# 
# # loop through each dataframe in the list and add it as a new tab inside the excel sheet
# for(scenario in names(split_abund_t115)) {
#   addWorksheet(wb, sheetName = scenario)    # name new tabs with scenario name
#   writeData(wb, sheet = scenario, split_abund_t115[[scenario]])  # write data to each tab
# }
# 
# # add the prop of chnage across species dataframe as another tab in the same workbook
# addWorksheet(wb, sheetName = "Prop_Abund_across_species")
# writeData(wb, sheet = "Prop_Abund_across_species", new_data)
# 
# # save the workbook with all sheets
# saveWorkbook(wb, file = "Prop_abundance_chnage_dataframes_without2sps.xlsx", overwrite = TRUE)
# invisible(gc())
# 

#######################
## CORRELATION PLOTS ##
#######################

# new idea with just the trend lines -------------------------------------------

corr_data_scenarios <- all_data[all_data$t == 115, c(9,10,12,14)]
corr_data_scenarios$abund_change <- as.numeric(corr_data_scenarios$abund_change) #the lm and the labels functions do not work with the integer64 format thta this column had previously
colnames(corr_data_scenarios)

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
gc() 

correlation_plot <- ggplot(corr_data_scenarios, aes(x = habitat_change, y = abund_change, color = scenario)) +
  #geom_point(alpha = 0.3) + # uncomment here to get the data points in the plot
  facet_wrap(species ~ ., ncol = 3, scales = "free_y") + # one pannel for each species
  labs(x = "Habitat suitability change", y = "Abundance change") + # label axis
  stat_smooth(method = "lm", formula = y~x) + # plot a linear regression model for each specieS
  scale_colour_manual(values = paletteR4) +
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
invisible(gc) 
ggsave(plot = correlation_plot, file = "Correlation_plots_scenarios_25Sept_without2sps.tiff",  bg = 'white', width = 210, height = 270, units = "mm", dpi = 1200, compression ="lzw")


