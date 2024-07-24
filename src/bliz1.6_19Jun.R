
## BLIZ ##
## Task 1.6 ##
#NEW TEST RUN WITH DATA FROM 19TH JUNE 2024

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
library(bit64)


setwd("C:/Users/User/OneDrive - Universidade de Lisboa (1)/ANDRE/19Jun")

#all_data <- list.files(recursive = T) %>% #get a list of the paths to datasets files
 #map_dfr(function(path) {
    #retrieve any characters from the beginning of the path until /
 #foldername <- str_extract(path, "^.+(?=/)")
 #fread(path) %>% #import the files  
 #mutate(folder = foldername) # add a new collumn with the folder name
#})
#gc()

#all_data <- all_data %>%
 #separate(folder, c("date", "time", "species", "scenario"), sep = "_", remove = FALSE) %>% #takes a VERY long time ^(more than 40 minutes)
 #dplyr::select(-c("folder", "date", "time"))
#gc()

#write tsv file with all species in all scenarios
#write_tsv(all_data, "full_dataset.tsv")

################################################################################
######################## test with the complete dataset ########################
################################################################################

all_data <- fread("./full_dataset.tsv")
#Step 2 → Create new variables

#occupancy
all_data <- all_data %>%
  mutate(occupancy = ifelse(all_data$abundance > 0, 1, 0)) 
gc()

#abundance change
all_data <- all_data%>%
  group_by(species, scenario) %>% 
  mutate(abund_change <- abundance - abundance[t == 1]) %>%
  ungroup()
gc()

all_data <- all_data %>%
  mutate(occupancy = ifelse(all_data$abundance > 0, 1, 0)) 
gc()

#abundance change
all_data <- all_data %>%
  group_by(species, scenario) %>% 
  mutate(abund_change <- abundance - abundance[t == 1]) %>%
  ungroup()
gc()

#occupancy change
all_data <- all_data %>%
  group_by(species, scenario) %>% 
  mutate(occup_change <- occupancy - occupancy[t == 1]) %>%
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
  mutate(habitat_change <- habitat - habitat[t == 1]) %>%
  ungroup()

#remove unused memory
gc()

#change column names
colnames(all_data) <- c("t", "x", "y",  "abundance", "reproduction", "habitat", "carry",  "bevmort", "species", "scenario", "occupancy", "abund_change", "occup_change", "abund_mismatch", "habitat_change" )

#Step 3 → Create Population Level Plots

#calculate mean values
data_all_mean_stdev<-all_data[, c(1,4:10)]

#Mean and standard deviation for ABUNDANCE, REPRODUCTION, CARRYING CAPACITY, HABITAT SUITABILITY and MORTALITY variables
mean_stDEV <- data_all_mean_stdev %>%
  dplyr::group_by(species, scenario, t) %>%
  dplyr::summarise(across(c(abundance, reproduction, carry, habitat, bevmort), .f = list(mean = mean, sd = sd), na.rm = TRUE), .groups = 'drop') %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) 

################################################################################
############ test with a subset of all data using fread function ###############
################################################################################


subset_data <- fread("./full_dataset.tsv", nrows =  8000000)
#unique(all_data_subset$species) #2 sps all scenarios at least for one sps
#gc()

#occupancy
subset_data <- subset_data %>%
  mutate(occupancy = ifelse(subset_data$abundance > 0, 1, 0)) 
gc()

#abundance change
subset_data <- subset_data %>%
  group_by(species, scenario) %>% 
  mutate(abund_change <- abundance - abundance[t == 1]) %>%
  ungroup()

#occupancy change
subset_data <- subset_data %>%
  group_by(species, scenario) %>% 
  mutate(occup_change <- occupancy - occupancy[t == 1]) %>%
  ungroup()
gc()

#abundance mismatch
subset_data <- subset_data %>%
  group_by(species, scenario) %>% 
  mutate(abund_mismatch <- carry - abundance) %>%
  ungroup()
gc()

#habitat change
subset_data <- subset_data %>%
  group_by(species, scenario) %>% 
  mutate(habitat_change <- habitat - habitat[t == 1]) %>%
  ungroup()

#remove unused memory
gc()

#change column names
colnames(subset_data) <- c("t", "x", "y",  "abundance", "reproduction", "habitat", "carry",  "bevmort", "species", "scenario", "occupancy", "abund_change", "occup_change", "abund_mismatch", "habitat_change" )

#Step 3 → Create Population Level Plots

#calculate mean values
subset_data_mean_stdev<-subset_data[, c(1,4:10)]


#Mean and standard deviation for ABUNDANCE, REPRODUCTION, CARRYING CAPACITY, HABITAT SUITABILITY and MORTALITY variables
mean_stDEV2 <- subset_data_mean_stdev %>%
  dplyr::group_by(species, scenario, t) %>%
  dplyr::summarise(across(c(abundance, reproduction, carry, habitat, bevmort), .f = list(mean = mean, sd = sd), na.rm = TRUE), .groups = 'drop') %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) 


################################################################################
###################### test with read.table function ###########################
################################################################################

all_data2 <- read.table(file = './full_dataset.tsv', sep = '\t', header = TRUE)

#occupancy
all_data2 <- all_data2 %>%
  mutate(occupancy = ifelse(all_data2$abundance > 0, 1, 0)) 
gc()

#abundance change
all_data2 <- all_data2 %>%
  group_by(species, scenario) %>% 
  mutate(abund_change <- abundance - abundance[t == 1]) %>%
  ungroup()

#occupancy change
all_data <- all_data2 %>%
  group_by(species, scenario) %>% 
  mutate(occup_change <- occupancy - occupancy[t == 1]) %>%
  ungroup()
gc()

#abundance mismatch
all_data2 <- all_data2 %>%
  group_by(species, scenario) %>% 
  mutate(abund_mismatch <- carry - abundance) %>%
  ungroup()
gc()

#habitat change
all_data2 <- all_data2 %>%
  group_by(species, scenario) %>% 
  mutate(habitat_change <- habitat - habitat[t == 1]) %>%
  ungroup()

#remove unused memory
gc()

#change column names
colnames(all_data2) <- c("t", "x", "y",  "abundance", "reproduction", "habitat", "carry",  "bevmort", "species", "scenario", "occupancy", "abund_change", "occup_change", "abund_mismatch", "habitat_change" )

#Step 3 → Create Population Level Plots

#calculate mean values
data_all2_mean_stdev<-all_data2[, c(1,4:10)]


#Mean and standard deviation for ABUNDANCE, REPRODUCTION, CARRYING CAPACITY, HABITAT SUITABILITY and MORTALITY variables
mean_stDEV3 <- data_all2_mean_stdev %>%
  dplyr::group_by(species, scenario, t) %>%
  dplyr::summarise(across(c(abundance, reproduction, carry, habitat, bevmort), .f = list(mean = mean, sd = sd), na.rm = TRUE), .groups = 'drop') %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) 


################################################################################
################################################################################
################################################################################





write.csv(mean_stDEV, "mean_values_19Jun.csv", row.names=FALSE)
#mean_stDEV <- fread("./mean_values_19Jun.csv")
datatable(mean_stDEV)
#print(mean_stDEV)
gc()


##DT TABLE CUSTOM CONTAINERS
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

## ABUNDANCE THROUGH TIME ##
abund_plot <- ggplot(mean_stDEV, aes(x = t, y = abundance_mean, group = scenario, color = scenario)) + 
  geom_line() + # line plot
  geom_ribbon(aes(x = t, ymin = abundance_mean - abundance_sd, ymax = abundance_mean + abundance_sd, fill = scenario), alpha = 0.3, colour = NA) + # add ribbon with standard deviation values
  facet_wrap(. ~ species, ncol =3, scales = "free_y") +  # one vertical panel for each species
  scale_fill_viridis_d(option = "D") + # colour palette
  scale_color_viridis_d(option = "D") + 
  labs(x ="Time" , y = "Abundance", color = "Scenarios", fill = "Scenarios") + # axis labels
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) + # set xx axis to start at zero and more space at the top of the yy axis
  line_plots_theme + 
  theme(legend.position="bottom")
abund_plot

#save abundance through time plot
ggsave(plot = abund_plot, file = "Abundance_through_time.tiff", bg = 'white', width = 200, height = 180, units = "mm", dpi = 1200, compression = "lzw")


## REPRODUCTION THROUGH TIME ##
reprod_plot <- ggplot(mean_stDEV, aes(x = t, y = reproduction_mean, group = scenario, color = scenario)) +
  geom_line() + # line plot
  geom_ribbon(aes(x = t, ymin = reproduction_mean - reproduction_sd, ymax = reproduction_mean + reproduction_sd, fill = scenario), alpha = 0.3, color =NA) + # add ribbon with standard deviation values
  facet_wrap(. ~ species,  scales = "free_y") + # one vertical panel for each species
  scale_fill_viridis_d(option = "D") + # colour palette
  scale_color_viridis_d(option = "D") + 
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +  # set xx axis to start at zero and more space at the top of the yy axis
  labs(x ="Time", y = "Reproduction") + # axis labels (note: fill and color must have the same name to have only one legend)
  line_plots_theme +
  theme(legend.position = "bottom")

#Save reproduction through time plot
ggsave(plot = reprod_plot, file = "Reproduction_through_time.tiff",  bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200, compression = "lzw")


## HABITAT SUITABILITY THROUGH TIME ##
habitat_plot0.1 <- ggplot(mean_stDEV, aes(x = t, y = habitat_mean, group = scenario, color = scenario)) +
  geom_line() + # line plot
  geom_ribbon(aes(x = t, ymin = habitat_mean - habitat_sd, ymax = habitat_mean + habitat_sd, fill = scenario), alpha = 0.3, color = NA) + # add ribbon with standard deviation values
  facet_wrap(. ~ species, ncol = 3) + # one vertical panel for each species
  scale_fill_viridis_d(option = "D") + # colour palette
  scale_color_viridis_d(option = "D") + 
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.1)), breaks = seq(0, 1, 0.1)) +  # set xx axis to start at zero and more space at the top of the yy axis
  labs( x ="Time" , y = "Habitat suitability", color = "Scenarios", fill = "Scenarios") + # axis labels (note: fill and color must have the same name to have only one legend)
  line_plots_theme + 
  theme(legend.position = "bottom")
ggsave(plot = habitat_plot0.1, file = "Habitat_suitability_through_time0.1.tiff",  bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200, compression = "lzw")



habitat_plot0.2 <- ggplot(mean_stDEV, aes(x = t, y = habitat_mean, group = scenario, color = scenario)) +
  geom_line() + # line plot
  geom_ribbon(aes(x = t, ymin = habitat_mean - habitat_sd, ymax = habitat_mean + habitat_sd, fill = scenario), alpha = 0.3, color = NA) + # add ribbon with standard deviation values
  facet_wrap(. ~ species, ncol = 3) + # one vertical panel for each species
  scale_fill_viridis_d(option = "D") + # colour palette
  scale_color_viridis_d(option = "D") + 
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.1)), breaks = seq(0, 1, 0.2)) +  # set xx axis to start at zero and more space at the top of the yy axis
  labs( x ="Time" , y = "Habitat suitability", color = "Scenarios", fill = "Scenarios") + # axis labels (note: fill and color must have the same name to have only one legend)
  line_plots_theme + 
  theme(legend.position = "bottom")
ggsave(plot = habitat_plot0.2, file = "Habitat_suitability_through_time0.2.tiff",  bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200, compression = "lzw")

#Save habitat suitability through time plot
#ggsave(plot = habitat_plot, file = "Habitat_suitability_through_time.tiff",  bg = 'white', width = 170, height = 150, units = "mm", dpi = 1200, compression = "lzw")



## CARRYING CAPACITY THROUGH TIME ##
carry_cap_plot <- ggplot(mean_stDEV, aes(x = t, y = carry_mean, group = scenario, color = scenario)) +
  geom_line() + # line plot
  geom_ribbon(aes(x = t, ymin = carry_mean - carry_sd, ymax = carry_mean + carry_sd, fill = scenario), alpha = 0.3, color = NA) +  # add ribbon with standard deviation values
  facet_wrap(. ~ species, ncol = 3, scales = "free_y") + # one vertical panel for each species
  scale_fill_viridis_d(option = "D") + # colour palette
  scale_color_viridis_d(option = "D") + 
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +  # set xx axis to start at zero and more space at the top of the yy axis
  labs( x ="Time" , y = "Carrying capacity", color = "Scenarios", fill = "Scenarios") + # axis labels (note: fill and color must have the same name to have only one legend)
  line_plots_theme + 
  theme(legend.position = "bottom")

#Save carrying capacity through time plot
ggsave(plot = carry_cap_plot, file = "Carrying_capacity_through_time.tiff",  bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200, compression = "lzw")


## MORTALITY THROUGH TIME
mort_plot <- ggplot(mean_stDEV, aes(x = t, y = bevmort_mean, group = scenario, color = scenario)) +
  geom_line() + # line plot
  geom_ribbon(aes(x = t, ymin = bevmort_mean - bevmort_sd, ymax = bevmort_mean + bevmort_sd, fill = scenario), alpha = 0.3, color = NA) + # add ribbon with standard deviation values
  facet_wrap(. ~ species, ncol = 3, scales = "free_y") + # vertical panels for each species distributed in e columns
  scale_fill_viridis_d(option = "D") + # colour palette
  scale_color_viridis_d(option = "D") + 
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +  # set xx axis to start at zero and more space at the top of the yy axis
  labs( x ="Time" , y = "Mortality", color = "Scenarios", fill = "Scenarios") + # axis labels (note: fill and color must have the same name to have only one legend)
  line_plots_theme + 
  theme(legend.position = "bottom")


#Save mortality through time plot
ggsave(plot = mort_plot, file = "Mortality_through_time.tiff",  bg = 'white', width = 250, height = 230, units = "mm", dpi = 1200, compression = "lzw")


## ABUNDANCE MISMATCH BOXPLOTS
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

#Save abundance mismatch per scenario plot
ggsave(plot = abund_mismatch_plot, file = "Abundance_mismatch.tiff",  bg = 'white', width = 170, height = 150, units = "mm", dpi = 1200, compression = "lzw")
#this plot takes a lot more time than the others including saving it to an image

gc()
################################################################################

#Step 4 → Create Cell level plots (per species)

# subset data for the last time step (t = 90)
abund_t90<-all_data[all_data$t == 90,]

#new way of having the spatial maps to have a different fill scale for each species
abundance_change_map <- abund_t90 %>%
  group_by(species) %>% 
  do(gg = {ggplot(., aes(x, -y, fill = abund_change)) + 
      geom_raster() +
      facet_wrap(.~species) + 
      #guides(fill = guide_colourbar(title.position = "bottom")) +
      scale_fill_viridis_c("Abundance \nchange", option = "H", na.value = "transparent") +
      labs(x = "Cell longitude ", y ="Cell latitude ") +
      theme(legend.position = "bottom", panel.background =  element_blank(), # no background grids
            # facets identification customization
            strip.background =  element_rect(fill = "grey94", color = "black"), 
            strip.text = element_text(face = "italic", size = rel(0.8)), 
            panel.border =element_rect(color = "black", fill = NA),
            # axis and legend font type
            legend.title = element_text(face = "bold"), 
            axis.title = element_text(face = "bold"))}) %>% 
  .$gg %>% arrangeGrob(grobs = ., nrow = 4) %>% grid.arrange()

#Save abundance mismatch per scenario plot
ggsave(plot = abundance_change_map, file = "abundance_change_map2.tiff",  bg = 'white', width = 200, height = 270, units = "mm", dpi = 1200, compression = "lzw")

################################################################################
################################################################################
################################################################################

## HABITAT SUITABILITY CHANGE SPATIAL MAP ##
habitat_change_map <- abund_t90 %>%
  group_by(species) %>% 
  do(gg = {ggplot(., aes(x, -y, fill = habitat_change)) + 
      geom_raster() +
      facet_wrap(.~species) + 
      #guides(fill = guide_colourbar(title.position = "bottom")) +
      scale_fill_viridis_c("Abundance \nchange", option = "H", na.value = "transparent") +
      labs(x = "Cell longitude ", y ="Cell latitude ") +
      theme(legend.position = "bottom", panel.background =  element_blank(), # no background grids
            # facets identification customization
            strip.background =  element_rect(fill = "grey94", color = "black"), 
            strip.text = element_text(face = "italic", size = rel(0.8)), 
            panel.border =element_rect(color = "black", fill = NA),
            # axis and legend font type
            legend.title = element_text(face = "bold"), 
            axis.title = element_text(face = "bold"))}) %>% 
  .$gg %>% arrangeGrob(grobs = ., nrow = 4) %>% grid.arrange()

#Save habitat suitability per scenario plot
ggsave(plot = habitat_change_map, file = "habitat_change_map.tiff",  bg = 'white', width = 200, height = 270, units = "mm", dpi = 1200, compression = "lzw")


## CORRELATION PLOTS
# subset of the data for the controlRun for abund_change and habitat_change variables
corr_data_two <- all_data[, c(9,12,15)]

# function to write the model equation and the r2 
lm_labels <- function(df) {
  mod <- lm(abund_change ~ habitat_change, data = df)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1], 2), round(coef(mod)[2], 2)) # write the equation formula
  r2 <- sprintf("italic(R^2) == %.2f", summary(mod)$r.squared) # write th r2 for each group
  data.frame(formula = formula, r2 = r2, stringsAsFactors = FALSE)
}
# SPRINTF function returns a character vector containing a formatted combination of text and variable values

# create dataframe with the equations and r2
labels <- corr_data_two %>%
  group_by(species) %>% # group data by species
  do(lm_labels(.)) # use the function above to create the dataframe with equation and r2 for each species
#labels # visualize lables created


sp_corr_plots_two <- ggplot(corr_data_two, aes(x = habitat_change, y = abund_change)) +
  geom_point(alpha = 0.3) +
  facet_wrap(species ~ .) + # one pannel for each species
  labs(x = "Habitat suitability change", y = "Abundance change") + # label axis
  #geom_smooth(method = "loess", span = 0.1, size = 1) +
  stat_smooth(method = "lm", formula = y~x) + # plot a linear regression model for each species
  geom_text(data = labels, aes(label = formula), x = 0, y = 150000, parse = TRUE, hjust = 0, size = 7/.pt) + # label for the equation
  geom_text(x = 0, y = -350000, aes(label = r2), data = labels, parse = TRUE, hjust = 0, size = 7/.pt) + # label for the r2
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
sp_corr_plots_two

gc()

########################
# SENSITIVITY ANALYSIS #
########################


