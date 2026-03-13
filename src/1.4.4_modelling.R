
#############################################################
####                                                     ####
####   Script 3 - Modelling the species distributions    #### 
####   Date: September 9th, 2025                         ####
####   Author: Afonso Barrocal                           ####
####                                                     ####
#############################################################

#### Step 0 - Run previous scripts ####

# run library script and load data
source("./1.4.0_libraries.R")
load("./1.4.2_occurrence_processing.RData")

# create dummy list
bgSpecies <- list()

# import background data
for(i in seq_along(unique(ocr$species))){
  # import background for species "i"
  bgSpecies[[i]] <- rast(paste0("./../data/landuse/species/",sub(pattern = " ",replacement = "_", x = unique(ocr$species)[i]),".tif"))
  # remove i-value
  rm(i)
  invisible(gc())
}

# create directories
dir.create("./../output/Surfaces")

#### Step 1 - Load projection layers ####



#### Step 2 - Modelling and projections ####

# create species vector
sp <- unique(ocr$species)

# create dummy list
myBiomodModelOut <- list()

for(i in seq_along(sp)){
  # start timer
  tic()
  # subset data
  subdata <- ocr[ocr$species == sp[i],]
  # formatting data
  formatted_data <- BIOMOD_FormatingData(
    resp.var = subdata$presence,              # response variable (1 or 0)
    expl.var = bgSpecies[[i]],                # environmental variables
    resp.xy = subdata[, c("x", "y")],         # coordinates
    resp.name = sub(pattern = " ",
                    replacement = ".",
                    x = sp[i]),               # a short name for the species
    dir.name = paste0("./../output/Surfaces/",
                      sub(pattern = " ", 
                          replacement = "_",
                          x = sp[i])),
    PA.nb.rep = 1,                            # number of pseudo-absence runs
    PA.nb.absences = sum(subdata$presence)*5, # number of pseudo-absence points
    PA.strategy = 'random',                   # strategy to generate pseudo-absences
    na.rm = TRUE,
    filter.raster = TRUE,
    seed.val = 123
  )
  # formatting modelling options
  myBiomodOptions <- BIOMOD_ModelingOptions()
  # modelling
  myBiomodModelOut[[i]] <- BIOMOD_Modeling(
    bm.format = formatted_data,              # from BIOMOD_FormatingData()
    modeling.id = "My_Model_ID",
    models = c('ANN', "XGBOOST",
               'RF', "MAXNET"),              # choose from supported algos
    CV.strategy = 'random',
    CV.nb.rep = 3,
    CV.perc = 0.8,                           # use 0–1 format (not 0–100)
    OPT.strategy = 'bigboss',                # built-in tuned model options
    # Alternatively: bm.options = myOptions  # for custom settings via bm_ModelingOptions()
    metric.eval = c('TSS', 'ROC'),
    var.import = 3,
    seed.val = 123                           # for reproducibility
  )
  # make progress bar
  progress(i,5)
  # end timer
  toc()
  # remove unnecessary objects
  rm(subdata,formatted_data,myBiomodOptions,i)
  invisible(gc())
}
