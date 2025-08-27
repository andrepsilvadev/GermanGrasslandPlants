
#############################################################
####                                                     ####
####   Script 3 - Modelling the species distributions    #### 
####   Date: August 7th, 2025                            ####
####   Author: Afonso Barrocal                           ####
####                                                     ####
#############################################################

#### Step 0 - Run previous scripts ####

# run library script and load data
source("./1.4.0_libraries.R")
load("./1.4.2_occurrence_processing.RData")
source("./1.4.3_background_data.R")

#### Step 1 - Prepare data and parameters for modelling ####

# create dummy list
models <- list()

# create species vector
species <- unique(ocr$species)

#### Step 2 - Modelling ###

# create new directory
dir.create("./../output/MaxEnt")

# create loop to model species distributions
for(i in seq_along(species)){
  # create new directory
  dir.create(paste0("./../output/MaxEnt/",sub(pattern = " ", replacement = "_", species[i])))
  # setting seed for reproducibility
  set.seed(12251424)
  # create models
  models[[i]] <- MaxEnt(bgSpecies[[i]],
                        p = ocr[ocr$species == species[i],c("x","y")],
                        nbg = 5*nrow(ocr[ocr$species == species[i],c("x","y")]),
                        args = c('jackknife=true',
                                 'writebackgroundpredictions=true',
                                 'responsecurves=true',
                                 'outputformat=logistic',
                                 'replicates=5',
                                 'writeplotdata=true',
                                 'linear=true',
                                 'quadratic=true',
                                 'product=true',
                                 'threshold=false',
                                 'hinge=false',
                                 'betamultiplier=0.05'),
                        silent = TRUE,
                        path = paste0("./../output/MaxEnt/",sub(pattern = " ", replacement = "_", species[i])))
  # remove i-value
  rm(i)
  invisible(gc())
}

# create directory
dir.create(paste0("./../output/MaxEnt"))

# create dummy lists
data <- list()
cv <- list()
tuning <- list()

# create loop to process data
for(i in seq_along(bgSpecies)){
  # create species vector
  species <- unique(ocr$species)
  # subset data
  subSp <- ocr[ocr$species == species[i],c("presence","x","y")]
  # process data (turn into SpatVector)
  subSp <- vect(subSp, crs = "epsg:4326",geom = c("x","y"))
  # create pseudo absences
  subSp <- bm_PseudoAbsences(resp.var = subSp,
                             expl.var = bgSpecies[[i]],
                             nb.absences = 10000,
                             nb.rep = 5,
                             strategy = 'random',
                             seed.val = 20000517)
  # process data (turn into data frame)
  subSp <- as.data.frame(subSp) %>% dplyr::select(xy.x,xy.y,sp)
  # process data (change column names)
  colnames(subSp) <- c("x","y","presence")
  # process data (turn into SpatVector)
  subSp <- vect(subSp, crs = "epsg:4326",geom = c("x","y"))
  # create directory
  dir.create(paste0("./../output/MaxEnt/",sub(" ","_",species[i])))
  # process data (prepare data for modelling)
  data[[i]] <- BIOMOD_FormatingData(resp.var = subSp,
                                    expl.var = bgSpecies[[i]],
                                    resp.name = species[i],
                                    dir.name = paste0("./../output/MaxEnt/",sub(" ","_",species[i])),
                                    seed.val = 20001705)
  # process data (prepare cross-validation)
  cv[[i]] <- bm_CrossValidation(bm.format = data[[i]],
                                strategy = "kfold",
                                nb.rep = 5,
                                k = 5)
  # prepare tuning parameters
  tuning[[i]] <- bm_ModelingOptions(data.type = "binary",
                                    models = c("MAXNET"),
                                    strategy = "default",
                                    user.val = user.val, # Phillips and Dudík (2008)
                                    bm.format = data[[i]])
  # remove unnecessary objects
  rm(species,subSp,MAXENTOptions,user.val,i)
  invisible(gc())
}

# remove unnecessary objects
rm(ocr,bgSpecies)
invisible(gc())

#### Step 2 - Modelling ####

# create dummy list
models <- list()

# create loop to model the species distributions
for(i in seq_along(data)){
  # modelling species distributions
  models[[i]] <- BIOMOD_Modeling(bm.format = data[[i]],
                                 models = c("MAXNET"),
                                 CV.perc = 0,
                                 seed.val = 20000517)
  # remove i-value
  rm(i)
  invisible(gc())
}
