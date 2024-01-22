
# import package
library("terra")

# import NetCDF file
r <- rast("GCAM_Demeter_LU_ssp1_rcp26_modelmean_2020.nc",
          drivers="NETCDF")

# plotting the first layer of stacked raster
plot(r[[1]])

# create empty raster
p <- rast(ncol = 7200, nrow = 3600,
          xmin = -180, xmax = 180,
          ymin = -90, ymax = 90)

# create empty stacked raster
p <- c(rep(p,33))

# transposing and fliping the raster layer
p[[1]] <- flip(t(r[[1]]), direction = "horizontal");gc()

# plotting the transposed and flipped layer
plot(p[[1]])

# the function for every NetCDF file would be something like this
Nc2Rast <- function(nc) {
  
  # load required package
  require("terra")
  
  # create empty raster
  p <- rast(ncol = 7200, nrow = 3600,
            xmin = -180, xmax = 180,
            ymin = -90, ymax = 90)
  
  # create empty stacked raster
  p <- c(rep(p,33))
  
  # import NetCDF file
  r <- rast(nc,
            drivers="NETCDF")
  
  # looping the tranposing and fliping process
  for(i in 1:length(longnames(r))) {
    
    p[[i]] <- flip(t(r[[i]]), direction = "horizontal");gc()
    
  }
  
  return(p)
  
}