## make-env-drivers.R
## Purpose: Explore Chesapeake Bay Atlas netCDF file and build raster stacks
## Input:  ./data/raw/ches-atlas-vims.nc
## Output: ./data/derived/env/<varname>_<depth>.tif (or .rds)

## Load packages
library(ncdf4)   ## For netCDF introspection
library(terra)   ## For raster stacks / spatial work
library(dplyr)   ## For convenience
library(stringr) ## For name handling

## File paths
nc_path   <- "./data/raw/ches-clim-atlas-vims.nc"
out_dir   <- "./data/derived/env"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## -----------------------------------------------------------------------------
## Explore the netCDF structure (variables, dims, metadata)

## --- Open file and list contents ---
nc <- nc_open(nc_path)

## Print high-level summary to console
print(nc)  ## Shows variables, dimensions, attributes

## Check dimensions (lon/lat, maybe time/band)
names(nc$dim)
lon  <- nc$dim$longitude$vals
lat  <- nc$dim$latitude$vals

range(lon); range(lat)
length(lon); length(lat)

## Close connection after exploring
nc_close(nc)

## Extract variable names
var_names <- names(nc$var); var_names

## -----------------------------------------------------------------------------
## Make stacks

## Variables to turn into raster stacks
vars_to_stack <- c(
  "salinity_bottom",
  "salinity_surface",
  "temperature_surface",
  "temperature_bottom",
  "O2_bottom"
)

## Main loop: build stacks and save
env_stacks <- list()

for (vn in vars_to_stack) {
#  vn = vars_to_stack[1]
  message("Processing variable: ", vn)
  
  ## terra::rast will create a SpatRaster with layers = monthly bands (1–12)
  r <- terra::rast(nc_path, sub = vn)
  print(r)## Check geometry
  
  ## Add a monthly time dimension (Jan–Dec climatology)
  months <- seq.Date(
    from = as.Date("2000-01-15"),  ## arbitrary year; just month-of-year
    by   = "1 month",
    length.out = nlyr(r)
  )
  time(r) <- months
  names(r) <- format(months, "%b")   ## Name layers as months
  
  ## Add to stacks
  env_stacks[[vn]] <- r
}

## Plot climatology --------------------------------------------------------

## Directory for figures
fig_dir <- "./environmental-drivers/env_climatology/figs"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

## Loop over variables and make a 12-panel plot for each
for (vn in vars_to_stack) {
#  vn = "salinity_bottom"; print(vn)
  r <- env_stacks[[vn]]
  
  ## --- Save to PNG ---
  png_file <- file.path(fig_dir, paste0(vn, "_climatology.png"))
  png(png_file, width = 1800, height = 1200, res = 150)
  
  par(mfrow = c(3, 4), mar = c(2, 2, 3, 4))
  
  ## Loop through 12 months
  for (i in 1:nlyr(r)) {
    plot(r[[i]], main = names(r)[i])
  }
  
  dev.off()
}



