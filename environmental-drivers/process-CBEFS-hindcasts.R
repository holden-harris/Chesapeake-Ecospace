## process-CBEFS-hindcasts.R
## 
## CBEFS info: https://comt.ioos.us/projects/cbefs
## Bever, A.J., M.A.M. Friedrichs, P. St-Laurent, 2021. Real-time environmental forecasts of the 
##      Chesapeake Bay: Model setup, improvements, and online visualization. 
##      Environmental Modelling and Software, 105036, https://doi.org/10.1016/j.envsoft.2021.105036
##
## Purpose: Explore Chesapeake Bay CBEFS netCDF files and build variable-specific
##          multi-year raster stacks for Ecospace environmental drivers
##
## Input:  ./data/raw/CBEFS-hindcast/holdenharris_YYYY_v20260112.nc
## Output: ./output-for-ecospace/env-drivers/CBEFS-hindcast/<varname>_1985_2024.nc
##         ./output-for-ecospace/env-drivers/CBEFS-hindcast/<varname>_1985_2024.rds
##
## Notes:
## - Assumes each yearly .nc file has the same grid and same subdataset ordering
## - Assumes each target variable is stored as its own netCDF subdataset
## - Assumes layers within each yearly file are daily values for that year

## Load packages
rm(list = ls())
library(ncdf4)   ## For netCDF introspection
library(terra)   ## For raster stacks / spatial work
library(dplyr)   ## For convenience
library(stringr) ## For name handling

## Set up directories
nc_path       <- "./data/raw/CBEFS-hindcast"
out_ascii_dir <- "./output-for-ecospace/env-drivers/CBEFS-hindcast"
dir.create(out_ascii_dir, showWarnings = FALSE, recursive = TRUE)

## -----------------------------------------------------------------------------
## Locate yearly netCDF files

nc_files <- list.files( ## Set up file name structure
  path = nc_path,
  pattern = "^holdenharris_[0-9]{4}_v[0-9]{8}\\.nc$",
  full.names = TRUE
)

if (length(nc_files) == 0) {  ## Stop if this doesn't exist
  stop("No netCDF files found in: ", nc_path)
}

file_tbl <- tibble( ## Set up table with file info
  file = nc_files,
  file_name = basename(nc_files),
  year = as.integer(str_match(file_name, "holdenharris_([0-9]{4})_v[0-9]{8}\\.nc$")[, 2])
) %>%
  arrange(year)
if (any(is.na(file_tbl$year))) { ## Flag if empty
  stop("Could not parse years from one or more filenames.")
}
if (anyDuplicated(file_tbl$year) > 0) { ## Flag duplicates
  stop("Duplicate years detected in the file list.")
} else print ("No duplicates detected")

## Check coverage
expected_years <- 1985:2024
if (!setequal(file_tbl$year, expected_years)) {
  warning("Years present do not exactly match 1985:2024. Check file coverage.")
} else print("Years match")
print(file_tbl)

## -----------------------------------------------------------------------------
##
## Helper functions for introspection
nc_att_value <- function(nc, varid, attname) {
  att <- ncatt_get(nc, varid = varid, attname = attname)
  if (isTRUE(att$hasatt)) {
    return(as.character(att$value))
  } else {
    return(NA_character_)
  }
}

inspect_nc_file <- function(nc_file) {
  nc <- nc_open(nc_file)
  on.exit(nc_close(nc), add = TRUE)
  
  dim_tbl <- tibble(
    dim_name = names(nc$dim),
    length = vapply(nc$dim, function(x) x$len, numeric(1)),
    units = vapply(
      nc$dim,
      function(x) {
        if (is.null(x$units) || identical(x$units, "")) NA_character_ else as.character(x$units)
      },
      character(1)
    )
  )
  
  var_tbl <- bind_rows(lapply(names(nc$var), function(vn) {
    vv <- nc$var[[vn]]
    
    tibble(
      var_name = vn,
      ndims = vv$ndims,
      dim_names = paste(vapply(vv$dim, function(d) d$name, character(1)), collapse = " | "),
      units = if (is.null(vv$units) || identical(vv$units, "")) NA_character_ else as.character(vv$units),
      long_name = nc_att_value(nc, vn, "long_name"),
      standard_name = nc_att_value(nc, vn, "standard_name")
    )
  }))
  
  list(
    dimensions = dim_tbl,
    variables = var_tbl
  )
}

## -----------------------------------------------------------------------------
## Helper to discover netCDF subdatasets readable by terra
##
## This is useful because a netCDF file may contain many variables, but only some
## will be exposed as raster-like subdatasets.

discover_subdatasets <- function(nc_file, max_subds = 50) {
  out <- vector("list", max_subds)
  keep <- logical(max_subds)
  
  for (i in seq_len(max_subds)) {
    rr <- try(rast(nc_file, subds = i), silent = TRUE)
    
    if (!inherits(rr, "try-error")) {
      keep[i] <- TRUE
      
      out[[i]] <- tibble(
        subds_id = i,
        varname = paste(unique(varnames(rr)), collapse = "; "),
        longname = paste(unique(longnames(rr)), collapse = "; "),
        nlyr = nlyr(rr),
        nrow = nrow(rr),
        ncol = ncol(rr),
        crs = crs(rr)
      )
    }
  }
  
  bind_rows(out[keep])
}

## -----------------------------------------------------------------------------
## Explore one file first

example_nc <- file_tbl$file[1]
print(example_nc)

example_info <- inspect_nc_file(example_nc)

cat("\n## Dimensions in example file\n")
print(example_info$dimensions, n = Inf)

cat("\n## Variables in example file\n")
print(example_info$variables, n = Inf)

#cat("\n## terra-readable subdatasets in example file\n")
#subds_tbl <- discover_subdatasets(example_nc, max_subds = 50)
#print(subds_tbl, n = Inf)

cat("\n## Candidate daily environmental subdatasets (nlyr >= 365)\n")
candidate_subds <- subds_tbl %>%
  filter(nlyr >= 365)

print(candidate_subds, n = Inf)


## -----------------------------------------------------------------------------
## Build depth-specific stacks for each variable from one file

variables <- c("temperature", "salinity", "diss_o2", "phytoplankton", "NO3")

## Store outputs here
env_stacks <- list()

for (v in variables) {
  
  cat("\n====================================================\n")
  cat("Reading variable:", v, "\n")
  cat("File:", basename(test_file_1), "\n")
  
  ## Read variable
  rr1 <- rast(test_file_1, subds = v)
  
  ## Flip vertically to match geographic orientation
  rr1 <- flip(rr1, direction = "vertical")
  
  ## Quick check
  print(rr1)
  cat("nlyr =", nlyr(rr1), "\n")
  
  ## Split into three depth-specific stacks
  bott <- rr1[[seq(1, nlyr(rr1), by = 3)]]
  surf <- rr1[[seq(2, nlyr(rr1), by = 3)]]
  davg <- rr1[[seq(3, nlyr(rr1), by = 3)]]
  
  ## Rename layers for clarity
  names(bott) <- paste0(v, "_bott_", seq_len(nlyr(bott)))
  names(surf) <- paste0(v, "_surf_", seq_len(nlyr(surf)))
  names(davg) <- paste0(v, "_davg_", seq_len(nlyr(davg)))
  
  ## Store in list
  env_stacks[[paste0(v, "_bott")]] <- bott
  env_stacks[[paste0(v, "_surf")]] <- surf
  env_stacks[[paste0(v, "_davg")]] <- davg
  
  ## Optional: also create standalone objects in workspace
  assign(paste0(v, "_bott"), bott)
  assign(paste0(v, "_surf"), surf)
  assign(paste0(v, "_davg"), davg)
}

## -----------------------------------------------------------------------------
## Check output names

names(env_stacks)

## Example checks
print(env_stacks[["temperature_bott"]])
print(env_stacks[["salinity_surf"]])
print(env_stacks[["diss_o2_davg"]])




































## -----------------------------------------------------------------------------
## Read the first file only

variables <- candidate_subds$varname
print(variables)

subs_id <- variables[3] ## salinity
test_file_1 <- file_tbl$file[1]
test_year_1 <- file_tbl$year[1]

cat("\nReading first file:\n", basename(test_file_1), " - ", subs_id, "\n")
rr1 <- rast(test_file_1, subds = subs_id)


## Note: Ok to ignore Warning message: [rast] unknown calendar (assuming standard): gregorian_proleptic 
## The NetCDF file probably uses a proleptic Gregorian calendar, which is common in ocean models and 
## simply means the modern Gregorian calendar extended backward in time. (docs.unidata.ucar.edu) 
## terra is just warning that it is assuming the normal calendar.
##
## Note: Ok to ignore Warning message: vobjtovarid4: ... dimension named x BUT this dimension HAS NO DIMVAR!
## It means the NetCDF file is not using simple 1-D x/y coordinate variables, so georeferencing is nonstandard.
## The raster currently represents model space, not geographic space. It's a curvilinear grid, common in ocean models.
## The exact longitude and latitude of the 336x564 points is stored inside the variable longitude, latitude.”
## From the metadata:
## 336 × 564 = model grid
## x,y = grid indices
## longitude[y,x] = real coordinate
## latitude[y,x]  = real coordinate

## Flip grid
## Raster plotting systems (including terra, raster, GDAL, etc.) assume that row 1 is the top row of the raster.
rr1 <- flip (rr1, direction = "vertical") 

## Check file
print(rr1); print(varnames(rr1)); print(longnames(rr1))
cat("nlyr =", nlyr(rr1), "\n"); cat("nrow =", nrow(rr1), " ncol =", ncol(rr1), "\n")


## Split into three depth-specific stacks --------------------------------------
bott  <- rr1[[seq(1, nlyr(rr1), by = 3)]]  ## Bottom
surf  <- rr1[[seq(2, nlyr(rr1), by = 3)]]  ## Surface
davg  <- rr1[[seq(3, nlyr(rr1), by = 3)]]  ## Depth-average

print(bott)
print(surf)
print(davg)

max <- max(terra::minmax(rr1))
par(mfrow=c(1,3))
plot(bott[[1]], range = c(0,max), main = "Bottom (Jan 1)")
plot(surf[[1]], range = c(0,max), main = "Surface (Jan 1)")
plot(davg[[1]], range = c(0,max), main = "Average (Jan 1)")
par(mfrow=c(1,3))





























## -----------------------------------------------------------------------------
## Step 2. Check expected number of daily layers in first year

expected_days_1 <- as.integer(
  as.Date(sprintf("%s-01-01", test_year_1 + 1)) -
    as.Date(sprintf("%s-01-01", test_year_1))
)

cat("\nExpected days in", test_year_1, "=", expected_days_1, "\n")
cat("Actual layers =", nlyr(rr1), "\n")

if (nlyr(rr1) != expected_days_1) {
  warning("Layer count does not match expected daily count in first file.")
}

## -----------------------------------------------------------------------------
## Step 3. Build dates for first file and assign them

dates_1 <- seq.Date(
  from = as.Date(sprintf("%s-01-01", test_year_1)),
  by = "day",
  length.out = nlyr(rr1)
)

time(rr1) <- dates_1
names(rr1) <- format(dates_1, "%Y%m%d")

print(time(rr1)[1:5])
print(names(rr1)[1:5])

## -----------------------------------------------------------------------------
## Step 4. Read a second file and compare geometry

test_file_2 <- file_tbl$file[2]
test_year_2 <- file_tbl$year[2]

cat("\nReading second file:\n", basename(test_file_2), "\n")

rr2 <- rast(test_file_2, subds = subds_id)

print(rr2)
cat("nlyr =", nlyr(rr2), "\n")
cat("nrow =", nrow(rr2), " ncol =", ncol(rr2), "\n")
print(ext(rr2))
print(crs(rr2))

geom_ok <- compareGeom(rr1[[1]], rr2[[1]], stopOnError = FALSE)
cat("\nGeometry match between first two years:", geom_ok, "\n")

if (!geom_ok) {
  stop("Geometry mismatch between first two files.")
}

## Assign dates to second file too
dates_2 <- seq.Date(
  from = as.Date(sprintf("%s-01-01", test_year_2)),
  by = "day",
  length.out = nlyr(rr2)
)

time(rr2) <- dates_2
names(rr2) <- format(dates_2, "%Y%m%d")

## -----------------------------------------------------------------------------
## Step 5. Try combining just two years

rr_2yr <- c(rr1, rr2)
time(rr_2yr) <- c(dates_1, dates_2)

print(rr_2yr)
cat("Combined layers =", nlyr(rr_2yr), "\n")

plot(rr_2yr[[1]])
plot(rr_2yr[[nlyr(rr_2yr)]])

## -----------------------------------------------------------------------------
## Step 6. Loop through all years, one by one, with explicit diagnostics

yearly_list <- vector("list", nrow(file_tbl))
date_list   <- vector("list", nrow(file_tbl))

for (j in seq_len(nrow(file_tbl))) {
  
  this_file <- file_tbl$file[j]
  this_year <- file_tbl$year[j]
  
  cat("\n--------------------------------------------------\n")
  cat("j =", j, "\n")
  cat("Year =", this_year, "\n")
  cat("File =", basename(this_file), "\n")
  cat("subds_id =", subds_id, "\n")
  
  rr_j <- try(rast(this_file, subds = subds_id), silent = TRUE)
  
  if (inherits(rr_j, "try-error")) {
    stop("Failed to read file for year ", this_year)
  }
  
  cat("Read successful\n")
  cat("nlyr =", nlyr(rr_j), "\n")
  cat("nrow =", nrow(rr_j), " ncol =", ncol(rr_j), "\n")
  
  geom_ok <- compareGeom(rr1[[1]], rr_j[[1]], stopOnError = FALSE)
  cat("Geometry match to first year =", geom_ok, "\n")
  
  if (!geom_ok) {
    stop("Geometry mismatch in year ", this_year)
  }
  
  expected_days_j <- as.integer(
    as.Date(sprintf("%s-01-01", this_year + 1)) -
      as.Date(sprintf("%s-01-01", this_year))
  )
  
  cat("Expected days =", expected_days_j, "\n")
  cat("Actual layers  =", nlyr(rr_j), "\n")
  
  if (nlyr(rr_j) != expected_days_j) {
    warning("Year ", this_year, " has unexpected layer count.")
  }
  
  dates_j <- seq.Date(
    from = as.Date(sprintf("%s-01-01", this_year)),
    by = "day",
    length.out = nlyr(rr_j)
  )
  
  time(rr_j) <- dates_j
  names(rr_j) <- format(dates_j, "%Y%m%d")
  
  yearly_list[[j]] <- rr_j
  date_list[[j]]   <- dates_j
}

## -----------------------------------------------------------------------------
## Step 7. Combine all years after the loop succeeds

rr_all <- do.call(c, yearly_list)
all_dates <- as.Date(unlist(date_list))
time(rr_all) <- all_dates

print(rr_all)
cat("\nTotal layers =", nlyr(rr_all), "\n")
cat("First date =", as.character(min(all_dates)), "\n")
cat("Last date  =", as.character(max(all_dates)), "\n")

plot(rr_all[[1]])
plot(rr_all[[nlyr(rr_all)]])

## -----------------------------------------------------------------------------
## Step 8. Define output paths

year_min <- min(file_tbl$year)
year_max <- max(file_tbl$year)

out_nc  <- file.path(out_dir, paste0(out_prefix, "_", year_min, "_", year_max, ".nc"))
out_rds <- file.path(out_dir, paste0(out_prefix, "_", year_min, "_", year_max, ".rds"))

cat("\nOutput netCDF:\n", out_nc, "\n")
cat("Output RDS:\n", out_rds, "\n")

## -----------------------------------------------------------------------------
## Step 9. Save RDS first
## This is the safest first export for diagnosis

saveRDS(rr_all, out_rds)

## Confirm reload works
example_stack <- readRDS(out_rds)
print(example_stack)

## -----------------------------------------------------------------------------
## Step 10. Write netCDF last
## Only do this after everything above works

writeCDF(
  rr_all,
  filename = out_nc,
  varname = out_prefix,
  longname = out_prefix,
  overwrite = TRUE,
  compression = 4
)











































################################################################################
##
## Helper functions for building variable-specific multi-year stacks

sanitize_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_+|_+$", "")
}

make_layer_dates <- function(year, n_layers) {
  seq.Date(
    from = as.Date(sprintf("%s-01-01", year)),
    by = "day",
    length.out = n_layers
  )
}

build_variable_stack <- function(subds_id,
                                 nc_tbl,
                                 out_dir,
                                 out_prefix = NULL,
                                 overwrite = FALSE,
                                 write_rds = TRUE,
                                 write_tif = FALSE) {
  
  ## Read first file to define output naming and template geometry
  rr0 <- rast(nc_tbl$file[1], subds = subds_id)
  
  var_name0 <- unique(varnames(rr0))
  long_name0 <- unique(longnames(rr0))
  
  if (is.null(out_prefix)) {
    out_prefix <- sanitize_name(var_name0[1])
    
    if (is.na(out_prefix) || identical(out_prefix, "")) {
      out_prefix <- paste0("subds_", sprintf("%02d", subds_id))
    }
  }
  
  template <- rr0[[1]]
  yearly_list <- vector("list", nrow(nc_tbl))
  time_list <- vector("list", nrow(nc_tbl))
  
  for (i in seq_len(nrow(nc_tbl))) {
    this_file <- nc_tbl$file[i]
    this_year <- nc_tbl$year[i]
    
    message("Reading subdataset ", subds_id, " from ", basename(this_file))
    
    rr_i <- rast(this_file, subds = subds_id)
    
    ## Ensure geometry is identical across all years
    same_geom <- compareGeom(template, rr_i[[1]], stopOnError = FALSE)
    if (!same_geom) {
      stop("Geometry mismatch detected in: ", basename(this_file))
    }
    
    ## Assign daily dates explicitly
    dates_i <- make_layer_dates(this_year, nlyr(rr_i))
    
    ## Check expected number of days
    expected_days <- as.integer(
      as.Date(sprintf("%s-01-01", this_year + 1)) -
        as.Date(sprintf("%s-01-01", this_year))
    )
    
    if (nlyr(rr_i) != expected_days) {
      warning(
        "Year ", this_year, " has ", nlyr(rr_i),
        " layers, but ", expected_days, " expected days."
      )
    }
    
    time(rr_i) <- dates_i
    names(rr_i) <- format(dates_i, "%Y%m%d")
    
    yearly_list[[i]] <- rr_i
    time_list[[i]] <- dates_i
  }
  
  ## Concatenate all years into one time series stack
  rr_all <- do.call(c, yearly_list)
  time(rr_all) <- as.Date(unlist(time_list))
  
  ## Output paths
  year_min <- min(nc_tbl$year)
  year_max <- max(nc_tbl$year)
  
  out_nc <- file.path(out_dir, paste0(out_prefix, "_", year_min, "_", year_max, ".nc"))
  out_rds <- file.path(out_dir, paste0(out_prefix, "_", year_min, "_", year_max, ".rds"))
  out_tif <- file.path(out_dir, paste0(out_prefix, "_", year_min, "_", year_max, ".tif"))
  
  ## Write combined NetCDF
  writeCDF(
    rr_all,
    filename = out_nc,
    varname = out_prefix,
    longname = ifelse(is.na(long_name0[1]), out_prefix, long_name0[1]),
    overwrite = overwrite,
    compression = 4
  )
  
  ## Optional R object save
  if (isTRUE(write_rds)) {
    saveRDS(rr_all, out_rds)
  }
  
  ## Optional GeoTIFF
  ## Note: NetCDF is usually the better format for long daily time series.
  if (isTRUE(write_tif)) {
    writeRaster(rr_all, out_tif, overwrite = overwrite)
  }
  
  return(rr_all)
}

## -----------------------------------------------------------------------------
## After inspecting 'subds_tbl', define the specific subdatasets to process
##
## Replace the example IDs below with the actual subdataset IDs from your files.
## The 'out_prefix' values are the filenames that will be written.

target_subds <- tibble(
  out_prefix = c(
    ## "salinity_surface",
    ## "salinity_bottom",
    ## "salinity_depthavg",
    ## "temperature_surface",
    ## "temperature_bottom",
    ## "temperature_depthavg",
    ## "do_surface",
    ## "do_bottom",
    ## "do_depthavg",
    ## "phytoplankton_surface",
    ## "phytoplankton_bottom",
    ## "phytoplankton_depthavg",
    ## "nitrate_surface",
    ## "nitrate_bottom",
    ## "nitrate_depthavg"
  ),
  subds_id = c(
    ## 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
  )
)

## -----------------------------------------------------------------------------
## Build all requested variable stacks

if (nrow(target_subds) > 0) {
  built_stacks <- vector("list", nrow(target_subds))
  
#  for (i in seq_len(nrow(target_subds))) {
    i = 1
    built_stacks[[i]] <- build_variable_stack(
      subds_id = target_subds$subds_id[i],
      nc_tbl = file_tbl,
      out_dir = out_dir,
      out_prefix = target_subds$out_prefix[i],
      overwrite = TRUE,
      write_rds = TRUE,
      write_tif = FALSE
    )
  }
  
  names(built_stacks) <- target_subds$out_prefix
#}

## -----------------------------------------------------------------------------
## Optional: quickly inspect one finished product

example_stack <- readRDS(file.path(out_dir, "salinity_surface_1985_2024.rds"))
print(example_stack)
plot(example_stack[[1]])
plot(example_stack[[365]])
