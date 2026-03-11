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
## Explore first file

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
## Build multi-year depth-specific stacks
## Workflow:
## outer loop = variable
## inner loop = file/year

variables <- c("temperature", "salinity", "diss_o2", "phytoplankton", "NO3")

## Store final outputs here
env_stacks <- list()

#for (v in variables) {
  v = variables[1]
  
  cat("\n====================================================\n")
  cat("Processing variable:", v, "\n")
  
  ## Initialize yearly lists for this variable
  bott_list <- vector("list", length = nrow(file_tbl))
  surf_list <- vector("list", length = nrow(file_tbl))
  davg_list <- vector("list", length = nrow(file_tbl))
  
  ## Optional: keep track of dates
  bott_dates <- vector("list", length = nrow(file_tbl))
  surf_dates <- vector("list", length = nrow(file_tbl))
  davg_dates <- vector("list", length = nrow(file_tbl))
  
  ## ---------------------------------------------------------------------------
  ## Loop over annual files
  
#  for (i in seq_len(nrow(file_tbl))) { ## Full loop
  for (i in seq(1:4)) {
    
    this_file <- file_tbl$file[i]
    this_year <- file_tbl$year[i]
    
    cat("\n----------------------------------------------------\n")
    cat("Variable:", v, "\n")
    cat("Year:", this_year, "\n")
    cat("File:", basename(this_file), "\n")
    
    ## Read variable from file
    rr <- rast(this_file, subds = v)
    
    ## Flip grid so south/north orientation is correct
    rr <- flip(rr, direction = "vertical")
    
    ## Check total layers
    if (nlyr(rr) %% 3 != 0) {
      stop("Variable ", v, " in year ", this_year,
           " does not have a layer count divisible by 3.")
    }
    
    n_days <- nlyr(rr) / 3
    cat("Total layers =", nlyr(rr), "\n")
    cat("Implied daily steps =", n_days, "\n")
    
    ## Split into three depth-specific stacks
    bott <- rr[[seq(1, nlyr(rr), by = 3)]]
    surf <- rr[[seq(2, nlyr(rr), by = 3)]]
    davg <- rr[[seq(3, nlyr(rr), by = 3)]]
    
    ## Check split worked
    if (!(nlyr(bott) == nlyr(surf) && nlyr(surf) == nlyr(davg))) {
      stop("Depth split failed for variable ", v, " in year ", this_year)
    }
    
    ## Build dates for this year
    dates_i <- seq.Date(
      from = as.Date(sprintf("%s-01-01", this_year)),
      by = "day",
      length.out = nlyr(bott)
    )
    
    ## Optional: check expected day count
    expected_days <- as.integer(
      as.Date(sprintf("%s-01-01", this_year + 1)) -
        as.Date(sprintf("%s-01-01", this_year))
    )
    
    if (nlyr(bott) != expected_days) {
      warning("Variable ", v, " in year ", this_year,
              " has ", nlyr(bott), " daily layers; expected ", expected_days)
    }
    
    ## Assign time
    time(bott) <- dates_i
    time(surf) <- dates_i
    time(davg) <- dates_i
    
    ## Assign names
    names(bott) <- paste0(v, "_bott_", format(dates_i, "%Y%m%d"))
    names(surf) <- paste0(v, "_surf_", format(dates_i, "%Y%m%d"))
    names(davg) <- paste0(v, "_davg_", format(dates_i, "%Y%m%d"))
    
    ## Store yearly stacks
    bott_list[[i]] <- bott
    surf_list[[i]] <- surf
    davg_list[[i]] <- davg
    
    bott_dates[[i]] <- dates_i
    surf_dates[[i]] <- dates_i
    davg_dates[[i]] <- dates_i
    
    cat("Stored yearly stacks for", v, this_year, "\n")
  }
  
  ## ---------------------------------------------------------------------------
  ## Combine all years for this variable
  
  cat("\nCombining all years for variable:", v, "\n")
  
  bott_all <- do.call(c, bott_list)
  surf_all <- do.call(c, surf_list)
  davg_all <- do.call(c, davg_list)
  
  time(bott_all) <- as.Date(unlist(bott_dates))
  time(surf_all) <- as.Date(unlist(surf_dates))
  time(davg_all) <- as.Date(unlist(davg_dates))
  
  ## Store final stacks
  env_stacks[[paste0(v, "_bott")]] <- bott_all
  env_stacks[[paste0(v, "_surf")]] <- surf_all
  env_stacks[[paste0(v, "_davg")]] <- davg_all
  
  ## Optional: also create standalone objects
  assign(paste0(v, "_bott"), bott_all)
  assign(paste0(v, "_surf"), surf_all)
  assign(paste0(v, "_davg"), davg_all)
  
  cat("Created final stacks:\n")
  cat("  ", paste0(v, "_bott"), "\n")
  cat("  ", paste0(v, "_surf"), "\n")
  cat("  ", paste0(v, "_davg"), "\n")
  cat("Final layer count:\n")
  cat("  bott =", nlyr(bott_all), "\n")
  cat("  surf =", nlyr(surf_all), "\n")
  cat("  davg =", nlyr(davg_all), "\n")
#}

## -----------------------------------------------------------------------------
## Check output names

names(env_stacks)

## Example checks
print(env_stacks[["temperature_bott"]])
print(env_stacks[["salinity_surf"]])
print(env_stacks[["diss_o2_davg"]])




























## -----------------------------------------------------------------------------
## Build depth-specific stacks for each variable from one file

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


variables <- c("temperature", "salinity", "diss_o2", "phytoplankton", "NO3")

## Store outputs here
env_stacks <- list()

for (v in variables) {
  
  cat("\n====================================================\n")
  cat("Reading variable:", v, "\n")
  cat("File:", basename(test_file_1), "\n")
  
  rr1 <- rast(test_file_1, subds = v)
  rr1 <- flip(rr1, direction = "vertical")
  
  if (nlyr(rr1) %% 3 != 0) {
    stop("Variable ", v, " does not have a layer count divisible by 3.")
  }
  
  n_days <- nlyr(rr1) / 3
  cat("Total layers =", nlyr(rr1), "\n")
  cat("Implied daily steps =", n_days, "\n")
  
  bott <- rr1[[seq(1, nlyr(rr1), by = 3)]]
  surf <- rr1[[seq(2, nlyr(rr1), by = 3)]]
  davg <- rr1[[seq(3, nlyr(rr1), by = 3)]]
  
  if (!(nlyr(bott) == nlyr(surf) && nlyr(surf) == nlyr(davg))) {
    stop("Depth split failed for variable ", v)
  }
  
  names(bott) <- paste0(v, "_bott_", seq_len(nlyr(bott)))
  names(surf) <- paste0(v, "_surf_", seq_len(nlyr(surf)))
  names(davg) <- paste0(v, "_davg_", seq_len(nlyr(davg)))
  
  env_stacks[[paste0(v, "_bott")]] <- bott
  env_stacks[[paste0(v, "_surf")]] <- surf
  env_stacks[[paste0(v, "_davg")]] <- davg
  
  assign(paste0(v, "_bott"), bott)
  assign(paste0(v, "_surf"), surf)
  assign(paste0(v, "_davg"), davg)
  
  cat("Created:\n")
  cat("  ", paste0(v, "_bott"), "\n")
  cat("  ", paste0(v, "_surf"), "\n")
  cat("  ", paste0(v, "_davg"), "\n")
}

## -----------------------------------------------------------------------------
## Check output names

names(env_stacks)

## Example checks
print(env_stacks[["temperature_bott"]])
print(env_stacks[["salinity_surf"]])
print(env_stacks[["diss_o2_davg"]])