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

cat("\n## terra-readable subdatasets in example file\n")
#subds_tbl <- discover_subdatasets(example_nc, max_subds = 50)
print(subds_tbl, n = Inf)

cat("\n## Candidate daily environmental subdatasets (nlyr >= 365)\n")
candidate_subds <- subds_tbl %>%
  filter(nlyr >= 365)

print(candidate_subds, n = Inf)

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
