## -----------------------------------------------------------------------------
## CBEFS processing
##
## Purpose: Read yearly CBEFS hindcast NetCDF files (1985–2024), split each
##          variable into 3 depth bands (_bott, _surf, _davg), and write
##          combined multi-year raster stacks for use as Ecospace env drivers.
##
## Input:  ./data-inputs/spatial-dynamic/CBEFS-hindcast/holdenharris_YYYY_v*.nc  (40 files, ~43 GB)
## Output: ./output-for-ecospace/env-drivers/CBEFS-hindcast/
##           var-stack-NC/<var>_<depth>_<yr>_<yr>.nc
##           var-stack-TIFF/<var>_<depth>_<yr>_<yr>.tif
##
## CBEFS reference: Bever et al. 2021, Env. Modelling & Software
##   https://doi.org/10.1016/j.envsoft.2021.105036

rm(list = ls())

library(terra)
library(stringr)
library(dplyr)

terraOptions(progress = 1, memfrac = 0.7)


################################################################################
##
## User options

out_format       <- "BOTH"   ## Options: "TIFF", "NC", "BOTH"
run_mode         <- "TEST"   ## Options: "TEST" (first 4 years), "FULL" (all years)
variables_to_run <- "ALL"    ## Options: "ALL" or specific: c("temperature","salinity")
verbose_mode     <- FALSE    ## TRUE = inspect NetCDF structure before processing

## -----------------------------------------------------------------------------
## Directories

nc_path <- "./data-inputs/spatial-dynamic/CBEFS-hindcast"

tmp_dir_tif <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/tmp-yearly-TIFF"
tmp_dir_nc  <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/tmp-yearly-NC"

out_dir_tif <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-TIFF"
out_dir_nc  <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-NC"

dir.create(tmp_dir_tif, showWarnings = FALSE, recursive = TRUE)
dir.create(tmp_dir_nc,  showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir_tif, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir_nc,  showWarnings = FALSE, recursive = TRUE)


################################################################################
##
## Set up processing

## -----------------------------------------------------------------------------
## Determine variables to process

variables_all <- c(
  "temperature",
  "salinity",
  "diss_o2",
  "phytoplankton",
  "NO3"
)

if (length(variables_to_run) == 1 && variables_to_run == "ALL") {
  variables <- variables_all
  cat("\nVariables selected: ALL\n")
  
} else {
  ## Validate requested variables
  bad_vars <- setdiff(variables_to_run, variables_all)
  
  if (length(bad_vars) > 0) {
    stop(
      "Unknown variable(s): ",
      paste(bad_vars, collapse = ", ")
    )
  }
  variables <- variables_to_run
  cat("\nVariables selected:\n")
  print(variables)
}

## -----------------------------------------------------------------------------
## Set up writing TIFF or NC or BOTH

## Use TIFF yearly chunks for TIFF and BOTH
## Use NC yearly chunks only when final output is NC only
if (out_format %in% c("TIFF", "BOTH")) {
  tmp_ext <- ".tif"
  tmp_dir <- tmp_dir_tif
} else if (out_format == "NC") {
  tmp_ext <- ".nc"
  tmp_dir <- tmp_dir_nc
} else {
  stop("out_format must be one of: 'TIFF', 'NC', 'BOTH'")
}

## -----------------------------------------------------------------------------
## Make file table

nc_files <- list.files(
  path = nc_path,
  pattern = "^holdenharris_[0-9]{4}_v[0-9]{8}\\.nc$",
  full.names = TRUE
)

file_tbl <- tibble(
  file = nc_files,
  file_name = basename(nc_files),
  year = as.integer(str_match(file_name, "holdenharris_([0-9]{4})_v[0-9]{8}\\.nc$")[, 2])
) %>%
  arrange(year)

if (any(is.na(file_tbl$year))) stop("Could not parse years from one or more filenames.")
if (anyDuplicated(file_tbl$year) > 0) stop("Duplicate years detected in the file list.")

expected_years <- 1985:2024
if (!setequal(file_tbl$year, expected_years)) {
  warning("Years present do not exactly match 1985:2024. Check file coverage.")
}

year_min <- min(file_tbl$year)
year_max <- max(file_tbl$year)

## -----------------------------------------------------------------------------
## Optional: inspect NetCDF structure of the first file before processing

if (verbose_mode) {
  library(ncdf4)

  nc_att_value <- function(nc, varid, attname) {
    att <- ncatt_get(nc, varid = varid, attname = attname)
    if (isTRUE(att$hasatt)) as.character(att$value) else NA_character_
  }

  inspect_nc_file <- function(nc_file) {
    nc <- nc_open(nc_file)
    on.exit(nc_close(nc), add = TRUE)

    dim_tbl <- tibble(
      dim_name = names(nc$dim),
      length   = vapply(nc$dim, function(x) x$len, numeric(1)),
      units    = vapply(nc$dim, function(x) {
        if (is.null(x$units) || identical(x$units, "")) NA_character_ else as.character(x$units)
      }, character(1))
    )

    var_tbl <- bind_rows(lapply(names(nc$var), function(vn) {
      vv <- nc$var[[vn]]
      tibble(
        var_name      = vn,
        ndims         = vv$ndims,
        dim_names     = paste(vapply(vv$dim, function(d) d$name, character(1)), collapse = " | "),
        units         = if (is.null(vv$units) || identical(vv$units, "")) NA_character_ else as.character(vv$units),
        long_name     = nc_att_value(nc, vn, "long_name"),
        standard_name = nc_att_value(nc, vn, "standard_name")
      )
    }))

    list(dimensions = dim_tbl, variables = var_tbl)
  }

  cat("\n## Inspecting first NetCDF file:", basename(file_tbl$file[1]), "\n")
  info <- inspect_nc_file(file_tbl$file[1])
  cat("\n## Dimensions:\n"); print(info$dimensions, n = Inf)
  cat("\n## Variables:\n");  print(info$variables,  n = Inf)
}

## -----------------------------------------------------------------------------
## Determine run mode

if (run_mode == "TEST") {
  
  cat("\nRunning in TEST mode (first 4 years only)\n")
  n_years <- min(4, nrow(file_tbl))
  
} else if (run_mode == "FULL") {
  
  cat("\nRunning in FULL mode (all years)\n")
  n_years <- nrow(file_tbl)
  
} else {
  
  stop("run_mode must be 'TEST' or 'FULL'")
}

## -----------------------------------------------------------------------------
## Quiet reader

quiet_rast <- function(file, subds) {
  zz <- file(tempfile())
  sink(zz)
  on.exit({
    sink()
    close(zz)
  }, add = TRUE)
  
  r <- suppressWarnings(suppressMessages(rast(file, subds = subds)))
  return(r)
}

## -----------------------------------------------------------------------------
## Helper writer for yearly chunks

write_yearly_chunk <- function(r, filename, format_out, varname_out, longname_out) {
  
  if (format_out == "TIFF") {
    writeRaster(r, filename, overwrite = TRUE)
  }
  
  if (format_out == "NC") {
    writeCDF(
      r,
      filename = filename,
      overwrite = TRUE,
      varname = varname_out,
      longname = longname_out,
      compression = 4
    )
  }
}

## -----------------------------------------------------------------------------
## Helper writer for final combined stacks

write_final_stack <- function(r, filename, format_out, varname_out, longname_out) {
  
  if (format_out == "TIFF") {
    writeRaster(r, filename, overwrite = TRUE)
  }
  
  if (format_out == "NC") {
    writeCDF(
      r,
      filename = filename,
      overwrite = TRUE,
      varname = varname_out,
      longname = longname_out,
      compression = 4
    )
  }
}


################################################################################
##
## Processing loop
##
## Process one variable at a time
## outer loop = variable
## inner loop = file/year

## -----------------------------------------------------------------------------
## Outer loop - variable

for (v in variables) {
  
  #  v <- variables[1]  ## Test one variable
  
  cat("\n====================================================\n")
  cat("Variable:", v, "\n")
  cat("Run mode:", run_mode, "\n")
  cat("Output format:", out_format, "\n")
  cat("Years processed:", n_years, "\n")
  
  bott_files <- character(n_years)
  surf_files <- character(n_years)
  davg_files <- character(n_years)
  
  ## ---------------------------------------------------------------------------
  ## Inner loop - read each year file
  ## Process each year and write yearly chunks
  
  for (i in seq_len(n_years)) {
    
    this_file <- file_tbl$file[i]
    this_year <- file_tbl$year[i]
    
    cat("  Year:", this_year, "\n")
    
    ## Read variable
    rr <- quiet_rast(this_file, v)
    
    ## Flip vertically
    rr <- flip(rr, direction = "vertical")
    
    ## Check expected structure
    if (nlyr(rr) %% 3 != 0) {
      stop("Variable ", v, " in year ", this_year,
           " does not have a layer count divisible by 3.")
    }
    
    ## Split depths
    bott <- rr[[seq(1, nlyr(rr), by = 3)]]
    surf <- rr[[seq(2, nlyr(rr), by = 3)]]
    davg <- rr[[seq(3, nlyr(rr), by = 3)]]
    
    ## Build dates
    dates_i <- seq.Date(
      from = as.Date(sprintf("%s-01-01", this_year)),
      by = "day",
      length.out = nlyr(bott)
    )
    
    ## Assign names and time
    names(bott) <- paste0(v, "_bott_", format(dates_i, "%Y%m%d"))
    names(surf) <- paste0(v, "_surf_", format(dates_i, "%Y%m%d"))
    names(davg) <- paste0(v, "_davg_", format(dates_i, "%Y%m%d"))
    
    time(bott) <- dates_i
    time(surf) <- dates_i
    time(davg) <- dates_i
    
    ## Yearly output files
    bott_file <- file.path(tmp_dir, paste0(v, "_bott_", this_year, tmp_ext))
    surf_file <- file.path(tmp_dir, paste0(v, "_surf_", this_year, tmp_ext))
    davg_file <- file.path(tmp_dir, paste0(v, "_davg_", this_year, tmp_ext))
    
    ## Write yearly chunks
    write_yearly_chunk(
      r = bott,
      filename = bott_file,
      format_out = ifelse(tmp_ext == ".tif", "TIFF", "NC"),
      varname_out = paste0(v, "_bott"),
      longname_out = paste0(v, " bottom")
    )
    
    write_yearly_chunk(
      r = surf,
      filename = surf_file,
      format_out = ifelse(tmp_ext == ".tif", "TIFF", "NC"),
      varname_out = paste0(v, "_surf"),
      longname_out = paste0(v, " surface")
    )
    
    write_yearly_chunk(
      r = davg,
      filename = davg_file,
      format_out = ifelse(tmp_ext == ".tif", "TIFF", "NC"),
      varname_out = paste0(v, "_davg"),
      longname_out = paste0(v, " depth-average")
    )
    
    bott_files[i] <- bott_file
    surf_files[i] <- surf_file
    davg_files[i] <- davg_file
    
    ## Clean up
    rm(rr, bott, surf, davg)
    gc()
  }
  
  ## ---------------------------------------------------------------------------
  ## Re-open yearly chunks as file-backed stacks and combine
  
  cat("  Combining yearly chunks for", v, "\n")
  
  bott_all <- do.call(c, lapply(bott_files, rast))
  surf_all <- do.call(c, lapply(surf_files, rast))
  davg_all <- do.call(c, lapply(davg_files, rast))
  
  ## ---------------------------------------------------------------------------
  ## Final output files
  
  out_bott_tif <- file.path(out_dir_tif, paste0(v, "_bott_", year_min, "_", year_max, ".tif"))
  out_surf_tif <- file.path(out_dir_tif, paste0(v, "_surf_", year_min, "_", year_max, ".tif"))
  out_davg_tif <- file.path(out_dir_tif, paste0(v, "_davg_", year_min, "_", year_max, ".tif"))
  
  out_bott_nc <- file.path(out_dir_nc, paste0(v, "_bott_", year_min, "_", year_max, ".nc"))
  out_surf_nc <- file.path(out_dir_nc, paste0(v, "_surf_", year_min, "_", year_max, ".nc"))
  out_davg_nc <- file.path(out_dir_nc, paste0(v, "_davg_", year_min, "_", year_max, ".nc"))
  
  ## ---------------------------------------------------------------------------
  ## Write final stacks based on user choice
  
  if (out_format %in% c("TIFF", "BOTH")) {
    
    write_final_stack(
      r = bott_all,
      filename = out_bott_tif,
      format_out = "TIFF",
      varname_out = paste0(v, "_bott"),
      longname_out = paste0(v, " bottom")
    )
    
    write_final_stack(
      r = surf_all,
      filename = out_surf_tif,
      format_out = "TIFF",
      varname_out = paste0(v, "_surf"),
      longname_out = paste0(v, " surface")
    )
    
    write_final_stack(
      r = davg_all,
      filename = out_davg_tif,
      format_out = "TIFF",
      varname_out = paste0(v, "_davg"),
      longname_out = paste0(v, " depth-average")
    )
    
    cat("  Wrote TIFF:\n")
    cat("   ", basename(out_bott_tif), "\n")
    cat("   ", basename(out_surf_tif), "\n")
    cat("   ", basename(out_davg_tif), "\n")
  }
  
  if (out_format %in% c("NC", "BOTH")) {
    
    write_final_stack(
      r = bott_all,
      filename = out_bott_nc,
      format_out = "NC",
      varname_out = paste0(v, "_bott"),
      longname_out = paste0(v, " bottom")
    )
    
    write_final_stack(
      r = surf_all,
      filename = out_surf_nc,
      format_out = "NC",
      varname_out = paste0(v, "_surf"),
      longname_out = paste0(v, " surface")
    )
    
    write_final_stack(
      r = davg_all,
      filename = out_davg_nc,
      format_out = "NC",
      varname_out = paste0(v, "_davg"),
      longname_out = paste0(v, " depth-average")
    )
    
    cat("  Wrote NC:\n")
    cat("   ", basename(out_bott_nc), "\n")
    cat("   ", basename(out_surf_nc), "\n")
    cat("   ", basename(out_davg_nc), "\n")
  }
  
  rm(bott_all, surf_all, davg_all)
  gc()
}

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
