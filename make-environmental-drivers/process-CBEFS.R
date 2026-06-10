## -----------------------------------------------------------------------------
## CBEFS processing
##
## Purpose: Read yearly CBEFS hindcast NetCDF files (1985-2024), split each
##          variable into 3 depth bands (_bott, _surf, _davg), and write
##          combined multi-year raster stacks (NetCDF) for use as Ecospace
##          environmental drivers.
##
## Input:  ./data-inputs/spatial-dynamic/CBEFS-hindcast/holdenharris_YYYY_v*.nc  (40 files, ~43 GB)
## Output: ./output-for-ecospace/env-drivers/CBEFS-hindcast/
##           var-stack-NC/<var>_<depth>_<yr>_<yr>.nc              (daily; write_daily_stack)
##           var-stack-TIFF/<var>_<depth>_<yr>_<yr>.tif           (only if out_format incl. TIFF)
##           var-stack-NC-monthly/<var>_<depth>_<yr>_<yr>_monthly_mean.nc  (write_monthly_stack)
##
## Monthly means are computed in-process while each year is in memory (one read
## of each raw file), so no separate daily->monthly re-aggregation step is needed.
## The shared monthly_mean() in cbefs-helpers.R does the per-year aggregation.
##
## Georeferencing note: the output stacks are kept in the native model grid
## (index space). They are NOT given a CRS here -- the curvilinear lon/lat are
## applied later, once, when regridding to the Ecospace basemap (see
## cbefs-helpers.R::build_regrid_index). The vertical flip below keeps the model
## grid north-up; build_regrid_index() applies the SAME flip so the regrid index
## stays aligned with these stacks.
##
## CBEFS reference: Bever et al. 2021, Env. Modelling & Software
##   https://doi.org/10.1016/j.envsoft.2021.105036

rm(list = ls())

library(terra)
library(stringr)
library(dplyr)
source("./make-environmental-drivers/cbefs-helpers.R")  ## monthly_mean(), init_terra(), log_step()

init_terra()  ## shared terra temp dir + progress + memfrac (see cbefs-helpers.R)


################################################################################
##
## User setup

out_format       <- "NC"     ## Options: "NC" (canonical), "TIFF", "BOTH"  (daily stack)
run_mode         <- "FULL"   ## Options: "TEST" (salinity/bott, first 4 years), "FULL" (all)
variables_to_run <- "ALL"    ## Options: "ALL" or specific: c("temperature","salinity")
nc_compression   <- 2        ## NetCDF deflate level 1-9. Lower = faster writes.
verbose_mode     <- FALSE    ## TRUE = inspect NetCDF structure before processing

## Outputs to produce. Monthly means are computed IN-PROCESS from each year's
## data while it is already in memory -- so the monthly stack never requires
## re-reading the large combined daily file (which is chunked for storage, not
## for time-series access, and is very slow to re-read).
##
## SPEED/STORAGE NOTE: the downstream Ecospace pipeline (ASCII/GIF/PDF) reads ONLY
## the monthly stacks. The combined daily stacks are an archive (~47 GB in FULL)
## and are the heaviest write here. If you do not need the daily archive, set
## write_daily_stack <- FALSE to skip the per-year temp chunks and the slow
## deflate-compressed combined write entirely.
write_daily_stack   <- FALSE   ## combined daily NC in var-stack-NC (archive / daily analyses)
write_monthly_stack <- TRUE   ## monthly-mean NC in var-stack-NC-monthly (feeds ASCII/GIF/PDF)

depths_all <- c("bott", "surf", "davg") ## bottom, surface, and depth-averaged

## -----------------------------------------------------------------------------
## Directories

nc_path <- "./data-inputs/spatial-dynamic/CBEFS-hindcast"

#tmp_dir_tif <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/tmp-yearly-TIFF"
tmp_dir_nc  <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/tmp-yearly-NC"

#out_dir_tif <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-TIFF"
out_dir_nc         <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-NC-daily"
out_dir_nc_monthly <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-NC-monthly"

if (write_daily_stack) {
  dir.create(out_dir_nc, showWarnings = FALSE, recursive = TRUE)
  if (out_format %in% c("TIFF", "BOTH")) {
    dir.create(out_dir_tif, showWarnings = FALSE, recursive = TRUE)
  }
}
if (write_monthly_stack) {
  dir.create(out_dir_nc_monthly, showWarnings = FALSE, recursive = TRUE)
}
if (!write_daily_stack && !write_monthly_stack) {
  stop("Nothing to do: set write_daily_stack and/or write_monthly_stack to TRUE.")
}


################################################################################
##
## Set up processing

## -----------------------------------------------------------------------------
## Validate out_format and decide which yearly-chunk format to use.
## Yearly chunks are a memory tactic (write per-year, then re-open as a
## file-backed stack and combine). Use TIFF chunks when TIFF is requested,
## otherwise NC chunks.

if (out_format == "NC") {
  tmp_ext <- ".nc";  tmp_dir <- tmp_dir_nc
} else if (out_format %in% c("TIFF", "BOTH")) {
  tmp_ext <- ".tif"; tmp_dir <- tmp_dir_tif
} else {
  stop("out_format must be one of: 'NC', 'TIFF', 'BOTH'")
}
## Yearly chunks are only needed when building the combined daily stack.
if (write_daily_stack) dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)

## -----------------------------------------------------------------------------
## Determine variables and depths to process

variables_all <- c("temperature", "salinity", "diss_o2", "phytoplankton", "NO3")

if (length(variables_to_run) == 1 && variables_to_run == "ALL") {
  variables <- variables_all
} else {
  bad_vars <- setdiff(variables_to_run, variables_all)
  if (length(bad_vars) > 0) {
    stop("Unknown variable(s): ", paste(bad_vars, collapse = ", "))
  }
  variables <- variables_to_run
}

depths <- depths_all

## -----------------------------------------------------------------------------
## Determine run mode (TEST = salinity / bott / first 4 years only)

if (run_mode == "TEST") {

  variables <- "salinity"
  depths    <- "bott"
  cat("\nRunning in TEST mode (salinity, bottom depth, first 4 years only)\n")

} else if (run_mode == "FULL") {

  cat("\nRunning in FULL mode (all selected variables, all depths, all years)\n")

} else {
  stop("run_mode must be 'TEST' or 'FULL'")
}


## -----------------------------------------------------------------------------
## Make file table

nc_files <- list.files(
  path    = nc_path,
  pattern = "^holdenharris_[0-9]{4}_v[0-9]{8}\\.nc$",
  full.names = TRUE
)

file_tbl <- tibble(
  file      = nc_files,
  file_name = basename(nc_files),
  year      = as.integer(str_match(file_name, "holdenharris_([0-9]{4})_v[0-9]{8}\\.nc$")[, 2])
) %>%
  arrange(year)

if (any(is.na(file_tbl$year)))      stop("Could not parse years from one or more filenames.")
if (anyDuplicated(file_tbl$year) > 0) stop("Duplicate years detected in the file list.")

expected_years <- 1985:2024
if (!setequal(file_tbl$year, expected_years)) {
  warning("Years present do not exactly match 1985:2024. Check file coverage.")
}

n_years <- if (run_mode == "TEST") min(4, nrow(file_tbl)) else nrow(file_tbl)

## Year range actually processed (TEST only does the first n_years) -- used to
## name the output stacks so a 4-year TEST file is not mislabelled 1985_2024.
processed_years <- file_tbl$year[seq_len(n_years)]
year_min <- min(processed_years)
year_max <- max(processed_years)


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
## Quiet reader

quiet_rast <- function(file, subds) {
  zz <- file(tempfile())
  sink(zz)
  on.exit({ sink(); close(zz) }, add = TRUE)
  suppressWarnings(suppressMessages(rast(file, subds = subds)))
}

## -----------------------------------------------------------------------------
## Single stack writer (yearly chunks and final stacks both use this)

write_stack <- function(r, filename, format_out, varname_out, longname_out,
                        compression = nc_compression) {

  if (format_out == "TIFF") {
    writeRaster(r, filename, overwrite = TRUE)
  } else if (format_out == "NC") {
    writeCDF(
      r,
      filename    = filename,
      overwrite   = TRUE,
      varname     = varname_out,
      longname    = longname_out,
      compression = compression
    )
  } else {
    stop("write_stack(): format_out must be 'TIFF' or 'NC'")
  }
}

depth_longname <- c(bott = "bottom", surf = "surface", davg = "depth-average")


################################################################################
##
## Processing loop: outer = variable, inner = year. Process one depth band set
## per variable; only the depths in `depths` are written.

## -----------------------------------------------------------------------------
## Print out update to self

cat("Variables selected:", paste(variables, collapse = ", "), "\n")
cat("Depths selected:   ", paste(depths, collapse = ", "), "\n")
cat("Output format:     ", out_format, "\n")
cat("Years processed:   ", year_min, "-", year_max, "\n")

## Run loop
for (v in variables) {

  cat("\n====================================================\n")
  cat("Variable:", v, "| Years processed:", n_years, "\n")

  ## Track yearly chunk paths per depth (only when building the daily stack)
  chunk_files <- if (write_daily_stack)
    setNames(lapply(depths, function(d) character(n_years)), depths) else NULL

  ## Accumulate per-year monthly means per depth (computed in-process, in memory)
  month_accum <- if (write_monthly_stack)
    setNames(lapply(depths, function(d) vector("list", n_years)), depths) else NULL

  ## Depth bands are interleaved: bott, surf, davg, bott, surf, davg, ...
  band_offset <- c(bott = 1, surf = 2, davg = 3)

  ## ---------------------------------------------------------------------------
  ## Inner loop - read each year, split depths, write daily chunk + month means

  v_start <- Sys.time()   ## for per-variable timing / ETA in the heartbeat below

  for (i in seq_len(n_years)) {

    t_year    <- Sys.time()
    this_file <- file_tbl$file[i]
    this_year <- file_tbl$year[i]

    rr <- quiet_rast(this_file, v)

    ## Keep the model grid north-up. build_regrid_index() applies the same flip.
    rr <- flip(rr, direction = "vertical")

    if (nlyr(rr) %% 3 != 0) {
      stop("Variable ", v, " in year ", this_year,
           " does not have a layer count divisible by 3.")
    }

    n_days  <- nlyr(rr) / 3
    dates_i <- seq.Date(
      from       = as.Date(sprintf("%s-01-01", this_year)),
      by         = "day",
      length.out = n_days
    )

    for (d in depths) {

      rd <- rr[[seq(band_offset[[d]], nlyr(rr), by = 3)]]

      names(rd)       <- paste0(v, "_", d, "_", format(dates_i, "%Y%m%d"))
      terra::time(rd) <- dates_i

      ## (a) Daily yearly chunk (only if building the combined daily stack)
      if (write_daily_stack) {
        chunk_file <- file.path(tmp_dir, paste0(v, "_", d, "_", this_year, tmp_ext))
        write_stack(
          r            = rd,
          filename     = chunk_file,
          format_out   = if (tmp_ext == ".tif") "TIFF" else "NC",
          varname_out  = paste0(v, "_", d),
          longname_out = paste0(v, " ", depth_longname[[d]])
        )
        chunk_files[[d]][i] <- chunk_file
      }

      ## (b) Monthly means for this year/depth -- data is already in memory here,
      ##     so this is cheap and avoids ever re-reading the big daily file. The
      ##     shared monthly_mean() keeps the aggregation logic in one place.
      if (write_monthly_stack) {
        month_accum[[d]][[i]] <- monthly_mean(rd, dates_i)
      }

      rm(rd)
    }

    rm(rr)
    gc()

    ## Heartbeat: confirms the loop is advancing and times each year. Includes a
    ## rough ETA from the running per-year average. log_step() flushes stdout so
    ## the line appears live (not buffered to the end of the run).
    avg_year <- elapsed_s(v_start) / i
    eta_min  <- avg_year * (n_years - i) / 60
    log_step(sprintf(
      "%-13s year %2d/%-2d (%d): %d days x %d depth(s) | %.1fs | ETA %.1f min",
      v, i, n_years, this_year, n_days, length(depths), elapsed_s(t_year), eta_min))
  }

  ## ---------------------------------------------------------------------------
  ## Combine and write final stacks (daily and/or monthly) per depth

  cat("  Writing combined stacks for", v, "\n")

  for (d in depths) {

    varname_out  <- paste0(v, "_", d)
    longname_out <- paste0(v, " ", depth_longname[[d]])
    stem         <- paste0(v, "_", d, "_", year_min, "_", year_max)

    ## Combined daily stack (re-open yearly chunks as a file-backed stack).
    ## This deflate write is the heavy tail of a FULL run; time it so the phase
    ## reports itself instead of looking stalled (set write_daily_stack <- FALSE
    ## to skip the daily archive entirely -- nothing downstream reads it).
    if (write_daily_stack) {
      t_daily <- Sys.time()
      d_all   <- do.call(c, lapply(chunk_files[[d]], rast))

      if (out_format %in% c("NC", "BOTH")) {
        out_nc <- file.path(out_dir_nc, paste0(stem, ".nc"))
        write_stack(d_all, out_nc, "NC", varname_out, longname_out)
        log_step(sprintf("  wrote daily NC   %s (%d layers) in %.1fs",
                         basename(out_nc), nlyr(d_all), elapsed_s(t_daily)))
      }
      if (out_format %in% c("TIFF", "BOTH")) {
        out_tif <- file.path(out_dir_tif, paste0(stem, ".tif"))
        write_stack(d_all, out_tif, "TIFF", varname_out, longname_out)
        log_step(sprintf("  wrote daily TIFF %s in %.1fs",
                         basename(out_tif), elapsed_s(t_daily)))
      }
      rm(d_all)
    }

    ## Combined monthly stack (in-memory per-year means, ~12 layers/year)
    if (write_monthly_stack) {
      t_mon   <- Sys.time()
      m_all   <- do.call(c, month_accum[[d]])
      out_mon <- file.path(out_dir_nc_monthly, paste0(stem, "_monthly_mean.nc"))
      write_stack(m_all, out_mon, "NC", varname_out, longname_out)
      log_step(sprintf("  wrote monthly NC %s (%d layers) in %.1fs",
                       basename(out_mon), nlyr(m_all), elapsed_s(t_mon)))
      rm(m_all)
    }

    gc()
  }

  log_step(sprintf("%-13s complete: %d year(s) in %.1f min",
                   v, n_years, elapsed_s(v_start) / 60))
}

## Note: Ok to ignore Warning: [rast] unknown calendar (assuming standard):
## gregorian_proleptic -- terra just assumes the standard Gregorian calendar.
##
## Note: Ok to ignore Warning: vobjtovarid4: ... dimension named x has no dimvar.
## The CBEFS grid is curvilinear (x,y are grid indices; the real coordinates live
## in the longitude/latitude variables). We keep these stacks in model space on
## purpose and georeference later via the lon/lat arrays. See cbefs-helpers.R.
