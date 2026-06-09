## -----------------------------------------------------------------------------
## make-ecospace-ascii-drivers.R
##
## Purpose: Turn the monthly CBEFS NetCDF stacks into Ecospace ASCII drivers on
##          the 88x56 basemap grid. Produces, per <var>_<depth>:
##            - a full monthly time series  (.../ASCII-monthly/<var>_<depth>_YYYY_MM.asc)
##            - a 12-month climatology       (.../ASCII-climatology/<var>_<depth>_<Mon>.asc)
##
## Regridding uses the stored CBEFS longitude/latitude arrays as ground truth
## (no projection reconstruction). The source->basemap cell index is built ONCE
## from a raw CBEFS file and reused for every variable/depth/layer.
##
## Run order: process-CBEFS.R -> THIS
## -----------------------------------------------------------------------------

rm(list = ls())

library(terra)
source("./make-environmental-drivers/cbefs-helpers.R")

init_terra()  ## shared terra temp dir + progress + memfrac (see cbefs-helpers.R)

## -----------------------------------------------------------------------------
## User settings

dir_in   <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-NC-monthly"
dir_base <- "./output-for-ecospace/env-drivers/CBEFS-hindcast"

dir_out_monthly <- file.path(dir_base, "ASCII-monthly")
dir_out_clim    <- file.path(dir_base, "ASCII-climatology")

basemap_path <- "./output-for-ecospace/habitat/base-depth-map-88x56.asc"

## A raw CBEFS yearly file -- only its longitude/latitude arrays are used, to
## build the regrid index. Any year works (all share the grid).
cbefs_raw_dir <- "./data-inputs/spatial-dynamic/CBEFS-hindcast"

write_series      <- TRUE   ## full monthly time series ASCII
write_climatology <- TRUE   ## 12-month climatology ASCII
naflag            <- -9999

## -----------------------------------------------------------------------------
## Basemap + regrid index (built once)

basemap <- rast(basemap_path)
crs(basemap) <- "EPSG:4326"   ## basemap is WGS84 degrees; the .asc stores no CRS

cat("\nBasemap:\n"); print(basemap)

raw_files <- list.files(
  cbefs_raw_dir,
  pattern = "^holdenharris_[0-9]{4}_v[0-9]{8}\\.nc$",
  full.names = TRUE
)
if (length(raw_files) == 0) {
  stop("No raw CBEFS NetCDF found in ", cbefs_raw_dir,
       " (needed for longitude/latitude to build the regrid index).")
}

cat("\nBuilding regrid index from:", basename(raw_files[1]), "\n")
ri <- build_regrid_index(raw_files[1], basemap, flip_vertical = TRUE)

n_in   <- sum(!is.na(ri$idx))
cat("Source cells mapped into basemap:", n_in, "of", length(ri$idx), "\n")
cat("Basemap cells receiving >=1 source cell:",
    length(unique(ri$idx[!is.na(ri$idx)])), "of", ncell(basemap), "\n")

## -----------------------------------------------------------------------------
## Loop monthly NC files -> regrid -> write ASCII

monthly_files <- list_monthly_nc(dir_in)

cat("\nMonthly NC files to process:\n"); print(basename(monthly_files))

for (f in monthly_files) {

  t_file <- Sys.time()
  prefix <- prefix_from_monthly(f)   ## "<var>_<depth>"

  cat("\n------------------------------------------------------------\n")
  log_step(sprintf("Processing %s (%d of %d)", prefix,
                   match(f, monthly_files), length(monthly_files)))

  x_native <- rast(f)
  dates    <- get_layer_dates(x_native)

  ## Regrid all monthly layers to the basemap in one pass
  x_grid <- regrid_to_basemap(x_native, basemap, ri)
  time(x_grid) <- dates

  cat("Regridded stack:", nlyr(x_grid), "layers ->", ncol(basemap), "x",
      nrow(basemap), "\n")

  ## --- Monthly time series ASCII --------------------------------------------
  if (write_series) {
    fnames <- paste0(prefix, "_", format(dates, "%Y_%m"), ".asc")
    write_ascii_layers(x_grid, dir_out_monthly, fnames, naflag = naflag)
    cat("Wrote", nlyr(x_grid), "monthly ASCII to", dir_out_monthly, "\n")
  }

  ## --- 12-month climatology ASCII -------------------------------------------
  if (write_climatology) {
    x_clim <- climatology_12(x_grid, dates = dates)
    fnames <- paste0(prefix, "_", names(x_clim), ".asc")
    write_ascii_layers(x_clim, dir_out_clim, fnames, naflag = naflag)
    cat("Wrote", nlyr(x_clim), "climatology ASCII to", dir_out_clim, "\n")
  }

  log_step(sprintf("Done %s in %.1fs", prefix, elapsed_s(t_file)))
  rm(x_native, x_grid); gc()
}

cat("\n============================================================\n")
cat("ASCII driver generation complete.\n")
if (write_series)      cat("Monthly series:  ", dir_out_monthly, "\n")
if (write_climatology) cat("Climatology:     ", dir_out_clim, "\n")
