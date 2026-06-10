## -----------------------------------------------------------------------------
## make-ecospace-ascii-drivers.R   (Stage 3: ASCII drivers)
##
## Turn the per-basemap regridded monthly stacks (Stage 2) into Ecospace ASCII
## drivers. Regridding already happened in regrid-to-basemaps.R, so this script
## just reads the regridded stacks and writes ASCII -- no regrid here.
##
## For each basemap resolution and each <var>_<depth>, writes into a per-variable
## folder (write_ascii_layers writes ONLY the .asc grid, no .prj/.aux sidecars):
##   grid-<label>/ST_drivers_ASCII/<var>_<depth>/<var>_<depth>_YYYY_MM.asc   (monthly series)
##   grid-<label>/ST_drivers_ASCII/<var>_<depth>/climatology/<var>_<depth>_<Mon>.asc
##
## ASCII drivers are produced for the real basemaps only (F01..F04); the native
## grid (F00) is not an Ecospace driver. Climatology is reference only (NOT a
## wired-in Ecospace driver).
##
## Flat top-to-bottom script: edit the settings block below and Source it, or set
## `resolutions` / `variables_to_run` in run-environmental-drivers.R to drive it.
##
## Run order: process-CBEFS.R -> regrid-to-basemaps.R -> THIS
## -----------------------------------------------------------------------------

library(terra)
source("./make-environmental-drivers/cbefs-helpers.R")

## -----------------------------------------------------------------------------
## Settings  (shared selectors fall back to defaults when run standalone)

if (!exists("resolutions"))      resolutions      <- NULL   ## NULL = all basemaps (F01..F04)
if (!exists("variables_to_run")) variables_to_run <- NULL   ## NULL = all <var>_<depth>
if (!exists("out_root"))         out_root         <- CBEFS_OUT_ROOT

write_series      <- TRUE    ## full monthly time series ASCII
write_climatology <- TRUE    ## 12-month climatology ASCII (reference only)
naflag            <- -9999

init_terra()  ## shared terra temp dir + progress + memfrac (see cbefs-helpers.R)

## ASCII is an Ecospace driver -> real basemaps only (native F00 is excluded by
## list_basemaps()).
basemaps <- list_basemaps(which = resolutions)

## -----------------------------------------------------------------------------
## Loop basemaps -> read regridded stacks -> write per-var-depth ASCII

for (r in seq_len(nrow(basemaps))) {

  row    <- basemaps[r, ]
  dir_in <- stack_dir_for(row, out_root)
  if (!dir.exists(dir_in)) {
    stop("Regridded stacks not found for ", row$label, " (", dir_in,
         "). Run regrid-to-basemaps.R first.")
  }
  ascii_root <- file.path(out_root, row$res_dir, ASCII_PRODUCT_SUBDIR)

  cat("\n============================================================\n")
  log_step(sprintf("ASCII drivers for %s (%s) -> %s",
                   row$label, row$dims, ascii_root))

  monthly_files <- filter_monthly_by_prefix(list_monthly_nc(dir_in), variables_to_run)

  for (f in monthly_files) {

    t_file <- Sys.time()
    prefix <- prefix_from_monthly(f)   ## "<var>_<depth>"

    cat("\n------------------------------------------------------------\n")
    log_step(sprintf("Processing %s (%d of %d)", prefix,
                     match(f, monthly_files), length(monthly_files)))

    x_grid <- rast(f)                  ## already regridded to this basemap
    dates  <- get_layer_dates(x_grid)

    var_dir <- file.path(ascii_root, prefix)

    ## --- Monthly time series ASCII ------------------------------------------
    if (write_series) {
      fnames <- paste0(prefix, "_", format(dates, "%Y_%m"), ".asc")
      write_ascii_layers(x_grid, var_dir, fnames, naflag = naflag)
      cat("Wrote", nlyr(x_grid), "monthly ASCII to", var_dir, "\n")
    }

    ## --- 12-month climatology ASCII (reference only) ------------------------
    if (write_climatology) {
      x_clim   <- climatology_12(x_grid, dates = dates)
      clim_dir <- file.path(var_dir, "climatology")
      fnames   <- paste0(prefix, "_", names(x_clim), ".asc")
      write_ascii_layers(x_clim, clim_dir, fnames, naflag = naflag)
      cat("Wrote", nlyr(x_clim), "climatology ASCII to", clim_dir, "\n")
    }

    log_step(sprintf("Done %s in %.1fs", prefix, elapsed_s(t_file)))
    rm(x_grid); gc()
  }
}

cat("\n============================================================\n")
cat("ASCII driver generation complete for:",
    paste(basemaps$label, collapse = ", "), "\n")
