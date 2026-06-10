## -----------------------------------------------------------------------------
## make-ecospace-ascii-drivers.R   (Stage 3: ASCII drivers)
##
## Turn the per-basemap regridded monthly stacks (Stage 2) into Ecospace ASCII
## drivers. Regridding already happened in regrid-to-basemaps.R, so this script
## just reads the regridded stacks and writes ASCII -- no regrid here.
##
## For each basemap resolution and each <var>_<depth>, writes into a per-variable
## folder:
##   grid-<label>/ST_drivers_ASCII/<var>_<depth>/<var>_<depth>_YYYY_MM.asc   (monthly series)
##   grid-<label>/ST_drivers_ASCII/<var>_<depth>/climatology/<var>_<depth>_<Mon>.asc
##
## ASCII drivers are produced for the real basemaps only (F01..F04); the native
## grid (F00) is not an Ecospace driver. Climatology is written for reference and
## is NOT currently wired in as an Ecospace driver.
##
## Run order: process-CBEFS.R -> regrid-to-basemaps.R -> THIS
## -----------------------------------------------------------------------------

library(terra)
source("./make-environmental-drivers/cbefs-helpers.R")

## -----------------------------------------------------------------------------
## make_ascii_drivers(): write per-variable-depth ASCII for each basemap.
##
## Args:
##   resolutions       basemap rows to write (default: all real basemaps, F01..F04).
##                     Native F00 is dropped (ASCII is an Ecospace driver).
##   prefixes          optional <var>_<depth> subset (NULL = all variables)
##   write_series      write the full monthly time series ASCII
##   write_climatology write the 12-month climatology ASCII (reference only)
make_ascii_drivers <- function(resolutions       = list_basemaps(),
                               out_root          = CBEFS_OUT_ROOT,
                               prefixes          = NULL,
                               write_series      = TRUE,
                               write_climatology = TRUE,
                               naflag            = -9999) {

  init_terra()  ## shared terra temp dir + progress + memfrac (see cbefs-helpers.R)

  ## ASCII is an Ecospace driver -> basemaps only; drop the native grid if present.
  resolutions <- resolutions[!resolutions$native, , drop = FALSE]
  if (nrow(resolutions) == 0) stop("make_ascii_drivers(): no basemap resolutions.")

  for (r in seq_len(nrow(resolutions))) {

    row    <- resolutions[r, ]
    dir_in <- stack_dir_for(row, out_root)
    if (!dir.exists(dir_in)) {
      stop("Regridded stacks not found for ", row$label, " (", dir_in,
           "). Run regrid-to-basemaps.R first.")
    }
    ascii_root <- file.path(out_root, row$res_dir, ASCII_PRODUCT_SUBDIR)

    cat("\n============================================================\n")
    log_step(sprintf("ASCII drivers for %s (%s) -> %s",
                     row$label, row$dims, ascii_root))

    monthly_files <- filter_monthly_by_prefix(list_monthly_nc(dir_in), prefixes)

    for (f in monthly_files) {

      t_file <- Sys.time()
      prefix <- prefix_from_monthly(f)   ## "<var>_<depth>"

      cat("\n------------------------------------------------------------\n")
      log_step(sprintf("Processing %s (%d of %d)", prefix,
                       match(f, monthly_files), length(monthly_files)))

      x_grid <- rast(f)                  ## already regridded to this basemap
      dates  <- get_layer_dates(x_grid)

      var_dir <- file.path(ascii_root, prefix)

      ## --- Monthly time series ASCII ----------------------------------------
      if (write_series) {
        fnames <- paste0(prefix, "_", format(dates, "%Y_%m"), ".asc")
        write_ascii_layers(x_grid, var_dir, fnames, naflag = naflag)
        cat("Wrote", nlyr(x_grid), "monthly ASCII to", var_dir, "\n")
      }

      ## --- 12-month climatology ASCII (reference only) ----------------------
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
      paste(resolutions$label, collapse = ", "), "\n")
  invisible(out_root)
}

## Run with defaults when invoked standalone (not when sourced by the run script).
if (!orchestrated()) make_ascii_drivers()
