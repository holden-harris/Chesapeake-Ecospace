## -----------------------------------------------------------------------------
## regrid-to-basemaps.R   (Stage 2 of the environmental-driver pipeline)
##
## Take the native CBEFS monthly stacks (336x564, curvilinear in lon/lat) and
## regrid them onto each Ecospace basemap ONCE, caching the result as per-basemap
## monthly NetCDF stacks. The Stage-3 product scripts (ASCII / PDF / GIF) then
## read these pre-regridded stacks instead of each regridding on its own.
##
## CBEFS is curvilinear, so terra::resample() does not apply -- regridding goes
## through the stored longitude/latitude arrays (build_regrid_index +
## regrid_to_basemap in cbefs-helpers.R), a many-to-one mean aggregation. The
## source (~600 m) is finer than every basemap, so averaging is the right op.
##
## Input : <out_root>/var-stack-NC-monthly/<var>_<depth>_..._monthly_mean.nc
## Output: <out_root>/grid-<label>/var-stack-NC-monthly-regridded/<same name>.nc
## The native grid (F00) needs no regrid and is NOT processed here.
##
## Flat top-to-bottom script: edit the settings block below and Source it, or set
## `resolutions` / `variables_to_run` in run-environmental-drivers.R to drive it.
##
## Run order: process-CBEFS.R -> THIS -> make-{ecospace-ascii-drivers,driver-pdfs,gif-videos}.R
## -----------------------------------------------------------------------------

library(terra)
source("./make-environmental-drivers/cbefs-helpers.R")

## -----------------------------------------------------------------------------
## Settings  (shared selectors fall back to defaults when run standalone)

if (!exists("resolutions"))      resolutions      <- NULL   ## NULL = all basemaps (F01..F04)
if (!exists("variables_to_run")) variables_to_run <- NULL   ## NULL = all <var>_<depth>
if (!exists("out_root"))         out_root         <- CBEFS_OUT_ROOT

dir_in        <- file.path(out_root, NATIVE_STACK_SUBDIR)
cbefs_raw_dir <- CBEFS_RAW_DIR_DEFAULT
overwrite_nc  <- TRUE   ## re-regrid even if the output NC already exists

init_terra()  ## shared terra temp dir + progress + memfrac (see cbefs-helpers.R)

## -----------------------------------------------------------------------------
## Resolve inputs (native F00 is the source grid; list_basemaps() excludes it)

basemaps      <- list_basemaps(which = resolutions)
monthly_files <- filter_monthly_by_prefix(list_monthly_nc(dir_in), variables_to_run)
raw_file      <- find_cbefs_raw(cbefs_raw_dir)

cat("\nNative monthly stacks to regrid:\n"); print(basename(monthly_files))
cat("\nTarget basemaps:\n"); print(basemaps[, c("label", "dims")])
cat("\nRegrid index source (lon/lat):", basename(raw_file), "\n")

## -----------------------------------------------------------------------------
## Regrid: one index per basemap, reused for every variable/depth/month

for (b in seq_len(nrow(basemaps))) {

  row     <- basemaps[b, ]
  basemap <- load_basemap(row$path)
  out_dir <- file.path(out_root, row$res_dir, REGRIDDED_STACK_SUBDIR)

  cat("\n============================================================\n")
  log_step(sprintf("Basemap %s (%s) -> %s", row$label, row$dims, out_dir))

  ri    <- build_regrid_index(raw_file, basemap, flip_vertical = TRUE)
  n_in  <- sum(!is.na(ri$idx))
  n_hit <- length(unique(ri$idx[!is.na(ri$idx)]))
  cat("Source cells mapped into basemap:", n_in, "of", length(ri$idx), "\n")
  cat("Basemap cells receiving >=1 source cell:", n_hit, "of",
      terra::ncell(basemap), "\n")

  for (f in monthly_files) {

    t_file <- Sys.time()
    nc_out <- file.path(out_dir, basename(f))

    if (file.exists(nc_out) && !overwrite_nc) {
      cat("  exists, skipping:", basename(nc_out), "\n"); next
    }

    regrid_stack_file(f, nc_out, basemap, ri)
    log_step(sprintf("  %s -> %s in %.1fs", basename(f),
                     row$res_dir, elapsed_s(t_file)))
  }
}

cat("\n============================================================\n")
cat("Regrid complete for basemaps:", paste(basemaps$label, collapse = ", "), "\n")
