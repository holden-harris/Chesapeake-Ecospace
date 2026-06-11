## -----------------------------------------------------------------------------
## cbefs-helpers.R
##
## Shared helpers for the CBEFS -> Ecospace environmental-driver workflow.
## Sourced by:
##   - regrid-to-basemaps.R           (Stage 2: native stacks -> per-basemap stacks)
##   - make-ecospace-ascii-drivers.R  (Stage 3: ASCII drivers)
##   - make-driver-pdfs.R             (Stage 3: PDF plots)
##   - make-gif-videos.R              (Stage 3: GIF animations)
##   - run-environmental-drivers.R    (orchestration over the above)
##
## The core idea: CBEFS lives on a 336x564 grid that is regular in an oblique
## stereographic projection but curvilinear in lon/lat. Rather than reconstruct
## that projection (which would require guessing datum + origin), we use the
## exact `longitude`/`latitude` arrays stored in every NetCDF as ground truth,
## and build a single source-cell -> basemap-cell index that is reused for every
## layer, variable, and depth. Source (~600 m) is finer than every basemap, so
## regridding is honest many-to-one mean aggregation.
##
## Pipeline (3 stages):
##   1. process-CBEFS.R     -> native monthly stacks  (var-stack-NC-monthly/)
##   2. regrid-to-basemaps  -> per-basemap regridded stacks, ONCE per basemap
##                             (grid-<label>/var-stack-NC-monthly-regridded/)
##   3. product scripts read the stack for a resolution and write ASCII / PDF /
##      GIF into grid-<label>/...  . The native grid is resolution "F00-336x564"
##      (PDFs + GIFs only; no regrid, no ASCII driver).
## -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(terra)
})

## Native CBEFS grid projection. Used ONLY for optional verification (project a
## corner lon/lat and confirm ~600 m spacing) -- never for the regridding itself.
CBEFS_STERE_CRS <- "+proj=stere +lon_0=283.54 +lat_0=37.75 +units=m"

## -----------------------------------------------------------------------------
## Canonical paths  <-- EDIT HERE to change input/output locations
##
## Single source of truth for the CBEFS-hindcast tree, so every stage (regrid /
## ASCII / PDF / GIF) builds the same grid-<label>/... paths. A constant edited
## here flows to every script -- e.g. rename a product subfolder once below.
## To relocate only the OUTPUT tree for one orchestrated run, set `out_root` in
## run-environmental-drivers.R (it overrides CBEFS_OUT_ROOT per run).

CBEFS_OUT_ROOT          <- "./output-for-ecospace/env-drivers/CBEFS-hindcast"
NATIVE_STACK_SUBDIR     <- "var-stack-NC-monthly"             ## Stage 1, also F00 source
REGRIDDED_STACK_SUBDIR  <- "var-stack-NC-monthly-regridded"   ## Stage 2, per basemap
ASCII_PRODUCT_SUBDIR    <- "ASCII"                            ## ASCII outputs for Ecospace
PDF_PRODUCT_SUBDIR      <- "PDFs"                             ## PDF plots
GIF_PRODUCT_SUBDIR      <- "GIFs"                             ## GIF animations
BASEMAP_DIR_DEFAULT     <- "./output-for-ecospace/habitat/basemaps"
CBEFS_RAW_DIR_DEFAULT   <- "./data-inputs/spatial-dynamic/CBEFS-hindcast"

## -----------------------------------------------------------------------------
## Runtime environment + diagnostics
##
## Shared terra setup (one temp location for the whole module) and lightweight
## progress logging. log_step() flushes stdout so the heartbeat shows up live
## even under Rscript with redirected output (where flush.console() is a no-op).

init_terra <- function(tempdir = "./make-environmental-drivers/terra-temp",
                       progress = 1, memfrac = 0.7) {
  dir.create(tempdir, showWarnings = FALSE, recursive = TRUE)
  terra::terraOptions(tempdir = tempdir, progress = progress, memfrac = memfrac)
  invisible(tempdir)
}

## Free physical RAM in MB (Windows via wmic; NA on other platforms / failure).
free_ram_mb <- function() {
  out <- tryCatch(
    system2("wmic", c("OS", "get", "FreePhysicalMemory", "/value"),
            stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  kb <- suppressWarnings(as.numeric(sub(".*=", "",
          grep("FreePhysicalMemory", out, value = TRUE)[1])))
  if (length(kb) && is.finite(kb)) round(kb / 1024) else NA_real_
}

## Seconds elapsed since a Sys.time() stamp.
elapsed_s <- function(since) as.numeric(difftime(Sys.time(), since, units = "secs"))

## Timestamped, RAM-tagged progress line, flushed for live display.
log_step <- function(...) {
  ram     <- free_ram_mb()
  ram_txt <- if (is.na(ram)) "" else sprintf(" | RAM %d MB", as.integer(ram))
  cat(sprintf("[%s%s] %s\n", format(Sys.time(), "%H:%M:%S"), ram_txt, paste0(...)))
  flush(stdout())
}

## -----------------------------------------------------------------------------
## build_regrid_index()
##
## Build a one-time mapping from CBEFS native grid cells -> Ecospace basemap
## cells. longitude/latitude are read with the SAME terra path as the data
## subdatasets and flipped the SAME way (see flip_vertical), so cell ordering
## matches the stored data stacks automatically -- no orientation guessing.
##
## Args:
##   nc_file       a raw CBEFS yearly NetCDF (any one; all share the grid)
##   basemap       SpatRaster template (88x56) defining the target grid
##   flip_vertical must match the flip applied in process-CBEFS.R (default TRUE)
##
## Returns a list:
##   idx       integer vector length ncell(native); target basemap cell for each
##             source cell, NA where the source cell falls outside the basemap
##   lon, lat  the (normalized) source coordinates, for verification/spot-checks

build_regrid_index <- function(nc_file, basemap, flip_vertical = TRUE) {

  lon_r <- terra::rast(nc_file, subds = "longitude")
  lat_r <- terra::rast(nc_file, subds = "latitude")

  if (flip_vertical) {
    lon_r <- terra::flip(lon_r, direction = "vertical")
    lat_r <- terra::flip(lat_r, direction = "vertical")
  }

  lon <- terra::values(lon_r)[, 1]
  lat <- terra::values(lat_r)[, 1]

  ## degree_east (0..360) -> -180..180 to match the WGS84 basemap extent
  lon[lon > 180] <- lon[lon > 180] - 360

  idx <- terra::cellFromXY(basemap, cbind(lon, lat))

  list(idx = idx, lon = lon, lat = lat)
}

## -----------------------------------------------------------------------------
## regrid_to_basemap()
##
## Many-to-one mean aggregation of a native-grid SpatRaster onto the basemap,
## using the precomputed index. Vectorized across ALL layers via rowsum(), so a
## 480-layer monthly stack regrids in a single pass. NA source values are
## ignored per target cell; target cells with no source coverage become NA.
##
## Args:
##   x_native  SpatRaster on the CBEFS native grid (any number of layers)
##   basemap   SpatRaster template (88x56)
##   ri        list returned by build_regrid_index()
##
## Returns a SpatRaster on the basemap grid (names/time preserved), masked to
## the basemap's non-NA cells.

regrid_to_basemap <- function(x_native, basemap, ri) {

  idx <- ri$idx

  if (terra::ncell(x_native) != length(idx)) {
    stop("regrid_to_basemap(): layer cell count (", terra::ncell(x_native),
         ") != regrid index length (", length(idx), "). The index must be ",
         "built from a file on the same native grid (and same flip_vertical).")
  }

  vals <- terra::values(x_native)            ## ncell_native x nlyr matrix

  ## Keep only source cells that fall inside the basemap
  keep <- !is.na(idx)
  grp  <- idx[keep]
  vals <- vals[keep, , drop = FALSE]

  ## Per-target-cell sums and counts, ignoring NA source values
  na_mask        <- is.na(vals)
  vals0          <- vals
  vals0[na_mask] <- 0

  sum_by <- rowsum(vals0, group = grp, reorder = TRUE)
  cnt_by <- rowsum((!na_mask) + 0, group = grp, reorder = TRUE)

  mean_by <- sum_by / cnt_by
  mean_by[cnt_by == 0] <- NA_real_

  target_cells <- as.integer(rownames(sum_by))

  ## Assemble output on the basemap grid
  out      <- terra::rast(basemap, nlyrs = terra::nlyr(x_native))
  out_vals <- matrix(NA_real_, nrow = terra::ncell(basemap),
                     ncol = terra::nlyr(x_native))
  out_vals[target_cells, ] <- mean_by
  terra::values(out) <- out_vals

  names(out) <- names(x_native)
  tt <- terra::time(x_native)
  if (length(tt) == terra::nlyr(x_native) && !all(is.na(tt))) {
    terra::time(out) <- tt
  }

  terra::mask(out, basemap)
}

## -----------------------------------------------------------------------------
## Monthly-NC file helpers
##
## The monthly-mean stacks produced by process-CBEFS (write_monthly_stack) are
## named "<var>_<depth>_<yr>_<yr>_monthly_mean.nc". These two helpers list them
## (with a clear error pointing at the producer) and recover the "<var>_<depth>"
## prefix -- shared by the ASCII / GIF / PDF scripts.

list_monthly_nc <- function(dir_in, full.names = TRUE) {
  f <- list.files(dir_in, pattern = "\\.nc$", full.names = full.names)
  if (length(f) == 0) {
    stop("No monthly .nc files in ", dir_in,
         ". Run process-CBEFS.R with write_monthly_stack <- TRUE first.")
  }
  f
}

prefix_from_monthly <- function(path) {
  stub <- tools::file_path_sans_ext(basename(path))
  sub("_[0-9]{4}_[0-9]{4}_monthly_mean$", "", stub)
}

## -----------------------------------------------------------------------------
## get_layer_dates()
##
## Recover a Date per layer: prefer native time metadata, else parse from layer
## names (handles 2001-07-15, 2001_07_15, 2001.07.15, and 20010715).

get_layer_dates <- function(x) {

  tvals <- try(terra::time(x), silent = TRUE)

  if (!inherits(tvals, "try-error") &&
      !is.null(tvals) &&
      length(tvals) == terra::nlyr(x)) {

    dates <- as.Date(tvals)
    if (all(!is.na(dates))) return(dates)
  }

  nm       <- names(x)
  date_chr <- rep(NA_character_, length(nm))

  hit_sep <- grepl("(19|20)\\d{2}[-_.](0[1-9]|1[0-2])[-_.]([0-2]\\d|3[01])", nm)
  date_chr[hit_sep] <- sub(
    ".*?((?:19|20)\\d{2})[-_.]((?:0[1-9]|1[0-2]))[-_.]((?:[0-2]\\d|3[01])).*",
    "\\1-\\2-\\3", nm[hit_sep]
  )

  hit_compact <- is.na(date_chr) &
    grepl("(19|20)\\d{2}(0[1-9]|1[0-2])([0-2]\\d|3[01])", nm)
  date_chr[hit_compact] <- sub(
    ".*?((?:19|20)\\d{2})(\\d{2})(\\d{2}).*",
    "\\1-\\2-\\3", nm[hit_compact]
  )

  dates <- as.Date(date_chr)

  if (length(dates) != terra::nlyr(x) || any(is.na(dates))) {
    stop("Could not recover dates for all layers. Use the NC stack (which ",
         "carries a time dimension) or rebuild dates from a known start date.")
  }

  dates
}

## -----------------------------------------------------------------------------
## monthly_mean()
##
## Daily (or sub-monthly) stack -> monthly mean stack. Layer order follows the
## sorted YYYY-MM levels (chronological, since the keys are zero-padded). Sets
## names and a first-of-month time stamp.

monthly_mean <- function(x, dates = NULL) {

  if (is.null(dates)) dates <- get_layer_dates(x)

  month_id <- format(dates, "%Y-%m")
  lev      <- sort(unique(month_id))

  xm <- terra::tapp(x, index = month_id, fun = mean, na.rm = TRUE)

  names(xm)        <- lev
  terra::time(xm)  <- as.Date(paste0(lev, "-01"))
  xm
}

## -----------------------------------------------------------------------------
## climatology_12()
##
## Monthly stack -> 12-layer calendar-month climatology (mean over all years).
## Names become Jan..Dec.

climatology_12 <- function(x_monthly, dates = NULL) {

  if (is.null(dates)) dates <- as.Date(terra::time(x_monthly))
  if (any(is.na(dates))) dates <- get_layer_dates(x_monthly)

  mon <- format(dates, "%m")
  lev <- sort(unique(mon))

  xc <- terra::tapp(x_monthly, index = mon, fun = mean, na.rm = TRUE)

  names(xc) <- month.abb[as.integer(lev)]
  xc
}

## -----------------------------------------------------------------------------
## write_ascii_layers()
##
## Write one ESRI ASCII grid per layer. `fnames` is a character vector of file
## names (no directory), one per layer.
##
## Writes ONLY the .asc grid. terra/GDAL otherwise drop three sidecars next to
## each .asc that Ecospace does not need (it reads only the .asc header):
##   .prj           <- the raster CRS          -> cleared per layer
##   .asc.aux.xml   <- GDAL PAM metadata        -> GDAL_PAM_ENABLED=NO
##   .asc.aux.json  <- terra layer name/time    -> name/time cleared per layer
## The .asc geometry is in degrees regardless of CRS, so clearing CRS is safe
## (the basemap .asc is stored CRS-less too).

write_ascii_layers <- function(x, out_dir, fnames, naflag = -9999) {

  stopifnot(length(fnames) == terra::nlyr(x))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  terra::setGDALconfig("GDAL_PAM_ENABLED=NO")   ## suppress .asc.aux.xml

  out <- file.path(out_dir, fnames)

  for (i in seq_len(terra::nlyr(x))) {
    lyr <- x[[i]]
    terra::crs(lyr)  <- ""        ## no .prj
    names(lyr)       <- ""        ## no .asc.aux.json
    terra::time(lyr) <- NULL      ## "
    terra::writeRaster(
      lyr,
      filename  = out[i],
      filetype  = "AAIGrid",   ## ESRI/Arc-Info ASCII grid (.asc); "ascii" is not a valid driver
      overwrite = TRUE,
      NAflag    = naflag
    )
  }

  out
}

## -----------------------------------------------------------------------------
## get_plot_cols()
##
## Robust palette builder: viridisLite families by name, else hcl.colors().

get_plot_cols <- function(n_cols, pal_name, reverse_palette = FALSE) {

  pal_lower <- tolower(pal_name)

  if (pal_lower %in% c("viridis", "magma", "inferno", "plasma",
                       "cividis", "rocket", "mako", "turbo")) {

    cols <- switch(
      pal_lower,
      "viridis" = viridisLite::viridis(n_cols),
      "magma"   = viridisLite::magma(n_cols),
      "inferno" = viridisLite::inferno(n_cols),
      "plasma"  = viridisLite::plasma(n_cols),
      "cividis" = viridisLite::cividis(n_cols),
      "rocket"  = viridisLite::rocket(n_cols),
      "mako"    = viridisLite::mako(n_cols),
      "turbo"   = viridisLite::turbo(n_cols)
    )

    if (reverse_palette) cols <- rev(cols)

  } else {
    cols <- hcl.colors(n = n_cols, palette = pal_name, rev = reverse_palette)
  }

  cols
}

## -----------------------------------------------------------------------------
## robust_zlim()
##
## Common color-scale limits across ALL layers of x, used so every GIF frame /
## climatology panel shares one scale. Tries setMinMax()/minmax() first, falls
## back to global() if that yields non-finite bounds.

robust_zlim <- function(x) {
  x    <- terra::setMinMax(x)
  zlim <- suppressWarnings(range(terra::minmax(x), na.rm = TRUE))
  if (any(!is.finite(zlim))) {
    zlim <- c(terra::global(x, "min", na.rm = TRUE)[1, 1],
              terra::global(x, "max", na.rm = TRUE)[1, 1])
  }
  if (any(!is.finite(zlim))) {
    stop("robust_zlim(): could not determine a finite plotting range.")
  }
  zlim
}

## -----------------------------------------------------------------------------
## Per-variable plotting style (single source of truth)
##
## Canonical label / units / palette per "<var>_<depth>" prefix, shared by the
## GIF and PDF scripts so the two never drift apart. get_var_style() falls back
## to a viridis default (prefix as label) for any unlisted variable.

cbefs_var_styles <- data.frame(
  prefix          = c("temperature_davg", "salinity_bott",   "NO3_surf",          "diss_o2_bott"),
  plot_label      = c("Avg temperature",  "Bottom salinity", "Surface nitrate",   "Bottom DO"),
  units           = c("degC",             "PSU",             "mmol N/m³",     "mg O₂ L⁻¹"),
  palette         = c("Heat",             "viridis",         "Purples",           "YlGnBu"),
  reverse_palette = c(TRUE,               FALSE,             FALSE,               FALSE),
  stringsAsFactors = FALSE
)

get_var_style <- function(prefix, styles = cbefs_var_styles) {
  hit <- styles[styles$prefix == prefix, ]
  if (nrow(hit) == 1) {
    list(plot_label      = hit$plot_label,
         units           = hit$units,
         palette         = hit$palette,
         reverse_palette = hit$reverse_palette)
  } else {
    list(plot_label = prefix, units = "", palette = "viridis",
         reverse_palette = FALSE)
  }
}


## -----------------------------------------------------------------------------
## Resolution registry
##
## list_basemaps()  -> the real downsampled basemaps (F01..F04), discovered from
##                     base-depth-map-F##-<dims>.asc files. The F## factor prefix
##                     is REQUIRED, which excludes the legacy non-F## copy that
##                     also lives in basemaps/ (avoids a duplicate resolution).
## resolution_set() -> list_basemaps() with the native grid (F00-336x564, no
##                     basemap file) prepended. This is what PDF/GIF iterate;
##                     ASCII + regrid iterate list_basemaps() (native excluded).
##
## Each row: label (e.g. "F02-88x56"), dims ("88x56"), res_dir ("grid-F02-88x56"),
## path (basemap .asc, NA for native), native (logical).

list_basemaps <- function(basemap_dir = BASEMAP_DIR_DEFAULT, which = NULL) {

  files <- list.files(
    basemap_dir,
    pattern    = "^base-depth-map-F[0-9]{2}-.*\\.asc$",
    full.names = TRUE
  )
  if (length(files) == 0) {
    stop("No basemaps matching 'base-depth-map-F##-*.asc' in ", basemap_dir, ".")
  }

  label <- sub("^base-depth-map-", "",
               tools::file_path_sans_ext(basename(files)))  ## "F02-88x56"
  dims  <- sub("^F[0-9]{2}-", "", label)                    ## "88x56"

  out <- data.frame(
    label = label, dims = dims, res_dir = paste0("grid-", label),
    path = files, native = FALSE, stringsAsFactors = FALSE
  )
  out <- out[order(out$label), , drop = FALSE]               ## F01..F04 ascending
  out <- .filter_resolutions(out, which)
  rownames(out) <- NULL
  out
}

resolution_set <- function(which = NULL, basemap_dir = BASEMAP_DIR_DEFAULT) {

  native <- data.frame(
    label = "F00-336x564", dims = "336x564", res_dir = "grid-F00-336x564",
    path = NA_character_, native = TRUE, stringsAsFactors = FALSE
  )
  all <- rbind(native, list_basemaps(basemap_dir = basemap_dir))
  all <- .filter_resolutions(all, which)
  rownames(all) <- NULL
  all
}

## Filter a resolution data frame by a vector of labels or dims (NULL = keep all).
.filter_resolutions <- function(df, which) {
  if (is.null(which)) return(df)
  keep <- df$label %in% which | df$dims %in% which
  if (!any(keep)) {
    stop("No resolutions matched which = ", paste(which, collapse = ", "),
         ". Available: ", paste(df$label, collapse = ", "), ".")
  }
  df[keep, , drop = FALSE]
}

## Input-stack directory for a resolution row: native stacks for F00, else the
## per-basemap regridded stacks. `res_row` is a one-row data frame.
stack_dir_for <- function(res_row, root = CBEFS_OUT_ROOT) {
  if (isTRUE(res_row$native)) {
    file.path(root, NATIVE_STACK_SUBDIR)
  } else {
    file.path(root, res_row$res_dir, REGRIDDED_STACK_SUBDIR)
  }
}

## Load a basemap .asc and assign WGS84 (the .asc stores no CRS).
load_basemap <- function(path) {
  b <- terra::rast(path)
  terra::crs(b) <- "EPSG:4326"
  b
}

## Locate a raw CBEFS yearly NetCDF (any one; all share the lon/lat grid) used to
## build the regrid index.
find_cbefs_raw <- function(cbefs_raw_dir = CBEFS_RAW_DIR_DEFAULT) {
  raw_files <- list.files(
    cbefs_raw_dir,
    pattern    = "^holdenharris_[0-9]{4}_v[0-9]{8}\\.nc$",
    full.names = TRUE
  )
  if (length(raw_files) == 0) {
    stop("No raw CBEFS NetCDF found in ", cbefs_raw_dir,
         " (needed for longitude/latitude to build the regrid index).")
  }
  raw_files[1]
}

## -----------------------------------------------------------------------------
## regrid_stack_file()
##
## Read one native monthly NC, regrid the whole stack onto `basemap` via the
## precomputed index, preserve per-layer dates, and write a NetCDF to nc_out.
## Uses writeCDF() (same writer as process-CBEFS.R) so the time dimension and
## varname round-trip; varname is the "<var>_<depth>" prefix.
regrid_stack_file <- function(nc_in, nc_out, basemap, ri) {
  x_native <- terra::rast(nc_in)
  dates    <- get_layer_dates(x_native)
  x_grid   <- regrid_to_basemap(x_native, basemap, ri)
  terra::time(x_grid) <- dates
  varname  <- prefix_from_monthly(nc_out)
  dir.create(dirname(nc_out), showWarnings = FALSE, recursive = TRUE)
  terra::writeCDF(x_grid, filename = nc_out, overwrite = TRUE,
                  varname = varname, longname = varname)
  invisible(nc_out)
}

## Keep only monthly NC files whose <var>_<depth> prefix is in `prefixes`
## (NULL = keep all).
filter_monthly_by_prefix <- function(files, prefixes = NULL) {
  if (is.null(prefixes)) return(files)
  files[vapply(files, function(f) prefix_from_monthly(f) %in% prefixes, logical(1))]
}
