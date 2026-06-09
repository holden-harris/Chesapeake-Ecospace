## -----------------------------------------------------------------------------
## cbefs-helpers.R
##
## Shared helpers for the CBEFS -> Ecospace environmental-driver workflow.
## Sourced by:
##   - make-ecospace-ascii-drivers.R
##   - make-gif-videos.R
##   - make-driver-pdfs.R
##
## The core idea: CBEFS lives on a 336x564 grid that is regular in an oblique
## stereographic projection but curvilinear in lon/lat. Rather than reconstruct
## that projection (which would require guessing datum + origin), we use the
## exact `longitude`/`latitude` arrays stored in every NetCDF as ground truth,
## and build a single source-cell -> basemap-cell index that is reused for every
## layer, variable, and depth. Source (~600 m) is ~6x finer than the Ecospace
## grid (~3.5 km), so regridding is honest many-to-one mean aggregation.
## -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(terra)
})

## Native CBEFS grid projection. Used ONLY for optional verification (project a
## corner lon/lat and confirm ~600 m spacing) -- never for the regridding itself.
CBEFS_STERE_CRS <- "+proj=stere +lon_0=283.54 +lat_0=37.75 +units=m"

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

write_ascii_layers <- function(x, out_dir, fnames, naflag = -9999) {

  stopifnot(length(fnames) == terra::nlyr(x))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  out <- file.path(out_dir, fnames)

  for (i in seq_len(terra::nlyr(x))) {
    terra::writeRaster(
      x[[i]],
      filename  = out[i],
      filetype  = "ascii",
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
