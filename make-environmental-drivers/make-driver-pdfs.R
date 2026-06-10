## -----------------------------------------------------------------------------
## make-driver-pdfs.R
##
## Build vector PDF plots of the CBEFS environmental drivers. The fields are
## regridded onto the 88x56 Ecospace basemap FIRST (same build_regrid_index() +
## regrid_to_basemap() path as make-ecospace-ascii-drivers.R), so every panel is
## on the identical grid/extent that Ecospace actually ingests -- not the native
## 336x564 model grid. Writes TWO PDFs per <var>_<depth>:
##   - <prefix>_climatology.pdf     12-month climatology, 3x4 panel grid
##   - <prefix>_monthly_by_year.pdf one page per year, 12 month panels per page
##
## Reads the precomputed MONTHLY NetCDF stacks (var-stack-NC-monthly) and derives
## the climatology with climatology_12().
##
## Run order: process-CBEFS.R -> THIS
## -----------------------------------------------------------------------------

rm(list = ls())

library(terra)
library(tools)
library(viridisLite)
source("./make-environmental-drivers/cbefs-helpers.R")

init_terra()  ## shared terra temp dir + progress + memfrac (see cbefs-helpers.R)

## -----------------------------------------------------------------------------
## User settings

dir_in  <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-NC-monthly"
dir_out <- "./make-environmental-drivers/PDFs"
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

basemap_path  <- "./output-for-ecospace/habitat/base-depth-map-88x56.asc"

## A raw CBEFS yearly file -- only its longitude/latitude arrays are used, to
## build the regrid index. Any year works (all share the grid).
cbefs_raw_dir <- "./data-inputs/spatial-dynamic/CBEFS-hindcast"

n_cols <- 100

## Per-variable styling (label/units/palette) is shared with the GIF script via
## cbefs-helpers.R::cbefs_var_styles / get_var_style().

## -----------------------------------------------------------------------------
## Basemap + regrid index (built once, reused for every variable/depth)
##
## Same path as make-ecospace-ascii-drivers.R, so the PDF panels land on the
## exact 88x56 WGS84 grid that the ASCII drivers are written on.

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

cat("Source cells mapped into basemap:", sum(!is.na(ri$idx)), "of",
    length(ri$idx), "\n")

## -----------------------------------------------------------------------------
## plot_year_page(): one page of up to 12 month panels for a single year.
##
## Lays layers into fixed Jan..Dec slots (blank cell for any missing month) so
## panel positions align across pages even when a year is partial. Uses a
## per-year color scale (zlim from that year's own months).

plot_year_page <- function(x_grid, dates, yr, plot_cols, units_lab,
                           plot_label) {

  yr_idx <- which(format(dates, "%Y") == yr)
  zlim   <- robust_zlim(x_grid[[yr_idx]])
  yr_mon <- as.integer(format(dates[yr_idx], "%m"))

  for (m in seq_len(12)) {
    hit <- yr_idx[match(m, yr_mon)]   ## first layer for this calendar month, or NA
    if (is.na(hit)) {
      plot.new(); title(main = month.abb[m])
    } else {
      plot(
        x_grid[[hit]],
        main  = month.abb[m],
        col   = plot_cols,
        range = zlim,
        plg   = list(title = units_lab),
        colNA = "gray85"
      )
    }
  }
  mtext(paste0(plot_label, " - ", yr), outer = TRUE, cex = 1.1, font = 2)
}

## -----------------------------------------------------------------------------
## Loop monthly NC files

monthly_files <- list_monthly_nc(dir_in)

cat("\nMonthly NC files to plot:\n"); print(basename(monthly_files))

for (f in monthly_files) {

  prefix <- prefix_from_monthly(f)

  st         <- get_var_style(prefix)
  plot_label <- st$plot_label; units_lab <- st$units
  pal_name   <- st$palette;    rev_pal   <- st$reverse_palette

  cat("\n------------------------------------------------------------\n")
  log_step(sprintf("PDF for %s", prefix))

  x_month <- rast(f)
  dates   <- get_layer_dates(x_month)

  ## Regrid the whole monthly stack onto the 88x56 basemap in one pass, then
  ## derive everything (climatology + per-year pages) from the regridded raster.
  x_grid <- regrid_to_basemap(x_month, basemap, ri)
  time(x_grid) <- dates
  x_clim <- climatology_12(x_grid, dates = dates)

  plot_cols <- get_plot_cols(n_cols, pal_name, rev_pal)

  ## --- 12-month climatology PDF ---------------------------------------------
  ## Consistent color scale across the 12 panels
  zlim <- robust_zlim(x_clim)

  pdf_file <- file.path(dir_out, paste0(prefix, "_climatology.pdf"))
  pdf(pdf_file, width = 11, height = 8.5)
  par(mfrow = c(3, 4), mar = c(2, 2, 3, 4), oma = c(0, 0, 2, 0))

  for (i in seq_len(nlyr(x_clim))) {
    plot(
      x_clim[[i]],
      main  = names(x_clim)[i],
      col   = plot_cols,
      range = zlim,
      plg   = list(title = units_lab),
      colNA = "gray85"
    )
  }
  mtext(paste0(plot_label, " - monthly climatology"),
        outer = TRUE, cex = 1.1, font = 2)

  dev.off()
  cat("Wrote PDF:", pdf_file, "\n")

  ## --- Per-year multi-page PDF (one page/year, 12 month panels) -------------
  years    <- sort(unique(format(dates, "%Y")))
  pdf_year <- file.path(dir_out, paste0(prefix, "_monthly_by_year.pdf"))
  pdf(pdf_year, width = 11, height = 8.5)
  par(mfrow = c(3, 4), mar = c(2, 2, 3, 4), oma = c(0, 0, 2, 0))

  for (yr in years) {
    plot_year_page(x_grid, dates, yr, plot_cols, units_lab, plot_label)
  }

  dev.off()
  cat("Wrote PDF:", pdf_year, "(", length(years), "pages )\n")
}

cat("\n============================================================\n")
cat("PDF generation complete. Output in:", dir_out, "\n")
