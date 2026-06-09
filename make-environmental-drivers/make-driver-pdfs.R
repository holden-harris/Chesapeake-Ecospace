## -----------------------------------------------------------------------------
## make-driver-pdfs.R
##
## Build vector PDF plots of the CBEFS environmental drivers: one PDF per
## <var>_<depth>, showing the 12-month climatology as a 3x4 panel grid.
##
## Reads the precomputed MONTHLY NetCDF stacks (var-stack-NC-monthly) and derives
## the climatology with climatology_12(). Frames use the native model grid, which
## process-CBEFS.R keeps north-up.
##
## Run order: process-CBEFS.R -> aggregate-daily-to-monthly.R -> THIS
## -----------------------------------------------------------------------------

rm(list = ls())

library(terra)
library(tools)
library(viridisLite)
source("./make-environmental-drivers/cbefs-helpers.R")

terra_tmp <- "./terra-temp"
dir.create(terra_tmp, showWarnings = FALSE, recursive = TRUE)
terraOptions(tempdir = terra_tmp, progress = 1, memfrac = 0.7)

## -----------------------------------------------------------------------------
## User settings

dir_in  <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-NC-monthly"
dir_out <- "./make-environmental-drivers/PDFs"
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

n_cols <- 100

## Per-prefix plotting style; unmatched prefixes fall back to default_style.
style_tbl <- data.frame(
  prefix          = c("temperature_davg", "salinity_bott", "NO3_surf",  "diss_o2_bott"),
  plot_label      = c("Avg temperature",  "Bottom salinity", "Surface nitrate", "Bottom DO"),
  units           = c("degC",             "PSU",           "mmol N/m³", "mg O₂ L⁻¹"),
  palette         = c("Heat",             "viridis",       "Purples",   "YlGnBu"),
  reverse_palette = c(TRUE,               FALSE,           FALSE,       FALSE),
  stringsAsFactors = FALSE
)
default_style <- list(plot_label = NULL, units = "", palette = "viridis",
                      reverse_palette = FALSE)

## -----------------------------------------------------------------------------
## Loop monthly NC files

monthly_files <- list.files(dir_in, pattern = "\\.nc$", full.names = TRUE)
if (length(monthly_files) == 0) {
  stop("No monthly .nc files in ", dir_in,
       ". Run aggregate-daily-to-monthly.R first.")
}

cat("\nMonthly NC files to plot:\n"); print(basename(monthly_files))

for (f in monthly_files) {

  stub   <- file_path_sans_ext(basename(f))
  prefix <- sub("_[0-9]{4}_[0-9]{4}_monthly_mean$", "", stub)

  st <- style_tbl[style_tbl$prefix == prefix, ]
  if (nrow(st) == 1) {
    plot_label <- st$plot_label; units_lab <- st$units
    pal_name   <- st$palette;    rev_pal   <- st$reverse_palette
  } else {
    plot_label <- prefix; units_lab <- default_style$units
    pal_name   <- default_style$palette; rev_pal <- default_style$reverse_palette
  }

  cat("\n------------------------------------------------------------\n")
  cat("PDF for:", prefix, "\n")

  x_month <- rast(f)
  dates   <- get_layer_dates(x_month)
  x_clim  <- climatology_12(x_month, dates = dates)

  ## Consistent color scale across the 12 panels
  x_clim <- setMinMax(x_clim)
  zlim   <- range(minmax(x_clim), na.rm = TRUE)
  if (any(!is.finite(zlim))) {
    zlim <- c(global(x_clim, "min", na.rm = TRUE)[1, 1],
              global(x_clim, "max", na.rm = TRUE)[1, 1])
  }

  plot_cols <- get_plot_cols(n_cols, pal_name, rev_pal)

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
}

cat("\n============================================================\n")
cat("PDF generation complete. Output in:", dir_out, "\n")
