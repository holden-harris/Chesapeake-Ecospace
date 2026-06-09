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

n_cols <- 100

## Per-variable styling (label/units/palette) is shared with the GIF script via
## cbefs-helpers.R::cbefs_var_styles / get_var_style().

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
  x_clim  <- climatology_12(x_month, dates = dates)

  ## Consistent color scale across the 12 panels
  zlim <- robust_zlim(x_clim)

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
