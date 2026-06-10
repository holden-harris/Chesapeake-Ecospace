## -----------------------------------------------------------------------------
## make-driver-pdfs.R   (Stage 3: PDF plots)
##
## Build vector PDF plots of the CBEFS environmental drivers, one set per
## resolution. Reads the stack for each resolution (native stacks for F00, the
## per-basemap regridded stacks for F01..F04) -- regridding already happened in
## regrid-to-basemaps.R, so there is no regrid here. Writes TWO PDFs per
## <var>_<depth> into grid-<label>/PDFs/:
##   - <prefix>_climatology.pdf     12-month climatology, 3x4 panel grid
##   - <prefix>_monthly_by_year.pdf one page per year, 12 month panels per page
##
## Plotting works on the native index-space grid (F00) as well as the basemap
## grids, so F00 gives a full-resolution view for comparison.
##
## Flat top-to-bottom script: edit the settings block below and Source it, or set
## `resolutions` / `variables_to_run` in run-environmental-drivers.R to drive it.
##
## Run order: process-CBEFS.R -> regrid-to-basemaps.R -> THIS
## -----------------------------------------------------------------------------

library(terra)
library(tools)
library(viridisLite)
source("./make-environmental-drivers/cbefs-helpers.R")

## -----------------------------------------------------------------------------
## plot_year_page(): one page of up to 12 month panels for a single year.
##
## Lays layers into fixed Jan..Dec slots (blank cell for any missing month) so
## panel positions align across pages even when a year is partial. Uses a
## per-year color scale (zlim from that year's own months).

plot_year_page <- function(x_grid, dates, yr, plot_cols, units_lab, plot_label) {

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
## Settings  (shared selectors fall back to defaults when run standalone)

if (!exists("resolutions"))      resolutions      <- NULL   ## NULL = all (F00..F04)
if (!exists("variables_to_run")) variables_to_run <- NULL   ## NULL = all <var>_<depth>
if (!exists("out_root"))         out_root         <- CBEFS_OUT_ROOT

n_cols <- 100

init_terra()  ## shared terra temp dir + progress + memfrac (see cbefs-helpers.R)

## PDFs for ALL resolutions, incl. native F00.
res <- resolution_set(which = resolutions)

## -----------------------------------------------------------------------------
## Loop resolutions -> read stacks -> climatology + per-year PDFs

for (r in seq_len(nrow(res))) {

  row    <- res[r, ]
  dir_in <- stack_dir_for(row, out_root)
  if (!dir.exists(dir_in)) {
    stop("Stacks not found for ", row$label, " (", dir_in,
         "). Run process-CBEFS.R (F00) / regrid-to-basemaps.R (basemaps) first.")
  }
  dir_out <- file.path(out_root, row$res_dir, PDF_PRODUCT_SUBDIR)
  dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

  cat("\n============================================================\n")
  log_step(sprintf("PDFs for %s (%s) -> %s", row$label, row$dims, dir_out))

  monthly_files <- filter_monthly_by_prefix(list_monthly_nc(dir_in), variables_to_run)

  for (f in monthly_files) {

    prefix <- prefix_from_monthly(f)

    st         <- get_var_style(prefix)
    plot_label <- st$plot_label; units_lab <- st$units
    pal_name   <- st$palette;    rev_pal   <- st$reverse_palette

    cat("\n------------------------------------------------------------\n")
    log_step(sprintf("PDF for %s", prefix))

    x_grid <- rast(f)                 ## already on this resolution's grid
    dates  <- get_layer_dates(x_grid)
    time(x_grid) <- dates
    x_clim <- climatology_12(x_grid, dates = dates)

    plot_cols <- get_plot_cols(n_cols, pal_name, rev_pal)

    ## --- 12-month climatology PDF (consistent scale across 12 panels) -------
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

    ## --- Per-year multi-page PDF (one page/year, 12 month panels) -----------
    years    <- sort(unique(format(dates, "%Y")))
    pdf_year <- file.path(dir_out, paste0(prefix, "_monthly_by_year.pdf"))
    pdf(pdf_year, width = 11, height = 8.5)
    par(mfrow = c(3, 4), mar = c(2, 2, 3, 4), oma = c(0, 0, 2, 0))

    for (yr in years) {
      plot_year_page(x_grid, dates, yr, plot_cols, units_lab, plot_label)
    }

    dev.off()
    cat("Wrote PDF:", pdf_year, "(", length(years), "pages )\n")

    rm(x_grid, x_clim); gc()
  }
}

cat("\n============================================================\n")
cat("PDF generation complete for:", paste(res$label, collapse = ", "), "\n")
