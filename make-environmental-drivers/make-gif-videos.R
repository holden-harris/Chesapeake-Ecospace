## -----------------------------------------------------------------------------
## make-gif-videos.R
##
## Build GIF animations of monthly CBEFS fields for selected variables.
##
## Reads the precomputed MONTHLY NetCDF stacks (var-stack-NC-monthly) -- it does
## NOT re-aggregate daily data (that was the old multi-hour bottleneck). Frames
## are rendered on the native model grid, which process-CBEFS.R already keeps
## north-up, so no display flip is needed.
##
## Run order: process-CBEFS.R -> aggregate-daily-to-monthly.R -> THIS
## -----------------------------------------------------------------------------

rm(list = ls())

library(terra)
library(gifski)
library(tools)
library(viridisLite)
source("./make-environmental-drivers/cbefs-helpers.R")

## -----------------------------------------------------------------------------
## User settings

dir_in  <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-NC-monthly"
dir_out <- "./make-environmental-drivers/GIFs"

start_year <- 2021
num_years  <- 4

gif_delay  <- 0.25
gif_width  <- 900
gif_height <- 1400
gif_res    <- 150
n_cols     <- 100

overwrite_gif <- TRUE

terra_tmp <- "./terra-temp"
dir.create(terra_tmp, showWarnings = FALSE, recursive = TRUE)
terraOptions(tempdir = terra_tmp, progress = 1, memfrac = 0.7)

## -----------------------------------------------------------------------------
## Which variables to animate (matched by <var>_<depth> prefix)

#hcl.pals() ## Show available hcl palette names

file_settings <- data.frame(
  prefix          = c("temperature_davg", "NO3_surf",        "diss_o2_bott"),
  plot_label      = c("Avg temperature",  "Surface nitrate", "Bottom DO"),
  units           = c("degC",             "mmol N/m³",   "mg O₂ L⁻¹"),
  palette         = c("Heat",             "Purples",         "YlGnBu"),
  reverse_palette = c(TRUE,               FALSE,             FALSE),
  stringsAsFactors = FALSE
)

dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

end_year   <- start_year + num_years - 1
start_date <- as.Date(paste0(start_year, "-01-01"))
end_date   <- as.Date(paste0(end_year,   "-12-31"))
period_tag <- paste0(start_year, "_", end_year)

cat("\nRequested date range:", format(start_date), "to", format(end_date), "\n")

## -----------------------------------------------------------------------------
## Resolve a monthly NC file from a <var>_<depth> prefix

available <- list.files(dir_in, pattern = "\\.nc$", full.names = TRUE)
if (length(available) == 0) {
  stop("No monthly .nc files in ", dir_in,
       ". Run aggregate-daily-to-monthly.R first.")
}

find_monthly_file <- function(prefix) {
  hit <- available[grepl(paste0("^", prefix, "_"), basename(available))]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

gif_log <- data.frame(
  prefix = character(), gif_file = character(),
  status = character(), n_frames = integer(),
  stringsAsFactors = FALSE
)

## -----------------------------------------------------------------------------
## Build GIFs

for (j in seq_len(nrow(file_settings))) {

  prefix          <- file_settings$prefix[j]
  plot_label      <- file_settings$plot_label[j]
  units_lab       <- file_settings$units[j]
  pal_name        <- file_settings$palette[j]
  reverse_palette <- file_settings$reverse_palette[j]

  file_in <- find_monthly_file(prefix)

  var_dir_out <- file.path(dir_out, prefix)
  dir.create(var_dir_out, showWarnings = FALSE, recursive = TRUE)
  gif_file <- file.path(var_dir_out, paste0(prefix, "_monthly_", period_tag, ".gif"))

  cat("\n------------------------------------------------------------\n")
  cat("GIF for:", prefix, "\n")

  if (is.na(file_in)) {
    cat("No monthly NC found for prefix; skipping.\n")
    gif_log <- rbind(gif_log, data.frame(prefix, gif_file,
      status = "skipped_missing_nc", n_frames = NA_integer_, stringsAsFactors = FALSE))
    next
  }
  if (file.exists(gif_file) && !overwrite_gif) {
    cat("GIF exists; skipping.\n")
    gif_log <- rbind(gif_log, data.frame(prefix, gif_file,
      status = "skipped_existing", n_frames = NA_integer_, stringsAsFactors = FALSE))
    next
  }

  ## --- Read monthly stack and subset to the requested year range ------------
  x_month <- rast(file_in)
  dates   <- get_layer_dates(x_month)

  keep <- which(dates >= start_date & dates <= end_date)
  if (length(keep) == 0) {
    cat("No months in requested range; skipping.\n")
    gif_log <- rbind(gif_log, data.frame(prefix, gif_file,
      status = "skipped_no_months_in_range", n_frames = 0L, stringsAsFactors = FALSE))
    next
  }
  x_month    <- x_month[[keep]]
  frame_dates <- dates[keep]

  ## --- Consistent color scale across frames ---------------------------------
  x_month <- setMinMax(x_month)
  zlim    <- round(range(minmax(x_month), na.rm = TRUE))
  if (any(!is.finite(zlim))) {
    zlim <- c(global(x_month, "min", na.rm = TRUE)[1, 1],
              global(x_month, "max", na.rm = TRUE)[1, 1])
  }
  if (any(!is.finite(zlim)) || zlim[1] == zlim[2]) {
    stop("Could not determine a valid plotting range for: ", prefix)
  }

  plot_cols <- get_plot_cols(n_cols, pal_name, reverse_palette)

  ## --- Render frames --------------------------------------------------------
  frame_dir <- file.path(var_dir_out, "gif-frames")
  if (dir.exists(frame_dir)) unlink(frame_dir, recursive = TRUE, force = TRUE)
  dir.create(frame_dir, showWarnings = FALSE, recursive = TRUE)

  png_files <- character(nlyr(x_month))
  for (i in seq_len(nlyr(x_month))) {
    png_file <- file.path(frame_dir, sprintf("frame_%03d.png", i))
    png(filename = png_file, width = gif_width, height = gif_height, res = gif_res)
    plot(
      x_month[[i]],
      main  = paste0(plot_label, ": ", format(frame_dates[i], "%Y-%m")),
      col   = plot_cols,
      range = zlim,
      plg   = list(title = units_lab),
      mar   = c(3, 3, 3, 6),
      colNA = "gray75"
    )
    dev.off()
    png_files[i] <- png_file
  }

  gifski(
    png_files = png_files,
    gif_file  = gif_file,
    width     = gif_width,
    height    = gif_height,
    delay     = gif_delay,
    loop      = TRUE
  )

  cat("Wrote GIF:", gif_file, "(", length(png_files), "frames )\n")
  gif_log <- rbind(gif_log, data.frame(prefix, gif_file,
    status = "created", n_frames = length(png_files), stringsAsFactors = FALSE))
}

cat("\n============================================================\n")
cat("GIF CREATION SUMMARY:\n")
print(gif_log)
