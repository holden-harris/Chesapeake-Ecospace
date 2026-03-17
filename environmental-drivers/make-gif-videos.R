## -----------------------------------------------------------------------------
## Aggregate daily NetCDF stacks to monthly means
## and build GIF animations for selected files
##
## Notes:
## - This ignores CRS / alignment issues for now
## - Assumes the NetCDF files already have a valid time dimension
## - Uses monthly mean for aggregation
## - One row in file_settings = one file to process
## -----------------------------------------------------------------------------

rm(list = ls())

library(terra)
library(gifski)
library(tools)

## -----------------------------------------------------------------------------
## User settings

dir_in  <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-NC"
dir_out <- "./environmental-drivers/GIFs"

start_year <- 2021
num_years  <- 4

gif_delay  <- 0.25
gif_width  <- 1400
gif_height <- 900
gif_res    <- 150

n_cols          <- 100
reverse_palette <- TRUE

## -----------------------------------------------------------------------------
## Optional terra settings

terra_tmp <- "./terra-temp"
dir.create(terra_tmp, showWarnings = FALSE, recursive = TRUE)

terraOptions(
  tempdir  = terra_tmp,
  progress = 1,
  memfrac  = 0.7
)

## -----------------------------------------------------------------------------
## List available NetCDF files in the input folder

available_files <- list.files(
  dir_in,
  pattern    = "\\.nc$",
  full.names = FALSE
)

cat("\nAvailable NetCDF files in folder:\n")
print(available_files)

## -----------------------------------------------------------------------------
## User-controlled file settings
##
## Edit this table to choose which files to run and how to label them
##
## Required columns:
## - file_name
## - plot_label
## - units
## - palette
##
## Example palettes for hcl.colors():
## "viridis", "turbo", "plasma", "inferno", "magma", "cividis", "YlGnBu

file_settings <- data.frame(
  file_name  = c(
    "salinity_bott_1985_2024.nc",
    "temperature_davg_1985_2024.nc",
    "NO3_surf_1985_2024.nc",
    "diss_o2_bott_1985_2024.nc"
  ),
  plot_label = c(
    "Bottom salinity",
    "Avg temperature",
    "Surface nitrate",
    "Bottom DO"
  ),
  units      = c(
    "PPT",
    "degC",
    "mmol N/m³",
    "mg O₂ L⁻¹"
  ),
  palette    = c(
    "viridis",
    "Temps",
    "TealGrn",
    "YlGnBu"
  ),
  stringsAsFactors = FALSE
)

## -----------------------------------------------------------------------------
## Check that all requested files exist in the folder

missing_files <- setdiff(file_settings$file_name, available_files)

if (length(missing_files) > 0) {
  stop(
    "These files were listed in file_settings but not found in dir_in:\n",
    paste(missing_files, collapse = "\n")
  )
}

## -----------------------------------------------------------------------------
## Create output directory

dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

## -----------------------------------------------------------------------------
## Set the date range to keep

end_year <- start_year + num_years - 1

start_date <- as.Date(paste0(start_year, "-01-01"))
end_date   <- as.Date(paste0(end_year,   "-12-31"))

cat("\nRequested date range:\n")
print(start_date)
print(end_date)

## -----------------------------------------------------------------------------
## Initialize a simple run log

run_log <- data.frame(
  file_name       = character(),
  start_date      = character(),
  end_date        = character(),
  n_daily_layers  = integer(),
  n_month_layers  = integer(),
  gif_file        = character(),
  monthly_tif     = character(),
  stringsAsFactors = FALSE
)

## -----------------------------------------------------------------------------
## Loop through the requested files

for (j in seq_len(nrow(file_settings))) {
  
  ## ---------------------------------------------------------------------------
  ## Pull settings for this file
  
  file_name  <- file_settings$file_name[j]
  plot_label <- file_settings$plot_label[j]
  units_lab  <- file_settings$units[j]
  pal_name   <- file_settings$palette[j]
  
  file_in <- file.path(dir_in, file_name)
  
  file_stub  <- file_path_sans_ext(basename(file_name))
  period_tag <- paste0(start_year, "_", end_year)
  
  var_dir_out <- file.path(dir_out, file_stub)
  dir.create(var_dir_out, showWarnings = FALSE, recursive = TRUE)
  
  frame_dir <- file.path(var_dir_out, "gif-frames")
  if (dir.exists(frame_dir)) {
    unlink(frame_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(frame_dir, showWarnings = FALSE, recursive = TRUE)
  
  gif_file <- file.path(
    var_dir_out,
    paste0(file_stub, "_monthly_", period_tag, ".gif")
  )
  
  monthly_tif <- file.path(
    var_dir_out,
    paste0(file_stub, "_monthly_mean_", period_tag, ".tif")
  )
  
  cat("\n============================================================\n")
  cat("Now processing:\n")
  print(file_name)
  cat("Plot label: ", plot_label, "\n", sep = "")
  cat("Units: ", units_lab, "\n", sep = "")
  cat("Palette: ", pal_name, "\n", sep = "")
  
  ## ---------------------------------------------------------------------------
  ## Read the daily raster stack
  
  x_raw <- rast(file_in)
  
  cat("\nInput raster:\n")
  print(x_raw)
  
  ## ---------------------------------------------------------------------------
  ## Extract dates from the time dimension
  
  dates <- as.Date(time(x_raw))
  
  cat("\nFirst few dates:\n")
  print(head(dates))
  
  cat("\nLast few dates:\n")
  print(tail(dates))
  
  if (all(is.na(dates))) {
    stop("No valid time dimension detected in: ", file_name)
  }
  
  ## ---------------------------------------------------------------------------
  ## Subset to the requested date range
  
  keep_idx <- which(dates >= start_date & dates <= end_date)
  
  if (length(keep_idx) == 0) {
    stop("No layers found in requested date range for: ", file_name)
  }
  
  x_test     <- x_raw[[keep_idx]]
  dates_test <- dates[keep_idx]
  
  cat("\nSubset raster:\n")
  print(x_test)
  
  cat("\nDate range in subset raster:\n")
  print(range(dates_test))
  
  ## ---------------------------------------------------------------------------
  ## Create monthly grouping variable
  
  month_id <- format(dates_test, "%Y-%m")
  
  cat("\nNumber of daily layers in subset raster:\n")
  print(length(month_id))
  
  cat("\nUnique months in subset raster:\n")
  print(length(unique(month_id)))
  
  ## ---------------------------------------------------------------------------
  ## Aggregate daily layers to monthly means
  ##
  ## This writes the monthly raster stack directly to disk
  
  x_month <- tapp(
    x_test,
    index     = month_id,
    fun       = mean,
    na.rm     = TRUE,
    filename  = monthly_tif,
    overwrite = TRUE
  )
  
  ## ---------------------------------------------------------------------------
  ## Name the monthly layers
  
  names(x_month) <- unique(month_id)
  
  cat("\nMonthly raster stack:\n")
  print(x_month)
  
  cat("\nMonthly TIFF written to:\n")
  print(monthly_tif)
  
  ## ---------------------------------------------------------------------------
  ## Set a consistent color scale across all monthly frames
  
  mm   <- minmax(x_month)
  zlim <- range(mm, na.rm = TRUE)
  
  cat("\nMonthly value range used for plotting:\n")
  print(zlim)
  
  ## ---------------------------------------------------------------------------
  ## Optional plot check of the first monthly layer
  
  plot(
    x_month[[1]],
    main  = paste0(plot_label, ": ", names(x_month)[1]),
    col   = hcl.colors(n_cols, pal_name),
    range = zlim,
    plg   = list(title = units_lab),
    mar   = c(3, 3, 3, 6),
    colNA = "gray75"
  )
  
  ## ---------------------------------------------------------------------------
  ## Make PNG frames for the GIF
  
  png_files <- character(nlyr(x_month))
  
  for (i in seq_len(nlyr(x_month))) {
    
    layer_i    <- x_month[[i]]
    layer_name <- names(x_month)[i]
    
    png_file <- file.path(frame_dir, sprintf("frame_%03d.png", i))
    
    png(
      filename = png_file,
      width    = gif_width,
      height   = gif_height,
      res      = gif_res
    )
    
    plot(
      layer_i,
      main  = paste0(plot_label, ": ", layer_name),
      col   = hcl.colors(n_cols, pal_name, rev = reverse_palette),
      range = zlim,
      plg   = list(title = units_lab),
      mar   = c(3, 3, 3, 6),
      colNA = "gray75"
    )
    
    dev.off()
    
    png_files[i] <- png_file
  }
  
  cat("\nNumber of PNG frames written:\n")
  print(length(png_files))
  
  ## ---------------------------------------------------------------------------
  ## Build the GIF
  
  gifski(
    png_files = png_files,
    gif_file  = gif_file,
    width     = gif_width,
    height    = gif_height,
    delay     = gif_delay,
    loop      = TRUE
  )
  
  cat("\nGIF written to:\n")
  print(gif_file)
  
  ## ---------------------------------------------------------------------------
  ## Add to run log
  
  run_log <- rbind(
    run_log,
    data.frame(
      file_name      = file_name,
      start_date     = as.character(min(dates_test)),
      end_date       = as.character(max(dates_test)),
      n_daily_layers = nlyr(x_test),
      n_month_layers = nlyr(x_month),
      gif_file       = gif_file,
      monthly_tif    = monthly_tif,
      stringsAsFactors = FALSE
    )
  )
}

## -----------------------------------------------------------------------------
## Final run summary

cat("\n============================================================\n")
cat("Run summary:\n")
print(run_log)

