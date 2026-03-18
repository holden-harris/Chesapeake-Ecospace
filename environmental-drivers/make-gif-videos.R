## -----------------------------------------------------------------------------
## Aggregate daily NetCDF stacks to monthly means
## and build GIF animations for selected files
##
## Updated workflow:
## 1) First loop = build monthly TIFFs only
##    - skip if monthly TIFF already exists
## 2) Second loop = build GIFs from monthly TIFFs
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
library(viridisLite)

## -----------------------------------------------------------------------------
## User settings

dir_in  <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-NC"
dir_out <- "./environmental-drivers/GIFs"

start_year <- 2021
num_years  <- 4

gif_delay  <- 0.25
gif_width  <- 900
gif_height <- 1400
gif_res    <- 150

n_cols     <- 100

overwrite_monthly <- FALSE
overwrite_gif     <- TRUE

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

hcl.pals() ## Plotting color options

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
    "Heat",
    "TealGrn",
    "YlGnBu"
  ),
  reverse_palette = c(
    FALSE,  ## salinity
    TRUE,  ## temperature
    FALSE,  ## nitrate
    FALSE   ## DO
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
## Helper: make palette colors robustly
##
## This allows:
## - viridis
## - magma
## - inferno
## - plasma
## - cividis
## - rocket
## - mako
## - turbo
##
## And otherwise falls back to hcl.colors()

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
    
    if (reverse_palette) {
      cols <- rev(cols)
    }
    
  } else {
    cols <- hcl.colors(
      n = n_cols,
      palette = pal_name,
      rev = reverse_palette
    )
  }
  
  return(cols)
}

## -----------------------------------------------------------------------------
## Initialize run logs

monthly_log <- data.frame(
  file_name         = character(),
  monthly_tif       = character(),
  aggregation_step  = character(),
  start_date        = character(),
  end_date          = character(),
  n_daily_layers    = integer(),
  n_month_layers    = integer(),
  stringsAsFactors  = FALSE
)

gif_log <- data.frame(
  file_name        = character(),
  monthly_tif      = character(),
  gif_file         = character(),
  gif_step         = character(),
  n_month_layers   = integer(),
  stringsAsFactors = FALSE
)

## -----------------------------------------------------------------------------
## LOOP 1: Build monthly TIFFs only

cat("\n============================================================\n")
cat("START LOOP 1: MONTHLY AGGREGATION\n")

for (j in seq_len(nrow(file_settings))) {
  
  file_name  <- file_settings$file_name[j]
  plot_label <- file_settings$plot_label[j]
  units_lab  <- file_settings$units[j]
  pal_name   <- file_settings$palette[j]
  
  file_in <- file.path(dir_in, file_name)
  
  file_stub  <- file_path_sans_ext(basename(file_name))
  period_tag <- paste0(start_year, "_", end_year)
  
  var_dir_out <- file.path(dir_out, file_stub)
  dir.create(var_dir_out, showWarnings = FALSE, recursive = TRUE)
  
  monthly_tif <- file.path(
    var_dir_out,
    paste0(file_stub, "_monthly_mean_", period_tag, ".tif")
  )
  
  cat("\n------------------------------------------------------------\n")
  cat("Monthly aggregation check for:\n")
  print(file_name)
  cat("Output monthly TIFF:\n")
  print(monthly_tif)
  
  ## ---------------------------------------------------------------------------
  ## Skip if monthly TIFF already exists
  
  if (file.exists(monthly_tif) && !overwrite_monthly) {
    
    cat("Monthly TIFF already exists. Skipping aggregation.\n")
    
    x_month_existing <- rast(monthly_tif)
    
    monthly_log <- rbind(
      monthly_log,
      data.frame(
        file_name        = file_name,
        monthly_tif      = monthly_tif,
        aggregation_step = "skipped_existing",
        start_date       = NA_character_,
        end_date         = NA_character_,
        n_daily_layers   = NA_integer_,
        n_month_layers   = nlyr(x_month_existing),
        stringsAsFactors = FALSE
      )
    )
    
    next
  }
  
  ## ---------------------------------------------------------------------------
  ## Read the daily raster stack
  
  x_raw <- rast(file_in)
  
  cat("\nInput raster:\n")
  print(x_raw)
  
  ## ---------------------------------------------------------------------------
  ## Extract dates from the time dimension
  
  dates <- as.Date(time(x_raw))
  
  if (all(is.na(dates))) {
    stop("No valid time dimension detected in: ", file_name)
  }
  
  cat("\nFirst few dates:\n")
  print(head(dates))
  
  cat("\nLast few dates:\n")
  print(tail(dates))
  
  ## ---------------------------------------------------------------------------
  ## Subset to requested date range
  
  keep_idx <- which(dates >= start_date & dates <= end_date)
  
  if (length(keep_idx) == 0) {
    stop("No layers found in requested date range for: ", file_name)
  }
  
  x_sub     <- x_raw[[keep_idx]]
  dates_sub <- dates[keep_idx]
  
  cat("\nSubset raster:\n")
  print(x_sub)
  
  cat("\nDate range in subset raster:\n")
  print(range(dates_sub))
  
  ## ---------------------------------------------------------------------------
  ## Create monthly grouping variable
  
  month_id <- format(dates_sub, "%Y-%m")
  
  cat("\nNumber of daily layers in subset raster:\n")
  print(length(month_id))
  
  cat("\nUnique months in subset raster:\n")
  print(length(unique(month_id)))
  
  ## ---------------------------------------------------------------------------
  ## Aggregate to monthly means and write directly to disk
  
  x_month <- tapp(
    x_sub,
    index     = month_id,
    fun       = mean,
    na.rm     = TRUE,
    filename  = monthly_tif,
    overwrite = TRUE
  )
  
  names(x_month) <- unique(month_id)
  
  cat("\nMonthly raster stack:\n")
  print(x_month)
  
  cat("\nMonthly TIFF written to:\n")
  print(monthly_tif)
  
  monthly_log <- rbind(
    monthly_log,
    data.frame(
      file_name        = file_name,
      monthly_tif      = monthly_tif,
      aggregation_step = "created",
      start_date       = as.character(min(dates_sub)),
      end_date         = as.character(max(dates_sub)),
      n_daily_layers   = nlyr(x_sub),
      n_month_layers   = nlyr(x_month),
      stringsAsFactors = FALSE
    )
  )
}

## -----------------------------------------------------------------------------
## LOOP 2: Build GIFs from monthly TIFFs

cat("\n============================================================\n")
cat("START LOOP 2: GIF CREATION\n")

for (j in seq_len(nrow(file_settings))) {
  
  file_name  <- file_settings$file_name[j]
  plot_label <- file_settings$plot_label[j]
  units_lab  <- file_settings$units[j]
  pal_name   <- file_settings$palette[j]
  
  file_stub  <- file_path_sans_ext(basename(file_name))
  period_tag <- paste0(start_year, "_", end_year)
  
  var_dir_out <- file.path(dir_out, file_stub)
  dir.create(var_dir_out, showWarnings = FALSE, recursive = TRUE)
  
  frame_dir <- file.path(var_dir_out, "gif-frames")
  if (dir.exists(frame_dir)) {
    unlink(frame_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(frame_dir, showWarnings = FALSE, recursive = TRUE)
  
  monthly_tif <- file.path(
    var_dir_out,
    paste0(file_stub, "_monthly_mean_", period_tag, ".tif")
  )
  
  gif_file <- file.path(
    var_dir_out,
    paste0(file_stub, "_monthly_", period_tag, ".gif")
  )
  
  cat("\n------------------------------------------------------------\n")
  cat("GIF creation for:\n")
  print(file_name)
  cat("Input monthly TIFF:\n")
  print(monthly_tif)
  cat("Output GIF:\n")
  print(gif_file)
  
  ## ---------------------------------------------------------------------------
  ## Check that monthly TIFF exists
  
  if (!file.exists(monthly_tif)) {
    cat("Monthly TIFF not found. Skipping GIF.\n")
    
    gif_log <- rbind(
      gif_log,
      data.frame(
        file_name        = file_name,
        monthly_tif      = monthly_tif,
        gif_file         = gif_file,
        gif_step         = "skipped_missing_monthly_tif",
        n_month_layers   = NA_integer_,
        stringsAsFactors = FALSE
      )
    )
    
    next
  }
  
  ## Optional: skip GIF if already exists
  if (file.exists(gif_file) && !overwrite_gif) {
    cat("GIF already exists. Skipping GIF creation.\n")
    
    x_month_existing <- rast(monthly_tif)
    
    gif_log <- rbind(
      gif_log,
      data.frame(
        file_name        = file_name,
        monthly_tif      = monthly_tif,
        gif_file         = gif_file,
        gif_step         = "skipped_existing",
        n_month_layers   = nlyr(x_month_existing),
        stringsAsFactors = FALSE
      )
    )
    
    next
  }
  
  ## ---------------------------------------------------------------------------
  ## Read monthly raster stack from disk
  
  x_month <- rast(monthly_tif)
  
  cat("\nMonthly raster stack read from disk:\n")
  print(x_month)
  
  ## ---------------------------------------------------------------------------
  ## Set consistent zlim across all frames
  ## Be more robust in case min/max metadata are missing
  
  x_month <- setMinMax(x_month)
  
  mm <- minmax(x_month)
  zlim <- round(range(mm, na.rm = TRUE))
  
  ## If minmax metadata still fail, compute directly from raster values
  if(any(!is.finite(zlim))) {
    
    cat("\nminmax() did not return finite values; computing global min/max...\n")
    
    global_min <- global(x_month, fun = "min", na.rm = TRUE)[1, 1]
    global_max <- global(x_month, fun = "max", na.rm = TRUE)[1, 1]
    
    zlim <- c(global_min, global_max)
  }
  
  ## Stop cleanly if still not valid
  if(any(!is.finite(zlim)) || is.na(zlim[1]) || is.na(zlim[2])) {
    stop("Could not determine finite plotting range for: ", file_name)
  }
  
  if(zlim[1] == zlim[2]) {
    stop("Plotting range has zero width for: ", file_name)
  }
  
  cat("\nMonthly value range used for plotting:\n")
  print(zlim)
#  zlim = c(0,80)
  
  ## ---------------------------------------------------------------------------
  ## Get plotting colors robustly
  
  plot_cols <- get_plot_cols(
    n_cols          = n_cols,
    pal_name        = pal_name,
    reverse_palette = reverse_palette
  )
  
  ## ---------------------------------------------------------------------------
  ## Optional quick plot check of first layer
  
  windows()
  plot(
    x_month[[1]],
    main  = paste0(plot_label, ": ", gsub("^X", "", names(x_month)[1])),
    col   = plot_cols,
    range = zlim,
    plg   = list(title = units_lab),
    mar   = c(3, 3, 3, 6),
    colNA = "gray75"
  )
  
  ## ---------------------------------------------------------------------------
  ## Make PNG frames
  
  png_files <- character(nlyr(x_month))
  
  for (i in seq_len(nlyr(x_month))) {
    
    layer_i    <- x_month[[i]]
    layer_name <- names(x_month)[i]
    png_file   <- file.path(frame_dir, sprintf("frame_%03d.png", i))
    
    png(
      filename = png_file,
      width    = gif_width,
      height   = gif_height,
      res      = gif_res
    )
    
    plot(
      layer_i,
      main  = paste0(plot_label, ": ", gsub("^X", "", names(x_month)[i])),
      col   = plot_cols,
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
  ## Build GIF
  
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
  
  gif_log <- rbind(
    gif_log,
    data.frame(
      file_name        = file_name,
      monthly_tif      = monthly_tif,
      gif_file         = gif_file,
      gif_step         = "created",
      n_month_layers   = nlyr(x_month),
      stringsAsFactors = FALSE
    )
  )
}

## -----------------------------------------------------------------------------
## Final summaries

cat("\n============================================================\n")
cat("MONTHLY AGGREGATION SUMMARY:\n")
print(monthly_log)

cat("\n============================================================\n")
cat("GIF CREATION SUMMARY:\n")
print(gif_log)
