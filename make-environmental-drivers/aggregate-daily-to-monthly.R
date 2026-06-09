## -----------------------------------------------------------------------------
## Aggregate daily NetCDF stacks to monthly means
##
## Notes:
## - Reads all .nc files in dir_in
## - Assumes each file has a valid time dimension
## - Aggregates daily layers to monthly mean layers
## - Writes one monthly .nc file per input .nc file
## - Prints progress and timestamps for each file and each year
## - Can skip existing outputs or overwrite them
## -----------------------------------------------------------------------------

rm(list = ls())

library(terra)
library(tools)

## -----------------------------------------------------------------------------
## User settings

dir_in  <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-NC"
dir_out <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-NC-monthly"

overwrite_monthly <- FALSE   ## FALSE = skip existing monthly files
write_log_csv     <- TRUE

## Optional: subset years
## Set to NULL to use full date range in each file
start_year <- NULL
end_year   <- NULL

## -----------------------------------------------------------------------------
## Create output and temp directories

dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

terra_tmp <- "./terra-temp"
dir.create(terra_tmp, showWarnings = FALSE, recursive = TRUE)

terraOptions(
  tempdir  = terra_tmp,
  progress = 1,
  memfrac  = 0.7
)

## -----------------------------------------------------------------------------
## Helper objects

timestamp_start <- Sys.time()

monthly_log <- data.frame(
  file_name        = character(),
  file_in          = character(),
  monthly_nc       = character(),
  status           = character(),
  start_date       = character(),
  end_date         = character(),
  n_daily_layers   = integer(),
  n_month_layers   = integer(),
  years_processed  = character(),
  time_started     = character(),
  time_finished    = character(),
  elapsed_minutes  = numeric(),
  stringsAsFactors = FALSE
)

## -----------------------------------------------------------------------------
## List available NetCDF files in the input folder

available_files <- list.files(
  dir_in,
  pattern    = "\\.nc$",
  full.names = FALSE
)

cat("\n============================================================\n")
cat("Available NetCDF files in folder:\n")
print(available_files)

if (length(available_files) == 0) {
  stop("No .nc files found in: ", dir_in)
}

## -----------------------------------------------------------------------------
## LOOP: Build monthly NetCDF stacks

cat("\n============================================================\n")
cat("START LOOP: MONTHLY AGGREGATION\n")
cat("Run started:", format(timestamp_start, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Overwrite existing monthly files:", overwrite_monthly, "\n")
cat("============================================================\n")

for (j in seq_along(available_files)) {
  
  file_name <- available_files[j]
  file_in   <- file.path(dir_in, file_name)
  
  file_stub <- file_path_sans_ext(basename(file_name))
  
  monthly_nc <- file.path(
    dir_out,
    paste0(file_stub, "_monthly_mean.nc")
  )
  
  file_time_start <- Sys.time()
  
  cat("\n------------------------------------------------------------\n")
  cat("File", j, "of", length(available_files), "\n")
  cat("Started:", format(file_time_start, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Input file:\n")
  print(file_in)
  cat("Output monthly NC:\n")
  print(monthly_nc)
  
  ## ---------------------------------------------------------------------------
  ## Skip if monthly NC already exists
  
  if (file.exists(monthly_nc) && !overwrite_monthly) {
    
    cat("Monthly file already exists. Skipping aggregation.\n")
    
    x_month_existing <- rast(monthly_nc)
    
    file_time_end <- Sys.time()
    
    monthly_log <- rbind(
      monthly_log,
      data.frame(
        file_name        = file_name,
        file_in          = file_in,
        monthly_nc       = monthly_nc,
        status           = "skipped_existing",
        start_date       = NA_character_,
        end_date         = NA_character_,
        n_daily_layers   = NA_integer_,
        n_month_layers   = nlyr(x_month_existing),
        years_processed  = NA_character_,
        time_started     = format(file_time_start, "%Y-%m-%d %H:%M:%S"),
        time_finished    = format(file_time_end, "%Y-%m-%d %H:%M:%S"),
        elapsed_minutes  = round(as.numeric(difftime(file_time_end, file_time_start, units = "mins")), 2),
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
  ## Subset to requested date range, if provided
  
  if (!is.null(start_year) && !is.null(end_year)) {
    
    keep_idx <- dates >= as.Date(paste0(start_year, "-01-01")) &
      dates <= as.Date(paste0(end_year, "-12-31"))
    
    if (!any(keep_idx)) {
      cat("No dates found in requested year range. Skipping file.\n")
      
      file_time_end <- Sys.time()
      
      monthly_log <- rbind(
        monthly_log,
        data.frame(
          file_name        = file_name,
          file_in          = file_in,
          monthly_nc       = monthly_nc,
          status           = "skipped_no_dates_in_range",
          start_date       = NA_character_,
          end_date         = NA_character_,
          n_daily_layers   = 0L,
          n_month_layers   = 0L,
          years_processed  = NA_character_,
          time_started     = format(file_time_start, "%Y-%m-%d %H:%M:%S"),
          time_finished    = format(file_time_end, "%Y-%m-%d %H:%M:%S"),
          elapsed_minutes  = round(as.numeric(difftime(file_time_end, file_time_start, units = "mins")), 2),
          stringsAsFactors = FALSE
        )
      )
      
      next
    }
    
    x_sub     <- x_raw[[which(keep_idx)]]
    dates_sub <- dates[keep_idx]
    
  } else {
    
    x_sub     <- x_raw
    dates_sub <- dates
  }
  
  cat("\nSubset raster:\n")
  print(x_sub)
  
  cat("\nDate range in subset raster:\n")
  print(range(dates_sub))
  
  ## ---------------------------------------------------------------------------
  ## Print yearly progress summary before aggregation
  
  years_in_file <- unique(format(dates_sub, "%Y"))
  
  cat("\nYears detected in file:\n")
  print(years_in_file)
  
  for (yy in years_in_file) {
    yr_dates <- dates_sub[format(dates_sub, "%Y") == yy]
    
    cat(
      paste0(
        "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
        "Preparing year ", yy,
        " | daily layers: ", length(yr_dates),
        " | months present: ", length(unique(format(yr_dates, "%Y-%m")))
      ),
      "\n"
    )
  }
  
  ## ---------------------------------------------------------------------------
  ## Create monthly grouping variable
  
  month_id <- format(dates_sub, "%Y-%m")
  
  cat("\nNumber of daily layers in subset raster:\n")
  print(length(month_id))
  
  cat("\nUnique months in subset raster:\n")
  print(length(unique(month_id)))
  
  ## ---------------------------------------------------------------------------
  ## Aggregate to monthly means
  ##
  ## Note:
  ## tapp() will compute all monthly means in one pass.
  ## The year-level messages above give you progress markers for long files.
  
  cat(
    "\n[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "] Starting monthly aggregation for ", file_name, " ...\n",
    sep = ""
  )
  
  x_month <- tapp(
    x_sub,
    index = month_id,
    fun   = mean,
    na.rm = TRUE
  )
  
  names(x_month) <- unique(month_id)
  
  ## Set monthly time stamps to first day of each month
  time(x_month) <- as.Date(paste0(unique(month_id), "-01"))
  
  cat(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "] Monthly aggregation finished for ", file_name, "\n",
    sep = ""
  )
  
  ## ---------------------------------------------------------------------------
  ## Write monthly NetCDF
  
  cat(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "] Writing monthly NC: ", monthly_nc, "\n",
    sep = ""
  )
  
  writeRaster(
    x_month,
    filename  = monthly_nc,
    overwrite = TRUE,
    filetype  = "CDF"
  )
  
  cat(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    "] Finished writing monthly NC\n",
    sep = ""
  )
  
  cat("\nMonthly raster stack:\n")
  print(x_month)
  
  file_time_end <- Sys.time()
  
  monthly_log <- rbind(
    monthly_log,
    data.frame(
      file_name        = file_name,
      file_in          = file_in,
      monthly_nc       = monthly_nc,
      status           = "created",
      start_date       = as.character(min(dates_sub)),
      end_date         = as.character(max(dates_sub)),
      n_daily_layers   = nlyr(x_sub),
      n_month_layers   = nlyr(x_month),
      years_processed  = paste(years_in_file, collapse = ","),
      time_started     = format(file_time_start, "%Y-%m-%d %H:%M:%S"),
      time_finished    = format(file_time_end, "%Y-%m-%d %H:%M:%S"),
      elapsed_minutes  = round(as.numeric(difftime(file_time_end, file_time_start, units = "mins")), 2),
      stringsAsFactors = FALSE
    )
  )
  
  cat(
    "\nCompleted file ", j, " of ", length(available_files),
    " in ", round(as.numeric(difftime(file_time_end, file_time_start, units = "mins")), 2),
    " minutes.\n",
    sep = ""
  )
}

## -----------------------------------------------------------------------------
## Write processing log

if (write_log_csv) {
  
  log_file <- file.path(
    dir_out,
    paste0("monthly_aggregation_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  )
  
  write.csv(monthly_log, log_file, row.names = FALSE)
  
  cat("\n============================================================\n")
  cat("Monthly aggregation log written to:\n")
  print(log_file)
}

## -----------------------------------------------------------------------------
## Wrap up

timestamp_end <- Sys.time()

cat("\n============================================================\n")
cat("MONTHLY AGGREGATION COMPLETE\n")
cat("Started :", format(timestamp_start, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Finished:", format(timestamp_end,   "%Y-%m-%d %H:%M:%S"), "\n")
cat(
  "Elapsed :", round(as.numeric(difftime(timestamp_end, timestamp_start, units = "mins")), 2),
  "minutes\n"
)
cat("============================================================\n")
