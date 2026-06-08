## -----------------------------------------------------------------------------
## CBEFS bottom salinity test
## Daily stack -> Ecospace basemap grid -> monthly ASCII + GIF

rm(list = ls())

library(terra)
library(gifski)

## -----------------------------------------------------------------------------
## Terra settings

terra_tmp <- "./terra-temp"
dir.create(terra_tmp, showWarnings = FALSE, recursive = TRUE)

terraOptions(
  progress = 1,
  memfrac  = 0.7,
  tempdir  = terra_tmp
)

## -----------------------------------------------------------------------------
## User settings

input_format <- "NC"   ## "NC" or "TIFF"

dir_in_nc   <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-NC"
dir_in_tiff <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-TIFF"

basemap <- rast("./output-for-ecospace/habitat/base-depth-map-88x56.asc")

## Update this pattern to match your bottom salinity filename
var_pattern <- "bottom.*salinity|salinity.*bottom|bot.*sal|sal.*bot"

out_dir_base  <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/bottom-salinity-test"
dir_out_ascii <- file.path(out_dir_base, "ASCII-monthly")
dir_out_gif   <- file.path(out_dir_base, "GIF")
dir_out_png   <- file.path(out_dir_base, "PNG-frames")

dir.create(out_dir_base,  showWarnings = FALSE, recursive = TRUE)
dir.create(dir_out_ascii, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_out_gif,   showWarnings = FALSE, recursive = TRUE)
dir.create(dir_out_png,   showWarnings = FALSE, recursive = TRUE)

ascii_prefix <- "bottom_salinity"
gif_file     <- file.path(dir_out_gif, "bottom_salinity_monthly.gif")

## -----------------------------------------------------------------------------
## Helper functions

find_var_file <- function(input_format, dir_in_nc, dir_in_tiff, var_pattern) {
  
  dir_in <- if (toupper(input_format) == "NC") dir_in_nc else dir_in_tiff
  
  ext_pattern <- if (toupper(input_format) == "NC") {
    "\\.nc$"
  } else {
    "\\.(tif|tiff)$"
  }
  
  files <- list.files(
    dir_in,
    pattern     = ext_pattern,
    full.names  = TRUE,
    ignore.case = TRUE
  )
  
  if (length(files) == 0L) {
    stop("No input files found in: ", dir_in)
  }
  
  hits <- files[grepl(var_pattern, basename(files), ignore.case = TRUE)]
  
  if (length(hits) == 0L) {
    cat("\nAvailable files in directory:\n")
    print(basename(files))
    stop(
      "\nNo file matched var_pattern = '", var_pattern,
      "'. Update var_pattern to match the bottom salinity filename."
    )
  }
  
  if (length(hits) > 1L) {
    cat("\nMultiple files matched; using the first one:\n")
    print(basename(hits))
  }
  
  hits[1]
}

get_layer_dates <- function(x) {
  
  ## First try native time metadata
  tvals <- try(terra::time(x), silent = TRUE)
  
  if (!inherits(tvals, "try-error") &&
      !is.null(tvals) &&
      length(tvals) == nlyr(x)) {
    
    dates <- as.Date(tvals)
    
    if (all(!is.na(dates))) {
      return(dates)
    }
  }
  
  ## Fallback: parse dates from layer names
  nm <- names(x)
  date_chr <- rep(NA_character_, length(nm))
  
  ## Pattern like 2001-07-15 or 2001_07_15 or 2001.07.15
  hit_sep <- grepl("(19|20)\\d{2}[-_.](0[1-9]|1[0-2])[-_.]([0-2]\\d|3[01])", nm)
  
  date_chr[hit_sep] <- sub(
    ".*?((?:19|20)\\d{2})[-_.]((?:0[1-9]|1[0-2]))[-_.]((?:[0-2]\\d|3[01])).*",
    "\\1-\\2-\\3",
    nm[hit_sep]
  )
  
  ## Pattern like 20010715
  hit_compact <- is.na(date_chr) &
    grepl("(19|20)\\d{2}(0[1-9]|1[0-2])([0-2]\\d|3[01])", nm)
  
  date_chr[hit_compact] <- sub(
    ".*?((?:19|20)\\d{2})(\\d{2})(\\d{2}).*",
    "\\1-\\2-\\3",
    nm[hit_compact]
  )
  
  dates <- as.Date(date_chr)
  
  if (length(dates) != nlyr(x) || any(is.na(dates))) {
    stop(
      "Could not recover dates for all layers.\n",
      "For a TIFF stack with no time metadata and generic layer names, ",
      "use the NC file or rebuild the dates from a known start date."
    )
  }
  
  dates
}

align_to_basemap <- function(x, basemap, method = "bilinear") {
  
  x_crs <- terra::crs(x, proj = TRUE)
  b_crs <- terra::crs(basemap, proj = TRUE)
  
  same_defined_crs <- !is.na(x_crs) && !is.na(b_crs) && nzchar(x_crs) && nzchar(b_crs)
  
  if (same_defined_crs && terra::same.crs(x, basemap)) {
    x_out <- terra::resample(x, basemap, method = method)
  } else {
    x_out <- terra::project(x, basemap, method = method)
  }
  
  terra::mask(x_out, basemap)
}

write_monthly_ascii <- function(x_month, out_dir, prefix, naflag = -9999) {
  
  out_files <- character(nlyr(x_month))
  
  for (i in seq_len(nlyr(x_month))) {
    
    suffix <- sub(paste0("^", prefix, "_"), "", names(x_month)[i])
    
    out_file <- file.path(
      out_dir,
      paste0(prefix, "_", suffix, ".asc")
    )
    
    writeRaster(
      x_month[[i]],
      filename  = out_file,
      filetype  = "ascii",
      overwrite = TRUE,
      NAflag    = naflag
    )
    
    out_files[i] <- out_file
  }
  
  out_files
}

make_monthly_gif <- function(x_month, out_gif, frame_dir,
                             title_prefix = "Bottom salinity",
                             fps = 2) {
  
  if (dir.exists(frame_dir)) {
    unlink(frame_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(frame_dir, showWarnings = FALSE, recursive = TRUE)
  
  mm   <- terra::minmax(x_month)
  zlim <- range(mm, na.rm = TRUE)
  
  png_files <- character(nlyr(x_month))
  
  for (i in seq_len(nlyr(x_month))) {
    
    month_lab <- gsub(
      "_", "-",
      sub("^.*?(\\d{4}_\\d{2})$", "\\1", names(x_month)[i])
    )
    
    png_file <- file.path(frame_dir, sprintf("frame_%03d.png", i))
    
    png(filename = png_file, width = 1400, height = 900, res = 150)
    
    plot(
      x_month[[i]],
      main  = paste0(title_prefix, " | ", month_lab),
      col   = hcl.colors(100, "YlGnBu", rev = TRUE),
      range = zlim,
      mar   = c(3, 3, 3, 6),
      plg   = list(title = "psu")
    )
    
    dev.off()
    
    png_files[i] <- png_file
  }
  
  gifski(
    png_files = png_files,
    gif_file  = out_gif,
    width     = 1400,
    height    = 900,
    delay     = 1 / fps,
    loop      = TRUE
  )
  
  out_gif
}

## -----------------------------------------------------------------------------
## Find and read the bottom salinity file

file_in <- find_var_file(
  input_format = input_format,
  dir_in_nc    = dir_in_nc,
  dir_in_tiff  = dir_in_tiff,
  var_pattern  = var_pattern
)

cat("\nUsing input file:\n", file_in, "\n")

x_raw <- rast(file_in)

cat("\nInput raster summary:\n")
print(x_raw)

cat("\nBasemap summary:\n")
print(basemap)

## -----------------------------------------------------------------------------
## Recover dates and align to the Ecospace basemap

dates <- get_layer_dates(x_raw)

cat("\nDate range:\n")
print(range(dates))

x_aligned <- align_to_basemap(
  x       = x_raw,
  basemap = basemap,
  method  = "bilinear"
)

time(x_aligned) <- dates

cat("\nAligned raster summary:\n")
print(x_aligned)

cat("\nGeometry check against basemap:\n")
print(compareGeom(x_aligned[[1]], basemap, stopOnError = FALSE))

## -----------------------------------------------------------------------------
## Aggregate daily stack to monthly means

month_id  <- format(dates, "%Y-%m")
month_fac <- factor(month_id, levels = unique(month_id))

x_month <- tapp(
  x_aligned,
  index = month_fac,
  fun   = mean,
  na.rm = TRUE
)

month_names   <- unique(format(dates, "%Y_%m"))
names(x_month) <- paste0(ascii_prefix, "_", month_names)

cat("\nMonthly raster summary:\n")
print(x_month)

## -----------------------------------------------------------------------------
## Write monthly ASCII grids

ascii_files <- write_monthly_ascii(
  x_month = x_month,
  out_dir = dir_out_ascii,
  prefix  = ascii_prefix,
  naflag  = -9999
)

cat("\nASCII files written:\n")
print(ascii_files)

## -----------------------------------------------------------------------------
## Make monthly GIF

gif_out <- make_monthly_gif(
  x_month      = x_month,
  out_gif      = gif_file,
  frame_dir    = dir_out_png,
  title_prefix = "Bottom salinity",
  fps          = 2
)

cat("\nGIF written:\n")
print(gif_out)