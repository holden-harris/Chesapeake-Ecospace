## -----------------------------------------------------------------------------
## Aggregate one daily NetCDF stack to monthly means
## and make a GIF animation of the monthly layers
##
## Example file:
## salinity_bott_1985_2024.nc
##
## Notes:
## - This ignores CRS / alignment issues for now
## - Assumes the NetCDF already has a valid time dimension
## - Uses monthly mean for aggregation
## -----------------------------------------------------------------------------

rm(list = ls())

library(terra)
library(gifski)

num_test_years <- 2

## -----------------------------------------------------------------------------
## Optional terra settings for large files

terraOptions(
  progress = 1,
  memfrac  = 0.7
)

## -----------------------------------------------------------------------------
## File paths

file_in <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-NC/salinity_bott_1985_2024.nc"

out_dir <- "./environmental-drivers/GIFs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

frame_dir <- file.path(out_dir, "gif-frames")
dir.create(frame_dir, showWarnings = FALSE, recursive = TRUE)

gif_file <- file.path(out_dir, "salinity_bott_monthly.gif")

## Optional output for the monthly raster stack
monthly_tif <- file.path(out_dir, "salinity_bott_monthly_mean.tif")

## -----------------------------------------------------------------------------
## Read the daily raster stack

x_raw <- rast(file_in)

cat("\nInput raster:\n")
print(x_raw)

## -----------------------------------------------------------------------------
## Extract dates from the time dimension

dates <- as.Date(time(x_raw))

cat("\nFirst few dates:\n")
print(head(dates))

cat("\nLast few dates:\n")
print(tail(dates))

if (all(is.na(dates))) {
  stop("No valid time dimension detected in the NetCDF.")
}

## -----------------------------------------------------------------------------
## Keep only the first two years as a test
## This example keeps 1985 and 1986

start_year <- format(min(dates), "%Y")
end_year   <- as.character(as.numeric(start_year) + num_test_years- 1)

keep_idx <- which(dates >= as.Date(paste0(start_year, "-01-01")) &
                    dates <= as.Date(paste0(end_year, "-12-31")))

x_test <- x_raw[[keep_idx]]
dates_test <- dates[keep_idx]

cat("\nTest raster:\n")
print(x_test)

cat("\nDate range in test raster:\n")
print(range(dates_test))

## -----------------------------------------------------------------------------
## Create monthly grouping variable

month_id <- format(dates_test, "%Y-%m")

cat("\nNumber of daily layers in test raster:\n")
print(length(month_id))

cat("\nUnique months in test raster:\n")
print(unique(month_id))

## -----------------------------------------------------------------------------
## Set terra temp directory

terra_tmp <- "./terra-temp"
dir.create(terra_tmp, showWarnings = FALSE, recursive = TRUE)

terraOptions(
  tempdir  = terra_tmp,
  progress = 1,
  memfrac  = 0.7
)

## -----------------------------------------------------------------------------
## Output directory

monthly_tif <- file.path(out_dir, "salinity_bott_monthly_mean_1985_1986.tif")

## -----------------------------------------------------------------------------
## Aggregate daily layers to monthly means

x_month <- tapp(
  x_test,
  index     = month_id,
  fun       = mean,
  na.rm     = TRUE,
  filename  = monthly_tif,
  overwrite = TRUE
)

## -----------------------------------------------------------------------------
## Name the monthly layers

names(x_month) <- paste0(unique(month_id))

cat("\nMonthly raster stack:\n")
print(x_month)

## -----------------------------------------------------------------------------
## Optional: write the monthly stack to disk as a TIFF

writeRaster(
  x_month,
  filename  = monthly_tif,
  overwrite = TRUE
)

cat("\nMonthly TIFF written to:\n")
print(monthly_tif)

## -----------------------------------------------------------------------------
## Set a consistent color scale across all monthly frames
## so the GIF does not rescale every frame

mm <- minmax(x_month)
zlim <- range(mm, na.rm = TRUE)

cat("\nMonthly value range used for plotting:\n")
print(zlim)

## -----------------------------------------------------------------------------
## Make PNG frames for the GIF

png_files <- character(nlyr(x_month))

for (i in seq_len(nlyr(x_month))) {
  
  layer_i <- x_month[[i]]
  layer_name <- names(x_month)[i]
  
  png_file <- file.path(frame_dir, sprintf("frame_%03d.png", i))
  
  png(
    filename = png_file,
    width    = 1400,
    height   = 900,
    res      = 150
  )
  
  plot(
    layer_i,
    main  = paste0("", gsub("salinity_bott_", "", layer_name)),
    col   = hcl.colors(100, "YlGnBu", rev = TRUE),
    range = zlim,
    plg   = list(title = "PSS-1978"),
    mar   = c(3, 3, 3, 6),
    colNA = "gray75"
  )
  
  dev.off()
  
  png_files[i] <- png_file
}

cat("\nNumber of PNG frames written:\n")
print(length(png_files))

## -----------------------------------------------------------------------------
## Build the GIF from the PNG frames

gifski(
  png_files = png_files,
  gif_file  = gif_file,
  width     = 1400,
  height    = 900,
  delay     = 0.25,
  loop      = TRUE
)

cat("\nGIF written to:\n")
print(gif_file)









library(terra)
library(magick)

dir_in <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-TIFF/"

basemap <- rast("./output-for-ecospace/habitat/base-depth-map-88x56.asc")
basemap
plot(basemap)

r <- rast(file.path(dir_in, "salinity_bott_1985_2024.tif"))

r_sub <- r[[seq(1, nlyr(r), by = 14)]]

rng <- range(values(r), na.rm = TRUE)

frames <- lapply(1:nlyr(r_sub), function(i) {
  
  f <- tempfile(fileext = ".png")
  
  png(f, width = 900, height = 700)
  plot(r_sub[[i]], zlim = rng)
  title(time(r_sub)[i])
  dev.off()
  
  image_read(f)
})

gif <- image_animate(image_join(frames), fps = 10)

image_write(gif, file.path(dir_in, "temperature_surface.gif"))









#rm(list = ls())

## make-cbefs-mp4-fast.R
## Purpose: Build a fast MP4 directly from a CBEFS raster stack
## without writing temporary PNG frames

rm(list = ls())

library(terra)
library(av)

## -----------------------------------------------------------------------------
## User settings

dir_in  <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/final"
dir_out <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/animations"
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

infile  <- file.path(dir_in, "temperature_bott_1985_2024.tif")
outfile <- file.path(dir_out, "temperature_bott_1985_2024_fast.mp4")

## Thin frames for speed
frame_by <- 30

## Video settings
fps_out <- 8
video_width  <- 900
video_height <- 700

main_title <- "CBEFS temperature bottom"

## -----------------------------------------------------------------------------
## Read raster stack

r <- rast(infile)

## Subsample layers
idx <- seq(1, nlyr(r), by = frame_by)
r_sub <- r[[idx]]

## Fixed z-range across all frames
mm <- terra::minmax(r)
zlim <- c(mm[1], mm[2])

## Fixed colors
cols <- hcl.colors(100, "RdYlBu", rev = TRUE)

cat("Input layers:", nlyr(r), "\n")
cat("Frames to render:", nlyr(r_sub), "\n")
cat("Output file:", outfile, "\n")

## -----------------------------------------------------------------------------
## Stream plots directly to MP4

av::av_capture_graphics(
  {
    for (i in seq_len(nlyr(r_sub))) {
      
      par(
        mar = c(3, 3, 4, 6),
        xaxs = "i",
        yaxs = "i"
      )
      
      plot(
        r_sub[[i]],
        zlim = zlim,
        col = cols,
        axes = FALSE,
        legend = TRUE,
        main = ""
      )
      
      title(
        main = paste0(main_title, "\n", as.character(time(r_sub)[i]))
      )
      
      if (i %% 25 == 0 || i == nlyr(r_sub)) {
        cat("Rendered frame", i, "of", nlyr(r_sub), "\n")
      }
    }
  },
  output = outfile,
  width = video_width,
  height = video_height,
  framerate = fps_out
)

cat("\nFinished MP4:\n", outfile, "\n")



















## -----------------------------------------------------------------------------
## Read the daily raster stack

x_raw <- rast(file_in)

cat("\nInput raster:\n")
print(x_raw)

## -----------------------------------------------------------------------------
## Check that time is available

dates <- as.Date(time(x_raw))

cat("\nFirst few dates:\n")
print(head(dates))

cat("\nLast few dates:\n")
print(tail(dates))

if (all(is.na(dates))) {
  stop("No valid time dimension detected in the NetCDF.")
}

## -----------------------------------------------------------------------------
## Create a year-month grouping variable
## Example: 1985-01, 1985-02, etc.

month_id <- format(dates, "%Y-%m")

cat("\nNumber of daily layers:\n")
print(length(month_id))

cat("\nNumber of unique months:\n")
print(length(unique(month_id)))

## -----------------------------------------------------------------------------
## Aggregate daily layers to monthly mean
##
## tapp() groups layers based on the index vector
## and calculates mean for each month

## -----------------------------------------------------------------------------
## Set a valid terra temp directory

terra_tmp <- "./terra-temp"
dir.create(terra_tmp, showWarnings = FALSE, recursive = TRUE)

terraOptions(
  tempdir  = terra_tmp,
  progress = 1,
  memfrac  = 0.7
)

## Check terra settings
terraOptions()

x_month <- tapp(
  x_raw,
  index = month_id,
  fun   = mean,
  na.rm = TRUE
)

## -----------------------------------------------------------------------------
## Give the monthly layers clean names

names(x_month) <- paste0("salinity_bott_", unique(month_id))

cat("\nMonthly raster stack:\n")
print(x_month)

## -----------------------------------------------------------------------------
## Optional: write the monthly stack to disk as a TIFF

writeRaster(
  x_month,
  filename  = monthly_tif,
  overwrite = TRUE
)

cat("\nMonthly TIFF written to:\n")
print(monthly_tif)

## -----------------------------------------------------------------------------
## Set a consistent color scale across all monthly frames
## so the GIF does not rescale every frame

mm <- minmax(x_month)
zlim <- range(mm, na.rm = TRUE)

cat("\nMonthly value range used for plotting:\n")
print(zlim)

## -----------------------------------------------------------------------------
## Make PNG frames for the GIF

png_files <- character(nlyr(x_month))

for (i in seq_len(nlyr(x_month))) {
  
  layer_i <- x_month[[i]]
  layer_name <- names(x_month)[i]
  
  png_file <- file.path(frame_dir, sprintf("frame_%03d.png", i))
  
  png(
    filename = png_file,
    width    = 1400,
    height   = 900,
    res      = 150
  )
  
  plot(
    layer_i,
    main  = paste0("Bottom salinity monthly mean: ", gsub("salinity_bott_", "", layer_name)),
    col   = hcl.colors(100, "YlGnBu", rev = TRUE),
    range = zlim,
    plg   = list(title = "PSS-1978"),
    mar   = c(3, 3, 3, 6)
  )
  
  dev.off()
  
  png_files[i] <- png_file
}

cat("\nNumber of PNG frames written:\n")
print(length(png_files))

## -----------------------------------------------------------------------------
## Build the GIF from the PNG frames

gifski(
  png_files = png_files,
  gif_file  = gif_file,
  width     = 1400,
  height    = 900,
  delay     = 0.25,
  loop      = TRUE
)

cat("\nGIF written to:\n")
print(gif_file)




library(terra)
library(magick)

dir_in <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-TIFF/"

basemap <- rast("./output-for-ecospace/habitat/base-depth-map-88x56.asc")
basemap
plot(basemap)

r <- rast(file.path(dir_in, "salinity_bott_1985_2024.tif"))

r_sub <- r[[seq(1, nlyr(r), by = 14)]]

rng <- range(values(r), na.rm = TRUE)

frames <- lapply(1:nlyr(r_sub), function(i) {
  
  f <- tempfile(fileext = ".png")
  
  png(f, width = 900, height = 700)
  plot(r_sub[[i]], zlim = rng)
  title(time(r_sub)[i])
  dev.off()
  
  image_read(f)
})

gif <- image_animate(image_join(frames), fps = 10)

image_write(gif, file.path(dir_in, "temperature_surface.gif"))









#rm(list = ls())

## make-cbefs-mp4-fast.R
## Purpose: Build a fast MP4 directly from a CBEFS raster stack
## without writing temporary PNG frames

rm(list = ls())

library(terra)
library(av)

## -----------------------------------------------------------------------------
## User settings

dir_in  <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/final"
dir_out <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/animations"
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

infile  <- file.path(dir_in, "temperature_bott_1985_2024.tif")
outfile <- file.path(dir_out, "temperature_bott_1985_2024_fast.mp4")

## Thin frames for speed
frame_by <- 30

## Video settings
fps_out <- 8
video_width  <- 900
video_height <- 700

main_title <- "CBEFS temperature bottom"

## -----------------------------------------------------------------------------
## Read raster stack

r <- rast(infile)

## Subsample layers
idx <- seq(1, nlyr(r), by = frame_by)
r_sub <- r[[idx]]

## Fixed z-range across all frames
mm <- terra::minmax(r)
zlim <- c(mm[1], mm[2])

## Fixed colors
cols <- hcl.colors(100, "RdYlBu", rev = TRUE)

cat("Input layers:", nlyr(r), "\n")
cat("Frames to render:", nlyr(r_sub), "\n")
cat("Output file:", outfile, "\n")

## -----------------------------------------------------------------------------
## Stream plots directly to MP4

av::av_capture_graphics(
  {
    for (i in seq_len(nlyr(r_sub))) {
      
      par(
        mar = c(3, 3, 4, 6),
        xaxs = "i",
        yaxs = "i"
      )
      
      plot(
        r_sub[[i]],
        zlim = zlim,
        col = cols,
        axes = FALSE,
        legend = TRUE,
        main = ""
      )
      
      title(
        main = paste0(main_title, "\n", as.character(time(r_sub)[i]))
      )
      
      if (i %% 25 == 0 || i == nlyr(r_sub)) {
        cat("Rendered frame", i, "of", nlyr(r_sub), "\n")
      }
    }
  },
  output = outfile,
  width = video_width,
  height = video_height,
  framerate = fps_out
)

cat("\nFinished MP4:\n", outfile, "\n")