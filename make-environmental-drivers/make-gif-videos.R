## -----------------------------------------------------------------------------
## make-gif-videos.R   (Stage 3: GIF animations)
##
## Build GIF animations of monthly CBEFS fields, one set per resolution. Reads
## the stack for each resolution (native stacks for F00, the per-basemap
## regridded stacks for F01..F04) and renders frames directly -- no regrid here.
## Output: grid-<label>/GIFs/<var>_<depth>/<var>_<depth>_monthly_YYYY_YYYY.gif
##
## The native grid is just resolution F00-336x564, so native and model-resolution
## videos come out of the same loop -- no separate "gif_grid" flag.
##
## Flat top-to-bottom script: edit the settings block below and Source it, or set
## `resolutions` / `gif_prefixes` in run-environmental-drivers.R to drive it.
##
## Run order: process-CBEFS.R -> regrid-to-basemaps.R -> THIS
## -----------------------------------------------------------------------------

library(terra)
library(gifski)
library(tools)
library(viridisLite)
source("./make-environmental-drivers/cbefs-helpers.R")

## Resolve a monthly NC file from a <var>_<depth> prefix within a file list.
find_monthly_file <- function(prefix, available) {
  hit <- available[grepl(paste0("^", prefix, "_"), basename(available))]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

## -----------------------------------------------------------------------------
## Settings  (shared selectors fall back to defaults when run standalone)

if (!exists("resolutions"))  resolutions  <- NULL   ## NULL = all (F00..F04)
if (!exists("gif_prefixes")) gif_prefixes <- c("temperature_davg", "NO3_surf", "diss_o2_bott")
if (!exists("out_root"))     out_root     <- CBEFS_OUT_ROOT

start_year    <- 2021
num_years     <- 4
gif_delay     <- 0.25
gif_width     <- 900
gif_height    <- 1400
gif_res       <- 150
n_cols        <- 100
overwrite_gif <- TRUE

init_terra()  ## shared terra temp dir + progress + memfrac (see cbefs-helpers.R)

end_year   <- start_year + num_years - 1
start_date <- as.Date(paste0(start_year, "-01-01"))
end_date   <- as.Date(paste0(end_year,   "-12-31"))
period_tag <- paste0(start_year, "_", end_year)

cat("\nRequested date range:", format(start_date), "to", format(end_date), "\n")

## GIFs for ALL resolutions, incl. native F00.
res <- resolution_set(which = resolutions)

gif_log <- data.frame(
  resolution = character(), prefix = character(), gif_file = character(),
  status = character(), n_frames = integer(), stringsAsFactors = FALSE
)

## -----------------------------------------------------------------------------
## Loop resolutions -> per-prefix -> render frames -> encode GIF

for (r in seq_len(nrow(res))) {

  row    <- res[r, ]
  dir_in <- stack_dir_for(row, out_root)
  if (!dir.exists(dir_in)) {
    stop("Stacks not found for ", row$label, " (", dir_in,
         "). Run process-CBEFS.R (F00) / regrid-to-basemaps.R (basemaps) first.")
  }
  dir_out   <- file.path(out_root, row$res_dir, GIF_PRODUCT_SUBDIR)
  available <- list_monthly_nc(dir_in)

  cat("\n============================================================\n")
  log_step(sprintf("GIFs for %s (%s) -> %s", row$label, row$dims, dir_out))

  for (prefix in gif_prefixes) {

    st              <- get_var_style(prefix)
    plot_label      <- st$plot_label
    units_lab       <- st$units
    pal_name        <- st$palette
    reverse_palette <- st$reverse_palette

    file_in     <- find_monthly_file(prefix, available)
    var_dir_out <- file.path(dir_out, prefix)
    dir.create(var_dir_out, showWarnings = FALSE, recursive = TRUE)
    gif_file <- file.path(var_dir_out,
                          paste0(prefix, "_monthly_", period_tag, ".gif"))

    cat("\n------------------------------------------------------------\n")
    cat("GIF for:", row$label, "/", prefix, "\n")

    if (is.na(file_in)) {
      cat("No monthly NC found for prefix; skipping.\n")
      gif_log <- rbind(gif_log, data.frame(resolution = row$label, prefix,
        gif_file, status = "skipped_missing_nc", n_frames = NA_integer_,
        stringsAsFactors = FALSE))
      next
    }
    if (file.exists(gif_file) && !overwrite_gif) {
      cat("GIF exists; skipping.\n")
      gif_log <- rbind(gif_log, data.frame(resolution = row$label, prefix,
        gif_file, status = "skipped_existing", n_frames = NA_integer_,
        stringsAsFactors = FALSE))
      next
    }

    ## --- Read stack and subset to the requested year range ------------------
    x_month <- rast(file_in)
    dates   <- get_layer_dates(x_month)

    keep <- which(dates >= start_date & dates <= end_date)
    if (length(keep) == 0) {
      cat("No months in requested range; skipping.\n")
      gif_log <- rbind(gif_log, data.frame(resolution = row$label, prefix,
        gif_file, status = "skipped_no_months_in_range", n_frames = 0L,
        stringsAsFactors = FALSE))
      next
    }
    x_month     <- x_month[[keep]]
    frame_dates <- dates[keep]

    ## --- Consistent color scale across frames -------------------------------
    zlim <- round(robust_zlim(x_month))
    if (zlim[1] == zlim[2]) {
      stop("Could not determine a valid plotting range for: ", prefix)
    }

    plot_cols <- get_plot_cols(n_cols, pal_name, reverse_palette)

    ## --- Render frames ------------------------------------------------------
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
    gif_log <- rbind(gif_log, data.frame(resolution = row$label, prefix,
      gif_file, status = "created", n_frames = length(png_files),
      stringsAsFactors = FALSE))
  }
}

cat("\n============================================================\n")
cat("GIF CREATION SUMMARY:\n")
print(gif_log)
