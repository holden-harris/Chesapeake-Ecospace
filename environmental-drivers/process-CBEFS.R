## -----------------------------------------------------------------------------
## CBEFS processing

rm(list = ls())

library(terra)
library(stringr)
library(dplyr)

terraOptions(progress = 1, memfrac = 0.7)

## -----------------------------------------------------------------------------
## Directories

nc_path <- "./data/raw/CBEFS-hindcast"
tmp_dir <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/tmp-yearly"
out_dir <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/final"
dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## -----------------------------------------------------------------------------
## File table

nc_files <- list.files(
  path = nc_path,
  pattern = "^holdenharris_[0-9]{4}_v[0-9]{8}\\.nc$",
  full.names = TRUE
)

file_tbl <- tibble(
  file = nc_files,
  file_name = basename(nc_files),
  year = as.integer(str_match(file_name, "holdenharris_([0-9]{4})_v[0-9]{8}\\.nc$")[, 2])
) %>%
  arrange(year)

variables <- c("temperature", "salinity", "diss_o2", "phytoplankton", "NO3")

## -----------------------------------------------------------------------------
## Quiet reader

quiet_rast <- function(file, subds) {
  zz <- file(tempfile())
  sink(zz)
  on.exit({
    sink()
    close(zz)
  }, add = TRUE)
  
  r <- suppressWarnings(suppressMessages(rast(file, subds = subds)))
  return(r)
}

## -----------------------------------------------------------------------------
## Process one variable at a time
## outer loop = variable
## inner loop = file/year

for (v in variables) { ## All variables
 
#  v = variables[1] ## Test one variable
  
  cat("\n====================================================\n")
  cat("Processing variable:", v, "\n")
  
  bott_files <- character(nrow(file_tbl))
  surf_files <- character(nrow(file_tbl))
  davg_files <- character(nrow(file_tbl))
  
  ## ---------------------------------------------------------------------------
  ## Process each year and write yearly chunks
  
for (i in seq_len(nrow(file_tbl))) { ## All years
#  for (i in seq(1:4)) { ## Test 4 years
    
    this_file <- file_tbl$file[i]
    this_year <- file_tbl$year[i]
    
    cat("  Year:", this_year, "\n")
    
    ## Read variable
    rr <- quiet_rast(this_file, v)
    
    ## Flip vertically
    rr <- flip(rr, direction = "vertical")
    
    ## Check expected structure
    if (nlyr(rr) %% 3 != 0) {
      stop("Variable ", v, " in year ", this_year,
           " does not have a layer count divisible by 3.")
    }
    
    ## Split depths
    bott <- rr[[seq(1, nlyr(rr), by = 3)]]
    surf <- rr[[seq(2, nlyr(rr), by = 3)]]
    davg <- rr[[seq(3, nlyr(rr), by = 3)]]
    
    ## Build dates
    dates_i <- seq.Date(
      from = as.Date(sprintf("%s-01-01", this_year)),
      by = "day",
      length.out = nlyr(bott)
    )
    
    ## Assign names and time
    names(bott) <- paste0(v, "_bott_", format(dates_i, "%Y%m%d"))
    names(surf) <- paste0(v, "_surf_", format(dates_i, "%Y%m%d"))
    names(davg) <- paste0(v, "_davg_", format(dates_i, "%Y%m%d"))
    
    time(bott) <- dates_i
    time(surf) <- dates_i
    time(davg) <- dates_i
    
    ## Yearly output files
    bott_file <- file.path(tmp_dir, paste0(v, "_bott_", this_year, ".tif"))
    surf_file <- file.path(tmp_dir, paste0(v, "_surf_", this_year, ".tif"))
    davg_file <- file.path(tmp_dir, paste0(v, "_davg_", this_year, ".tif"))
    
    ## Write yearly chunks
    writeRaster(bott, bott_file, overwrite = TRUE)
    writeRaster(surf, surf_file, overwrite = TRUE)
    writeRaster(davg, davg_file, overwrite = TRUE)
    
    bott_files[i] <- bott_file
    surf_files[i] <- surf_file
    davg_files[i] <- davg_file
    
    ## Clean up
    rm(rr, bott, surf, davg)
    gc()
  }
  
  ## ---------------------------------------------------------------------------
  ## Re-open yearly chunks as file-backed stacks and combine
  
  cat("  Combining yearly chunks for", v, "\n")
  
  bott_all <- rast(bott_files)
  surf_all <- rast(surf_files)
  davg_all <- rast(davg_files)
  
  ## Final output files
  out_bott <- file.path(out_dir, paste0(v, "_bott_1985_2024.tif"))
  out_surf <- file.path(out_dir, paste0(v, "_surf_1985_2024.tif"))
  out_davg <- file.path(out_dir, paste0(v, "_davg_1985_2024.tif"))
  
  ## Write final stacks
  writeRaster(bott_all, out_bott, overwrite = TRUE)
  writeRaster(surf_all, out_surf, overwrite = TRUE)
  writeRaster(davg_all, out_davg, overwrite = TRUE)
  
  cat("  Wrote:\n")
  cat("   ", basename(out_bott), "\n")
  cat("   ", basename(out_surf), "\n")
  cat("   ", basename(out_davg), "\n")
  
  rm(bott_all, surf_all, davg_all)
  gc()
}
  

## -----------------------------------------------------------------------------
## Review output

print(bott_all)
print(surf_all)
print(davg_all)

cat("\nFinal layer counts:\n")
cat("  bott =", nlyr(bott_all), "\n")
cat("  surf =", nlyr(surf_all), "\n")
cat("  davg =", nlyr(davg_all), "\n")

cat("\nFirst / last dates:\n")
cat("  bott:", as.character(time(bott_all)[1]), "to",
    as.character(time(bott_all)[nlyr(bott_all)]), "\n")
cat("  surf:", as.character(time(surf_all)[1]), "to",
    as.character(time(surf_all)[nlyr(surf_all)]), "\n")
cat("  davg:", as.character(time(davg_all)[1]), "to",
    as.character(time(davg_all)[nlyr(davg_all)]), "\n")

## Optional plot check
par(mfrow = c(1, 3))
plot(bott_all[[1]], main = paste0(v, " bott first layer"))
plot(surf_all[[1]], main = paste0(v, " surf first layer"))
plot(davg_all[[1]], main = paste0(v, " davg first layer"))
par(mfrow = c(1, 1))  