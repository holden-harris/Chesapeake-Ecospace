## -----------------------------------------------------------------------------
## stage-data.R -- assemble a self-contained `apps/driver-viewer/data/` folder so
## the app can be bundled and deployed to shinyapps.io (which only ships files
## inside the app directory).
##
## Run from the REPO ROOT:
##   Rscript apps/driver-viewer/deploy/stage-data.R
##
## Produces:
##   data/env-drivers/grid-<res>/var-stack-NC-monthly-regridded/*.nc   (F02-F04)
##   data/basemaps/base-depth-map-<res>.{asc,prj}
##   data/overlays/coast.rds, juris.rds        (prebaked; no rnaturalearth at runtime)
##
## Re-run whenever the underlying drivers change, then redeploy.
## -----------------------------------------------------------------------------

suppressWarnings(suppressMessages(library(terra)))

APP_DIR  <- "apps/driver-viewer"
REPO_OUT <- "output-for-ecospace/env-drivers/CBEFS-hindcast"
REPO_BM  <- "output-for-ecospace/habitat/basemaps"
JURIS_SRC <- "data-inputs/spatial-static/jurisdictions/jurisraster.tif"
REGRID_SUBDIR <- "var-stack-NC-monthly-regridded"

RESOLUTIONS <- c("F02-88x56", "F03-59x37", "F04-44x28")   # keep in sync with app.R

DATA     <- file.path(APP_DIR, "data")
ENV_DST  <- file.path(DATA, "env-drivers")
BM_DST   <- file.path(DATA, "basemaps")
OVL_DST  <- file.path(DATA, "overlays")
for (d in c(ENV_DST, BM_DST, OVL_DST)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

copy_into <- function(files, dest) {
  ok <- file.copy(files, dest, overwrite = TRUE)
  if (!all(ok)) warning("failed to copy: ", paste(files[!ok], collapse = ", "))
  invisible(ok)
}

## --- 1. NetCDF stacks + basemaps per resolution ------------------------------
total_mb <- 0
for (res in RESOLUTIONS) {
  src_dir <- file.path(REPO_OUT, paste0("grid-", res), REGRID_SUBDIR)
  dst_dir <- file.path(ENV_DST, paste0("grid-", res), REGRID_SUBDIR)
  dir.create(dst_dir, recursive = TRUE, showWarnings = FALSE)
  ncs <- list.files(src_dir, pattern = "\\.nc$", full.names = TRUE)
  if (length(ncs) == 0) stop("No .nc found in ", src_dir)
  copy_into(ncs, dst_dir)
  total_mb <- total_mb + sum(file.size(ncs)) / 1e6

  bm <- list.files(REPO_BM, pattern = paste0("^base-depth-map-", res, "\\.(asc|prj)$"),
                   full.names = TRUE)
  copy_into(bm, BM_DST)
  cat(sprintf("staged %-12s  %2d stacks + %d basemap files\n", res, length(ncs), length(bm)))
}

## --- 2. Prebake overlays to .rds (uses rnaturalearth* locally; NOT in bundle) -
source(file.path(APP_DIR, "R", "prebuild.R"))
base <- terra::rast(list.files(BM_DST,
          pattern = paste0("^base-depth-map-", RESOLUTIONS[1], "\\.asc$"),
          full.names = TRUE))
ext_named <- as.vector(terra::ext(base))   # named xmin,xmax,ymin,ymax (EPSG:4326)

coast <- build_coast(ext_named)
juris <- build_juris(ext_named, JURIS_SRC)
saveRDS(coast, file.path(OVL_DST, "coast.rds"))
saveRDS(juris, file.path(OVL_DST, "juris.rds"))
cat(sprintf("overlays: coast=%s  juris=%s\n",
            class(coast)[1], class(juris)[1]))

cat(sprintf("\nStaged %.0f MB of NetCDF into %s\n", total_mb, normalizePath(DATA)))
cat("STAGE_DONE\n")
