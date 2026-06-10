## -----------------------------------------------------------------------------
## run-environmental-drivers.R   (orchestration)
##
## One entry point to run the Stage-2 / Stage-3 environmental-driver pipeline and
## choose WHICH stages and resolutions to (re)build. The heavy work (writing many
## ASCII files, plotting PDFs, encoding GIFs) is slow, so each product stays in
## its own flat script and you toggle the ones you want here.
##
## How it works: this script sets a few shared globals (resolutions /
## variables_to_run / gif_prefixes) and then `source()`s the chosen stage scripts.
## Each stage script is flat top-to-bottom and reads those globals via an
## `if (!exists(...))` fallback, so it also runs standalone with its own defaults.
##
## Pipeline:
##   process-CBEFS.R        (run separately) -> native monthly stacks
##   do_regrid -> regrid-to-basemaps.R       -> per-basemap regridded stacks
##   do_ascii  -> make-ecospace-ascii-drivers.R
##   do_pdf    -> make-driver-pdfs.R
##   do_gif    -> make-gif-videos.R
## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## What to run

do_regrid <- TRUE    ## Stage 2: native -> per-basemap regridded NC (basemaps only)
do_ascii  <- TRUE    ## Stage 3: ASCII drivers (basemaps only)
do_pdf    <- TRUE    ## Stage 3: PDF plots (all resolutions, incl. native F00)
do_gif    <- TRUE    ## Stage 3: GIF animations (all resolutions, incl. native F00)

## -----------------------------------------------------------------------------
## Shared config (injected into the stage scripts as globals)

## Resolution subset by label (e.g. "F02-88x56") or dims (e.g. "88x56"); NULL =
## all. F00 (native) only applies to PDFs/GIFs; ASCII/regrid skip it.
resolutions <- NULL

## Variable subset by <var>_<depth> prefix (e.g. "salinity_bott"); NULL = all.
variables_to_run <- NULL

## Variables to animate as GIFs (subset of the available prefixes).
gif_prefixes <- c("temperature_davg", "NO3_surf", "diss_o2_bott")

## Output root (edit to relocate the whole grid-<label>/... tree).
out_root <- "./output-for-ecospace/env-drivers/CBEFS-hindcast"

## -----------------------------------------------------------------------------
## Run the selected stages. Sourcing each flat script runs it top-to-bottom; the
## shared globals above flow in via the scripts' exists() fallbacks.

cat("\n############################################################\n")
cat("run-environmental-drivers.R\n")
cat("  regrid:", do_regrid, "| ascii:", do_ascii,
    "| pdf:", do_pdf, "| gif:", do_gif, "\n")
cat("  resolutions:", if (is.null(resolutions)) "ALL"
    else paste(resolutions, collapse = ", "), "\n")
cat("  variables:", if (is.null(variables_to_run)) "ALL"
    else paste(variables_to_run, collapse = ", "), "\n")
cat("############################################################\n")

if (do_regrid) source("./make-environmental-drivers/regrid-to-basemaps.R")
if (do_ascii)  source("./make-environmental-drivers/make-ecospace-ascii-drivers.R")
if (do_pdf)    source("./make-environmental-drivers/make-driver-pdfs.R")
if (do_gif)    source("./make-environmental-drivers/make-gif-videos.R")

cat("\n############################################################\n")
cat("run-environmental-drivers.R complete.\n")
cat("############################################################\n")
