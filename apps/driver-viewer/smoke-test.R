## Headless smoke test for the driver-viewer app. Not part of the app runtime.
## Run from repo root:
##   Rscript apps/driver-viewer/smoke-test.R
APP_DIR <- "apps/driver-viewer"
REPO_ROOT <- normalizePath(".", mustWork = FALSE)

OUT_ROOT      <- file.path(REPO_ROOT, "output-for-ecospace/env-drivers/CBEFS-hindcast")
BASEMAP_DIR   <- file.path(REPO_ROOT, "output-for-ecospace/habitat/basemaps")
REGRID_SUBDIR <- "var-stack-NC-monthly-regridded"
YEAR_START <- 1985L; YEAR_END <- 2024L
VARS   <- c("salinity", "temperature", "diss_o2", "phytoplankton", "NO3")
DEPTHS <- c("surf", "bott", "davg")
VAR_STYLES <- list(
  temperature   = list(label = "Temperature",      units = "°C",          palette = "Heat",    rev = TRUE),
  salinity      = list(label = "Salinity",         units = "PSU",         palette = "viridis", rev = FALSE),
  diss_o2       = list(label = "Dissolved oxygen", units = "mg O2 L-1",   palette = "YlGnBu",  rev = FALSE),
  phytoplankton = list(label = "Phytoplankton",    units = "mmol N m-3",  palette = "YlGn",    rev = FALSE),
  NO3           = list(label = "Nitrate",          units = "mmol N m-3",  palette = "Purples", rev = FALSE)
)
DEPTH_LABELS <- list(surf = "Surface", bott = "Bottom", davg = "Depth-averaged")

source(file.path(APP_DIR, "R", "helpers.R"))

res <- "F02-88x56"
ix  <- month_index(res)
cat("n months:", length(ix$layer), "\n")
cat("first label:", head(ix$label, 1), " last label:", tail(ix$label, 1), "\n")
cat("any duplicated labels:", any(duplicated(ix$label)), "\n")
cat("2024-01 count:", sum(ix$label == "2024-01"), "\n")

cat("salinity domain:", paste(round(var_domain(res, "salinity"), 2), collapse = " .. "), "\n")
cat("temperature domain:", paste(round(var_domain(res, "temperature"), 2), collapse = " .. "), "\n")

coast <- load_coast(res)
cat("coast class:", paste(class(coast), collapse = "/"), "\n")

## mid-record month 2005-07
midx <- ym_idx <- match("2005-07", ix$label)
cat("rendering month idx", midx, "=", ix$label[midx], "\n")

p1 <- plot_driver(res, "salinity", "bott", midx)
ggplot2::ggsave(file.path(APP_DIR, "smoke-single.png"), p1, width = 5, height = 7, dpi = 90)

combos <- expand.grid(depth = c("bott", "surf"), var = VARS, stringsAsFactors = FALSE)
plots <- Map(function(v, d) plot_driver(res, v, d, midx), combos$var, combos$depth)
pg <- patchwork::wrap_plots(plots, ncol = 2) +
  patchwork::plot_annotation(title = ix$label[midx])
ggplot2::ggsave(file.path(APP_DIR, "smoke-panels.png"), pg, width = 9, height = 16, dpi = 80, limitsize = FALSE)
cat("SMOKE_OK\n")
