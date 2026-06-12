## -----------------------------------------------------------------------------
## helpers.R -- data loading, color domains, and panel rendering for the
## Chesapeake environmental-driver Shiny viewer.
##
## This module only READS finished monthly NetCDF stacks produced by the
## make-environmental-drivers/ pipeline. It never re-runs the pipeline.
##
## Sourced by app.R, which defines the config constants (OUT_ROOT, BASEMAP_DIR,
## REGRID_SUBDIR, VARS, DEPTHS, VAR_STYLES, ...). All functions here read those
## from the global environment so the single config block stays the source of
## truth (mirrors make-environmental-drivers/cbefs-helpers.R).
## -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(terra)
  library(ggplot2)
})

## Module-level memo caches. Keyed by resolution label / "<res>|<var>" so that
## switching resolution back and forth, or re-rendering a month, is instant.
.cache <- new.env(parent = emptyenv())
.cache$stacks  <- list()   # res label -> list("<var>_<depth>" -> SpatRaster)
.cache$index   <- list()   # res label -> list(layer = int[480], label = chr[480])
.cache$domains <- list()   # "<res>|<var>" -> c(lo, hi)
.cache$masks   <- list()   # res label -> SpatRaster land/water mask
.cache$coast   <- NULL     # sf coastline+states, cropped to extent
.cache$gridln  <- list()   # res label -> sf cell-edge outline
.cache$juris   <- NULL     # sf MD/VA/Potomac dividing lines, cropped to extent

## --- Path building (mirrors cbefs-helpers.R::stack_dir_for) -------------------

stack_path <- function(res, var, depth) {
  file.path(OUT_ROOT, paste0("grid-", res), REGRID_SUBDIR,
            sprintf("%s_%s_%d_%d_monthly_mean.nc", var, depth, YEAR_START, YEAR_END))
}

basemap_path <- function(res) {
  file.path(BASEMAP_DIR, paste0("base-depth-map-", res, ".asc"))
}

## --- Stack loading + month index (de-dups the documented 2024-01 layer) ------
##
## The regridded stacks carry 481 layers / 480 distinct months: process-CBEFS.R
## emits a duplicate 2024-01 layer. We build a chronological month index that
## keeps the first occurrence of each date, so month_index 1..480 always maps to
## 1985-01 .. 2024-12 with no gap or duplicate.

load_stacks <- function(res) {
  if (!is.null(.cache$stacks[[res]])) return(.cache$stacks[[res]])

  stacks <- list()
  ref_time <- NULL
  for (var in VARS) for (depth in DEPTHS) {
    key <- paste0(var, "_", depth)
    p   <- stack_path(res, var, depth)
    if (!file.exists(p)) {
      warning("Missing stack: ", p)
      next
    }
    r <- terra::rast(p)
    stacks[[key]] <- r
    if (is.null(ref_time)) ref_time <- terra::time(r)
  }
  if (length(stacks) == 0) stop("No stacks found for resolution ", res)

  ## Chronological, de-duplicated month index from the reference stack's time.
  ord       <- order(ref_time)
  keep      <- !duplicated(ref_time[ord])
  layer_idx <- ord[keep]                                   # 480 layer positions
  labels    <- format(ref_time[ord][keep], "%Y-%m")        # "1985-01" .. "2024-12"

  .cache$stacks[[res]] <- stacks
  .cache$index[[res]]  <- list(layer = layer_idx, label = labels)
  stacks
}

month_index <- function(res) {
  load_stacks(res)
  .cache$index[[res]]
}

## Number of distinct months (should be 480 for the full hindcast).
n_months <- function(res) length(month_index(res)$layer)

## --- Per-variable color domain (robust 1-99% across depths & all months) -----
##
## One fixed domain per variable, shared across surf/bott/davg and all 480
## months, so panels and animation frames are directly comparable. Computed
## lazily on first use (sampling cells to stay fast) and cached.

var_domain <- function(res, var) {
  key <- paste0(res, "|", var)
  if (!is.null(.cache$domains[[key]])) return(.cache$domains[[key]])

  stacks <- load_stacks(res)
  vals <- c()
  for (depth in DEPTHS) {
    r <- stacks[[paste0(var, "_", depth)]]
    if (is.null(r)) next
    ## Sample to bound cost: F02 is tiny (88x56) so for small grids take all
    ## cells; spatSample with a generous size handles finer grids too.
    s <- terra::spatSample(r, size = min(2000, terra::ncell(r)),
                           method = "regular", na.rm = TRUE, warn = FALSE)
    vals <- c(vals, unlist(s, use.names = FALSE))
  }
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) return(c(0, 1))
  dom <- as.numeric(stats::quantile(vals, c(0.01, 0.99), names = FALSE))
  if (dom[1] == dom[2]) dom <- range(vals)                 # degenerate guard
  .cache$domains[[key]] <- dom
  dom
}

## --- Land/water mask ---------------------------------------------------------
## Basemap .asc: NA = land, value = water depth. Used as a grey land underlay.

load_mask <- function(res) {
  if (!is.null(.cache$masks[[res]])) return(.cache$masks[[res]])
  m <- terra::rast(basemap_path(res))
  .cache$masks[[res]] <- m
  m
}

## --- Coastline + state boundaries (rnaturalearth, cropped to F02 extent) -----
## Falls back to medium-scale coastline if rnaturalearthhires is unavailable.

## Read a prebuilt overlay .rds from OVERLAY_DIR, or NULL if absent / not in
## deployed mode. OVERLAY_DIR is set by app.R (NULL in local-dev mode).
read_overlay_rds <- function(name) {
  if (is.null(get0("OVERLAY_DIR", ifnotfound = NULL))) return(NULL)
  f <- file.path(OVERLAY_DIR, name)
  if (file.exists(f)) readRDS(f) else NULL
}

## Source the (non-bundled) overlay builders for the local-dev fallback path.
ensure_prebuild <- function() {
  if (!exists("build_coast", mode = "function")) {
    source(file.path(get0("APP_DIR", ifnotfound = "."), "R", "prebuild.R"),
           local = FALSE)
  }
}

load_coast <- function(res) {
  if (!is.null(.cache$coast)) return(.cache$coast)
  ## Deployed: read the prebaked overlay (no rnaturalearth needed at runtime).
  pre <- read_overlay_rds("coast.rds")
  if (!is.null(pre)) { .cache$coast <- pre; return(pre) }
  ## Local-dev fallback: build it now via prebuild.R.
  ensure_prebuild()
  e <- as.vector(terra::ext(load_mask(res)))   # named: xmin, xmax, ymin, ymax
  coast <- build_coast(e)
  .cache$coast <- coast
  coast
}

## --- Grid outline (cell-edge lines from the basemap, optional overlay) -------

load_gridlines <- function(res) {
  if (!is.null(.cache$gridln[[res]])) return(.cache$gridln[[res]])
  if (!requireNamespace("sf", quietly = TRUE)) { .cache$gridln[[res]] <- NA; return(NA) }
  pol <- terra::as.polygons(load_mask(res), aggregate = FALSE, na.rm = TRUE)
  g   <- sf::st_as_sf(pol)
  .cache$gridln[[res]] <- g
  g
}

## --- Jurisdictional boundaries (MD / VA / Potomac dividing lines) -------------
## Polygonize the high-res source jurisdiction raster (1 = MD, 2 = VA,
## 3 = Potomac; from JURIS_SRC), then extract the lines SHARED between adjacent
## jurisdictions — i.e. the internal dividing boundaries, not each region's full
## (coastline-following) perimeter. Resolution-independent: the lines are vector
## and only cropped to the basemap extent, so one cache serves all of F01-F04.

load_juris <- function(res) {
  if (!is.null(.cache$juris)) return(.cache$juris)
  ## Deployed: read the prebaked overlay.
  pre <- read_overlay_rds("juris.rds")
  if (!is.null(pre)) { .cache$juris <- pre; return(pre) }
  ## Local-dev fallback: build it now via prebuild.R from the source raster.
  ensure_prebuild()
  e   <- as.vector(terra::ext(load_mask(res)))
  src <- get0("JURIS_SRC", ifnotfound = NULL)
  bnd <- build_juris(e, src)
  .cache$juris <- bnd
  bnd
}

## --- One panel ---------------------------------------------------------------
## var x depth at one month_index. viridis/HCL water raster on a white panel +
## optional coastline, jurisdiction boundaries, and grid outline; extent/aspect
## locked north-up.

plot_driver <- function(res, var, depth, midx,
                        show_coast = TRUE, show_juris = FALSE, show_grid = FALSE) {
  stacks <- load_stacks(res)
  key    <- paste0(var, "_", depth)
  r      <- stacks[[key]]
  style  <- VAR_STYLES[[var]]
  e      <- as.vector(terra::ext(load_mask(res)))   # named: xmin, xmax, ymin, ymax

  if (is.null(r)) {
    return(ggplot() + theme_void() +
             annotate("text", 0, 0, label = paste("missing:", key)) +
             ggtitle(panel_title(var, depth)))
  }

  lyr  <- month_index(res)$layer[midx]
  df   <- terra::as.data.frame(r[[lyr]], xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "value"
  dom  <- var_domain(res, var)
  cols <- grDevices::hcl.colors(256, palette = style$palette, rev = style$rev)

  p <- ggplot() +
    geom_raster(data = df, aes(x = .data$x, y = .data$y, fill = .data$value)) +
    scale_fill_gradientn(colours = cols, limits = dom, oob = scales::squish,
                         name = style$units)

  coast <- if (show_coast) load_coast(res) else NA
  if (inherits(coast, "sf")) {
    p <- p + geom_sf(data = coast, fill = NA, color = "grey20", linewidth = 0.25)
  }
  if (show_juris) {
    jb <- load_juris(res)
    if (inherits(jb, "sf")) {
      ## white halo under a magenta line so the boundary reads over any palette.
      p <- p +
        geom_sf(data = jb, color = "white",   linewidth = 0.9) +
        geom_sf(data = jb, color = "#d01c8b", linewidth = 0.4)
    }
  }
  if (show_grid) {
    g <- load_gridlines(res)
    if (inherits(g, "sf")) {
      p <- p + geom_sf(data = g, fill = NA, color = "grey50", linewidth = 0.1)
    }
  }

  p +
    coord_sf(xlim = c(e[["xmin"]], e[["xmax"]]), ylim = c(e[["ymin"]], e[["ymax"]]),
             expand = FALSE) +
    ggtitle(panel_title(var, depth)) +
    theme_minimal(base_size = 11) +
    theme(axis.title = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          panel.grid = element_blank(),
          plot.title = element_text(face = "bold", size = 11))
}

panel_title <- function(var, depth) {
  paste0(VAR_STYLES[[var]]$label, " — ", DEPTH_LABELS[[depth]])
}
