## -----------------------------------------------------------------------------
## prebuild.R -- build the static map overlays (coastline + jurisdiction
## boundaries) that the viewer draws on every panel.
##
## This file is the ONLY place that touches rnaturalearth* (non-CRAN
## rnaturalearthhires) and terra::project of the jurisdiction raster. It is
## EXCLUDED from the shinyapps.io bundle (see .rscignore) so those packages never
## reach the deploy manifest. deploy/stage-data.R runs these once to write
## data/overlays/{coast,juris}.rds; the deployed app just readRDS() them.
## helpers.R also sources this as a local-dev fallback when no .rds is present.
##
## Both builders take an explicit named extent vector (xmin, xmax, ymin, ymax in
## EPSG:4326) so they have no dependency on the app's config globals.
## -----------------------------------------------------------------------------

## Coastline + MD/VA state boundaries, cropped to the model extent (EPSG:4326).
## Prefers high-res boundaries (rnaturalearthhires); falls back to medium-scale
## (rnaturalearthdata); returns NA if neither is available.
build_coast <- function(ext_named) {
  if (!requireNamespace("sf", quietly = TRUE) ||
      !requireNamespace("rnaturalearth", quietly = TRUE)) {
    return(NA)
  }
  bb <- sf::st_bbox(c(xmin = ext_named[["xmin"]], ymin = ext_named[["ymin"]],
                      xmax = ext_named[["xmax"]], ymax = ext_named[["ymax"]]),
                    crs = 4326)
  states <- if (requireNamespace("rnaturalearthhires", quietly = TRUE)) {
    tryCatch(rnaturalearth::ne_states(country = "United States of America",
                                      returnclass = "sf"),
             error = function(err) NA)
  } else if (requireNamespace("rnaturalearthdata", quietly = TRUE)) {
    tryCatch(rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf"),
             error = function(err) NA)
  } else {
    NA
  }
  if (inherits(states, "logical")) return(NA)
  states <- sf::st_transform(states, 4326)
  suppressWarnings(sf::st_crop(states, bb))
}

## Internal MD/VA/Potomac dividing lines from the high-res jurisdiction raster
## (1 = MD, 2 = VA, 3 = Potomac), cropped to the model extent (EPSG:4326).
## Extracts only the boundaries SHARED between adjacent jurisdictions (not each
## region's full coastline-following perimeter). Returns NA if the source is
## missing or no shared lines are found.
build_juris <- function(ext_named, juris_src) {
  if (!requireNamespace("sf", quietly = TRUE) ||
      is.null(juris_src) || !file.exists(juris_src)) {
    return(NA)
  }
  j  <- terra::project(terra::rast(juris_src), "EPSG:4326", method = "near")
  jp <- sf::st_make_valid(sf::st_as_sf(
          terra::as.polygons(j, dissolve = TRUE, na.rm = TRUE)))
  names(jp)[1] <- "code"
  geom_of <- function(code) sf::st_geometry(jp[jp$code == code, ])
  shared <- function(a, b) {
    if (length(a) == 0 || length(b) == 0) return(NULL)
    g <- suppressWarnings(sf::st_intersection(sf::st_boundary(a), sf::st_boundary(b)))
    g <- sf::st_collection_extract(g, "LINESTRING")
    if (length(g) == 0) NULL else sf::st_union(g)
  }
  md <- geom_of(1); va <- geom_of(2); pot <- geom_of(3)
  parts <- Filter(Negate(is.null),
                  list(shared(md, va), shared(md, pot), shared(va, pot)))
  if (length(parts) == 0) return(NA)
  bnd <- sf::st_sf(geometry = do.call(c, parts))
  bb  <- sf::st_bbox(c(xmin = ext_named[["xmin"]], ymin = ext_named[["ymin"]],
                       xmax = ext_named[["xmax"]], ymax = ext_named[["ymax"]]),
                     crs = 4326)
  suppressWarnings(sf::st_crop(bnd, bb))
}
