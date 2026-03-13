library(terra)

dir_in  <- "./data/raw/jurisdictions"
juris   <- rast(file.path(dir_in, "jurisraster.tif"))
basemap <- rast("./output-for-ecospace/habitat/base-depth-map-88x56.asc")

## -----------------------------------------------------------------------------
## Build MD/VA base map only
## Remove Potomac before reprojection so it cannot compete with 1 or 2

juris_mdva_src <- ifel(juris == 3, NA, juris)

## Reproject to basemap grid
## Use "near" for categorical data; if "mode" works on your system, that is even better
juris_mdva <- project(juris_mdva_src, basemap, method = "near")
## juris_mdva <- project(juris_mdva_src, basemap, method = "mode")

## -----------------------------------------------------------------------------
## Build Potomac priority mask from source raster

potomac_src <- ifel(juris == 3, 1, NA)

## Convert Potomac to polygons
potomac_poly <- as.polygons(
  potomac_src,
  values = TRUE,
  na.rm = TRUE,
  aggregate = TRUE
)

## Reproject Potomac polygons to basemap CRS
potomac_poly <- project(potomac_poly, crs(basemap))

## -----------------------------------------------------------------------------
## Slightly buffer Potomac so near-miss coarse cells become Potomac
## Start with ~1/3 to 1/2 of a basemap cell
## Increase a bit if you still see holes; decrease if it gets too wide

potomac_buffer <- 1000
potomac_poly_buf <- buffer(potomac_poly, width = potomac_buffer)

## Rasterize buffered Potomac mask to basemap grid
potomac_poly_buf$potomac <- 1

potomac_mask <- rasterize(
  potomac_poly_buf,
  basemap,
  field = "potomac",
  touches = TRUE,
  background = NA
)
plot(potomac_mask)

## Combine layers with Potomac priority

juris_final <- juris_mdva
juris_final[!is.na(potomac_mask)] <- 3

## Mask land
juris_final <- mask(juris_final, basemap)


## -----------------------------------------------------------------------------
## Plot
plot(
  juris_final,
  col = c("purple4", "darkcyan", "gold"),
  legend = FALSE,
  axes = TRUE,
  box = TRUE,
  main = "Chesapeake Bay Jurisdictions"
)


