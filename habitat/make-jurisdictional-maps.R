library(terra)

pot_buffer_m <- 7200   ## start small: 2000 m = 2 km

fig_dir <- "./habitat/plots/"
asc_dir <- "./output-for-ecospace/jurisdictions/ascii"
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
if (!dir.exists(asc_dir)) dir.create(asc_dir, recursive = TRUE)

## -----------------------------------------------------------------------------
## Read data
dir_in  <- "./data/raw/jurisdictions"
juris   <- rast(file.path(dir_in, "jurisraster.tif"))
basemap <- rast("./output-for-ecospace/habitat/base-depth-map-88x56.asc")

plot(juris, main = "Original jurisdiction raster")
plot(basemap, main = "Basemap")
crs(juris, proj = TRUE)
crs(basemap, proj = TRUE)

## -----------------------------------------------------------------------------
## Build separate binary rasters from the high-resolution source
## 1 = present, 0 = absent

md_src  <- ifel(juris == 1, 1, 0)
va_src  <- ifel(juris == 2, 1, 0)
pot_src <- ifel(juris == 3, 1, 0)

## -----------------------------------------------------------------------------
## Buffer Potomac in the source CRS
## Since juris is in Mercator, width is in meters

## -----------------------------------------------------------------------------
## Potomac only: 1 = Potomac, NA = everything else

pot_src <- ifel(juris == 3, 1, NA)
plot(pot_src, main = "Potomac source only")

## Convert only Potomac cells to polygons
pot_poly <- as.polygons(
  pot_src,
  dissolve = TRUE,
  na.rm = TRUE
)

plot(pot_poly, main = "Potomac polygon only")

## -----------------------------------------------------------------------------
## Buffer Potomac polygon in source CRS (meters, since juris is Mercator)
pot_poly_buf <- buffer(pot_poly, width = pot_buffer_m)

plot(pot_poly_buf, main = paste("Buffered Potomac =", pot_buffer_m, "m"))

## -----------------------------------------------------------------------------
## Add a field explicitly for rasterization

pot_poly_buf$potomac <- 1

## Rasterize buffered Potomac back to the source raster grid
pot_src_buf <- rasterize(
  pot_poly_buf,
  juris,
  field = "potomac",
  background = NA,
  touches = TRUE
)

## Convert to clean binary if desired
pot_src_buf <- ifel(!is.na(pot_src_buf), 1, 0)

plot(pot_src_buf, main = "Buffered Potomac raster")

## -----------------------------------------------------------------------------
## Project each layer directly to the basemap grid
## Use "max" so that if any source cell of that class falls in a target cell,
## the target cell becomes 1 for that layer

md  <- project(md_src,  basemap, method = "max")
va  <- project(va_src,  basemap, method = "max")
pot <- project(pot_src_buf, basemap, method = "max")

## Force clean binary values
md  <- ifel(md  >= 1, 1, 0)
va  <- ifel(va  >= 1, 1, 0)
pot <- ifel(pot >= 1, 1, 0)

## -----------------------------------------------------------------------------
## Optional check: where do projected layers overlap?
## Cells > 1 mean more than one class is present after projection

overlap_count <- md + va + pot
freq(overlap_count)
plot(overlap_count, main = "Number of overlapping classes per cell")

## -----------------------------------------------------------------------------
## Reassemble in priority order:
## first Virginia, then Maryland, then Potomac

juris_coarse <- rast(basemap)
values(juris_coarse) <- NA

juris_coarse[va == 1]  <- 2
juris_coarse[md == 1]  <- 1
juris_coarse[pot == 1] <- 3

## Keep only water cells
juris_coarse <- mask(juris_coarse, basemap)

plot(juris_coarse,
     col = c("purple4", "darkcyan", "gold"),
     main = "Jurisdictions after layer-by-layer projection", 
     colNA = 'gray75')

## -----------------------------------------------------------------------------
## Fill missing water cells with nearest assigned jurisdiction
fill_na_nearest <- function(r, water_mask) {
  
  out  <- r
  vals <- values(out, mat = FALSE)
  water_vals <- values(water_mask, mat = FALSE)
  
  water_idx   <- which(!is.na(water_vals))
  donor_idx   <- which(!is.na(vals) & !is.na(water_vals))
  target_idx  <- which(is.na(vals)  & !is.na(water_vals))
  
  if (length(target_idx) == 0) return(out)
  
  xy <- xyFromCell(out, 1:ncell(out))
  
  donor_xy   <- xy[donor_idx, , drop = FALSE]
  donor_vals <- vals[donor_idx]
  
  for (i in seq_along(target_idx)) {
    this_xy <- xy[target_idx[i], ]
    d2 <- (donor_xy[, 1] - this_xy[1])^2 + (donor_xy[, 2] - this_xy[2])^2
    vals[target_idx[i]] <- donor_vals[which.min(d2)]
  }
  
  values(out) <- vals
  return(out)
}

juris_final <- fill_na_nearest(juris_coarse, basemap)
juris_final <- mask(juris_final, basemap)

## -----------------------------------------------------------------------------
## Plot final result

land_mask <- ifel(is.na(basemap), 1, NA)

plot(juris_final,
     col = c("purple4", "darkcyan", "gold"),
     legend = FALSE,
     axes = TRUE,
     box = TRUE,
     main = "Chesapeake Bay Jurisdictions")

plot(land_mask, add = TRUE, col = "grey80", legend = FALSE)

legend("topright",
       legend = c("Maryland", "Virginia", "Potomac"),
       fill   = c("purple4", "darkcyan", "gold"),
       bty    = "n")

## -----------------------------------------------------------------------------
## Quick check of missing water cells

sum(is.na(values(juris_final, mat = FALSE)) &
      !is.na(values(basemap, mat = FALSE)))


## -----------------------------------------------------------------------------
## Export jurisdiction map as PNG

png(
  filename = file.path(fig_dir, "chesapeake_bay_jurisdictions.png"),
  width = 6, height = 6, units = 'in', res = 1000
)

plot(
  juris_final,
  col = c("purple4", "darkcyan", "gold"),
  legend = FALSE,
  axes = TRUE,
  box = TRUE,
  main = "Chesapeake Bay Jurisdictions", 
  colNA = "gray75"
)

legend(
  "topright",
  legend = c("Maryland", "Virginia", "Potomac"),
  fill = c("purple4", "darkcyan", "gold"),
  bty = "n"
)

dev.off()

## -----------------------------------------------------------------------------
## Create binary jurisdiction layers for Ecospace
## 1 = present
## 0 = absent in water cells
## NA = land

md_layer <- ifel(
  is.na(basemap), NA,
  ifel(juris_final == 1, 1, 0)
)

va_layer <- ifel(
  is.na(basemap), NA,
  ifel(juris_final == 2, 1, 0)
)

pot_layer <- ifel(
  is.na(basemap), NA,
  ifel(juris_final == 3, 1, 0)
)

names(md_layer)  <- "Maryland"
names(va_layer)  <- "Virginia"
names(pot_layer) <- "Potomac"

## -----------------------------------------------------------------------------
## Quick check plots

par(mfrow = c(1, 3))
plot(md_layer,  main = "Maryland")
plot(va_layer,  main = "Virginia")
plot(pot_layer, main = "Potomac")
par(mfrow = c(1, 1))

## -----------------------------------------------------------------------------
## Export each jurisdiction as ESRI ASCII

writeRaster(
  md_layer,
  filename = file.path(asc_dir, "jurisdiction_maryland.asc"),
  filetype = "AAIGrid",
  overwrite = TRUE,
  NAflag = -9999,
  datatype = "INT2S"
)

writeRaster(
  va_layer,
  filename = file.path(asc_dir, "jurisdiction_virginia.asc"),
  filetype = "AAIGrid",
  overwrite = TRUE,
  NAflag = -9999,
  datatype = "INT2S"
)

writeRaster(
  pot_layer,
  filename = file.path(asc_dir, "jurisdiction_potomac.asc"),
  filetype = "AAIGrid",
  overwrite = TRUE,
  NAflag = -9999,
  datatype = "INT2S"
)

## -----------------------------------------------------------------------------
## Optional: confirm files read correctly

md_check  <- rast(file.path(asc_dir, "jurisdiction_maryland.asc"))
va_check  <- rast(file.path(asc_dir, "jurisdiction_virginia.asc"))
pot_check <- rast(file.path(asc_dir, "jurisdiction_potomac.asc"))

par(mfrow = c(1, 3))
plot(md_check,  main = "Maryland ASC")
plot(va_check,  main = "Virginia ASC")
plot(pot_check, main = "Potomac ASC")
par(mfrow = c(1, 1))

