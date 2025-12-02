rm(list = ls())

## Load packages
library(marmap)
library(terra)
windows()

## Set resolution ------------------------------------------------------------
lon1 <- -77.4   ## westernmost lon
lon2 <- -75.55   ## easternmost lon
lat1 <-  36.7    ## southernmost lat
lat2 <-  39.65    ## northernmost lat

## 
## Resolution is in arc-minutes; 1' ≈ ~1–2 km depending on latitude
bathy_raw <- getNOAA.bathy(
  lon1 = lon1,
  lon2 = lon2,
  lat1 = lat1,
  lat2 = lat2,
  resolution = 1
)

## Check
bathy_raw
plot(bathy_raw, image = TRUE)

## Convert bathy object to xyz data.frame
bathy_xyz <- as.xyz(bathy_raw)
head(bathy_xyz)
## Columns: x (lon), y (lat), z (depth, usually negative)

## Convert to terra raster
bathy_rast <- rast(
  x = bathy_xyz,
  type = "xyz",
  crs  = "EPSG:4326"  ## WGS84 lon/lat
)

## Convert negative depth to positive; keep land as 0
depth_rast <- bathy_rast

## Values are in a single layer
vals <- values(depth_rast, mat = FALSE)

## Convert: water (<0) to positive depth; land (>=0) to 0
vals_depth <- ifelse(vals < 0, -vals, 0)
values(depth_rast) <- vals_depth

## Mask out land as NA instead of 0
vals_depth_na <- ifelse(vals < 0, -vals, NA)
values(depth_rast) <- vals_depth_na

## Check raster
depth_rast
plot(depth_rast)  ## Depth in meters, 0 or NA for land
## --> 192 rows x 120 col = 23040 cells


## Adjust resolution -----------------------------------------------------------
depth_rast2 <- aggregate(depth_rast, fact = 2, fun = mean, na.rm = TRUE)
depth_rast2
plot(depth_rast2)
## --> 96 rows x 60 col = 5760 cells

depth_rast3 <- aggregate(depth_rast, fact = 3, fun = mean, na.rm = TRUE)
depth_rast3
plot(depth_rast3)
## --> 64 rows x 40 cols = 2560 total cells

depth_rast4 <- aggregate(depth_rast, fact = 4, fun = mean, na.rm = TRUE)
depth_rast4
plot(depth_rast4)
## --> 48 rows x 30 cols = 1440

## Get resolutions -------------------------------------------------------------
## Cell size of original
res(depth_rast)
res(depth_rast2)
res(depth_rast3)
res(depth_rast4)

dy_m  <- res(depth_rast)[2] * 111320
dy2_m <- res(depth_rast2)[2] * 111320
dy3_m <- res(depth_rast3)[2] * 111320
dy4_m <- res(depth_rast4)[2] * 111320

## For east–west (dx), multiply by cosine(latitude):
mean_lat <- (lat1 + lat2) / 2
dx_m  <- res(depth_rast)[1] * 111320 * cos(mean_lat * pi/180)
dx2_m <- res(depth_rast2)[1] * 111320 * cos(mean_lat * pi/180)
dx3_m <- res(depth_rast3)[1] * 111320 * cos(mean_lat * pi/180)
dx4_m <- res(depth_rast4)[1] * 111320 * cos(mean_lat * pi/180)

## Make a helper function for resolution summaries
summ <- function(rast, name) {
  res_deg <- res(rast)
  
  dy_m  <- res_deg[2] * 111320
  dx_m  <- res_deg[1] * 111320 * cos(mean_lat * pi/180)
  
  data.frame(
    Raster   = name,
    Lon_deg  = res_deg[1],
    Lat_deg  = res_deg[2],
    Long_sec = res_deg[1] * 3600,
    Lat_sec  = res_deg[2] * 3600, 
    dx_m     = dx_m,
    dy_m     = dy_m,
    dx_km    = dx_m / 1000,
    dy_km    = dy_m / 1000,
    Cell_km  = paste0(round(dx_m/1000, 2), " × ", round(dy_m/1000, 2)),
    sq_km =   (dx_m / 1000) * (dy_m / 1000)
  )
}

## Build the table
tab <- rbind(
  summ(depth_rast,  "depth_rast (base)"),
  summ(depth_rast2, "depth_rast2 (fact=2)"),
  summ(depth_rast3, "depth_rast3 (fact=3)"),
  summ(depth_rast4, "depth_rast4 (fact=4)")
); print(tab)

## Compare plots
par(mfrow=c(2,2))
plot(depth_rast,  colNA='gray', main=paste0(round(tab$sq_km[1]), " sq. km | ", dim(depth_rast)[1],'x',dim(depth_rast)[2]))
plot(depth_rast2, colNA='gray', main=paste0(round(tab$sq_km[2]), " sq. km | ", dim(depth_rast2)[1],'x',dim(depth_rast2)[2]))
plot(depth_rast3, colNA='gray', main=paste0(round(tab$sq_km[3]), " sq. km | ", dim(depth_rast3)[1],'x',dim(depth_rast3)[2]))
plot(depth_rast4, colNA='gray', main=paste0(round(tab$sq_km[4]), " sq. km | ", dim(depth_rast4)[1],'x',dim(depth_rast4)[2]))
par(mfrow=c(1,1))

## ----------------------------------------------------------------------------
## Write out depth/base map

## Choose the raster to export
depth_base <- depth_rast2   ## aggregated (fact = 2) depth in meters
plot(depth_base)

## Write ESRI ASCII grid for Ecospace
writeRaster(
  depth_base,
  filename = "./data/derived/base_map.asc",  
  filetype = "ascii",                       ## ESRI ASCII grid
  overwrite = TRUE,
)



