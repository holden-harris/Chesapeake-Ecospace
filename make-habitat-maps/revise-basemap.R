## revise-basemap.R
## -----------------------------------------------------------------------------
## Review and (optionally) revise the F02 Ecospace basemap.
##
## 1. Loads the 88x56 basemap written by make-baythymetry-basemap.R
## 2. Downloads high-resolution (15 arc-sec) NOAA ETOPO 2022 bathymetry via
##    marmap and extracts the Chesapeake Bay "waterline" as the 0-m contour
## 3. Overlays the waterline + cell grid on the basemap for visual review
##    (full-bay map + three zoom panels), and flags suspect cells:
##      - basemap water cells that are mostly land at high resolution
##      - basemap land cells that are mostly water at high resolution
## 4. After review: fill in the manual-edit tables below, set
##    apply_revisions <- TRUE, and re-run to write the revised basemap.
##
## Run from the repo root (like all other scripts in this project).

rm(list = ls())

library(marmap)
library(terra)

## User flags -----------------------------------------------------------------
write_png          <- TRUE    ## save review figures to fig_dir
apply_revisions    <- TRUE    ## apply the reviewed proposal + manual tables
overwrite_original <- FALSE   ## FALSE = write *-revised.asc; TRUE = overwrite
                              ## base-depth-map-F02-88x56.asc in place

fig_dir     <- "./make-habitat-maps/plots"
dir_basemap <- "./output-for-ecospace/habitat/basemaps"
etopo_cache <- "./data-inputs/spatial-static/etopo"  ## getNOAA.bathy csv cache

if (!dir.exists(fig_dir))     dir.create(fig_dir,     recursive = TRUE)
if (!dir.exists(etopo_cache)) dir.create(etopo_cache, recursive = TRUE)

## Write a PNG, skipping (with a message) if the file is locked by a viewer
safe_png <- function(path, plot_expr, width, height, res) {
  ok <- tryCatch({
    png(path, width = width, height = height, units = "in", res = res,
        bg = "white")
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok) {
    message("Skipped writing ", path, " (file locked? close the viewer and re-run)")
    return(invisible(FALSE))
  }
  on.exit(dev.off())
  force(plot_expr)
  invisible(TRUE)
}

## -----------------------------------------------------------------------------
## Load the F02 basemap

basemap <- rast(file.path(dir_basemap, "base-depth-map-F02-88x56.asc"))
crs(basemap) <- "EPSG:4326"   ## ESRI ASCII carries no CRS
basemap
## 88 rows x 56 cols; depth (m) in water cells, NA on land

## -----------------------------------------------------------------------------
## Fetch high-resolution bathymetry (ETOPO 2022, 15 arc-sec ~ 450 m)
## Same extent as make-baythymetry-basemap.R; ~8x finer than the F02 cells.
## keep = TRUE caches the download as csv in etopo_cache, so re-runs are local.

lon1 <- -77.4
lon2 <- -75.55
lat1 <-  36.7
lat2 <-  39.65

bathy_hi_raw <- getNOAA.bathy(
  lon1 = lon1, lon2 = lon2,
  lat1 = lat1, lat2 = lat2,
  resolution = 0.25,
  keep = TRUE,
  path = etopo_cache
)

## Convert to terra raster (same as.xyz idiom as make-baythymetry-basemap.R)
bathy_hi <- rast(
  x    = as.xyz(bathy_hi_raw),
  type = "xyz",
  crs  = "EPSG:4326"
)
names(bathy_hi) <- "elev"   ## signed: negative = water depth, positive = land
bathy_hi

## Waterline = 0-m contour of the high-res bathymetry
waterline <- as.contour(bathy_hi, levels = 0)

## -----------------------------------------------------------------------------
## Per-cell water fraction and suggested depth from the high-res grid

## Binary water mask at 15 arc-sec (1 = water, 0 = land)
water_hi <- ifel(bathy_hi < 0, 1, 0)

## Fraction of each F02 cell that is water in reality
water_frac <- resample(water_hi, basemap, method = "average")
names(water_frac) <- "water_frac"

## Mean water depth of each F02 cell (positive m, land excluded)
depth_hi <- ifel(bathy_hi < 0, -bathy_hi, NA)
depth_suggest <- resample(depth_hi, basemap, method = "average")
names(depth_suggest) <- "depth_suggest"

## -----------------------------------------------------------------------------
## Flag suspect cells

bm_vals   <- values(basemap,       mat = FALSE)
frac_vals <- values(water_frac,    mat = FALSE)
dsug_vals <- values(depth_suggest, mat = FALSE)

cell_ids <- 1:ncell(basemap)
xy       <- xyFromCell(basemap, cell_ids)

qa <- data.frame(
  cell       = cell_ids,
  row        = rowFromCell(basemap, cell_ids),  ## row 1 = north
  col        = colFromCell(basemap, cell_ids),
  lon        = round(xy[, 1], 4),
  lat        = round(xy[, 2], 4),
  depth_bm   = round(bm_vals, 2),
  water_frac = round(frac_vals, 3),
  depth_sug  = round(dsug_vals, 2)
)

## Basemap water cells that are mostly land at high resolution
suspect_water <- subset(qa, !is.na(depth_bm) & water_frac < 0.5)
suspect_water <- suspect_water[order(suspect_water$water_frac), ]

## Basemap land cells that are mostly water at high resolution
suspect_land <- subset(qa, is.na(depth_bm) & water_frac > 0.5)
suspect_land <- suspect_land[order(-suspect_land$water_frac), ]

cat("\nBasemap water cells that are MOSTLY LAND at 15 arc-sec (candidates to remove):\n")
print(suspect_water, row.names = FALSE)

cat("\nBasemap land cells that are MOSTLY WATER at 15 arc-sec (candidates to add):\n")
print(suspect_land, row.names = FALSE)

## Save the QA table for the review discussion
suspects <- rbind(
  cbind(flag = "mostly_land_remove?", suspect_water),
  cbind(flag = "mostly_water_add?",   suspect_land)
)
write.csv(suspects, "./make-habitat-maps/basemap-suspect-cells-F02.csv",
          row.names = FALSE)

## -----------------------------------------------------------------------------
## Review plots: basemap + cell grid + high-res waterline

## Cell edges for grid lines
x_edges <- seq(xmin(basemap), xmax(basemap), by = res(basemap)[1])
y_edges <- seq(ymin(basemap), ymax(basemap), by = res(basemap)[2])

plot_review <- function(zoom_ext = NULL, main = "", label_every = 5,
                        waterline_lwd = 0.8, mark_suspects = TRUE,
                        show_legend = TRUE) {

  ext_use <- if (is.null(zoom_ext)) ext(basemap) else ext(zoom_ext)

  plot(basemap, ext = ext_use, colNA = "gray75", legend = FALSE,
       main = main, axes = TRUE, mar = c(2.5, 2.5, 2.5, 2.5))

  ## Cell grid
  abline(v = x_edges, h = y_edges,
         col = adjustcolor("black", 0.15), lwd = 0.3)

  ## Row / col index labels (top = col number, right = row number; row 1 = north)
  ## Drawn just INSIDE the mapped extent. Do not use xpd here (inline or via
  ## par): toggling the clip region corrupts the device state and a later
  ## legend() silently drops its first entry (R 4.5.3 windows png device).
  cols  <- seq(1, ncol(basemap), by = label_every)
  rows  <- seq(1, nrow(basemap), by = label_every)
  col_x <- xFromCol(basemap, cols)
  row_y <- yFromRow(basemap, rows)
  in_x  <- col_x >= ext_use[1] & col_x <= ext_use[2]
  in_y  <- row_y >= ext_use[3] & row_y <= ext_use[4]
  text(col_x[in_x], ext_use[4] - 0.6 * res(basemap)[2], labels = cols[in_x],
       cex = 0.45, col = "gray30")
  text(ext_use[2] - 0.8 * res(basemap)[1], row_y[in_y], labels = rows[in_y],
       cex = 0.45, col = "gray30")

  ## High-res waterline
  lines(waterline, col = "red", lwd = waterline_lwd)

  ## Mark suspect cells
  if (mark_suspects) {
    if (nrow(suspect_water) > 0)
      points(suspect_water$lon, suspect_water$lat,
             pch = 4, col = "blue", cex = 0.6, lwd = 1.2)
    if (nrow(suspect_land) > 0)
      points(suspect_land$lon, suspect_land$lat,
             pch = 3, col = "magenta", cex = 0.6, lwd = 1.2)
  }

  ## Legend anchored inside the mapped extent (terra clips to the raster)
  if (show_legend) {
    legend(x = ext_use[1] + 0.02 * (ext_use[2] - ext_use[1]),
           y = ext_use[3] + 0.02 * (ext_use[4] - ext_use[3]),
           yjust = 0,
           legend = c("waterline (ETOPO 15 arc-sec, 0 m)",
                      "water cell, mostly land",
                      "land cell, mostly water"),
           col = c("red", "blue", "magenta"),
           lty = c(1, NA, NA), pch = c(NA, 4, 3),
           cex = 0.55, bg = "white")
  }
}

## Zoom regions (xmin, xmax, ymin, ymax)
ext_upper <- c(-76.6, -75.9,  38.9, 39.65)  ## Upper Bay / Susquehanna
ext_mid   <- c(-77.4, -75.9,  37.9, 39.0)   ## Mid Bay / Potomac
ext_lower <- c(-77.0, -75.55, 36.7, 38.0)   ## Lower Bay / James-York-Rapp.

## On-screen review
if (interactive()) {
  windows()
  plot_review(main = "F02 basemap vs. ETOPO 15 arc-sec waterline")
}

## PNG: full bay
if (write_png) {
  safe_png(file.path(fig_dir, "basemap-waterline-review-F02.png"),
           plot_review(main = "F02 basemap (88x56) vs. ETOPO 15 arc-sec waterline"),
           width = 6, height = 8, res = 1200)

  ## PNG: zoom panels
  safe_png(file.path(fig_dir, "basemap-waterline-review-F02-zooms.png"), {
    par(mfrow = c(1, 3))
    plot_review(ext_upper, "Upper Bay",         label_every = 2, waterline_lwd = 1.2)
    plot_review(ext_mid,   "Mid Bay / Potomac", label_every = 2, waterline_lwd = 1.2)
    plot_review(ext_lower, "Lower Bay",         label_every = 2, waterline_lwd = 1.2)
    par(mfrow = c(1, 1))
  }, width = 15, height = 6, res = 1000)
}

## -----------------------------------------------------------------------------
## Isolated water cells
##
## Water cells not 4-connected (rook adjacency -- Ecospace only moves biomass
## between cells sharing a side) to the main Bay water body are dead pockets in
## the model. Every isolated patch is connected back to the main body with
## bridge cells forming a side-contiguous path, EXCEPT the patches listed in
## exclude_patches (different waterbody -> remove instead).
##
## Bridge paths come from a least-cost search (multi-source Dijkstra from the
## main body, rook moves): crossing existing water is ~free, adding a land cell
## costs 1 + (1 - true water fraction), so paths follow real channels where the
## high-res data shows them and add as few cells as possible.

## Patch ids (see patch map / CSV) that belong to a different waterbody
## (upper Delaware / C&D corner): remove rather than connect
exclude_patches <- c(2, 4)

## Patches on the F02 basemap (NA = land is skipped automatically)
patch_f02  <- patches(basemap, directions = 4)
patch_vals <- values(patch_f02, mat = FALSE)
patch_tab  <- sort(table(patch_vals), decreasing = TRUE)
main_id    <- as.integer(names(patch_tab)[1])

cat("\nF02 connectivity (rook):", length(patch_tab), "water component(s);",
    "main body =", patch_tab[1], "of", sum(patch_tab), "water cells\n")

## True water components at 15 arc-sec
patch_hi      <- patches(ifel(water_hi == 1, 1, NA), directions = 4)
patch_hi_vals <- values(patch_hi, mat = FALSE)
hi_tab        <- sort(table(patch_hi_vals), decreasing = TRUE)
main_hi_id    <- as.integer(names(hi_tab)[1])

## Dominant high-res component under a set of F02 cells
dominant_hi_component <- function(cells) {
  xy_c <- xyFromCell(basemap, cells)
  ids  <- terra::extract(patch_hi, xy_c)[, 1]
  ids  <- ids[!is.na(ids)]
  if (length(ids) == 0) return(NA_integer_)
  as.integer(names(sort(table(ids), decreasing = TRUE))[1])
}

main_cells <- which(!is.na(patch_vals) & patch_vals == main_id)

iso_ids <- setdiff(as.integer(names(patch_tab)), main_id)

## ---- Least-cost paths from the main body (multi-source Dijkstra, rook moves)
## Cost to ENTER a cell: existing water ~free; land = 1 + (1 - water fraction),
## so an added bridge cell is preferred where the high-res data shows water.
n_cell <- ncell(basemap)
n_colb <- ncol(basemap)
cell_r <- rowFromCell(basemap, 1:n_cell)
cell_c <- colFromCell(basemap, 1:n_cell)

wf <- ifelse(is.na(frac_vals), 0, pmin(pmax(frac_vals, 0), 1))
enter_cost <- ifelse(!is.na(bm_vals), 0.001, 1 + (1 - wf))

dist_v <- rep(Inf, n_cell)
prev_v <- rep(NA_integer_, n_cell)
done_v <- rep(FALSE, n_cell)
dist_v[main_cells] <- 0

repeat {
  d_active <- ifelse(done_v, Inf, dist_v)
  u <- which.min(d_active)
  if (is.infinite(d_active[u])) break
  done_v[u] <- TRUE
  nbrs <- c(if (cell_r[u] > 1)              u - n_colb,
            if (cell_r[u] < nrow(basemap))  u + n_colb,
            if (cell_c[u] > 1)              u - 1,
            if (cell_c[u] < n_colb)         u + 1)
  for (v in nbrs) {
    nd <- dist_v[u] + enter_cost[v]
    if (nd < dist_v[v]) { dist_v[v] <- nd; prev_v[v] <- u }
  }
}

## Walk a cell's least-cost path back to the main body
trace_path <- function(cell) {
  path <- cell
  u <- cell
  while (!is.na(prev_v[u])) { u <- prev_v[u]; path <- c(path, u) }
  path
}

iso_summary <- data.frame()
bridge_all  <- data.frame()

for (pid in iso_ids) {

  p_cells <- which(!is.na(patch_vals) & patch_vals == pid)
  p_xy    <- xyFromCell(basemap, p_cells)

  ## Reality check (informational): same high-res water component as the Bay?
  hi_id     <- dominant_hi_component(p_cells)
  hi_linked <- !is.na(hi_id) && hi_id == main_hi_id

  bridge <- data.frame()
  if (pid %in% exclude_patches) {
    action <- "remove (other waterbody)"
  } else {
    action <- "connect"
    ## Bridge = land cells on the least-cost path from the best entry cell
    entry   <- p_cells[which.min(dist_v[p_cells])]
    path    <- trace_path(entry)
    b_cells <- path[is.na(bm_vals[path])]
    if (length(b_cells) > 0) {
      bridge <- data.frame(
        patch      = pid,
        row        = cell_r[b_cells],
        col        = cell_c[b_cells],
        depth_sug  = round(dsug_vals[b_cells], 2),
        water_frac = round(frac_vals[b_cells], 3)
      )
      bridge_all <- rbind(bridge_all, bridge)
    }
  }

  iso_summary <- rbind(iso_summary, data.frame(
    patch          = pid,
    n_cells        = length(p_cells),
    rows           = paste(cell_r[p_cells], collapse = ";"),
    cols           = paste(cell_c[p_cells], collapse = ";"),
    lon            = round(mean(p_xy[, 1]), 4),
    lat            = round(mean(p_xy[, 2]), 4),
    mean_wfrac     = round(mean(frac_vals[p_cells]), 3),
    path_cost      = round(min(dist_v[p_cells]), 2),
    hi_res_linked  = hi_linked,
    action         = action,
    n_bridge_cells = nrow(bridge)
  ))
}

## Deduplicate bridge cells shared by several patches (e.g. a chain of patches
## up one tributary reuses the same trunk)
if (nrow(bridge_all) > 0) {
  bridge_cells_u <- unique(cellFromRowCol(basemap, bridge_all$row, bridge_all$col))
  bridge_prop <- data.frame(
    row        = cell_r[bridge_cells_u],
    col        = cell_c[bridge_cells_u],
    depth_sug  = round(dsug_vals[bridge_cells_u], 2),
    water_frac = round(frac_vals[bridge_cells_u], 3),
    serves     = sapply(bridge_cells_u, function(cc) {
      paste(sort(unique(bridge_all$patch[
        cellFromRowCol(basemap, bridge_all$row, bridge_all$col) == cc])),
        collapse = ";")
    })
  )
  ## Depth fallback where the high-res grid has no water in the cell:
  ## mean depth of rook-adjacent water cells, else 1 m
  na_d <- which(is.na(bridge_prop$depth_sug))
  for (i in na_d) {
    cc   <- bridge_cells_u[i]
    nbrs <- c(if (cell_r[cc] > 1)             cc - n_colb,
              if (cell_r[cc] < nrow(basemap)) cc + n_colb,
              if (cell_c[cc] > 1)             cc - 1,
              if (cell_c[cc] < n_colb)        cc + 1)
    nb_d <- bm_vals[nbrs]
    bridge_prop$depth_sug[i] <-
      if (any(!is.na(nb_d))) round(mean(nb_d, na.rm = TRUE), 2) else 1
  }
} else {
  bridge_prop <- data.frame(row = integer(0), col = integer(0),
                            depth_sug = numeric(0), water_frac = numeric(0),
                            serves = character(0))
}

## ---- Manual overrides to the auto-proposal (from review):
## - Patch 26 belongs to the Rappahannock, not the Potomac: drop the col-9
##   chain running north and connect east into patch 27 (-> 28 -> main body)
## - Patch 3's bridge moved one cell south + east: (4,44) -> (5,45)
bridge_drop <- data.frame(
  row = c(39, 40, 41, 42, 43, 4),
  col = c( 9,  9,  9,  9,  9, 44)
)
bridge_manual <- data.frame(
  row    = c(44, 45,  5),
  col    = c(10, 10, 45),
  serves = c("26", "26", "3")
)

## Uniform depth assigned to every bridge cell (m)
bridge_depth_m <- 1

if (nrow(bridge_drop) > 0 && nrow(bridge_prop) > 0) {
  keep <- !(paste(bridge_prop$row, bridge_prop$col) %in%
              paste(bridge_drop$row, bridge_drop$col))
  bridge_prop <- bridge_prop[keep, ]
}
if (nrow(bridge_manual) > 0) {
  man_cells <- cellFromRowCol(basemap, bridge_manual$row, bridge_manual$col)
  bridge_prop <- rbind(bridge_prop, data.frame(
    row        = bridge_manual$row,
    col        = bridge_manual$col,
    depth_sug  = round(dsug_vals[man_cells], 2),
    water_frac = round(frac_vals[man_cells], 3),
    serves     = bridge_manual$serves
  ))
}
bridge_prop$depth <- bridge_depth_m

## -----------------------------------------------------------------------------
## Ocean clip: the model domain ends at the Bay mouth (~ -76.0 deg lon,
## Cape Henry to Cape Charles). All water outside the mouth -- the Atlantic
## corner and the seaside of the Delmarva Peninsula -- becomes land.
## (Land is NA/-9999 in these basemap files; EwE reads NODATA and 0 alike
## as land, matching the depth-map-export.asc convention.)

## Working map: proposal so far (bridges added, other-waterbody cells removed)
excl_cells <- which(!is.na(patch_vals) & patch_vals %in% exclude_patches)
clip_vals  <- bm_vals
if (nrow(bridge_prop) > 0)
  clip_vals[cellFromRowCol(basemap, bridge_prop$row, bridge_prop$col)] <-
    bridge_prop$depth
clip_vals[excl_cells] <- NA

## Mouth barrier: water cells in the column containing -76.0 lon, south of
## Cape Charles (lat < 37.3). Barrier cells are Bay-side and are kept; they
## only stop the ocean flood-fill from entering the Bay.
mouth_col  <- colFromX(basemap, -76.0)
cell_y     <- yFromCell(basemap, 1:n_cell)
barrier    <- which(cell_c == mouth_col & cell_y < 37.3 & !is.na(clip_vals))

flood_vals <- clip_vals
flood_vals[barrier] <- NA

## Flood from an ocean seed (the SE-most water cell) across the barrier-masked
## map: every cell it reaches is Atlantic-side
flood_p <- patches(setValues(rast(basemap), flood_vals), directions = 4)
fp_vals <- values(flood_p, mat = FALSE)
wet     <- which(!is.na(flood_vals))
seed    <- wet[order(-cell_r[wet], -cell_c[wet])][1]
ocean_cells <- which(!is.na(fp_vals) & fp_vals == fp_vals[seed])

## Seaside pockets connected only through the ocean drop out of the main
## component once the ocean is removed -- they are outside the Bay as well
after_vals <- clip_vals
after_vals[ocean_cells] <- NA
ap_vals <- values(patches(setValues(rast(basemap), after_vals),
                          directions = 4), mat = FALSE)
ap_tab  <- sort(table(ap_vals), decreasing = TRUE)
pocket_cells <- which(!is.na(ap_vals) &
                        ap_vals != as.integer(names(ap_tab)[1]))

outside_cells <- sort(unique(c(ocean_cells, pocket_cells)))

ocean_prop <- data.frame(
  row      = cell_r[outside_cells],
  col      = cell_c[outside_cells],
  lon      = round(xyFromCell(basemap, outside_cells)[, 1], 4),
  lat      = round(xyFromCell(basemap, outside_cells)[, 2], 4),
  depth_bm = round(bm_vals[outside_cells], 2)
)
cat("\nOcean clip at the Bay mouth (col", mouth_col, "= lon -76.0):",
    length(ocean_cells), "Atlantic-side cells +", length(pocket_cells),
    "seaside pocket cells =", length(outside_cells), "cells -> land\n")
write.csv(ocean_prop, "./make-habitat-maps/basemap-ocean-cells-F02.csv",
          row.names = FALSE)

## ---- Dry-run check: proposal must leave exactly one water body
prop_vals <- clip_vals
prop_vals[outside_cells] <- NA

prop_check <- setValues(rast(basemap), prop_vals)
prop_tab   <- table(values(patches(prop_check, directions = 4), mat = FALSE))
cat("\nDry-run of the proposal (", nrow(bridge_prop), "bridge cells added,",
    length(excl_cells), "cells removed,", length(outside_cells),
    "ocean cells clipped ):", length(prop_tab), "water component(s),",
    sum(!is.na(prop_vals)), "water cells\n")
if (length(prop_tab) > 1)
  warning("Proposal does NOT fully connect the basemap -- inspect bridge paths.")

if (nrow(iso_summary) > 0) {

  cat("\nIsolated water patches (not connected to the main Bay):\n")
  print(iso_summary, row.names = FALSE)
  if (nrow(bridge_prop) > 0) {
    cat("\nProposed bridge cells (land -> water; deduplicated across patches):\n")
    print(bridge_prop, row.names = FALSE)
  }

  write.csv(iso_summary,
            "./make-habitat-maps/basemap-isolated-patches-F02.csv",
            row.names = FALSE)
  write.csv(bridge_prop,
            "./make-habitat-maps/basemap-bridge-cells-F02.csv",
            row.names = FALSE)

  ## Patch map: main body muted, isolated patches highlighted + labeled,
  ## proposed bridge cells outlined
  plot_patches <- function() {
    main_col <- adjustcolor("steelblue", 0.35)
    iso_cols <- rep(c("red3", "orange", "green3", "magenta", "cyan3",
                      "purple", "gold"), length.out = length(iso_ids))
    r_show <- ifel(patch_f02 == main_id, 0, patch_f02)
    plot(r_show, col = c("0" = main_col), colNA = "gray75", legend = FALSE,
         main = "Isolated water patches -- F02 basemap",
         mar = c(2.5, 2.5, 2.5, 2.5))
    lines(waterline, col = adjustcolor("red", 0.5), lwd = 0.4)
    ## Patch id labels are clamped inside the extent; no xpd (see note in
    ## plot_review -- clip-region toggles break the following legend)
    for (k in seq_along(iso_ids)) {
      pid     <- iso_ids[k]
      p_cells <- which(!is.na(patch_vals) & patch_vals == pid)
      p_xy    <- xyFromCell(basemap, p_cells)
      points(p_xy[, 1], p_xy[, 2], pch = 15, col = iso_cols[k], cex = 0.9)
      lab_x <- min(max(mean(p_xy[, 1]), xmin(basemap) + res(basemap)[1]),
                   xmax(basemap) - res(basemap)[1])
      lab_y <- min(max(mean(p_xy[, 2]) + 1.2 * res(basemap)[2],
                       ymin(basemap) + res(basemap)[2]),
                   ymax(basemap) - 0.6 * res(basemap)[2])
      text(lab_x, lab_y, labels = pid, col = iso_cols[k], cex = 0.8, font = 2)
    }
    if (nrow(bridge_prop) > 0) {
      b_xy <- xyFromCell(basemap,
                         cellFromRowCol(basemap, bridge_prop$row, bridge_prop$col))
      points(b_xy[, 1], b_xy[, 2], pch = 0, col = "black", cex = 1.0, lwd = 1.4)
    }
    ## Cells slated for removal (different waterbody): gray X
    if (length(excl_cells) > 0) {
      e_xy <- xyFromCell(basemap, excl_cells)
      points(e_xy[, 1], e_xy[, 2], pch = 4, col = "gray20", cex = 0.9, lwd = 1.6)
    }
    ## Ocean cells outside the Bay mouth: darkened
    if (length(outside_cells) > 0) {
      o_xy <- xyFromCell(basemap, outside_cells)
      points(o_xy[, 1], o_xy[, 2], pch = 15,
             col = adjustcolor("gray20", 0.35), cex = 0.9)
    }
    legend(x = xmin(basemap) + 0.02 * (xmax(basemap) - xmin(basemap)),
           y = ymax(basemap) - 0.02 * (ymax(basemap) - ymin(basemap)),
           yjust = 1,
           legend = c("main water body", "isolated patch (id labeled)",
                      "proposed bridge cell", "remove (other waterbody)",
                      "outside Bay mouth -> land",
                      "waterline (ETOPO 15 arc-sec)"),
           col    = c("black", "black", "black", "gray20",
                      adjustcolor("gray20", 0.5), adjustcolor("red", 0.5)),
           pt.bg  = c(main_col, "red3", NA, NA, adjustcolor("gray20", 0.35), NA),
           pch    = c(22, 22, 0, 4, 22, NA),
           lty    = c(NA, NA, NA, NA, NA, 1),
           pt.cex = 1.1,
           cex = 0.55, bg = "white")
  }

  if (interactive()) {
    windows()
    plot_patches()
  }
  if (write_png) {
    safe_png(file.path(fig_dir, "basemap-isolated-patches-F02.png"),
             plot_patches(), width = 6, height = 8, res = 1200)
  }

} else {
  cat("\nNo isolated water patches: the basemap is fully 4-connected.\n")
}

## =============================================================================
## APPLY REVISIONS
##
## The revision starts from the reviewed proposal above (bridge cells added at
## bridge_depth_m, other-waterbody patches removed, ocean outside the Bay
## mouth clipped -- exactly the dry-run-checked prop_vals). The manual tables
## below are a hook for additional one-off edits on top of the proposal.
##
## row/col follow the labels on the review plots (row 1 = north, col 1 = west);
## these match the printed QA table above.
## =============================================================================

## Water cells to convert to land (set NA)
cells_to_remove <- data.frame(
  row = integer(0),
  col = integer(0)
)

## Land cells to convert to water. depth = NA means "use the mean high-res
## depth of that cell" (depth_sug in the QA table).
cells_to_add <- data.frame(
  row   = integer(0),
  col   = integer(0),
  depth = numeric(0)
)

if (apply_revisions) {

  ## Start from the reviewed, dry-run-checked proposal
  basemap_rev <- setValues(rast(basemap), prop_vals)
  names(basemap_rev) <- names(basemap)

  ## Remove water cells
  if (nrow(cells_to_remove) > 0) {
    rm_cells <- cellFromRowCol(basemap_rev,
                               cells_to_remove$row, cells_to_remove$col)
    basemap_rev[rm_cells] <- NA
  }

  ## Add water cells
  if (nrow(cells_to_add) > 0) {
    add_cells <- cellFromRowCol(basemap_rev,
                                cells_to_add$row, cells_to_add$col)
    add_depth <- cells_to_add$depth
    use_sug   <- is.na(add_depth)
    add_depth[use_sug] <- dsug_vals[add_cells[use_sug]]
    if (anyNA(add_depth))
      stop("No suggested depth available for some cells_to_add; ",
           "specify depth explicitly for rows: ",
           paste(which(is.na(add_depth)), collapse = ", "))
    basemap_rev[add_cells] <- add_depth
  }

  ## Before / after check
  if (interactive()) windows()
  par(mfrow = c(1, 2))
  plot(basemap,     colNA = "gray75", main = "Original F02 basemap")
  lines(waterline, col = "red", lwd = 0.5)
  plot(basemap_rev, colNA = "gray75", main = "Revised F02 basemap")
  lines(waterline, col = "red", lwd = 0.5)
  par(mfrow = c(1, 1))

  cat("\nWater cells: original =", sum(!is.na(values(basemap))),
      "| revised =", sum(!is.na(values(basemap_rev))), "\n")

  ## Post-revision connectivity check: edits (esp. removals) must not orphan
  ## any water cells from the main body
  patch_rev      <- patches(basemap_rev, directions = 4)
  patch_rev_vals <- values(patch_rev, mat = FALSE)
  rev_tab        <- sort(table(patch_rev_vals), decreasing = TRUE)
  if (length(rev_tab) > 1) {
    orphan_ids   <- as.integer(names(rev_tab)[-1])
    orphan_cells <- which(!is.na(patch_rev_vals) & patch_rev_vals %in% orphan_ids)
    warning("Revised basemap has ", length(orphan_ids),
            " isolated water patch(es) (", length(orphan_cells),
            " cells) -- review before using in Ecospace.")
    cat("\nWARNING: isolated water cells remain in the revised basemap:\n")
    print(data.frame(
      patch = patch_rev_vals[orphan_cells],
      row   = rowFromCell(basemap_rev, orphan_cells),
      col   = colFromCell(basemap_rev, orphan_cells)
    ), row.names = FALSE)
  } else {
    cat("Connectivity check passed: revised basemap is one water body.\n")
  }

  ## Areal coverage of water: sum of true ellipsoidal cell areas (WGS84) over
  ## water cells, via terra::cellSize(). In this lon/lat grid the cell area
  ## varies with latitude (~10.9 km2 in the south to ~10.5 km2 in the north).
  cell_area_km2 <- cellSize(basemap_rev, unit = "km")
  water_area    <- global(ifel(!is.na(basemap_rev), cell_area_km2, NA),
                          "sum", na.rm = TRUE)[1, 1]
  total_area    <- global(cell_area_km2, "sum")[1, 1]
  orig_area     <- global(ifel(!is.na(basemap), cell_area_km2, NA),
                          "sum", na.rm = TRUE)[1, 1]
  cat(sprintf(paste0(
    "\nAreal coverage (WGS84 ellipsoidal cell areas):\n",
    "  water cells: %d of %d (%.1f%% of grid)\n",
    "  water area:  %.0f km2 (grid total %.0f km2; original basemap %.0f km2)\n",
    "  mean water-cell area: %.2f km2\n"),
    sum(!is.na(values(basemap_rev))), ncell(basemap_rev),
    100 * sum(!is.na(values(basemap_rev))) / ncell(basemap_rev),
    water_area, total_area, orig_area,
    water_area / sum(!is.na(values(basemap_rev)))))

  ## Write out
  out_name <- if (overwrite_original) {
    "base-depth-map-F02-88x56.asc"
  } else {
    "base-depth-map-F02-88x56-revised.asc"
  }

  writeRaster(
    basemap_rev,
    filename  = file.path(dir_basemap, out_name),
    overwrite = TRUE,
    NAflag    = -9999
  )
  cat("Wrote", file.path(dir_basemap, out_name), "\n")

  if (write_png) {
    safe_png(file.path(fig_dir, "basemap-revised-F02.png"), {
      par(mfrow = c(1, 2))
      plot(basemap,     colNA = "gray75", main = "Original F02 basemap")
      lines(waterline, col = "red", lwd = 0.5)
      plot(basemap_rev, colNA = "gray75", main = "Revised F02 basemap")
      lines(waterline, col = "red", lwd = 0.5)
      par(mfrow = c(1, 1))
    }, width = 10, height = 8, res = 1000)
  }
}
