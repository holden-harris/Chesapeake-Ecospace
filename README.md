# Chesapeake-Ecospace

R project to build spatial, environmental, and species preference inputs for a Chesapeake Bay ecosystem model in **Ecopath with Ecosim (EwE) / Ecospace**. Outputs are ESRI ASCII grids, NetCDF raster stacks, and CSV preference tables ready for import into Ecospace.

Ecospace grid: **88 columns × 56 rows**, ~2-arcminute resolution, covering the Chesapeake Bay (−77.4 to −75.55°W, 36.7 to 39.65°N).

## Quick Start

Scripts are independent entry points (no `source()` calls). Run them in pipeline order; each pipeline can be run independently once its prerequisites exist.

### Pipeline A — Spatial Basemap (run once)
1. `habitat/make-baythymetry-basemap.R` — downloads NOAA bathymetry, creates `output-for-ecospace/habitat/base-depth-map-88x56.asc`
2. `habitat/make-jurisdictional-maps.R` — creates binary MD/VA/Potomac ASC grids (requires basemap from step 1)

### Pipeline B — Environmental Drivers (long-running, requires ~43 GB CBEFS data)
3. `environmental-drivers/make-climatology-maps.R` — Bay Atlas climatology → monthly ASC files (5 variables × 12 months)
4. `environmental-drivers/process-CBEFS.R` — CBEFS hindcasts (1985–2024) → daily NC/TIFF stacks (set `run_mode <- "TEST"` for a 4-year trial run)
5. `environmental-drivers/aggregate-daily-stacks-to-monthly.R` — daily NC stacks → monthly mean NC stacks

### Pipeline C — Species Preference Functions
6. `preference-functions/query-env-preference-parameters.R` — queries FishBase API for species traits
7. `preference-functions/Make-preference-functions.R` — compiles `data/derived/env-pref-parameters.csv` → logistic preference curves

> **Note:** `preference-functions/query-aquamaps-data.R` (AquaMaps HSPEN query) needs rework before use — see [docs/INVENTORY.md](docs/INVENTORY.md#6-known-issues).

## Full Documentation

See **[docs/INVENTORY.md](docs/INVENTORY.md)** for:
- Complete R script inventory with inputs, outputs, and run order
- ASCII pipeline diagrams
- All final Ecospace output file paths
- Known issues and cleanup items
