# Chesapeake-Ecospace

R project to build spatial, environmental, and species preference inputs for a Chesapeake Bay ecosystem model in **Ecopath with Ecosim (EwE) / Ecospace**. Outputs are ESRI ASCII grids, NetCDF raster stacks, and CSV preference tables ready for import into Ecospace.

Ecospace grids: four resolutions sharing one extent (Chesapeake Bay, −77.4 to −75.55°W, 36.7 to 39.65°N, WGS84). The standard grid is **F02 = 88×56** (56 columns × 88 rows, ~2-arcminute / ~3.5 km cells), with a finer option (F01 176×111) and coarser ones (F03 59×37, F04 44×28).

## Quick Start

Most scripts are independent entry points. The **environmental-driver module** is the exception: it is a 3-stage pipeline whose stage scripts share `cbefs-helpers.R` and are driven by an orchestrator (`run-environmental-drivers.R`). Run pipelines in order; each can run independently once its prerequisites exist.

### Pipeline A — Spatial Basemap (run once)
1. `make-habitat-maps/make-baythymetry-basemap.R` — downloads NOAA bathymetry, creates the Ecospace depth basemaps in `output-for-ecospace/habitat/basemaps/` (`base-depth-map-F01..F04-*.asc`)
2. `make-habitat-maps/make-jurisdictional-maps.R` — creates binary MD/VA/Potomac ASC grids (requires basemap from step 1)

### Pipeline B — Environmental Drivers (3-stage, long-running, requires ~43 GB CBEFS data)
A 3-stage pipeline that turns CBEFS hindcasts (1985–2024) into Ecospace drivers at all four resolutions. **See [`make-environmental-drivers/README.md`](make-environmental-drivers/README.md) for the full technical reference**, and [`docs/environmental-drivers-methods.md`](docs/environmental-drivers-methods.md) for the methods/rationale.

3. **Stage 1 — `make-environmental-drivers/process-CBEFS.R`** (run separately): raw CBEFS → native monthly-mean NC stacks (monthly computed in-process; native curvilinear grid, index space). `run_mode <- "TEST"` for a quick trial; `write_daily_stack <- FALSE` skips the ~47 GB daily archive (unused downstream).
4. **Stages 2–3 — `make-environmental-drivers/run-environmental-drivers.R`** (orchestrator): toggle `do_regrid` (Stage 2: regrid native → each basemap), `do_ascii` (Stage 3a: ASCII drivers), `do_pdf` / `do_gif` (Stage 3b/c: PDF/GIF visualizations). Select `resolutions` and `variables_to_run` (`NULL` = all).
5. *(independent)* `make-environmental-drivers/make-atlas-climatology-maps.R` — Bay Atlas climatology → monthly ASC files (separate from the 3-stage pipeline).

### Pipeline C — Species Preference Functions
6. `make-preference-functions/query-env-preference-parameters.R` — queries FishBase API for species traits
7. `make-preference-functions/Make-preference-functions.R` — compiles `data/derived/env-pref-parameters.csv` → logistic preference curves

> **Note:** `make-preference-functions/query-aquamaps-data.R` (AquaMaps HSPEN query) needs rework before use — see [docs/INVENTORY.md](docs/INVENTORY.md#6-known-issues).

## Full Documentation

See **[docs/INVENTORY.md](docs/INVENTORY.md)** for:
- Complete R script inventory with inputs, outputs, and run order
- ASCII pipeline diagrams
- All final Ecospace output file paths
- Known issues and cleanup items
