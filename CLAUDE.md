# Chesapeake-Ecospace — Claude Code Orientation

## What This Project Is

R project that prepares spatial and environmental inputs for a Chesapeake Bay Ecopath/Ecosim/Ecospace model. All final outputs land in `output-for-ecospace/` as ESRI ASCII grids (`.asc`), NetCDF stacks, or CSV tables.

**Ecospace grid spec:** 88 columns × 56 rows, ~2-arcminute (~3.5 km) cells, EPSG:4326, bounding box −77.4 to −75.55°W / 36.7 to 39.65°N.

## Script Execution Order

All scripts are standalone (no `source()` dependencies). Run in this order:

| Step | Script | Key user flags |
|------|--------|----------------|
| 1 | `habitat/make-baythymetry-basemap.R` | None — downloads live from NOAA |
| 2 | `habitat/make-jurisdictional-maps.R` | `pot_buffer_m` (default 7200 m) |
| 3 | `environmental-drivers/make-climatology-maps.R` | None |
| 4 | `environmental-drivers/process-CBEFS.R` | `run_mode <- "TEST"\|"FULL"`, `out_format <- "NC"\|"TIFF"\|"BOTH"` |
| 5 | `environmental-drivers/aggregate-daily-stacks-to-monthly.R` | `overwrite_monthly`, `start_year`/`end_year` |
| 6 | `preference-functions/query-env-preference-parameters.R` | None — requires internet |
| 7 | `preference-functions/Make-preference-functions.R` | None |

Optional visualization:
- `environmental-drivers/make-gif-videos.R` — configure `file_settings` table and `start_year`/`num_years`
- `environmental-drivers/make-monthly-maps.R` — single-variable QC pipeline (bottom salinity)

## Key Naming Conventions

**Depth bands** (used in all CBEFS variable names):
- `_bott` = bottom layer
- `_surf` = surface layer
- `_davg` = depth-averaged

**CBEFS variable names:** `temperature`, `salinity`, `diss_o2`, `phytoplankton`, `NO3`

**Output file name pattern:** `<variable>_<depth>_<year_start>_<year_end>.<ext>`
Example: `temperature_bott_1985_2024.nc`

**Functional group (FG) IDs** come from `data/raw/species-list.csv` (column `FG`). There are ~12 FGs including: Blue Catfish, Striped Bass, Sturgeon, Alosines, Blue Crab, Sciaenids, Menhaden, Freshwater Demersal, Other Forage.

## Critical Files — Do Not Modify

| File | Why |
|------|-----|
| `output-for-ecospace/habitat/base-depth-map-88x56.asc` | The Ecospace basemap — all raster work reprojects to this grid |
| `data/raw/species-list.csv` | Master FG list used by preference-function scripts |
| `data/derived/env-pref-parameters.csv` | Manually curated preference parameters (AquaMaps HSPEN + literature) — re-derivation requires rerunning `query-aquamaps-data.R` (currently broken) |
| `data/raw/CBEFS-hindcast/*.nc` | 40 raw hindcast files, ~43 GB total — gitignored, do not move |

## Known Issues to Address

1. **`process-CBEFS-hindcasts.R`** — duplicate/broken draft of `process-CBEFS.R`; candidate for deletion
2. **`query-aquamaps-data.R`** — references undefined `fg` variable and wrong output path `./global-data/`; needs repair before re-running
3. **GIF/PNG outputs** — `environmental-drivers/GIFs/` should be gitignored (large binary media, regenerable from scripts)
4. **`make-gif-videos.R`** — has hardcoded `zlim = c(0,80)` and `windows()` call that breaks on non-Windows
5. **Preference functions** — only salinity is implemented in `Make-preference-functions.R`; temperature, DO, depth, NO₃ still needed
6. **`dispersal-rates/`** — empty placeholder directory

## Large Data Files (Not in Git)

- `data/raw/CBEFS-hindcast/*.nc` — 40 files, ~1.1 GB each → gitignored via `*.nc`
- `data/raw/ches-clim-atlas-vims.nc` — Bay Atlas climatology → gitignored via `*.nc`
- `environmental-drivers/GIFs/` — generated animations → should be gitignored

## R Packages Required

`terra`, `ncdf4`, `marmap`, `gifski`, `viridisLite`, `dplyr`, `tidyr`, `stringr`, `tools`, `rfishbase`, `aquamapsdata` (for AquaMaps queries)
