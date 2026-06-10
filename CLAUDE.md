# Chesapeake-Ecospace — Claude Code Orientation

## What This Project Is

This is the **Chesapeake Bay Blue Catfish Ecospace Project**. The scientific goal is to use a spatially explicit, ecosystem-scale Ecospace model to evaluate stakeholder-preferred Blue Catfish harvest strategies and their ecological and economic trade-offs across the Chesapeake Bay.

This repository prepares all spatial and environmental model inputs. All final outputs land in `output-for-ecospace/` as ESRI ASCII grids (`.asc`), NetCDF stacks, or CSV tables ready for the EwE/Ecospace software.

**Ecospace grid spec:** 88 columns × 56 rows, ~2-arcminute (~3.5 km) cells, EPSG:4326, bounding box −77.4 to −75.55°W / 36.7 to 39.65°N.

---

## Script Execution Order

All scripts are standalone (no `source()` dependencies). Run in this order:

| Step | Script | Key user flags |
|------|--------|----------------|
| 1 | `make-habitat-maps/make-baythymetry-basemap.R` | None — downloads live from NOAA |
| 2 | `make-habitat-maps/make-jurisdictional-maps.R` | `pot_buffer_m` (default 7200 m) |
| 3 | `make-environmental-drivers/make-climatology-maps.R` | None |
| 4 | `make-environmental-drivers/process-CBEFS.R` | `run_mode <- "TEST"\|"FULL"`, `out_format <- "NC"\|"TIFF"\|"BOTH"`, `write_daily_stack`, `write_monthly_stack` |
| 5 | `make-environmental-drivers/make-ecospace-ascii-drivers.R` | `write_series`, `write_climatology` |
| 6 | `make-preference-functions/query-aquamaps-data.R` | None — requires internet (FishBase API) |
| 7 | `make-preference-functions/Make-preference-functions.R` | None |

Optional visualization:
- `make-environmental-drivers/make-gif-videos.R` — configure `gif_prefixes` and `start_year`/`num_years` (per-variable styling lives in `cbefs-helpers.R::cbefs_var_styles`)
- `make-environmental-drivers/make-monthly-maps.R` — single-variable QC pipeline (bottom salinity)

---

## Ecospace Functional Groups

The target Ecospace FG list, including life-stage splits. Animal/fish groups have entries in `data/raw/species-list.csv` and stage-specific preference parameters in `data/derived/env-pref-parameters.csv`. Lower trophic groups (marked †) are not yet in either file.

| Functional Group | Status |
|-----------------|--------|
| Blue Catfish 0–1 yr | In `env-pref-parameters.csv` |
| Blue Catfish 1-harvest | In `env-pref-parameters.csv` |
| Blue Catfish trophy | In `env-pref-parameters.csv` |
| Striped Bass (prey-sized) | In `species-list.csv` as "Striped Bass" |
| Striped Bass (harvest-sized) | Not yet split by stage |
| Striped Bass (trophy) | Not yet split by stage |
| Sturgeon 0–1 yr | In `species-list.csv` as "Sturgeon" |
| Sturgeon 1+ | Not yet split by stage |
| Alosines | In `species-list.csv` |
| Blue Crab juveniles | In `species-list.csv` as "Blue Crab" |
| Blue Crab sublegals | Not yet split by stage |
| Blue Crab legals | Not yet split by stage |
| Sciaenids | In `species-list.csv` |
| Menhaden | In `species-list.csv` |
| Freshwater Demersals | In `species-list.csv` |
| Other Forage | In `species-list.csv` |
| Bivalves † | Not yet in species files |
| Benthic Invertebrates † | Not yet in species files |
| SAV † | Not yet in species files |
| Zooplankton † | Not yet in species files |
| Phytoplankton † | Not yet in species files |
| Detritus † | Not yet in species files |

---

## Key Naming Conventions

**Depth bands** (used in all CBEFS variable names):
- `_bott` = bottom layer
- `_surf` = surface layer
- `_davg` = depth-averaged

**CBEFS variable names:** `temperature`, `salinity`, `diss_o2`, `phytoplankton`, `NO3`

**Output file name pattern:** `<variable>_<depth>_<year_start>_<year_end>.<ext>`
Example: `temperature_bott_1985_2024.nc`

---

## Critical Files — Do Not Modify

| File | Why |
|------|-----|
| `output-for-ecospace/habitat/base-depth-map-88x56.asc` | The Ecospace basemap — all raster work reprojects to this grid |
| `data-inputs/species-info/species-list.csv` | Master species-level FG list used by preference-function scripts |
| `data-inputs/env-preference-functions/env-pref-parameters.csv` | Manually curated preference parameters (AquaMaps HSPEN Dec 2025 + literature) |
| `data-inputs/spatial-dynamic/CBEFS-hindcast/*.nc` | 40 raw hindcast files, ~43 GB total — gitignored, do not move |

---

## CBEFS CRS / Alignment Caveat

CBEFS hindcast data is on an **oblique stereographic projection** (+proj=stere +lon_0=283.54 +lat_0=37.75), 336×564 grid, ~600 m cell resolution. `process-CBEFS.R` keeps its stacks in the native model grid (no CRS) on purpose; georeferencing to the Ecospace basemap (88×56, EPSG:4326) happens once downstream in `make-ecospace-ascii-drivers.R`, which regrids via the stored lon/lat arrays (`cbefs-helpers.R::build_regrid_index`).

---

## Missing Habitat Layer Scripts

Five planned static habitat layers have no scripts yet:

| Layer | Data source needed |
|-------|--------------------|
| SAV (submerged aquatic vegetation) | VIMS/CBNERR SAV survey data |
| Soft-bottom substrate | NOAA or state benthic survey |
| Hard-bottom / structured habitat | NOAA or state benthic survey |
| Oyster / bivalve reef | MD/VA oyster reef GIS layers |
| Marsh / emergent vegetation | NOAA Coastal Change Analysis Program (C-CAP) or NWI |

Each will need to: read source data → resample to basemap → write `.asc` + PNG QA map to `output-for-ecospace/`.

---

## Known Issues Still Open

1. **`query-aquamaps-data.R`** — references undefined `fg` variable and writes to `./global-data/` path that does not exist; needs repair before re-running AquaMaps HSPEN queries
2. **Preference functions** — only salinity is implemented in `Make-preference-functions.R`; temperature, DO, depth, and NO₃ curves still needed
3. **CBEFS CRS/alignment** — see section above; env-driver stacks not yet reprojected to basemap grid
4. **Missing lower trophic FGs** — Bivalves, Benthic Invertebrates, SAV, Zooplankton, Phytoplankton, Detritus not yet in `species-list.csv` or `env-pref-parameters.csv`
5. **Missing habitat layer scripts** — SAV, soft-bottom, hard-bottom, oyster, marsh
6. **`dispersal-rates/`** — empty placeholder directory

---

## Large Data Files (Not in Git)

- `data-inputs/spatial-dynamic/CBEFS-hindcast/*.nc` — 40 files, ~1.1 GB each → gitignored via `*.nc`
- `data-inputs/spatial-dynamic/CBEFS-climatology/ches-clim-atlas-vims.nc` — Bay Atlas climatology → gitignored via `*.nc`
- `make-environmental-drivers/GIFs/` — generated animations → gitignored

---

## R Packages Required

`terra`, `ncdf4`, `marmap`, `gifski`, `viridisLite`, `dplyr`, `tidyr`, `stringr`, `tools`, `rfishbase`, `aquamapsdata` (for AquaMaps queries)
