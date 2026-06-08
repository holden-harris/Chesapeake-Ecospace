# Chesapeake-Ecospace Repository Inventory

## 1. Repository Overview

This repository builds all spatial and environmental inputs required to run a Chesapeake Bay ecosystem model in **Ecopath with Ecosim (EwE) / Ecospace**. The outputs are ASCII grid files, NetCDF raster stacks, and CSV preference function tables that feed directly into the EwE Ecospace spatial module.

**Ecospace grid:** 88 columns × 56 rows, ~2-arcminute resolution (~3.5 km × 3.7 km cells), covering the Chesapeake Bay bounding box (−77.4 to −75.55°W, 36.7 to 39.65°N), WGS84.

**Three main output categories for Ecospace:**
1. Spatial basemap — depth grid + jurisdiction layers
2. Environmental driver stacks — monthly time series (1985–2024) for 5 variables × 3 depths
3. Species preference functions — double-logistic habitat suitability curves by functional group

---

## 2. Directory Map

| Directory | Contents |
|-----------|----------|
| `data/raw/` | Raw input data: CBEFS hindcast NetCDF files (~43 GB, gitignored), jurisdiction rasters, master species list |
| `data/derived/` | Processed species trait data from FishBase API queries; manually compiled environmental preference parameter table |
| `environmental-drivers/` | R scripts for processing CBEFS hindcasts and Bay Atlas climatology; GIF animation outputs (gitignored) |
| `habitat/` | R scripts for creating the depth basemap and jurisdiction maps; PNG plot outputs |
| `preference-functions/` | R scripts for querying FishBase / AquaMaps APIs and generating logistic preference curves |
| `misc-code/` | Stand-alone example visualization scripts (not part of the main pipeline) |
| `output-for-ecospace/` | **Final outputs** — all files consumed directly by Ecospace (ASC grids, NetCDF stacks, preference CSVs) |
| `terra-temp/` | Scratch space for terra raster processing (gitignored, auto-created) |
| `dispersal-rates/` | Empty placeholder — dispersal rate workflow not yet started |

---

## 3. R Script Inventory

### habitat/

| Script | Purpose | Reads | Writes | Pipeline step |
|--------|---------|-------|--------|---------------|
| `make-baythymetry-basemap.R` | Downloads NOAA bathymetry; tests 4 resolutions; exports the 88×56 depth grid used by all other scripts | NOAA online API (`marmap::getNOAA.bathy`) | `output-for-ecospace/habitat/base-depth-map-88x56.asc`; `habitat/plots/depth-map.png` | **Step 1 — run once** |
| `make-jurisdictional-maps.R` | Creates binary (0/1) jurisdiction rasters (MD, VA, Potomac) aligned to the basemap; buffers Potomac by 7.2 km | `data/raw/jurisdictions/jurisraster.tif`; basemap ASC | `output-for-ecospace/jurisdictions/ascii/jurisdiction_{maryland,virginia,potomac}.asc`; `habitat/plots/chesapeake_bay_jurisdictions.png` | **Step 2 — run once** (requires basemap) |

### environmental-drivers/

| Script | Purpose | Reads | Writes | Pipeline step |
|--------|---------|-------|--------|---------------|
| `make-climatology-maps.R` | Reads Bay Atlas climatology NetCDF; builds 12-month stacks for 5 variables; reprojects to basemap; writes monthly ASC files | `data/raw/ches-clim-atlas-vims.nc` (gitignored); basemap ASC | `output-for-ecospace/env-drivers/ches-atlas-climatology/<var>/<var>_<Mon>.asc` (60 files); PNG 12-panel figures | **Step 3 — run once** |
| `process-CBEFS.R` | **Primary CBEFS processor.** Reads 40 yearly CBEFS NetCDF files; splits each variable into 3 depth bands (`_bott`, `_surf`, `_davg`); combines into multi-year stacks. User controls: `out_format` (TIFF/NC/BOTH), `run_mode` (TEST/FULL), `variables_to_run` | `data/raw/CBEFS-hindcast/holdenharris_YYYY_v*.nc` (40 files) | `output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-NC/<var>_<depth>_1985_2024.nc` (15 files); same in `var-stack-TIFF/` if TIFF format selected | **Step 4 — long-running** (~hours in FULL mode) |
| `aggregate-daily-stacks-to-monthly.R` | Aggregates daily NC stacks from Step 4 to monthly means using `tapp()`; writes one monthly NC per input file; logs processing metadata to CSV | `output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-NC/*.nc` | `output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-NC-monthly/<var>_<depth>_..._monthly_mean.nc`; timestamped CSV log | **Step 5** (requires Step 4 NC output) |
| `make-gif-videos.R` | Aggregates selected daily stacks to monthly TIFFs; renders GIF animations with configurable color palettes and year range. User controls: `start_year`, `num_years`, `file_settings` table, `overwrite_*` flags | `output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-NC/*.nc` | `environmental-drivers/GIFs/<var_stub>/<var>_monthly_mean_<yr>_<yr>.tif`; `<var>_xM_<yr>_<yr>.gif` + frame PNGs (not tracked in git) | **Visualization only** — optional |
| `make-monthly-maps.R` | Single-variable daily→monthly pipeline with basemap alignment; writes monthly ASC grids and a GIF. Focused on bottom salinity testing | `output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-NC/salinity_bott_*.nc`; basemap ASC | `output-for-ecospace/env-drivers/CBEFS-hindcast/bottom-salinity-test/ASCII-monthly/*.asc`; `GIF/bottom_salinity_monthly.gif` | **Visualization / QC** — single variable |
| `process-CBEFS-hindcasts.R` | Earlier exploratory/introspection version of `process-CBEFS.R`. Contains `ncdf4`-based `inspect_nc_file()` and `discover_subdatasets()` helpers. Loop is partially commented out; references undefined `test_file_1`. **Candidate for archiving/deletion** | Same as `process-CBEFS.R` | In-memory stacks only (no file output in current state) | **Deprecated draft** — do not use for production |

### preference-functions/

| Script | Purpose | Reads | Writes | Pipeline step |
|--------|---------|-------|--------|---------------|
| `query-env-preference-parameters.R` | Queries **FishBase** API (`rfishbase`) for 7 trait tables (species, growth, ecology, reproduction, stocks, oxygen tolerance, swimming speed); joins selected fields into a combined attributes table | `data/raw/species-list.csv`; FishBase API (online) | `data/derived/aquamaps-species-info/aquamaps-info_{species,growth,ecology,repro,stocks,oxygen,speed}.csv`; `data/derived/aquamaps-species-info/species_attributes.csv` | **Step 6 — run once per species update** |
| `query-aquamaps-data.R` | Queries **AquaMaps** HSPEN database (`aquamapsdata`) for environmental preference envelopes by species key; aggregates by Ecospace functional group. **Needs rework** — references undefined `fg` variable; writes to `./global-data/` path that does not exist in repo | `data/raw/species-list.csv`; AquaMaps local SQLite DB | `./global-data/` (wrong path — needs updating) | **Step 7 — needs repair before use** |
| `Make-preference-functions.R` | Generates double-logistic (habitat suitability) preference curves from the compiled parameter table; currently implements salinity only; writes Ecospace-formatted CSV matrix | `data/derived/env-pref-parameters.csv` | `output-for-ecospace/pref-functions/pref-funcs_Sal.csv` | **Step 8** (requires `env-pref-parameters.csv`) |

### misc-code/

| Script | Purpose | Reads | Writes | Notes |
|--------|---------|-------|--------|-------|
| `create-example-spider-plots.R` | Demonstrates multi-scenario radar/spider plots for Ecospace output comparison | Synthetic data (internal) | PNG plots (if saved) | Stand-alone example — not part of pipeline |
| `create-example-bar-plots.R` | Demonstrates multi-panel bar plots for scenario comparison | Synthetic data (internal) | PNG plots (if saved) | Stand-alone example — not part of pipeline |

---

## 4. Data Pipeline

```
╔══════════════════════════════════════════════════════╗
║  PIPELINE A — Spatial Basemap (run once)             ║
╠══════════════════════════════════════════════════════╣
║  NOAA API (online)                                   ║
║    → make-baythymetry-basemap.R                      ║
║    → output-for-ecospace/habitat/base-depth-map-     ║
║      88x56.asc  ← KEY FILE (required by all below)  ║
║                                                      ║
║  data/raw/jurisdictions/jurisraster.tif              ║
║    → make-jurisdictional-maps.R                      ║
║    → output-for-ecospace/jurisdictions/ascii/        ║
║      jurisdiction_{maryland,virginia,potomac}.asc    ║
╚══════════════════════════════════════════════════════╝

╔══════════════════════════════════════════════════════╗
║  PIPELINE B — Environmental Drivers                  ║
╠══════════════════════════════════════════════════════╣
║  data/raw/CBEFS-hindcast/*.nc  (~43 GB, 40 files)    ║
║    → process-CBEFS.R  (run_mode = "FULL")            ║
║    → var-stack-NC/<var>_<depth>_1985_2024.nc  (15)   ║
║        → aggregate-daily-stacks-to-monthly.R         ║
║        → var-stack-NC-monthly/*_monthly_mean.nc (15) ║
║        → [optional] make-gif-videos.R                ║
║            → GIFs/<var>/*.gif  (not in git)          ║
║                                                      ║
║  data/raw/ches-clim-atlas-vims.nc  (gitignored)      ║
║    → make-climatology-maps.R                         ║
║    → ches-atlas-climatology/<var>/<var>_<Mon>.asc    ║
╚══════════════════════════════════════════════════════╝

╔══════════════════════════════════════════════════════╗
║  PIPELINE C — Species Preference Functions           ║
╠══════════════════════════════════════════════════════╣
║  data/raw/species-list.csv (25 rows / ~12 FGs)       ║
║    → query-env-preference-parameters.R  (FishBase)   ║
║    → data/derived/aquamaps-species-info/*.csv         ║
║                                                      ║
║    → query-aquamaps-data.R  (AquaMaps, needs rework) ║
║    → [manually compiled]                             ║
║    → data/derived/env-pref-parameters.csv            ║
║        → Make-preference-functions.R                 ║
║        → output-for-ecospace/pref-functions/         ║
║          pref-funcs_Sal.csv  (salinity only so far)  ║
╚══════════════════════════════════════════════════════╝
```

---

## 5. Key Outputs for Ecospace

All final Ecospace inputs live in `output-for-ecospace/`:

| File / Directory | Description | Format |
|-----------------|-------------|--------|
| `habitat/base-depth-map-88x56.asc` | Depth basemap (88×56 grid, meters, NA = land) | ESRI ASCII grid |
| `jurisdictions/ascii/jurisdiction_maryland.asc` | Binary Maryland presence layer (1/0/NA) | ESRI ASCII grid |
| `jurisdictions/ascii/jurisdiction_virginia.asc` | Binary Virginia presence layer (1/0/NA) | ESRI ASCII grid |
| `jurisdictions/ascii/jurisdiction_potomac.asc` | Binary Potomac buffer layer (1/0/NA) | ESRI ASCII grid |
| `env-drivers/ches-atlas-climatology/<var>/` | Monthly climatology ASC files (5 vars × 12 months = 60 files) | ESRI ASCII grid |
| `env-drivers/CBEFS-hindcast/var-stack-NC-monthly/` | Monthly mean env driver stacks, 1985–2024 (5 vars × 3 depths = 15 NC files) | NetCDF |
| `pref-functions/pref-funcs_Sal.csv` | Salinity preference function matrix (functional groups × 1200 steps) | CSV |

**Variables processed from CBEFS:**

| Variable | Depth bands | Units |
|----------|-------------|-------|
| `temperature` | `_bott`, `_surf`, `_davg` | °C |
| `salinity` | `_bott`, `_surf`, `_davg` | PSU |
| `diss_o2` | `_bott`, `_surf`, `_davg` | mg O₂ L⁻¹ |
| `phytoplankton` | `_bott`, `_surf`, `_davg` | — |
| `NO3` | `_bott`, `_surf`, `_davg` | mmol N m⁻³ |

---

## 6. Known Issues

| Issue | Location | Priority |
|-------|----------|----------|
| `query-aquamaps-data.R` references undefined `fg` variable and writes to `./global-data/` path that does not exist — needs repair before re-running AquaMaps HSPEN queries | `preference-functions/` | High |
| CBEFS hindcast stacks are not yet reprojected to the basemap grid — CRS is intentionally ignored in current scripts but must be resolved before Ecospace ingestion | `environmental-drivers/` | High |
| Preference functions only implemented for salinity; temperature, dissolved oxygen, depth, and NO₃ curves not yet generated | `Make-preference-functions.R` | Medium |
| `dispersal-rates/` is an empty placeholder directory | repo root | Low |
| Git history contains ~500 MB of previously committed GIF/PNG media; files are now untracked but history not cleaned — use BFG Repo-Cleaner or `git filter-branch` if repo size is a concern | git | Low |

---

## 7. Outstanding Work / Development Roadmap

Items needed before the Ecospace model can run, listed in approximate priority order.

### 7a. Commit staged changes

All changes from the documentation and cleanup session are staged but not yet committed. Run:

```r
git commit -m "Add documentation, clean up code, remove GIF tracking"
```

### 7b. Resolve CBEFS CRS and alignment

CBEFS hindcast data is on an oblique stereographic projection (+proj=stere +lon_0=283.54 +lat_0=37.75), 336×564 grid, ~600 m resolution. All current CBEFS scripts deliberately skip CRS handling. Before `output-for-ecospace/env-drivers/` products can be used in Ecospace, they must be reprojected and resampled to the 88×56 basemap. This likely requires adding a reprojection step inside `process-CBEFS.R` or a new post-processing script.

### 7c. Build missing static habitat layer scripts

Five habitat layers are needed for Ecospace but have no scripts:

| Layer | Suggested script | Notes |
|-------|-----------------|-------|
| SAV | `habitat/make-sav-layer.R` | VIMS annual SAV survey data |
| Soft-bottom substrate | `habitat/make-substrate-layers.R` | NOAA/state benthic survey |
| Hard-bottom / structured habitat | same script | |
| Oyster / bivalve reef | `habitat/make-oyster-layer.R` | MD/VA oyster reef GIS |
| Marsh / emergent vegetation | `habitat/make-marsh-layer.R` | NOAA C-CAP or NWI |

Each script should follow the existing pattern: read source → resample to basemap (`resample(x, basemap, method = "max")`) → write `.asc` to `output-for-ecospace/` + PNG to `habitat/plots/`.

### 7d. Extend preference functions to remaining variables

`Make-preference-functions.R` currently only generates salinity curves. Extend to:
- Temperature (`driver = "Temp"`, `max = 40`)
- Dissolved oxygen (`driver = "DO"`)
- Depth (`driver = "Depth"`, `max = 400`, `range = "wide"`)
- NO₃ / nutrient (`driver = "NO3"`)

Parameters for each variable need entries in `data/derived/env-pref-parameters.csv`.

### 7e. Repair `query-aquamaps-data.R`

The AquaMaps HSPEN query script has two bugs:
1. References `fg` (should be `sp_list`)
2. Writes to `./global-data/` (should be `./data/derived/`)

Fix these and re-run to programmatically regenerate `env-pref-parameters.csv` entries.

### 7f. Expand species and FG lists for lower trophic groups

`data/raw/species-list.csv` only covers fish/crustacean groups. Add representative entries (or placeholders) for:
- Bivalves
- Benthic Invertebrates
- Zooplankton

SAV, Phytoplankton, and Detritus do not need FishBase queries but should appear in `env-pref-parameters.csv` as FG rows.

### 7g. Add life-stage splits to species-list.csv

The target Ecospace model uses stage-specific FGs (e.g., Blue Catfish 0–1 yr, 1-harvest, trophy; three striped bass stages; two sturgeon stages; three blue crab stages). The current `species-list.csv` does not distinguish stages. `env-pref-parameters.csv` already has stage-specific rows for Blue Catfish — the same treatment is needed for Striped Bass, Sturgeon, and Blue Crab.

### 7h. QA output bundle for every spatial product

For each final `.asc` file, generate a small QA record:
- PNG map saved to `habitat/plots/` or `environmental-drivers/env_climatology/figs/`
- `compareGeom()` check against basemap
- Summary of min, max, NA count, and dimensions

This is already partially done for the basemap and jurisdiction layers.
