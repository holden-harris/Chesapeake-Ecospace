# Chesapeake-Ecospace Repository Inventory

## 1. Repository Overview

This repository builds all spatial and environmental inputs required to run a Chesapeake Bay ecosystem model in **Ecopath with Ecosim (EwE) / Ecospace**. The outputs are ASCII grid files, NetCDF raster stacks, and CSV preference function tables that feed directly into the EwE Ecospace spatial module.

**Ecospace grids:** four resolutions sharing one extent (Chesapeake Bay bounding box −77.4 to −75.55°W, 36.7 to 39.65°N, WGS84): F01 (176×111), **F02 (88×56, standard, ~2-arcminute / ~3.5 km cells)**, F03 (59×37), F04 (44×28). Labels are `<rows>×<cols>` (F02 = 88 rows × 56 cols). Environmental drivers are produced at all four; the native CBEFS grid (F00, 336×564) is kept for visualization only.

**Three main output categories for Ecospace:**
1. Spatial basemap — depth grid + jurisdiction layers
2. Environmental driver stacks — monthly time series (1985–2024) for 5 variables × 3 depths
3. Species preference functions — double-logistic habitat suitability curves by functional group

---

## 2. Directory Map

| Directory | Contents |
|-----------|----------|
| `data-inputs/spatial-static/` | Static spatial inputs: jurisdiction rasters |
| `data-inputs/spatial-dynamic/` | Time-varying spatial inputs: CBEFS hindcast NetCDF files (~43 GB, gitignored) and Bay Atlas climatology NetCDF |
| `data-inputs/species-info/` | Species list CSV; FishBase API query outputs (`aquamaps-species-info/`) |
| `data-inputs/env-preference-functions/` | Manually compiled environmental preference parameter table (`env-pref-parameters.csv`) |
| `make-environmental-drivers/` | R scripts for processing CBEFS hindcasts and Bay Atlas climatology; GIF animation outputs (gitignored) |
| `make-habitat-maps/` | R scripts for creating the depth basemap and jurisdiction maps; PNG plot outputs |
| `make-preference-functions/` | R scripts for querying FishBase / AquaMaps APIs and generating logistic preference curves |
| `misc-code/` | Stand-alone example visualization scripts (not part of the main pipeline) |
| `output-for-ecospace/` | **Final outputs** — all files consumed directly by Ecospace (ASC grids, NetCDF stacks, preference CSVs) |
| `terra-temp/` | Scratch space for terra raster processing (gitignored, auto-created) |
| `hoard/` | Archived / reference files (gitignored) |

---

## 3. R Script Inventory

### make-habitat-maps/

| Script | Purpose | Reads | Writes | Pipeline step |
|--------|---------|-------|--------|---------------|
| `make-baythymetry-basemap.R` | Downloads NOAA bathymetry; tests 4 resolutions; exports the 88×56 depth grid used by all other scripts | NOAA online API (`marmap::getNOAA.bathy`) | `output-for-ecospace/habitat/base-depth-map-88x56.asc`; `make-habitat-maps/plots/depth-map.png` | **Step 1 — run once** |
| `make-jurisdictional-maps.R` | Creates binary (0/1) jurisdiction rasters (MD, VA, Potomac) aligned to the basemap; buffers Potomac by 7.2 km | `data-inputs/spatial-static/jurisdictions/jurisraster.tif`; basemap ASC | `output-for-ecospace/jurisdictions/ascii/jurisdiction_{maryland,virginia,potomac}.asc`; `make-habitat-maps/plots/chesapeake_bay_jurisdictions.png` | **Step 2 — run once** (requires basemap) |

### make-environmental-drivers/

| Script | Purpose | Reads | Writes | Pipeline step |
|--------|---------|-------|--------|---------------|
> **3-stage pipeline** (see [`make-environmental-drivers/README.md`](../make-environmental-drivers/README.md) for the full reference, [`docs/environmental-drivers-methods.md`](environmental-drivers-methods.md) for methods). Stage 1 extracts/aggregates once; Stage 2 regrids onto each basemap once; Stage 3 builds products. `run-environmental-drivers.R` orchestrates Stages 2–3. Outputs live under `output-for-ecospace/env-drivers/CBEFS-hindcast/{var-stack-NC-monthly, grid-<label>/…}`.

| Script | Purpose | Reads | Writes | Pipeline step |
|--------|---------|-------|--------|---------------|
| `cbefs-helpers.R` | **Shared helper module** (sourced, not run): canonical paths; lon/lat regrid index + many-to-one mean (`build_regrid_index`/`regrid_to_basemap`/`regrid_stack_file`); resolution registry (`list_basemaps`/`resolution_set`/`stack_dir_for`); monthly/climatology aggregation; date recovery; ASCII writer (sidecar-suppressing); `init_terra()`; diagnostics; per-variable plot styles | — | — | **Sourced by all stage scripts** |
| `process-CBEFS.R` | **Stage 1.** Reads 40 yearly CBEFS files; splits each variable into 3 depth bands (`_bott`/`_surf`/`_davg`); writes monthly-mean stacks (computed **in-process**; native curvilinear grid, index space). Flags: `run_mode` (TEST/FULL), `out_format` (NC/TIFF/BOTH), `variables_to_run`, `nc_compression`, `write_daily_stack` (off by default, ~47 GB), `write_monthly_stack` | `data-inputs/spatial-dynamic/CBEFS-hindcast/holdenharris_YYYY_v*.nc` (40) | `var-stack-NC-monthly/<var>_<depth>_<yr>_<yr>_monthly_mean.nc` (15); optional `var-stack-NC-daily/` | **Stage 1 — long-running (~hours)** |
| `regrid-to-basemaps.R` | **Stage 2.** Regrids native monthly stacks onto each basemap (F01–F04) once via the lon/lat index (many-to-one mean; `terra::resample` does not apply to the curvilinear grid). Flags: `resolutions`, `variables_to_run`, `overwrite_nc` | `var-stack-NC-monthly/*.nc`; one raw CBEFS file (lon/lat); basemaps | `grid-<label>/var-stack-NC-monthly-regridded/*.nc` (15 per basemap) | **Stage 2** |
| `make-ecospace-ascii-drivers.R` | **Stage 3a — key deliverable.** Regridded stacks → Ecospace ASCII (sidecars suppressed; basemaps only, no F00). Flags: `resolutions`, `variables_to_run`, `write_series`, `write_climatology` | `grid-<label>/var-stack-NC-monthly-regridded/*.nc` | `grid-<label>/ASCII/<var>_<depth>/<var>_<depth>_YYYY_MM.asc` + `climatology/<var>_<depth>_<Mon>.asc` | **Stage 3a** |
| `make-driver-pdfs.R` | **Stage 3b.** Per `<var>_<depth>`: 12-month climatology panel + per-year monthly PDF, all resolutions incl. native F00 | native + regridded stacks | `grid-<label>/PDFs/<prefix>_{climatology,monthly_by_year}.pdf` | **Visualization** |
| `make-gif-videos.R` | **Stage 3c.** Monthly GIF animations, all resolutions incl. F00. Flags: `gif_prefixes` (NULL = all), `gif_start_year`/`gif_end_year`, `overwrite_gif` | native + regridded stacks | `grid-<label>/GIFs/<var>_<depth>/<...>_monthly_<yr>_<yr>.gif` | **Visualization** |
| `run-environmental-drivers.R` | **Orchestrator** for Stages 2–3. Sets shared globals and sources the chosen stages. Flags: `do_regrid/ascii/pdf/gif`, `resolutions`, `variables_to_run`, `gif_prefixes`, `gif_start_year/end_year`, `out_root` | (sources stage scripts) | — | **Entry point (Stages 2–3)** |
| `make-atlas-climatology-maps.R` | **Independent** Bay Atlas pipeline (renamed from `make-climatology-maps.R`). Reads Bay Atlas climatology NetCDF → monthly ASC + PNG panels | `data-inputs/spatial-dynamic/CBEFS-climatology/ches-clim-atlas-vims.nc`; basemap | `output-for-ecospace/env-drivers/ches-atlas-climatology/<var>/<var>_<Mon>.asc`; PNG | **Independent** — run once |

### make-preference-functions/

| Script | Purpose | Reads | Writes | Pipeline step |
|--------|---------|-------|--------|---------------|
| `query-aquamaps-data.R` | Queries **FishBase** API (`rfishbase`) for 7 trait tables (species, growth, ecology, reproduction, stocks, oxygen tolerance, swimming speed); joins selected fields into a combined attributes table | `data-inputs/species-info/species-list.csv`; FishBase API (online) | `data-inputs/species-info/aquamaps-species-info/aquamaps-info_{species,growth,ecology,repro,stocks,oxygen,speed}.csv`; `species_attributes.csv` | **Step 6 — run once per species update** |
| `query-env-preference-parameters.R` | Queries **AquaMaps** HSPEN database (`aquamapsdata`) for environmental preference envelopes by species key; aggregates by Ecospace functional group. **Needs rework** — references undefined `fg` variable | `data-inputs/species-info/species-list.csv`; AquaMaps local SQLite DB | `data-inputs/species-info/speciesListGoM_QAQC_AMkeys.csv`; `data-inputs/env-preference-functions/fg-env-preference-parameters.csv` | **Step 7 — needs repair before use** |
| `Make-preference-functions.R` | Generates double-logistic (habitat suitability) preference curves from the compiled parameter table; currently implements salinity only; writes Ecospace-formatted CSV matrix | `data-inputs/env-preference-functions/env-pref-parameters.csv` | `output-for-ecospace/pref-functions/pref-funcs_Sal.csv` | **Step 8** (requires `env-pref-parameters.csv`) |

### misc-code/

| Script | Purpose | Reads | Writes | Notes |
|--------|---------|-------|--------|-------|
| `create-example-spider-plots.R` | Demonstrates multi-scenario radar/spider plots for Ecospace output comparison | Synthetic data (internal) | PNG plots (if saved) | Stand-alone example — not part of pipeline |
| `create-example-bar-plots.R` | Demonstrates multi-panel bar plots for scenario comparison | Synthetic data (internal) | PNG plots (if saved) | Stand-alone example — not part of pipeline |

---

## 4. Data Pipeline

> **Note:** the Environmental-Drivers (Pipeline B) box below predates the 3-stage refactor and is kept only as a high-level sketch. For the current flow use the [`make-environmental-drivers/` table](#3-r-script-inventory) above, [`make-environmental-drivers/README.md`](../make-environmental-drivers/README.md), and the [methods report](environmental-drivers-methods.md): **Stage 1** `process-CBEFS.R` → **Stage 2** `regrid-to-basemaps.R` (regrid native → each basemap F01–F04) → **Stage 3** `make-ecospace-ascii-drivers.R` / `make-driver-pdfs.R` / `make-gif-videos.R`, orchestrated by `run-environmental-drivers.R`. ASCII drivers now land in `grid-<label>/ASCII/<var>_<depth>/` (per resolution), not a single `ASCII-monthly/`.

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
║  data-inputs/spatial-dynamic/CBEFS-hindcast/*.nc     ║
║    (~43 GB, 40 files)                                ║
║    → process-CBEFS.R  (run_mode = "FULL", NC only)   ║
║      ├ var-stack-NC/<var>_<depth>_1985_2024.nc (15)  ║
║      └ var-stack-NC-monthly/*_monthly_mean.nc  (15)  ║
║          (monthly computed in-process)               ║
║            → make-ecospace-ascii-drivers.R           ║
║              → ASCII-monthly/<var>_<depth>_YYYY_MM.asc║
║              → ASCII-climatology/<var>_<depth>_<Mon>.asc║
║            → [optional] make-gif-videos.R → GIFs/    ║
║            → [optional] make-driver-pdfs.R → PDFs/   ║
║                                                      ║
║  data-inputs/spatial-dynamic/CBEFS-climatology/      ║
║    ches-clim-atlas-vims.nc  (gitignored)             ║
║    → make-climatology-maps.R  (independent)          ║
║    → ches-atlas-climatology/<var>/<var>_<Mon>.asc    ║
╚══════════════════════════════════════════════════════╝

╔══════════════════════════════════════════════════════╗
║  PIPELINE C — Species Preference Functions           ║
╠══════════════════════════════════════════════════════╣
║  data-inputs/species-info/species-list.csv           ║
║    → query-aquamaps-data.R  (FishBase)               ║
║    → data-inputs/species-info/aquamaps-species-info/ ║
║                                                      ║
║    → query-env-preference-parameters.R               ║
║      (AquaMaps, needs rework — fg variable bug)      ║
║    → [manually compiled]                             ║
║    → data-inputs/env-preference-functions/           ║
║      env-pref-parameters.csv                         ║
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
| `env-drivers/CBEFS-hindcast/grid-<label>/ASCII/<var>_<depth>/` | **Ecospace environmental drivers** — monthly ASCII series (480/var) + 12-month climatology, per basemap F01–F04 (15 vars × 492 × 4 = 29,520 files) | ESRI ASCII grid |
| `env-drivers/CBEFS-hindcast/var-stack-NC-monthly/` | Native monthly-mean stacks, 1985–2024 (15 NC; Stage 1, also F00 source) | NetCDF |
| `env-drivers/CBEFS-hindcast/grid-<label>/var-stack-NC-monthly-regridded/` | Per-basemap regridded monthly stacks (15 × 4 = 60 NC; Stage 2) | NetCDF |
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
| `query-env-preference-parameters.R` — `fg` variable bug fixed; output paths corrected. Script has not been re-run against AquaMaps DB; verify results before treating output as authoritative | `make-preference-functions/` | Medium |
| ~~CBEFS hindcast stacks not yet reprojected to the basemap grid~~ — **RESOLVED**: Stage 2 `regrid-to-basemaps.R` regrids native → each basemap via the stored lon/lat arrays (many-to-one mean) | `make-environmental-drivers/` | Done |
| CBEFS monthly stacks carry a duplicated `2024-01` layer (481 layers, 480 distinct months) — upstream `process-CBEFS.R` artifact; harmless downstream (480 distinct ASCII per variable) but should be fixed at source | `process-CBEFS.R` | Low |
| Preference functions only implemented for salinity; temperature, dissolved oxygen, depth, and NO₃ curves not yet generated | `Make-preference-functions.R` | Medium |
| Git history contains ~500 MB of previously committed GIF/PNG media; files are now untracked but history not cleaned — use BFG Repo-Cleaner or `git filter-branch` if repo size is a concern | git | Low |

---

## 7. Outstanding Work / Development Roadmap

Items needed before the Ecospace model can run, listed in approximate priority order.

### 7a. ~~Resolve CBEFS CRS and alignment~~ — DONE (2026-06)

CBEFS hindcast data is on an oblique stereographic projection (+proj=stere +lon_0=283.54 +lat_0=37.75), 336×564 grid, ~600 m resolution, curvilinear in lon/lat. **Resolved:** Stage 2 `regrid-to-basemaps.R` regrids the native stacks onto each Ecospace basemap (F01–F04) using the stored `longitude`/`latitude` arrays (`cbefs-helpers.R::build_regrid_index` + `regrid_to_basemap`, a many-to-one mean) — `terra::resample` does not apply to the curvilinear grid. Drivers are produced at all four resolutions; see the [methods report](environmental-drivers-methods.md) for rationale.

### 7b. Build missing static habitat layer scripts

Five habitat layers are needed for Ecospace but have no scripts:

| Layer | Suggested script | Notes |
|-------|-----------------|-------|
| SAV | `make-habitat-maps/make-sav-layer.R` | VIMS annual SAV survey data |
| Soft-bottom substrate | `make-habitat-maps/make-substrate-layers.R` | NOAA/state benthic survey |
| Hard-bottom / structured habitat | same script | |
| Oyster / bivalve reef | `make-habitat-maps/make-oyster-layer.R` | MD/VA oyster reef GIS |
| Marsh / emergent vegetation | `make-habitat-maps/make-marsh-layer.R` | NOAA C-CAP or NWI |

Source data for these layers should go in `data-inputs/spatial-static/habitats/`. Each script should follow the existing pattern: read source → resample to basemap (`resample(x, basemap, method = "max")`) → write `.asc` to `output-for-ecospace/` + PNG to `make-habitat-maps/plots/`.

### 7c. Extend preference functions to remaining variables

`Make-preference-functions.R` currently only generates salinity curves. Extend to:
- Temperature (`driver = "Temp"`, `max = 40`)
- Dissolved oxygen (`driver = "DO"`)
- Depth (`driver = "Depth"`, `max = 400`, `range = "wide"`)
- NO₃ / nutrient (`driver = "NO3"`)

Parameters for each variable need entries in `data-inputs/env-preference-functions/env-pref-parameters.csv`.

### 7d. Re-run `query-env-preference-parameters.R` and verify AquaMaps output

The `fg` variable bug and all output paths are now fixed. Re-run the script against the AquaMaps local SQLite DB to regenerate `data-inputs/env-preference-functions/fg-env-preference-parameters.csv`, then verify the output matches the manually curated `env-pref-parameters.csv` (or use it to fill in missing FG rows).

### 7e. Expand species and FG lists for lower trophic groups

`data-inputs/species-info/species-list.csv` only covers fish/crustacean groups. Add representative entries (or placeholders) for:
- Bivalves
- Benthic Invertebrates
- Zooplankton

SAV, Phytoplankton, and Detritus do not need FishBase queries but should appear in `env-pref-parameters.csv` as FG rows.

### 7f. Add life-stage splits to species-list.csv

The target Ecospace model uses stage-specific FGs (e.g., Blue Catfish 0–1 yr, 1-harvest, trophy; three striped bass stages; two sturgeon stages; three blue crab stages). The current `species-list.csv` does not distinguish stages. `env-pref-parameters.csv` already has stage-specific rows for Blue Catfish — the same treatment is needed for Striped Bass, Sturgeon, and Blue Crab.

### 7g. QA output bundle for every spatial product

For each final `.asc` file, generate a small QA record:
- PNG map saved to `make-habitat-maps/plots/` or `make-environmental-drivers/env_climatology/figs/`
- `compareGeom()` check against basemap
- Summary of min, max, NA count, and dimensions

This is already partially done for the basemap and jurisdiction layers.
