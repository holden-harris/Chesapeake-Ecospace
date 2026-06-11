# `make-environmental-drivers/` — CBEFS → Ecospace environmental drivers

Technical / reproducibility reference for the module that turns **CBEFS** ocean-model
hindcasts (1985–2024) into spatial environmental drivers for the Chesapeake Bay
Ecospace model. For the *why* (rationale, decisions, citations) see the methods
report: [`../docs/environmental-drivers-methods.md`](../docs/environmental-drivers-methods.md).

---

## Overview

The workflow is a **3-stage pipeline**. Stage 1 extracts and aggregates the raw model
output once; Stage 2 regrids the result onto each Ecospace basemap once; Stage 3
turns the regridded stacks into the products (ASCII drivers, PDF plots, GIF
animations). Stages 2–3 are orchestrated by `run-environmental-drivers.R`.

```
Raw CBEFS NetCDF (40 yearly files, 1985–2024, ~43 GB)
        │
        ▼  process-CBEFS.R                         ── Stage 1 (run separately)
   var-stack-NC-monthly/<var>_<depth>_..._monthly_mean.nc
   (15 stacks, native 336×564 curvilinear grid, index space, no CRS)
        │
        ▼  regrid-to-basemaps.R                    ── Stage 2  (do_regrid)
   grid-<label>/var-stack-NC-monthly-regridded/<same>.nc
   (one set per basemap F01..F04, on the WGS84 Ecospace grid)
        │
        ├─▶ make-ecospace-ascii-drivers.R          ── Stage 3a (do_ascii)  basemaps only
        ├─▶ make-driver-pdfs.R                      ── Stage 3b (do_pdf)    F00 + basemaps
        └─▶ make-gif-videos.R                       ── Stage 3c (do_gif)    F00 + basemaps
```

`cbefs-helpers.R` is a shared library sourced by every stage script (functions +
the canonical paths). `make-atlas-climatology-maps.R` is an **independent** Bay-Atlas
climatology pipeline, not part of these stages.

---

## Quickstart / run order

Run from the **repository root** (paths in the scripts are relative to it).

1. **Stage 1 — run once, separately** (long-running; needs the ~43 GB raw CBEFS files):
   ```r
   source("make-environmental-drivers/process-CBEFS.R")
   ```
   Produces the 15 native monthly stacks in `var-stack-NC-monthly/`. Set
   `run_mode <- "TEST"` for a fast salinity/bottom 4-year trial first.

2. **Stages 2–3 — orchestrated:** open `run-environmental-drivers.R`, set the toggles
   and selectors at the top, then source it:
   ```r
   do_regrid <- TRUE; do_ascii <- TRUE; do_pdf <- TRUE; do_gif <- TRUE
   resolutions      <- NULL          # NULL = all basemaps
   variables_to_run <- NULL          # NULL = all 15 <var>_<depth>
   source("make-environmental-drivers/run-environmental-drivers.R")
   ```

Each stage script is **flat and standalone** — sourcing it runs it top-to-bottom. The
orchestrator just sets shared globals (read via `if (!exists(...))` fallbacks) and
`source()`s the chosen stages, so you can also run any stage on its own with its
built-in defaults.

A full all-resolutions, all-variables Stage 2–3 run takes **~7 hours**; regrid is
~135 s per variable-basemap, ASCII ~140 s per variable-basemap.

---

## Directory layout

**Input** (gitignored, not in the repo):
```
data-inputs/spatial-dynamic/CBEFS-hindcast/holdenharris_YYYY_v20260112.nc   # 40 files, 1985–2024
output-for-ecospace/habitat/basemaps/base-depth-map-F##-<dims>.asc          # F01–F04 basemaps
```

**Output** (under `output-for-ecospace/env-drivers/CBEFS-hindcast/`):
```
var-stack-NC-monthly/                         # Stage 1: native monthly stacks (also F00 source)
var-stack-NC-daily/                           # Stage 1 optional archive (~47 GB, off by default)
grid-F00-336x564/                             # native grid — PDFs + GIFs only (no ASCII, no regrid)
│   ├── PDFs/                                 #   <prefix>_climatology.pdf, <prefix>_monthly_by_year.pdf
│   └── GIFs/<var>_<depth>/<...>.gif
grid-F01-176x111/  grid-F02-88x56/  grid-F03-59x37/  grid-F04-44x28/
    ├── var-stack-NC-monthly-regridded/       # Stage 2: regridded stacks (15 per basemap)
    ├── ASCII/<var>_<depth>/                  # Stage 3a: Ecospace drivers
    │   ├── <var>_<depth>_YYYY_MM.asc         #   monthly time series (480 per variable)
    │   └── climatology/<var>_<depth>_<Mon>.asc   # 12-month climatology (reference only)
    ├── PDFs/
    └── GIFs/<var>_<depth>/
```

Resolution labels: **F00** is the native CBEFS grid (336×564, used for PDF/GIF
comparison only); **F01–F04** are downsampled Ecospace basemaps (NOAA aggregation
factors). `F02-88x56` is the standard Ecospace grid. Note the dims convention: the
F01–F04 labels are **rows×cols** (e.g. F02 = 88 rows × 56 cols), while the native
**F00 `336×564` follows CBEFS's own east–west × north–south order**.

---

## Scripts reference

| Script | Stage | Reads | Writes | Key settings |
|---|---|---|---|---|
| `process-CBEFS.R` | 1 | raw `holdenharris_YYYY_*.nc` | `var-stack-NC-monthly/` (+ optional `var-stack-NC-daily/`) | `run_mode` (`TEST`/`FULL`), `out_format` (`NC`/`TIFF`/`BOTH`), `variables_to_run`, `nc_compression`, `write_daily_stack`, `write_monthly_stack` |
| `regrid-to-basemaps.R` | 2 | native monthly stacks; one raw CBEFS file (lon/lat) | `grid-<label>/var-stack-NC-monthly-regridded/` | `resolutions`, `variables_to_run`, `out_root`, `cbefs_raw_dir`, `overwrite_nc` |
| `make-ecospace-ascii-drivers.R` | 3a | regridded stacks (F01–F04) | `grid-<label>/ASCII/<var>_<depth>/` + `climatology/` | `resolutions`, `variables_to_run`, `write_series`, `write_climatology`, `naflag` (−9999) |
| `make-driver-pdfs.R` | 3b | native (F00) + regridded stacks | `grid-<label>/PDFs/` | `resolutions`, `variables_to_run`, `n_cols` |
| `make-gif-videos.R` | 3c | native (F00) + regridded stacks | `grid-<label>/GIFs/<var>_<depth>/` | `resolutions`, `gif_prefixes` (NULL = all), `gif_start_year`, `gif_end_year`, gif size/delay, `overwrite_gif` |
| `run-environmental-drivers.R` | 2–3 | (sources the stage scripts) | — | `do_regrid/ascii/pdf/gif`, `resolutions`, `variables_to_run`, `gif_prefixes`, `gif_start_year/end_year`, `out_root` |
| `cbefs-helpers.R` | lib | — | — | the **Canonical paths** block (single source of truth) + all shared functions |
| `make-atlas-climatology-maps.R` | indep. | Bay-Atlas climatology NetCDF | `env-drivers/ches-atlas-climatology/` | — (separate from the 3-stage pipeline) |

Key helper functions (all in `cbefs-helpers.R`): `build_regrid_index()` (`:110`),
`regrid_to_basemap()` (`:147`), `regrid_stack_file()` (`:524`), `monthly_mean()`
(`:266`), `climatology_12()` (`:286`), `write_ascii_layers()` (`:314`),
`list_basemaps()`/`resolution_set()`/`stack_dir_for()` (`:437`–`:493`),
`load_basemap()` (`:496`), `init_terra()` (`:62`).

---

## Configuration

**Per-run knobs** (set in `run-environmental-drivers.R`, or directly in a stage script):

| Setting | Meaning |
|---|---|
| `do_regrid` / `do_ascii` / `do_pdf` / `do_gif` | which stages to (re)build |
| `resolutions` | `NULL` = all; or labels (`"F02-88x56"`) / dims (`"88x56"`). F00 applies to PDF/GIF only |
| `variables_to_run` | `NULL` = all 15 `<var>_<depth>`; or a prefix subset |
| `gif_prefixes` | `NULL` = animate all variables (like PDF/ASCII); or a subset |
| `gif_start_year` / `gif_end_year` | inclusive year range for GIF frames only |
| `out_root` | relocate the whole `grid-<label>/…` output tree for this run |

**Canonical paths** — the single source of truth for input/output locations and
product subfolder names lives in **`cbefs-helpers.R:38–53`** (`CBEFS_OUT_ROOT`,
`NATIVE_STACK_SUBDIR`, `REGRIDDED_STACK_SUBDIR`, `ASCII_PRODUCT_SUBDIR`,
`PDF_PRODUCT_SUBDIR`, `GIF_PRODUCT_SUBDIR`, `BASEMAP_DIR_DEFAULT`,
`CBEFS_RAW_DIR_DEFAULT`). Edit a constant there once and it flows to every stage;
`out_root` in the run script overrides only the output root per run.

---

## Outputs catalog

For a full run (5 variables × 3 depths = **15** `<var>_<depth>` combinations):

| Product | Location | Count | Notes |
|---|---|---|---|
| Native monthly stacks | `var-stack-NC-monthly/` | 15 | ~1.8 GB total; native grid; F00 source |
| Regridded stacks | `grid-F01..F04/var-stack-NC-monthly-regridded/` | 60 | 15 × 4 basemaps |
| ASCII drivers | `grid-F01..F04/ASCII/<var>_<depth>/` | **29,520** | per variable: 480 monthly + 12 climatology = 492; × 15 × 4 |
| PDF plots | `grid-F00..F04/PDFs/` | 150 | 2 per variable (climatology + monthly-by-year) × 15 × 5 resolutions |
| GIF animations | `grid-F00..F04/GIFs/<var>_<depth>/` | = `(#gif_prefixes)` × `(#resolutions)` × `(#year-ranges run)` | e.g. all 15 vars × 5 resolutions = 75 per year-range |
| Daily archive (optional) | `var-stack-NC-daily/` | 15 | ~47 GB; `write_daily_stack`; unused downstream |

> The **480** monthly ASCII per variable (not 481) is expected — see Known issues.

---

## Environment & dependencies

- **R 4.5.1** (`C:\Program Files\R\R-4.5.1`).
- Packages (currently **unpinned**): `terra`, `ncdf4`, `gifski`, `viridisLite`,
  `stringr`, `dplyr`, `tools`. Stage 1 also uses `stringr`/`dplyr`; GIF uses `gifski`;
  plotting uses `viridisLite`.
- `init_terra()` (`cbefs-helpers.R:62`) sets a project-local terra scratch dir
  (`make-environmental-drivers/terra-temp/`, gitignored), `progress = 1`, and
  `memfrac = 0.7` (terra may spill to disk for large stacks).
- For exact reproducibility, consider pinning versions with `renv::snapshot()`.

---

## Known issues

- **Duplicate `2024-01` month.** The native monthly stacks carry a duplicated
  `2024-01` layer (481 layers, 480 distinct months) — an upstream `process-CBEFS.R`
  artifact. Downstream it is harmless: the two layers write to the same
  `…_2024_01.asc` filename, so each variable yields **480 distinct** monthly ASCII.
  Root cause should eventually be fixed in `process-CBEFS.R`.
- **Daily stacks are archive-only** (`var-stack-NC-daily/`, ~47 GB). Nothing
  downstream reads them; left off by default (`write_daily_stack <- FALSE`).
- Large rasters (`*.nc`, `*.tif`, `*.gif`), `terra-temp/`, and `gif-frames/` are
  gitignored — outputs are reproduced by re-running the pipeline, not pulled from git.

---

## Provenance

CBEFS = Chesapeake Bay Environmental Forecast System, a ROMS-ECB hindcast produced by
**Pierre St-Laurent (VIMS)** for this project (file version `v20260112`, created
2026-01-12). The hindcast runs on the model's own 336×564 oblique-stereographic
computational grid (`+proj=stere +lon_0=283.54 +lat_0=37.75`, ~600 m; St-Laurent and
Friedrichs 2024), delivered with three depth representations (bottom/surface/depth-averaged). Source metadata:
[`CBEFS-notes-metadata.txt`](CBEFS-notes-metadata.txt). Full attribution, units, and
references are in the [methods report](../docs/environmental-drivers-methods.md).
