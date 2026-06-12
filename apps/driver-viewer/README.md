# Driver viewer — Chesapeake environmental drivers (Ches-ICAT)

A read-only R Shiny app for exploring the monthly **CBEFS ROMS-ECB hindcast**
environmental drivers (1985–2024) on the Ecospace grids. It is an internal,
stakeholder-facing viewer for the **Chesapeake Invasive Catfish Assessment Tool
(Ches-ICAT)**: it visualizes the finished NetCDF stacks produced by the
[`make-environmental-drivers/`](../../make-environmental-drivers/) pipeline and
**only reads** those outputs — it never re-runs the pipeline.

**Live app:** <https://holdenharris.shinyapps.io/ches-icat-env-drivers/>
(public link; free-tier app sleeps when idle and cold-starts in ~5–10 s).

---

## Contents

- [Quick start (run locally)](#quick-start-run-locally)
- [What the app shows](#what-the-app-shows)
- [Data contract](#data-contract)
- [Design notes](#design-notes)
- [Run modes (local vs deployed)](#run-modes-local-vs-deployed)
- [Repository layout](#repository-layout)
- [Deploying / updating the live app](#deploying--updating-the-live-app)
- [Dependencies](#dependencies)
- [Known limitations](#known-limitations)

---

## Quick start (run locally)

From the repository root:

```r
shiny::runApp("apps/driver-viewer")
```

The app resolves all paths relative to its own directory, so it works as long as
it lives at `apps/driver-viewer/` inside the repo. No arguments or environment
setup are needed for local use. To reproduce the exact package versions, run
`renv::restore()` first (the repo uses `renv`).

---

## What the app shows

- **Variables:** salinity, temperature, dissolved oxygen, phytoplankton, nitrate.
- **Depths:** surface, bottom, depth-averaged.
- **Panels** — a single **mix-and-match picker** over all 15 variable × depth
  combinations, so any arbitrary set can be shown (e.g. bottom salinity +
  surface temperature). Panels fill **left-to-right up to 4 per row**, then wrap;
  a warning appears above 8 panels. The default view is **Salinity — Bottom,
  Temperature — Depth-averaged, Dissolved oxygen — Bottom, Phytoplankton —
  Surface**.
- **Each panel** is a `ggplot2` `geom_raster` map on a white background with a
  coastline + MD/VA state boundaries, optional **MD/VA/Potomac jurisdictional
  boundaries** and grid outline (toggles), and a per-variable color bar with units.
- **Time** — a 480-step month slider (1985-01 … 2024-12) plus a year + month
  jump-to, kept in sync. **Play / Pause / Step** animate through months at a
  selectable speed (Slow / Medium / Fast / Very fast).
- **Resolution** — switch live between **F02 (88×56, default), F03 (59×37), and
  F04 (44×28)**. Switching reloads the stacks (memoised, so switching back is
  instant). F01 (176×111) is intentionally excluded — see
  [Known limitations](#known-limitations).
- **Overlay toggles** — coastline (on), jurisdictions (on), grid outline (off).

---

## Data contract

The app reads finished, regridded monthly stacks; it does not compute them.

**Input NetCDF stacks** — one per `<var>_<depth>`, **480 monthly layers**
(1985-01 … 2024-12):

```
output-for-ecospace/env-drivers/CBEFS-hindcast/grid-<LABEL>/var-stack-NC-monthly-regridded/
    <var>_<depth>_1985_2024_monthly_mean.nc
```

| Element | Values |
|---|---|
| Variables (`<var>`) | `salinity`, `temperature`, `diss_o2`, `phytoplankton`, `NO3` |
| Depths (`<depth>`)  | `surf`, `bott`, `davg` |
| Grid labels (`<LABEL>`) used by the app | `F02-88x56`, `F03-59x37`, `F04-44x28` |

**Grid / CRS** — WGS84 (EPSG:4326). F02 is 56 cols × 88 rows, extent
≈ −77.42…−75.56 °W, 36.69…39.61 °N (read from the basemap at runtime; all grids
share one extent).

**Land mask / extent source:**
`output-for-ecospace/habitat/basemaps/base-depth-map-<LABEL>.asc` (+`.prj`).
NA cells = land / outside the model domain; valued cells = water depth.

**Jurisdiction source (overlay):**
`data-inputs/spatial-static/jurisdictions/jurisraster.tif`
(EPSG:3857; 1 = Maryland, 2 = Virginia, 3 = Potomac).

**Display units** (confirmed against
[`CBEFS-notes-metadata.txt`](../../make-environmental-drivers/CBEFS-notes-metadata.txt)
and [`environmental-drivers-methods.md`](../../docs/environmental-drivers-methods.md)):

| Variable | Display | Units |
|---|---|---|
| temperature | Temperature | °C |
| salinity | Salinity | PSU (PSS-1978) |
| diss_o2 | Dissolved oxygen | mg O₂ L⁻¹ |
| phytoplankton | Phytoplankton | mmol N m⁻³ |
| NO3 | Nitrate | mmol N m⁻³ |

---

## Design notes

- **Consistent color scales.** Each variable has one fixed color domain — robust
  1–99 % quantiles computed across all depths and all months — shared across
  panels and animation frames so they are directly comparable. Domains are
  computed lazily on first use and cached per `(resolution, variable)`.
- **Per-variable palettes** reuse the repo's PDF/GIF palette vocabulary
  (`grDevices::hcl.colors` names; see
  `make-environmental-drivers/cbefs-helpers.R::cbefs_var_styles`):
  temperature = Heat (reversed), salinity = viridis, DO = YlGnBu,
  phytoplankton = YlGn, nitrate = Purples.
- **Jurisdictional boundaries** are the *internal dividing lines* between MD, VA,
  and the Potomac — derived by polygonizing `jurisraster.tif`, dissolving by
  class, and extracting the lines **shared** between adjacent jurisdictions (not
  each region's full coastline-following perimeter). Drawn as a magenta line with
  a white halo so they read over any palette. Being vector, they overlay
  identically at every resolution.
- **Duplicate 2024-01 layer.** The regridded stacks carry 481 layers / 480
  distinct months (an upstream `process-CBEFS.R` artifact). The month index is
  built chronologically from `terra::time()` and keeps the first occurrence of
  each date, so the slider spans exactly 480 months with no gap or duplicate.
- **Config block.** Paths, resolution list, variable/depth sets, and the palette
  table live in a single block at the top of `app.R` (mirroring the canonical
  constants in `cbefs-helpers.R`). Change inputs or add a resolution there
  without touching the rest of the code.

---

## Run modes (local vs deployed)

`app.R` auto-detects how it is running from the presence of a `data/` folder
inside the app directory:

- **Local dev** (no `data/`): reads the NetCDF and basemaps straight from the
  repo's `output-for-ecospace/` tree, and builds the coastline + jurisdiction
  overlays on the fly via `R/prebuild.R` (needs `rnaturalearth*`).
- **Deployed** (a `data/` folder is present): reads everything from that
  self-contained folder, including **prebaked overlay `.rds` files** — so the
  deployed bundle needs no `rnaturalearth*` package at runtime. The `data/` folder
  is produced by `deploy/stage-data.R` and is **gitignored** (~242 MB).

This split exists because shinyapps.io bundles only files inside the app
directory and cannot install the non-CRAN `rnaturalearthhires` — see
[Deploying](#deploying--updating-the-live-app).

---

## Repository layout

| File | Purpose |
|------|---------|
| `app.R` | Config block (run-mode detection) + Shiny UI + server |
| `R/helpers.R` | Data loading, month-index dedup, per-variable color domains, overlay loading, `plot_driver()` |
| `R/prebuild.R` | Builds the coastline + MD/VA/Potomac overlays (uses `rnaturalearth*` + `terra`); run at staging, **excluded from the deploy bundle** |
| `deploy/stage-data.R` | Assembles the self-contained `data/` folder (F02–F04 stacks, basemaps, prebaked overlay `.rds`) |
| `deploy/deploy.R` | Publishes to shinyapps.io with an explicit `appFiles` list |
| `smoke-test.R` | Headless dev check (renders single + multi-panel composites); not part of the app runtime |
| `data/` | **Generated, gitignored.** The deploy bundle: `env-drivers/`, `basemaps/`, `overlays/{coast,juris}.rds` |
| `.rscignore` | Excludes dev files from the upload |

---

## Deploying / updating the live app

The app is deployed to **shinyapps.io** (managed Posit hosting). The deployed
bundle contains **F02 + F03 + F04** (~242 MB staged → ~84 MB compressed on
upload); F01 is omitted to keep deploys fast.

### One-time setup

```r
install.packages("rsconnect")
# Authenticate with YOUR token: shinyapps.io dashboard -> Account -> Tokens ->
# Show -> "With rsconnect" -> Copy to clipboard, then paste & run that whole line:
rsconnect::setAccountInfo(name="<acct>", token="<token>", secret="<secret>")
rsconnect::accounts()   # should list your account
```

### Deploy / redeploy (from the repo root)

```sh
Rscript apps/driver-viewer/deploy/stage-data.R   # (re)build data/ — re-run only if drivers change
Rscript apps/driver-viewer/deploy/deploy.R       # upload + publish
# -> https://<acct>.shinyapps.io/ches-icat-env-drivers/
```

First deploy takes ~5–8 min (one-time server-side package build + upload); later
redeploys are ~2 min (image cached). Re-run `stage-data.R` before `deploy.R` only
when the underlying drivers change.

### How the non-CRAN package is kept out of the deploy

`rnaturalearthhires` (used to build the high-res coastline) is **not on CRAN**, so
if it reached the deploy manifest the build would fail. Two mechanisms prevent
that:

1. The overlays are **prebaked to `data/overlays/*.rds`** at staging time, so the
   runtime never calls `rnaturalearth`.
2. `deploy.R` passes an explicit **`appFiles`** list that omits `R/prebuild.R`
   (the only file referencing `rnaturalearth*`) and `deploy/`. This matters
   because `.rscignore` controls *uploads* but **not** rsconnect's dependency
   scan — `appFiles` scopes both. The resulting manifest contains only CRAN/PPM
   packages.

### Operational notes

- **Public link** on the free tier (password protection needs a paid plan).
- The app **sleeps when idle** and cold-starts in a few seconds.
- Free tier ≈ 25 active-hours/month and 1 GB instance RAM (ample here; `terra`
  reads layers lazily).
- Each redeploy re-uploads the full bundle.

---

## Dependencies

**Runtime (deployed):** `shiny`, `terra`, `sf`, `ggplot2`, `patchwork`, `scales`,
`viridisLite` — all CRAN/PPM, so shinyapps.io installs them cleanly.

**Staging / local-dev only:** `rnaturalearth`, `rnaturalearthdata`,
`rnaturalearthhires` (build the overlays), plus `rsconnect` (deploy).
`rnaturalearthhires` comes from the rOpenSci R-universe
(`https://ropensci.r-universe.dev`); if it is unavailable, `build_coast()` falls
back to the medium-scale `rnaturalearthdata` coastline. All packages are pinned in
the repo-level `renv.lock` (`renv::restore()` to reproduce).

---

## Known limitations

- **F01 (176×111) is excluded** from the picker and the deploy bundle — it is
  539 MB (~80 % of the full dataset) and adds little for a viewer, since F02 is
  the project's standard Ecospace grid. To re-enable it, add `"F01-176x111"` to
  `RESOLUTIONS` in `app.R` and to `RESOLUTIONS` in `deploy/stage-data.R`, then
  re-stage and redeploy.
- **Public link.** Anyone with the URL can view the app (free-tier limitation).
- **Read-only.** The app is a viewer; it does not modify or regenerate any
  pipeline outputs.
