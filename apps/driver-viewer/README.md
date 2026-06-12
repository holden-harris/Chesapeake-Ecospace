# Driver viewer — Chesapeake environmental drivers (Ches-ICAT)

A read-only R Shiny app for exploring the monthly **CBEFS ROMS-ECB hindcast**
environmental drivers (1985–2024) on the Ecospace grids. It visualizes the
finished NetCDF stacks produced by [`../../make-environmental-drivers/`](../../make-environmental-drivers/);
it **only reads** those outputs and never re-runs the pipeline.

## Launch

From the repository root:

```r
shiny::runApp("apps/driver-viewer")
```

The app resolves its data paths relative to its own directory, so it works as
long as it lives at `apps/driver-viewer/` inside the repo.

## What it shows

- **Variables:** salinity, temperature, dissolved oxygen, phytoplankton, nitrate.
- **Depths:** surface, bottom, depth-averaged.
- **Panels** = a mix-and-match picker over all variable × depth combinations, so
  any arbitrary set can be shown (a warning appears above 8 panels). Panels fill
  left-to-right up to 4 per row, then wrap. The default view is Salinity — Bottom,
  Temperature — Depth-averaged, Dissolved oxygen — Bottom, Phytoplankton — Surface.
  Each panel is a `ggplot2` `geom_raster` map on a white background with an
  `rnaturalearth` coastline, optional **MD/VA/Potomac jurisdictional boundaries**
  and grid outline, and a per-variable color bar with units.
- **Time:** a 480-step month slider (1985-01 … 2024-12) plus a year + month
  jump-to, kept in sync. Play / Pause / Step animate through months at a
  selectable speed (Slow / Medium / Fast / Very fast).
- **Resolution:** switch live between **F01, F02, F03, F04**; F02 (88×56) is the
  default. Switching reloads the stacks (memoised, so switching back is instant).

## Design notes

- **Consistent color scales.** Each variable has one fixed color domain — robust
  1–99 % quantiles computed across all depths and all months — shared across
  panels and animation frames so they are directly comparable. Domains are
  computed lazily on first use and cached.
- **Per-variable palettes** reuse the repo's PDF/GIF palette vocabulary
  (`grDevices::hcl.colors` names; see `make-environmental-drivers/cbefs-helpers.R::cbefs_var_styles`):
  temperature = Heat (reversed), salinity = viridis, DO = YlGnBu,
  phytoplankton = YlGn, nitrate = Purples.
- **Units** (confirmed against `../../make-environmental-drivers/CBEFS-notes-metadata.txt`
  and `../../docs/environmental-drivers-methods.md`): temperature °C; salinity PSU
  (PSS-1978); dissolved oxygen mg O₂ L⁻¹; phytoplankton mmol N m⁻³; nitrate
  mmol N m⁻³.
- **Duplicate 2024-01 layer.** The regridded stacks carry 481 layers / 480
  distinct months (an upstream `process-CBEFS.R` artifact). The month index is
  built chronologically from `terra::time()` and keeps the first occurrence of
  each date, so the slider spans exactly 480 months with no gap or duplicate.
- **Config block.** Paths, resolution list, variable/depth sets, and the palette
  table live in a single block at the top of `app.R` (mirroring the canonical
  constants in `cbefs-helpers.R`). Change inputs or add a resolution there
  without touching the rest of the code.

## Run modes

The app auto-detects how it is being run from the top of `app.R`:

- **Local dev** (no `data/` folder): reads the NetCDF/basemaps straight from the
  repo's `output-for-ecospace/` tree and builds the coastline + jurisdiction
  overlays on the fly via `R/prebuild.R`.
- **Deployed** (a `data/` folder is present): reads everything from that
  self-contained folder, including prebaked overlay `.rds` — so the deployed
  bundle needs no `rnaturalearth*` packages at runtime.

## Files

| File | Purpose |
|------|---------|
| `app.R` | Config block (run-mode detection) + Shiny UI + server |
| `R/helpers.R` | Data loading, month index/dedup, per-variable domains, overlays, `plot_driver()` |
| `R/prebuild.R` | Builds the coastline + MD/VA/Potomac overlays (uses `rnaturalearth*`); run at staging, **excluded from the deploy bundle** |
| `deploy/stage-data.R` | Assembles the self-contained `data/` folder (F02–F04 stacks, basemaps, prebaked overlays) |
| `deploy/deploy.R` | Publishes to shinyapps.io |
| `smoke-test.R` | Headless dev check; not part of the app runtime |

## Deploying to shinyapps.io

Gives collaborators a public URL (no R needed on their end). Bundles **F02 + F03 +
F04** (~232 MB); F01 is omitted to keep uploads fast.

```r
install.packages("rsconnect")                       # once
# Authenticate with YOUR token: shinyapps.io dashboard -> Account -> Tokens:
rsconnect::setAccountInfo(name="<acct>", token="<token>", secret="<secret>")
```

Then, from the **repo root**:

```sh
Rscript apps/driver-viewer/deploy/stage-data.R      # build data/ (re-run if drivers change)
Rscript apps/driver-viewer/deploy/deploy.R          # upload -> https://<acct>.shinyapps.io/ches-icat-env-drivers/
```

Notes: free-tier links are **public** (password needs a paid plan); the app
**sleeps when idle** and cold-starts in a few seconds; each redeploy re-uploads
the full bundle (~2 min). `deploy.R` passes an explicit `appFiles` list so
`R/prebuild.R` and `deploy/` are excluded from both the upload and the dependency
manifest — that is what keeps the non-CRAN `rnaturalearthhires` out of the deploy.

## Dependencies

**Runtime (deployed):** `shiny`, `terra`, `sf`, `ggplot2`, `patchwork`, `scales`,
`viridisLite` — all CRAN/PPM, so shinyapps.io installs them cleanly.

**Staging / local dev only:** `rnaturalearth`, `rnaturalearthdata`,
`rnaturalearthhires` (for building the overlays), plus `rsconnect` (to deploy).
`rnaturalearthhires` comes from the rOpenSci R-universe
(`https://ropensci.r-universe.dev`); if unavailable, `build_coast()` falls back to
the medium-scale `rnaturalearthdata` coastline. All are pinned in the repo-level
`renv.lock` (`renv::restore()` to reproduce).
