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
- **Panels** = selected variables × selected depths (a warning appears above 8
  panels). Each panel is a `ggplot2` `geom_raster` map with a grey land mask, an
  `rnaturalearth` coastline + MD/VA state boundaries, an optional grid outline,
  and a per-variable color bar with units.
- **Time:** a 480-step month slider (1985-01 … 2024-12) plus a year + month
  jump-to, kept in sync. Play / Pause / Step animate through months at an
  adjustable ms/frame speed.
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

## Files

| File | Purpose |
|------|---------|
| `app.R` | Config block + Shiny UI + server |
| `R/helpers.R` | Data loading, month index/dedup, per-variable domains, overlays, `plot_driver()` |
| `smoke-test.R` | Headless dev check (renders one panel + a multi-panel composite); not part of the app runtime |

## Dependencies

`shiny`, `terra`, `ggplot2`, `patchwork`, `viridisLite`, `sf`, `rnaturalearth`,
`rnaturalearthdata`, `rnaturalearthhires`, `scales`. All are pinned in the
repo-level `renv.lock`; run `renv::restore()` to reproduce the library.
`rnaturalearthhires` comes from the rOpenSci R-universe
(`https://ropensci.r-universe.dev`); if it is unavailable the coastline falls
back to the medium-scale `rnaturalearthdata` coastline automatically.
