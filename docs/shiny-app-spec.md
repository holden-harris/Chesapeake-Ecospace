---
title: "Spec — Shiny viewer for Chesapeake environmental drivers"
subtitle: "Kickoff document for the implementation session"
format: gfm
---

# Shiny driver-viewer — kickoff spec

**Read this first.** It is a self-contained brief for building an internal,
stakeholder-facing R Shiny app that visualizes the monthly CBEFS environmental
drivers already produced by the `make-environmental-drivers/` pipeline. The inputs
**already exist on disk** — this app only *reads* finished NetCDF stacks; it does
**not** re-run the pipeline.

Companion docs:

- Pipeline reference: [`../make-environmental-drivers/README.md`](../make-environmental-drivers/README.md)
- Methods / provenance: [`environmental-drivers-methods.md`](environmental-drivers-methods.md)

---

## 1. Purpose & audience

A read-only viewer of monthly CBEFS ROMS-ECB hindcast drivers (1985–2024). Ultimately, we'll make 
all resolutaions; however, let's start by building a minimum viable product on the
Ecospace **F02 (88×56)** grid. For now, this is for internal use by project stakeholders to explore
spatial/seasonal/interannual patterns; it's not a public deployment.

---

## 2. Data contract (verified on disk 2026-06)

> Keep these facts exact. They were confirmed against the filesystem.

**Input files** — 15 NetCDF stacks, one per `<var>_<depth>`, ~9.5 MB each,
**480 monthly layers** (1985-01 … 2024-12):

```
output-for-ecospace/env-drivers/CBEFS-hindcast/grid-F02-88x56/var-stack-NC-monthly-regridded/
    <var>_<depth>_1985_2024_monthly_mean.nc
```

| Element | Values |
|---|---|
| Variables (`<var>`) | `salinity`, `temperature`, `diss_o2`, `phytoplankton`, `NO3` |
| Depths (`<depth>`)  | `surf`, `bott`, `davg` |
| Example filename    | `salinity_bott_1985_2024_monthly_mean.nc`, `diss_o2_surf_1985_2024_monthly_mean.nc` |

**Grid (F02-88x56):**

| Property | Value |
|---|---|
| Dimensions | 56 cols × 88 rows |
| CRS | WGS84 / **EPSG:4326** (`base-depth-map-F02-88x56.prj`) |
| `xllcorner` | −77.4228 |
| `yllcorner` | 36.6917 |
| `dx` | 0.033254 |
| `dy` | 0.033143 |
| Extent | ≈ −77.42…−75.56 °W, 36.69…39.61 °N |

**Land mask:** `output-for-ecospace/habitat/basemaps/base-depth-map-F02-88x56.asc`
— NA cells = land, data cells = water (depth). Use as the model-domain water mask.

**No coastline shapefile is in the repo** → coastline comes from `rnaturalearth`.

**Other resolutions** (`grid-F00..F04`) exist. The prototype targets **F02 only**, but
read the grid label from config so swapping to F01/F03/F04 is a one-line change.

**Display labels & units — VERIFY before labeling.** Units are not guaranteed; confirm
against [`../make-environmental-drivers/CBEFS-notes-metadata.txt`](../make-environmental-drivers/CBEFS-notes-metadata.txt)
and the methods report, and mark anything unconfirmed as a TODO rather than guessing.
Suggested labels:

| `<var>` | Display name | Units (confirm) |
|---|---|---|
| `salinity` | Salinity | PSU |
| `temperature` | Temperature | °C |
| `diss_o2` | Dissolved oxygen | mg L⁻¹ (confirm) |
| `phytoplankton` | Phytoplankton | chl-a / mg C m⁻³ (confirm) |
| `NO3` | Nitrate | mmol N m⁻³ (confirm) |

| `<depth>` | Display name |
|---|---|
| `surf` | Surface |
| `bott` | Bottom |
| `davg` | Depth-averaged |

---

## 3. Confirmed design decisions

- **Render engine:** `ggplot2` (`geom_raster`) + `patchwork` for multi-panel layout.
  Panels are static images redrawn on each control change; animation = step the month
  and re-render.
- **Overlays:** **both** —
  1. grey land/water mask from the F02 basemap,
  2. `rnaturalearth` coastline + MD/VA state boundaries drawn on top,
  3. optional Ecospace grid outline (toggle, off by default).
- **Color scales:** **consistent per variable** — one fixed color domain per
  `variable`, shared across depths *and* across all 480 months, so panels and animation
  frames are directly comparable. Compute the domain once at startup as robust 1–99 %
  quantiles (resist outliers) across all layers and depths of that variable; cache it.

---

## 4. App layout (UI)

**Sidebar controls**

- **Variable** picker — multi-select (salinity, temperature, dissolved oxygen,
  phytoplankton, NO3).
- **Depth** picker — multi-select (surface, bottom, depth-averaged).
- Panels shown = chosen variables × chosen depths. Warn when > 8 panels.
- **Month/year** — `sliderInput` over 1985-01 … 2024-12 (480 steps via an integer
  index mapped to a `YYYY-MM` label), **plus** a precise year + month jump-to control.
- **Animation** — Play / Pause / Step buttons (`reactiveTimer` + `invalidateLater`)
  and a frame-speed (ms/frame) control.
- **Overlay toggles** — land mask (on), coastline (on), grid outline (off).

**Main panel**

- A `patchwork` grid of ggplot panels (`renderPlot`), one per variable×depth, each
  titled `"<Variable> — <Depth>"` with its own per-variable color bar and units.
- A shared header showing the selected **YYYY-MM**.
- Example default view: salinity-bottom, temperature-bottom, DO-bottom,
  phytoplankton-surface, NO3-surface.

---

## 5. Data-loading strategy

- At startup, build a named list keyed `"<var>_<depth>"` → `terra::rast(<path>)`
  (lazy — terra reads layers on demand). 15 stacks × 9.5 MB is light.
- Precompute and cache, per **variable**:
  - the fixed color domain (1–99 % quantiles across all layers & depths), and
  - a `month_index → layer` map built from the layer time/order.
- **De-duplicate the documented duplicate `2024-01` layer.** The regridded stacks may
  carry 481 layers / 480 distinct months (an upstream `process-CBEFS.R` artifact);
  index by date and keep one `2024-01`.
- For rendering, convert the selected layer with `terra::as.data.frame(xy = TRUE)` for
  `geom_raster`, or use `tidyterra::geom_spatraster` (note the added dependency).

---

## 6. Rendering spec (one panel)

- `geom_raster(aes(x, y, fill = value))` +
  `scale_fill_viridis_c(limits = <fixed per-variable domain>, name = "<units>")`
  (viridis is already used elsewhere in the repo).
- **Land:** render basemap-NA cells as grey — either a background `geom_raster` of the
  mask, or a grey panel background showing through the water raster.
- **Coastline:** `rnaturalearth::ne_states("United States of America")` /
  `ne_coastline`, cropped to the F02 extent, drawn with `geom_sf(fill = NA)`; ensure
  CRS 4326; lock extent/aspect with `coord_sf(xlim, ylim, expand = FALSE)`.
- **Grid outline (optional):** cell-edge lines derived from the basemap.
- **Labels:** panel title (variable + depth), legend title (units), and a shared
  `YYYY-MM` supertitle via `patchwork::plot_annotation`.

---

## 7. Dependencies

`shiny`, `terra`, `ggplot2`, `patchwork`, `viridisLite`, `sf`, `rnaturalearth`
(+`rnaturalearthhires` for a fine coastline), optionally `tidyterra`, `ncdf4`.

After the prototype works, run `renv::snapshot()` to pin versions — the repo already
uses `renv`.

---

## 8. File / directory layout for the app

- `apps/driver-viewer/app.R` — single-file Shiny to start; split into
  `ui.R` / `server.R` / `R/helpers.R` if it grows.
- Read the **grid label** and **output root** from a small config block at the top of
  `app.R` (mirror the canonical-paths constants in `make-environmental-drivers/cbefs-helpers.R`
  around lines 38–53) instead of hardcoding paths — so other resolutions or a relocated
  output tree work without code edits.
- `apps/driver-viewer/README.md` — how to launch:
  `shiny::runApp("apps/driver-viewer")`.

---

## 9. Build order

1. **Smoke test** — load one stack (`salinity_bott`), plot a single month with mask +
   coastline. Confirm extent / CRS / orientation (north up, bay shape correct).
2. Generalize to a panel function `plot_driver(var, depth, month_index)`.
3. Multi-panel via `patchwork` over the selected var × depth set.
4. Wire Shiny controls (pickers, slider, jump-to).
5. Animation (timer + play / pause / step).
6. Fixed per-variable scales + units + labels + overlay toggles.
7. `renv::snapshot()`; write the app README.

---

## 10. Known gotchas (carry forward)

- **Duplicate `2024-01`** layer may appear — de-dup by date when indexing months.
- **Units unconfirmed** — verify against the metadata/methods docs before labeling;
  leave a TODO if unknown rather than guessing.
- **Orientation** — the regridded F02 stacks are already georeferenced WGS84, so plot
  them as-is; but visually confirm the bay is north-up on the smoke test (mask +
  coastline should align). The native-grid vertical-flip handling was a Stage-1 concern
  and does not apply to these regridded outputs.
- **`rnaturalearthhires`** may need install from a non-CRAN repo — fall back to
  `ne_coastline(scale = "medium")` if it is unavailable.
