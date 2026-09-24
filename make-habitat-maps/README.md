# Habitat Basemaps — `make-habitat-maps/`

Builds and quality-controls the Ecospace depth basemaps for the Chesapeake Bay
Blue Catfish Ecospace model. All outputs land in
`output-for-ecospace/habitat/basemaps/` as ESRI ASCII grids (`.asc`,
EPSG:4326, land = NODATA `-9999`).

| Script | Purpose |
|---|---|
| `make-baythymetry-basemap.R` | Downloads NOAA bathymetry and writes the four basemap resolutions (F01–F04) |
| `revise-basemap.R` | QA + revision of the F02 basemap: waterline review, connectivity repair, ocean clip, final export |
| `make-jurisdictional-maps.R` | MD / VA / Potomac jurisdiction layers on the basemap grid |

---

## 1. `make-baythymetry-basemap.R` — original basemaps

Fetches 1-arc-minute NOAA ETOPO bathymetry via `marmap::getNOAA.bathy()` for
the model extent (−77.4 to −75.55°E, 36.7 to 39.65°N), converts elevation to
positive depth (land → NA), and aggregates to the four Ecospace grids
(F01 176×111, **F02 88×56**, F03 59×37, F04 44×28). Aggregation uses
`mean(na.rm = TRUE)`, so **any** coarse cell touching 1′ water becomes a water
cell — the root cause of most issues addressed by `revise-basemap.R`.

## 2. `revise-basemap.R` — F02 review and revision

**Final product:** `output-for-ecospace/habitat/basemaps/base-depth-map-F02-88x56-revised.asc`
(the original `base-depth-map-F02-88x56.asc` is left untouched).

The script is standalone and fully reproducible: every decision below is
encoded in the script (flags and override tables at the top of each section),
so re-running it regenerates all figures, QA tables, and the revised grid.
Run it from the repo root; the first run downloads ~16 MB of ETOPO 2022 data,
cached to `data-inputs/spatial-static/etopo/` (gitignored) for later runs.

### 2.1 Waterline review

High-resolution "truth" reference: **ETOPO 2022 at 15 arc-sec (~450 m)** —
the same data family the basemap was built from, ~8× finer than the F02 cells.
The Bay **waterline** is extracted as the 0-m elevation contour and overlaid
on the basemap together with the cell grid and row/col indices
(row 1 = north, col 1 = west):

![Waterline review](plots/basemap-waterline-review-F02.png)

Zoom panels (Upper Bay, Mid Bay/Potomac, Lower Bay) support cell-level review:

![Waterline zooms](plots/basemap-waterline-review-F02-zooms.png)

For every F02 cell the script computes the **true water fraction** (mean of
the 15″ binary water mask resampled to the F02 grid, `terra::resample`
`method = "average"`) and flags suspects to
`basemap-suspect-cells-F02.csv`:

- water cells that are mostly land at 15″ (water fraction < 0.5) — 343 cells,
  the expected shoreline fringe of the `na.rm = TRUE` aggregation;
- land cells that are mostly water (fraction > 0.5) — 4 cells.

*Decision: the fringe cells were **kept** — trimming them risks severing
tributary connectivity that matters for Blue Catfish; misclassification is
handled instead by the connectivity repair and ocean clip below.*

### 2.2 Connectivity analysis (isolated water cells)

Ecospace moves biomass only between cells **sharing a side** (rook / 4-neighbour
adjacency), so water cells not 4-connected to the main Bay are dead pockets.
`terra::patches(directions = 4)` found **26 components**: the main Bay
(1,570 of 1,623 water cells) and **25 isolated patches** (53 cells) — mostly
upper tributary arms pinched off by the coarse grid (upper James, upper
Potomac chain, Pocomoke, Piankatank, C&D corner, etc.).

Each patch was reality-checked against the 15″ water mask: is it part of the
same real-world water body as the Bay? This informed, but did not decide, the
action — the final decisions were made in review (Section 2.3).

![Isolated patches and proposal](plots/basemap-isolated-patches-F02.png)

### 2.3 Bridge proposal and review decisions

Bridges (land cells converted to water so a patch connects by contiguous
sides) are proposed by a **least-cost path search**: a multi-source Dijkstra
from the main body over the 88×56 grid with rook moves, where crossing
existing water is ~free and adding a land cell costs
`1 + (1 − true water fraction)`. Paths therefore follow real channels where
the high-res data shows them and add as few cells as possible. Bridge cells
shared by chains of patches (e.g. the upper-Potomac trunk) are deduplicated;
the `serves` column of `basemap-bridge-cells-F02.csv` lists which patches
each bridge cell connects.

Review decisions (encoded as `exclude_patches`, `bridge_drop`,
`bridge_manual`, `bridge_depth_m` in the script):

| Decision | Choice | Rationale |
|---|---|---|
| Patches 2, 4 (NE corner, col 56) | **Removed** | Different waterbody (upper Delaware / C&D side), outside the model domain |
| All other 23 patches | **Connected** | Real Bay waters; Ecospace needs biomass exchange with the main body |
| Patch 26 (upper Rappahannock) auto-route | **Overridden**: connect east via (44,10), (45,10) into patches 27→28 | The auto path ran north to the Potomac; the patch belongs to the Rappahannock system |
| Patch 3 bridge (4,44) | **Moved** one cell south + east to (5,45) | Better placement at the Elk River head on review |
| Bridge cell depth | **Uniform 1 m** | Bridges are narrow channels below grid resolution; ETOPO cell-mean depths there are unreliable (kept as `depth_sug` info column) |

Result: **25 bridge cells** added at 1 m depth.

### 2.4 Ocean clip at the Bay mouth

The model domain ends at the Bay mouth (Cape Henry → Cape Charles, ≈ −76.0°
longitude). Identification is programmatic:

1. a **mouth barrier** is defined as the water cells of the grid column
   containing −76.0° (col 43), south of Cape Charles (lat < 37.3°);
2. a **flood fill** (`terra::patches`) from the SE-most water cell labels all
   water reachable without crossing the barrier — the Atlantic corner plus the
   seaside strip along the Delmarva Peninsula (**299 cells**);
3. any water left disconnected once the ocean is removed would also be
   outside (seaside pockets) — none existed (0 cells).

All 299 cells become land (`basemap-ocean-cells-F02.csv`). The barrier cells
themselves are Bay-side and are kept — the mouth stays open water up to −76.0°.

### 2.5 Applied revision and verification

| Step | Cells |
|---|---|
| Original F02 water cells | 1,623 |
| − patches 2, 4 (other waterbody) | −4 |
| − ocean outside the Bay mouth | −299 |
| + bridge cells (1 m depth) | +25 |
| **Revised F02 water cells** | **1,345** |

Automatic checks on the written grid:
- **Connectivity:** exactly **1** 4-connected water component (re-verified on
  file read-back);
- dimensions/extent unchanged (88×56, EPSG:4326), depths otherwise identical
  to the original.

![Original vs revised](plots/basemap-revised-F02.png)

### 2.6 Areal coverage of water

Computed from **true WGS84 ellipsoidal cell areas** via
`terra::cellSize(basemap, unit = "km")`, summed over water (non-NA) cells —
not from a constant cell size, since in this lon/lat grid the cell area
shrinks with latitude (≈10.94 km² at 36.7°N to ≈10.45 km² at 39.65°N;
equivalently ≈ dx·dy with dx = 0.03325° · 111.32 km · cos(lat) and
dy = 0.03314° · 111.32 km).

| Quantity | Value |
|---|---|
| Water cells | 1,345 of 4,928 (27.3 % of grid) |
| **Water area** | **14,439 km²** |
| Mean water-cell area | 10.74 km² |
| Grid total area | 52,838 km² |
| (Original basemap water area) | (17,464 km², incl. ocean) |

For context, the commonly cited surface area of Chesapeake Bay including
tidal tributaries is ≈ 11,600 km²; the F02 figure is larger because every
shoreline-fringe cell counts its full ~10.7 km² even where the true water
fraction is < 1 (see §2.1). The mean true water fraction over the revised
water cells gives an effective open-water area closer to that figure.

### 2.7 Outputs

| File | Content |
|---|---|
| `output-for-ecospace/habitat/basemaps/base-depth-map-F02-88x56-revised.asc` | **Final revised basemap** |
| `make-habitat-maps/basemap-suspect-cells-F02.csv` | Water-fraction QA flags |
| `make-habitat-maps/basemap-isolated-patches-F02.csv` | Isolated-patch summary + actions |
| `make-habitat-maps/basemap-bridge-cells-F02.csv` | Bridge cells (row, col, 1 m depth, ETOPO depth, water fraction, serves) |
| `make-habitat-maps/basemap-ocean-cells-F02.csv` | Cells clipped at the Bay mouth |
| `make-habitat-maps/plots/basemap-*.png` | Review figures (this README) |

### 2.8 Known considerations

- **Near-zero depths** inherited from the original aggregation remain (min
  0.004 m at row 37/col 42, Tangier flats; 7 cells < 0.05 m). EwE treats
  depth 0 as land — consider flooring very shallow cells if Ecospace
  complains.
- Land is written as NODATA `-9999`, consistent with the other basemaps; EwE
  reads NODATA and 0 alike as land (Ecospace's own exports use 0).
- Only **F02** is revised. F01/F03/F04 still carry the original footprint;
  re-apply the workflow if they are ever used.
- Downstream scripts (`make-environmental-drivers/`, jurisdiction maps)
  target `base-depth-map-F02-88x56.asc`. Adopting the revision means pointing
  them at `…-revised.asc` (or renaming after review) — deliberately not done
  automatically.
- Implementation note: on the R 4.5.3 Windows `png()` device, toggling the
  clip region (`xpd`, inline or via `par()`) before `legend()` makes the
  legend silently drop its first entry. The script avoids `xpd` entirely;
  keep it that way when editing the plots.

## 3. `make-jurisdictional-maps.R` — jurisdictions

Rasterizes the MD / VA / Potomac jurisdiction layers onto the F02 grid
(`pot_buffer_m` flag controls the Potomac buffer, default 7,200 m) and writes
binary presence grids to `output-for-ecospace/jurisdictions/ascii/`. Note: it
still reads the **original** basemap; update its `basemap` path when the
revised map is adopted.
