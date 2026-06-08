# Chesapeake Bay Blue Catfish Ecospace Project  
## R Coding Work Summary to Date

**Prepared:** 2026-06-08  
**Scope reviewed:** Project conversation history and uploaded project materials available in this ChatGPT project. This summary focuses on the R workflow developed to support Chesapeake Bay Ecospace model preparation, especially habitat maps, jurisdiction/region masks, species preference functions, environmental drivers, and preliminary scenario-output visualizations.

---

## 1. Project coding context

The R work to date supports the development of a spatial Ecospace model for the Chesapeake Bay Blue Catfish project. The project goal is to use a spatially explicit, ecosystem-scale model to evaluate stakeholder-preferred Blue Catfish harvest strategies and their ecological and economic trade-offs.

The coding work has therefore focused on preparing model-ready spatial inputs, environmental drivers, species/environment preference tables, and visualization tools that can be used for stakeholder-facing scenario comparisons.

Major themes:

1. Build **static spatial layers** for Ecospace, including depth, habitat, jurisdiction, and region masks.
2. Develop **species and functional-group preference functions** for environmental drivers.
3. Process **spatial-temporal environmental drivers** into monthly Ecospace-ready NetCDF products.
4. Create **quality-control visualizations** and example stakeholder-facing model-output figures.
5. Keep the workflow reproducible in a GitHub repository while excluding large data files.

---

## 2. Repository and workflow organization

### Repository

The project repository is:

```text
C:/Repos/Chesapeake-Ecospace
```

The branch is `main`, tracking:

```text
https://github.com/holden-harris/Chesapeake-Ecospace
```

### Large-file handling

A `.gitignore` workflow was developed to keep the repository clean and avoid committing large data products. Suggested ignored files included:

- NetCDF files: `*.nc`, `*.nc4`
- GIS files: `*.shp`, `*.shx`, `*.dbf`, `*.prj`, and related shapefile components
- large model outputs
- local output folders such as `outputs/`, `model-output/`, `ecospace-runs/`
- temporary/cache/log folders
- R workspace files such as `*.RData`, `*.rds`
- compressed tables such as `*.csv.gz`
- local Ecopath/Ecosim/Ecospace files such as `*.eii`, `*.eif`, `*.mex`
- local R/Python environment files

### General R style used

The coding style has generally emphasized:

- explicit user settings at the top of each script
- straightforward, readable R code
- comments explaining each processing block
- avoiding excessive function abstraction while workflows are still being tested
- progress messages with timestamps for long-running processing
- user-controlled `run_mode`, overwrite/skip options, and variable-selection options

Core R packages used or assumed across scripts include:

```r
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(tools)
```

---

## 3. Ecospace functional-group framing

A working set of functional groups has been defined or discussed for the Ecospace model, including:

- Blue Catfish groups:
  - `BC 0-1 yr`
  - `BC 1-harvest`
  - `BC trophy`
- Striped bass groups:
  - prey-sized striped bass
  - harvest-sized striped bass
  - trophy striped bass
- Sturgeon:
  - `sturgeon 0-1`
  - `sturgeon 1+`
- alosines
- blue crab:
  - juveniles
  - sublegals
  - legals
- sciaenids
- menhaden
- freshwater demersals
- other forage
- bivalves
- benthic invertebrates
- SAV
- zooplankton
- phytoplankton
- detritus

These groups are important because the habitat maps, environmental preference functions, and scenario-output plots are ultimately organized around Ecospace functional groups and fleets rather than only species-level data.

---

## 4. Habitat-map workflow

### 4.1 Target Ecospace habitat layers

A conceptual habitat-layer set was identified for the Chesapeake Bay Ecospace model. The full candidate set included:

1. depth
2. salinity zones
3. temperature
4. dissolved oxygen
5. submerged aquatic vegetation (SAV)
6. marsh / emergent vegetation / shoreline habitat
7. soft-bottom substrate
8. hard-bottom / structured habitat
9. oyster or bivalve reef habitat

A simplified minimum set was also discussed for initial modeling:

1. depth
2. salinity
3. temperature
4. dissolved oxygen
5. SAV
6. soft-bottom habitat

### 4.2 Depth raster development

The depth workflow was designed to create a model-ready depth habitat layer that could be rasterized and exported as ASCII for Ecospace.

Rasters discussed included:

```r
depth_rast
depth_rast2
depth_rast3
depth_rast4
```

Reported grid sizes included:

| Raster | Approximate grid size | Notes |
|---|---:|---|
| `depth_rast` | 192 × 120 | base grid |
| `depth_rast2` | 96 × 60 | aggregated grid |
| `depth_rast3` | 64 × 40 | aggregated grid |
| `depth_rast4` | not explicitly confirmed in available notes | created with aggregation factor 4 |

The `depth_rast4` object was created using:

```r
depth_rast4 <- aggregate(depth_rast, fact = 4, fun = mean, na.rm = TRUE)
```

Earlier guidance noted the approximate cell sizes expected from these aggregation factors:

| Aggregation | Approx. resolution | Approx. cell size |
|---:|---:|---:|
| base | 0.01666667° | ~1.8 km |
| factor 2 | 0.03333333° | ~3.6 km |
| factor 3 | 0.05° | ~5.5 km |
| factor 4 | 0.06666667° | ~7.3 km |

### 4.3 Habitat-map outputs

The intended outputs from the habitat-map scripts are:

- quick-look PNG maps for QA/QC
- Ecospace-compatible `.asc` raster files
- eventually a consistent set of static habitat layers on the same grid, extent, resolution, and alignment

The current coding direction is appropriate: build a clear base grid first, resample or rasterize each habitat layer onto that grid, inspect the output visually, and then export the final layers in Ecospace-compatible formats.

---

## 5. Jurisdiction and region-mask workflow

### 5.1 Purpose

Jurisdiction masks are needed to support region-specific Ecospace outputs and management scenarios. They are also important because the project is explicitly organized around Chesapeake Bay management jurisdictions, especially Maryland, Virginia, and the Potomac River.

### 5.2 Overlay strategy

A practical overlay strategy was developed to reduce jurisdictional-resolution problems:

1. Separate the jurisdiction layers.
2. Resample each jurisdiction to the basemap individually.
3. Overlay them in a controlled order:
   - Virginia first
   - Maryland second
   - Potomac last
4. Use the Potomac layer last so that it can overwrite encroaching cells from neighboring jurisdictions where needed.

### 5.3 Potomac buffer issue

A specific problem was identified: some cells were still encroaching into the Potomac region. A buffer approach was attempted using code conceptually similar to:

```r
pot_poly <- as.polygons(pot_src, dissolve = TRUE, na.rm = TRUE)

pot_buffer_m <- 20000
pot_poly_buf <- buffer(pot_poly, width = pot_buffer_m)

pot_src_buf <- rasterize(
  pot_poly_buf,
  juris,
  field = 1,
  background = 0,
  touches = FALSE
)
```

However, this did not behave as intended because the buffering step effectively treated the Potomac area as one dissolved geometry, rather than buffering only the specific Potomac cells or local features that needed correction.

### 5.4 Export workflow

The next coding step identified was to export jurisdiction products as:

- PNG maps for visual inspection
- `.asc` files for each jurisdiction

Recommended output naming convention:

```text
jurisdiction_md.asc
jurisdiction_va.asc
jurisdiction_potomac.asc
jurisdiction_all.asc
```

Recommended QA/QC checks:

- confirm all rasters have the same `ext()`, `res()`, `crs()`, `nrow()`, and `ncol()`
- confirm that Potomac cells are not overwritten by Maryland or Virginia
- inspect cells along jurisdiction boundaries
- confirm that exported `.asc` files preserve the intended `NA` or background values

---

## 6. Species environmental preference functions

### 6.1 Purpose

Species preference functions are being developed to parameterize Ecospace habitat suitability or environmental response relationships. The project has focused especially on salinity preferences and tolerances, with the same structure extendable to temperature, dissolved oxygen, depth, or other drivers.

### 6.2 Preference table structure

A four-point environmental preference/tolerance structure was used:

| Column | Interpretation |
|---|---|
| `abs_min` | absolute minimum tolerated value |
| `pref_min_10` | lower preferred bound or lower percentile |
| `pref_max_90` | upper preferred bound or upper percentile |
| `abs_max` | absolute maximum tolerated value |

### 6.3 Functional-group averaging

Because Ecospace parameters are generally applied at the functional-group level, a dplyr workflow was developed to average species-level parameters by functional group:

```r
fg_prefs <- species_prefs %>%
  group_by(FG) %>%
  summarise(
    n_species = n(),
    across(
      c(abs_min, pref_min_10, pref_max_90, abs_max),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )
```

A more flexible version was also discussed using `where(is.numeric)` and rounding, while preserving functional-group order where needed.

### 6.4 Example salinity values discussed

Example salinity values discussed for several species or groups included:

| Species / group | `abs_min` | `pref_min_10` | `pref_max_90` | `abs_max` |
|---|---:|---:|---:|---:|
| Blue Catfish | 0 | 0 | 7 | 22 |
| Redear Sunfish | 0 | 0 | 4 | 15 |
| White Perch | 0 | 0 | 15 | 25 |
| Bluegill | 0 | 0 | 3.5 | 5.6 |
| Pumpkinseed | 0 | 0 | 4 | 18 |
| Gizzard Shad | 0 | 0 | 10 | 34 |

These were treated as ppt ≈ psu for practical Ecospace parameterization.

### 6.5 Recommended preference-function next steps

The next step is to turn the preference table into a traceable parameter product with:

- one row per species or functional group
- variable name, units, and source notes
- values for `abs_min`, `pref_min_10`, `pref_max_90`, and `abs_max`
- flags for assumed, literature-derived, expert-elicited, or averaged values
- notes on whether values differ by life stage, especially for Blue Catfish, striped bass, sturgeon, and blue crab

---

## 7. Environmental-driver processing for Ecospace

### 7.1 Data source and model-grid issue

The environmental driver work focused on processing CBEFS hindcast NetCDF files for Ecospace.

Notes from the CBEFS data indicated:

- year 1985 model results are on a 336 × 564 grid
- approximate grid resolution is 600 m × 600 m
- projection is an oblique stereographic projection:

```text
+proj=stere +lon_0=283.54 +lat_0=37.75
```

- the first dimension roughly corresponds to east/west
- the second dimension roughly corresponds to north/south

For initial testing, CRS and alignment issues were intentionally set aside so that temporal aggregation and output mechanics could be developed first.

### 7.2 Raw-to-stack workflow

A CBEFS processing script was developed to write environmental-driver stacks as TIFF, NetCDF, or both, depending on user settings.

Output directories were:

```r
out_dir_tiff <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-TIFF"
out_dir_nc   <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-NC"
```

A user-facing format option was added conceptually as:

```r
out_format <- "NC"    # or "TIFF" or "BOTH"
```

The script was also updated to support:

```r
run_mode <- "TEST"    # first 4 years
```

and:

```r
variables_to_run <- "ALL"
```

or a vector of selected variables.

### 7.3 Monthly aggregation workflow

The monthly-aggregation workflow reads daily NetCDF stacks from:

```r
dir_in <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-NC"
```

and writes monthly NetCDF files to:

```r
dir_out <- "./output-for-ecospace/env-drivers/CBEFS-hindcast/var-stack-NC-monthly"
```

Core R logic:

```r
x_raw <- rast(file_in)

month_id <- format(time(x_raw), "%Y-%m")

x_month <- tapp(
  x_raw,
  index = month_id,
  fun   = mean,
  na.rm = TRUE
)
```

Features requested for this workflow:

- process one file setting per row
- aggregate daily layers to monthly means
- print progress updates and timestamps for each year and variable
- optionally skip existing monthly files
- optionally overwrite existing monthly files
- support test runs before full production

### 7.4 Error encountered

A key error occurred during early testing:

```r
Error: [tapp] path does not exist
```

This likely indicated that the raster object or file path passed into `tapp()` was not pointing to a valid existing NetCDF source, or that `x_raw` had been created from an invalid path. The recommended debugging direction was to explicitly check file existence before `rast()` and before `tapp()`:

```r
file.exists(file_in)
```

and to print the path being processed.

### 7.5 GIF animation workflow

A simple animation workflow was also requested for a single file as a proof of concept. The intent was to:

1. read one environmental-driver NetCDF stack
2. aggregate it to monthly means
3. create a sequence of maps
4. write a GIF animation for visual QA/QC

This is useful for checking whether the monthly driver behaves sensibly through time before scaling the workflow to all variables and years.

### 7.6 Recommended environmental-driver next steps

Recommended next steps:

1. finalize a single master Ecospace grid
2. explicitly resolve CBEFS CRS and alignment
3. decide whether monthly drivers should be retained as NetCDF or converted to ASCII/CSV formats for Ecospace ingestion
4. add QA plots for the first and last month of each output file
5. record variable units and transformations
6. validate that the time dimension survives NetCDF writing and re-reading
7. scale from test years to all years only after one complete variable passes QA

---

## 8. Scenario and output-visualization code

### 8.1 Spider/radar plots

A spider/radar plot workflow was developed for comparing management scenarios across multiple metrics. Updates included:

- renaming metrics
- expanding from three to four scenarios
- using consistent scenario colors across plots

### 8.2 Example Ecospace output bar plots

A multi-panel bar-plot workflow was developed to show example outputs from Ecospace. The x-axis used example scenarios:

```text
Scenario 1, Scenario 2, Scenario 3, Scenario 4, Scenario 5, Scenario 6
```

Panels included:

- Biomass: pre-harvest BCF
- Catches: harvest BCF
- Catches: trophy BCF
- Effort: BCF recreational fleets
- Catches: Blue crab
- Catches: Striped bass
- Ecological composite metric
- Reduced BCF predation

The visualization was updated so that scenario colors are consistent across panels, for example Scenario 1 always has the same color, Scenario 2 always has the same color, and so on.

These plots are useful as placeholders for stakeholder meetings because they communicate how scenario outputs could be compared once model runs are available.

---

## 9. Current status by workstream

| Workstream | Status | Notes |
|---|---|---|
| Repository organization | Started | Repo created; `.gitignore` strategy developed for large files |
| Functional-group list | Drafted | Blue Catfish, striped bass, sturgeon, blue crab, forage, benthos, primary producers, detritus included |
| Habitat-layer conceptual design | Drafted | Full and simplified layer sets identified |
| Depth raster workflow | Started | Multiple aggregated grids tested; ASCII export is intended |
| Jurisdiction masks | In progress | Overlay strategy developed; Potomac boundary issue still needs refinement |
| Preference functions | Started | Salinity table structure and functional-group averaging workflow developed |
| CBEFS raw environmental processing | Started | TIFF/NC/BOTH output logic and test mode developed |
| Monthly environmental aggregation | Started | Daily-to-monthly NetCDF workflow designed; early path error encountered |
| GIF QA animation | Started | Single-file proof-of-concept requested |
| Scenario visualizations | Started | Radar and multi-panel bar plots developed for example outputs |

---

## 10. Key unresolved technical issues

### 10.1 Spatial grid finalization

The project needs one authoritative Ecospace grid. All static habitats, jurisdiction masks, and environmental drivers should be forced to this grid before final export.

Recommended checks:

```r
compareGeom(layer1, layer2, stopOnError = FALSE)
ext(layer)
res(layer)
crs(layer)
nrow(layer)
ncol(layer)
```

### 10.2 CRS and alignment

CBEFS drivers are in an oblique stereographic projection, while some habitat and jurisdiction data may be geographic or projected differently. CRS/alignment was intentionally ignored during early temporal-processing tests, but this must be resolved before model-ready products are finalized.

### 10.3 Potomac jurisdiction correction

The Potomac mask needs a more targeted correction than dissolving and buffering the entire polygon. Better options may include:

- buffering individual raster cells before dissolving
- using a separate high-confidence Potomac centerline or polygon
- applying a rule-based overwrite only within a narrow boundary zone
- manually editing a small number of ambiguous cells after reproducible rasterization

### 10.4 Preference-function provenance

The preference-function table needs source tracking. Each value should be labeled as:

- empirical/literature-derived
- expert judgment
- species-level average
- functional-group average
- assumed placeholder

This is especially important for stakeholder review and for later manuscript/report writing.

### 10.5 NetCDF time metadata

Before running all years and variables, confirm that:

- input files have valid time dimensions
- monthly aggregation preserves the intended time index
- output NetCDF files can be re-opened with correct layer names and dates
- units are documented

---

## 11. Recommended next coding priorities

### Priority 1: Create a master grid script

Create a script that defines the official Ecospace raster template and saves it as a reusable object.

Suggested output:

```text
data-processed/ecospace_grid_template.tif
data-processed/ecospace_grid_template.rds
```

### Priority 2: Finalize static habitat layers

Create one script per static layer or one controlled pipeline that produces:

```text
depth.asc
sav.asc
soft_bottom.asc
hard_bottom.asc
oyster_reef.asc
jurisdiction_md.asc
jurisdiction_va.asc
jurisdiction_potomac.asc
```

Each output should have a matching PNG QA map.

### Priority 3: Finalize environmental preference tables

Create a clean CSV or Excel file with:

```text
functional_group
species
variable
units
abs_min
pref_min_10
pref_max_90
abs_max
source_type
source_citation
notes
```

Then write an R script to summarize species-level rows to functional-group rows.

### Priority 4: Stabilize CBEFS monthly aggregation

Add robust checks around file paths, time metadata, skip/overwrite behavior, and output naming.

Suggested output pattern:

```text
<variable>_<year>_monthly.nc
```

or, if files span multiple years:

```text
<variable>_monthly_<start_year>_<end_year>.nc
```

### Priority 5: Add automated QA outputs

For every final spatial product, generate a small QA bundle:

- PNG map
- summary table of values
- raster dimensions
- CRS
- min/max
- count of `NA` and non-`NA` cells

### Priority 6: Prepare stakeholder-facing visualization templates

Keep the spider/radar and multi-panel bar plots as separate scripts that can later be linked to true Ecospace scenario outputs.

---

## 12. Suggested folder structure

```text
Chesapeake-Ecospace/
  R/
    01_make_ecospace_grid.R
    02_make_static_habitats.R
    03_make_jurisdiction_masks.R
    04_prepare_preference_functions.R
    05_process_cbeifs_daily_stacks.R
    06_aggregate_drivers_monthly.R
    07_qaqc_driver_animation.R
    08_plot_example_scenarios.R

  data-raw/
    habitats/
    jurisdictions/
    environmental-drivers/

  data-processed/
    ecospace-grid/
    habitats/
    jurisdictions/
    preference-functions/

  output-for-ecospace/
    habitat-asc/
    jurisdiction-asc/
    env-drivers/
      CBEFS-hindcast/
        var-stack-TIFF/
        var-stack-NC/
        var-stack-NC-monthly/

  figures/
    qaqc/
    stakeholder-examples/

  docs/
    workflow-summary.md
```

---

## 13. Bottom-line assessment

The R coding work has moved from conceptual model setup into practical Ecospace input preparation. The strongest progress is in:

- identifying the required spatial layers
- developing depth raster aggregation/export logic
- designing jurisdiction masks and region outputs
- establishing a functional-group preference-function workflow
- building a scalable CBEFS environmental-driver processing pipeline
- creating early scenario-visualization templates

The main remaining challenge is integration: all spatial products need to be brought onto one authoritative grid, checked for CRS/alignment consistency, documented with metadata, and exported in final Ecospace-ready formats. Once that spatial foundation is stable, the environmental-driver and preference-function workflows can be scaled up with much lower risk.
