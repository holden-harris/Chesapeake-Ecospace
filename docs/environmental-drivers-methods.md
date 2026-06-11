---
title: "Environmental drivers for the Chesapeake Bay Ecospace model: data, processing, and regridding methods"
author: "Holden Harris"
date: "2026-06-11"
format:
  html:
    toc: true
    number-sections: true
  docx:
    toc: true
    number-sections: true
---

<!-- Quarto-convertible methods draft. Render with:  quarto render environmental-drivers-methods.md  (to .docx/HTML/PDF).
     Model description integrated from Bever, Friedrichs & St-Laurent (2021), Env. Modelling & Software 140:105036.
     TODO before submission, confirm with P. St-Laurent: (1) how the hindcast was interpolated from the native
     ChesROMS grid (~430 m-1 km, 20 vertical levels) to the delivered 336x564 ~600 m stereographic grid, and how
     the three depth bands (bottom/surface/depth-averaged) were derived; (2) full bibliographic details for the two
     ROMS-ECB references supplied with the data (DOIs 10.1029/2023MS003845 and 10.1016/j.ecss.2025.109632). -->

## Environmental data source

Spatially resolved environmental conditions were derived from a multidecadal hindcast of
the Chesapeake Bay Environmental Forecast System (CBEFS; Bever et al. 2021). CBEFS is a
real-time coupled hydrodynamic–biogeochemical model of Chesapeake Bay built on the
Chesapeake Bay implementation of the Regional Ocean Modeling System (ChesROMS; Xu et al.
2012), coupled to the Estuarine–Carbon–Biogeochemistry module (ECB; Feng et al. 2015)
that carries the carbon and nitrogen state variables — including phytoplankton, nitrate,
and dissolved oxygen — used here. The 1985–2024 hindcast was produced with this ROMS-ECB
system by Pierre St-Laurent (Virginia Institute of Marine Science) for the present study
(file series `v20260112`, generated 12 January 2026) and provided as 40 annual files of
daily-averaged fields. The hindcast is forced by North American Mesoscale (NAM)
atmospheric fields, open-boundary tides, water levels, and temperature/salinity from
operational ocean products, and freshwater discharge with associated riverine
biogeochemistry for the Bay's major tributaries (Bever et al. 2021).

Five state variables were used, each available at three vertical levels — bottom
(`_bott`), surface (`_surf`), and depth-averaged (`_davg`) — giving 15 variable–depth
combinations:

| Variable | Description | Units |
|---|---|---|
| `temperature` | potential temperature | °C |
| `salinity` | practical salinity | PSS-1978 |
| `diss_o2` | dissolved oxygen | mg O₂ L⁻¹ |
| `phytoplankton` | phytoplankton concentration | mmol N m⁻³ |
| `NO3` | nitrate + nitrite | mmol N m⁻³ |

The CBEFS model is integrated on a curvilinear grid with 20 terrain-following vertical
levels and a horizontal resolution of roughly 430 m in the northern Bay to ~1 km in the
middle and southern Bay (Bever et al. 2021). For this study the model output was supplied
as a regularized hindcast product rather than on that native grid: it was interpolated
onto a regular 336 (east–west) × 564 (north–south) oblique-stereographic grid
(`+proj=stere +lon_0=283.54 +lat_0=37.75`, ~600 m spacing) and reduced to three vertical
representations — bottom, surface, and depth-averaged. The exact longitude and latitude
of every delivery-grid cell are stored as two-dimensional `longitude(y,x)` and
`latitude(y,x)` arrays within each NetCDF file; this grid is regular in projection space
but curvilinear in geographic coordinates, which motivates the regridding approach below.
CBEFS has been evaluated against the Chesapeake Bay Program Water Quality Monitoring
Program at 13 mainstem stations using target-diagram skill metrics, with bottom
temperature, salinity, and dissolved-oxygen skill comparable to other established
Chesapeake Bay models (Bever et al. 2021; Irby et al. 2016); skill for this specific
1985–2024 hindcast is documented in the accompanying model atlas (St-Laurent 2026).

## Temporal aggregation

For the Ecospace application we required monthly fields rather than daily values.
Monthly means were computed in a single pass while each annual file was held in memory,
averaging all daily layers within each calendar month (ignoring missing values). This
avoided writing and then re-reading the very large combined daily archive (~47 GB),
which is chunked for storage rather than time-series access. The result is, for each of
the 15 variable–depth combinations, one multi-year monthly stack covering 1985–2024
(480 monthly layers).

## Coordinate system and regridding

The central methodological choice concerns how the curvilinear model output was placed
onto the regular geographic grids used by Ecospace. Because the CBEFS grid is
curvilinear in geographic space — its rows and columns are regular only in the native
oblique-stereographic projection — standard raster resampling (e.g. `terra::resample`,
which assumes a regular source grid) does not apply. Reconstructing the source
projection analytically would require assumptions about the datum and origin. We
therefore treated the stored `longitude`/`latitude` arrays as ground truth and regridded
through them directly. (The data provider had already interpolated the model output from
its native ChesROMS grid onto this 600 m stereographic delivery grid; the regridding
described here is the subsequent step that places those fields onto the Ecospace basemaps.)

Processing kept the native model fields in **index space with no coordinate reference
system assigned**, and georeferenced **once**, late in the pipeline, at the regridding
step. Two design consequences follow. First, the monthly stacks are stored north-up via
a single vertical flip applied when the data are read; the same flip is applied to the
longitude/latitude arrays when the regridding index is built, so cell ordering between
the data and its coordinates is guaranteed to match without any orientation guessing.
Second, the geographic transformation is performed exactly once per target grid rather
than repeatedly for every variable, depth, and month.

Regridding proceeds in two steps. A **regrid index** is built once per target basemap:
each source cell's (longitude, latitude) pair (with longitudes normalized from the
0–360° to the −180–180° convention) is mapped to the target basemap cell that contains
it. The native ~600 m resolution is finer than every target grid, so multiple source
cells fall within each target cell, and the mapping is many-to-one. The **regridded
value** for each target cell is then the mean of all source cells assigned to it,
computed as a vectorized group-mean across all 480 layers in a single pass; missing
source values are excluded per target cell, and target cells receiving no source
coverage are set to missing. Because the source is finer than the target, this
many-to-one averaging is the appropriate (mass-preserving in the mean sense) operation
for the fine→coarse transformation; on the order of 140,000 of the 189,504 native cells
fall within the bay-covering basemaps.

Coordinate reference handling is correspondingly explicit: native stacks carry no CRS
(index space); the target basemaps are assigned WGS84 (EPSG:4326) on load; the regridded
stacks inherit that geographic grid; and the final ASCII grids are written without a CRS,
since the ESRI ASCII format encodes geometry directly in its header and the Ecospace
basemap is itself stored CRS-less. The regridded monthly stacks are cached to disk (one
NetCDF per variable–depth per basemap) so that all downstream products read pre-regridded
data and no regridding is repeated.

## Multi-resolution targets

Drivers were produced for four Ecospace basemaps that share a common extent
(approximately −77.4 to −75.55°W, 36.7 to 39.65°N) but differ in resolution, generated
as integer aggregations of a base depth grid:

| Label | Grid (rows × cols) | Role |
|---|---|---|
| F01 | 176 × 111 | finest |
| F02 | 88 × 56 | **standard Ecospace grid** |
| F03 | 59 × 37 | coarse |
| F04 | 44 × 28 | coarsest |

Producing drivers at several resolutions supports sensitivity analysis of model
behavior to spatial grain and allows the Ecospace configuration to trade spatial detail
against computational cost. The native CBEFS grid (336 × 564, here labeled F00) was
retained for full-resolution visualization and comparison but is not used as an Ecospace
driver. Because the regrid index is rebuilt per basemap from the same source
coordinates, drivers at every resolution are mutually consistent and aligned to their
basemap to the cell.

## Driver products and quality assurance

For each variable–depth combination and each Ecospace basemap, the regridded monthly
stack was exported as a time series of ESRI ASCII grids (`<var>_<depth>_YYYY_MM.asc`),
the native format ingested by Ecospace, with missing values flagged as −9999. A 12-month
mean climatology was also exported for reference but is not wired in as a model driver. To
keep the driver directories clean for Ecospace import, only the `.asc` grids were written:
the auxiliary sidecar files that the GIS toolchain emits by default (a projection file and
two metadata files) were suppressed, as Ecospace reads only the ASCII header.

Two visualization products supported visual quality control across all resolutions
(including the native grid): multi-panel PDF figures (a 12-panel monthly climatology and a
per-year monthly sequence for each variable–depth) and GIF animations of the monthly
fields over user-selected year ranges. These were used to confirm that fields were
spatially coherent, correctly oriented, and free of regridding artifacts.

## Reproducibility and software

All processing was scripted in R 4.5.1 using the `terra` package for raster operations
and NetCDF I/O, with `ncdf4`, `gifski`, `viridisLite`, `stringr`, `dplyr`, and `tools`.
The workflow is organized as a three-stage pipeline (extraction/aggregation → per-basemap
regridding → product generation) orchestrated by a single entry-point script; the code
and its technical documentation are in the project repository
(github.com/holden-harris/Chesapeake-Ecospace, `make-environmental-drivers/`). The raw
CBEFS files and all large raster outputs are excluded from version control and are
regenerated by re-running the pipeline. Exact package versions can be pinned with `renv`
for archival reproducibility.

## Known limitations

The 1985–2024 monthly series contains a single duplicated month (January 2024) inherited
from the upstream extraction; because duplicate months resolve to the same output filename,
each variable yields 480 distinct monthly grids and the duplication does not propagate to
the drivers. This is documented as an open item to correct at the source.

## References

- Bever, A.J., Friedrichs, M.A.M., St-Laurent, P. (2021). Real-time environmental
  forecasts of the Chesapeake Bay: Model setup, improvements, and online visualization.
  *Environmental Modelling & Software* 140, 105036.
  https://doi.org/10.1016/j.envsoft.2021.105036  *(CBEFS system description)*
- Xu, J., Long, W., Wiggert, J.D., Lanerolle, L.W.J., Brown, C.W., Murtugudde, R.,
  Hood, R.R. (2012). Climate forcing and salinity variability in Chesapeake Bay, USA.
  *Estuaries and Coasts* 35(1), 237–261. https://doi.org/10.1007/s12237-011-9423-5
  *(ChesROMS hydrodynamic model)*
- Feng, Y., Friedrichs, M.A.M., Wilkin, J., Tian, H., Yang, Q., Hofmann, E.E.,
  Wiggert, J.D., Hood, R.R. (2015). Quantifying Chesapeake Bay nitrogen fluxes using a
  land–estuarine ocean biogeochemical modeling system: model description, evaluation and
  budgets. *Journal of Geophysical Research: Biogeosciences* 120, 1666–1695.
  https://doi.org/10.1002/2015JG002931  *(ECB biogeochemistry module)*
- Irby, I.D., Friedrichs, M.A.M., Friedrichs, C.T., Bever, A.J., Hood, R.R., et al.
  (2016). Challenges associated with modeling low oxygen waters in Chesapeake Bay: a
  multiple model comparison. *Biogeosciences* 13(7), 2011–2028.
  https://doi.org/10.5194/bg-13-2011-2016  *(multi-model skill context)*
- St-Laurent, P. (2026). Chesapeake Bay model atlas — hindcast skill documentation and
  modeled/observed data archive. https://doi.org/10.17882/99441
- Additional ROMS-ECB references supplied with the source data (full bibliographic
  details to confirm): https://doi.org/10.1029/2023MS003845 ;
  https://doi.org/10.1016/j.ecss.2025.109632
- Hijmans, R.J. (2024). *terra: Spatial Data Analysis*. R package.
- R Core Team (2025). *R: A Language and Environment for Statistical Computing*. R 4.5.1.

<!-- Bever 2021, Xu 2012, Feng 2015, Irby 2016 were verified from the Bever et al. (2021)
     reference list. The two DOIs carried in CBEFS-notes-metadata.txt (2023MS003845;
     ecss.2025.109632) still need full author/title/year — confirm with P. St-Laurent. -->
