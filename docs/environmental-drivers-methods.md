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
     Model description integrated from Bever et al. (2021); St-Laurent & Friedrichs (2024, JAMES, doi:10.1029/2023MS003845);
     and the Chesapeake Bay atlas documentation (St-Laurent & Friedrichs, doi:10.17882/99441, v2025-12-29).
     RESOLVED: the 336x564 ~600 m oblique-stereographic grid IS the hindcast model's own computational grid (not an
     interpolation); depth bands are bottom level, surface level, and water-column mean (per the atlas doc). Both
     previously-unresolved DOIs are now identified (see References). Remaining nicety: complete the bibliographic
     entries for the ERA5 (Hersbach 2023) and Phase 6 watershed (Bhatt 2023) forcing datasets. -->

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
daily-averaged fields. It is forced by the ERA5 atmospheric reanalysis (Hersbach et al.
2023), terrestrial freshwater and nutrient loads from the Chesapeake Bay Program Phase 6
watershed model (Bhatt et al. 2023, distributed across ECB state variables following Irby
and Friedrichs 2019), and atmospheric nitrogen deposition following Da et al. (2018)
(St-Laurent and Friedrichs 2024).

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

The hindcast was run on the model's own horizontal grid: a 336 (east–west) × 564
(north–south) grid that is regular in an oblique stereographic projection
(`+proj=stere +lon_0=283.54 +lat_0=37.75`) at ~600 m resolution. This uniform 600 m
configuration was developed for the multidecadal hindcast and atlas (St-Laurent and
Friedrichs 2024), refining the coarser ~430 m–1 km grid of the earlier real-time forecast
system (Bever et al. 2021). The model resolves the vertical with 20 terrain-following
levels; the fields delivered here reduce that dimension to three representations per
variable — bottom (the bottom level), surface (the surface level), and depth-averaged (the
water-column mean from surface to bottom). The model bathymetry is a mosaic compiled from
regional bathymetric and coastal-plain elevation datasets (National Geophysical Data Center
1999; Forte et al. 2011; Pope et al. 2016; Ye et al. 2017; NOAA 2022). The exact longitude
and latitude of every grid
cell are stored as two-dimensional `longitude(y,x)` and `latitude(y,x)` arrays within each
NetCDF file; because the grid is regular in projection space but curvilinear in geographic
coordinates, it motivates the regridding approach below. The hindcast has been evaluated
against the Chesapeake Bay Program Water Quality Monitoring Program (over three million
matched model–observation pairs across 13 variables), with skill comparable to other
established Chesapeake Bay models (Bever et al. 2021; Irby et al. 2016); skill diagnostics
for this 1985–2024 hindcast are distributed with the atlas (St-Laurent and Friedrichs 2024).

## Temporal aggregation

For the Ecospace application, we required monthly fields rather than daily values.
Monthly means were computed in a single pass while each annual file was held in memory,
averaging all daily layers within each calendar month (ignoring missing values). This
avoided writing and then re-reading the very large combined daily archive (~47 GB),
which is chunked for storage rather than time-series access. The result is, for each of
the 15 variable–depth combinations, one multi-year monthly stack covering 1985–2024
(480 monthly layers).

## Coordinate system and regridding

Regridding the CBEFS outputs for Ecospace meant placing outputs from a curvilinear model 
onto the regular geographic grids used by Ecospace. Because the CBEFS grid is
curvilinear in geographic space (i.e., its rows and columns are regular only in the native
oblique-stereographic projection) standard raster resampling (e.g. `terra::resample`,
which assumes a regular source grid) does not apply. Reconstructing the source
projection analytically would require assumptions about the datum and origin. We
therefore treated the stored `longitude`/`latitude` arrays as ground truth and regridded
through them directly. (This 600 m grid is the model's own computational grid, so the
regridding described here — onto the Ecospace basemaps — is the only horizontal
interpolation applied to the fields; the provider reduced only the vertical dimension to
the three depth representations.)

Processing kept the native model fields in **index space with no coordinate reference
system assigned**, and georeferenced **once**, late in the pipeline, at the regridding
step. Two design consequences follow. First, the monthly stacks are stored north-up via
a single vertical flip applied when the data are read; the same flip is applied to the
longitude/latitude arrays when the regridding index is built, so cell ordering between
the data and its coordinates is guaranteed to match without any orientation guessing.
Second, the geographic transformation is performed exactly once per target grid rather
than repeatedly for every variable, depth, and month.

Regridding proceeds in two steps. First, a **regrid index** is built once per target basemap:
each source cell's (longitude, latitude) pair (with longitudes normalized from the
0–360° to the −180–180° convention) is mapped to the target basemap cell that contains
it. The native ~600 m resolution is finer than every target grid, so multiple source
cells fall within each target cell, and the mapping is many-to-one. Second, the **regridded
value** for each target cell is then computed as the mean of all source cells assigned to it. 
Specifically, it is calculated as a vectorized group-mean across all 480 layers in a single pass; missing
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
| F02 | 88 × 56   | fine |
| F03 | 59 × 37   | coarse |
| F04 | 44 × 28   | coarsest |

Producing drivers at several resolutions supports sensitivity analysis of model
behavior to spatial grain and allows the Ecospace configuration to trade spatial detail
against computational cost. The native CBEFS grid (336 × 564, here labeled F00) was
retained for full-resolution visualization and comparison but is not used as an Ecospace
driver. 

## Driver products and quality assurance

For each variable–depth combination and each Ecospace basemap, the regridded monthly
stack was exported as a time series of ESRI ASCII grids (`<var>_<depth>_YYYY_MM.asc`),
the native format ingested by Ecospace, with missing values flagged as −9999. 

A 12-month mean climatology was also exported for reference but is not wired in as a model driver. To
keep the driver directories clean for Ecospace import, only the `.asc` grids were written:
the auxiliary sidecar files that the GIS toolchain emits by default (a projection file and
two metadata files) were suppressed, as Ecospace reads only the ASCII header.

Two visualization products supported visual quality control across all resolutions
(including the native grid): 
 - multi-panel PDF figures (a 12-panel monthly climatology and a
per-year monthly sequence for each variable–depth) and 
 - GIF animations of the monthly fields over user-selected year ranges. 

These were used to confirm that fields were spatially coherent, correctly oriented, and free of regridding artifacts.

## Reproducibility and software

All processing was scripted in R 4.5.1 using the `terra` package for raster operations
and NetCDF I/O, with `ncdf4`, `gifski`, `viridisLite`, `stringr`, `dplyr`, and `tools`.
The workflow is organized as a three-stage pipeline (extraction/aggregation → per-basemap
regridding → product generation) orchestrated by a single entry-point script; the code
and its technical documentation are in the project repository
(github.com/holden-harris/Chesapeake-Ecospace, `make-environmental-drivers/`). The raw
CBEFS files and all large raster outputs are excluded from version control and are
regenerated by re-running the pipeline. Exact package versions are pinned with `renv`
(`renv.lock`) for archival reproducibility.

## Known limitations

The 1985–2024 monthly series contains a single duplicated month (January 2024) inherited
from the upstream extraction; because duplicate months resolve to the same output filename,
each variable yields 480 distinct monthly grids and the duplication does not propagate to
the drivers. This is documented as an open item to correct at the source.

## References

- Bever, A.J., Friedrichs, M.A.M., St-Laurent, P. (2021). Real-time environmental
  forecasts of the Chesapeake Bay: Model setup, improvements, and online visualization.
  *Environmental Modelling & Software* 140, 105036.
  https://doi.org/10.1016/j.envsoft.2021.105036  *(CBEFS real-time system description)*
- St-Laurent, P., Friedrichs, M.A.M. (2024). On the sensitivity of coastal hypoxia to its
  external physical forcings. *Journal of Advances in Modeling Earth Systems* 16,
  e2023MS003845. https://doi.org/10.1029/2023MS003845
  *(600 m, 20-level hindcast model configuration)*
- St-Laurent, P., Friedrichs, M.A.M. (2024). An atlas for physical and biogeochemical
  conditions in the Chesapeake Bay (documentation v2025-12-29). SEANOE.
  https://doi.org/10.17882/99441  *(hindcast data product, depth bands, skill diagnostics)*
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
- Da, F., Friedrichs, M.A.M., St-Laurent, P. (2018). Impacts of atmospheric nitrogen
  deposition and coastal nitrogen fluxes on oxygen concentrations in Chesapeake Bay.
  *Journal of Geophysical Research: Oceans* 123, 5004–5025.
  https://doi.org/10.1029/2018JC014009  *(atmospheric N deposition)*
- St-Laurent, P. (2026). Water quality impacts during Hurricane Irene (2011) in a large
  coastal-plain estuary. *Estuarine, Coastal and Shelf Science* 329, 109632.
  https://doi.org/10.1016/j.ecss.2025.109632  *(application of the same hindcast)*
- Hersbach, H., Bell, B., Berrisford, P., Biavati, G., Horányi, A., Muñoz-Sabater, J.,
  Nicolas, J., Peubey, C., Radu, R., Rozum, I., Schepers, D., Simmons, A., Soci, C.,
  Dee, D., Thépaut, J.-N. (2023). ERA5 hourly data on single levels from 1940 to present.
  Copernicus Climate Change Service (C3S) Climate Data Store.
  https://doi.org/10.24381/cds.adbb2d47  *(atmospheric forcing)*
- Bhatt, G., Linker, L., Shenk, G., Bertani, I., Tian, R., Rigelman, J., Hinson, K.,
  Claggett, P. (2023). Water quality impacts of climate change, land use, and population
  growth in the Chesapeake Bay watershed. *Journal of the American Water Resources
  Association*, 1–29. https://doi.org/10.1111/1752-1688.13144
  *(Phase 6 watershed terrestrial loads)*
- Irby, I.D., Friedrichs, M.A.M. (2019). Evaluating confidence in the impact of regulatory
  nutrient reduction on Chesapeake Bay water quality. *Estuaries and Coasts* 42, 16–32.
  https://doi.org/10.1007/s12237-018-0440-5  *(distribution of loads to ECB state variables)*
- National Geophysical Data Center (1999). 3 arc-second Coastal Relief Model (CRM),
  Volume 2 (Southeast Atlantic). NOAA. https://doi.org/10.7289/V53R0QR5
  *(model bathymetry source)*
- Forte, M.F., Hanson, J.L., Stillwell, L., Blanchard-Montgomery, M., Blanton, B.,
  Luettich, R., Roberts, H., Atkinson, J., Miller, J. (2011). FEMA Region III storm surge
  study: Coastal storm surge analysis system digital elevation model. ERDC/CHL TR-11-1,
  U.S. Army Corps of Engineers, Engineer Research and Development Center.
  *(model topography source)*
- Pope, J.P., Andreasen, D.C., McFarland, E.R., Watt, M.K. (2016). Digital elevations and
  extents of regional hydrogeologic units in the northern Atlantic Coastal Plain aquifer
  system from Long Island, New York, to North Carolina. U.S. Geological Survey data
  release. https://doi.org/10.5066/F70V89WN  *(model topography source)*
- Ye, F., Zhang, Y.J., Wang, H.V., Friedrichs, M.A.M., Irby, I.D., Valle-Levinson, A.,
  Wang, Z., Huang, H., Shen, J., Du, J. (2017). Assessment of a 3D unstructured-grid model
  for the Chesapeake Bay and adjacent shelf: Supplementary materials. William & Mary
  ScholarWorks. https://doi.org/10.21220/V5HK5S  *(model bathymetry source)*
- NOAA (2022). Estuarine bathymetric digital elevation models. NOAA National Centers for
  Environmental Information (NCEI).
  https://www.ngdc.noaa.gov/mgg/bathymetry/estuarine/index.html (accessed 2022-11-02)
  *(model bathymetry source)*
- Hijmans, R.J. (2024). *terra: Spatial Data Analysis*. R package.
- R Core Team (2025). *R: A Language and Environment for Statistical Computing*. R 4.5.1.

<!-- All references verified against the source PDFs in resources/CBEFS/ (Bever 2021;
     St-Laurent & Friedrichs 2024 JAMES; the Chesapeake Bay atlas documentation + its
     reference list for ERA5/Phase 6/Irby & Friedrichs; St-Laurent 2026). -->
