# Replication Package: Omitted Variable Bias in Wetland-Flood Regressions

**Matthew Cherfane and Will Rafey**, UCLA Department of Economics

This repository contains data and code needed to reproduce the results in our paper,
"Omitted variable bias in wetland-flood regressions" (available at <https://rafey.io/files/floods.pdf>), a
comment on Taylor and Druckenmiller (2022), "Wetlands, Flooding, and the
Clean Water Act," *American Economic Review* 112(4): 1334–63 (hereafter TD).

Our analysis starts from TD's zip-code-level long-difference dataset. For some of our results, we also rely on three new
sources of information on flood exposure and flood risk: (i) USGS observations of
Hurricane Harvey flooding, (ii) zip-code elevation, and (iii) FEMA flood hazard zones.
This README explains where we obtain the data (Section 1), what you need to run
the code (Section 2), what the code does (Section 3), how to replicate our results (Section 4), and the map between the code and each table and figure (Section 5).

**License.** All rights reserved. See [`LICENSE`](LICENSE).

## Contents

1. [Data sources and access](#1-data-sources-and-access)
2. [Computational requirements](#2-computational-requirements)
3. [Description of code](#3-description-of-code)
4. [Instructions to replicators](#4-instructions-to-replicators)
5. [List of tables, figures, and programs](#5-list-of-tables-figures-and-programs)
6. [References](#6-references)

---

## 1. Data sources and access

Appendix A of the paper ("Data Sources and Sample Construction") describes each data source
in more detail. This section covers provenance and access.

### 1.1 Statement about rights

All data used in this project are publicly available. Data we constructed (elevation and
FEMA flood-zone shares at the zip-code level) and the USGS and Census files we use directly
are included in this repository. So are the files we use from TD's publicly available
replication package (Section 1.2), so that the analysis does not depend on that package
remaining unchanged online. The one exception is TD's copy of the national Census ZCTA
shapefile, which exceeds GitHub's file-size limit and is available directly from the
Census Bureau.

### 1.2 Taylor and Druckenmiller (2022) replication package

**Source.** Taylor, Charles A., and Hannah Druckenmiller. Data and Code for: "Wetlands,
Flooding, and the Clean Water Act." Version V1 (2022-03-25). Ann Arbor, MI:
Inter-university Consortium for Political and Social Research.
<https://www.openicpsr.org/openicpsr/project/153221/version/V1/view>

**Access.** The full package is a free download from openICPSR (requires an account). This
repository includes the files our code uses, in `replication_files_TD/`, with the same
folder structure as the original package. The only file not included is the 2010 Census
ZCTA shapefile (about 850 MB), which replicators must download from the Census Bureau
(Section 4).

**Files from TD used** (paths relative to `replication_files_TD/`).

| File | Contents |
|---|---|
| `data/final/zip_LD_data.csv` | TD's zip-code long-difference dataset (NFIP claims growth, wetland change, controls) |
| `data/final/codebook_final.xlsx` | TD's codebook for the final datasets (included for reference) |
| `data/geo/coastal_zips.csv` | TD's coastal zip-code indicator |
| `data/geo/zip_state_crosswalk.Rdata` | Zip-to-state crosswalk, used to restrict to Texas and Louisiana |
| `data/geo/tl_2019_us_zcta510/` | 2010 Census ZCTA polygons (TIGER/Line 2019), about 850 MB. **Not included**; download `tl_2019_us_zcta510.zip` from the Census Bureau's TIGER/Line 2019 ZCTA5 files (<https://www2.census.gov/geo/tiger/TIGER2019/ZCTA5/>) |
| `data/geo/tl_2020_us_state/` | 2020 Census state boundaries (TIGER/Line) |

### 1.3 USGS Hurricane Harvey observations

**Source.** U.S. Geological Survey, Short-Term Network (STN) Flood Event Data, Hurricane
Harvey (event 180, August–September 2017). <https://stn.wim.usgs.gov/STNDataPortal/>

**Access.** Public. Downloaded August 2023 with the commands in
`data/raw/usgs_harvey/wget_usgs_harvey.sh`.

| File | Contents | Used for |
|---|---|---|
| `data/raw/usgs_harvey/peak_summary.csv` | Peak-stage summaries with coordinates, peak stage, and height above ground | Harvey indicator and intensity measures |
| `data/raw/usgs_harvey/filtered_hwms.csv` | High-water marks with coordinates and elevation | High-water-mark count |
| `data/raw/usgs_harvey/filtered_sensors.csv` | Sensor deployments | Not used directly; included for completeness |

### 1.4 U.S. Census Bureau

| Data | Source | Access |
|---|---|---|
| 2010 ZCTA-to-county relationship file (`data/raw/census/zcta_county_rel_10.txt`) | <https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.2010.html> | Public; included |
| 2020 cartographic-boundary ZCTAs and Texas places | Downloaded at run time by the `tigris` R package in `06_extensions.R` | Public; requires internet |

### 1.5 Elevation (SimpleMaps and Open-Elevation)

**Source.** Zip-code representative coordinates from the SimpleMaps US Zip Codes Database,
Basic version 1.94 (<https://simplemaps.com/data/us-zips>). Elevation in meters at each
coordinate from the Open-Elevation API (<https://open-elevation.com>).

**Access.** Both are free. The resulting file, `data/derived/txla_zip_elevation.csv`
(Texas and Louisiana ZCTAs: `zip`, `lat`, `lng`, `elevation_m`), is included. The code that
queried the API is at the end of `01_build_data.R` (commented out; it takes several minutes
and requires the SimpleMaps file, which is not redistributed here).

`elevation_m` is a representative-point elevation, not an area-weighted average over the
zip polygon.

### 1.6 FEMA National Flood Hazard Layer (NFHL)

**Source.** Federal Emergency Management Agency, National Flood Hazard Layer.
<https://msc.fema.gov/portal/advanceSearch>

**Access.** Public, but large: each state is downloaded separately as a geodatabase
(about 33 GB to download and roughly 100 GB once extracted). The raw files are not
included. We downloaded them between August and October 2023 by selecting
`A STATE, ANY COU, ANY COMM`, choosing `CURRENT`, and downloading `NFHL DATA-STATE`. The
exact product IDs (one per state) are listed in Appendix A.5 of the paper. To rerun the processing, place each extracted state folder (for example
`NFHL_48_20230902/`, containing the `.gdb`) in `data/raw/fema_nfhl/each_state/`.

**Processing.** `code/floods_A2_import_nfhl.R` reads the flood hazard polygons
(`S_FLD_HAZ_AR` layer) for a state, unions all polygons in Zone A (`A`, `AE`, `AH`, `AO`,
`A99`; the one-percent annual chance floodplain), Zone V (`V`, `VE`; coastal high-hazard
areas subject to wave action), and Zone X, intersects each union with that state's ZCTA
polygons, and divides by ZCTA area. It runs one state at a time (the loop index selects the
state; as saved, it runs Texas) and writes `data/derived/nfhl_pct_zip_<FIPS>.RData`. The
per-state results for all states other than Texas are stacked in `nfhl_z_df0.rds`. Zone X (low to moderate flood risk)
shares are computed but not used. We include the output, so replicators do not need the
raw NFHL files:

| File | Contents |
|---|---|
| `data/derived/nfhl_pct_zip_48.RData` | Object `z_df`: Texas zip codes with `pct_A`, `pct_V` |
| `data/derived/nfhl_z_df0.rds` | All other states: `zip`, `pct_A`, `pct_V` |

`pct_A` and `pct_V` range from 0 to 1. The analysis code defines `pct_nfhl = pct_A + pct_V`
and rescales all three to percentage points (`share_A`, `share_V`, `share_nfhl`) for the
tables. Our specific flood zone calculations did take us a week or two to download and construct; if you or your LLM use them, please cite our paper and this replication archive. 

---

## 2. Computational requirements

All code is written in R. Required packages:

AER, car, conflicted, corrplot, dplyr, ggplot2, knitr, lmtest, lwgeom, raster, readr,
sandwich, scales, sf, stargazer, stringr, tidyr, tigris, units, xtable.

`sf` requires the GDAL, GEOS, and PROJ system libraries, which install automatically with
`sf` on macOS and Windows.

Scripts `00`–`06` run on a laptop. The most memory-intensive steps are reading the national
ZCTA shapefile and the point-in-polygon and distance calculations in `01_build_data.R`.
Running `floods_A2_import_nfhl.R` requires the raw NFHL files (roughly 100 GB extracted),
and the polygon unions and intersections for large states are slow and memory-intensive.
Its outputs are therefore included.

---

## 3. Description of code

### 3.1 Folder structure

```
floods/
├── README.md
├── LICENSE
├── floods.Rproj                 open in RStudio to set the working directory
├── code/                        R scripts (Section 3.2)
├── data/
│   ├── raw/usgs_harvey/         USGS STN Harvey files (Section 1.3)
│   ├── raw/census/              Census ZCTA-to-county relationship file (Section 1.4)
│   ├── raw/fema_nfhl/           empty: place FEMA NFHL state files here to rerun A2 (Section 1.6)
│   └── derived/                 elevation, NFHL shares, city lookups (Sections 1.5–1.6)
├── replication_files_TD/        files used from TD's replication package (Section 1.2)
└── output/
    ├── tables/                  LaTeX tables written by the code
    └── figures/                 PNG images of the maps written by the code
```

### 3.2 Scripts

Scripts `00`–`06` must be run in numerical order in a single R session, because each uses
objects created by antecedent scripts.

**Data construction**

- `floods_A2_import_nfhl.R`: computes zip-code Zone A and Zone V land shares from the raw
  FEMA NFHL files, one state at a time (Section 1.6). Its outputs are included, so it does
  not need to be run.
- `01_build_data.R`: loads TD's data, builds the baseline estimating sample, and constructs
  the Harvey indicator and intensity measures (Section 3.3).

**Analysis**

- `00_tex_functions.R`: helper functions (`teX()`, `deathstar()`) that write regression
  tables to LaTeX.
- `02_regressions.R`: merges elevation and NFHL shares and estimates the regressions for
  the added-controls, NFHL, elevation, coastal, and identification specifications.

**Results**

- `03_tables_raw.R`: writes the regressions from `02` to raw LaTeX tables.
- `04_tables_format.R`: rewrites the control-panel rows and labels of those tables.
- `05_figures.R`: Texas–Louisiana maps and summary tables.
- `06_extensions.R`: Tables 1–4, state-by-state estimates, winsorization and trimming
  robustness, the consolidated identification tables, and Figure 1.

### 3.3 How the new variables enter the analysis data

The results in Section 2 of our paper rely only on the TD data. Section 3 uses the new variables, which we graft onto TD's zip-code data by 5-digit zip code. Zip codes are 2010 ZCTAs throughout.

| Data frame | Built in | Contents | Rows |
|---|---|---|---|
| `td` | `01` | TD's `zip_LD_data.csv`, unmodified | — |
| `td0` | `01` | Baseline TD estimating sample: zip codes with nonmissing claims growth, wetland change, all TD controls, state FIPS, and county FIPS, merged with TD's coastal indicator. Harvey variables are added below. | 25,734 |
| `td_txla` | `01` | `td0` restricted to Texas and Louisiana (an `sf` object) | 1,982 |
| `td_harvey` | `02` | `td_txla` restricted to `ind_Harvey == 1` | 242 |
| `td_txla_elev`, `td_harvey_elev` | `02` | The above plus `elevation_m` | 1,982 / 242 |
| `td_harvey_nfhl`, `td_harvey_nfhl_elev` | `02` | Harvey zips plus `pct_A`, `pct_V`, `pct_nfhl`, `share_*` (and elevation) | 242 |
| `td0_nfhl` | `02` | `td0` plus national NFHL shares | 22,680 matched |

**Harvey variables** (added to `td0` in `01_build_data.R`). USGS peak-summary and
high-water-mark points are assigned to ZCTA polygons by point-in-polygon overlay.

| Variable | Definition |
|---|---|
| `ind_Harvey` | 1 if the zip contains at least one Harvey peak-summary point (261 TX/LA ZCTAs; 242 in `td0`) |
| `height_above_gnd_max` | Maximum height above ground among peak-summary points in the zip; 0 if none |
| `harvey_hwm_count` | Number of high-water marks in the zip |
| `harvey_peak_stage_max`, `harvey_peak_stage_mean` | Maximum and mean peak stage in the zip; 0 if none |
| `harvey_any_peak`, `harvey_any_hwm`, `harvey_hwm_elev_max`, `height_above_gnd_mean` | Additional point summaries |

**Elevation and NFHL variables** (merged in `02_regressions.R`): `elevation_m`
(Section 1.5); `pct_A`, `pct_V`, `pct_nfhl`, and their percentage-point versions `share_A`,
`share_V`, `share_nfhl` (Section 1.6).

**Sample restrictions** used in the tables: the 99pct and 98pct samples drop zip codes
outside the 0.5–99.5 and 1–99 percentiles of claims growth (25,476 and 25,218 zips); the No
TX/LA sample drops Texas and Louisiana (23,752 zips).

### 3.4 A note on the table files

`output/tables/` contains several versions of most tables. Tables are first written by
`teX()`, whose control-panel rows and labels are fixed; `04_tables_format.R` and
`06_extensions.R` then rewrite those rows with the correct entries. Files ending in
`_all_NEW.tex` or `_clean.tex` are the final versions. Other versions are intermediate
files. Section 5 lists the file behind each table in the paper.

---

## 4. Instructions to replicators

1. If you want maps, download `tl_2019_us_zcta510.zip` from the Census Bureau
   (<https://www2.census.gov/geo/tiger/TIGER2019/ZCTA5/>), unzip it, and place its
   contents in `replication_files_TD/data/geo/tl_2019_us_zcta510/`, so that
   `replication_files_TD/data/geo/tl_2019_us_zcta510/tl_2019_us_zcta510.shp` exists. All
   other input data are included.
2. Open `floods.Rproj` in RStudio. All file paths in the code are relative to the top of
   the repository.
3. Run `code/00_tex_functions.R` through `code/06_extensions.R` in order, in one R session.
   Tables are written to `output/tables/` and figures to `output/figures/`.

`floods_A2_import_nfhl.R` does not need to be run; its outputs are in `data/derived/`. To
run it, first download the NFHL state files described in Section 1.6.

---

## 5. List of tables, figures, and programs

Line numbers refer to the location in the script that writes each output file.

| Exhibit | Title | Program |
|---|---|---|
| Figure 1 | NFIP Claims Change in Texas and Louisiana | `06_extensions.R`, line 827 |
| Table 1 | Reassessing the Effect of Wetlands on Flood Damage | `06_extensions.R`, line 174 |
| Table 2 | Recalculating Average Wetland Value | `06_extensions.R`, line 264 |
| Table 3 | Claims, Wetlands, Elevation, and Flood Hazard Summary | `06_extensions.R`, line 724 |
| Table 4 | Harvey Exposure and Flood-Risk Geography Predict Wetland Change | `06_extensions.R`, line 589 |
| Table A1 | State-by-State Baseline Wetland Coefficients | `06_extensions.R`, line 80 |
| Table A2 | Reassessing the Effect of Wetlands on Flood Damage (Winsorized) | `06_extensions.R`, line 373 |
| Table A3 | Long Difference Regressions with Added Controls | `04_tables_format.R`, line 107 |
| Table A4 | Long Difference Regressions with NFHL Layer Controls | `04_tables_format.R`, line 270 |
| Table A5 | Harvey Exposure and Wetland Change in Texas and Louisiana | `06_extensions.R`, line 629 |
| Table A6 | Harvey Exposure and Wetland Loss in Texas and Louisiana | `04_tables_format.R`, line 221 |
| Figure A1 | Net Wetland Change in Texas and Louisiana | `05_figures.R`, line 153 |
| Figure A2 | FEMA Flood Hazard (NFHL) Exposure in Texas and Louisiana | `05_figures.R`, line 60 |
| Figure A3 | Elevation in Texas and Louisiana | `05_figures.R`, line 169 |
| Figure A4 | FEMA Flood Hazard Zone A Exposure in Texas and Louisiana | `05_figures.R`, line 77 |
| Figure A5 | FEMA Flood Hazard Zone V Exposure in Texas and Louisiana | `05_figures.R`, line 87 |
| Figure A6 | Wetland Area in Texas and Louisiana, 2001 | `05_figures.R`, line 210 |
| Figure A7 | Wetland Area in Texas and Louisiana, 2016 | `05_figures.R`, line 221 |

The paper uses PNG images of the map PDFs (in `output/figures/`); the full-resolution PDFs
(about 9 MB each) are regenerated by the code but not stored in the repository.

---

## 6. References

Federal Emergency Management Agency. National Flood Hazard Layer (NFHL). State
geodatabases, versions as listed in Appendix A.5 of the paper, accessed August–October 2023.
<https://msc.fema.gov/portal/advanceSearch>

Open-Elevation. Open-Elevation API. <https://open-elevation.com>

SimpleMaps. US Zip Codes Database, Basic v1.94. <https://simplemaps.com/data/us-zips>

Taylor, Charles A., and Hannah Druckenmiller. 2022. "Wetlands, Flooding, and the Clean
Water Act." *American Economic Review* 112(4): 1334–63.

Taylor, Charles A., and Hannah Druckenmiller. 2022. Data and Code for: "Wetlands, Flooding,
and the Clean Water Act." Version V1. Inter-university Consortium for Political and
Social Research. <https://www.openicpsr.org/openicpsr/project/153221/version/V1/view>

U.S. Census Bureau. 2010 ZCTA to County Relationship File.
<https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.2010.html>

U.S. Geological Survey. Short-Term Network (STN) Flood Event Data: Hurricane Harvey (event
180). Accessed August 2023. <https://stn.wim.usgs.gov/STNDataPortal/>
