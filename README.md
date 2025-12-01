# Measuring mixed land use with address-level census data

R code to read Brazilian CNEFE municipal files and compute mixed land use indicators from address records on H3 hexagonal grids.

This repository is a regular R project with scripts you can source and run in your own workflow.

------------------------------------------------------------------------

## Goals

This project provides a reproducible workflow to:

-   build an index of download URLs for the 2022 CNEFE municipal files
-   read raw CNEFE CSV data for a given municipality
-   compute land-use mix indicators on H3 hexagonal grids for Brazilian cities, using CNEFE address points

------------------------------------------------------------------------

## Repository contents

-   `R/`
    -   `build_cnefe_index_2022.R` – builds a spatial index of all Brazilian municipalities with:
        -   state and municipality identifiers
        -   municipality name and geometry
        -   URL of the corresponding CNEFE 2022 ZIP file on the IBGE FTP
    -   `read_cnefe.R` – reads CNEFE data for a given municipality, based on the index
    -   `compute_lumi.R` – computes land use mix indicators on H3 grids for a given municipality
-   `vignettes/`
    -   `example.R` – end-to-end example workflow:
        -   computes indicators for selected large cities
        -   optionally downloads arterial streets from OpenStreetMap
        -   produces maps and scatterplots used in the research

------------------------------------------------------------------------

## Requirements

You will need:

-   R (version 4.0 or later is recommended)
-   The following R packages:

``` r
required_pkgs <- c(
  "tidyverse",
  "sf",
  "geobr",
  "h3jsr",
  "arrow",
  "archive",
  "ggspatial",
  "patchwork"
)

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}
```

------------------------------------------------------------------------

## Getting started

### 1. Clone the repository

Using Git from the command line:

``` bash
git clone https://github.com/pedreirajr/lumi_cnefe.git
cd lumi_cnefe
```

Then open the R project (if using RStudio) and set the working directory to the repository root.

------------------------------------------------------------------------

### 2. Build or load the CNEFE index

The CNEFE index for 2022 is a spatial object with one row per municipality and the download URL for each CNEFE ZIP file. It can be built directly from the IBGE FTP.

From the project root:

``` r
source("R/build_cnefe_index_2022.R")

cnefe_index_2022 <- build_cnefe_index_2022(verbose = TRUE)

# Optionally save to disk for later reuse
saveRDS(cnefe_index_2022, "cnefe_index_2022.rds")
```

In later sessions you can simply load the saved object:

``` r
cnefe_index_2022 <- readRDS("cnefe_index_2022.rds")
```

------------------------------------------------------------------------

### 3. Read CNEFE data for a municipality

Once you have `cnefe_index_2022`, you can download and read the CNEFE file for a given municipality.

Example: Salvador (BA):

``` r
library(geobr)

source("R/read_cnefe.R")

# Look up municipal code for Salvador
ssa_code <- geobr::lookup_muni(name_muni = "Salvador")$code_muni

# Read CNEFE data for Salvador
cnefe_ssa <- read_cnefe(
  code_muni = ssa_code,
  index     = cnefe_index_2022,
  cache     = TRUE,          # keep the ZIP locally
  cache_dir = "data_cnefe",  # folder for cached downloads
  verbose   = TRUE
)
```

The result is an Arrow table that reflects the original CNEFE CSV file, with columns such as `LONGITUDE`, `LATITUDE`, `COD_ESPECIE` and many others.

------------------------------------------------------------------------

### 4. Compute land use mix indicators on H3 grids

The function `compute_lumi()` wraps reading and processing in a single call. It returns an `sf` object with H3 hexagons and indicators.

``` r
library(sf)
library(h3jsr)

source("R/compute_lumi.R")

# Example for Salvador, H3 resolution 9
ssa_code <- geobr::lookup_muni(name_muni = "Salvador")$code_muni

ssa_lumi_r9 <- compute_lumi(
  code_muni     = ssa_code,
  h3_resolution = 9,
  index         = cnefe_index_2022,
  verbose       = TRUE
)

ssa_lumi_r9
```

The output has at least the following columns:

-   `id_hex`: H3 cell id
-   `geometry`: hexagon geometry
-   `code_muni`, `name_muni`
-   address counts per `COD_ESPECIE` (wide format)
-   local proportions of residential vs non residential addresses
-   indicators:
    -   `ei`: entropy index (normalized between 0 and 1)
    -   `hhi`: traditional HHI based on residential vs others
    -   `hhi_adp`: adapted directional HHI, negative for non residential dominance and positive for residential dominance
    -   `bgbi`: Bidirectional Global centered Balance Index, with 0 at the global balance of residential use in the city

------------------------------------------------------------------------

### 5. Example workflow

The file `example.R` contains a more complete workflow:

-   defines a set of large Brazilian cities
-   computes indicators for multiple H3 resolutions
-   optionally downloads arterial streets from OpenStreetMap for each city
-   produces:
    -   city maps comparing EI, HHI, directional HHI and BGBI
    -   scatterplots comparing indicators
    -   maps comparing different H3 resolutions

You can run it step by step, or adapt parts of the script to your own analysis.

------------------------------------------------------------------------

## Notes and limitations

-   The current implementation is focused on **CNEFE 2022**. It assumes the structure and column names from that release. Changes in the IBGE FTP layout or CSV structure may require code updates.
-   The indicators are computed at the level of **H3 hexagons**, not at parcel or block level. Interpretation should consider the chosen H3 resolution.
-   Indicators such as BGBI depend on the **global residential share** in the city, which is estimated from the CNEFE address counts. Results may be sensitive to coverage and classification of addresses.
