# ---------------------------------------------------------------------------
# Package loading with auto-install
# ---------------------------------------------------------------------------

required_pkgs <- c(
  "tidyverse",  # dplyr, tidyr, purrr, readr, stringr, tibble, etc.
  "sf",
  "geobr",
  "rvest",
  "archive",
  "arrow",
  "glue",
  "h3jsr"
)

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Compute mixed land use indicators on H3 hexagons ------------------------
# 
# This function:
#   1) Reads CNEFE for a municipality and minimally cleans the data
#   2) Builds the municipal polygon (from the index sf object)
#   3) Generates an H3 grid covering the municipality
#   4) Assigns each CNEFE point to an H3 cell
#   5) Counts addresses by land-use category (COD_ESPECIE)
#   6) Computes per-cell proportions of residential vs. non-residential
#   7) Computes:
#      - Local entropy index (ei)
#      - Herfindahl-Hirschman index (hhi) and scaled version (hhi_sc)
#      - Directional HHI (hhi_adp)
#      - Bidirectional Global-centered Balance Index (bgbi)
# 
# Returns:
#   An sf object of H3 hexagons with all indicators and municipality metadata.
compute_lumi <- function(code_muni,
                         h3_resolution = 9,
                         index   = cnefe_index_2022,
                         verbose = TRUE) {
  
  code_muni <- as.integer(code_muni)
  
  # Look up municipality row in the index (with geometry)
  muni_row <- index %>%
    filter(.data$code_muni == !!code_muni)
  if (nrow(muni_row) == 0) {
    stop("Municipality code not found in `index`: ", code_muni)
  }
  
  city_name <- muni_row$name_muni[1]
  if (verbose) message(glue("Processing {city_name} (code {code_muni})..."))
  
  # ---------------------------------------------------------------------------
  # 1. Read CNEFE (raw table) and apply minimal preprocessing
  # ---------------------------------------------------------------------------
  tab <- read_cnefe(code_muni, index = index, verbose = verbose, cache = TRUE)
  
  # Convert to data.frame and standardize column names to upper case
  end <- as.data.frame(tab, stringsAsFactors = FALSE)
  names(end) <- toupper(names(end))
  
  # Handle possible alternative name for COD_ESPECIE
  if (!"COD_ESPECIE" %in% names(end) && "CODIGO_ESPECIE" %in% names(end)) {
    end <- end %>% rename(COD_ESPECIE = CODIGO_ESPECIE)
  }
  
  # Check that required columns are present
  required_cols <- c("LONGITUDE", "LATITUDE", "COD_ESPECIE")
  if (!all(required_cols %in% names(end))) {
    stop("Required columns not found in CNEFE for ", city_name)
  }
  
  # Cleaning and standardization of coordinates and land use codes
  end <- end %>%
    mutate(
      LONGITUDE = as.numeric(gsub(",", ".", as.character(LONGITUDE))),
      LATITUDE  = as.numeric(gsub(",", ".", as.character(LATITUDE)))
    ) %>%
    filter(!is.na(LONGITUDE), !is.na(LATITUDE)) %>%
    filter(!is.na(COD_ESPECIE)) %>%
    filter(COD_ESPECIE != 7)  # remove "under construction"
  
  if (nrow(end) == 0) {
    warning(glue("No valid points found for {city_name}."))
    return(NULL)
  }
  
  # Detect coordinates in micro-degrees (e.g., integer values > 180) and rescale
  max_abs_lon <- max(abs(end$LONGITUDE), na.rm = TRUE)
  if (max_abs_lon > 180) {
    if (verbose) message("Detected coordinates in microdegrees; dividing by 1e6.")
    end <- end %>%
      mutate(
        LONGITUDE = LONGITUDE / 1e6,
        LATITUDE  = LATITUDE  / 1e6
      )
  }
  
  # Convert to sf points in SIRGAS 2000 and then to WGS 84
  end_sf <- end %>%
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4674) %>%
    st_transform(4326)
  
  # ---------------------------------------------------------------------------
  # 2. Municipal polygon (from index geometry)
  # ---------------------------------------------------------------------------
  muni <- muni_row %>%
    st_as_sf() %>%
    st_make_valid() %>%
    st_union() %>%
    st_cast("MULTIPOLYGON") %>%
    st_transform(4326)
  
  # ---------------------------------------------------------------------------
  # 3. H3 grid covering the municipal polygon
  # ---------------------------------------------------------------------------
  if (verbose) message("Generating H3 grid...")
  muni_h3 <- unlist(
    h3jsr::polygon_to_cells(muni, res = h3_resolution, simple = TRUE),
    use.names = FALSE
  )
  
  hex_grid <- st_sf(
    id_hex   = muni_h3,
    geometry = st_sfc(h3jsr::cell_to_polygon(muni_h3)),
    crs      = 4326
  )
  
  # ---------------------------------------------------------------------------
  # 4. Spatial join: assign each CNEFE point to an H3 hexagon
  # ---------------------------------------------------------------------------
  pts_in_hex <- st_join(end_sf, hex_grid, left = FALSE)
  if (nrow(pts_in_hex) == 0) {
    warning(glue("No points fell inside the H3 grid for {city_name}."))
    return(
      hex_grid %>%
        mutate(
          ei        = NA_real_,
          hhi       = NA_real_,
          hhi_adp   = NA_real_,
          bgbi      = NA_real_,
          code_muni = code_muni,
          name_muni = city_name
        )
    )
  }
  
  # ---------------------------------------------------------------------------
  # 5. Compute land use indicators by hexagon
  # ---------------------------------------------------------------------------
  
  # Count addresses by hexagon and COD_ESPECIE
  landuse_counts <- pts_in_hex %>%
    st_drop_geometry() %>%
    count(id_hex, COD_ESPECIE, name = "count")
  
  # Wide table: one column per COD_ESPECIE category
  landuse_wide <- landuse_counts %>%
    pivot_wider(
      names_from   = COD_ESPECIE,
      values_from  = count,
      names_prefix = "COD_ESPECIE",
      values_fill  = 0
    )
  
  # Ensure residential column exists (COD_ESPECIE == 1)
  if (!"COD_ESPECIE1" %in% names(landuse_wide)) {
    warning("No residential establishments (COD_ESPECIE == 1) in ", city_name)
    landuse_wide$COD_ESPECIE1 <- 0L
  }
  
  # Global residential proportion P = (total residential) / (total addresses)
  P <- landuse_wide %>%
    mutate(
      tot = rowSums(across(starts_with("COD_ESPECIE")), na.rm = TRUE)
    ) %>%
    summarize(
      P = sum(COD_ESPECIE1, na.rm = TRUE) / sum(tot, na.rm = TRUE)
    ) %>%
    pull(P)
  
  # BGBI transformation: Bidirectional Global-centered Balance Index
  # p = local residential proportion, P = global residential proportion
  bgbi_fun <- function(p, P) {
    ((2 * p - 1) - (2 * P - 1)) / (1 - (2 * p - 1) * (2 * P - 1))
  }
  
  # Local indicators per hexagon
  city_indices <- hex_grid %>%
    left_join(landuse_wide, by = "id_hex") %>%
    rowwise() %>%
    mutate(
      # Total addresses in the hexagon (all COD_ESPECIE columns)
      tot = sum(c_across(starts_with("COD_ESPECIE")), na.rm = TRUE),
      
      # Local residential share and complement
      p_res  = if (!is.na(tot) && tot > 0) COD_ESPECIE1 / tot else NA_real_,
      q_rest = if (!is.na(p_res)) 1 - p_res else NA_real_,
      
      # Number of land use classes (here we collapse to 2: residential vs. other)
      k = 2,
      
      # --- Conventional indicators ---
      
      # Entropy index (normalized by log(k))
      ei = if (!is.na(p_res) && !is.na(q_rest))
        -(p_res * log(p_res) + q_rest * log(q_rest)) / log(k)
      else NA_real_,
      
      # Herfindahl-Hirschman index (HHI)
      hhi = if (!is.na(p_res) && !is.na(q_rest))
        p_res^2 + q_rest^2 else NA_real_,
      
      # Scaled HHI in [0,1]
      min_hhi = 1 / k,
      hhi_sc  = if (!is.na(hhi)) (hhi - min_hhi) / (1 - min_hhi) else NA_real_,
      
      # --- Proposed directional indicators ---
      
      # Directional HHI: sign indicates residential dominance vs. non-residential
      hhi_adp = if (!is.na(p_res) && !is.na(hhi_sc))
        sign(p_res - q_rest) * hhi_sc else NA_real_,
      
      # BGBI: bidirectional index centered at the global balance
      bgbi = if (!is.na(p_res)) bgbi_fun(p_res, P) else NA_real_
    ) %>%
    ungroup() %>%
    mutate(
      code_muni = code_muni,
      name_muni = city_name
    )
  
  city_indices
}

## Example: computing land-use mix indicators for Salvador in H3 res. 7:
## ssa_lumi_r7 <- compute_lumi(lookup_muni(name_muni = "Salvador")$code_muni, h3_resolution = 7)

