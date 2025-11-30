# example.R
# Example workflow using lumi_cnefe:
#  - compute mixed land-use indicators on H3 grids
#  - retrieve main arterial streets (optional)
#  - produce maps and scatterplots for selected cities


# (0) Load required packages ----------------------------------------------

required_pkgs <- c(
  "tidyverse",  # dplyr, purrr, ggplot2, etc.
  "sf",
  "geobr",
  "ggspatial",  # annotation_map_tile, annotation_scale, annotation_north_arrow
  "patchwork"   # combine multiple ggplots with | and /
  # "osmdata",  # uncomment if you use get_streets()
  # "osmextract" # uncomment if you use get_streets_oe()
)

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# If you are not using this inside a package, you may need:
# source("R/build_cnefe_index_2022.R")
# source("R/read_cnefe.R")
# source("R/compute_lumi.R")
# and to create or load `cnefe_index_2022` before running the examples.



# (1) Cities and helper to compute indicators -----------------------------

# List of large Brazilian municipalities used in the paper
cities <- c(
  "Salvador",
  "São Paulo",
  "Rio de Janeiro",
  "Fortaleza",
  "Belo Horizonte",
  "Brasília"
)

lookup_tbl <- geobr::lookup_muni(name_muni = "all")

city_codes <- tibble::tibble(name_muni = cities) %>%
  dplyr::left_join(lookup_tbl, by = "name_muni") %>%
  dplyr::pull(code_muni)

city_codes

# Helper function: compute lumi for all cities at a given H3 resolution
compute_lumi_for_cities <- function(code_muni_vec, h3_resolution = 9) {
  purrr::map(
    code_muni_vec,
    ~ compute_lumi(code_muni = .x, h3_resolution = h3_resolution)
  )
}


# (2) Compute and save results (optional, heavy step) ------------------


# Uncomment and run once if you want to build the res_r7 / res_r8 / res_r9
# objects from scratch. This may take some time.

# # H3 resolution 7
# res_r7 <- compute_lumi_for_cities(city_codes, h3_resolution = 7)
# saveRDS(res_r7, "res_r7.rds")
#
# # H3 resolution 8
# res_r8 <- compute_lumi_for_cities(city_codes, h3_resolution = 8)
# saveRDS(res_r8, "res_r8.rds")
#
# # H3 resolution 9
# res_r9 <- compute_lumi_for_cities(city_codes, h3_resolution = 9)
# saveRDS(res_r9, "res_r9.rds")


# (3) Load precomputed results --------------------------------------------

res_r7 <- readRDS("res_r7.rds")
res_r8 <- readRDS("res_r8.rds")
res_r9 <- readRDS("res_r9.rds")

# Example quick check
summary(res_r9[[1]]$hhi)



# (4) Optional: retrieve arterial streets from OSM and save to RDS --------
# You only need this if you want to re-download streets instead of reusing
# a precomputed 'streets.rds'.

# ---- Option A: Overpass API (osmdata) -------------------------------------
# Requires: library(osmdata)
#
# get_streets <- function(local) {
#   # Ensure WGS84 (EPSG:4326) to talk to OSM
#   pol_wgs <- sf::st_transform(local, 4326)
#
#   # Bounding box of the polygon
#   bb <- sf::st_bbox(pol_wgs)
#
#   # Which arterial road classes to include?
#   arterial_types <- c("motorway", "primary", "secondary", "tertiary", "trunk")
#
#   # Build Overpass query
#   q <- osmdata::opq(bbox = bb) %>%
#     osmdata::add_osm_feature(key = "highway", value = arterial_types)
#
#   # Download as sf
#   osm <- osmdata::osmdata_sf(q)
#
#   # Road lines (transformed back to the CRS of the input polygon)
#   arterial_lines <- osm$osm_lines %>%
#     sf::st_transform(sf::st_crs(local))
#
#   # Exact spatial intersection with the polygon
#   arterial_in_poly <- sf::st_intersection(arterial_lines, local)
#
#   # Optionally keep just a few attributes
#   arterial_in_poly %>%
#     dplyr::select(highway, name, dplyr::everything())
# }

# ---- Option B: osmextract (oe_get) ----------------------------------------
# Requires: library(osmextract)
#
# get_streets_oe <- function(
    #   local,
#   arterial_types = c("motorway", "trunk", "primary", "secondary", "tertiary")
# ) {
#   # Ensure WGS84 for spatial filtering
#   pol_wgs <- sf::st_transform(local, 4326)
#
#   # Build SQL clause with the desired highway classes
#   type_str <- paste(shQuote(arterial_types), collapse = ", ")
#   query <- glue::glue("
#     SELECT *
#     FROM lines
#     WHERE highway IN ({type_str})
#   ")
#
#   osmextract::oe_get(
#     place         = pol_wgs,
#     layer         = "lines",
#     boundary      = pol_wgs,
#     boundary_type = "spat",
#     query         = query,
#     quiet         = FALSE
#   ) %>%
#     sf::st_transform(sf::st_crs(local)) %>%
#     dplyr::select(highway, name, dplyr::everything())
# }

# Example (heavy): compute streets for each city at resolution 9
# and save as a list. Uncomment if you want to rebuild.
#
# city_polygons_r9 <- purrr::map(res_r9, ~ .x %>% sf::st_union())
#
# ssa_streets <- get_streets(city_polygons_r9[[1]])
# spo_streets <- get_streets(city_polygons_r9[[2]])
# rdj_streets <- get_streets(city_polygons_r9[[3]])
# ftl_streets <- get_streets(city_polygons_r9[[4]])
# bhz_streets <- get_streets(city_polygons_r9[[5]])
# bsb_streets <- get_streets(city_polygons_r9[[6]])
#
# streets <- list(ssa_streets, spo_streets, rdj_streets,
#                 ftl_streets, bhz_streets, bsb_streets)
# saveRDS(streets, "streets.rds")


# (5) Load precomputed streets --------------------------------------------

streets <- readRDS("streets.rds")

# We assume the order in 'streets' matches the order of 'res_r9':
# 1 = Salvador, 2 = São Paulo, 3 = Rio de Janeiro,
# 4 = Fortaleza, 5 = Belo Horizonte, 6 = Brasília.

city_ids <- c("ssa", "spo", "rdj", "ftl", "bhz", "bsb")



# (6) Map functions for each indicator ------------------------------------

plot_ei <- function(data, street) {
  data %>%
    dplyr::filter(!is.na(ei)) %>%
    ggplot2::ggplot() +
    ggspatial::annotation_map_tile(type = "cartolight", zoom = 12) +
    ggspatial::annotation_scale(location = "br") +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      height = grid::unit(0.5, "cm"),
      width  = grid::unit(0.25, "cm"),
      pad_x  = grid::unit(0.1, "cm"),
      pad_y  = grid::unit(0.1, "cm")
    ) +
    ggplot2::geom_sf(ggplot2::aes(fill = ei), color = NA) +
    ggplot2::geom_sf(
      data = street %>%
        dplyr::filter(highway %in% c("motorway", "primary", "trunk")),
      color     = "gray40",
      linewidth = 0.3,
      alpha     = 0.6
    ) +
    ggplot2::scale_fill_viridis_c(limits = c(0, 1)) +
    ggplot2::labs(
      fill     = "",
      title    = "EI",
      subtitle = "(0) Homogeneous | (+1) Balanced"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(hjust = 0.5, size = 14),
      plot.subtitle   = ggplot2::element_text(hjust = 0.5, size = 12),
      axis.text.x     = ggplot2::element_text(size = 6),
      axis.text.y     = ggplot2::element_text(size = 6),
      legend.title    = ggplot2::element_text(size = 9),
      legend.text     = ggplot2::element_text(size = 7),
      legend.key.height = grid::unit(0.8, "lines"),
      legend.key.width  = grid::unit(0.8, "lines")
    )
}

plot_hhi <- function(data, street) {
  data %>%
    dplyr::filter(!is.na(hhi)) %>%
    ggplot2::ggplot() +
    ggspatial::annotation_map_tile(type = "cartolight", zoom = 12) +
    ggspatial::annotation_scale(location = "br") +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      height = grid::unit(0.5, "cm"),
      width  = grid::unit(0.25, "cm"),
      pad_x  = grid::unit(0.1, "cm"),
      pad_y  = grid::unit(0.1, "cm")
    ) +
    ggplot2::geom_sf(ggplot2::aes(fill = hhi), color = NA) +
    ggplot2::geom_sf(
      data = street %>%
        dplyr::filter(highway %in% c("motorway", "primary", "trunk")),
      color     = "gray40",
      linewidth = 0.3,
      alpha     = 0.6
    ) +
    ggplot2::scale_fill_viridis_c(limits = c(0.5, 1)) +
    ggplot2::labs(
      fill     = "",
      title    = "HHI",
      subtitle = "(0.5) Balanced | (+1) Homogeneous"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(hjust = 0.5, size = 14),
      plot.subtitle   = ggplot2::element_text(hjust = 0.5, size = 12),
      axis.text.x     = ggplot2::element_text(size = 6),
      axis.text.y     = ggplot2::element_text(size = 6),
      legend.title    = ggplot2::element_text(size = 9),
      legend.text     = ggplot2::element_text(size = 7),
      legend.key.height = grid::unit(0.8, "lines"),
      legend.key.width  = grid::unit(0.8, "lines")
    )
}

plot_ahhi <- function(data, street) {
  data %>%
    dplyr::filter(!is.na(hhi_adp)) %>%
    ggplot2::ggplot() +
    ggspatial::annotation_map_tile(type = "cartolight", zoom = 12) +
    ggspatial::annotation_scale(location = "br") +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      height = grid::unit(0.5, "cm"),
      width  = grid::unit(0.25, "cm"),
      pad_x  = grid::unit(0.1, "cm"),
      pad_y  = grid::unit(0.1, "cm")
    ) +
    ggplot2::geom_sf(ggplot2::aes(fill = hhi_adp), color = NA) +
    ggplot2::geom_sf(
      data = street %>%
        dplyr::filter(highway %in% c("motorway", "primary", "trunk")),
      color     = "gray40",
      linewidth = 0.3,
      alpha     = 0.6
    ) +
    ggplot2::scale_fill_gradient2(
      low      = "red3",
      mid      = "white",
      high     = "blue3",
      midpoint = 0,
      limits   = c(-1, 1)
    ) +
    ggplot2::labs(
      fill     = "",
      title    = "aHHI",
      subtitle = "(-1) Non-residential | (0) Balanced | (+1) Residential"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(hjust = 0.5, size = 14),
      plot.subtitle   = ggplot2::element_text(hjust = 0.5, size = 12),
      axis.text.x     = ggplot2::element_text(size = 6),
      axis.text.y     = ggplot2::element_text(size = 6),
      legend.title    = ggplot2::element_text(size = 9),
      legend.text     = ggplot2::element_text(size = 7),
      legend.key.height = grid::unit(0.8, "lines"),
      legend.key.width  = grid::unit(0.8, "lines")
    )
}

plot_bgbi <- function(data, street) {
  data %>%
    dplyr::filter(!is.na(bgbi)) %>%
    ggplot2::ggplot() +
    ggspatial::annotation_map_tile(type = "cartolight", zoom = 12) +
    ggspatial::annotation_scale(location = "br") +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      height = grid::unit(0.5, "cm"),
      width  = grid::unit(0.25, "cm"),
      pad_x  = grid::unit(0.1, "cm"),
      pad_y  = grid::unit(0.1, "cm")
    ) +
    ggplot2::geom_sf(ggplot2::aes(fill = bgbi), color = NA) +
    ggplot2::geom_sf(
      data = street %>%
        dplyr::filter(highway %in% c("motorway", "primary", "trunk")),
      color     = "gray40",
      linewidth = 0.3,
      alpha     = 0.6
    ) +
    ggplot2::scale_fill_gradient2(
      low      = "red3",
      mid      = "white",
      high     = "blue3",
      midpoint = 0,
      limits   = c(-1, 1)
    ) +
    ggplot2::labs(
      fill     = "",
      title    = "BGBI",
      subtitle = "(-1) Non-residential | (0) Balanced | (+1) Residential"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(hjust = 0.5, size = 14),
      plot.subtitle   = ggplot2::element_text(hjust = 0.5, size = 12),
      axis.text.x     = ggplot2::element_text(size = 6),
      axis.text.y     = ggplot2::element_text(size = 6),
      legend.title    = ggplot2::element_text(size = 9),
      legend.text     = ggplot2::element_text(size = 7),
      legend.key.height = grid::unit(0.8, "lines"),
      legend.key.width  = grid::unit(0.8, "lines")
    )
}


# (7) Panel maps for H3 resolution 9 (EI, HHI, aHHI, BGBI) ----------------

# Helper function to build and save a 2x2 panel for one city at resolution 9
save_city_panel_r9 <- function(i, file_stub) {
  p <- (plot_ei(res_r9[[i]], streets[[i]]) |
          plot_hhi(res_r9[[i]], streets[[i]])) /
    (plot_ahhi(res_r9[[i]], streets[[i]]) |
       plot_bgbi(res_r9[[i]], streets[[i]]))
  
  ggplot2::ggsave(
    filename = paste0(file_stub, "_r9.jpg"),
    plot     = p,
    dpi      = 900,
    height   = 9,
    width    = 12
  )
}

# Save a 2x2 panel for each city
for (i in seq_along(city_ids)) {
  save_city_panel_r9(i, city_ids[i])
}



# (8) Scatterplots: relationship between indicators -----------------------

df_all <- res_r9 %>%
  purrr::map_dfr(
    ~ .x %>%
      sf::st_drop_geometry() %>%
      dplyr::select(name_muni, ei, hhi, bgbi, hhi_adp)
  )

# aHHI vs BGBI
p_bgbi_ahhi <- df_all %>%
  ggplot2::ggplot(ggplot2::aes(x = hhi_adp, y = bgbi)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::facet_wrap(. ~ name_muni) +
  ggplot2::labs(x = "aHHI", y = "BGBI") +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::geom_vline(xintercept = 0) +
  ggplot2::theme_bw(base_size = 14)

ggplot2::ggsave("scat_bgbi_ahhi.jpg", plot = p_bgbi_ahhi, dpi = 300,
                width = 8, height = 6)

# EI vs HHI
p_hhi_ei <- df_all %>%
  ggplot2::ggplot(ggplot2::aes(x = ei, y = hhi)) +
  ggplot2::geom_point(alpha = 0.6) +
  ggplot2::facet_wrap(. ~ name_muni) +
  ggplot2::labs(x = "EI", y = "HHI") +
  ggplot2::theme_bw(base_size = 14)

ggplot2::ggsave("scat_hhi_ei.jpg", plot = p_hhi_ei, dpi = 300,
                width = 8, height = 6)



# (9) Comparing resolutions (example for Salvador and São Paulo, B --------

# Salvador: BGBI at resolutions 8 and 9
ssa_bgbi_r8 <- res_r8[[1]] %>%
  dplyr::filter(!is.na(bgbi)) %>%
  ggplot2::ggplot() +
  ggspatial::annotation_map_tile(type = "cartolight", zoom = 12) +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    height = grid::unit(0.5, "cm"),
    width  = grid::unit(0.25, "cm"),
    pad_x  = grid::unit(0.1, "cm"),
    pad_y  = grid::unit(0.1, "cm")
  ) +
  ggplot2::geom_sf(ggplot2::aes(fill = bgbi), color = NA) +
  ggplot2::geom_sf(
    data = streets[[1]] %>%
      dplyr::filter(highway %in% c("motorway", "primary", "trunk")),
    color     = "gray40",
    linewidth = 0.25,
    alpha     = 0.8
  ) +
  ggplot2::scale_fill_gradient2(
    low      = "red3",
    mid      = "white",
    high     = "blue3",
    midpoint = 0,
    limits   = c(-1, 1)
  ) +
  ggplot2::labs(fill = "") +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title    = ggplot2::element_text(hjust = 0.5, size = 10),
    plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 8),
    axis.text.x   = ggplot2::element_text(size = 6),
    axis.text.y   = ggplot2::element_text(size = 6),
    legend.position = "none"
  )

ssa_bgbi_r9 <- res_r9[[1]] %>%
  dplyr::filter(!is.na(bgbi)) %>%
  ggplot2::ggplot() +
  ggspatial::annotation_map_tile(type = "cartolight", zoom = 12) +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    height = grid::unit(0.5, "cm"),
    width  = grid::unit(0.25, "cm"),
    pad_x  = grid::unit(0.1, "cm"),
    pad_y  = grid::unit(0.1, "cm")
  ) +
  ggplot2::geom_sf(ggplot2::aes(fill = bgbi), color = NA) +
  ggplot2::geom_sf(
    data = streets[[1]] %>%
      dplyr::filter(highway %in% c("motorway", "primary", "trunk")),
    color     = "gray40",
    linewidth = 0.25,
    alpha     = 0.8
  ) +
  ggplot2::scale_fill_gradient2(
    low      = "red3",
    mid      = "white",
    high     = "blue3",
    midpoint = 0,
    limits   = c(-1, 1)
  ) +
  ggplot2::labs(fill = "") +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title    = ggplot2::element_text(hjust = 0.5, size = 10),
    plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 8),
    axis.text.x   = ggplot2::element_text(size = 6),
    axis.text.y   = ggplot2::element_text(size = 6),
    legend.title  = ggplot2::element_text(size = 9),
    legend.text   = ggplot2::element_text(size = 7),
    legend.key.height = grid::unit(0.8, "lines"),
    legend.key.width  = grid::unit(0.8, "lines")
  )

spo_bgbi_r8 <- res_r8[[2]] %>%
  dplyr::filter(!is.na(bgbi)) %>%
  ggplot2::ggplot() +
  ggspatial::annotation_map_tile(type = "cartolight", zoom = 12) +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    height = grid::unit(0.5, "cm"),
    width  = grid::unit(0.25, "cm"),
    pad_x  = grid::unit(0.1, "cm"),
    pad_y  = grid::unit(0.1, "cm")
  ) +
  ggplot2::geom_sf(ggplot2::aes(fill = bgbi), color = NA) +
  ggplot2::geom_sf(
    data = streets[[2]] %>%
      dplyr::filter(highway %in% c("motorway", "primary", "trunk")),
    color     = "gray40",
    linewidth = 0.25,
    alpha     = 0.8
  ) +
  ggplot2::scale_fill_gradient2(
    low      = "red3",
    mid      = "white",
    high     = "blue3",
    midpoint = 0,
    limits   = c(-1, 1)
  ) +
  ggplot2::labs(fill = "") +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title    = ggplot2::element_text(hjust = 0.5, size = 10),
    plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 8),
    axis.text.x   = ggplot2::element_text(size = 6),
    axis.text.y   = ggplot2::element_text(size = 6),
    legend.position = "none"
  )

spo_bgbi_r9 <- res_r9[[2]] %>%
  dplyr::filter(!is.na(bgbi)) %>%
  ggplot2::ggplot() +
  ggspatial::annotation_map_tile(type = "cartolight", zoom = 12) +
  ggspatial::annotation_scale(location = "br") +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    height = grid::unit(0.5, "cm"),
    width  = grid::unit(0.25, "cm"),
    pad_x  = grid::unit(0.1, "cm"),
    pad_y  = grid::unit(0.1, "cm")
  ) +
  ggplot2::geom_sf(ggplot2::aes(fill = bgbi), color = NA) +
  ggplot2::geom_sf(
    data = streets[[2]] %>%
      dplyr::filter(highway %in% c("motorway", "primary", "trunk")),
    color     = "gray40",
    linewidth = 0.25,
    alpha     = 0.8
  ) +
  ggplot2::scale_fill_gradient2(
    low      = "red3",
    mid      = "white",
    high     = "blue3",
    midpoint = 0,
    limits   = c(-1, 1)
  ) +
  ggplot2::labs(fill = "") +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.title    = ggplot2::element_text(hjust = 0.5, size = 10),
    plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 8),
    axis.text.x   = ggplot2::element_text(size = 6),
    axis.text.y   = ggplot2::element_text(size = 6),
    legend.title  = ggplot2::element_text(size = 9),
    legend.text   = ggplot2::element_text(size = 7),
    legend.key.height = grid::unit(0.8, "lines"),
    legend.key.width  = grid::unit(0.8, "lines")
  )

p_ssa_res <- ssa_bgbi_r8 | ssa_bgbi_r9
ggplot2::ggsave("ssa_diff_res.jpg", plot = p_ssa_res, dpi = 900,
                width = 8, height = 4)

p_spo_res <- spo_bgbi_r8 | spo_bgbi_r9
ggplot2::ggsave("spo_diff_res.jpg", plot = p_spo_res, dpi = 900,
                width = 8, height = 4)
