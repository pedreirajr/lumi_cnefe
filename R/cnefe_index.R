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


# Build index table with CNEFE URLs for each municipality -----------------
# 
# This function:
#   1) Downloads the 2022 municipality geometries from geobr
#   2) Builds the URL of each state folder at the IBGE FTP
#   3) Scrapes all .zip files for each state
#   4) Extracts the 7-digit municipality code from the .zip name
#   5) Returns an sf object with municipality geometry + CNEFE zip URL
build_cnefe_index_2022 <- function(
    base_url = "https://ftp.ibge.gov.br/Cadastro_Nacional_de_Enderecos_para_Fins_Estatisticos/Censo_Demografico_2022/Arquivos_CNEFE/CSV/Municipio/",
    verbose = TRUE
) {
  # 1) Download and load all municipalities with geometry (simplified = FALSE)
  muni_sf <- geobr::read_municipality(
    year         = 2022,
    simplified   = FALSE,
    showProgress = verbose
  )
  
  ## We will use this table as metadata (no geometry) where needed
  muni_meta <- muni_sf %>%
    st_drop_geometry() %>%
    distinct(code_state, abbrev_state, code_muni, name_muni)
  
  # 2) Build state folder URLs on the IBGE FTP
  states <- muni_meta %>%
    distinct(code_state, abbrev_state) %>%
    mutate(
      state_folder = sprintf("%02d_%s", code_state, abbrev_state),
      state_url    = glue("{base_url}{state_folder}/")
    )
  
  ## Helper function to scrape .zip files from a given state folder
  scrape_state_zips <- function(state_url) {
    if (verbose) message("Reading index from: ", state_url)
    
    safe_read_html <- possibly(read_html, otherwise = NULL)
    html <- safe_read_html(state_url)
    if (is.null(html)) {
      warning("Failed to access ", state_url)
      return(tibble(zip_name = character()))
    }
    
    hrefs <- html %>%
      html_elements("a") %>%
      html_attr("href")
    
    tibble(zip_name = hrefs[grepl("\\.zip$", hrefs, ignore.case = TRUE)])
  }
  
  # 3) Scrape all state folders and extract code_muni + zip_url
  state_files <- states %>%
    mutate(zips = map(state_url, scrape_state_zips)) %>%
    unnest(zips) %>%
    mutate(
      code_muni = parse_integer(str_extract(zip_name, "^[0-9]{7}")),
      zip_url   = paste0(state_url, zip_name)
    ) %>%
    filter(!is.na(code_muni)) %>%
    select(code_state, abbrev_state, code_muni, zip_url)
  
  # 4) Join with muni_sf, preserving geometry and ordering by state / muni
  index_sf <- muni_sf %>%
    left_join(
      state_files,
      by = c("code_state", "abbrev_state", "code_muni")
    ) %>%
    arrange(code_state, code_muni)
  
  index_sf
}

# Example usage to build and save the index:
# cnefe_index_2022 <- build_cnefe_index_2022()
# saveRDS(cnefe_index_2022, "cnefe_index_2022.rds")
#
# When working in analysis scripts you can load the pre-built index:
# cnefe_index_2022 <- readRDS("cnefe_index_2022.rds")
