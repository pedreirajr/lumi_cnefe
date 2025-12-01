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

# Read CNEFE for a given municipality ------------------------------------
# 
# This function:
#   - Looks up the CNEFE zip URL for a municipality from an index
#   - Downloads the zip (optionally to a cache directory)
#   - Finds the CSV inside the zip
#   - Reads the CSV using Arrow and returns an Arrow Table (not a data.frame)
# 
# Arguments:
#   code_muni : 7-digit municipality code
#   index     : index table created by build_cnefe_index_2022()
#   delim     : CSV delimiter (default ";")
#   verbose   : if TRUE, prints progress messages
#   cache     : if TRUE, stores the downloaded zip in cache_dir
#   cache_dir : directory used when cache = TRUE
read_cnefe <- function(code_muni,
                       index     = cnefe_index_2022,
                       delim     = ";",
                       verbose   = TRUE,
                       cache     = FALSE,
                       cache_dir = "data_cnefe_cache") {
  
  code_muni <- as.integer(code_muni)
  
  # Look up municipality metadata in the index
  info <- index %>%
    filter(.data$code_muni == !!code_muni)
  
  if (nrow(info) == 0) {
    stop("Municipality code not found in `index`: ", code_muni)
  }
  
  url <- info$zip_url[1]
  if (is.na(url) || !nzchar(url)) {
    stop("URL (`zip_url`) is missing for municipality: ", code_muni)
  }
  ext <- tools::file_ext(url)
  
  # Decide where to save the zip file (temporary or cache)
  if (cache) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    zip_path <- file.path(cache_dir, basename(url))
    cleanup_zip <- FALSE
  } else {
    zip_path <- tempfile(fileext = paste0(".", ext))
    cleanup_zip <- TRUE
  }
  
  # Download zip if not available locally
  if (!file.exists(zip_path)) {
    if (verbose) message("Downloading file from: ", url)
    download.file(url, destfile = zip_path, mode = "wb", quiet = !verbose)
  } else if (verbose) {
    message("Using cached file: ", zip_path)
  }
  
  # Temporary directory to extract the CSV
  tmp_dir <- tempfile("cnefe_unzip_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Ensure temporary files are cleaned up
  on.exit({
    if (cleanup_zip && file.exists(zip_path)) unlink(zip_path)
    if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)
  
  # List contents of the compressed file and find the first CSV inside
  if (verbose) message("Listing compressed file contents...")
  arch_info <- archive(zip_path)
  csv_inside <- arch_info$path[grepl("\\.csv$", arch_info$path, ignore.case = TRUE)][1]
  if (is.na(csv_inside)) {
    stop("No .csv file found for municipality: ", code_muni)
  }
  
  # Extract only the CSV we need
  if (verbose) message("Extracting ", csv_inside, " ...")
  archive_extract(zip_path, file = csv_inside, dir = tmp_dir)
  
  csv_path <- file.path(tmp_dir, csv_inside)
  if (!file.exists(csv_path)) {
    stop("Failed to extract CSV to ", csv_path)
  }
  
  # Read CSV into an Arrow Table (lazy / efficient in memory)
  if (verbose) message("Reading CSV with arrow...")
  tab <- suppressWarnings(
    read_delim_arrow(csv_path, delim = delim, col_names = TRUE, as_data_frame = FALSE)
  )
  
  tab
}

## Reading cnefe_index
### ((after computing it with build_cnefe_index_2022() and saving it as cnefe_index_2022.rds)
cnefe_index_2022 <- readRDS("cnefe_index_2022.rds")

## Examples:
## cnefe_ssa <- read_cnefe(lookup_muni(name_muni = "Salvador")$code_muni, cache = TRUE)
## cnefe_spo <- read_cnefe(lookup_muni(name_muni = "São Paulo")$code_muni, cache = TRUE)
## cnefe_rdj <- read_cnefe(lookup_muni(name_muni = "Rio de Janeiro")$code_muni, cache = TRUE)

