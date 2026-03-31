# Internal helper: read the bundled metadata table
.read_metadata <- function() {
  read.csv(
    system.file("extdata", "metadata.csv", package = "HumanRetinaLRSData"),
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# Internal helper: return base dataset names derived from RDataPath
.available_datasets <- function() {
  meta <- .read_metadata()
  tools::file_path_sans_ext(basename(meta$RDataPath))
}

# Internal helper: download one file from OSF to BiocFileCache and return
# the cached file path
.cache_file <- function(file_name, bfc) {
  q <- BiocFileCache::bfcquery(bfc, file_name, exact = TRUE)
  n <- nrow(q)
  if (n > 1L) {
    message("Multiple cache entries for ", file_name, "; using last")
    return(q$rpath[n])
  }
  if (n == 1L) {
    message("Loading from cache: ", file_name)
    return(q$rpath[1L])
  }
  message("Downloading from OSF: ", file_name)
  proj_url <- "https://osf.io/z2yvs/"
  ret_project <- osfr::osf_retrieve_node(proj_url)
  osf_files  <- osfr::osf_ls_files(ret_project, "HumanRetinaLRSData", n_max = Inf)

  # Download directly into the BiocFileCache directory so the path is known
  cache_dir <- BiocFileCache::bfccache(bfc)
  osfr::osf_download(
    osf_files[osf_files$name == file_name, ],
    path = cache_dir,
    conflicts = "overwrite"
  )
  fpath <- file.path(cache_dir, file_name)
  BiocFileCache::bfcadd(bfc, file_name, fpath = fpath, action = "asis")
  fpath
}

#' List available datasets
#'
#' Returns the names that can be passed to \code{\link{load_object}}.
#' Reads from the bundled \file{inst/extdata/metadata.csv}; no internet
#' connection is required.
#'
#' @return A sorted character vector of dataset names.
#' @export
#' @examples
#' list_osf_files()
list_osf_files <- function() {
  sort(.available_datasets())
}

#' Load a dataset from OSF with BiocFileCache
#'
#' Validates \code{osf_file_name} against the bundled
#' \file{inst/extdata/metadata.csv} (no internet connection required for
#' validation). On first use the data files are downloaded from OSF and
#' cached via \pkg{BiocFileCache}; subsequent calls load from the local
#' cache.
#'
#' Each \code{SummarizedExperiment} dataset is stored on OSF as three CSV
#' files (\code{<name>_counts.csv}, \code{<name>_colData.csv},
#' \code{<name>_rowData.csv}); a CPM assay is computed from the raw counts
#' on load.  Matrix datasets are stored as a single \code{<name>.csv}.
#'
#' @param osf_file_name Character scalar. Dataset base name; see
#'   \code{\link{list_osf_files}}.
#' @param bfc A \code{\link[BiocFileCache]{BiocFileCache}} object.
#'   Defaults to a per-user package cache.
#' @return A \code{\link[SummarizedExperiment]{SummarizedExperiment}} or a
#'   \code{matrix}, depending on the dataset.
#' @importFrom BiocFileCache BiocFileCache bfcquery bfcadd bfccache
#' @importFrom osfr osf_retrieve_node osf_ls_files osf_download
#' @importFrom utils read.csv
#' @export
#' @examples
#' \dontrun{
#' se <- load_object("ROGeneLevelData")
#' se
#' }
load_object <- function(
    osf_file_name,
    bfc = BiocFileCache::BiocFileCache(
      tools::R_user_dir("HumanRetinaLRSData", which = "cache"),
      ask = FALSE
    )) {

  stopifnot(is.character(osf_file_name), length(osf_file_name) == 1L)

  # Validate against local metadata — no internet connection needed
  osf_file_name <- match.arg(osf_file_name, .available_datasets())

  meta      <- .read_metadata()
  row_idx   <- which(
    tools::file_path_sans_ext(basename(meta$RDataPath)) == osf_file_name
  )
  rdata_class <- meta$RDataClass[row_idx]

  if (rdata_class == "SummarizedExperiment") {
    counts_path   <- .cache_file(
      paste0(osf_file_name, "_counts.csv"), bfc
    )
    col_data_path <- .cache_file(
      paste0(osf_file_name, "_colData.csv"), bfc
    )
    row_data_path <- .cache_file(
      paste0(osf_file_name, "_rowData.csv"), bfc
    )

    counts   <- as.matrix(
      read.csv(counts_path,   row.names = 1L, check.names = FALSE)
    )
    col_data <- read.csv(col_data_path, row.names = 1L, check.names = FALSE)
    row_data <- read.csv(row_data_path, row.names = 1L, check.names = FALSE)

    # Compute counts-per-million from raw counts
    cpm <- sweep(counts, 2L, colSums(counts) / 1e6, FUN = "/")

    SummarizedExperiment::SummarizedExperiment(
      assays  = list(counts = counts, cpm = cpm),
      colData = col_data,
      rowData = row_data
    )
  } else {
    csv_path <- .cache_file(paste0(osf_file_name, ".csv"), bfc)
    as.matrix(read.csv(csv_path, row.names = 1L, check.names = FALSE))
  }
}

#' Clear the local OSF cache
#'
#' Removes all files cached by \code{\link{load_object}} from the
#' BiocFileCache store.
#'
#' @param bfc A \code{\link[BiocFileCache]{BiocFileCache}} object.
#'   Defaults to the per-user package cache.
#' @return \code{NULL} invisibly. Called for its side effect.
#' @export
#' @examples
#' \dontrun{
#' clear_osf_cache()
#' }
clear_osf_cache <- function(
    bfc = BiocFileCache::BiocFileCache(
      tools::R_user_dir("HumanRetinaLRSData", which = "cache"),
      ask = FALSE
    )) {
  BiocFileCache::removebfc(bfc, ask = FALSE)
  invisible(NULL)
}
