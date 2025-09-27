# datagouv_loader.R — charge des ressources data.gouv.fr par resource_id (sans Python)

# Dépendances:
# install.packages(c("httr","jsonlite","data.table","readxl"))
# Optionnels selon formats: install.packages(c("sf","arrow"))

DATAGOUV_URL <- "https://www.data.gouv.fr/"

.dgr_api_v2_resource <- function(resource_id) {
  url <- sprintf("%sapi/2/datasets/resources/%s", DATAGOUV_URL, resource_id)
  resp <- httr::GET(url, httr::timeout(60), httr::add_headers(`User-Agent`="R (httr)"))
  if (httr::http_error(resp)) stop(httr::http_status(resp)$message, call. = FALSE)
  jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
}

.dgr_download_url <- function(resource_id) sprintf("%sfr/datasets/r/%s", DATAGOUV_URL, resource_id)

# lecteur tabulaire rapide (CSV/TSV/TXT, gère .gz)
.dgr_read_tabular <- function(path, sep=";", ...) {
  as.data.frame(data.table::fread(path, sep = sep, showProgress = FALSE, ...))
}

# ---- Fonction principale (avec cache) ----
#' Charger une ressource data.gouv.fr par resource_id
#' @param resource_id UUID de la ressource
#' @param sep séparateur CSV (par défaut ";")
#' @param cache_dir dossier de cache local
#' @param force_download TRUE pour ignorer le cache
#' @param .file_and_format interne: list(path, format) quand lecture d’un fichier extrait d’un ZIP
#' @param ... options passées aux lecteurs (readxl, fread, etc.)
load_table_from_resource_id <- function(resource_id,
                                        sep = ";",
                                        cache_dir = ".datagouv_cache",
                                        force_download = FALSE,
                                        .file_and_format = NULL,
                                        ...) {
  if (!is.null(resource_id)) resource_id <- trimws(resource_id)
  
  # Déterminer le format
  if (!is.null(.file_and_format)) {
    fmt <- tolower(.file_and_format[[2]])
    target <- .file_and_format[[1]]
  } else {
    meta <- .dgr_api_v2_resource(resource_id)
    if (is.null(meta$resource$format)) stop("Format introuvable dans les métadonnées.", call. = FALSE)
    fmt <- tolower(meta$resource$format)
    target <- .dgr_download_url(resource_id)
  }
  
  # Préparer cache
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  ext_for_cache <- switch(fmt,
                          "csv.gz"="csv.gz", "xls"="xls", "xlsx"="xlsx", "parquet"="parquet",
                          "geojson"="geojson", "zip"="zip", "csv"="csv", "tsv"="tsv", "txt"="txt", fmt)
  cache_file <- if (!is.null(resource_id)) file.path(cache_dir, paste0(resource_id, ".", ext_for_cache)) else NULL
  
  # Fonction utilitaire: télécharger si besoin
  .get_local <- function(url, fileext) {
    tmp <- if (!is.null(cache_file)) cache_file else tempfile(fileext = paste0(".", fileext))
    if (!file.exists(tmp) || isTRUE(force_download)) {
      utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    }
    tmp
  }
  
  # --- formats ---
  if (fmt == "geojson") {
    if (!requireNamespace("sf", quietly = TRUE))
      stop("Installez le package 'sf' pour lire du GeoJSON.", call. = FALSE)
    if (!is.null(resource_id)) target <- .get_local(target, "geojson")
    return(suppressMessages(sf::st_read(target, quiet = TRUE)))
  }
  
  if (fmt == "zip") {
    zpath <- if (!is.null(resource_id)) .get_local(target, "zip") else target
    td <- tempfile("unz_"); dir.create(td)
    utils::unzip(zpath, exdir = td)
    files <- list.files(td, recursive = TRUE, full.names = TRUE)
    if (!length(files)) stop("Le ZIP est vide.", call. = FALSE)
    largest <- files[which.max(file.info(files)$size)]
    fmt2 <- tolower(tools::file_ext(largest))
    message(sprintf("Info : ZIP multi-fichiers, ouverture du plus gros: %s", basename(largest)))
    return(load_table_from_resource_id(NULL, sep = sep, cache_dir = cache_dir,
                                       .file_and_format = list(largest, fmt2), ...))
  }
  
  if (fmt == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE))
      stop("Installez le package 'arrow' pour lire du Parquet.", call. = FALSE)
    p <- if (!is.null(resource_id)) .get_local(target, "parquet") else target
    return(as.data.frame(arrow::read_parquet(p, ...)))
  }
  
  if (fmt %in% c("xls","xlsx")) {
    x <- if (!is.null(resource_id)) .get_local(target, fmt) else target
    return(suppressMessages(readxl::read_excel(x, ...)))
  }
  
  # CSV & assimilés (inclut csv.gz)
  if (fmt %in% c("csv.gz","gz")) {
    cgz <- if (!is.null(resource_id)) .get_local(target, "csv.gz") else target
    return(.dgr_read_tabular(cgz, sep = sep, ...))
  }
  
  # CSV/TSV/TXT: essaye lecture directe; sinon télécharge puis relis
  out <- tryCatch(.dgr_read_tabular(target, sep = sep, ...),
                  error = function(e) {
                    loc <- if (!is.null(resource_id)) .get_local(target, fmt) else {
                      tmp <- tempfile(fileext = paste0(".", fmt)); utils::download.file(target, tmp, mode="wb", quiet=TRUE); tmp
                    }
                    .dgr_read_tabular(loc, sep = sep, ...)
                  })
  out
}
