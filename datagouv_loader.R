# datagouv_loader.R
# Ajout d'un cache local pour éviter de retélécharger les ressources
# Dépendances : httr, readr, data.table, jsonlite, readxl
# Optionnels : sf (GeoJSON/Shapefile), arrow (Parquet)

# -------- Helpers internes --------
.dgr_resource_url <- function(resource_id) {
  sprintf("https://www.data.gouv.fr/api/1/datasets/r/%s", resource_id)
}

.dgr_guess_extension <- function(resp, url_finale) {
  cd <- httr::headers(resp)[["content-disposition"]]
  if (!is.null(cd)) {
    m <- regmatches(cd, regexpr('filename="([^"]+)"', cd))
    if (length(m)) {
      fname <- sub('^filename="', "", sub('"$', "", m))
      ext <- tools::file_ext(fname)
      if (nzchar(ext)) return(tolower(ext))
    }
  }
  ext <- tools::file_ext(url_finale)
  if (nzchar(ext)) return(tolower(ext))
  ct <- httr::headers(resp)[["content-type"]]
  if (!is.null(ct)) {
    if (grepl("parquet", ct, TRUE)) return("parquet")
    if (grepl("zip", ct, TRUE)) return("zip")
    if (grepl("excel|spreadsheetml", ct, TRUE)) return("xlsx")
    if (grepl("geo\\+json|geojson", ct, TRUE)) return("geojson")
    if (grepl("json", ct, TRUE)) return("json")
    if (grepl("csv", ct, TRUE)) return("csv")
    if (grepl("gzip", ct, TRUE)) return("gz")
  }
  ""
}

.dgr_read_delim_auto <- function(path_or_url) {
  as.data.frame(data.table::fread(path_or_url, showProgress = FALSE))
}

.dgr_read_zip <- function(zip_path) {
  td <- tempfile("unz_")
  dir.create(td)
  utils::unzip(zip_path, exdir = td)
  files <- list.files(td, recursive = TRUE, full.names = TRUE)
  if (!length(files)) stop("Le ZIP est vide.", call. = FALSE)
  if (length(files) == 1) return(.dgr_read_any(files[1]))
  out <- list()
  for (f in files) {
    ext <- tolower(tools::file_ext(f))
    if (!ext %in% c("csv","tsv","txt","xlsx","xls","json","ndjson","geojson","parquet","shp"))
      next
    nm <- basename(f)
    out[[nm]] <- tryCatch(.dgr_read_any(f, ext), error = function(e) e)
  }
  out
}

.dgr_read_any <- function(path_or_url, ext = tolower(tools::file_ext(path_or_url))) {
  if (ext == "gz")     return(.dgr_read_delim_auto(path_or_url))
  if (ext == "csv")    return(.dgr_read_delim_auto(path_or_url))
  if (ext == "tsv")    return(.dgr_read_delim_auto(path_or_url))
  if (ext == "txt")    return(.dgr_read_delim_auto(path_or_url))
  if (ext %in% c("xlsx","xls")) return(suppressMessages(readxl::read_excel(path_or_url)))
  if (ext == "json")   return(jsonlite::fromJSON(path_or_url, flatten = TRUE))
  if (ext == "ndjson") return(jsonlite::stream_in(file(path_or_url), verbose = FALSE))
  if (ext == "geojson") {
    if (!requireNamespace("sf", quietly = TRUE))
      stop("Format geojson: installez le package 'sf'.", call. = FALSE)
    return(suppressMessages(sf::st_read(path_or_url, quiet = TRUE)))
  }
  if (ext == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE))
      stop("Format parquet: installez le package 'arrow'.", call. = FALSE)
    return(as.data.frame(arrow::read_parquet(path_or_url)))
  }
  if (ext == "shp") {
    if (!requireNamespace("sf", quietly = TRUE))
      stop("Format shapefile: installez le package 'sf'.", call. = FALSE)
    return(suppressMessages(sf::st_read(path_or_url, quiet = TRUE)))
  }
  if (ext == "zip")    return(.dgr_read_zip(path_or_url))
  # fallback: tenter fread
  tryCatch(.dgr_read_delim_auto(path_or_url),
           error = function(e) stop("Extension non supportée ou lecture échouée: ", ext, call. = FALSE))
}

# -------- Fonction publique --------
#' Charger une ressource data.gouv.fr par resource_id avec cache local
#' @param resource_id UUID de la ressource sur data.gouv.fr
#' @param cache_dir Dossier où stocker les fichiers téléchargés (par défaut ".datagouv_cache")
#' @param force_download Forcer le téléchargement même si présent en cache
#' @param sheet Feuille Excel à lire (index/nom) si applicable
#' @return data.frame/tibble, objet sf (géo) ou liste (ZIP multi-fichiers)
load_datagouv_resource <- function(resource_id,
                                   cache_dir = ".datagouv_cache",
                                   force_download = FALSE,
                                   sheet = NULL) {
  url <- .dgr_resource_url(resource_id)
  
  # Création du cache si besoin
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  
  # HEAD pour récupérer métadonnées
  resp <- httr::HEAD(url, httr::timeout(60))
  if (httr::http_error(resp)) {
    stop("Impossible d'accéder à la ressource (HEAD): ",
         httr::http_status(resp)$message, call. = FALSE)
  }
  final_url <- resp$url
  ext <- .dgr_guess_extension(resp, final_url)
  if (!nzchar(ext)) ext <- "dat"
  
  cache_file <- file.path(cache_dir, paste0(resource_id, ".", ext))
  
  # Vérifier le cache
  if (file.exists(cache_file) && !force_download) {
    message("Lecture depuis le cache: ", cache_file)
    return(.dgr_read_any(cache_file, ext))
  }
  
  # Télécharger et mettre en cache
  message("Téléchargement de la ressource...")
  resp_get <- httr::GET(url, httr::write_disk(cache_file, overwrite = TRUE), httr::timeout(600))
  if (httr::http_error(resp_get)) {
    stop("Téléchargement échoué: ", httr::http_status(resp_get)$message, call. = FALSE)
  }
  
  obj <- .dgr_read_any(cache_file, ext)
  
  if (!is.null(sheet) && (ext %in% c("xlsx","xls"))) {
    obj <- suppressMessages(readxl::read_excel(cache_file, sheet = sheet))
  }
  
  obj
}
