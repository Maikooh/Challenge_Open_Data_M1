# datagouv_loader.R — loader data.gouv.fr robuste (ZIP UTF-8, filtres de formats)

DATAGOUV_URL <- "https://www.data.gouv.fr/"

# -- utils --
`%||%` <- function(a,b) if (is.null(a) || (is.character(a) && !nzchar(a))) b else a

.dgr_api_v2_resource <- function(resource_id) {
  url <- sprintf("%sapi/2/datasets/resources/%s", DATAGOUV_URL, resource_id)
  resp <- httr::GET(url, httr::timeout(60), httr::add_headers(`User-Agent`="R (httr)"))
  if (httr::http_error(resp)) stop(httr::http_status(resp)$message, call. = FALSE)
  jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
}

.dgr_download_url <- function(resource_id) sprintf("%sfr/datasets/r/%s", DATAGOUV_URL, resource_id)

# Détecte le format réel de l'URL cible (Content-Type / filename des headers)
.dgr_probe_url_format <- function(url) {
  h <- httr::HEAD(url, httr::timeout(60), httr::add_headers(`User-Agent`="R (httr)"))
  if (httr::http_error(h)) return(list(ext = "", ct = "", filename = "", url = url))
  hdr <- httr::headers(h)
  cd  <- hdr[["content-disposition"]] %||% ""
  ct  <- tolower(hdr[["content-type"]] %||% "")
  fname <- ""
  if (nzchar(cd)) {
    m <- regmatches(cd, regexpr('filename="([^"]+)"', cd))
    if (length(m)) fname <- sub('^filename="', "", sub('"$', "", m))
  }
  ext_from_name <- tolower(tools::file_ext(fname))
  ext_from_ct <- if (grepl("zip", ct)) "zip"
    else if (grepl("parquet", ct)) "parquet"
    else if (grepl("excel|spreadsheetml", ct)) "xlsx"
    else if (grepl("geo\\+json|geojson", ct)) "geojson"
    else if (grepl("csv", ct)) "csv"
    else if (grepl("json", ct)) "json"
    else if (grepl("pdf", ct)) "pdf"
    else ""
  list(ext = if (nzchar(ext_from_name)) ext_from_name else ext_from_ct,
       ct = ct, filename = fname, url = h$url %||% url)
}

# Lecture tabulaire rapide (CSV/TSV/TXT, gère .gz)
.dgr_read_tabular <- function(path, sep=";", ...) {
  as.data.frame(data.table::fread(path, sep = sep, showProgress = FALSE, ...))
}

# Est-ce un vrai ZIP ?
.is_zip_file <- function(path) {
  if (!file.exists(path)) return(FALSE)
  con <- file(path, "rb"); on.exit(clos
