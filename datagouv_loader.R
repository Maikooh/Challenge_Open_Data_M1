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
  con <- file(path, "rb"); on.exit(close(con), add = TRUE)
  sig <- readBin(con, "raw", n = 4)
  identical(as.raw(c(0x50,0x4B,0x03,0x04)), sig)
}

# Liste des extensions supportées (dans ou hors ZIP)
.SUP_FMT <- c("csv","tsv","txt","csv.gz","gz","xlsx","xls","xlsm","parquet","geojson")
# Formats explicitement non tabulaires
.UNSUPPORTED <- c("pdf","ivt")

# -- fonction principale --
#' Charger une ressource data.gouv.fr par resource_id (avec gestion ZIP/UTF-8 & formats)
#' @param resource_id UUID de la ressource
#' @param sep séparateur CSV (par défaut ";")
#' @param cache_dir dossier de cache
#' @param force_download TRUE pour ignorer le cache
#' @param .file_and_format interne (extraction ZIP) : list(path, format)
#' @param ... options passées aux lecteurs (sheet= pour Excel, etc.)
load_table_from_resource_id <- function(resource_id,
                                        sep = ";",
                                        cache_dir = ".datagouv_cache",
                                        force_download = FALSE,
                                        .file_and_format = NULL,
                                        ...) {
  if (!is.null(resource_id)) resource_id <- trimws(resource_id)

  # 1) Déterminer format + cible
  if (!is.null(.file_and_format)) {
    fmt <- tolower(.file_and_format[[2]])
    target <- .file_and_format[[1]]
    probe <- list(ext = fmt, ct = "", filename = "", url = target)
  } else {
    meta <- .dgr_api_v2_resource(resource_id)
    fmt <- tolower(meta$resource$format %||% "")
    target <- .dgr_download_url(resource_id)
    probe <- .dgr_probe_url_format(target)
    # En cas de contradiction, faire confiance au HEAD
    if (nzchar(probe$ext)) fmt <- probe$ext
  }

  # 2) Gestion formats non supportés (PDF, IVT…)
  if (fmt %in% .UNSUPPORTED) {
    stop(sprintf(
      "Format non supporté: %s.\n- PDF: pas tabulaire.\n- IVT: format Beyond 20/20 (INSEE). "+
      "Cherche une ressource CSV/Excel ou convertis manuellement.", toupper(fmt)
    ), call. = FALSE)
  }

  # 3) Préparer cache
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  ext_for_cache <- switch(fmt,
    "csv.gz"="csv.gz","xls"="xls","xlsx"="xlsx","xlsm"="xlsm","parquet"="parquet",
    "geojson"="geojson","zip"="zip","csv"="csv","tsv"="tsv","txt"="txt", fmt)
  cache_file <- if (!is.null(resource_id)) file.path(cache_dir, paste0(resource_id, ".", ext_for_cache)) else NULL

  .get_local <- function(url, fileext) {
    dest <- if (!is.null(cache_file)) cache_file else tempfile(fileext = paste0(".", fileext))
    if (!file.exists(dest) || isTRUE(force_download)) {
      httr::GET(url,
        httr::write_disk(dest, overwrite = TRUE),
        httr::timeout(600),
        httr::add_headers(`User-Agent`="R (httr)")
      ) -> g
      if (httr::http_error(g))
        stop("Téléchargement échoué: ", httr::http_status(g)$message, call. = FALSE)
    }
    dest
  }

  # 4) Routage par format
  # GeoJSON
  if (fmt == "geojson") {
    if (!requireNamespace("sf", quietly = TRUE))
      stop("Installez 'sf' pour lire du GeoJSON.", call. = FALSE)
    if (!is.null(resource_id)) target <- .get_local(target, "geojson")
    return(suppressMessages(sf::st_read(target, quiet = TRUE)))
  }

  # ZIP (via {archive}, gère les noms accentués)
  if (fmt == "zip") {
    if (!requireNamespace("archive", quietly = TRUE))
      stop("Installez le package 'archive' (nécessaire pour lire les ZIP UTF-8).", call. = FALSE)
    zpath <- if (!is.null(resource_id)) .get_local(target, "zip") else target

    if (!.is_zip_file(zpath)) {
      message("Avertissement : le serveur n'a pas renvoyé un ZIP. Lecture comme fichier tabulaire.")
      return(.dgr_read_tabular(zpath, sep = sep, ...))
    }

    zls <- archive::archive_ls(zpath)
    if (!nrow(zls)) stop("Le ZIP est vide.", call. = FALSE)

    # Filtrer sur les formats supportés
    zls$ext <- tolower(tools::file_ext(zls$path))
    zls_ok <- zls[zls$ext %in% .SUP_FMT, ]
    if (!nrow(zls_ok)) {
      exts <- sort(unique(zls$ext))
      stop(sprintf(
        "Aucun fichier supporté dans ce ZIP.\nExtensions trouvées: %s\nFormats attendus: %s",
        paste(exts, collapse=", "), paste(.SUP_FMT, collapse=", ")
      ), call. = FALSE)
    }

    # Choisir le plus gros parmi les supportés
    largest_row <- zls_ok[which.max(zls_ok$size), ]
    inside <- largest_row$path
    fmt2 <- largest_row$ext
    message(sprintf("Info : ZIP multi-fichiers, ouverture du plus gros supporté : %s", basename(inside)))

    td <- tempfile("unz_"); dir.create(td)
    archive::archive_extract(zpath, files = inside, dir = td)
    extracted <- file.path(td, inside)

    return(load_table_from_resource_id(
      NULL, sep = sep, cache_dir = cache_dir,
      .file_and_format = list(extracted, fmt2), ...
    ))
  }

  # Parquet
  if (fmt == "parquet") {
    if (!requireNamespace("arrow", quietly = TRUE))
      stop("Installez 'arrow' pour lire du Parquet.", call. = FALSE)
    p <- if (!is.null(resource_id)) .get_local(target, "parquet") else target
    return(as.data.frame(arrow::read_parquet(p, ...)))
  }

  # Excel (xls/xlsx/xlsm)
  if (fmt %in% c("xls","xlsx","xlsm")) {
    x <- if (!is.null(resource_id)) .get_local(target, fmt) else target
    return(suppressMessages(readxl::read_excel(x, ...)))
  }

  # CSV & assimilés (incl. csv.gz)
  if (fmt %in% c("csv.gz","gz")) {
    cgz <- if (!is.null(resource_id)) .get_local(target, "csv.gz") else target
    return(.dgr_read_tabular(cgz, sep = sep, ...))
  }

  # CSV/TSV/TXT ou format incertain -> essai direct puis fallback local
  out <- tryCatch(.dgr_read_tabular(target, sep = sep, ...),
                  error = function(e) {
                    loc <- if (!is.null(resource_id)) .get_local(target, fmt %||% "csv") else {
                      tmp <- tempfile(fileext = paste0(".", fmt %||% "csv"))
                      utils::download.file(target, tmp, mode="wb", quiet=TRUE); tmp
                    }
                    .dgr_read_tabular(loc, sep = sep, ...)
                  })
  out
}
