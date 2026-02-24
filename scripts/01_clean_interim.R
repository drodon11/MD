# scripts/01_clean_interim.R
# Limpieza + renombrado + tipado según metadata
# + cálculo de taxAmount (totalFare - baseFare)
# + cálculo de layoverNumber (n_segments - 1) desde strings "||"
# + segmentDistance total (suma de segmentos)

# ---------- helpers ----------
ensure_pkg <- function(pkgs) {
  for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
ensure_pkg(c("readr", "lubridate"))
library(readr)
library(lubridate)

# ---------- paths ----------
project_root <- getwd()
raw_path <- file.path(project_root, "data", "raw", "flightprices_subset.csv")
out_dir  <- file.path(project_root, "data", "interim")
out_path <- file.path(out_dir, "flightprices_interim.rds")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(raw_path)) {
  stop(paste0("No encuentro el CSV en:\n  ", raw_path,
              "\nAbre el .Rproj del proyecto para que getwd() sea la raíz (flightprices-ml)."))
}

# ---------- 1) read ----------
df <- read_csv(raw_path, show_col_types = FALSE)

# ---------- 2) rename (Original -> Assigned) ----------
rename_map <- c(
  "flightDate"                   = "date",
  "startingAirport"              = "startApt",
  "destinationAirport"           = "destApt",
  "elapsedDays"                  = "elapsedDays",
  "isBasicEconomy"               = "economy",
  "isNonStop"                    = "nonStop",
  "baseFare"                     = "baseFare",
  "totalFare"                    = "totalPrice",
  "seatsRemaining"               = "seatsLeft",
  "totalTravelDistance"          = "travelDistance",
  "segmentsDepartureTimeRaw"     = "departure_raw",
  "segmentsArrivalTimeRaw"       = "arrival_raw",
  "segmentsAirlineName"          = "airline",
  "segmentsEquipmentDescription" = "equipment",
  "segmentsDistance"             = "segmentDistance_raw"
)

rename_using_map <- function(data, map) {
  nms <- names(data)
  for (orig in names(map)) {
    idx <- which(tolower(nms) == tolower(orig))
    if (length(idx) == 1) nms[idx] <- unname(map[[orig]])
  }
  names(data) <- nms
  data
}
df <- rename_using_map(df, rename_map)

# ---------- 3) CALCULAR taxAmount y layoverNumber desde columnas originales ----------
# taxAmount = totalPrice - baseFare
if (!"taxAmount" %in% names(df)) df$taxAmount <- NA_real_
if ("totalPrice" %in% names(df) && "baseFare" %in% names(df)) {
  df$taxAmount <- suppressWarnings(as.numeric(df$totalPrice) - as.numeric(df$baseFare))
}

# layoverNumber = n_segments - 1
# Usamos departure_raw (que viene con "||" cuando hay varios segmentos)
if (!"layoverNumber" %in% names(df)) df$layoverNumber <- NA_real_

count_segments <- function(x) {
  if (is.na(x) || is.null(x)) return(NA_integer_)
  parts <- unlist(strsplit(as.character(x), "\\|\\|"))
  parts <- trimws(parts)
  parts <- parts[parts != ""]
  length(parts)
}

if ("departure_raw" %in% names(df)) {
  nseg <- vapply(df$departure_raw, count_segments, integer(1))
  df$layoverNumber <- pmax(nseg - 1, 0)
  # coherencia con nonStop si existe
  if ("nonStop" %in% names(df)) {
    df$layoverNumber[df$nonStop %in% TRUE] <- 0
  }
}

# segmentDistance total: si viene "327||221" -> 548
sum_segments_num <- function(x) {
  if (is.na(x) || is.null(x)) return(NA_real_)
  parts <- unlist(strsplit(as.character(x), "\\|\\|"))
  parts <- trimws(parts)
  parts <- parts[parts != "" & parts != "None"]
  nums <- suppressWarnings(as.numeric(parts))
  if (all(is.na(nums))) return(NA_real_)
  sum(nums, na.rm = TRUE)
}

df$segmentDistance <- NA_real_
if ("segmentDistance_raw" %in% names(df)) {
  df$segmentDistance <- vapply(df$segmentDistance_raw, sum_segments_num, numeric(1))
}

# ---------- 4) type conversions (según metadata) ----------
# date: DATE
# datetimes: convertimos SOLO el primer segmento a POSIXct
parse_iso_dt_first <- function(x) {
  if (is.na(x) || is.null(x)) return(NA_character_)
  first <- strsplit(as.character(x), "\\|\\|")[[1]][1]
  gsub("T", " ", first, fixed = TRUE)
}

if ("departure_raw" %in% names(df)) {
  dep_chr <- sapply(df$departure_raw, parse_iso_dt_first, USE.NAMES = FALSE)
  df$departure <- suppressWarnings(ymd_hms(dep_chr, tz = "UTC"))
} else {
  df$departure <- as.POSIXct(NA)
}

if ("arrival_raw" %in% names(df)) {
  arr_chr <- sapply(df$arrival_raw, parse_iso_dt_first, USE.NAMES = FALSE)
  df$arrival <- suppressWarnings(ymd_hms(arr_chr, tz = "UTC"))
} else {
  df$arrival <- as.POSIXct(NA)
}

# Booleans
to_logical <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(x != 0)
  x <- tolower(trimws(as.character(x)))
  x %in% c("true", "t", "1", "yes", "y")
}
if ("economy" %in% names(df)) df$economy <- to_logical(df$economy)
if ("nonStop" %in% names(df)) df$nonStop <- to_logical(df$nonStop)

# Numerics (metadata)
to_numeric <- function(x) suppressWarnings(as.numeric(x))
num_cols <- intersect(
  c("elapsedDays","taxAmount","totalPrice","seatsLeft","travelDistance","segmentDistance","layoverNumber"),
  names(df)
)
for (cname in num_cols) df[[cname]] <- to_numeric(df[[cname]])

# Texto
char_cols <- intersect(c("startApt","destApt","airline","equipment"), names(df))
for (cname in char_cols) df[[cname]] <- as.character(df[[cname]])

# ---------- 5) range checks (metadata) ----------
clamp <- function(x, lo, hi) if (is.numeric(x)) pmin(pmax(x, lo), hi) else x
if ("elapsedDays" %in% names(df))   df$elapsedDays   <- clamp(df$elapsedDays,   0, 1.04)
if ("seatsLeft" %in% names(df))     df$seatsLeft     <- clamp(df$seatsLeft,     0, 10)
if ("layoverNumber" %in% names(df)) df$layoverNumber <- clamp(df$layoverNumber, 0, 3)

# ---------- 6) keep only metadata columns (orden final) ----------
keep_order <- c(
  "date","startApt","destApt","elapsedDays","economy","nonStop",
  "taxAmount","totalPrice","seatsLeft","travelDistance",
  "departure","arrival","airline","equipment","segmentDistance","layoverNumber"
)

# asegúrate de que existen todas
for (nm in keep_order) if (!nm %in% names(df)) df[[nm]] <- NA

df <- df[, keep_order, drop = FALSE]

# ---------- 7) save ----------
saveRDS(df, out_path)

message("\n✅ Archivo guardado correctamente en:")
message("   ", normalizePath(out_path, winslash = "/"))

# ---------- 8) quick sanity print ----------
message("\nColumnas finales:")
print(colnames(df))

message("\nTipos de datos:")
print(data.frame(
  Variable = names(df),
  Tipo = sapply(df, function(x) paste(class(x), collapse = "/")),
  row.names = NULL
))

message("\nDimensiones:")
print(dim(df))

message("\nPrimeras filas:")
print(head(df))