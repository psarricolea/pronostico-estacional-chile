#!/usr/bin/env Rscript
# =============================================================================
#  Pronóstico estacional · Chile · rutina mensual (GitHub Actions)
#  Multi-sistema Copernicus C3S: Tmax, Tmin y precipitación por terciles
#
#  1. Descarga del CDS (ecmwfr) el pronóstico de la última inicialización y el
#     hindcast 1993–2016 de cada sistema, para un recuadro que cubre Chile (1°).
#  2. Por sistema, celda y período: terciles del hindcast (todos los miembros x
#     años) y fracción de miembros del pronóstico en cada tercil. En Tmax y Tmin
#     el hindcast se lleva al nivel del año actual con su tendencia lineal.
#  3. Multi-sistema: promedio con igual peso por sistema.
#  4. Escribe data.js e img/*.png del visor. index.html no se toca.
#
#  Variables de entorno:
#    ecmwfr_PAT   token personal del CDS (en GitHub: secreto CDS_API_KEY)
#    INIT_YEAR    año de inicialización  (vacío = automático)
#    INIT_MONTH   mes de inicialización  (vacío = automático)
#    FORZAR       "true" para recalcular aunque el visor ya tenga esa inicialización
#    DESCARGAR    "false" para usar solo lo que ya esté en raw/ (pruebas)
# =============================================================================

# Textos con tildes: forzar UTF-8 (en Linux el locale por defecto puede ser "C")
if (.Platform$OS.type == "unix")
  for (loc in c("C.UTF-8", "en_US.UTF-8", "es_CL.UTF-8"))
    if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break

suppressPackageStartupMessages({
  library(ncdf4); library(terra); library(jsonlite)
})
terraOptions(progress = 0)

# ---- 1. Configuración --------------------------------------------------------
RAIZ     <- normalizePath(Sys.getenv("RAIZ", "."), mustWork = TRUE)
DIR_RAW  <- file.path(RAIZ, "raw")
HC_YEARS <- 1993:2016                    # hindcast común a todos los sistemas
AREA     <- c(-15, -78, -58, -64)        # N, W, S, E (recuadro de Chile)
LEADS      <- 2:5                        # meses que se publican (1 = mes de inicio)
TRIMESTRES <- list(2:4, 3:5)             # trimestres que se publican
UMBRAL_SENAL <- 0.40                     # categoría con señal (como C3S)
PREC_SECO_MM <- 5                        # lluvia normal < 5 mm/mes = zona seca, sin pronóstico
MIN_SISTEMAS <- 4                        # mínimo de sistemas para publicar
MAX_ACTIVOS  <- 8                        # solicitudes simultáneas al CDS
LIMITE_HORAS <- 5                        # tiempo máximo de descargas
AUTOR  <- "Pablo Sarricolea"
LIMITES <- "GADM 4.1"

# Sistemas C3S (CDS 'system'). Si alguno cambia de versión, editar aquí:
# https://confluence.ecmwf.int/display/CKB/Description+of+the+C3S+seasonal+multi-system
SYSTEMS <- data.frame(
  centre = c("ecmwf", "ukmo", "meteo_france", "dwd", "cmcc",
             "ncep", "jma", "eccc", "eccc", "bom"),
  system = c("51", "610", "9", "22", "4", "2", "4", "4", "5", "2"),
  label  = c("ECMWF-SEAS5", "UKMO-GloSea6", "MF-S9", "DWD-GCFS2.2", "CMCC-SPS4",
             "NCEP-CFSv2", "JMA-CPS4", "ECCC-CanESM5.1", "ECCC-GEM5.2", "BOM-ACCESS-S2"),
  stringsAsFactors = FALSE
)

VARS <- list(
  tmax = list(cds = "maximum_2m_temperature_in_the_last_24_hours",
              nc = c("mx2t24", "tasmax", "tmax"), nombre = "Temperatura máxima",
              tendencia = TRUE),
  tmin = list(cds = "minimum_2m_temperature_in_the_last_24_hours",
              nc = c("mn2t24", "tasmin", "tmin"), nombre = "Temperatura mínima",
              tendencia = TRUE),
  prec = list(cds = "total_precipitation",
              nc = c("tprate", "tp", "pr"), nombre = "Precipitación",
              tendencia = FALSE)
)

# ---- 2. Inicialización -------------------------------------------------------
# C3S publica el día 10 a las 12 UTC. Sin INIT_* explícitos: mes actual desde
# el 10 (13 UTC) en adelante; antes de eso, el mes anterior.
ahora <- as.POSIXlt(Sys.time(), tz = "UTC")
iy <- Sys.getenv("INIT_YEAR"); im <- Sys.getenv("INIT_MONTH")
if (nzchar(iy) && nzchar(im)) {
  INIT_YEAR <- as.integer(iy); INIT_MONTH <- as.integer(im)
} else {
  f <- as.Date(sprintf("%d-%02d-01", ahora$year + 1900, ahora$mon + 1))
  if (ahora$mday < 10 || (ahora$mday == 10 && ahora$hour < 13))
    f <- seq(f, by = "-1 month", length.out = 2)[2]
  INIT_YEAR <- as.integer(format(f, "%Y")); INIT_MONTH <- as.integer(format(f, "%m"))
}
stopifnot(INIT_MONTH %in% 1:12)
INIT_TAG  <- sprintf("%d-%02d", INIT_YEAR, INIT_MONTH)
FORZAR    <- tolower(Sys.getenv("FORZAR", "false")) %in% c("true", "1", "si", "sí")
DESCARGAR <- !tolower(Sys.getenv("DESCARGAR", "true")) %in% c("false", "0", "no")

salida_gh <- function(k, v) {
  f <- Sys.getenv("GITHUB_OUTPUT")
  if (nzchar(f)) cat(sprintf("%s=%s\n", k, v), file = f, append = TRUE)
}
salida_gh("init_tag", INIT_TAG)

f_data <- file.path(RAIZ, "data.js")
if (!FORZAR && file.exists(f_data)) {
  l1 <- readLines(f_data, n = 1, warn = FALSE, encoding = "UTF-8")
  previo <- regmatches(l1, regexpr('"init_tag":"[0-9]{4}-[0-9]{2}"', l1))
  if (length(previo) && grepl(INIT_TAG, previo, fixed = TRUE)) {
    message("El visor ya tiene la inicialización ", INIT_TAG, ". Nada que hacer.")
    salida_gh("actualizado", "false")
    quit(save = "no", status = 0)
  }
}
message("== Inicialización ", INIT_TAG, " ==")

# ---- 3. Períodos -------------------------------------------------------------
MESES <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio",
           "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
COD   <- c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DIC")
f0 <- as.Date(sprintf("%d-%02d-01", INIT_YEAR, INIT_MONTH))
fecha_lead <- function(l) seq(f0, by = "month", length.out = l)[l]
mes_de  <- function(l) as.integer(format(fecha_lead(l), "%m"))
anio_de <- function(l) format(fecha_lead(l), "%Y")

PERIODOS <- list(); ETIQ <- list()
for (l in LEADS) {
  k <- COD[mes_de(l)]; PERIODOS[[k]] <- l; ETIQ[[k]] <- paste(MESES[mes_de(l)], anio_de(l))
}
for (tl in TRIMESTRES) {
  ms <- vapply(tl, mes_de, 1L)
  k <- paste(substr(MESES[ms], 1, 1), collapse = "")
  PERIODOS[[k]] <- tl; ETIQ[[k]] <- paste(substr(MESES[ms], 1, 3), collapse = "–")
}
ALL_LEADS <- sort(unique(unlist(PERIODOS)))

# ---- 4. Descargas ------------------------------------------------------------
archivo_raw <- function(tipo, i, v) {
  s <- SYSTEMS[i, ]
  if (tipo == "fc") file.path(DIR_RAW, "fc", sprintf("%s%s_%s_%s.nc", s$centre, s$system, v, INIT_TAG))
  else file.path(DIR_RAW, "hc", sprintf("%s%s_%s_m%02d_%d%d.nc", s$centre, s$system, v,
                                        INIT_MONTH, min(HC_YEARS), max(HC_YEARS)))
}
peticion <- function(tipo, i, v) {
  s <- SYSTEMS[i, ]
  list(dataset_short_name = "seasonal-monthly-single-levels",
       originating_centre = s$centre, system = s$system,
       variable = VARS[[v]]$cds, product_type = "monthly_mean",
       year = if (tipo == "fc") as.character(INIT_YEAR) else as.character(HC_YEARS),
       month = sprintf("%02d", INIT_MONTH),
       leadtime_month = as.character(ALL_LEADS),
       area = AREA, grid = c(1, 1),
       data_format = "netcdf",
       target = basename(archivo_raw(tipo, i, v)))
}

# Si el CDS entrega un .zip, extrae el .nc y lo deja con el nombre esperado
acomodar <- function(bajado, destino) {
  if (is.na(bajado) || !file.exists(bajado)) return(FALSE)
  es_zip <- identical(readBin(bajado, "raw", 2), charToRaw("PK"))
  if (es_zip) {
    tmp <- tempfile(); dir.create(tmp)
    utils::unzip(bajado, exdir = tmp)
    nc <- list.files(tmp, pattern = "\\.nc$", full.names = TRUE, recursive = TRUE)
    if (!length(nc)) return(FALSE)
    file.copy(nc[1], destino, overwrite = TRUE); unlink(c(tmp, bajado), recursive = TRUE)
  } else if (normalizePath(bajado) != normalizePath(destino, mustWork = FALSE)) {
    file.rename(bajado, destino)
  }
  file.exists(destino)
}

descargar <- function(trab) {
  pend <- which(!file.exists(trab$archivo))
  message(length(trab$archivo) - length(pend), " archivos ya estaban; faltan ", length(pend))
  if (!length(pend) || !DESCARGAR) return(invisible())
  suppressPackageStartupMessages(library(ecmwfr))
  if (!nzchar(Sys.getenv("ecmwfr_PAT")))
    message("Aviso: no hay ecmwfr_PAT; se intentará con el token guardado en el keyring.")
  cola <- pend; activos <- list()
  t_lim <- Sys.time() + LIMITE_HORAS * 3600
  while ((length(cola) || length(activos)) && Sys.time() < t_lim) {
    while (length(cola) && length(activos) < MAX_ACTIVOS) {
      i <- cola[1]; cola <- cola[-1]
      dir.create(dirname(trab$archivo[i]), recursive = TRUE, showWarnings = FALSE)
      ob <- tryCatch(
        wf_request(trab$req[[i]], path = dirname(trab$archivo[i]), transfer = FALSE,
                   verbose = FALSE, retry = 30),
        error = function(e) { message("  ✗ ", basename(trab$archivo[i]), " no se pudo enviar: ",
                                      conditionMessage(e)); NULL })
      if (!is.null(ob)) activos[[as.character(i)]] <- ob
    }
    Sys.sleep(15)
    for (k in names(activos)) {
      i <- as.integer(k); ob <- activos[[k]]
      r <- tryCatch({ ob$download(); if (ob$is_success()) "ok" else "pend" },
                    error = function(e) { message("  ✗ ", basename(trab$archivo[i]), ": ",
                                                  conditionMessage(e)); "error" })
      if (r == "pend") next
      activos[[k]] <- NULL
      if (r == "ok") {
        ok <- acomodar(ob$get_file(), trab$archivo[i])
        message(if (ok) "  ✓ " else "  ✗ (archivo inválido) ", basename(trab$archivo[i]))
        try(ob$delete(), silent = TRUE)
      }
    }
  }
  if (length(cola) || length(activos)) message("Se acabó el tiempo de descarga; sigo con lo que hay.")
}

trab <- expand.grid(i = seq_len(nrow(SYSTEMS)), v = names(VARS), tipo = c("hc", "fc"),
                    stringsAsFactors = FALSE)
trab$archivo <- mapply(archivo_raw, trab$tipo, trab$i, trab$v)
trab$req <- Map(peticion, trab$tipo, trab$i, trab$v)
descargar(trab)

# ---- 5. Lectura de NetCDF del CDS --------------------------------------------
# Devuelve array [lon, lat, lead, miembro, año] + coordenadas
leer_nc <- function(f, cands) {
  nc <- nc_open(f); on.exit(nc_close(nc))
  v <- names(nc$var)[tolower(names(nc$var)) %in% tolower(cands)]
  if (!length(v)) stop("sin variable ", paste(cands, collapse = "/"), " en ", basename(f))
  v <- v[1]
  a  <- ncvar_get(nc, v, collapse_degen = FALSE)
  dn <- vapply(nc$var[[v]]$dim, function(d) d$name, "")
  dims <- list(lon = "longitude|^lon", lat = "latitude|^lat", lead = "forecastMonth|leadtime",
               mem = "^number$|realization|member", t = "forecast_reference_time|^time$|indexing_time")
  pos <- vapply(dims, function(p) { w <- grep(p, dn); if (length(w)) w[1] else NA_integer_ }, 1L)
  if (anyNA(pos[c("lon", "lat")])) stop("dimensiones inesperadas en ", basename(f), ": ", paste(dn, collapse = ","))
  otras <- setdiff(seq_along(dn), pos[!is.na(pos)])
  if (length(otras)) {            # dimensiones extra de largo 1: se descartan
    if (any(dim(a)[otras] > 1)) stop("dimensión extra en ", basename(f), ": ", paste(dn[otras], collapse = ","))
  }
  orden <- pos[!is.na(pos)]
  a <- aperm(a, c(orden, otras))
  d <- dim(a)[seq_along(orden)]; names(d) <- names(pos)[!is.na(pos)]
  full <- c(lon = 1, lat = 1, lead = 1, mem = 1, t = 1); full[names(d)] <- d
  dim(a) <- full
  coord <- function(nm) if (nm %in% names(nc$dim)) as.numeric(nc$dim[[nm]]$vals) else NULL
  lon <- coord(dn[pos["lon"]]); lat <- coord(dn[pos["lat"]])
  lead <- if (!is.na(pos["lead"])) coord(dn[pos["lead"]]) else 1
  anios <- NA
  if (!is.na(pos["t"])) {
    tv <- coord(dn[pos["t"]]); u <- nc$dim[[dn[pos["t"]]]]$units
    if (!is.null(tv) && length(u) && grepl("since", u)) {
      paso <- tolower(sub(" since.*", "", u))
      orig <- as.POSIXct(sub(".*since ", "", u), tz = "UTC",
                         tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OS", "%Y-%m-%d"))
      mult <- switch(paso, seconds = 1, minutes = 60, hours = 3600, days = 86400, 1)
      anios <- as.integer(format(orig + tv * mult, "%Y"))
    }
  }
  list(a = a, lon = lon, lat = lat, lead = lead, anios = anios)
}

# Lleva el array a la grilla de referencia si viene en otra (bilineal, terra)
a_grilla <- function(x, ref) {
  if (length(x$lon) == length(ref$lon) && length(x$lat) == length(ref$lat) &&
      max(abs(x$lon - ref$lon)) < 1e-4 && max(abs(x$lat - ref$lat)) < 1e-4) return(x$a)
  message("    regrillado a la grilla común")
  d <- dim(x$a); n3 <- prod(d[3:5])
  m <- matrix(x$a, d[1] * d[2], n3)
  ilat <- order(x$lat, decreasing = TRUE)
  r <- rast(nrows = d[2], ncols = d[1], nlyrs = n3,
            xmin = min(x$lon) - abs(diff(x$lon[1:2])) / 2, xmax = max(x$lon) + abs(diff(x$lon[1:2])) / 2,
            ymin = min(x$lat) - abs(diff(x$lat[1:2])) / 2, ymax = max(x$lat) + abs(diff(x$lat[1:2])) / 2,
            crs = "EPSG:4326")
  # celdas de terra: fila 1 = norte; columnas oeste->este
  ilon <- order(x$lon)
  idx <- as.vector(outer(ilon, (ilat - 1) * d[1], function(i, j) i + j))   # orden (lon rápido, lat norte->sur)
  values(r) <- m[idx, , drop = FALSE]
  rr <- rast(nrows = length(ref$lat), ncols = length(ref$lon), nlyrs = n3,
             xmin = min(ref$lon) - 0.5, xmax = max(ref$lon) + 0.5,
             ymin = min(ref$lat) - 0.5, ymax = max(ref$lat) + 0.5, crs = "EPSG:4326")
  rr <- resample(r, rr, method = "bilinear")
  v <- values(rr, mat = TRUE)                      # filas: norte->sur, oeste->este
  # devolver en el orden de ref$lon / ref$lat
  jlat <- match(ref$lat, sort(ref$lat, decreasing = TRUE)); jlon <- match(ref$lon, sort(ref$lon))
  out <- array(NA_real_, c(length(ref$lon), length(ref$lat), d[3:5]))
  vv <- array(v, c(length(ref$lon), length(ref$lat), d[3:5]))
  out[, , , , ] <- vv[jlon, jlat, , , , drop = FALSE]
  out
}

# ---- 6. Terciles por sistema -------------------------------------------------
SEG_MES <- 86400 * 1000 * 30.44           # tasa m/s -> mm/mes

media_periodo <- function(a, leads_arch, leads) {
  w <- match(leads, leads_arch)
  if (anyNA(w)) return(NULL)
  apply(a[, , w, , , drop = FALSE], c(1, 2, 4, 5), mean)    # [lon, lat, mem, t]
}

calc_sistema <- function(fc, hc, leads, var) {
  pf <- media_periodo(fc$a, fc$lead, leads); ph <- media_periodo(hc$a, hc$lead, leads)
  if (is.null(pf) || is.null(ph)) return(NULL)
  nlon <- dim(ph)[1]; nlat <- dim(ph)[2]; nc_ <- nlon * nlat
  H <- matrix(ph, nc_, dim(ph)[3] * dim(ph)[4])           # celdas x (miembro, año)
  Fm <- matrix(pf, nc_, dim(pf)[3] * dim(pf)[4])
  anio_col <- rep(if (length(hc$anios) == dim(ph)[4] && !anyNA(hc$anios)) hc$anios
                  else HC_YEARS[seq_len(dim(ph)[4])], each = dim(ph)[3])
  if (VARS[[var]]$tendencia) {                             # hindcast al nivel de INIT_YEAR
    anios_u <- sort(unique(anio_col))
    em <- sapply(anios_u, function(y) rowMeans(H[, anio_col == y, drop = FALSE], na.rm = TRUE))
    xc <- anios_u - mean(anios_u)
    b <- as.vector((em - rowMeans(em)) %*% xc) / sum(xc^2)
    H <- H + outer(b, INIT_YEAR - anio_col)
  }
  t1 <- apply(H, 1, stats::quantile, probs = 1 / 3, na.rm = TRUE, names = FALSE)
  t2 <- apply(H, 1, stats::quantile, probs = 2 / 3, na.rm = TRUE, names = FALSE)
  nf <- rowSums(!is.na(Fm))
  pb <- rowSums(Fm < t1, na.rm = TRUE) / nf
  pa <- rowSums(Fm > t2, na.rm = TRUE) / nf
  pn <- 1 - pb - pa
  mh <- rowMeans(H, na.rm = TRUE); mf <- rowMeans(Fm, na.rm = TRUE)
  if (var == "prec") {
    seco <- mh * SEG_MES < PREC_SECO_MM | t2 <= t1
    an <- 100 * (mf / mh - 1)
    an[seco] <- NA; pb[seco] <- NA; pn[seco] <- NA; pa[seco] <- NA
  } else an <- mf - mh
  list(anom = an, pb = pb, pn = pn, pa = pa)
}

# ---- 7. Cálculo multi-sistema ------------------------------------------------
ref <- NULL; res <- list(); usados <- list()
for (v in names(VARS)) {
  usados[[v]] <- character()
  for (i in seq_len(nrow(SYSTEMS))) {
    ff <- archivo_raw("fc", i, v); fh <- archivo_raw("hc", i, v)
    if (!file.exists(ff) || !file.exists(fh)) next
    r <- tryCatch({
      fc <- leer_nc(ff, VARS[[v]]$nc); hc <- leer_nc(fh, VARS[[v]]$nc)
      if (is.null(ref)) ref <- list(lon = fc$lon, lat = fc$lat)
      fc$a <- a_grilla(fc, ref); hc$a <- a_grilla(hc, ref)
      fc$lon <- hc$lon <- ref$lon; fc$lat <- hc$lat <- ref$lat
      lapply(PERIODOS, function(L) calc_sistema(fc, hc, L, v))
    }, error = function(e) { message("  ✗ ", SYSTEMS$label[i], " ", v, ": ", conditionMessage(e)); NULL })
    if (is.null(r)) next
    usados[[v]] <- c(usados[[v]], SYSTEMS$label[i])
    for (p in names(PERIODOS)) if (!is.null(r[[p]]))
      res[[v]][[p]] <- c(res[[v]][[p]], list(r[[p]]))
    message("  ok ", SYSTEMS$label[i], " · ", v,
            " (", dim(fc$a)[4], " miembros pronóstico, ", dim(hc$a)[4], "x", dim(hc$a)[5], " hindcast)")
  }
  message(v, ": ", length(usados[[v]]), " sistemas · ", paste(usados[[v]], collapse = ", "))
}
n_temp <- min(length(usados$tmax), length(usados$tmin))
if (n_temp < MIN_SISTEMAS)
  stop("Solo ", n_temp, " sistemas con Tmax y Tmin para ", INIT_TAG,
       ". ¿Ya está publicada la inicialización en el CDS? No se actualiza el visor.")
VARS_PUB <- names(VARS)[vapply(names(VARS), function(v) length(usados[[v]]) >= MIN_SISTEMAS, TRUE)]

promedio <- function(lst, k) {
  M <- sapply(lst, `[[`, k); if (is.null(dim(M))) M <- matrix(M, ncol = 1)
  out <- rowMeans(M, na.rm = TRUE)
  out[rowSums(!is.na(M)) < ncol(M) / 2] <- NA           # mayoría de sistemas sin dato
  out[is.nan(out)] <- NA
  out
}

# ---- 8. Productos web --------------------------------------------------------
nlon <- length(ref$lon); nlat <- length(ref$lat)
lon_o <- order(ref$lon); lat_o <- order(ref$lat, decreasing = TRUE)
dx <- abs(diff(sort(ref$lon))[1]); dy <- abs(diff(sort(ref$lat))[1])
grid_info <- list(xmin = min(ref$lon) - dx / 2, ymax = max(ref$lat) + dy / 2,
                  dx = dx, dy = dy, nx = nlon, ny = nlat)
# vector por celda (orden lon x lat del archivo) -> fila norte->sur, columna oeste->este
a_filas <- function(x) as.vector(matrix(x, nlon, nlat)[lon_o, lat_o])
a_rast <- function(lst) {
  r <- rast(nrows = nlat, ncols = nlon, nlyrs = length(lst),
            xmin = grid_info$xmin, xmax = grid_info$xmin + nlon * dx,
            ymin = grid_info$ymax - nlat * dy, ymax = grid_info$ymax, crs = "EPSG:4326")
  values(r) <- sapply(lst, a_filas); names(r) <- names(lst); r
}

# Límites de Chile desde regiones.js del propio visor
lr <- readLines(file.path(RAIZ, "regiones.js"), warn = FALSE, encoding = "UTF-8")
lr[1] <- sub("^\\s*window\\.REGIONES\\s*=\\s*", "", lr[1])
lr <- sub(";\\s*$", "", lr)
f_geo <- tempfile(fileext = ".geojson"); writeLines(lr, f_geo, useBytes = TRUE)
chile <- aggregate(vect(f_geo))

CFG <- list(
  tmax = list(unidad = "°C", dec = 1, brk = seq(-3, 3, 0.5),
              pal = c("#11467F", "#2B71B2", "#4B97C5", "#8CC1DC", "#C2DDEC", "#E7EFF4",
                      "#FAEBE3", "#FCCDB5", "#F29F7D", "#D96651", "#BB2F33", "#850824"),
              pal_cat = c("#08306b", "#2171b5", "#6baed6", "#c6dbef", "#bdbdbd",
                          "#f7f7f7", "#fcbba1", "#fb6a4a", "#cb181d", "#67000d"),
              neg = "más frío", pos = "más cálido",
              leyenda = "ANOMALÍA RESPECTO DE LO NORMAL (°C)",
              sub = "cuánto más cálido o más frío que lo normal promedian los modelos.",
              clima = "climatología 1993–2016 ajustada por tendencia"),
  prec = list(unidad = "%", dec = 0, brk = seq(-60, 60, 10),
              pal = c("#543005", "#8C510A", "#BF812D", "#DFC27D", "#F0E0B5", "#F7F2E6",
                      "#E8F4F1", "#C7EAE5", "#80CDC1", "#35978F", "#01665E", "#003C30"),
              pal_cat = c("#6B3D07", "#A6611A", "#D2A35C", "#EED9B0", "#bdbdbd",
                          "#f7f7f7", "#B9E3DD", "#66BDB0", "#1B8A80", "#00504A"),
              neg = "más seco", pos = "más lluvioso",
              leyenda = "ANOMALÍA RESPECTO DE LA LLUVIA NORMAL (%)",
              sub = "cuánto más lluvioso o más seco que lo normal (en % de la lluvia normal) promedian los modelos.",
              clima = "climatología 1993–2016",
              nota_seco = sprintf("Sin color: zona seca en el período (lluvia normal bajo %d mm/mes).", PREC_SECO_MM))
)
CFG$tmin <- CFG$tmax
CLASES <- c("Bajo ≥70%", "Bajo 60–70%", "Bajo 50–60%", "Bajo 40–50%",
            "Normal ≥40%", "Sin señal",
            "Sobre 40–50%", "Sobre 50–60%", "Sobre 60–70%", "Sobre ≥70%")

idx_categoria <- function(pb, pn, pa) {                   # 1..10 según CLASES
  P <- cbind(pb, pn, pa); ok <- stats::complete.cases(P)
  out <- rep(NA_integer_, length(pb)); if (!any(ok)) return(out)
  Pk <- P[ok, , drop = FALSE]
  cat <- max.col(Pk, ties.method = "first"); pmx <- Pk[cbind(seq_len(nrow(Pk)), cat)]
  tr <- findInterval(pmx, c(.5, .6, .7))
  v <- ifelse(pmx < UMBRAL_SENAL, 6L, ifelse(cat == 2, 5L, ifelse(cat == 1, 4L - tr, 7L + tr)))
  out[ok] <- as.integer(v); out
}

png_rgba <- function(idx, cols, file) {
  rgb <- grDevices::col2rgb(cols)
  ch <- lapply(1:3, function(k) subst(idx, from = seq_along(cols), to = rgb[k, ]))
  a <- ifel(is.na(idx), 0, 255)
  out <- rast(c(ch, list(a))); out <- ifel(is.na(out), 0, out)
  writeRaster(out, file, datatype = "INT1U", filetype = "PNG", overwrite = TRUE,
              NAflag = NA, gdal = c("WORLDFILE=NO"))
  unlink(paste0(file, ".aux.xml"))
}

plantilla <- rast(ext(-77.5, -65.5, -56.5, -17), resolution = 0.05, crs = "EPSG:4326")
dir.create(file.path(RAIZ, "img"), showWarnings = FALSE)
unlink(list.files(file.path(RAIZ, "img"), pattern = "\\.png$", full.names = TRUE))

r3 <- function(x) round(x, 3)
capas <- list()
for (v in VARS_PUB) for (p in names(PERIODOS)) {
  lst <- res[[v]][[p]]; if (!length(lst)) next
  mm <- lapply(c(anom = "anom", pb = "pb", pn = "pn", pa = "pa"), function(k) promedio(lst, k))
  r <- a_rast(mm)
  rr <- mask(resample(r, plantilla, method = "bilinear"), chile)
  rm <- project(rr, "EPSG:3857", method = "bilinear", res = 3000)
  b <- CFG[[v]]$brk; nb <- length(b)
  i_anom <- classify(clamp(rm[["anom"]], b[1] + 1e-3, b[nb] - 1e-3, values = TRUE),
                     cbind(b[-nb], b[-1], seq_len(nb - 1)), include.lowest = TRUE)
  i_cat <- lapp(rm[[c("pb", "pn", "pa")]], idx_categoria)
  png_rgba(i_anom, CFG[[v]]$pal, file.path(RAIZ, "img", sprintf("%s_%s_anom.png", v, p)))
  png_rgba(i_cat, CFG[[v]]$pal_cat, file.path(RAIZ, "img", sprintf("%s_%s_cat.png", v, p)))
  esq <- crds(project(vect(rbind(c(xmin(rm), ymin(rm)), c(xmax(rm), ymax(rm))), crs = "EPSG:3857"),
                      "EPSG:4326"))
  capas[[paste(v, p, sep = "_")]] <- c(
    list(var = v, periodo = p,
         img_anom = sprintf("img/%s_%s_anom.png", v, p),
         img_cat  = sprintf("img/%s_%s_cat.png", v, p),
         bounds = list(c(esq[1, 2], esq[1, 1]), c(esq[2, 2], esq[2, 1]))),
    lapply(mm, function(x) r3(a_filas(x))))
  message("  img ", v, " ", p)
}

DATA <- list(
  init = paste(tolower(MESES[INIT_MONTH]), INIT_YEAR),
  init_tag = INIT_TAG,
  modo = "temperaturas: climatología 1993–2016 ajustada por tendencia; precipitación: climatología 1993–2016",
  autor = AUTOR,
  fecha = format(Sys.time(), "%d-%m-%Y", tz = "America/Santiago"),
  limites = LIMITES,
  sistemas = usados[VARS_PUB],
  vars = setNames(lapply(VARS_PUB, function(v) VARS[[v]]$nombre), VARS_PUB),
  vcfg = CFG[VARS_PUB],
  periodos = ETIQ,
  grid = grid_info,
  ciudades = fromJSON(file.path(RAIZ, "config", "ciudades.json")),
  capas = capas,
  pal_anom = CFG$tmax$pal, brk_anom = CFG$tmax$brk, pal_cat = CFG$tmax$pal_cat,
  clases = CLASES
)
js <- paste0("window.DATA = ", toJSON(DATA, auto_unbox = TRUE, na = "null", digits = NA), ";")
writeLines(enc2utf8(js), f_data, useBytes = TRUE)
salida_gh("actualizado", "true")
message("Listo: data.js + ", length(list.files(file.path(RAIZ, "img"), pattern = "png$")),
        " imágenes · inicialización ", INIT_TAG)
