# Maps raw Catapult period_name values to canonical activity categories.
#
# Uses a keyword-based approach: checks each keyword list against the period
# name (case-insensitive). Order matters -- first match wins, so more
# specific patterns come before general ones.
#
# To add new mappings, add entries to EXACT_MAP or KEYWORD_MAP below.
# Run `Rscript R/categories.R --unmapped data/stats_df.xlsx` to find
# unmapped names.

EXACT_MAP <- c(
  "movilidad" = "movilidad",
  "activación" = "activacion",
  "activacion" = "activacion",
  "calentamiento" = "activacion",
  "rondo" = "rondo",
  "rondos" = "rondo",
  "tactico" = "tactico",
  "táctico" = "tactico",
  "torito" = "torito",
  "velocidad" = "velocidad",
  "duelos" = "duelos",
  "posesión" = "posesion",
  "posesion" = "posesion",
  "preventivos" = "preventivos",
  "futbol" = "futbol_tactico",
  "fútbol" = "futbol_tactico",
  "recreativo" = "recreativo",
  "reducido" = "reducido",
  "reducidos" = "reducido",
  "transferencia" = "transferencia",
  "definición" = "definicion",
  "definicion" = "definicion",
  "compensatorio" = "compensatorio",
  "complementario" = "complementario",
  "regenerativo" = "regenerativo",
  "trote" = "regenerativo",
  "rtp" = "rtp",
  "presión" = "presion",
  "presion" = "presion",
  "fartlek" = "fartlek",
  "hexágono" = "coordinacion",
  "enfrentamientos" = "duelos",
  "específico" = "especifico",
  "especificos" = "especifico",
  "específicos" = "especifico",
  "ec" = "especifico",
  "seleccionados" = "seleccionados",
  "principal" = "tactico",
  "trote thiago" = "regenerativo"
)

# Keyword-based matches -- first match wins.
# Each element: list(keywords = c(...), category = "...")
KEYWORD_MAP <- list(
  # Warmup / activation
  list(keywords = c("activación y movilidad", "activacion y movilidad"), category = "activacion"),
  list(keywords = c("activación y velocidad", "activacion y velocidad"), category = "activacion"),
  list(keywords = c("activación y fuerza", "activacion y fuerza"), category = "activacion"),
  list(keywords = c("activación y aceleración", "activacion y aceleracion"), category = "activacion"),
  list(keywords = c("activación + pase", "activacion + pase"), category = "activacion"),
  list(keywords = c("activación + tarea", "activacion + tarea"), category = "activacion"),
  list(keywords = c("calentamiento"), category = "activacion"),
  list(keywords = c("movilidad"), category = "movilidad"),
  list(keywords = c("vuelta a la calma"), category = "vuelta_calma"),

  # Rondos
  list(keywords = c("rondo + velocidad"), category = "rondo_velocidad"),
  list(keywords = c("rondo presión", "rondo presion", "rondos presión", "rondos presion"), category = "rondo_presion"),
  list(keywords = c("rondo posesión", "rondo posesion", "rondos posesión", "rondos posesion"), category = "rondo"),
  list(keywords = c("rondos de pases", "rondos + enfrent"), category = "rondo"),
  list(keywords = c("rondo de recuper"), category = "rondo"),
  list(keywords = c("rondo"), category = "rondo"),

  # Tactical
  list(keywords = c("futbol tactico", "futbol táctico", "fútbol táctico"), category = "futbol_tactico"),
  list(keywords = c("táctico y def", "tactico y def"), category = "tactico"),
  list(keywords = c("táctico", "tactico", "trabajo tactico"), category = "tactico"),

  # Small-sided games / reducidos
  list(keywords = c("7 vs 7", "7vs7"), category = "reducido"),
  list(keywords = c("6v6", "6vs6"), category = "reducido"),
  list(keywords = c("5vs5", "5v5"), category = "reducido"),
  list(keywords = c("3v3", "3vs2", "3vs3"), category = "reducido"),
  list(keywords = c("reducido"), category = "reducido"),
  list(keywords = c("doble area", "doble área"), category = "reducido"),

  # Speed
  list(keywords = c("velocidad de reacción", "velocidad de reaccion"), category = "velocidad"),
  list(keywords = c("velocidad y definición", "velocidad y definicion"), category = "velocidad"),
  list(keywords = c("velocidad 3vs2"), category = "velocidad"),
  list(keywords = c("velocidad"), category = "velocidad"),
  list(keywords = c("aceleraciones"), category = "velocidad"),
  list(keywords = c("100 m", "150 m", "200 metro", "400 metro", "70 m", "1000 metro"), category = "velocidad"),
  list(keywords = c("pasadas"), category = "velocidad"),

  # Passing / dynamic
  list(keywords = c("pase dinámico", "pase dinamico", "pases dinámicos", "pases dinamicos"), category = "pases_dinamicos"),

  # Finishing
  list(keywords = c("definición", "definicion", "centro y remate", "gol salen"), category = "definicion"),

  # Possession
  list(keywords = c("posesión", "posesion", "cuenta toques"), category = "posesion"),
  list(keywords = c("3 equipos"), category = "posesion"),

  # Physical / fitness
  list(keywords = c("fuerza explosiva"), category = "fuerza"),
  list(keywords = c("estaciones fuerza"), category = "fuerza"),
  list(keywords = c("circuito"), category = "circuito"),
  list(keywords = c("coordinación", "coordinacion", "acc + coor"), category = "coordinacion"),
  list(keywords = c("tareas de reacción", "tareas de reaccion"), category = "coordinacion"),
  list(keywords = c("fartlek"), category = "fartlek"),
  list(keywords = c("tennis balon", "fut tennis"), category = "recreativo"),

  # Position-specific
  list(keywords = c("especifico def", "específico def"), category = "especifico"),
  list(keywords = c("especifico ofen", "específico ofen"), category = "especifico"),
  list(keywords = c("especifico por posicion"), category = "especifico"),
  list(keywords = c("específico", "especifico", "especificos"), category = "especifico"),

  # Recovery
  list(keywords = c("compensatorio"), category = "compensatorio"),
  list(keywords = c("complemento", "complementario"), category = "complementario"),
  list(keywords = c("regenerativo", "recuperación"), category = "regenerativo"),
  list(keywords = c("preventivos"), category = "preventivos"),

  # Presión continua
  list(keywords = c("presión continua", "presion continua"), category = "presion"),

  # RTP (Return to Play) -- individual work
  list(keywords = c("rtp", "perea + edwin"), category = "rtp"),

  # Match periods (within training matches / amistosos)
  list(keywords = c("primer tiempo", "1er tiempo"), category = "partido_entrenamiento"),
  list(keywords = c("segundo tiempo", "2do tiempo"), category = "partido_entrenamiento"),
  list(keywords = c("tercer tiempo", "cuarto tiempo", "cuarto 3"), category = "partido_entrenamiento"),

  # Bloques (generic training blocks)
  list(keywords = c("bloque"), category = "bloque"),

  # Groups / individual
  list(keywords = c("grupo especial", "grupo 1", "grupo 2", "grupo 3"), category = "grupo"),
  list(keywords = c("trabajo"), category = "trabajo_especial"),
  list(keywords = c("seleccionados"), category = "seleccionados")
)

# Display names for the frontend
DISPLAY_NAMES <- c(
  movilidad = "Movilidad",
  activacion = "Activación / Calentamiento",
  rondo = "Rondo",
  rondo_presion = "Rondo Presión",
  rondo_velocidad = "Rondo + Velocidad",
  tactico = "Táctico",
  futbol_tactico = "Fútbol Táctico",
  torito = "Torito",
  velocidad = "Velocidad / Sprints",
  duelos = "Duelos / Enfrentamientos",
  posesion = "Posesión",
  reducido = "Reducido / SSG",
  pases_dinamicos = "Pases Dinámicos",
  definicion = "Definición / Remates",
  presion = "Presión Continua",
  circuito = "Circuito Físico",
  fuerza = "Fuerza Explosiva",
  coordinacion = "Coordinación / Agilidad",
  fartlek = "Fartlek",
  especifico = "Específico por Posición",
  preventivos = "Preventivos",
  compensatorio = "Compensatorio",
  complementario = "Complementario",
  regenerativo = "Regenerativo / Recuperación",
  recreativo = "Recreativo",
  rtp = "RTP (Retorno)",
  transferencia = "Transferencia",
  partido_entrenamiento = "Partido de Entrenamiento",
  bloque = "Bloque de Entrenamiento",
  grupo = "Grupo Especial",
  trabajo_especial = "Trabajo Especial",
  seleccionados = "Seleccionados",
  vuelta_calma = "Vuelta a la Calma"
)

#' Map a single raw period_name to a canonical category.
#' Returns NA_character_ if no match found.
categorize_period <- function(period_name) {
  if (is.na(period_name) || !is.character(period_name) || nchar(trimws(period_name)) == 0) {
    return(NA_character_)
  }

  clean <- tolower(trimws(period_name))

  # Try exact match first
  if (clean %in% names(EXACT_MAP)) {
    return(unname(EXACT_MAP[clean]))
  }

  # Try keyword match, first match wins
  for (entry in KEYWORD_MAP) {
    for (kw in entry$keywords) {
      if (grepl(kw, clean, fixed = TRUE)) {
        return(entry$category)
      }
    }
  }

  NA_character_
}

#' Vectorized version of categorize_period.
categorize_periods <- function(period_names) {
  vapply(period_names, categorize_period, character(1), USE.NAMES = FALSE)
}

get_display_name <- function(category) {
  if (category %in% names(DISPLAY_NAMES)) {
    return(unname(DISPLAY_NAMES[category]))
  }
  # Title-case fallback, replacing underscores with spaces
  words <- strsplit(gsub("_", " ", category), " ")[[1]]
  paste(toupper(substring(words, 1, 1)), substring(words, 2), sep = "", collapse = " ")
}

# ── CLI: find unmapped period names ─────────────────────────────────────────
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) >= 2 && args[1] == "--unmapped") {
    suppressPackageStartupMessages(library(readxl))
    df <- read_excel(args[2])
    mask <- !startsWith(as.character(df$activity_name), "Partido")
    mask[is.na(mask)] <- FALSE
    periods <- sort(unique(na.omit(df$period_name[mask])))

    mapped <- list()
    unmapped <- c()
    for (p in periods) {
      cat_ <- categorize_period(p)
      if (!is.na(cat_)) {
        mapped[[cat_]] <- c(mapped[[cat_]], p)
      } else {
        unmapped <- c(unmapped, p)
      }
    }

    cat("=== MAPPED ===\n")
    for (cat_ in sort(names(mapped))) {
      cat(sprintf("\n  %s:\n", cat_))
      for (n in mapped[[cat_]]) cat(sprintf("    - %s\n", n))
    }

    if (length(unmapped) > 0) {
      cat(sprintf("\n=== UNMAPPED (%d) ===\n", length(unmapped)))
      for (n in unmapped) {
        count <- sum(df$period_name == n, na.rm = TRUE)
        cat(sprintf("  %4dx  %s\n", count, n))
      }
    } else {
      cat("\n\u2705 All period names are mapped!\n")
    }
  } else {
    cat("Usage: Rscript R/categories.R --unmapped data/stats_df.xlsx\n")
  }
}
