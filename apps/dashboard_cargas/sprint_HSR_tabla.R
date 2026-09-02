library(tidyverse)
library(dplyr)
library(lubridate)
library(readr)
library(writexl)
library(gt)

micros_shiny_comb <- read_csv("micros/micros_shiny_comb.csv")
micros_shiny_comb$date <- as.Date(micros_shiny_comb$date)

selected_players <- c(
  "Israel Reyes","Henry Martín","Alejandro Zendejas",
  "Isaías Violante","Alan Cervantes","Ramón Juárez","Erick Sánchez",
  "Brian Rodríguez","Kevin Álvarez","Dagoberto Espinoza","Víctor Dávila",
  "Cristian Borja","Alexis Gutiérrez","Néstor Araujo",
  "Sebastián Cáceres","Miguel Vázquez","Ralph Orquín",
  "Jonathan Dos Santos","Santiago Naveda","José Raúl Zúñiga",
  "Patricio Salas", "Rodrigo Dourado", "Aaron Mejia", "Raphael Veiga",
  "Vinícius Lima", "Thiago Espinosa"
)

# TABLA DISTANCIA HSR Y SPRINT FEB Y MAR 2026 ----

hsr_dist_tabla <- micros_shiny_comb |>
  filter(player %in% selected_players) |>
  filter(month(date) %in% c(2, 3), match_day != "MD") |>
  group_by(player, date) |>
  summarize(
    match_day    = first(match_day),
    HSR_abs_dist = sum(HSR_abs_dist, na.rm = TRUE),
    distance_abs = sum(distance_abs, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(player, date) |>
  rename(
    Jugador                = player,
    Fecha                  = date,
    `Sesión`               = match_day,
    `Distancia HSR (m)`    = HSR_abs_dist,
    `Distancia Sprint (m)` = distance_abs
  )

write_xlsx(hsr_dist_tabla, path = "/Users/mateorodriguez/Desktop/hsr_dist_feb_mar_2026.xlsx")


# --- Jugadores que superaron 800 m en HSR_abs_dist ----

hsr_800 <- micros_shiny_comb |>
  filter(player %in% selected_players, HSR_abs_dist > 800, match_day != "MD") |>
  select(player, date, match_day, HSR_abs_dist) |>
  arrange(date)

tabla_hsr_800 <- hsr_800 |>
  gt() |>
  cols_label(
    player       = "Jugador",
    date         = "Fecha",
    match_day    = "Tipo de Sesión",
    HSR_abs_dist = "HSR (m)"
  ) |>
  fmt_number(columns = HSR_abs_dist, decimals = 1) |>
  fmt_date(columns = date, date_style = "yMd") |>
  tab_header(title = "Sesiones con HSR > 800 m")


# TABLA RESUMEN HENRY MARTÍN vs PLANTILLA (ap24_cl26) ----

library(readxl)
library(gt)

ap24_cl26 <- read_excel("/Users/mateorodriguez/Desktop/ap24_cl26.xlsx")

# Aggregate drills to session level
ap24_sessions <- ap24_cl26 |>
  group_by(player, date, match_day) |>
  summarize(
    distance_m       = sum(distance_m,       na.rm = TRUE),
    HSR_abs_dist     = sum(HSR_abs_dist,     na.rm = TRUE),
    distance_abs     = sum(distance_abs,     na.rm = TRUE),
    max_speed        = max(max_speed,        na.rm = TRUE),
    .groups = "drop"
  )

henry_train <- ap24_sessions |>
  filter(player == "Henry Martín", match_day != "MD") |>
  summarize(across(c(distance_m, HSR_abs_dist, distance_abs), \(x) mean(x, na.rm = TRUE)),
            p95_speed = quantile(max_speed, 0.95, na.rm = TRUE)) |>
  mutate(categoria = "Henry Martín — Entrenamiento")

henry_match <- ap24_sessions |>
  filter(player == "Henry Martín", match_day == "MD") |>
  summarize(across(c(distance_m, HSR_abs_dist, distance_abs), \(x) mean(x, na.rm = TRUE)),
            p95_speed = quantile(max_speed, 0.95, na.rm = TRUE)) |>
  mutate(categoria = "Henry Martín — Partido")

all_train <- ap24_sessions |>
  filter(match_day != "MD") |>
  summarize(across(c(distance_m, HSR_abs_dist, distance_abs), \(x) mean(x, na.rm = TRUE)),
            p95_speed = quantile(max_speed, 0.95, na.rm = TRUE)) |>
  mutate(categoria = "Plantilla — Entrenamiento")

all_match <- ap24_sessions |>
  filter(match_day == "MD") |>
  summarize(across(c(distance_m, HSR_abs_dist, distance_abs), \(x) mean(x, na.rm = TRUE)),
            p95_speed = quantile(max_speed, 0.95, na.rm = TRUE)) |>
  mutate(categoria = "Plantilla — Partido")

maximos <- ap24_sessions |>
  summarize(across(c(distance_m, HSR_abs_dist, distance_abs), \(x) max(x, na.rm = TRUE)),
            p95_speed = quantile(max_speed, 0.95, na.rm = TRUE)) |>
  mutate(categoria = "Máximo registrado")

henry_max <- ap24_sessions |>
  filter(player == "Henry Martín") |>
  summarize(across(c(distance_m, HSR_abs_dist, distance_abs), \(x) max(x, na.rm = TRUE)),
            p95_speed = quantile(max_speed, 0.95, na.rm = TRUE)) |>
  mutate(categoria = "Henry Martín — Máximo")

resumen_henry <- bind_rows(henry_train, henry_match, henry_max, all_train, all_match, maximos) |>
  select(categoria, distance_m, HSR_abs_dist, distance_abs, p95_speed)

tabla_resumen_henry <- resumen_henry |>
  gt() |>
  tab_header(
    title    = "Resumen de Cargas — Henry Martín vs. Plantilla",
    subtitle = "Todas las sesiones desde el inicio del Apertura 2024"
  ) |>
  cols_label(
    categoria    = "Categoría",
    distance_m   = "Distancia Total (m)",
    HSR_abs_dist = "HSR (m)",
    distance_abs = "Sprint (m)",
    p95_speed    = "Vel. P95 (km/h)"
  ) |>
  fmt_number(columns = c(distance_m, HSR_abs_dist, distance_abs), decimals = 1) |>
  fmt_number(columns = p95_speed, decimals = 2) |>
  tab_style(
    style     = list(cell_fill(color = "#dce8f5"), cell_text(weight = "bold")),
    locations = cells_body(rows = grepl("Henry Martín", categoria))
  ) |>
  tab_style(
    style     = cell_fill(color = "#f5f5f5"),
    locations = cells_body(rows = grepl("Máximo", categoria))
  ) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

tabla_resumen_henry

# Helper to build a styled gt table from a 5-row summary tibble
make_resumen_gt <- function(data, title, footnote_text, footnote_row_pattern) {
  data |>
    gt() |>
    tab_header(
      title    = title,
      subtitle = "Todas las sesiones desde el inicio del Apertura 2024"
    ) |>
    cols_label(
      categoria    = "Categoría",
      distance_m   = "Distancia Total (m)",
      HSR_abs_dist = "HSR (m)",
      distance_abs = "Sprint (m)",
      p95_speed    = "Vel. P95 (km/h)"
    ) |>
    fmt_number(columns = c(distance_m, HSR_abs_dist, distance_abs), decimals = 1) |>
    fmt_number(columns = p95_speed, decimals = 2) |>
    tab_style(
      style     = list(cell_fill(color = "#dce8f5"), cell_text(weight = "bold")),
      locations = cells_body(rows = grepl("Henry Martín", categoria))
    ) |>
    tab_style(
      style     = cell_fill(color = "#f5f5f5"),
      locations = cells_body(rows = grepl("Máximo", categoria))
    ) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) |>
    tab_footnote(
      footnote  = footnote_text,
      locations = cells_body(rows = grepl(footnote_row_pattern, categoria), columns = categoria)
    )
}

# Shared aggregation for MD sessions at session level (used by both filtered tables)
md_sessions <- ap24_cl26 |>
  filter(match_day == "MD") |>
  group_by(player, date, match_day) |>
  summarize(
    distance_m     = sum(distance_m,     na.rm = TRUE),
    HSR_abs_dist   = sum(HSR_abs_dist,   na.rm = TRUE),
    distance_abs   = sum(distance_abs,   na.rm = TRUE),
    max_speed      = max(max_speed,      na.rm = TRUE),
    drill_duration = sum(drill_duration, na.rm = TRUE),
    .groups = "drop"
  )

# --- Tabla 2: Top 10 por partido (los 10 con mayor drill_duration en cada MD) ----

all_match_top10 <- md_sessions |>
  group_by(date) |>
  slice_max(drill_duration, n = 10, with_ties = FALSE) |>
  ungroup() |>
  summarize(across(c(distance_m, HSR_abs_dist, distance_abs), \(x) mean(x, na.rm = TRUE)),
            p95_speed = quantile(max_speed, 0.95, na.rm = TRUE)) |>
  mutate(categoria = "Plantilla (Top 10 min.) — Partido")

tabla_resumen_henry_top10 <- bind_rows(henry_train, henry_match, henry_max, all_train, all_match_top10, maximos) |>
  select(categoria, distance_m, HSR_abs_dist, distance_abs, p95_speed) |>
  make_resumen_gt(
    title                 = "Resumen de Cargas — Henry Martín vs. Plantilla (Top 10 por partido)",
    footnote_text         = "Promedio calculado sobre los 10 jugadores con mayor duración en cada partido individual.",
    footnote_row_pattern  = "Top 10"
  )

tabla_resumen_henry_top10

# --- Tabla 3: Jugadores con drill_duration >= 90 min en partido ----

all_match_90 <- md_sessions |>
  filter(drill_duration >= 90) |>
  summarize(across(c(distance_m, HSR_abs_dist, distance_abs), \(x) mean(x, na.rm = TRUE)),
            p95_speed = quantile(max_speed, 0.95, na.rm = TRUE)) |>
  mutate(categoria = "Plantilla (≥90 min.) — Partido")

tabla_resumen_henry_90 <- bind_rows(henry_train, henry_match, henry_max, all_train, all_match_90, maximos) |>
  select(categoria, distance_m, HSR_abs_dist, distance_abs, p95_speed) |>
  make_resumen_gt(
    title                = "Resumen de Cargas — Henry Martín vs. Plantilla (≥90 min. en partido)",
    footnote_text        = "Promedio calculado sobre jugadores con duración de partido ≥ 90 minutos.",
    footnote_row_pattern = "≥90"
  )

tabla_resumen_henry_90

# TABLA MÁXIMOS GLOBALES — JUGADOR Y FECHA ----

maximos_detalle <- bind_rows(
  ap24_sessions |> slice_max(distance_m,   n = 1, with_ties = FALSE) |> mutate(metrica = "Distancia Total (m)",  valor = distance_m),
  ap24_sessions |> slice_max(HSR_abs_dist, n = 1, with_ties = FALSE) |> mutate(metrica = "HSR (m)",              valor = HSR_abs_dist),
  ap24_sessions |> slice_max(distance_abs, n = 1, with_ties = FALSE) |> mutate(metrica = "Sprint (m)",           valor = distance_abs),
  ap24_sessions |> slice_max(max_speed,    n = 1, with_ties = FALSE) |> mutate(metrica = "Vel. Máxima (km/h)",   valor = max_speed)
) |>
  select(metrica, valor, player, date, match_day)

tabla_maximos_detalle <- maximos_detalle |>
  gt() |>
  tab_header(title = "Máximos Globales — Jugador y Fecha") |>
  cols_label(
    metrica   = "Métrica",
    valor     = "Valor",
    player    = "Jugador",
    date      = "Fecha",
    match_day = "Tipo de Sesión"
  ) |>
  fmt_number(columns = valor, decimals = 2) |>
  fmt_date(columns = date, date_style = "yMd") |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

tabla_maximos_detalle


# TABLAS P95 — HSR, SPRINT Y VELOCIDAD EN PERCENTIL 95 ----
# New versions of all three tables where HSR, sprint distance, and speed are p95;
# total distance stays as mean (average rows) or max (max rows).

p95 <- \(x) quantile(x, 0.95, na.rm = TRUE)

henry_train_p95 <- ap24_sessions |>
  filter(player == "Henry Martín", match_day != "MD") |>
  summarize(distance_m = mean(distance_m, na.rm = TRUE),
            across(c(HSR_abs_dist, distance_abs, max_speed), p95)) |>
  rename(p95_speed = max_speed) |>
  mutate(categoria = "Henry Martín — Entrenamiento")

henry_match_p95 <- ap24_sessions |>
  filter(player == "Henry Martín", match_day == "MD") |>
  summarize(distance_m = mean(distance_m, na.rm = TRUE),
            across(c(HSR_abs_dist, distance_abs, max_speed), p95)) |>
  rename(p95_speed = max_speed) |>
  mutate(categoria = "Henry Martín — Partido")

henry_max_p95 <- ap24_sessions |>
  filter(player == "Henry Martín") |>
  summarize(distance_m = max(distance_m, na.rm = TRUE),
            across(c(HSR_abs_dist, distance_abs, max_speed), p95)) |>
  rename(p95_speed = max_speed) |>
  mutate(categoria = "Henry Martín — Máximo")

all_train_p95 <- ap24_sessions |>
  filter(match_day != "MD") |>
  summarize(distance_m = mean(distance_m, na.rm = TRUE),
            across(c(HSR_abs_dist, distance_abs, max_speed), p95)) |>
  rename(p95_speed = max_speed) |>
  mutate(categoria = "Plantilla — Entrenamiento")

all_match_p95 <- ap24_sessions |>
  filter(match_day == "MD") |>
  summarize(distance_m = mean(distance_m, na.rm = TRUE),
            across(c(HSR_abs_dist, distance_abs, max_speed), p95)) |>
  rename(p95_speed = max_speed) |>
  mutate(categoria = "Plantilla — Partido")

maximos_p95 <- ap24_sessions |>
  summarize(distance_m = max(distance_m, na.rm = TRUE),
            across(c(HSR_abs_dist, distance_abs, max_speed), p95)) |>
  rename(p95_speed = max_speed) |>
  mutate(categoria = "Máximo registrado")

make_p95_gt <- function(data, title, footnote_text, footnote_row_pattern) {
  data |>
    gt() |>
    tab_header(
      title    = title,
      subtitle = "Todas las sesiones desde el inicio del Apertura 2024"
    ) |>
    cols_label(
      categoria    = "Categoría",
      distance_m   = "Distancia Total (m)",
      HSR_abs_dist = "HSR P95 (m)",
      distance_abs = "Sprint P95 (m)",
      p95_speed    = "Vel. P95 (km/h)"
    ) |>
    fmt_number(columns = c(distance_m, HSR_abs_dist, distance_abs), decimals = 1) |>
    fmt_number(columns = p95_speed, decimals = 2) |>
    tab_style(
      style     = list(cell_fill(color = "#dce8f5"), cell_text(weight = "bold")),
      locations = cells_body(rows = grepl("Henry Martín", categoria))
    ) |>
    tab_style(
      style     = cell_fill(color = "#f5f5f5"),
      locations = cells_body(rows = grepl("Máximo", categoria))
    ) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) |>
    tab_footnote(
      footnote  = footnote_text,
      locations = cells_body(rows = grepl(footnote_row_pattern, categoria), columns = categoria)
    )
}

# # --- Tabla P95-1: Todos los partidos ----
# 
# tabla_p95_henry <- bind_rows(henry_train_p95, henry_match_p95, henry_max_p95,
#                               all_train_p95, all_match_p95, maximos_p95) |>
#   select(categoria, distance_m, HSR_abs_dist, distance_abs, p95_speed) |>
#   make_p95_gt(
#     title                = "Resumen P95 — Henry Martín vs. Plantilla",
#     footnote_text        = "HSR, sprint y velocidad corresponden al percentil 95; distancia total es el promedio.",
#     footnote_row_pattern = "Plantilla — Partido"
#   )
# 
# tabla_p95_henry
# 
# # --- Tabla P95-2: Top 10 por partido ----
# 
# all_match_top10_p95 <- md_sessions |>
#   group_by(date) |>
#   slice_max(drill_duration, n = 10, with_ties = FALSE) |>
#   ungroup() |>
#   summarize(distance_m = mean(distance_m, na.rm = TRUE),
#             across(c(HSR_abs_dist, distance_abs, max_speed), p95)) |>
#   rename(p95_speed = max_speed) |>
#   mutate(categoria = "Plantilla (Top 10 min.) — Partido")
# 
# tabla_p95_henry_top10 <- bind_rows(henry_train_p95, henry_match_p95, henry_max_p95,
#                                     all_train_p95, all_match_top10_p95, maximos_p95) |>
#   select(categoria, distance_m, HSR_abs_dist, distance_abs, p95_speed) |>
#   make_p95_gt(
#     title                = "Resumen P95 — Henry Martín vs. Plantilla (Top 10 por partido)",
#     footnote_text        = "Promedio calculado sobre los 10 jugadores con mayor duración en cada partido individual.",
#     footnote_row_pattern = "Top 10"
#   )
# 
# tabla_p95_henry_top10

# --- Tabla P95-3: Jugadores con drill_duration >= 90 min ----

all_match_90_p95 <- md_sessions |>
  filter(drill_duration >= 90) |>
  summarize(distance_m = mean(distance_m, na.rm = TRUE),
            across(c(HSR_abs_dist, distance_abs, max_speed), p95)) |>
  rename(p95_speed = max_speed) |>
  mutate(categoria = "Plantilla (≥90 min.) — Partido")

tabla_p95_henry_90 <- bind_rows(henry_train_p95, henry_match_p95, henry_max_p95,
                                 all_train_p95, all_match_90_p95, maximos_p95) |>
  select(categoria, distance_m, HSR_abs_dist, distance_abs, p95_speed) |>
  make_p95_gt(
    title                = "Resumen P95 — Henry Martín vs. Plantilla (≥90 min. en partido)",
    footnote_text        = "Promedio calculado sobre jugadores con duración de partido ≥ 90 minutos.",
    footnote_row_pattern = "≥90"
  )

tabla_p95_henry_90
