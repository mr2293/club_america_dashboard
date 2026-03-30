#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# app.R
# Métricas de Carga Física — Acumulado Últimos 7 Días

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(scales)
library(readxl)
library(gt)

source("cargas7.R")

# ----------------------------
# Helpers
# ----------------------------
get_last7_window <- function(datos) {
  end_date <- max(datos$date, na.rm = TRUE)
  start_date <- end_date - days(6)
  list(start_date = start_date, end_date = end_date)
}

base_theme <- function() {
  theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(face = "bold", size = 22, margin = margin(b = 12)),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14, face = "bold"),
      axis.line = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
      plot.margin = margin(15, 30, 15, 20),
      legend.position = "top",
      legend.text = element_text(size = 16, face = "bold")
    )
}

# ----------------------------
# Plot builders
# ----------------------------

plot_hsr_7d_with_4w_avg <- function(datos) {
  
  end_date  <- max(datos$date, na.rm = TRUE)
  start_28d <- end_date - lubridate::days(27)
  
  df_28 <- datos |>
    dplyr::filter(date >= start_28d, date <= end_date) |>
    dplyr::mutate(
      day_diff = as.integer(end_date - date),
      week_28d = dplyr::case_when(
        day_diff >= 0  & day_diff <= 6  ~ 1L,
        day_diff >= 7  & day_diff <= 13 ~ 2L,
        day_diff >= 14 & day_diff <= 20 ~ 3L,
        day_diff >= 21 & day_diff <= 27 ~ 4L,
        TRUE ~ NA_integer_
      )
    ) |>
    dplyr::filter(!is.na(week_28d))
  
  hsr_labels <- df_28 |>
    dplyr::group_by(player, week_28d) |>
    dplyr::summarise(hsr_week_total = sum(HSR_abs_dist, na.rm = TRUE), .groups = "drop") |>
    tidyr::complete(player, week_28d = 1:4, fill = list(hsr_week_total = 0)) |>
    dplyr::group_by(player) |>
    dplyr::summarise(hsr_4w_weekly_avg = mean(hsr_week_total), .groups = "drop")
  
  w <- get_last7_window(datos)
  df_7d <- datos |>
    dplyr::filter(date >= w$start_date, date <= w$end_date) |>
    dplyr::group_by(player) |>
    dplyr::summarise(
      HSR_abs_dist = sum(HSR_abs_dist, na.rm = TRUE),
      distance_abs = sum(distance_abs, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(total_dist = HSR_abs_dist + distance_abs)
  
  player_levels <- df_7d |>
    dplyr::arrange(dplyr::desc(HSR_abs_dist)) |>
    dplyr::pull(player)
  
  df_plot <- df_7d |>
    dplyr::select(player, value = HSR_abs_dist) |>
    dplyr::left_join(hsr_labels, by = "player") |>
    dplyr::mutate(player = factor(player, levels = rev(player_levels)))
  
  ggplot2::ggplot(df_plot, ggplot2::aes(x = player, y = value)) +
    ggplot2::geom_col(fill = "#0B1B4A", color = "white", linewidth = 0.2) +
    
    # yellow dot = 4w weekly avg
    ggplot2::geom_point(
      ggplot2::aes(y = hsr_4w_weekly_avg),
      color = "#FFD60A",
      size = 3.5
    ) +
    
    ggplot2::geom_text(
      ggplot2::aes(label = scales::comma(round(value, 0))),
      color = "white", size = 4, fontface = "bold", hjust = 1.05
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
    ggplot2::labs(
      title = "Distancia en HSR (Últimos 7 Días)",
      subtitle = "Punto amarillo: Promedio semanal (promedio de 4 semanas, últimos 28 días)",
      x = NULL, y = "Distancia (m)"
    ) +
    base_theme()
}

plot_sprint_7d_with_4w_avg <- function(datos) {
  
  end_date  <- max(datos$date, na.rm = TRUE)
  start_28d <- end_date - lubridate::days(27)
  
  df_28 <- datos |>
    dplyr::filter(date >= start_28d, date <= end_date) |>
    dplyr::mutate(
      day_diff = as.integer(end_date - date),
      week_28d = dplyr::case_when(
        day_diff >= 0  & day_diff <= 6  ~ 1L,
        day_diff >= 7  & day_diff <= 13 ~ 2L,
        day_diff >= 14 & day_diff <= 20 ~ 3L,
        day_diff >= 21 & day_diff <= 27 ~ 4L,
        TRUE ~ NA_integer_
      )
    ) |>
    dplyr::filter(!is.na(week_28d))
  
  sprint_labels <- df_28 |>
    dplyr::group_by(player, week_28d) |>
    dplyr::summarise(sprint_week_total = sum(distance_abs, na.rm = TRUE), .groups = "drop") |>
    tidyr::complete(player, week_28d = 1:4, fill = list(sprint_week_total = 0)) |>
    dplyr::group_by(player) |>
    dplyr::summarise(sprint_4w_weekly_avg = mean(sprint_week_total), .groups = "drop")
  
  w <- get_last7_window(datos)
  df_7d <- datos |>
    dplyr::filter(date >= w$start_date, date <= w$end_date) |>
    dplyr::group_by(player) |>
    dplyr::summarise(
      HSR_abs_dist = sum(HSR_abs_dist, na.rm = TRUE),
      distance_abs = sum(distance_abs, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(total_dist = HSR_abs_dist + distance_abs)
  
  player_levels <- df_7d |>
    dplyr::arrange(dplyr::desc(distance_abs)) |>
    dplyr::pull(player)
  
  df_plot <- df_7d |>
    dplyr::select(player, value = distance_abs) |>
    dplyr::left_join(sprint_labels, by = "player") |>
    dplyr::mutate(player = factor(player, levels = rev(player_levels)))
  
  ggplot2::ggplot(df_plot, ggplot2::aes(x = player, y = value)) +
    ggplot2::geom_col(fill = "#C1121F", color = "white", linewidth = 0.2) +
    
    # yellow dot = 4w weekly avg
    ggplot2::geom_point(
      ggplot2::aes(y = sprint_4w_weekly_avg),
      color = "#FFD60A",
      size = 3.5
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::comma(round(value, 0))),
      color = "white", size = 4, fontface = "bold", hjust = 1.05
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
    ggplot2::labs(
      title = "Distancia en Sprint (Últimos 7 Días)",
      subtitle = "Punto amarillo: Promedio semanal (promedio de 4 semanas, últimos 28 días)",
      x = NULL, y = "Distancia (m)"
    ) +
    base_theme()
}

plot_distance_total <- function(datos) {
  w <- get_last7_window(datos)
  df_dist7 <- datos |>
    filter(date >= w$start_date, date <= w$end_date) |>
    group_by(player) |>
    summarise(distance_m_7d = sum(distance_m, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(distance_m_7d)) |>
    mutate(player = factor(player, levels = rev(player)))
  
  team_avg <- mean(df_dist7$distance_m_7d, na.rm = TRUE)
  x_label <- levels(df_dist7$player)[length(levels(df_dist7$player))]
  
  ggplot(df_dist7, aes(x = player, y = distance_m_7d)) +
    geom_col(fill = "#0B1B4A", color = "white", linewidth = 0.2) +
    geom_hline(yintercept = team_avg, color = "#C1121F", linetype = "dashed", linewidth = 1.2) +
    geom_text(
      aes(label = paste0(round(distance_m_7d / 1000), "k")),
      color = "white",
      size = 4,
      fontface = "bold",
      hjust = 1.05
    ) +
    annotate(
      "text",
      y = team_avg,
      x = x_label,
      label = paste0("Promedio equipo: ", round(team_avg / 1000, 1), "k"),
      color = "#C1121F",
      fontface = "bold",
      size = 5,
      hjust = -0.13,
      vjust = 55
    ) +
    coord_flip() +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Distancia Total (Últimos 7 Días)",
      x = NULL,
      y = "Distancia (m)"
    ) +
    base_theme()
}

plot_acc_decc <- function(datos) {
  w <- get_last7_window(datos)
  df_acc7 <- datos |>
    filter(date >= w$start_date, date <= w$end_date) |>
    group_by(player) |>
    summarise(acc_plus_decc_7d = sum(acc_plus_decc, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(acc_plus_decc_7d)) |>
    mutate(player = factor(player, levels = rev(player)))
  
  team_avg <- mean(df_acc7$acc_plus_decc_7d, na.rm = TRUE)
  x_label <- levels(df_acc7$player)[length(levels(df_acc7$player))]
  
  ggplot(df_acc7, aes(x = player, y = acc_plus_decc_7d)) +
    geom_col(fill = "#0B1B4A", color = "white", linewidth = 0.2) +
    geom_hline(yintercept = team_avg, color = "#C1121F", linetype = "dashed", linewidth = 1.2) +
    geom_text(
      aes(label = scales::comma(round(acc_plus_decc_7d, 0))),
      color = "white",
      size = 4,
      fontface = "bold",
      hjust = 1.05
    ) +
    annotate(
      "text",
      y = team_avg,
      x = x_label,
      label = paste0("Promedio equipo: ", scales::comma(round(team_avg, 0))),
      color = "#C1121F",
      fontface = "bold",
      size = 5,
      hjust = -0.1,
      vjust = 45
    ) +
    coord_flip() +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "ACC + DECC (Últimos 7 Días)",
      x = NULL,
      y = "ACC + DECC (acumulado)"
    ) +
    base_theme()
}

plot_player_load <- function(datos) {
  w <- get_last7_window(datos)
  df_pl7 <- datos |>
    filter(date >= w$start_date, date <= w$end_date) |>
    group_by(player) |>
    summarise(player_load_7d = sum(player_load, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(player_load_7d)) |>
    mutate(player = factor(player, levels = rev(player)))
  
  team_avg <- mean(df_pl7$player_load_7d, na.rm = TRUE)
  x_label <- levels(df_pl7$player)[length(levels(df_pl7$player))]
  
  ggplot(df_pl7, aes(x = player, y = player_load_7d)) +
    geom_col(fill = "#0B1B4A", color = "white", linewidth = 0.2) +
    geom_hline(yintercept = team_avg, color = "#C1121F", linetype = "dashed", linewidth = 1.2) +
    geom_text(
      aes(label = scales::comma(round(player_load_7d, 0))),
      color = "white",
      size = 4,
      fontface = "bold",
      hjust = 1.05
    ) +
    annotate(
      "text",
      y = team_avg,
      x = x_label,
      label = paste0("Promedio equipo: ", scales::comma(round(team_avg, 0))),
      color = "#C1121F",
      fontface = "bold",
      size = 5,
      hjust = -1,
      vjust = 45
    ) +
    coord_flip() +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Player Load (Últimos 7 Días)",
      x = NULL,
      y = "Player Load"
    ) +
    base_theme()
}

plot_pct_hist_speed <- function(datos) {
  w <- get_last7_window(datos)
  
  df_speed7 <- datos |>
    filter(date >= w$start_date, date <= w$end_date) |>
    group_by(player) |>
    summarise(
      avg_speed_7d  = mean(max_speed, na.rm = TRUE),
      max_speed_7d  = max(max_speed, na.rm = TRUE),
      vel_max_hist  = max(vel_max_hist, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      pct_hist_7d = if_else(
        is.finite(vel_max_hist) & vel_max_hist > 0,
        100 * (max_speed_7d / vel_max_hist),
        NA_real_
      ),
      avg_speed_pct_hist = if_else(
        is.finite(vel_max_hist) & vel_max_hist > 0,
        100 * (avg_speed_7d / vel_max_hist),
        NA_real_
      )
    ) |>
    arrange(desc(pct_hist_7d)) |>
    mutate(player = factor(player, levels = rev(player)))
  
  team_avg_pct <- mean(df_speed7$pct_hist_7d, na.rm = TRUE)
  x_label <- levels(df_speed7$player)[length(levels(df_speed7$player))]
  
  ggplot(df_speed7, aes(x = player, y = pct_hist_7d)) +
    geom_col(fill = "#0B1B4A", color = "white", linewidth = 0.2) +
    geom_hline(yintercept = team_avg_pct, color = "#C1121F", linetype = "dashed", linewidth = 1.2) +
    
    # % label inside bar
    geom_text(
      aes(label = paste0(round(pct_hist_7d, 0), "%")),
      color = "white",
      size = 4,
      fontface = "bold",
      hjust = 1.05
    ) +
    
    # Average speed label INSIDE bar, next to left edge
    geom_text(
      aes(
        y = 1,
        label = paste0(
          round(avg_speed_7d, 1), " km/h (",
          round(avg_speed_pct_hist, 0), "%)"
        )
      ),
      color = "white",
      size = 4,
      fontface = "bold",
      hjust = 0
    ) +
    annotate(
      "text",
      y = team_avg_pct,
      x = x_label,
      label = paste0("Promedio equipo: ", round(team_avg_pct, 1), "%"),
      color = "#C1121F",
      fontface = "bold",
      size = 5,
      hjust = 0.41,
      vjust = 54.3
    ) +
    coord_flip() +
    scale_y_continuous(
      labels = ~ paste0(scales::comma(.x), "%"),
      limits = c(0, NA)
    ) +
    labs(
      title = "% de Velocidad Máxima Histórica (Últimos 7 Días)",
      subtitle = "Etiqueta izquierda: Promedio de velocidad máxima de los últimos 7 días \n(% de promedio en comparación a su velocidad máxima histórica.)",
      x = NULL,
      y = "% de Velocidad Máxima Histórica"
    ) +
    base_theme() +
    theme(
      plot.margin = margin(t = 10, r = 10, b = 10, l = 30)
    )
}

# ----------------------------
# NEW: Sprints Absolutos (>95%)
# ----------------------------
plot_sprints_abs_count <- function(datos) {
  
  end_date  <- max(datos$date, na.rm = TRUE)
  start_28d <- end_date - lubridate::days(27)
  
  # 4-week weekly average (same rolling window logic as HSR/Sprint plots)
  df_28 <- datos |>
    dplyr::filter(date >= start_28d, date <= end_date) |>
    dplyr::mutate(
      day_diff = as.integer(end_date - date),
      week_28d = dplyr::case_when(
        day_diff >= 0  & day_diff <= 6  ~ 1L,
        day_diff >= 7  & day_diff <= 13 ~ 2L,
        day_diff >= 14 & day_diff <= 20 ~ 3L,
        day_diff >= 21 & day_diff <= 27 ~ 4L,
        TRUE ~ NA_integer_
      )
    ) |>
    dplyr::filter(!is.na(week_28d))
  
  avg_labels <- df_28 |>
    dplyr::group_by(player, week_28d) |>
    dplyr::summarise(week_total = sum(sprints_abs_count, na.rm = TRUE), .groups = "drop") |>
    tidyr::complete(player, week_28d = 1:4, fill = list(week_total = 0)) |>
    dplyr::group_by(player) |>
    dplyr::summarise(avg_4w = mean(week_total), .groups = "drop")
  
  # Last 7 days sum
  w <- get_last7_window(datos)
  df_7d <- datos |>
    dplyr::filter(date >= w$start_date, date <= w$end_date) |>
    dplyr::group_by(player) |>
    dplyr::summarise(value = sum(sprints_abs_count, na.rm = TRUE), .groups = "drop")
  
  player_levels <- df_7d |>
    dplyr::arrange(dplyr::desc(value)) |>
    dplyr::pull(player)
  
  df_plot <- df_7d |>
    dplyr::left_join(avg_labels, by = "player") |>
    dplyr::mutate(player = factor(player, levels = rev(player_levels)))
  
  ggplot2::ggplot(df_plot, ggplot2::aes(x = player, y = value)) +
    ggplot2::geom_col(fill = "#0B1B4A", color = "white", linewidth = 0.2) +
    ggplot2::geom_point(
      ggplot2::aes(y = avg_4w),
      color = "#FFD60A",
      size = 3.5
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::comma(round(value, 0))),
      color = "white", size = 4, fontface = "bold", hjust = 1.05
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
    ggplot2::labs(
      title = "Número de Sprints Absolutos >95% (Últimos 7 Días)",
      subtitle = "Punto amarillo: Promedio semanal (promedio de 4 semanas, últimos 28 días)",
      x = NULL,
      y = "Número de Sprints"
    ) +
    base_theme()
}

# ----------------------------
# NEW: Sprints Relativos (>85%)
# ----------------------------
plot_sprints_rel_count <- function(datos) {
  
  end_date  <- max(datos$date, na.rm = TRUE)
  start_28d <- end_date - lubridate::days(27)
  
  df_28 <- datos |>
    dplyr::filter(date >= start_28d, date <= end_date) |>
    dplyr::mutate(
      day_diff = as.integer(end_date - date),
      week_28d = dplyr::case_when(
        day_diff >= 0  & day_diff <= 6  ~ 1L,
        day_diff >= 7  & day_diff <= 13 ~ 2L,
        day_diff >= 14 & day_diff <= 20 ~ 3L,
        day_diff >= 21 & day_diff <= 27 ~ 4L,
        TRUE ~ NA_integer_
      )
    ) |>
    dplyr::filter(!is.na(week_28d))
  
  avg_labels <- df_28 |>
    dplyr::group_by(player, week_28d) |>
    dplyr::summarise(week_total = sum(sprints_rel_count, na.rm = TRUE), .groups = "drop") |>
    tidyr::complete(player, week_28d = 1:4, fill = list(week_total = 0)) |>
    dplyr::group_by(player) |>
    dplyr::summarise(avg_4w = mean(week_total), .groups = "drop")
  
  # Last 7 days sum
  w <- get_last7_window(datos)
  df_7d <- datos |>
    dplyr::filter(date >= w$start_date, date <= w$end_date) |>
    dplyr::group_by(player) |>
    dplyr::summarise(value = sum(sprints_rel_count, na.rm = TRUE), .groups = "drop")
  
  player_levels <- df_7d |>
    dplyr::arrange(dplyr::desc(value)) |>
    dplyr::pull(player)
  
  df_plot <- df_7d |>
    dplyr::left_join(avg_labels, by = "player") |>
    dplyr::mutate(player = factor(player, levels = rev(player_levels)))
  
  ggplot2::ggplot(df_plot, ggplot2::aes(x = player, y = value)) +
    ggplot2::geom_col(fill = "#C1121F", color = "white", linewidth = 0.2) +
    ggplot2::geom_point(
      ggplot2::aes(y = avg_4w),
      color = "#FFD60A",
      size = 3.5
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::comma(round(value, 0))),
      color = "white", size = 4, fontface = "bold", hjust = 1.05
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
    ggplot2::labs(
      title = "Número de Sprints Relativos >85% (Últimos 7 Días)",
      subtitle = "Punto amarillo: Promedio semanal (promedio de 4 semanas, últimos 28 días)",
      x = NULL,
      y = "Número de Sprints"
    ) +
    base_theme()
}

# ----------------------------
# Table builder: HSR / Sprint / Sprints Abs by day
# ----------------------------
build_resumen_table <- function(datos) {
  
  # Use the latest date in the data as "today"
  today      <- max(datos$date, na.rm = TRUE)
  
  # --- Rolling 7-day window for TOTALS ---
  end_date   <- today
  start_date <- today - lubridate::days(6)
  
  # --- Current week window (Monday → today) for INDIVIDUAL DAY COLUMNS ---
  days_since_monday <- as.integer(format(today, "%u")) - 1L  # %u: Mon=1 … Sun=7
  monday     <- today - days_since_monday
  all_dates  <- seq.Date(monday, today, by = "day")
  
  # Filter for totals (last 7 days)
  df_7d <- datos |>
    dplyr::filter(date >= start_date, date <= end_date)
  
  # Filter for individual day columns (current week)
  df_win <- datos |>
    dplyr::filter(date >= monday, date <= today)
  
  # Players present in either window
  all_players <- sort(unique(c(df_7d$player, df_win$player)))
  
  # --- Totals: rolling last 7 days ---
  totals <- df_7d |>
    dplyr::group_by(player) |>
    dplyr::summarise(
      tot_hsr    = sum(HSR_abs_dist,      na.rm = TRUE),
      tot_sprint = sum(distance_abs,      na.rm = TRUE),
      tot_sabs   = sum(sprints_abs_count, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Unique date → match_day mapping, prioritizing MD over Rehab/other labels
  date_md <- df_win |>
    dplyr::distinct(date, match_day) |>
    dplyr::group_by(date) |>
    dplyr::arrange(
      dplyr::case_when(
        match_day == "MD"    ~ 1L,
        match_day == "Rehab" ~ 99L,
        TRUE                 ~ 2L
      ),
      .by_group = TRUE
    ) |>
    dplyr::slice(1) |>
    dplyr::ungroup()
  
  # --- Per-day values via pivot_wider (safe regardless of which players appear) ---
  # Add a day-index column so column names are d1/d2/… not actual dates
  date_index <- tibble::tibble(
    date  = all_dates,
    d_idx = paste0("d", seq_along(all_dates))
  )
  
  daily_long <- df_win |>
    dplyr::select(player, date, HSR_abs_dist, distance_abs, sprints_abs_count) |>
    dplyr::left_join(date_index, by = "date") |>
    dplyr::filter(!is.na(d_idx))   # drop any dates outside the 7-day window (safety)
  
  daily_hsr <- daily_long |>
    dplyr::select(player, d_idx, HSR_abs_dist) |>
    tidyr::pivot_wider(names_from = d_idx, values_from = HSR_abs_dist,
                       names_glue = "{d_idx}_hsr",
                       values_fn = sum)
  
  daily_sprint <- daily_long |>
    dplyr::select(player, d_idx, distance_abs) |>
    tidyr::pivot_wider(names_from = d_idx, values_from = distance_abs,
                       names_glue = "{d_idx}_sprint",
                       values_fn = sum)
  
  daily_sabs <- daily_long |>
    dplyr::select(player, d_idx, sprints_abs_count) |>
    tidyr::pivot_wider(names_from = d_idx, values_from = sprints_abs_count,
                       names_glue = "{d_idx}_sabs",
                       values_fn = sum)
  
  # Interleave columns: d1_hsr, d1_sprint, d1_sabs, d2_hsr, …
  ordered_cols <- unlist(lapply(seq_along(all_dates), function(i)
    c(paste0("d", i, "_hsr"), paste0("d", i, "_sprint"), paste0("d", i, "_sabs"))))
  
  # Base frame with all players in window
  base_players <- tibble::tibble(player = all_players)
  
  master <- base_players |>
    dplyr::left_join(totals,       by = "player") |>
    dplyr::left_join(daily_hsr,    by = "player") |>
    dplyr::left_join(daily_sprint, by = "player") |>
    dplyr::left_join(daily_sabs,   by = "player")
  
  # Ensure all expected day columns exist (fill missing dates with NA)
  for (col in ordered_cols) {
    if (!col %in% names(master)) master[[col]] <- NA_real_
  }
  
  # Select in correct column order
  master <- master |>
    dplyr::select(player, tot_hsr, tot_sprint, tot_sabs,
                  tidyselect::all_of(ordered_cols))
  
  # ---- Build spanner metadata for the 7 day-groups ----
  day_spanners <- lapply(seq_along(all_dates), function(i) {
    d        <- all_dates[i]
    md_label <- date_md |> dplyr::filter(date == d) |> dplyr::pull(match_day)
    md_label <- if (length(md_label) == 0) "—" else as.character(md_label[1])
    list(
      id    = paste0("day", i),
      label = paste0(format(d, "%d/%m"), " · ", md_label),
      cols  = c(paste0("d", i, "_hsr"), paste0("d", i, "_sprint"), paste0("d", i, "_sabs"))
    )
  })
  
  # ---- gt table: build base then add day spanners iteratively ----
  
  # Compact column labels (short so columns stay narrow)
  day_col_labels <- setNames(
    rep(list("HSR (m)", "Spr. (m)", "Nº Spr."), length(all_dates)),
    unlist(lapply(seq_along(all_dates), function(i)
      c(paste0("d", i, "_hsr"), paste0("d", i, "_sprint"), paste0("d", i, "_sabs"))))
  )
  # Keep only columns that actually exist in master
  day_col_labels <- day_col_labels[names(day_col_labels) %in% names(master)]
  
  tbl <- master |>
    gt::gt() |>
    gt::tab_header(
      title    = "Resumen HSR \u00b7 Sprint \u00b7 N\u00ba Sprints",
      subtitle = gt::md("Columnas **rojas**: acumulado \u00faltimos 7 d\u00edas | Columnas **negras**: sesi\u00f3n individual (semana actual)")
    ) |>
    gt::cols_label(
      player     = "Jugador",
      tot_hsr    = "HSR (m)",
      tot_sprint = "Spr. (m)",
      tot_sabs   = "Nº Spr."
    ) |>
    gt::cols_label(.list = day_col_labels) |>
    gt::tab_spanner(
      label   = "Últ. 7 Días",
      columns = c(tot_hsr, tot_sprint, tot_sabs)
    ) |>
    gt::fmt_missing(columns = tidyselect::everything(), missing_text = "\u2014") |>
    gt::tab_style(
      style     = gt::cell_text(color = "#C1121F", weight = "bold"),
      locations = gt::cells_body(columns = c(tot_hsr, tot_sprint, tot_sabs))
    ) |>
    gt::tab_style(
      style     = gt::cell_text(color = "#C1121F", weight = "bold"),
      locations = gt::cells_column_labels(columns = c(tot_hsr, tot_sprint, tot_sabs))
    ) |>
    gt::tab_style(
      style     = gt::cell_text(color = "#111827"),
      locations = gt::cells_body(columns = tidyselect::matches("^d[1-7]_"))
    ) |>
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = player)
    ) |>
    gt::tab_options(
      table.font.size                 = gt::px(11),
      heading.title.font.size         = gt::px(15),
      heading.subtitle.font.size      = gt::px(11),
      column_labels.font.weight       = "bold",
      column_labels.background.color  = "#0B1B4A",
      stub.font.weight                = "bold",
      row.striping.include_table_body = TRUE,
      row.striping.background_color   = "#f8f9fa",
      table.border.top.color          = "#0B1B4A",
      table.border.top.width          = gt::px(3),
      table.width                     = pct(100),
      data_row.padding                = gt::px(2),
      column_labels.padding           = gt::px(4)
    ) |>
    gt::tab_style(
      style     = gt::cell_text(color = "white", weight = "bold"),
      locations = gt::cells_column_labels(columns = tidyselect::everything())
    ) |>
    gt::opt_horizontal_padding(scale = 0.5) |>
    gt::fmt_number(
      columns  = c(tot_hsr, tot_sprint, tidyselect::matches("_hsr$|_sprint$")),
      decimals = 0,
      use_seps = FALSE
    ) |>
    gt::fmt_number(
      columns  = c(tot_sabs, tidyselect::matches("_sabs$")),
      decimals = 0,
      use_seps = FALSE
    ) |>
    gt::cols_width(
      player     ~ gt::px(90),
      tidyselect::matches("_hsr$|tot_hsr")       ~ gt::px(52),
      tidyselect::matches("_sprint$|tot_sprint")  ~ gt::px(52),
      tidyselect::matches("_sabs$|tot_sabs")      ~ gt::px(38)
    )
  
  # Add day spanners via a plain for-loop (avoids |> dot-placeholder issue)
  for (sp in day_spanners) {
    existing <- sp$cols[sp$cols %in% names(master)]
    if (length(existing) > 0) {
      tbl <- gt::tab_spanner(tbl, label = sp$label,
                             columns = tidyselect::all_of(existing))
    }
  }
  
  # Red spanner label for the totals group (applied after day spanners)
  tbl <- gt::tab_style(
    tbl,
    style     = gt::cell_text(color = "#C1121F", weight = "bold"),
    locations = gt::cells_column_spanners(spanners = "Últ. 7 Días")
  )
  
  # Top 3 (green) / Bottom 3 (red) shading per individual day column
  # Only applied to day columns (d1_hsr, d1_sprint, d1_sabs, etc.) — not totals
  day_metric_cols <- unlist(lapply(seq_along(all_dates), function(i)
    c(paste0("d", i, "_hsr"), paste0("d", i, "_sprint"), paste0("d", i, "_sabs"))))
  day_metric_cols <- day_metric_cols[day_metric_cols %in% names(master)]
  
  for (col in day_metric_cols) {
    vals <- master[[col]]
    # Only rank rows where value is non-NA and > 0 (ignore rest days / absent players)
    valid_idx <- which(!is.na(vals) & vals > 0)
    if (length(valid_idx) < 2) next  # need at least 2 valid rows to rank
    
    ranked   <- order(vals[valid_idx], decreasing = TRUE)
    n_top    <- min(3, length(valid_idx))
    n_bot    <- min(3, length(valid_idx))
    
    top_rows <- valid_idx[ranked[seq_len(n_top)]]
    bot_rows <- valid_idx[ranked[(length(ranked) - n_bot + 1):length(ranked)]]
    
    # Avoid overlap: if fewer than 7 valid players, top and bottom might share rows
    bot_rows <- setdiff(bot_rows, top_rows)
    
    if (length(top_rows) > 0) {
      tbl <- gt::tab_style(
        tbl,
        style     = gt::cell_fill(color = "#d4edda"),   # soft green
        locations = gt::cells_body(columns = tidyselect::all_of(col), rows = top_rows)
      )
    }
    if (length(bot_rows) > 0) {
      tbl <- gt::tab_style(
        tbl,
        style     = gt::cell_fill(color = "#f8d7da"),   # soft red
        locations = gt::cells_body(columns = tidyselect::all_of(col), rows = bot_rows)
      )
    }
  }
  
  tbl
}

# ----------------------------
# UI
# ----------------------------
ui <- fluidPage(
  titlePanel("Métricas de Carga Física - Club América"),
  tags$div(
    style = "margin-top:-10px; margin-bottom:20px; color:#4b5563; font-size:18px; font-weight:600;",
    "Acumulado Últimos 7 Días"
  ),
  
  tags$div(
    style = "margin-top:-8px; margin-bottom:20px; color:#111827; font-size:16px; font-weight:700;",
    textOutput("ultima_sesion")
  ),
  
  tabsetPanel(
    tabPanel("Resumen Diario",
             div(style = "overflow-x: auto; padding: 20px;",
                 gt::gt_output("tabla_resumen"))),
    tabPanel("HSR",               plotOutput("plot_hsr",          height = "700px")),
    tabPanel("Sprint",            plotOutput("plot_sprint",        height = "700px")),
    tabPanel("Distancia Total",   plotOutput("plot_distance",      height = "700px")),
    tabPanel("ACC + DECC",        plotOutput("plot_acc",           height = "700px")),
    tabPanel("Player Load",       plotOutput("plot_pl",            height = "700px")),
    tabPanel("% Vel. Máx. Hist",  plotOutput("plot_pct_speed",     height = "700px")),
    tabPanel("Sprints Abs. >95%", plotOutput("plot_sprints_abs",   height = "700px")),
    tabPanel("Sprints Rel. >85%", plotOutput("plot_sprints_rel",   height = "700px"))
  )
)

# ----------------------------
# Server
# ----------------------------
server <- function(input, output, session) {
  
  output$ultima_sesion <- renderText({
    req(datos)
    last_date <- as.Date(max(datos$date, na.rm = TRUE))
    paste0("Última Sesión Considerada: ", format(last_date, "%d/%m/%Y"))
  })
  
  output$plot_hsr <- renderPlot({
    plot_hsr_7d_with_4w_avg(datos)
  })
  
  output$plot_sprint <- renderPlot({
    plot_sprint_7d_with_4w_avg(datos)
  })
  
  output$plot_distance <- renderPlot({
    req(datos)
    plot_distance_total(datos)
  })
  
  output$plot_acc <- renderPlot({
    req(datos)
    plot_acc_decc(datos)
  })
  
  output$plot_pl <- renderPlot({
    req(datos)
    plot_player_load(datos)
  })
  
  output$plot_pct_speed <- renderPlot({
    req(datos)
    plot_pct_hist_speed(datos)
  })
  
  output$plot_sprints_abs <- renderPlot({
    req(datos)
    plot_sprints_abs_count(datos)
  })
  
  output$plot_sprints_rel <- renderPlot({
    req(datos)
    plot_sprints_rel_count(datos)
  })
  
  output$tabla_resumen <- gt::render_gt({
    req(datos)
    build_resumen_table(datos)
  })
}

shinyApp(ui, server)

# rsconnect::deployApp(                                                                                                                                                                                     
#   appDir         = ".",                                                              
#   appFiles       = c("app.R", "cargas7.R", "data/Sessions_micro01.xlsx"),                                                                                                                                 
#   appName        = "cargas_fisicas_7",                                                                                                                                                                  
#   account        = "mateo-rodriguez-23",                                                                                                                                                                  
#   server         = "shinyapps.io",
#   forceUpdate    = TRUE,                                                                                                                                                                                  
#   launch.browser = FALSE                                                                                                                                                                                  
# ) 