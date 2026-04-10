# app.R
# Métricas de Carga Física — Club América

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
  end_date   <- max(datos$date, na.rm = TRUE)
  start_date <- end_date - days(6)
  list(start_date = start_date, end_date = end_date)
}

period_label <- function(d) {
  paste0(format(min(d$date, na.rm = TRUE), "%d/%m"),
         " \u2013 ",
         format(max(d$date, na.rm = TRUE), "%d/%m/%Y"))
}

base_theme <- function() {
  theme_minimal(base_size = 16) +
    theme(
      plot.title      = element_text(face = "bold", size = 22, margin = margin(b = 12)),
      axis.text.x     = element_text(size = 14),
      axis.text.y     = element_text(size = 14, face = "bold"),
      axis.line       = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
      plot.margin     = margin(15, 30, 15, 20),
      legend.position = "top",
      legend.text     = element_text(size = 16, face = "bold")
    )
}

# Returns a df with columns: player, avg_4w
# avg_4w = average weekly total of `col` over the 4 rolling 7-day windows ending at ref_date
get_4w_weekly_avg <- function(datos, col, ref_date = NULL) {
  end_date  <- if (is.null(ref_date)) max(datos$date, na.rm = TRUE) else as.Date(ref_date)
  start_28d <- end_date - lubridate::days(27)

  datos |>
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
    dplyr::filter(!is.na(week_28d)) |>
    dplyr::group_by(player, week_28d) |>
    dplyr::summarise(week_total = sum(.data[[col]], na.rm = TRUE), .groups = "drop") |>
    tidyr::complete(player, week_28d = 1:4, fill = list(week_total = 0)) |>
    dplyr::group_by(player) |>
    dplyr::summarise(avg_4w = mean(week_total), .groups = "drop")
}

# ----------------------------
# Plot builders
# datos_win  = already date-filtered data (drives bar heights)
# datos_full = full dataset (drives 4-week rolling average context)
# ----------------------------

plot_hsr_7d_with_4w_avg <- function(datos_win, datos_full = datos) {

  hsr_labels <- get_4w_weekly_avg(datos_full, "HSR_abs_dist",
                                   ref_date = max(datos_win$date, na.rm = TRUE))

  df_7d <- datos_win |>
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
    ggplot2::geom_point(ggplot2::aes(y = avg_4w), color = "#FFD60A", size = 3.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::comma(round(value, 0))),
      color = "white", size = 4, fontface = "bold", hjust = 1.05
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
    ggplot2::labs(
      title    = paste0("Distancia en HSR \u00b7 ", period_label(datos_win)),
      subtitle = "Punto amarillo: Promedio semanal (promedio de 4 semanas, \u00faltimos 28 d\u00edas)",
      x = NULL, y = "Distancia (m)"
    ) +
    base_theme()
}

plot_sprint_7d_with_4w_avg <- function(datos_win, datos_full = datos) {

  sprint_labels <- get_4w_weekly_avg(datos_full, "distance_abs",
                                      ref_date = max(datos_win$date, na.rm = TRUE))

  df_7d <- datos_win |>
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
    ggplot2::geom_point(ggplot2::aes(y = avg_4w), color = "#FFD60A", size = 3.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::comma(round(value, 0))),
      color = "white", size = 4, fontface = "bold", hjust = 1.05
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
    ggplot2::labs(
      title    = paste0("Distancia en Sprint \u00b7 ", period_label(datos_win)),
      subtitle = "Punto amarillo: Promedio semanal (promedio de 4 semanas, \u00faltimos 28 d\u00edas)",
      x = NULL, y = "Distancia (m)"
    ) +
    base_theme()
}

plot_distance_total <- function(datos_win) {
  df_dist7 <- datos_win |>
    group_by(player) |>
    summarise(distance_m_7d = sum(distance_m, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(distance_m_7d)) |>
    mutate(player = factor(player, levels = rev(player)))

  team_avg  <- mean(df_dist7$distance_m_7d, na.rm = TRUE)
  n_players <- nrow(df_dist7)

  ggplot(df_dist7, aes(x = player, y = distance_m_7d)) +
    geom_col(fill = "#0B1B4A", color = "white", linewidth = 0.2) +
    geom_hline(yintercept = team_avg, color = "#C1121F", linetype = "dashed", linewidth = 1.2) +
    geom_text(
      aes(label = paste0(round(distance_m_7d / 1000), "k")),
      color = "white", size = 4, fontface = "bold", hjust = 1.05
    ) +
    annotate(
      "text", y = team_avg, x = n_players,
      label     = paste0("Promedio equipo: ", round(team_avg / 1000, 1), "k"),
      color     = "#C1121F", fontface = "bold", size = 4.5, hjust = -0.1, vjust = -0.5
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = paste0("Distancia Total \u00b7 ", period_label(datos_win)),
      x = NULL, y = "Distancia (m)"
    ) +
    base_theme()
}

plot_acc_decc <- function(datos_win) {
  df_acc7 <- datos_win |>
    group_by(player) |>
    summarise(acc_plus_decc_7d = sum(acc_plus_decc, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(acc_plus_decc_7d)) |>
    mutate(player = factor(player, levels = rev(player)))

  team_avg  <- mean(df_acc7$acc_plus_decc_7d, na.rm = TRUE)
  n_players <- nrow(df_acc7)

  ggplot(df_acc7, aes(x = player, y = acc_plus_decc_7d)) +
    geom_col(fill = "#0B1B4A", color = "white", linewidth = 0.2) +
    geom_hline(yintercept = team_avg, color = "#C1121F", linetype = "dashed", linewidth = 1.2) +
    geom_text(
      aes(label = scales::comma(round(acc_plus_decc_7d, 0))),
      color = "white", size = 4, fontface = "bold", hjust = 1.05
    ) +
    annotate(
      "text", y = team_avg, x = n_players,
      label     = paste0("Promedio equipo: ", scales::comma(round(team_avg, 0))),
      color     = "#C1121F", fontface = "bold", size = 4.5, hjust = -0.1, vjust = -0.5
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = paste0("ACC + DECC \u00b7 ", period_label(datos_win)),
      x = NULL, y = "ACC + DECC (acumulado)"
    ) +
    base_theme()
}

plot_player_load <- function(datos_win) {
  df_pl7 <- datos_win |>
    group_by(player) |>
    summarise(player_load_7d = sum(player_load, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(player_load_7d)) |>
    mutate(player = factor(player, levels = rev(player)))

  team_avg  <- mean(df_pl7$player_load_7d, na.rm = TRUE)
  n_players <- nrow(df_pl7)

  ggplot(df_pl7, aes(x = player, y = player_load_7d)) +
    geom_col(fill = "#0B1B4A", color = "white", linewidth = 0.2) +
    geom_hline(yintercept = team_avg, color = "#C1121F", linetype = "dashed", linewidth = 1.2) +
    geom_text(
      aes(label = scales::comma(round(player_load_7d, 0))),
      color = "white", size = 4, fontface = "bold", hjust = 1.05
    ) +
    annotate(
      "text", y = team_avg, x = n_players,
      label     = paste0("Promedio equipo: ", scales::comma(round(team_avg, 0))),
      color     = "#C1121F", fontface = "bold", size = 4.5, hjust = -0.1, vjust = -0.5
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = paste0("Player Load \u00b7 ", period_label(datos_win)),
      x = NULL, y = "Player Load"
    ) +
    base_theme()
}

plot_pct_hist_speed <- function(datos_win) {
  df_speed7 <- datos_win |>
    group_by(player) |>
    summarise(
      avg_speed_7d = mean(max_speed,    na.rm = TRUE),
      max_speed_7d = max(max_speed,     na.rm = TRUE),
      vel_max_hist = max(vel_max_hist,  na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      pct_hist_7d = if_else(
        is.finite(vel_max_hist) & vel_max_hist > 0,
        100 * (max_speed_7d / vel_max_hist), NA_real_
      ),
      avg_speed_pct_hist = if_else(
        is.finite(vel_max_hist) & vel_max_hist > 0,
        100 * (avg_speed_7d / vel_max_hist), NA_real_
      )
    ) |>
    arrange(desc(pct_hist_7d)) |>
    mutate(player = factor(player, levels = rev(player)))

  team_avg_pct <- mean(df_speed7$pct_hist_7d, na.rm = TRUE)
  n_players    <- nrow(df_speed7)

  ggplot(df_speed7, aes(x = player, y = pct_hist_7d)) +
    geom_col(fill = "#0B1B4A", color = "white", linewidth = 0.2) +
    geom_hline(yintercept = team_avg_pct, color = "#C1121F", linetype = "dashed", linewidth = 1.2) +
    geom_text(
      aes(label = paste0(round(pct_hist_7d, 0), "%")),
      color = "white", size = 4, fontface = "bold", hjust = 1.05
    ) +
    geom_text(
      aes(y = 1, label = paste0(round(avg_speed_7d, 1), " km/h (",
                                round(avg_speed_pct_hist, 0), "%)")),
      color = "white", size = 4, fontface = "bold", hjust = 0
    ) +
    annotate(
      "text", y = team_avg_pct, x = n_players,
      label     = paste0("Promedio equipo: ", round(team_avg_pct, 1), "%"),
      color     = "#C1121F", fontface = "bold", size = 4.5, hjust = -0.1, vjust = -0.5
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(labels = ~ paste0(scales::comma(.x), "%"), limits = c(0, NA)) +
    labs(
      title    = paste0("% Vel. M\u00e1x. Hist\u00f3rica \u00b7 ", period_label(datos_win)),
      subtitle = "Etiqueta izquierda: Promedio de velocidad m\u00e1xima del per\u00edodo\n(% de promedio en comparaci\u00f3n a su velocidad m\u00e1xima hist\u00f3rica.)",
      x = NULL, y = "% de Velocidad M\u00e1xima Hist\u00f3rica"
    ) +
    base_theme() +
    theme(plot.margin = margin(t = 10, r = 10, b = 10, l = 30))
}

plot_sprints_abs_count <- function(datos_win, datos_full = datos) {

  avg_labels <- get_4w_weekly_avg(datos_full, "sprints_abs_count",
                                   ref_date = max(datos_win$date, na.rm = TRUE))

  df_7d <- datos_win |>
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
    ggplot2::geom_point(ggplot2::aes(y = avg_4w), color = "#FFD60A", size = 3.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::comma(round(value, 0))),
      color = "white", size = 4, fontface = "bold", hjust = 1.05
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
    ggplot2::labs(
      title    = paste0("Sprints Absolutos >24 km/h \u00b7 ", period_label(datos_win)),
      subtitle = "Punto amarillo: Promedio semanal (promedio de 4 semanas, \u00faltimos 28 d\u00edas)",
      x = NULL, y = "N\u00famero de Sprints"
    ) +
    base_theme()
}

plot_sprints_rel_count <- function(datos_win, datos_full = datos) {

  avg_labels <- get_4w_weekly_avg(datos_full, "sprints_rel_count",
                                   ref_date = max(datos_win$date, na.rm = TRUE))

  df_7d <- datos_win |>
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
    ggplot2::geom_point(ggplot2::aes(y = avg_4w), color = "#FFD60A", size = 3.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::comma(round(value, 0))),
      color = "white", size = 4, fontface = "bold", hjust = 1.05
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
    ggplot2::labs(
      title    = paste0("Sprints Relativos >85% \u00b7 ", period_label(datos_win)),
      subtitle = "Punto amarillo: Promedio semanal (promedio de 4 semanas, \u00faltimos 28 d\u00edas)",
      x = NULL, y = "N\u00famero de Sprints"
    ) +
    base_theme()
}

# ----------------------------
# Table builder: HSR / Sprint / Sprints Abs by day
# ----------------------------
build_resumen_table <- function(datos_win) {

  today      <- max(datos_win$date, na.rm = TRUE)
  end_date   <- today
  start_date <- today - lubridate::days(6)

  days_since_monday <- as.integer(format(today, "%u")) - 1L
  monday    <- today - days_since_monday
  all_dates <- seq.Date(monday, today, by = "day")

  df_7d <- datos_win |>
    dplyr::filter(date >= start_date, date <= end_date)

  df_win <- datos_win |>
    dplyr::filter(date >= monday, date <= today)

  all_players <- sort(unique(c(df_7d$player, df_win$player)))

  totals <- df_7d |>
    dplyr::group_by(player) |>
    dplyr::summarise(
      tot_hsr    = sum(HSR_abs_dist,      na.rm = TRUE),
      tot_sprint = sum(distance_abs,      na.rm = TRUE),
      tot_sabs   = sum(sprints_abs_count, na.rm = TRUE),
      .groups = "drop"
    )

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

  date_index <- tibble::tibble(
    date  = all_dates,
    d_idx = paste0("d", seq_along(all_dates))
  )

  daily_long <- df_win |>
    dplyr::select(player, date, HSR_abs_dist, distance_abs, sprints_abs_count) |>
    dplyr::left_join(date_index, by = "date") |>
    dplyr::filter(!is.na(d_idx))

  daily_hsr <- daily_long |>
    dplyr::select(player, d_idx, HSR_abs_dist) |>
    tidyr::pivot_wider(names_from = d_idx, values_from = HSR_abs_dist,
                       names_glue = "{d_idx}_hsr", values_fn = sum)

  daily_sprint <- daily_long |>
    dplyr::select(player, d_idx, distance_abs) |>
    tidyr::pivot_wider(names_from = d_idx, values_from = distance_abs,
                       names_glue = "{d_idx}_sprint", values_fn = sum)

  daily_sabs <- daily_long |>
    dplyr::select(player, d_idx, sprints_abs_count) |>
    tidyr::pivot_wider(names_from = d_idx, values_from = sprints_abs_count,
                       names_glue = "{d_idx}_sabs", values_fn = sum)

  ordered_cols <- unlist(lapply(seq_along(all_dates), function(i)
    c(paste0("d", i, "_hsr"), paste0("d", i, "_sprint"), paste0("d", i, "_sabs"))))

  base_players <- tibble::tibble(player = all_players)

  master <- base_players |>
    dplyr::left_join(totals,       by = "player") |>
    dplyr::left_join(daily_hsr,    by = "player") |>
    dplyr::left_join(daily_sprint, by = "player") |>
    dplyr::left_join(daily_sabs,   by = "player")

  for (col in ordered_cols) {
    if (!col %in% names(master)) master[[col]] <- NA_real_
  }

  master <- master |>
    dplyr::select(player, tot_hsr, tot_sprint, tot_sabs,
                  tidyselect::all_of(ordered_cols))

  day_spanners <- lapply(seq_along(all_dates), function(i) {
    d        <- all_dates[i]
    md_label <- date_md |> dplyr::filter(date == d) |> dplyr::pull(match_day)
    md_label <- if (length(md_label) == 0) "\u2014" else as.character(md_label[1])
    list(
      id    = paste0("day", i),
      label = paste0(format(d, "%d/%m"), " \u00b7 ", md_label),
      cols  = c(paste0("d", i, "_hsr"), paste0("d", i, "_sprint"), paste0("d", i, "_sabs"))
    )
  })

  day_col_labels <- setNames(
    rep(list("HSR (m)", "Spr. (m)", "N\u00ba Spr."), length(all_dates)),
    unlist(lapply(seq_along(all_dates), function(i)
      c(paste0("d", i, "_hsr"), paste0("d", i, "_sprint"), paste0("d", i, "_sabs"))))
  )
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
      tot_sabs   = "N\u00ba Spr."
    ) |>
    gt::cols_label(.list = day_col_labels) |>
    gt::tab_spanner(label = "\u00dalt. 7 D\u00edas", columns = c(tot_hsr, tot_sprint, tot_sabs)) |>
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
      decimals = 0, use_seps = FALSE
    ) |>
    gt::fmt_number(
      columns  = c(tot_sabs, tidyselect::matches("_sabs$")),
      decimals = 0, use_seps = FALSE
    ) |>
    gt::cols_width(
      player                                        ~ gt::px(90),
      tidyselect::matches("_hsr$|tot_hsr")          ~ gt::px(52),
      tidyselect::matches("_sprint$|tot_sprint")    ~ gt::px(52),
      tidyselect::matches("_sabs$|tot_sabs")        ~ gt::px(38)
    )

  for (sp in day_spanners) {
    existing <- sp$cols[sp$cols %in% names(master)]
    if (length(existing) > 0)
      tbl <- gt::tab_spanner(tbl, label = sp$label, columns = tidyselect::all_of(existing))
  }

  tbl <- gt::tab_style(
    tbl,
    style     = gt::cell_text(color = "#C1121F", weight = "bold"),
    locations = gt::cells_column_spanners(spanners = "\u00dalt. 7 D\u00edas")
  )

  day_metric_cols <- unlist(lapply(seq_along(all_dates), function(i)
    c(paste0("d", i, "_hsr"), paste0("d", i, "_sprint"), paste0("d", i, "_sabs"))))
  day_metric_cols <- day_metric_cols[day_metric_cols %in% names(master)]

  for (col in day_metric_cols) {
    vals      <- master[[col]]
    valid_idx <- which(!is.na(vals) & vals > 0)
    if (length(valid_idx) < 2) next

    ranked   <- order(vals[valid_idx], decreasing = TRUE)
    n_top    <- min(3, length(valid_idx))
    n_bot    <- min(3, length(valid_idx))
    top_rows <- valid_idx[ranked[seq_len(n_top)]]
    bot_rows <- setdiff(valid_idx[ranked[(length(ranked) - n_bot + 1):length(ranked)]], top_rows)

    if (length(top_rows) > 0)
      tbl <- gt::tab_style(tbl,
        style     = gt::cell_fill(color = "#d4edda"),
        locations = gt::cells_body(columns = tidyselect::all_of(col), rows = top_rows))
    if (length(bot_rows) > 0)
      tbl <- gt::tab_style(tbl,
        style     = gt::cell_fill(color = "#f8d7da"),
        locations = gt::cells_body(columns = tidyselect::all_of(col), rows = bot_rows))
  }

  tbl
}

# ----------------------------
# Player profile table
# ----------------------------
build_player_profile <- function(datos_win, player_name) {

  cols <- c("hsr", "sprint", "dist", "acc_decc", "pl", "sp_abs", "sp_rel", "vel_max", "pct_vel")

  team_sums <- datos_win |>
    dplyr::group_by(player) |>
    dplyr::summarise(
      hsr      = sum(HSR_abs_dist,      na.rm = TRUE),
      sprint   = sum(distance_abs,      na.rm = TRUE),
      dist     = sum(distance_m,        na.rm = TRUE),
      acc_decc = sum(acc_plus_decc,     na.rm = TRUE),
      pl       = sum(player_load,       na.rm = TRUE),
      sp_abs   = sum(sprints_abs_count, na.rm = TRUE),
      sp_rel   = sum(sprints_rel_count, na.rm = TRUE),
      vel_max  = max(max_speed,         na.rm = TRUE),
      vel_hist = max(vel_max_hist,      na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      pct_vel = dplyr::if_else(
        is.finite(vel_hist) & vel_hist > 0, 100 * vel_max / vel_hist, NA_real_
      )
    )

  player_row <- dplyr::filter(team_sums, player == player_name)

  if (nrow(player_row) == 0) {
    return(gt::gt(tibble::tibble(
      Mensaje = paste0("Sin datos para ", player_name, " en el per\u00edodo seleccionado.")
    )))
  }

  team_avg <- team_sums |>
    dplyr::summarise(dplyr::across(dplyr::all_of(cols), \(x) mean(x, na.rm = TRUE)))

  player_vals <- as.numeric(player_row[1, cols])
  avg_vals    <- as.numeric(team_avg[1, cols])

  profile <- tibble::tibble(
    metrica     = c(
      "HSR (m)", "Sprint (m)", "Distancia Total (m)",
      "ACC + DECC", "Player Load",
      "Sprints Abs. >24 km/h", "Sprints Rel. >85%",
      "Vel. M\u00e1xima (km/h)", "% Vel. Hist\u00f3rica"
    ),
    Jugador     = player_vals,
    Prom_Equipo = avg_vals
  ) |>
    dplyr::mutate(
      vs_Equipo = 100 * Jugador / Prom_Equipo - 100,
      # Pre-format label in R to avoid gt's Unicode minus breaking as.numeric()
      vs_label  = dplyr::case_when(
        vs_Equipo > 0  ~ paste0("+", formatC(vs_Equipo, digits = 1, format = "f"), "%"),
        vs_Equipo < 0  ~ paste0(formatC(vs_Equipo, digits = 1, format = "f"), "%"),
        TRUE           ~ "0.0%"
      )
    )

  profile |>
    gt::gt() |>
    gt::tab_header(
      title    = player_name,
      subtitle = gt::md(paste0("Per\u00edodo: **", period_label(datos_win), "**"))
    ) |>
    gt::cols_label(
      metrica     = "M\u00e9trica",
      Jugador     = "Jugador",
      Prom_Equipo = "Prom. Equipo",
      vs_label    = "% vs Equipo"
    ) |>
    gt::cols_hide(vs_Equipo) |>
    gt::fmt_number(columns = c(Jugador, Prom_Equipo), decimals = 1, use_seps = TRUE) |>
    gt::tab_style(
      style     = gt::cell_fill(color = "#d4edda"),
      locations = gt::cells_body(columns = vs_label, rows = vs_Equipo > 0)
    ) |>
    gt::tab_style(
      style     = gt::cell_fill(color = "#f8d7da"),
      locations = gt::cells_body(columns = vs_label, rows = vs_Equipo < 0)
    ) |>
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = metrica)
    ) |>
    gt::tab_options(
      table.font.size                 = gt::px(14),
      heading.title.font.size         = gt::px(18),
      heading.subtitle.font.size      = gt::px(13),
      column_labels.font.weight       = "bold",
      column_labels.background.color  = "#0B1B4A",
      row.striping.include_table_body = TRUE,
      row.striping.background_color   = "#f8f9fa",
      table.border.top.color          = "#0B1B4A",
      table.border.top.width          = gt::px(3),
      table.width                     = pct(60),
      data_row.padding                = gt::px(6)
    ) |>
    gt::tab_style(
      style     = gt::cell_text(color = "white", weight = "bold"),
      locations = gt::cells_column_labels(columns = tidyselect::everything())
    )
}

# ----------------------------
# UI
# ----------------------------
ui <- fluidPage(
  titlePanel("M\u00e9tricas de Carga F\u00edsica - Club Am\u00e9rica"),

  tags$div(
    style = "margin-top:-10px; margin-bottom:10px; color:#4b5563; font-size:18px; font-weight:600;",
    "Acumulado por Per\u00edodo Seleccionado"
  ),

  tags$div(
    style = "margin-top:-4px; margin-bottom:8px; color:#111827; font-size:16px; font-weight:700;",
    textOutput("ultima_sesion")
  ),

  fluidRow(
    column(4,
      dateRangeInput(
        inputId  = "date_range",
        label    = "Per\u00edodo:",
        start    = max(datos$date, na.rm = TRUE) - days(6),
        end      = max(datos$date, na.rm = TRUE),
        min      = min(datos$date, na.rm = TRUE),
        max      = max(datos$date, na.rm = TRUE),
        format   = "dd/mm/yyyy",
        language = "es",
        separator = " \u2013 "
      )
    )
  ),

  tabsetPanel(
    tabPanel("Resumen Diario",
             div(style = "overflow-x: auto; padding: 20px;",
                 gt::gt_output("tabla_resumen"))),
    tabPanel("HSR",                   plotOutput("plot_hsr",         height = "700px")),
    tabPanel("Sprint",                plotOutput("plot_sprint",       height = "700px")),
    tabPanel("Distancia Total",       plotOutput("plot_distance",     height = "700px")),
    tabPanel("ACC + DECC",            plotOutput("plot_acc",          height = "700px")),
    tabPanel("Player Load",           plotOutput("plot_pl",           height = "700px")),
    tabPanel("% Vel. M\u00e1x. Hist", plotOutput("plot_pct_speed",    height = "700px")),
    tabPanel("Sprints Abs. >24 km/h", plotOutput("plot_sprints_abs",  height = "700px")),
    tabPanel("Sprints Rel. >85%",     plotOutput("plot_sprints_rel",  height = "700px")),
    tabPanel("Perfil Jugador",
      fluidRow(
        column(3,
          tags$div(style = "padding: 20px 20px 0 20px;",
            selectInput(
              inputId  = "profile_player",
              label    = "Jugador:",
              choices  = sort(selected_players),
              selected = sort(selected_players)[1]
            )
          )
        )
      ),
      fluidRow(
        column(8,
          div(style = "padding: 10px 20px 20px 20px;",
              gt::gt_output("tabla_perfil"))
        )
      )
    )
  )
)

# ----------------------------
# Server
# ----------------------------
server <- function(input, output, session) {

  # Reactive filtered dataset — drives all team-level plots
  datos_win <- reactive({
    req(input$date_range)
    start <- as.Date(input$date_range[1])
    end   <- as.Date(input$date_range[2])
    validate(need(start <= end, "La fecha de inicio debe ser anterior a la fecha final."))
    datos |> dplyr::filter(date >= start, date <= end)
  })

  output$ultima_sesion <- renderText({
    req(datos_win())
    last_date <- as.Date(max(datos_win()$date, na.rm = TRUE))
    paste0("\u00daltima Sesi\u00f3n Considerada: ", format(last_date, "%d/%m/%Y"))
  })

  output$plot_hsr <- renderPlot({
    req(datos_win())
    plot_hsr_7d_with_4w_avg(datos_win(), datos)
  })

  output$plot_sprint <- renderPlot({
    req(datos_win())
    plot_sprint_7d_with_4w_avg(datos_win(), datos)
  })

  output$plot_distance <- renderPlot({
    req(datos_win())
    plot_distance_total(datos_win())
  })

  output$plot_acc <- renderPlot({
    req(datos_win())
    plot_acc_decc(datos_win())
  })

  output$plot_pl <- renderPlot({
    req(datos_win())
    plot_player_load(datos_win())
  })

  output$plot_pct_speed <- renderPlot({
    req(datos_win())
    plot_pct_hist_speed(datos_win())
  })

  output$plot_sprints_abs <- renderPlot({
    req(datos_win())
    plot_sprints_abs_count(datos_win(), datos)
  })

  output$plot_sprints_rel <- renderPlot({
    req(datos_win())
    plot_sprints_rel_count(datos_win(), datos)
  })

  output$tabla_resumen <- gt::render_gt({
    req(datos_win())
    build_resumen_table(datos_win())
  })

  output$tabla_perfil <- gt::render_gt({
    req(datos_win(), input$profile_player)
    build_player_profile(datos_win(), input$profile_player)
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
