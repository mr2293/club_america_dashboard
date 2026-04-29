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
library(httr)

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

  team_avg <- mean(df_dist7$distance_m_7d, na.rm = TRUE)

  ggplot(df_dist7, aes(x = player, y = distance_m_7d)) +
    geom_col(fill = "#0B1B4A", color = "white", linewidth = 0.2) +
    geom_hline(yintercept = team_avg, color = "#C1121F", linetype = "dashed", linewidth = 1.2) +
    geom_text(
      aes(label = paste0(round(distance_m_7d / 1000), "k")),
      color = "white", size = 4, fontface = "bold", hjust = 1.05
    ) +
    annotate(
      "text", y = team_avg, x = 1,
      label     = paste0("Promedio equipo: ", round(team_avg / 1000, 1), "k"),
      color     = "#C1121F", fontface = "bold", size = 4.5, hjust = -0.1, vjust = 1.5
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

  team_avg <- mean(df_acc7$acc_plus_decc_7d, na.rm = TRUE)

  ggplot(df_acc7, aes(x = player, y = acc_plus_decc_7d)) +
    geom_col(fill = "#0B1B4A", color = "white", linewidth = 0.2) +
    geom_hline(yintercept = team_avg, color = "#C1121F", linetype = "dashed", linewidth = 1.2) +
    geom_text(
      aes(label = scales::comma(round(acc_plus_decc_7d, 0))),
      color = "white", size = 4, fontface = "bold", hjust = 1.05
    ) +
    annotate(
      "text", y = team_avg, x = 1,
      label     = paste0("Promedio equipo: ", scales::comma(round(team_avg, 0))),
      color     = "#C1121F", fontface = "bold", size = 4.5, hjust = -0.1, vjust = 1.5
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

  team_avg <- mean(df_pl7$player_load_7d, na.rm = TRUE)

  ggplot(df_pl7, aes(x = player, y = player_load_7d)) +
    geom_col(fill = "#0B1B4A", color = "white", linewidth = 0.2) +
    geom_hline(yintercept = team_avg, color = "#C1121F", linetype = "dashed", linewidth = 1.2) +
    geom_text(
      aes(label = scales::comma(round(player_load_7d, 0))),
      color = "white", size = 4, fontface = "bold", hjust = 1.05
    ) +
    annotate(
      "text", y = team_avg, x = 1,
      label     = paste0("Promedio equipo: ", scales::comma(round(team_avg, 0))),
      color     = "#C1121F", fontface = "bold", size = 4.5, hjust = -0.1, vjust = 1.5
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
      "text", y = team_avg_pct, x = 1,
      label     = paste0("Promedio equipo: ", round(team_avg_pct, 1), "%"),
      color     = "#C1121F", fontface = "bold", size = 4.5, hjust = -0.1, vjust = 1.5
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
build_player_profile <- function(datos_win, player_name, datos_full = datos) {

  cols <- c("hsr", "sprint", "dist", "acc_decc", "pl", "sp_abs", "sp_rel", "vel_max", "pct_vel")

  # --- Selected-window aggregates (all players, for team average) ---
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

  # --- 28-day personal average (anchored to selected end date) ---
  ref_date  <- max(datos_win$date, na.rm = TRUE)
  start_28d <- ref_date - lubridate::days(27)

  # Cumulative metrics: reuse get_4w_weekly_avg (weekly avg over 4 rolling weeks)
  col_map <- c(
    hsr = "HSR_abs_dist", sprint = "distance_abs", dist = "distance_m",
    acc_decc = "acc_plus_decc", pl = "player_load",
    sp_abs = "sprints_abs_count", sp_rel = "sprints_rel_count"
  )
  mes_cum <- sapply(names(col_map), function(k) {
    df  <- get_4w_weekly_avg(datos_full, col_map[[k]], ref_date = ref_date)
    val <- df$avg_4w[df$player == player_name]
    if (length(val) == 0 || is.na(val)) NA_real_ else val
  })

  # Speed metrics: mean of daily values over the 28-day window
  speed_28d <- datos_full |>
    dplyr::filter(player == player_name, date >= start_28d, date <= ref_date) |>
    dplyr::summarise(
      vel_max  = mean(max_speed,   na.rm = TRUE),
      vel_hist = max(vel_max_hist, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      pct_vel = dplyr::if_else(
        is.finite(vel_hist) & vel_hist > 0, 100 * vel_max / vel_hist, NA_real_
      )
    )

  mes_vals <- c(
    mes_cum,
    vel_max = as.numeric(speed_28d$vel_max),
    pct_vel = as.numeric(speed_28d$pct_vel)
  )[cols]  # reorder to match cols vector

  # --- Helper: pre-format a % deviation as "+X.X%" / "-X.X%" ---
  fmt_pct <- function(pct) {
    dplyr::case_when(
      pct > 0  ~ paste0("+", formatC(pct, digits = 1, format = "f"), "%"),
      pct < 0  ~ paste0(formatC(pct, digits = 1, format = "f"), "%"),
      TRUE     ~ "0.0%"
    )
  }

  profile <- tibble::tibble(
    metrica     = c(
      "HSR (m)", "Sprint (m)", "Distancia Total (m)",
      "ACC + DECC", "Player Load",
      "Sprints Abs. >24 km/h", "Sprints Rel. >85%",
      "Vel. M\u00e1xima (km/h)", "% Vel. Hist\u00f3rica"
    ),
    Jugador     = player_vals,
    Prom_Equipo = avg_vals,
    Prom_Mes    = mes_vals
  ) |>
    dplyr::mutate(
      vs_Equipo = 100 * Jugador / Prom_Equipo - 100,
      vs_Mes    = 100 * Jugador / Prom_Mes    - 100,
      vs_eq_label  = fmt_pct(vs_Equipo),
      vs_mes_label = fmt_pct(vs_Mes)
    )

  profile |>
    gt::gt() |>
    gt::tab_header(
      title    = player_name,
      subtitle = gt::md(paste0("Per\u00edodo: **", period_label(datos_win), "**"))
    ) |>
    gt::cols_label(
      metrica      = "M\u00e9trica",
      Jugador      = "Jugador",
      Prom_Equipo  = "Prom. Equipo",
      vs_eq_label  = "% vs Equipo",
      Prom_Mes     = "Prom. Sem. (28d)",
      vs_mes_label = "% vs \u00dclt. Mes"
    ) |>
    gt::cols_hide(c(vs_Equipo, vs_Mes)) |>
    gt::fmt_number(columns = c(Jugador, Prom_Equipo, Prom_Mes), decimals = 1, use_seps = TRUE) |>
    # vs Equipo highlighting
    gt::tab_style(
      style     = gt::cell_fill(color = "#d4edda"),
      locations = gt::cells_body(columns = vs_eq_label, rows = vs_Equipo > 0)
    ) |>
    gt::tab_style(
      style     = gt::cell_fill(color = "#f8d7da"),
      locations = gt::cells_body(columns = vs_eq_label, rows = vs_Equipo < 0)
    ) |>
    # vs Último Mes highlighting
    gt::tab_style(
      style     = gt::cell_fill(color = "#d4edda"),
      locations = gt::cells_body(columns = vs_mes_label, rows = vs_Mes > 0)
    ) |>
    gt::tab_style(
      style     = gt::cell_fill(color = "#f8d7da"),
      locations = gt::cells_body(columns = vs_mes_label, rows = vs_Mes < 0)
    ) |>
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = metrica)
    ) |>
    # Spanner grouping the two reference columns
    gt::tab_spanner(
      label   = "vs Promedio Equipo (periodo)",
      columns = c(Prom_Equipo, vs_eq_label)
    ) |>
    gt::tab_spanner(
      label   = "vs Promedio Personal (últ. 28 días)",
      columns = c(Prom_Mes, vs_mes_label)
    ) |>
    gt::tab_options(
      table.font.size                 = gt::px(13),
      heading.title.font.size         = gt::px(18),
      heading.subtitle.font.size      = gt::px(13),
      column_labels.font.weight       = "bold",
      column_labels.background.color  = "#0B1B4A",
      row.striping.include_table_body = TRUE,
      row.striping.background_color   = "#f8f9fa",
      table.border.top.color          = "#0B1B4A",
      table.border.top.width          = gt::px(3),
      table.width                     = pct(85),
      data_row.padding                = gt::px(6)
    ) |>
    gt::tab_style(
      style     = gt::cell_text(color = "white", weight = "bold"),
      locations = gt::cells_column_labels(columns = tidyselect::everything())
    ) |>
    gt::tab_style(
      style     = gt::cell_text(color = "white", weight = "bold"),
      locations = gt::cells_column_spanners(spanners = tidyselect::everything())
    )
}

# ----------------------------
# Match Day (MD) table
# ----------------------------
build_md_table <- function(datos, selected_date = NULL) {
  col_above <- "#D5F5E3"
  col_below <- "#FADBD8"

  # Filter to MD sessions only
  all_md <- datos |> dplyr::filter(match_day == "MD")

  if (nrow(all_md) == 0) {
    return(gt::gt(tibble::tibble(Mensaje = "No hay sesiones MD en los datos.")))
  }

  # Use selected date or fall back to most recent
  target_date <- if (!is.null(selected_date) && !is.na(selected_date)) {
    as.Date(selected_date)
  } else {
    max(all_md$date, na.rm = TRUE)
  }
  latest_md_date <- target_date
  latest_md      <- all_md |> dplyr::filter(date == latest_md_date)

  # Only include players who actually participated (non-NA / non-zero session_duration)
  if ("session_duration" %in% names(latest_md)) {
    latest_md <- latest_md |>
      dplyr::filter(!is.na(session_duration) & session_duration > 0)
  }

  active_players <- unique(latest_md$player)

  if (length(active_players) == 0) {
    return(gt::gt(tibble::tibble(
      Mensaje = "No hay jugadores con datos en el \u00faltimo partido (MD)."
    )))
  }

  # Columns that always exist in datos
  core_sum_cols <- c("distance_m", "HSR_abs_dist", "distance_abs",
                     "sprints_abs_count", "acc_plus_decc", "player_load")
  core_max_cols <- "max_speed"

  # Optional columns — only use if present
  opt_sum_cols  <- intersect("HMLD_m",  names(all_md))
  opt_num_cols  <- intersect("min",     names(all_md))   # minutes played
  opt_meta_cols <- intersect(c("group", "pos"), names(all_md))

  all_agg_cols <- c(core_sum_cols, opt_sum_cols, core_max_cols, opt_num_cols)

  # Historical MD averages per player (across every MD session)
  player_md_avgs <- all_md |>
    dplyr::group_by(player) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(intersect(all_agg_cols, names(all_md))),
        \(x) mean(x, na.rm = TRUE),
        .names = "avg_{.col}"
      ),
      .groups = "drop"
    )

  # Columns to show in table (ordered)
  display_order <- c("player", opt_meta_cols, opt_num_cols,
                     core_sum_cols, opt_sum_cols, core_max_cols)
  select_cols   <- intersect(display_order, names(latest_md))

  # Join current MD data with historical averages
  report_df <- latest_md |>
    dplyr::filter(player %in% active_players) |>
    dplyr::select(dplyr::all_of(select_cols)) |>
    dplyr::left_join(player_md_avgs, by = "player")

  # Sort: group (if exists) then distance descending
  if ("group" %in% names(report_df)) {
    report_df <- report_df |> dplyr::arrange(group, dplyr::desc(distance_m))
  } else {
    report_df <- report_df |> dplyr::arrange(dplyr::desc(distance_m))
  }

  # Metrics to colour-shade (current value vs. historical MD avg)
  metric_cfg <- list(
    list(col = "distance_m",        avg = "avg_distance_m"),
    list(col = "HSR_abs_dist",      avg = "avg_HSR_abs_dist"),
    list(col = "distance_abs",      avg = "avg_distance_abs"),
    list(col = "sprints_abs_count", avg = "avg_sprints_abs_count"),
    list(col = "acc_plus_decc",     avg = "avg_acc_plus_decc"),
    list(col = "player_load",       avg = "avg_player_load"),
    list(col = "HMLD_m",            avg = "avg_HMLD_m"),
    list(col = "max_speed",         avg = "avg_max_speed")
  )
  metric_cfg <- Filter(
    function(cfg) cfg$col %in% names(report_df) && cfg$avg %in% names(report_df),
    metric_cfg
  )

  # Strip avg_ columns from the display frame (used only for row-level comparisons)
  display_df <- report_df |> dplyr::select(-dplyr::starts_with("avg_"))

  # Column labels
  all_col_labels <- list(
    player            = "Jugador",
    group             = "Grupo",
    pos               = "Pos.",
    min               = "Min.",
    distance_m        = "Dist. Total (m)",
    HSR_abs_dist      = "HSR (m)",
    distance_abs      = "Sprint (m)",
    sprints_abs_count = "N\u00ba Sprints",
    acc_plus_decc     = "ACC+DECC",
    player_load       = "Player Load",
    HMLD_m            = "HMLD (m)",
    max_speed         = "Vel. M\u00e1x. (km/h)"
  )
  col_labels_filtered <- all_col_labels[
    intersect(names(all_col_labels), names(display_df))
  ]

  # gt groupname_col (only when group column is present)
  grp_col <- if ("group" %in% names(display_df)) "group" else NULL

  tbl <- display_df |>
    gt::gt(groupname_col = grp_col) |>
    gt::tab_header(
      title    = paste0("Partido (MD) \u00b7 ", format(latest_md_date, "%d/%m/%Y")),
      subtitle = "Verde: sobre promedio hist\u00f3rico MD del jugador \u00b7 Rojo: bajo promedio"
    ) |>
    gt::cols_label(.list = col_labels_filtered) |>
    gt::fmt_number(
      columns  = dplyr::any_of(c("distance_m", "HSR_abs_dist", "distance_abs", "HMLD_m")),
      decimals = 0, use_seps = FALSE
    ) |>
    gt::fmt_number(
      columns  = dplyr::any_of(c("sprints_abs_count", "acc_plus_decc", "player_load")),
      decimals = 0, use_seps = FALSE
    ) |>
    gt::fmt_number(
      columns  = dplyr::any_of(c("max_speed", "min")),
      decimals = 1, use_seps = FALSE
    ) |>
    gt::fmt_missing(columns = tidyselect::everything(), missing_text = "\u2014") |>
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = player)
    ) |>
    gt::tab_options(
      table.font.size                 = gt::px(12),
      heading.title.font.size         = gt::px(16),
      heading.subtitle.font.size      = gt::px(11),
      column_labels.font.weight       = "bold",
      column_labels.background.color  = "#0B1B4A",
      row.striping.include_table_body = TRUE,
      row.striping.background_color   = "#f8f9fa",
      table.border.top.color          = "#0B1B4A",
      table.border.top.width          = gt::px(3),
      table.width                     = pct(100),
      data_row.padding                = gt::px(4),
      column_labels.padding           = gt::px(4)
    ) |>
    gt::tab_style(
      style     = gt::cell_text(color = "white", weight = "bold"),
      locations = gt::cells_column_labels(columns = tidyselect::everything())
    )

  # Hide group column when used as groupname_col (gt keeps it visible by default)
  if (!is.null(grp_col)) {
    tbl <- gt::cols_hide(tbl, columns = dplyr::all_of(grp_col))
  }

  # Apply green / red cell shading vs historical MD average
  for (cfg in metric_cfg) {
    col_name <- cfg$col
    avg_name <- cfg$avg
    curr <- report_df[[col_name]]
    hist <- report_df[[avg_name]]

    above_rows <- which(!is.na(curr) & !is.na(hist) & curr > hist)
    below_rows <- which(!is.na(curr) & !is.na(hist) & curr < hist)

    if (length(above_rows) > 0)
      tbl <- gt::tab_style(tbl,
        style     = gt::cell_fill(color = col_above),
        locations = gt::cells_body(columns = dplyr::all_of(col_name), rows = above_rows))

    if (length(below_rows) > 0)
      tbl <- gt::tab_style(tbl,
        style     = gt::cell_fill(color = col_below),
        locations = gt::cells_body(columns = dplyr::all_of(col_name), rows = below_rows))
  }

  tbl
}

# ----------------------------
# ACWR (Acute:Chronic Workload Ratio)
# ----------------------------
build_acwr_table <- function(datos) {
  ref_date      <- max(datos$date, na.rm = TRUE)
  start_7d      <- ref_date - lubridate::days(6)
  ref_date_prev <- ref_date - lubridate::days(7)
  start_prev    <- ref_date_prev - lubridate::days(6)

  safe_acwr <- function(acute, chronic)
    dplyr::if_else(!is.na(chronic) & chronic > 0, round(acute / chronic, 2), NA_real_)

  # Current acute (last 7 days)
  acute <- datos |>
    dplyr::filter(date >= start_7d, date <= ref_date) |>
    dplyr::group_by(player) |>
    dplyr::summarise(
      a_dist = sum(distance_m,   na.rm = TRUE),
      a_hsr  = sum(HSR_abs_dist, na.rm = TRUE),
      a_pl   = sum(player_load,  na.rm = TRUE),
      .groups = "drop"
    )

  # Previous acute (days 7–13)
  acute_prev <- datos |>
    dplyr::filter(date >= start_prev, date <= ref_date_prev) |>
    dplyr::group_by(player) |>
    dplyr::summarise(
      p_dist = sum(distance_m,   na.rm = TRUE),
      p_hsr  = sum(HSR_abs_dist, na.rm = TRUE),
      p_pl   = sum(player_load,  na.rm = TRUE),
      .groups = "drop"
    )

  # Current & previous chronic
  chr  <- list(
    dist = get_4w_weekly_avg(datos, "distance_m",   ref_date = ref_date)      |> dplyr::rename(c_dist = avg_4w),
    hsr  = get_4w_weekly_avg(datos, "HSR_abs_dist", ref_date = ref_date)      |> dplyr::rename(c_hsr  = avg_4w),
    pl   = get_4w_weekly_avg(datos, "player_load",  ref_date = ref_date)      |> dplyr::rename(c_pl   = avg_4w),
    pdist= get_4w_weekly_avg(datos, "distance_m",   ref_date = ref_date_prev) |> dplyr::rename(pc_dist= avg_4w),
    phsr = get_4w_weekly_avg(datos, "HSR_abs_dist", ref_date = ref_date_prev) |> dplyr::rename(pc_hsr = avg_4w),
    ppl  = get_4w_weekly_avg(datos, "player_load",  ref_date = ref_date_prev) |> dplyr::rename(pc_pl  = avg_4w)
  )

  df <- acute |>
    dplyr::left_join(acute_prev,  by = "player") |>
    dplyr::left_join(chr$dist,    by = "player") |>
    dplyr::left_join(chr$hsr,     by = "player") |>
    dplyr::left_join(chr$pl,      by = "player") |>
    dplyr::left_join(chr$pdist,   by = "player") |>
    dplyr::left_join(chr$phsr,    by = "player") |>
    dplyr::left_join(chr$ppl,     by = "player") |>
    dplyr::mutate(
      acwr_dist  = safe_acwr(a_dist, c_dist),
      acwr_hsr   = safe_acwr(a_hsr,  c_hsr),
      acwr_pl    = safe_acwr(a_pl,   c_pl),
      pacwr_dist = safe_acwr(p_dist, pc_dist),
      pacwr_hsr  = safe_acwr(p_hsr,  pc_hsr),
      pacwr_pl   = safe_acwr(p_pl,   pc_pl),
      delta_dist = acwr_dist - pacwr_dist,
      delta_hsr  = acwr_hsr  - pacwr_hsr,
      delta_pl   = acwr_pl   - pacwr_pl
    ) |>
    dplyr::arrange(dplyr::desc(acwr_dist))

  # Format delta as directional arrow + value
  fmt_delta <- function(delta) {
    dplyr::case_when(
      is.na(delta)    ~ "\u2014",
      delta >  0.005  ~ paste0("\u2191 +", formatC(delta, digits = 2, format = "f")),
      delta < -0.005  ~ paste0("\u2193 ",  formatC(delta, digits = 2, format = "f")),
      TRUE            ~ "\u2192 0.00"
    )
  }

  df <- df |>
    dplyr::mutate(
      trend_dist = fmt_delta(delta_dist),
      trend_hsr  = fmt_delta(delta_hsr),
      trend_pl   = fmt_delta(delta_pl)
    )

  date_label <- paste0(format(start_7d, "%d/%m"), " \u2013 ", format(ref_date, "%d/%m/%Y"))

  tbl <- df |>
    dplyr::select(player, acwr_dist, trend_dist, acwr_hsr, trend_hsr, acwr_pl, trend_pl) |>
    gt::gt() |>
    gt::tab_header(
      title    = paste0("ACWR \u00b7 ", date_label),
      subtitle = gt::md(
        "**Azul** < 0.8 \u2014 subcarga &nbsp;|&nbsp; **Verde** 0.8\u20131.3 \u2014 \u00f3ptimo &nbsp;|&nbsp; **Naranja** 1.3\u20131.5 \u2014 precauci\u00f3n &nbsp;|&nbsp; **Rojo** > 1.5 \u2014 riesgo &nbsp;|&nbsp; Tendencia vs. semana anterior"
      )
    ) |>
    gt::cols_label(
      player     = "Jugador",
      acwr_dist  = "ACWR",  trend_dist = "\u0394",
      acwr_hsr   = "ACWR",  trend_hsr  = "\u0394",
      acwr_pl    = "ACWR",  trend_pl   = "\u0394"
    ) |>
    gt::tab_spanner(label = "Dist. Total",  columns = c(acwr_dist, trend_dist)) |>
    gt::tab_spanner(label = "HSR",          columns = c(acwr_hsr,  trend_hsr))  |>
    gt::tab_spanner(label = "Player Load",  columns = c(acwr_pl,   trend_pl))   |>
    gt::fmt_number(columns = c(acwr_dist, acwr_hsr, acwr_pl), decimals = 2) |>
    gt::fmt_missing(columns = tidyselect::everything(), missing_text = "\u2014") |>
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = player)
    ) |>
    gt::tab_style(
      style     = gt::cell_text(color = "#9ca3af", size = gt::px(11)),
      locations = gt::cells_body(columns = c(trend_dist, trend_hsr, trend_pl))
    ) |>
    gt::tab_options(
      table.font.size                 = gt::px(13),
      heading.title.font.size         = gt::px(16),
      heading.subtitle.font.size      = gt::px(11),
      column_labels.font.weight       = "bold",
      column_labels.background.color  = "#0B1B4A",
      row.striping.include_table_body = TRUE,
      row.striping.background_color   = "#f8f9fa",
      table.border.top.color          = "#0B1B4A",
      table.border.top.width          = gt::px(3),
      table.width                     = gt::pct(75),
      data_row.padding                = gt::px(5)
    ) |>
    gt::tab_style(
      style     = gt::cell_text(color = "white", weight = "bold"),
      locations = gt::cells_column_labels(columns = tidyselect::everything())
    ) |>
    gt::tab_style(
      style     = gt::cell_text(color = "white", weight = "bold"),
      locations = gt::cells_column_spanners(spanners = tidyselect::everything())
    )

  # Zone fill on ACWR value columns
  zone_map <- list(
    list(color = "#AED6F1", test = function(x) !is.na(x) & x < 0.8),
    list(color = "#D5F5E3", test = function(x) !is.na(x) & x >= 0.8 & x <= 1.3),
    list(color = "#FAD7A0", test = function(x) !is.na(x) & x > 1.3  & x <= 1.5),
    list(color = "#FADBD8", test = function(x) !is.na(x) & x > 1.5)
  )
  for (col in c("acwr_dist", "acwr_hsr", "acwr_pl")) {
    vals <- df[[col]]
    for (z in zone_map) {
      rows <- which(z$test(vals))
      if (length(rows) > 0)
        tbl <- gt::tab_style(tbl,
          style     = gt::cell_fill(color = z$color),
          locations = gt::cells_body(columns = dplyr::all_of(col), rows = rows))
    }
  }

  # Trend text coloring: context-aware (rising in danger zone = red, etc.)
  trend_cols  <- c(trend_dist = "acwr_dist", trend_hsr = "acwr_hsr", trend_pl = "acwr_pl")
  delta_cols  <- c(trend_dist = "delta_dist", trend_hsr = "delta_hsr", trend_pl = "delta_pl")
  for (tcol in names(trend_cols)) {
    acwr  <- df[[trend_cols[[tcol]]]]
    delta <- df[[delta_cols[[tcol]]]]
    red_rows   <- which(!is.na(delta) & ((acwr > 1.3 & delta > 0) | (acwr < 0.8 & delta < 0)))
    green_rows <- which(!is.na(delta) & ((acwr > 1.3 & delta < 0) | (acwr < 0.8 & delta > 0)))
    if (length(red_rows) > 0)
      tbl <- gt::tab_style(tbl,
        style     = gt::cell_text(color = "#C1121F", weight = "bold", size = gt::px(11)),
        locations = gt::cells_body(columns = dplyr::all_of(tcol), rows = red_rows))
    if (length(green_rows) > 0)
      tbl <- gt::tab_style(tbl,
        style     = gt::cell_text(color = "#166534", weight = "bold", size = gt::px(11)),
        locations = gt::cells_body(columns = dplyr::all_of(tcol), rows = green_rows))
  }

  tbl
}

# ----------------------------
# MD-relative session profiles
# ----------------------------
build_md_relative_plot <- function(datos, metric = "distance_m") {
  metric_labels <- c(
    distance_m        = "Distancia Total (m)",
    HSR_abs_dist      = "HSR (m)",
    distance_abs      = "Sprint (m)",
    acc_plus_decc     = "ACC + DECC",
    player_load       = "Player Load",
    sprints_abs_count = "Sprints >24 km/h"
  )

  # Map match_day labels directly — no proximity calculation needed
  rel_levels <- c("MD-5", "MD-4", "MD-3", "MD-2", "MD-1", "MD", "MD+1", "MD+2")

  label_map <- c(
    "-5" = "MD-5", "-4" = "MD-4", "-3" = "MD-3",
    "-2" = "MD-2", "-1" = "MD-1", "MD" = "MD",
    "+1" = "MD+1", "+2" = "MD+2"
  )

  avg_by_day <- datos |>
    dplyr::filter(match_day %in% names(label_map)) |>
    dplyr::mutate(
      label    = factor(label_map[match_day], levels = rel_levels),
      is_match = match_day == "MD"
    ) |>
    dplyr::group_by(label, is_match) |>
    dplyr::summarise(avg_val = mean(.data[[metric]], na.rm = TRUE), .groups = "drop")

  if (nrow(avg_by_day) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
                          label = "No hay sesiones MD etiquetadas en los datos.", size = 6) +
        ggplot2::theme_void()
    )
  }

  metric_name <- metric_labels[[metric]]

  ggplot2::ggplot(avg_by_day, ggplot2::aes(x = label, y = avg_val, fill = is_match)) +
    ggplot2::geom_col(color = "white", linewidth = 0.3, width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::comma(round(avg_val, 0))),
      color = "white", size = 4.5, fontface = "bold", vjust = 1.5
    ) +
    ggplot2::scale_fill_manual(values = c("FALSE" = "#0B1B4A", "TRUE" = "#C1121F"), guide = "none") +
    ggplot2::scale_y_continuous(labels = scales::comma, expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(
      title    = paste0("MD comparado con otros días del Microciclo \u00b7 ", metric_name),
      subtitle = "Promedio del equipo por tipo de sesi\u00f3n \u00b7 Rojo = d\u00eda de partido (MD)",
      x = NULL, y = metric_name
    ) +
    base_theme() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey85", linewidth = 0.4)
    )
}

# ----------------------------
# Claude API helpers
# ----------------------------
call_claude_api <- function(prompt, max_tokens = 500) {
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (nchar(trimws(api_key)) == 0)
    return("Error: la variable ANTHROPIC_API_KEY no est\u00e1 configurada en el servidor.")

  tryCatch({
    resp <- httr::POST(
      url  = "https://api.anthropic.com/v1/messages",
      httr::add_headers(
        "x-api-key"         = api_key,
        "anthropic-version" = "2023-06-01",
        "content-type"      = "application/json"
      ),
      body   = jsonlite::toJSON(list(
        model      = "claude-haiku-4-5-20251001",
        max_tokens = max_tokens,
        messages   = list(list(role = "user", content = prompt))
      ), auto_unbox = TRUE),
      encode = "raw"
    )
    if (httr::status_code(resp) != 200) {
      err <- httr::content(resp, as = "parsed")
      return(paste0("Error de API (HTTP ", httr::status_code(resp), "): ",
                    err$error$message))
    }
    parsed <- httr::content(resp, as = "parsed", encoding = "UTF-8")
    parsed$content[[1]]$text
  }, error = function(e) {
    paste0("Error de conexi\u00f3n: ", conditionMessage(e))
  })
}

build_narrative_prompt <- function(datos) {
  ref_date <- max(datos$date, na.rm = TRUE)
  start_7d <- ref_date - lubridate::days(6)

  acute <- datos |>
    dplyr::filter(date >= start_7d, date <= ref_date) |>
    dplyr::group_by(player) |>
    dplyr::summarise(
      dist = sum(distance_m,   na.rm = TRUE),
      hsr  = sum(HSR_abs_dist, na.rm = TRUE),
      pl   = sum(player_load,  na.rm = TRUE),
      .groups = "drop"
    )

  chr_dist <- get_4w_weekly_avg(datos, "distance_m", ref_date = ref_date)

  df <- acute |>
    dplyr::left_join(chr_dist, by = "player") |>
    dplyr::mutate(
      acwr = dplyr::if_else(!is.na(avg_4w) & avg_4w > 0, round(dist / avg_4w, 2), NA_real_)
    ) |>
    dplyr::arrange(dplyr::desc(dist))

  rows <- df |>
    dplyr::mutate(
      txt = sprintf("%-22s | Dist: %5.0f m | HSR: %4.0f m | PL: %5.0f | ACWR: %s",
                    player, dist, hsr, pl,
                    dplyr::if_else(is.na(acwr), "N/D", sprintf("%.2f", acwr)))
    ) |>
    dplyr::pull(txt)

  recent_mds <- datos |>
    dplyr::filter(match_day == "MD") |>
    dplyr::distinct(date) |>
    dplyr::arrange(dplyr::desc(date)) |>
    dplyr::slice_head(n = 3) |>
    dplyr::pull(date) |>
    format("%d/%m/%Y") |>
    paste(collapse = ", ")

  paste0(
    "Eres un analista de ciencias del deporte del Club Am\u00e9rica.\n",
    "Analiza los datos de carga f\u00edsica y escribe UN solo p\u00e1rrafo de 4-5 oraciones en espa\u00f1ol.\n",
    "S\u00e9 concreto: menciona jugadores por nombre y sus valores num\u00e9ricos exactos.\n",
    "Comenta: qui\u00e9n tuvo m\u00e1s y menos carga, ACWRs preocupantes (>1.3 o <0.8), ",
    "y c\u00f3mo fue la semana en general.\n",
    "REGLAS ESTRICTAS — no las rompas bajo ninguna circunstancia:\n",
    "- No menciones al portero ni a ning\u00fan jugador que juegue en esa posici\u00f3n.\n",
    "- No menciones aspectos t\u00e1cticos, esquemas de juego, ni decisiones de entrenador.\n",
    "- No hagas sugerencias, recomendaciones ni predicciones de ning\u00fan tipo.\n",
    "- No uses vi\u00f1etas. No pongas t\u00edtulo. Solo el p\u00e1rrafo.\n\n",
    "PER\u00cdODO: ", format(start_7d, "%d/%m/%Y"), " a ", format(ref_date, "%d/%m/%Y"), "\n",
    "\u00daltimos partidos (MD): ", if (nchar(recent_mds) == 0) "ninguno" else recent_mds, "\n\n",
    "DATOS POR JUGADOR:\n",
    paste(rows, collapse = "\n")
  )
}

build_nl_prompt <- function(datos, question) {
  ref_date  <- max(datos$date, na.rm = TRUE)
  min_date  <- min(datos$date, na.rm = TRUE)
  start_30d <- ref_date - lubridate::days(29)

  summary_7d <- datos |>
    dplyr::filter(date >= start_30d, date <= ref_date) |>
    dplyr::group_by(player) |>
    dplyr::summarise(
      dist  = sum(distance_m,        na.rm = TRUE),
      hsr   = sum(HSR_abs_dist,      na.rm = TRUE),
      spr_m = sum(distance_abs,      na.rm = TRUE),
      n_spr = sum(sprints_abs_count, na.rm = TRUE),
      pl    = sum(player_load,       na.rm = TRUE),
      vmax  = max(max_speed,         na.rm = TRUE),
      .groups = "drop"
    )

  # Helper: build a numbered ranking for one metric
  rank_block <- function(df, col, title, fmt) {
    sorted <- dplyr::arrange(df, dplyr::desc(.data[[col]]))
    lines  <- sprintf("%2d. %-22s \u2014 %s", seq_len(nrow(sorted)),
                      sorted$player, fmt(sorted[[col]]))
    paste0(title, ":\n", paste(lines, collapse = "\n"))
  }

  m <- function(x) paste0(scales::comma(round(x, 0)), " m")
  n <- function(x) as.character(round(x, 0))
  v <- function(x) paste0(round(x, 1), " km/h")

  rankings <- paste(
    rank_block(summary_7d, "dist",  "DISTANCIA TOTAL",         m),
    rank_block(summary_7d, "hsr",   "HSR (alta intensidad)",   m),
    rank_block(summary_7d, "spr_m", "SPRINT (distancia)",      m),
    rank_block(summary_7d, "n_spr", "N\u00daMERO DE SPRINTS",  n),
    rank_block(summary_7d, "pl",    "PLAYER LOAD",             n),
    rank_block(summary_7d, "vmax",  "VELOCIDAD M\u00c1XIMA",   v),
    sep = "\n\n"
  )

  md_dates_str <- datos |>
    dplyr::filter(match_day == "MD") |>
    dplyr::distinct(date) |>
    dplyr::arrange(dplyr::desc(date)) |>
    dplyr::slice_head(n = 5) |>
    dplyr::pull(date) |>
    format("%d/%m/%Y") |>
    paste(collapse = ", ")

  paste0(
    "Eres un analista de ciencias del deporte del Club Am\u00e9rica. ",
    "Responde la siguiente pregunta usando \u00fanicamente los datos pre-ordenados que se muestran abajo. ",
    "Los rankings ya est\u00e1n calculados y ordenados correctamente de mayor a menor — \u00fasalos directamente. ",
    "S\u00e9 directo y menciona valores num\u00e9ricos. Responde en espa\u00f1ol.\n\n",
    "DATOS DISPONIBLES: ", format(min_date, "%d/%m/%Y"), " a ", format(ref_date, "%d/%m/%Y"), "\n",
    "PARTIDOS RECIENTES (MD): ",
    if (nchar(md_dates_str) == 0) "ninguno" else md_dates_str, "\n\n",
    "RANKINGS \u00daltimos 30 d\u00edas (ordenados de mayor a menor):\n\n",
    rankings, "\n\n",
    "PREGUNTA: ", question
  )
}

# ----------------------------
# Pre-compute MD date choices (used in selectInput)
# ----------------------------
md_dates_vec <- datos |>
  dplyr::filter(match_day == "MD") |>
  dplyr::distinct(date) |>
  dplyr::arrange(dplyr::desc(date)) |>
  dplyr::pull(date)

md_date_choices <- setNames(
  as.character(md_dates_vec),
  format(md_dates_vec, "%d/%m/%Y")
)

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
        label    = "Periodo:",
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
    tabPanel("Partido (MD)",
      fluidRow(
        column(3,
          tags$div(style = "padding: 20px 20px 0 20px;",
            selectInput(
              inputId  = "md_date",
              label    = "Partido:",
              choices  = md_date_choices,
              selected = md_date_choices[1]
            )
          )
        )
      ),
      fluidRow(
        column(12,
          div(style = "overflow-x: auto; padding: 10px 20px 20px 20px;",
              gt::gt_output("tabla_md")))
      )
    ),
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
        column(12,
          div(style = "padding: 10px 20px 20px 20px; overflow-x: auto;",
              gt::gt_output("tabla_perfil"))
        )
      )
    ),
    tabPanel("ACWR",
      fluidRow(
        column(12,
          div(style = "padding: 20px; overflow-x: auto;",
              gt::gt_output("tabla_acwr"))
        )
      )
    ),
    tabPanel("Perfil MD",
      fluidRow(
        column(3,
          tags$div(style = "padding: 20px 20px 0 20px;",
            selectInput(
              inputId  = "md_rel_metric",
              label    = "M\u00e9trica:",
              choices  = c(
                "Distancia Total"  = "distance_m",
                "HSR"              = "HSR_abs_dist",
                "Sprint"           = "distance_abs",
                "ACC + DECC"       = "acc_plus_decc",
                "Player Load"      = "player_load",
                "Sprints >24 km/h" = "sprints_abs_count"
              ),
              selected = "distance_m"
            )
          )
        )
      ),
      fluidRow(
        column(12,
          plotOutput("plot_md_relative", height = "520px"))
      )
    ),
    tabPanel("An\u00e1lisis IA",
      fluidRow(
        column(12,
          tags$div(style = "padding: 24px 24px 8px 24px;",
            tags$h4("Narrativa Semanal",
                    style = "font-weight:700; color:#0B1B4A; margin-bottom:4px;"),
            tags$p("Resumen autom\u00e1tico sobre la carga de los \u00faltimos 7 d\u00edas.",
                   style = "color:#6b7280; font-size:14px; margin-bottom:12px;"),
            actionButton("btn_narrative", "Generar Narrativa",
                         style = paste0("background-color:#0B1B4A; color:white; font-weight:bold;",
                                        " border:none; border-radius:4px; padding:8px 18px;")),
            tags$div(
              style = paste0("margin-top:14px; padding:16px; background:#f8f9fa;",
                             " border-left:4px solid #0B1B4A; border-radius:4px; min-height:80px;",
                             " font-size:15px; line-height:1.65;"),
              textOutput("narrative_out")
            )
          )
        )
      ),
      tags$hr(style = "margin: 4px 24px;"),
      fluidRow(
        column(12,
          tags$div(style = "padding: 16px 24px 24px 24px;",
            tags$h4("Consulta en Lenguaje Natural",
                    style = "font-weight:700; color:#0B1B4A; margin-bottom:4px;"),
            tags$p("Haz cualquier pregunta sobre los datos de carga f\u00edsica.",
                   style = "color:#6b7280; font-size:14px; margin-bottom:12px;"),
            fluidRow(
              column(8,
                textInput("nl_query", NULL, width = "100%",
                          placeholder = "\u00bfQui\u00e9n tuvo m\u00e1s HSR la \u00faltima semana?")
              ),
              column(2,
                tags$div(style = "padding-top:0px;",
                  actionButton("btn_query", "Consultar",
                               style = paste0("background-color:#C1121F; color:white; font-weight:bold;",
                                              " border:none; border-radius:4px; padding:8px 18px;"))
                )
              )
            ),
            tags$div(
              style = paste0("margin-top:14px; padding:16px; background:#f8f9fa;",
                             " border-left:4px solid #C1121F; border-radius:4px; min-height:80px;",
                             " font-size:15px; line-height:1.65;"),
              textOutput("nl_answer_out")
            )
          )
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

  output$tabla_md <- gt::render_gt({
    req(input$md_date)
    build_md_table(datos, input$md_date)
  })

  output$tabla_perfil <- gt::render_gt({
    req(datos_win(), input$profile_player)
    build_player_profile(datos_win(), input$profile_player)
  })

  # ACWR — always anchored to the most recent date in datos
  output$tabla_acwr <- gt::render_gt({
    build_acwr_table(datos)
  })

  # MD-relative profile — reacts to metric selector
  output$plot_md_relative <- renderPlot({
    req(input$md_rel_metric)
    build_md_relative_plot(datos, input$md_rel_metric)
  })

  # IA — narrative (button-triggered, blocking with progress indicator)
  narrative_val <- reactiveVal("")
  observeEvent(input$btn_narrative, {
    withProgress(message = "Consultando IA\u2026", value = 0.6, {
      prompt <- build_narrative_prompt(datos)
      result <- call_claude_api(prompt, max_tokens = 450)
      narrative_val(result)
    })
  })
  output$narrative_out <- renderText(narrative_val())

  # IA — natural language query (button-triggered)
  nl_answer_val <- reactiveVal("")
  observeEvent(input$btn_query, {
    req(nchar(trimws(input$nl_query)) > 0)
    withProgress(message = "Consultando IA\u2026", value = 0.6, {
      prompt <- build_nl_prompt(datos, input$nl_query)
      result <- call_claude_api(prompt, max_tokens = 500)
      nl_answer_val(result)
    })
  })
  output$nl_answer_out <- renderText(nl_answer_val())
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
