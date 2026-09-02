library(tidyverse)
library(zoo)
library(reshape2)
library(gt)
library(ggrepel)
library(lubridate)
library(readxl)
library(dplyr)
library(httr)
library(jsonlite)

# Leer Cuestionario de Bienestar de Jugadores ---------
survey_path <- "data/bienestar_jugador_primer_equipo_respuestas.xlsx"

recuperacion_df <- read_xlsx(survey_path)

# Darle Puntuación a Recuperación de Jugadores -------------

compute_recovery_score <- function(fatigue, sleep_quality, sleep_hours, soreness, pain_zone) {
  score <- 0
  
  # Fatigue scoring
  score <- score + case_when(
    fatigue == "Muy fresco" ~ 2.5,
    fatigue == "Fresco" ~ 2.2,
    fatigue == "Mejor que lo normal" ~ 1.8,
    fatigue == "Normal" ~ 1.5,
    fatigue == "Peor que lo normal" ~ 1.2,
    fatigue == "Cansado" ~ 0.8,
    fatigue == "Muy cansado" ~ 0.5,
    TRUE ~ 0
  )
  
  # Sleep quality
  score <- score + case_when(
    sleep_quality == "Muy buena noche" ~ 2.5,
    sleep_quality == "Buena noche" ~ 2.2,
    sleep_quality == "Mejor que normal" ~ 1.8,
    sleep_quality == "Normal" ~ 1.5,
    sleep_quality == "Peor que normal" ~ 1.2,
    sleep_quality == "Mala noche" ~ 0.8,
    sleep_quality == "Muy mala noche" ~ 0.5,
    sleep_quality == "Me desperté mucho" ~ 0.5,
    TRUE ~ 0
  )
  
  # Sleep duration
  score <- score + case_when(
    sleep_hours == "9 o mas"    ~ 2.5,
    sleep_hours == "8.0"          ~ 2,
    sleep_hours == "7.0"          ~ 1.5,
    sleep_hours == "6.0"          ~ 1,
    sleep_hours == "5 o menos"  ~ 0.5,
    TRUE ~ 0
  )
  
  # Pain
  score <- score + case_when(
    soreness == "Normal" ~ 2.5,
    soreness == "No me duele nada" ~ 2,
    soreness == "Adolorido de una zona" ~ 1.5,
    soreness == "Muy adolorido en general" ~ 1,
    
    TRUE ~ 0
  )
  
  return(round(score, 1))  # Max = 10
}

compute_rest_score <- function(fatigue, sleep_quality, sleep_hours) {
  score <- 0
  
  # Fatigue scoring
  score <- score + case_when(
    fatigue == "Muy fresco" ~ 5,
    fatigue == "Fresco" ~ 4.5,
    fatigue == "Mejor que lo normal" ~ 3.5,
    fatigue == "Normal" ~ 3,
    fatigue == "Peor que lo normal" ~ 2.5,
    fatigue == "Cansado" ~ 2,
    fatigue == "Muy cansado" ~ 1,
    TRUE ~ 0
  )
  
  # Sleep quality
  score <- score + case_when(
    sleep_quality == "Muy buena noche" ~ 2.5,
    sleep_quality == "Buena noche" ~ 2.2,
    sleep_quality == "Mejor que normal" ~ 1.8,
    sleep_quality == "Normal" ~ 1.5,
    sleep_quality == "Peor que normal" ~ 1.2,
    sleep_quality == "Mala noche" ~ 0.8,
    sleep_quality == "Muy mala noche" ~ 0.5,
    sleep_quality == "Me desperté mucho" ~ 0.5,
    TRUE ~ 0
  )
  
  # Sleep duration
  score <- score + case_when(
    sleep_hours == "9 o mas"    ~ 2.5,
    sleep_hours == "8.0"          ~ 2,
    sleep_hours == "7.0"          ~ 1.5,
    sleep_hours == "6.0"          ~ 1,
    sleep_hours == "5 o menos"  ~ 0.5,
    TRUE ~ 0
  )
  
  return(round(score, 1))  # Max = 10
}

compute_pain_score <- function(soreness) {
  score <- 0
  
  # Pain
  score <- score + case_when(
    soreness == "Normal" ~ 4,
    soreness == "No me duele nada" ~ 5,
    soreness == "Adolorido de una zona" ~ 6,
    soreness == "Muy adolorido en general" ~ 8,
    
    TRUE ~ 0
  )
  
  return(round(score, 1))  # Max = 10
}

recuperacion_df <- recuperacion_df|>
  mutate(recovery_score = compute_recovery_score(
    `Nivel de Cansancio hoy (Fatiga)`,
    `Qué tal descansaste ayer?`,
    `Cuántas horas dormiste ayer?`,
    `Estás adolorido de alguna parte?`,
    `Donde te encuentras adolorido? Indica cada zona de dolor`
  )) |>
  mutate(rest_score = compute_rest_score(
    `Nivel de Cansancio hoy (Fatiga)`,
    `Qué tal descansaste ayer?`,
    `Cuántas horas dormiste ayer?`)) |>
  mutate(pain_score = compute_pain_score(
    `Estás adolorido de alguna parte?`))

# Gráficas Sueño, Fatiga, Dolor Muscular -----------

recuperacion_df <- recuperacion_df |>
  mutate(`Marca temporal` = as.Date(`Marca temporal`))

# Scoring definitions
niveles_fatiga <- c("Muy fresco" = 10, "Fresco" = 9, "Mejor que lo normal" = 8, 
                    "Normal" = 6, "Peor que lo normal" = 5, "Cansado" = 4, "Muy cansado" = 3)
niveles_sueño <- c("Muy buena noche" = 10, "Buena noche" = 9, "Mejor que normal" = 8,
                   "Normal" = 6, "Peor que normal" = 5, "Mala noche" = 4, 
                   "Muy mala noche" = 3, "Me desperté mucho" = 3)
niveles_dolor <- c("No me duele nada" = 10, "Normal" = 8,
                   "Adolorido de una zona" = 6, "Muy adolorido en general" = 4)

# Apply mappings
recuperacion_df <- recuperacion_df |> 
  mutate(
    fatiga_score = niveles_fatiga[`Nivel de Cansancio hoy (Fatiga)`],
    sueño_score = niveles_sueño[`Qué tal descansaste ayer?`],
    dolor_score = niveles_dolor[`Estás adolorido de alguna parte?`]
  )

str(recuperacion_df[3043:3068, ])



# Function to plot by player
plot_player_recuperacion <- function(player_name) {
  
  player_df <- recuperacion_df |> 
    filter(Nombre == player_name)
  
  max_day <- max(player_df$`Marca temporal`, na.rm = TRUE)
  player_df <- player_df |> filter(`Marca temporal` >= max_day - 20)
  
  player_long <- player_df |> 
    select(`Marca temporal`, Nombre,
           `Nivel de Cansancio hoy (Fatiga)`, 
           `Qué tal descansaste ayer?`,
           `Estás adolorido de alguna parte?`,
           `Donde te encuentras adolorido? Indica cada zona de dolor`,
           fatiga_score, sueño_score, dolor_score) |> 
    rename(`Zona Adolorida` = `Donde te encuentras adolorido? Indica cada zona de dolor`) |>
    pivot_longer(
      cols = c(fatiga_score, sueño_score, dolor_score),
      names_to = "score_type", values_to = "score"
    ) |> 
    mutate(
      type = case_when(
        score_type == "fatiga_score" ~ "Fatiga",
        score_type == "sueño_score" ~ "Sueño",
        score_type == "dolor_score" ~ "Dolor Muscular"
      ),
      respuesta = case_when(
        score_type == "fatiga_score" ~ `Nivel de Cansancio hoy (Fatiga)`,
        score_type == "sueño_score" ~ `Qué tal descansaste ayer?`,
        score_type == "dolor_score" ~ `Estás adolorido de alguna parte?`
      ),
      color = case_when(
        type == "Dolor Muscular" & score >= 7 ~ "Alta",
        type == "Dolor Muscular" & score < 7 ~ "Baja",
        type != "Dolor Muscular" & score >= 6 ~ "Alta",
        type != "Dolor Muscular" & score < 6 ~ "Baja",
        TRUE ~ "Indefinido"
      )    
      )
  
  threshold_df <- data.frame(
    type      = c("Fatiga", "Sueño", "Dolor Muscular"),
    threshold = c(6, 6, 7)
  )

  ggplot(player_long, aes(x = `Marca temporal`, y = score, group = type)) +
    geom_hline(data = threshold_df, aes(yintercept = threshold),
               linetype = "dashed", color = "gray40", linewidth = 0.5, alpha = 0.7) +
    geom_line(aes(color = type), linewidth = 0.6, alpha = 0.5) +
    geom_point(aes(
      fill = color,
      text = paste0(
        "Jugador: ", Nombre,
        "<br>Fecha: ", `Marca temporal`,
        "<br>Respuesta: ", respuesta,
        ifelse(type == "Dolor Muscular" & !is.na(`Zona Adolorida`),
               paste0("<br>Zona Adolorida: ", `Zona Adolorida`), ""),
        "<br>Score: ", score
      )
    ),
    shape = 21, size = 5, color = "white") +
    facet_wrap(~type, ncol = 1, scales = "free_y") +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
    scale_fill_manual(values = c("Alta" = "#1a9850", "Baja" = "#d7191c")) +
    scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
    labs(
      title = paste("Indicadores de Recuperación Diaria –", player_name),
      x = NULL, y = "Puntuación", subtitle = "Score Diario"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_line(color = "gray85"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.text = element_text(face = "bold", size = 14),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}

# Example usage:
plot_player_recuperacion("Henry Martín")

## Plot A:C 7:21 Individual -----------

micros_shiny_comb <- read_csv("micros/micros_shiny_comb.csv") |>
  mutate(player = case_when(
    # player == "Pato Salas" ~ "Patricio Salas",
    player == "Miguel Ram’rez" ~ "Miguel Ramírez",
    player == "Miguel Ramirez" ~ "Miguel Ramírez",
    player == "Miguel  Vazquez" ~ "Miguel Vázquez",
    player == "Luis Ángel Malagón Velázquez" ~ "Luis Ángel Malagón",
    player == "Alexis Gutierrez" ~ "Alexis Gutiérrez",
    player == "Sebastian Cáceres" ~ "Sebastián Cáceres",
    player == "Isaias Violante" ~ "Isaías Violante",
    player == "Jose Zuniga" ~ "José Raúl Zúñiga",
    player == "kevin alvarez" ~ "Kevin Álvarez",
    player == "Erick Sanchez" ~ "Erick Sánchez",
    player == "Brian Rodriguez" ~ "Brian Rodríguez",
    player == "Alejandro Zendejas Saavedra" ~ "Alejandro Zendejas",
    player == "Santiago Naveda Lara" ~ "Santiago Naveda",
    player == "Dago Espinoza" ~ "Dagoberto Espinoza",
    player == "Alejandro Cardenas" ~ "Alejandro Cárdenas",
    player == "Alejandro Fernandez" ~ "Adrián Fernández",
    player == "Guillermo Cortes" ~ "Guillermo Cortéz",
    player == "Icaro Conceicao" ~ "Ícaro da Conceicao",
    player == "Ricardo Gonzalez" ~ "Ricardo González",
    player == "Adrian Fernandez" ~ "Adrián Fernández",
    player == "Oscar Perea" ~ "Óscar Perea",
    player == "Edwin Cerrillo" ~ "Edwin Cerrillo",
    TRUE ~ player
  ),
  date = as.Date(date))

jugs <- micros_shiny_comb |>
  distinct(player, .keep_all = TRUE)

# --- MD flag & minutes (per player/day) ---
md_day_minutes <- micros_shiny_comb |>
  group_by(player, date) |>
  summarise(
    is_md   = any(match_day == "MD"),
    minutos = sum(session_duration[match_day == "MD"], na.rm = TRUE),
    .groups = "drop"
  )

# --- Current microcycle phase (from latest session) ---
current_session_date <- max(micros_shiny_comb$date, na.rm = TRUE)
current_md_phase <- micros_shiny_comb |>
  filter(date == current_session_date) |>
  count(match_day, sort = TRUE) |>
  slice_head(n = 1) |>
  pull(match_day)

# --- EWMA helpers ---
alpha_acute   <- 0.75
alpha_chronic <- 0.35

ewma <- function(x, alpha) {
  n <- length(x)
  if (n == 0) return(NA_real_)
  w <- (1 - alpha)^((n - 1):0)
  w <- w / sum(w)
  sum(x * w)
}

process_ewma <- function(data, col) {
  data |>
    mutate(
      !!paste0("acute_load_", col) := zoo::rollapply(
        .data[[col]], width = 7,
        FUN = \(x) ewma(x, alpha_acute), align = "right",
        fill = NA, partial = TRUE
      ),
      !!paste0("chronic_load_", col) := zoo::rollapply(
        .data[[col]], width = 21,
        FUN = \(x) ewma(x, alpha_chronic), align = "right",
        fill = NA, partial = TRUE
      ),
      !!paste0("ACWR_", col) := .data[[paste0("acute_load_", col)]] /
        .data[[paste0("chronic_load_", col)]]
    )
}

# --- Players subset (as before) ---
selected_players <- c(
  "Israel Reyes","Henry Martín","Alejandro Zendejas",
  "Isaías Violante","Alan Cervantes","Ramón Juárez","Erick Sánchez",
  "Brian Rodríguez","Kevin Álvarez","Dagoberto Espinoza","Víctor Dávila",
  "Cristian Borja","Alexis Gutiérrez", "Sebastián Cáceres","Miguel Vázquez",
  "José Raúl Zúñiga", "Pato Salas", "Raphael Veiga", "Thiago Espinosa", 
  "Emilio Lara", "Franco Rossano", "Santiago Naveda", "Alejandro Cárdenas",
  "Adrián Fernández", "Guillermo Cortéz", "Ícaro da Conceicao",
  "Ricardo González", "Rodolfo Cota", "Luis Ángel Malagón", "Diego Arriaga",
  "Óscar Perea", "Edwin Cerrillo"
)

# --- Build acute/chronic/ACWR and join MD info ---
micros_individual <- micros_shiny_comb |>
  filter(player %in% selected_players) |>
  group_by(player, date) |>
  summarize(load = sum(HSR_abs_dist, na.rm = TRUE), .groups = "drop") |>
  arrange(player, date) |>
  group_by(player) |>
  arrange(date) |>
  group_modify(~ process_ewma(.x, "load")) |>
  ungroup() |>
  rename(
    carga_aguda   = acute_load_load,
    carga_cronica = chronic_load_load,
    ac_ratio      = ACWR_load
  ) |>
  left_join(md_day_minutes, by = c("player", "date")) |>
  mutate(
    is_md   = coalesce(is_md, FALSE),
    minutos = if_else(is_md, minutos, NA_real_)
  )

# --- Flag: last four MDs ≥ 80 minutes ---
last4_md_80_flag <- function(player, up_to_date) {
  last4 <- md_day_minutes |>
    filter(player == !!player, is_md, date <= up_to_date) |>
    arrange(desc(date)) |>
    slice_head(n = 4)
  nrow(last4) == 4 && all(!is.na(last4$minutos) & last4$minutos >= 80)
}

# --- Plot function ---
plot_individual_ac <- function(player) {
  
  max_day <- max(micros_individual$date, na.rm = TRUE)
  
  filtered_data <- micros_individual |>
    filter(player == !!player, date >= max_day - 20) |>
    mutate(
      tooltip_acute = if_else(
        is_md,
        paste0("<b>MATCH DAY</b>",
               "<br>Jugador: ", player,
               "<br>Fecha: ", format(date, "%Y-%m-%d"),
               "<br>Carga Aguda: ", round(carga_aguda, 1),
               "<br>Minutos: ", round(minutos)),
        paste0("Jugador: ", player,
               "<br>Fecha: ", format(date, "%Y-%m-%d"),
               "<br>Carga Aguda: ", round(carga_aguda, 1))
      )
    )
  
  # High-competition banner flag
  high_comp_flag <- last4_md_80_flag(player, up_to_date = max_day)
  
  # Dynamic scaling for A:C
  max_load <- max(c(filtered_data$carga_aguda, filtered_data$carga_cronica), na.rm = TRUE)
  left_ylim <- if (is.finite(max_load)) max_load * 1.2 else 1
  ac_min <- max(0.5, floor(min(filtered_data$ac_ratio, na.rm = TRUE) * 2) / 2)
  ac_max <- max(2.0, ceiling(max(filtered_data$ac_ratio, na.rm = TRUE) * 2) / 2)
  ac_max <- min(ac_max, 3.0)
  ac_breaks <- seq(ac_min, ac_max, by = 0.5)
  scaling_factor <- left_ylim / ac_max
  
  g <- ggplot(filtered_data, aes(x = date)) +
    # Acute
    geom_line(aes(y = carga_aguda, color = "Carga Aguda"), linewidth = 2) +
    geom_point(
      data = filtered_data |> filter(!is_md),
      aes(y = carga_aguda, color = "Carga Aguda", text = tooltip_acute),
      size = 3.5, shape = 16
    ) +
    geom_point(
      data = filtered_data |> filter(is_md),
      aes(y = carga_aguda, color = "Carga Aguda", text = tooltip_acute),
      size = 4, shape = 21, fill = "white", stroke = 1.5
    ) +
    
    # Chronic
    geom_line(aes(y = carga_cronica, color = "Carga Crónica"), linewidth = 2) +
    geom_point(aes(y = carga_cronica, color = "Carga Crónica",
                   text = paste0("Jugador: ", player,
                                 "<br>Fecha: ", format(date, "%Y-%m-%d"),
                                 "<br>Carga Crónica: ", round(carga_cronica, 1))),
               size = 3.5) +
    
    # A:C (scaled)
    geom_line(aes(y = ac_ratio * scaling_factor, color = "Relación A:C"),
              linewidth = 2, linetype = "dashed") +
    geom_point(aes(y = ac_ratio * scaling_factor, color = "Relación A:C",
                   text = paste0("Jugador: ", player,
                                 "<br>Fecha: ", format(date, "%Y-%m-%d"),
                                 "<br>Relación A:C: ", round(ac_ratio, 2))),
               size = 3.5) +
    
    # Guide lines for A:C
    geom_hline(yintercept = c(0.8, 1.3) * scaling_factor,
               linetype = "dashed", color = "goldenrod", alpha = 0.35) +
    
    # Axes
    scale_y_continuous(
      limits = c(0, left_ylim),
      name   = "Nivel de Carga",
      sec.axis = sec_axis(~ . / scaling_factor,
                          name = "Relación A:C", breaks = ac_breaks)
    ) +
    scale_x_date(date_breaks = "1 day", date_labels = "%d-%b") +
    
    # Right-edge labels for A:C
    geom_text(
      data = tibble(
        x = max(filtered_data$date, na.rm = TRUE) + 0.2,
        y = ac_breaks * scaling_factor,
        label = as.character(ac_breaks)
      ),
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 0, size = 5.8, fontface = "bold", color = "black"
    ) +
    
    scale_color_manual(values = c(
      "Carga Aguda"   = "#0072B2",
      "Carga Crónica" = "#4D4D4D",
      "Relación A:C"  = "goldenrod"
    )) +
    theme_minimal() +
    theme(
      axis.text.x   = element_text(angle = 45, hjust = 1, size = 14),
      axis.text.y   = element_text(size = 16),
      axis.title.y  = element_text(size = 16, margin = margin(r = 10)),
      plot.title    = element_text(hjust = 0.5, size = 22, face = "bold",
                                   margin = margin(t = 35, b = 10)),
      plot.margin   = margin(t = 25, r = 20, b = 20, l = 20),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position = "bottom",
      legend.title    = element_text(size = 14),
      legend.text     = element_text(size = 13)
    ) +
    labs(title = paste("Relación A:C 7:21 –", player),
         x = NULL, y = "Nivel de Carga", color = NULL)
  
  # Red banner if last four MDs >= 80'
  if (high_comp_flag) {
    g <- g + annotate(
      "label",
      x = mean(range(filtered_data$date, na.rm = TRUE)),
      y = left_ylim * 0.98, vjust = 1,
      label = "Carga Alta de Competencia",
      fill = "#C62828", color = "white",
      fontface = "bold", size = 6.5,
      label.size = 0,
      label.padding = grid::unit(0.25, "lines")
    )
  }
  
  g
}

# Example
plot_individual_ac("Henry Martín")

# Plot Individual HSR -------

micros_hsr <- micros_shiny_comb |>
  filter(player %in% selected_players) |>
  mutate(date = as.Date(date)) |>
  group_by(player, date) |>
  summarize(HSR_abs_dist = sum(HSR_abs_dist, na.rm = TRUE), .groups = "drop") |>
  arrange(player, date) |>
  group_by(player) |>
  group_modify(~ process_ewma(.x, "HSR_abs_dist")) |>
  ungroup() |>
  rename(
    acute_HSR   = acute_load_HSR_abs_dist,
    chronic_HSR = chronic_load_HSR_abs_dist
  ) |>
  # <- NEW: bring in MD flag and minutes
  left_join(md_day_minutes, by = c("player", "date")) |>
  mutate(
    is_md   = dplyr::coalesce(is_md, FALSE),
    minutos = dplyr::if_else(is_md, minutos, NA_real_)
  )

plot_individual_hsr <- function(player) {
  max_day <- max(micros_hsr$date, na.rm = TRUE)
  
  # keep last ~3 weeks for the selected player and build tooltip for acute HSR
  filtered_data <- micros_hsr |>
    filter(player == !!player, date >= max_day - 20) |>
    mutate(
      tooltip_acute = if_else(
        is_md,
        paste0(
          "<b>MATCH DAY</b>",
          "<br>Jugador: ", player,
          "<br>Fecha: ", format(date, "%Y-%m-%d"),
          "<br>HSR Agudo: ", round(acute_HSR, 1),
          "<br>Minutos: ", round(minutos)
        ),
        paste0(
          "Jugador: ", player,
          "<br>Fecha: ", format(date, "%Y-%m-%d"),
          "<br>HSR Agudo: ", round(acute_HSR, 1)
        )
      )
    )
  
  # Safeguard against empty data
  if (nrow(filtered_data) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste("No hay datos recientes para", player),
                 size = 6, color = "red") +
        theme_void()
    )
  }
  
  # Top limit for annotation headroom
  max_hsr <- max(c(filtered_data$acute_HSR, filtered_data$chronic_HSR), na.rm = TRUE)
  left_ylim <- if (is.finite(max_hsr)) max_hsr * 1.2 else 1
  
  # Check last 4 MDs ≥ 80'
  high_comp_flag <- last4_md_80_flag(player, up_to_date = max_day)
  
  g <- ggplot(filtered_data, aes(x = date)) +
    # Acute HSR
    geom_line(aes(y = acute_HSR, color = "HSR Agudo"), linewidth = 2) +
    # non-MD: filled points
    geom_point(
      data = dplyr::filter(filtered_data, !is_md),
      aes(y = acute_HSR, color = "HSR Agudo", text = tooltip_acute),
      size = 3.5, shape = 16
    ) +
    # MD: hollow points (white fill, blue outline)
    geom_point(
      data = dplyr::filter(filtered_data, is_md),
      aes(y = acute_HSR, color = "HSR Agudo", text = tooltip_acute),
      size = 4, shape = 21, fill = "white", stroke = 1.5
    ) +
    
    # Chronic HSR
    geom_line(aes(y = chronic_HSR, color = "HSR Crónico"), linewidth = 2) +
    geom_point(aes(y = chronic_HSR, color = "HSR Crónico",
                   text = paste0("Jugador: ", player,
                                 "<br>Fecha: ", format(date, "%Y-%m-%d"),
                                 "<br>HSR Crónico: ", round(chronic_HSR, 1))),
               size = 3.5) +
    
    scale_x_date(date_breaks = "1 day", date_labels = "%d-%b") +
    # ensure room for the banner
    scale_y_continuous(limits = c(0, left_ylim)) +
    labs(
      title = paste("HSR (>21 km/h) Agudo vs. Crónico –", player),
      x = NULL, y = "HSR en metros", color = NULL,
      subtitle = "Valores de distancia de HSR en metros"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 16, margin = margin(r = 10)),
      plot.title = element_text(hjust = 0.5, size = 22, face = "bold", margin = margin(t = 35, b = 10)),
      plot.subtitle = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.margin = margin(t = 25, r = 20, b = 20, l = 20),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 13)
    ) +
    scale_color_manual(values = c(
      "HSR Agudo"   = "#0072B2",
      "HSR Crónico" = "#4D4D4D"
    ))
  
  # Red banner if last four MDs are all ≥ 80'
  if (high_comp_flag) {
    g <- g + annotate(
      "label",
      x = mean(range(filtered_data$date, na.rm = TRUE)),
      y = left_ylim * 0.98, vjust = 1,
      label = "Carga Alta de Competencia",
      fill = "#C62828", color = "white",
      fontface = "bold", size = 6.5,
      label.size = 0,
      label.padding = grid::unit(0.25, "lines")
    )
  }
  
  g
}

# Example
plot_individual_hsr("Rodrigo Dourado")

jugs = c("Israel Reyes","Henry Martín","Alejandro Zendejas",
         "Isaías Violante","Alan Cervantes","Ramón Juárez","Erick Sánchez",
         "Brian Rodríguez","Kevin Álvarez","Dagoberto Espinoza","Víctor Dávila",
         "Cristian Borja","Alexis Gutiérrez", "Sebastián Cáceres","Miguel Vázquez",
         "José Raúl Zúñiga", "Pato Salas", "Raphael Veiga", "Thiago Espinosa", 
         "Emilio Lara", "Franco Rossano", "Santiago Naveda", "Alejandro Cárdenas",
         "Adrián Fernández", "Guillermo Cortéz", "Ícaro da Conceicao",
         "Ricardo González", "Rodolfo Cota", "Luis Ángel Malagón", "Diego Arriaga",
         "Óscar Perea", "Edwin Cerrillo")

ACWR_MISSING_Y <- 0.65

# =========================
#   1. ACWR x RPE -----------
# =========================

# 1) Load RPE XLSX
path_rpe <- "data/rpe_primera.xlsx"
rpe_raw <- read_excel(path_rpe)

rpe_df <- rpe_raw |>
  rename(
    fecha_rpe = `Marca temporal`,
    # player    = `Nombre del Jugador`,
    player = `Columna 3`,
    rpe_txt   = `Valor`
  ) |>
  mutate(
    # FORCE types (fixes your error)
    player  = as.character(player),
    rpe_txt = as.character(rpe_txt),
    date    = as.Date(fecha_rpe),
    
    # clean whitespace/case a bit (optional but helps matching)
    player  = str_squish(player),
    
    player = case_when(
      # player == "Pato Salas" ~ "Patricio Salas",
      player == "Miguel  Vazquez" ~ "Miguel Vázquez",
      player == "Luis Ángel Malagón Velázquez" ~ "Luis Ángel Malagón",
      player == "Alexis Gutierrez" ~ "Alexis Gutiérrez",
      player == "Sebastian Cáceres" ~ "Sebastián Cáceres",
      player == "Isaias Violante" ~ "Isaías Violante",
      player == "Jose Zuniga" ~ "José Raúl Zúñiga",
      player == "kevin alvarez" ~ "Kevin Álvarez",
      player == "Erick Sanchez" ~ "Erick Sánchez",
      player == "Brian Rodriguez" ~ "Brian Rodríguez",
      player == "Victor Davila" ~ "Víctor Dávila",
      TRUE ~ player
    ),
    
    # RPE -> numeric
    rpe_val = case_when(
      str_detect(rpe_txt, "^1") ~ 1,
      str_detect(rpe_txt, "^2") ~ 2,
      str_detect(rpe_txt, "^3") ~ 3,
      str_detect(rpe_txt, "^4") ~ 4,
      str_detect(rpe_txt, "^5") ~ 5,
      str_detect(rpe_txt, "^6") ~ 6,
      str_detect(rpe_txt, "^7") ~ 7,
      str_detect(rpe_txt, "^8") ~ 8,
      str_detect(rpe_txt, "^9") ~ 9,
      TRUE ~ suppressWarnings(as.numeric(rpe_txt))
    )
  ) |>
  filter(!is.na(player), !is.na(date), !is.na(rpe_val)) |>
  filter(rpe_val >= 1, rpe_val <= 10)

# 3) Ensure dates in micros_individual
micros_individual <- micros_individual |>
  mutate(date = as.Date(date))

# 4) Latest ACWR per player
latest_acwr_rpe <- micros_individual |>
  group_by(player) |>
  filter(date == max(date, na.rm = TRUE)) |>
  ungroup() |>
  select(player, ac_ratio, acwr_date = date)

# 5) Latest RPE per player (1 row per player, no ties)
latest_rpe <- rpe_df |>
  arrange(player, date) |>
  group_by(player) |>
  slice_max(order_by = date, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(
    player,
    rpe_val,
    rpe_date = date
  )

# 6) Join + statuses/colors (GK-safe: keep players without ACWR)
scatter_df_rpe <- latest_rpe |>
  # LEFT JOIN so GKs stay even if ac_ratio is NA
  left_join(latest_acwr_rpe, by = "player") |>
  mutate(
    # y position for plotting
    y_plot = if_else(is.na(ac_ratio), ACWR_MISSING_Y, ac_ratio),
    
    load_status = case_when(
      is.na(ac_ratio) ~ "Sin Catapult (sin ACWR)",
      ac_ratio < 0.8 ~ "Carga Baja",
      ac_ratio > 1.3 ~ "Carga Alta",
      TRUE ~ "Carga Óptima"
    ),
    
    rpe_status = case_when(
      rpe_val <= 4 ~ "RPE Bajo",
      rpe_val <= 6 ~ "RPE Medio",
      TRUE ~ "RPE Alto"
    ),
    
    # Color logic:
    # - If NO ACWR: color by RPE only
    # - Else: your combined logic
    color_status_rpe = case_when(
      is.na(ac_ratio) & rpe_val <= 4 ~ "green",
      is.na(ac_ratio) & rpe_val >= 7 ~ "red",
      is.na(ac_ratio) ~ "yellow",
      
      ac_ratio >= 0.8 & ac_ratio <= 1.3 & rpe_val <= 4 ~ "green",
      rpe_val >= 7 & (ac_ratio < 0.8 | ac_ratio > 1.3) ~ "red",
      TRUE ~ "yellow"
    )
  ) |>
  filter(!is.na(rpe_val))

# 7) Plot
acwr_rpe_scatter_plot <- ggplot(
  scatter_df_rpe,
  aes(
    x = rpe_val,
    y = y_plot,
    fill = color_status_rpe,
    text = paste0(
      "Jugador: ", player,
      "<br>Fecha (RPE): ", rpe_date,
      "<br>RPE: ", rpe_val, " (", rpe_status, ")",
      "<br>Fecha (ACWR): ", acwr_date,
      "<br>Índice de Carga (ACWR): ", round(ac_ratio, 2),
      "<br>Estatus de Carga: ", load_status
    ),
    customdata = player
  )
) +
  geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
  geom_point(shape = 21, size = 6, alpha = 0.9, color = "black", stroke = 0.7) +
  scale_x_continuous(breaks = 1:10) +
  scale_fill_manual(values = c(green = "#2ca02c", yellow = "#ffbf00", red = "#d62728")) +
  labs(
    x = "RPE de la sesión (1–10)",
    y = "Índice de Carga (ACWR)",
    title = "ACWR & RPE: Resumen del equipo (última respuesta)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  )

# Example:
# acwr_rpe_scatter_plot
# ggplotly(acwr_rpe_scatter_plot, tooltip = "text")

# ACWR x Rest Score Plot --------

recuperacion_df <- recuperacion_df |>
  mutate(
    date = as.Date(`Marca temporal`),
    rest_score = compute_rest_score(
      `Nivel de Cansancio hoy (Fatiga)`,
      `Qué tal descansaste ayer?`,
      `Cuántas horas dormiste ayer?`
    ),
    pain_score = compute_pain_score(
      `Estás adolorido de alguna parte?`
    )
  )

micros_individual <- micros_individual |>
  mutate(date = as.Date(date))

# --- Latest ACWR per player (unchanged) ---
latest_acwr <- micros_individual |>
  group_by(player) |>
  filter(date == max(date, na.rm = TRUE)) |>
  select(player, ac_ratio)

# --- Latest dates per player from wellness survey (unchanged) ---
latest_dates <- recuperacion_df |>
  group_by(Nombre) |>
  summarize(latest_date = max(date, na.rm = TRUE), .groups = "drop") |>
  rename(player = Nombre)

# =========================
#   2) ACWR x REST SCORE --------
# =========================
# Latest rest score per player
latest_rest <- recuperacion_df |>
  group_by(Nombre) |> 
  filter(Nombre %in% jugs) |>
  filter(date == max(date, na.rm = TRUE)) |>
  select(player = Nombre, rest_score)

rest_scatter_df <- latest_rest |>
  inner_join(latest_dates, by = "player") |>
  # LEFT JOIN so GKs stay
  left_join(latest_acwr, by = "player") |>
  mutate(
    y_plot = if_else(is.na(ac_ratio), ACWR_MISSING_Y, ac_ratio),
    
    rest_status = if_else(rest_score >= 6, "Descansado", "Cansado"),
    
    load_status = case_when(
      is.na(ac_ratio) ~ "Sin Catapult (sin ACWR)",
      ac_ratio < 0.8 ~ "Carga Baja",
      ac_ratio > 1.3 ~ "Carga Alta",
      TRUE ~ "Carga Óptima"
    ),
    
    # Color logic:
    # - If NO ACWR: color by rest only
    # - Else: your combined logic
    color_status_rest = case_when(
      is.na(ac_ratio) & rest_score >= 6 ~ "green",
      is.na(ac_ratio) & rest_score < 6  ~ "red",
      
      ac_ratio >= 0.8 & ac_ratio <= 1.3 & rest_score >= 6 ~ "green",
      (ac_ratio >= 0.8 & ac_ratio <= 1.3 & rest_score < 6) |
        (rest_score >= 6 & (ac_ratio < 0.8 | ac_ratio > 1.3)) ~ "yellow",
      TRUE ~ "red"
    )
  ) |>
  filter(!is.na(rest_score))

acwr_rest_scatter_plot <- ggplot(
  rest_scatter_df,
  aes(
    x = rest_score,
    y = y_plot,
    fill = color_status_rest,
    text = paste0(
      "Jugador: ", player,
      "<br>Fecha: ", latest_date,
      "<br>Score de Descanso: ", rest_score,
      "<br>Índice de Carga: ", round(ac_ratio, 2),
      "<br>Estatus de Descanso: ", rest_status,
      "<br>Estatus de Carga: ", load_status
    ),
    customdata = player
  )
) +
  geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
  # use a filled shape so 'fill' shows; keep black outline
  geom_point(shape = 21, size = 6, alpha = 0.9, color = "black", stroke = 0.7) +
  scale_fill_manual(values = c(green = "#2ca02c", yellow = "#ffbf00", red = "#d62728")) +
  labs(
    x = "Score de Descanso", 
    y = "Índice de Carga (ACWR)",
    title = "ACWR & Descanso: Resumen del equipo de hoy"
    ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank())

# =========================
#   3) ACWR x PAIN SCORE -----------
# =========================
# Latest pain score per player
latest_pain2 <- recuperacion_df |>
  mutate(date = as.Date(`Marca temporal`)) |>
  filter(Nombre %in% jugs) |>
  arrange(Nombre, date) |>
  group_by(Nombre) |>
  mutate(
    soreness_resp  = `Estás adolorido de alguna parte?`,
    zona_adolorida = `Donde te encuentras adolorido? Indica cada zona de dolor`,
    
    # 1) define "pain day" using your two OR conditions
    pain_day =
      (soreness_resp %in% c("Adolorido de una zona", "Muy adolorido en general")) |
      (!is.na(zona_adolorida) & zona_adolorida != "Nada"),
    
    # 2) 3-day streak based on the new pain_day
    three_day_pain =
      pain_day &
      lag(pain_day, 1, default = FALSE) &
      lag(pain_day, 2, default = FALSE)
  ) |>
  # keep only ONE latest row per player
  slice_max(order_by = date, n = 1, with_ties = FALSE) |>
  transmute(
    player = Nombre,
    soreness_resp,
    zona_adolorida,
    pain_flag  = three_day_pain,
    pain_score = pain_score
  )

pain_scatter_df <- latest_pain2 |>
  # bring date (for tooltip) from latest_dates
  left_join(latest_dates, by = "player") |>
  # LEFT JOIN so GKs stay
  left_join(latest_acwr, by = "player") |>
  mutate(
    y_plot = if_else(is.na(ac_ratio), ACWR_MISSING_Y, ac_ratio),
    
    pain_status = if_else(pain_score < 6, "Sin dolor", "Con dolor"),
    
    load_status = case_when(
      is.na(ac_ratio) ~ "Sin Catapult (sin ACWR)",
      ac_ratio < 0.8 ~ "Carga Baja",
      ac_ratio > 1.3 ~ "Carga Alta",
      TRUE ~ "Carga Óptima"
    ),
    
    # Color logic:
    # - If NO ACWR: color by pain only (match your meaning)
    #   pain_score < 6 = green, else red
    # - Else: your combined logic
    color_status_pain = case_when(
      is.na(ac_ratio) & pain_score < 6 ~ "green",
      is.na(ac_ratio) & pain_score >= 6 ~ "red",
      
      ac_ratio >= 0.8 & ac_ratio <= 1.3 & pain_score < 6 ~ "green",
      (ac_ratio >= 0.8 & ac_ratio <= 1.3 & pain_score >= 6) |
        (pain_score < 6 & (ac_ratio < 0.8 | ac_ratio > 1.3)) ~ "yellow",
      TRUE ~ "red"
    )
  ) |>
  filter(!is.na(pain_score))

acwr_pain_scatter_plot <- ggplot(
  pain_scatter_df,
  aes(
    x = pain_score,
    y = y_plot,
    fill = color_status_pain,
    text = paste0(
      "Jugador: ", player,
      "<br>Fecha: ", latest_date,
      "<br>Score de Dolor Muscular: ", pain_score,
      "<br>Índice de Carga: ", round(ac_ratio, 2),
      "<br>Estatus de Dolor Muscular: ", pain_status,
      "<br>Estatus de Carga: ", load_status,
      ifelse(pain_flag, paste0("<br>Zona Adolorida: ", zona_adolorida), "")
      
    ),
    customdata = player
  )
) +
  geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
  geom_point(shape = 21, size = 6, alpha = 0.9, color = "black", stroke = 0.7) +
  # anillo rojo para jugadores con dolor
  geom_point(
    data = dplyr::filter(pain_scatter_df, pain_flag),
    aes(x = pain_score, y = y_plot),
    inherit.aes = FALSE,
    shape = 21, size = 9, stroke = 1.8, fill = NA, color = "#d62728"
  ) +
  scale_fill_manual(values = c(green = "#2ca02c", yellow = "#ffbf00", red = "#d62728")) +
  labs(
    x = "Score de Dolor Muscular", 
    y = "Índice de Carga (ACWR)",
    title = "ACWR & Dolor Muscular: Resumen del equipo de hoy"
    ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()
)

# 4. Scatter de ACWR & Recuperación -------

# Asegurarse que las fechas están en formato Date
recuperacion_df <- recuperacion_df |>
  mutate(date = as.Date(`Marca temporal`))

# # === FIX: compute 3-day streak, THEN filter to latest date (same as the correct chunk) ===
# latest_pain <- recuperacion_df |>
#   filter(Nombre %in% jugs) |>
#   arrange(Nombre, date) |>
#   group_by(Nombre) |>
#   mutate(
#     zona_adolorida = `Donde te encuentras adolorido? Indica cada zona de dolor`,
#     pain_day = !is.na(zona_adolorida) & zona_adolorida != "Nada",
#     three_day_pain = pain_day &
#       lag(pain_day, 1, default = FALSE) &
#       lag(pain_day, 2, default = FALSE)
#   ) |>
#   filter(date == max(date, na.rm = TRUE)) |>
#   transmute(
#     player = Nombre,
#     zona_adolorida,
#     pain_flag = three_day_pain
#   )

# Autoridad para los anillos: usar únicamente latest_pain2 ya calculado en el bloque de Dolor
rings_auth <- latest_pain2 |>
  dplyr::filter(pain_flag) |>
  dplyr::distinct(player, .keep_all = TRUE) |>
  dplyr::select(player, soreness_resp, zona_adolorida, pain_flag)

# Asegurar fecha en micros
micros_individual <- micros_individual |>
  dplyr::mutate(date = as.Date(date))

# Último ACWR por jugador
latest_acwr <- micros_individual |>
  dplyr::group_by(player) |>
  dplyr::filter(date == max(date, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::select(player, ac_ratio)

# Último recovery score por jugador
latest_recovery <- recuperacion_df |>
  dplyr::group_by(Nombre) |>
  dplyr::filter(date == max(date, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::select(player = Nombre, recovery_score)

# Última fecha por jugador
latest_dates <- recuperacion_df |>
  dplyr::group_by(Nombre) |>
  dplyr::summarize(latest_date = max(date, na.rm = TRUE), .groups = "drop") |>
  dplyr::rename(player = Nombre)

# Data frame para el scatter de Recuperación x ACWR
scatter_df <- latest_recovery |>
  inner_join(latest_dates, by = "player") |>
  # LEFT JOIN so GKs stay even if ACWR missing
  left_join(latest_acwr, by = "player") |>
  left_join(rings_auth, by = "player") |>
  mutate(
    pain_flag = tidyr::replace_na(pain_flag, FALSE),
    
    y_plot = if_else(is.na(ac_ratio), ACWR_MISSING_Y, ac_ratio),
    
    recovery_status = dplyr::if_else(recovery_score >= 6, "Recuperado", "Fatigado"),
    
    load_status = dplyr::case_when(
      is.na(ac_ratio) ~ "Sin Catapult (sin ACWR)",
      ac_ratio < 0.8 ~ "Carga Baja",
      ac_ratio > 1.3 ~ "Carga Alta",
      TRUE ~ "Carga Óptima"
    ),
    
    # Color logic:
    # - If NO ACWR: color by recovery only
    # - Else: your combined logic
    color_status = dplyr::case_when(
      is.na(ac_ratio) & recovery_score >= 6 ~ "green",
      is.na(ac_ratio) & recovery_score < 6  ~ "red",
      
      ac_ratio >= 0.8 & ac_ratio <= 1.3 & recovery_score >= 6 ~ "green",
      (ac_ratio >= 0.8 & ac_ratio <= 1.3 & recovery_score < 6) |
        (recovery_score >= 6 & (ac_ratio < 0.8 | ac_ratio > 1.3)) ~ "yellow",
      TRUE ~ "red"
    )
  ) |>
  dplyr::filter(!is.na(recovery_score)) |>
  dplyr::distinct(player, .keep_all = TRUE)

# Un anillo rojo por jugador con bandera TRUE
rings_df <- scatter_df |>
  dplyr::filter(pain_flag) |>
  dplyr::distinct(player, .keep_all = TRUE)

acwr_scatter_plot <- ggplot(
  scatter_df,
  aes(
    x = recovery_score,
    y = y_plot,
    fill = color_status,
    text = paste0(
      "Jugador: ", player,
      "<br>Fecha: ", latest_date,
      "<br>Score de Recuperación: ", recovery_score,
      "<br>Índice de Carga: ", round(ac_ratio, 2),
      "<br>Estatus de Recuperación: ", recovery_status,
      "<br>Estatus de Carga: ", load_status,
      ifelse(pain_flag, paste0("<br>Zona Adolorida: ", zona_adolorida), "")
    )
  )
) +
  geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
  geom_point(shape = 21, size = 6, alpha = 0.95, color = "black") +
  # ring layer uses the deduped data
  geom_point(
    data = rings_df,
    aes(x = recovery_score, y = y_plot),
    inherit.aes = FALSE,
    shape = 21, size = 9, stroke = 1.8, fill = NA, color = "#d62728"
  ) +
  scale_fill_manual(values = c(green = "#2ca02c", yellow = "#ffbf00", red = "#d62728")) +
  labs(
    x = "Score de Recuperación",
    y = "Índice de Carga (ACWR)",
    title = "ACWR & Recuperación: Resumen del equipo de hoy"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  )


# =============================================================================
# AI Chat Functions
# =============================================================================

call_claude_api <- function(prompt, max_tokens = 600) {
  api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (nchar(trimws(api_key)) == 0)
    return("Error: la variable ANTHROPIC_API_KEY no está configurada.")

  tryCatch({
    resp <- httr::POST(
      url  = "https://api.anthropic.com/v1/messages",
      httr::add_headers(
        "x-api-key"         = api_key,
        "anthropic-version" = "2023-06-01",
        "content-type"      = "application/json"
      ),
      body = jsonlite::toJSON(list(
        model      = "claude-haiku-4-5-20251001",
        max_tokens = max_tokens,
        messages   = list(list(role = "user", content = prompt))
      ), auto_unbox = TRUE),
      encode = "raw"
    )
    if (httr::status_code(resp) != 200) {
      err <- httr::content(resp, as = "parsed")
      return(paste0("Error de API (HTTP ", httr::status_code(resp), "): ", err$error$message))
    }
    parsed <- httr::content(resp, as = "parsed", encoding = "UTF-8")
    parsed$content[[1]]$text
  }, error = function(e) paste0("Error de conexión: ", conditionMessage(e)))
}

build_narrative_prompt <- function(scatter_data, micros_data, recuperacion_data, md_phase) {
  md_label <- switch(md_phase,
    "MD" = "MD (día de partido)", "No" = "sin fase MD",
    ">-5" = "MD >-5", paste0("MD", md_phase)
  )

  n_green  <- sum(scatter_data$color_status == "green",  na.rm = TRUE)
  n_yellow <- sum(scatter_data$color_status == "yellow", na.rm = TRUE)
  n_red    <- sum(scatter_data$color_status == "red",    na.rm = TRUE)

  fmt_players <- function(df, val_col, fmt = "%.2f") {
    if (nrow(df) == 0) return("Ninguno.")
    paste(sprintf("  %s: %s", df$player, sprintf(fmt, df[[val_col]])), collapse = "\n")
  }

  high_acwr <- dplyr::filter(scatter_data, !is.na(ac_ratio), ac_ratio > 1.3)
  low_acwr  <- dplyr::filter(scatter_data, !is.na(ac_ratio), ac_ratio < 0.8)
  fatigados <- dplyr::filter(scatter_data, recovery_score < 6)
  doloridos <- dplyr::filter(scatter_data, pain_flag == TRUE)

  pain_lines <- if (nrow(doloridos) > 0)
    paste(sprintf("  %s%s", doloridos$player,
      ifelse(!is.na(doloridos$zona_adolorida),
             paste0(" (", doloridos$zona_adolorida, ")"), "")), collapse = "\n")
  else "Ninguno."

  context <- paste0(
    "Fase del microciclo (última sesión): ", md_label, "\n\n",
    "Estado del equipo hoy:\n",
    "  Verde  (todo OK):  ", n_green,  " jugadores\n",
    "  Amarillo (alerta): ", n_yellow, " jugadores\n",
    "  Rojo   (problema): ", n_red,    " jugadores\n\n",
    "ACWR alto (>1.3):\n",    fmt_players(high_acwr, "ac_ratio"), "\n\n",
    "ACWR bajo (<0.8):\n",    fmt_players(low_acwr,  "ac_ratio"), "\n\n",
    "Score recuperación bajo (<6):\n", fmt_players(fatigados, "recovery_score", "%.1f"), "\n\n",
    "Dolor muscular reportado:\n", pain_lines, "\n"
  )

  paste0(
    "Eres un analista de ciencias del deporte del Club América.\n",
    "Escribe UN solo párrafo de 4-5 oraciones en español resumiendo el estado del equipo.\n",
    "Menciona jugadores por nombre cuando sea relevante e incluye valores numéricos exactos.\n",
    "REGLAS:\n",
    "- No menciones aspectos tácticos ni decisiones de entrenador.\n",
    "- No hagas sugerencias, recomendaciones ni predicciones.\n",
    "- No uses viñetas. No pongas título. Solo el párrafo.\n\n",
    "DATOS:\n", context
  )
}

build_nl_prompt <- function(scatter_data, micros_raw, micros_data, recuperacion_data, md_phase, question) {
  md_label <- switch(md_phase,
    "MD" = "MD", "No" = "No MD", ">-5" = "MD >-5", paste0("MD", md_phase)
  )

  # --- Current team snapshot ---
  team_rows <- dplyr::arrange(scatter_data, player) |>
    dplyr::mutate(
      acwr_str = ifelse(is.na(ac_ratio), "Sin Catapult", sprintf("%.2f", ac_ratio)),
      pain_str = dplyr::case_when(
        pain_flag & !is.na(zona_adolorida) ~ paste0("Sí (", zona_adolorida, ")"),
        pain_flag ~ "Sí",
        TRUE ~ "No"
      )
    )
  team_block <- paste(
    sprintf("%-26s | Rec: %4.1f | %-11s | ACWR: %-7s | Carga: %-20s | Dolor: %s",
            team_rows$player, team_rows$recovery_score,
            team_rows$recovery_status, team_rows$acwr_str,
            team_rows$load_status, team_rows$pain_str),
    collapse = "\n"
  )

  # --- Catapult ranked totals (7d, prev 7d, 30d) ---
  ref_catapult   <- max(micros_raw$date, na.rm = TRUE)
  start_30d  <- ref_catapult - 29
  start_7d   <- ref_catapult - 6
  start_prev <- ref_catapult - 13

  agg_catapult <- function(start, end) {
    micros_raw |>
      dplyr::filter(date >= start, date <= end) |>
      dplyr::group_by(player) |>
      dplyr::summarise(
        dist = sum(distance_m,   na.rm = TRUE),
        hsr  = sum(HSR_abs_dist, na.rm = TRUE),
        pl   = sum(player_load,  na.rm = TRUE),
        vmax = max(max_speed,    na.rm = TRUE),
        .groups = "drop"
      )
  }

  rank_block <- function(df, col, title, fmt) {
    sorted <- dplyr::arrange(df, dplyr::desc(.data[[col]]))
    lines  <- sprintf("%2d. %-24s — %s", seq_len(nrow(sorted)), sorted$player, fmt(sorted[[col]]))
    paste0(title, ":\n", paste(lines, collapse = "\n"))
  }
  m <- function(x) paste0(round(x, 0), " m")
  v <- function(x) paste0(round(x, 1), " km/h")
  p <- function(x) as.character(round(x, 0))

  make_catapult_rankings <- function(df, label) {
    paste0("=== ", label, " ===\n",
           paste(rank_block(df, "dist", "DISTANCIA TOTAL", m),
                 rank_block(df, "hsr",  "HSR (>21 km/h)",  m),
                 rank_block(df, "pl",   "PLAYER LOAD",      p),
                 rank_block(df, "vmax", "VELOCIDAD MÁXIMA", v),
                 sep = "\n\n"))
  }

  catapult_rankings <- paste(
    make_catapult_rankings(agg_catapult(start_7d,   ref_catapult),
                       paste0("Últimos 7 días (", format(start_7d, "%d/%m"), "–", format(ref_catapult, "%d/%m/%Y"), ")")),
    make_catapult_rankings(agg_catapult(start_prev, ref_catapult - 7),
                       paste0("Semana anterior (", format(start_prev, "%d/%m"), "–", format(ref_catapult - 7, "%d/%m/%Y"), ")")),
    make_catapult_rankings(agg_catapult(start_30d,  ref_catapult),
                       paste0("Últimos 30 días (", format(start_30d, "%d/%m"), "–", format(ref_catapult, "%d/%m/%Y"), ")")),
    sep = "\n\n"
  )

  # --- Catapult daily detail (30d, 1 row per player per day) ---
  catapult_daily <- micros_raw |>
    dplyr::filter(date >= start_30d) |>
    dplyr::group_by(date, player, match_day) |>
    dplyr::summarise(
      dist = sum(distance_m,   na.rm = TRUE),
      hsr  = sum(HSR_abs_dist, na.rm = TRUE),
      pl   = sum(player_load,  na.rm = TRUE),
      vmax = max(max_speed,    na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(date, player) |>
    dplyr::mutate(row = sprintf(
      "%s | %-24s | Dist: %5.0fm | HSR: %4.0fm | PL: %5.0f | Vmax: %.1f km/h | %s",
      format(date, "%d/%m/%Y"), player, dist, hsr, pl, vmax, match_day
    )) |>
    dplyr::pull(row)

  # --- A:C (EWMA) daily detail (30d) ---
  ref_ac   <- max(micros_data$date, na.rm = TRUE)
  ac_daily <- micros_data |>
    dplyr::filter(date >= ref_ac - 29) |>
    dplyr::arrange(player, date) |>
    dplyr::mutate(row = sprintf(
      "%s | %-24s | Aguda: %6.1f | Crónica: %6.1f | A:C: %.2f%s",
      date, player, carga_aguda, carga_cronica, ac_ratio,
      ifelse(is_md, " [MD]", "")
    )) |>
    dplyr::pull(row)

  # --- Wellness daily detail (30d) ---
  ref_rec   <- max(recuperacion_data$`Marca temporal`, na.rm = TRUE)
  rec_daily <- recuperacion_data |>
    dplyr::filter(`Marca temporal` >= ref_rec - 29) |>
    dplyr::select(Nombre, `Marca temporal`, fatiga_score, sueño_score, dolor_score, recovery_score) |>
    dplyr::arrange(`Marca temporal`, Nombre) |>
    dplyr::mutate(row = sprintf(
      "%s | %-24s | Fatiga: %4.1f | Sueño: %4.1f | Dolor: %4.1f | Rec: %4.1f",
      `Marca temporal`, Nombre, fatiga_score, sueño_score, dolor_score, recovery_score
    )) |>
    dplyr::pull(row)

  # --- Recent match dates ---
  md_dates_str <- micros_raw |>
    dplyr::filter(match_day == "MD") |>
    dplyr::distinct(date) |>
    dplyr::arrange(dplyr::desc(date)) |>
    dplyr::slice_head(n = 5) |>
    dplyr::pull(date) |>
    format("%d/%m/%Y") |>
    paste(collapse = ", ")

  paste0(
    "Eres un analista de ciencias del deporte del Club América. ",
    "Para totales y rankings usa EXCLUSIVAMENTE los TOTALES PRE-CALCULADOS. ",
    "NUNCA sumes el detalle diario para obtener totales. ",
    "El detalle diario es solo para preguntas sobre fechas específicas. ",
    "Responde en español con valores numéricos exactos.\n\n",
    "Fase del microciclo (última sesión): ", md_label, "\n",
    "Partidos recientes (MD): ", if (nchar(md_dates_str) == 0) "ninguno" else md_dates_str, "\n\n",
    "=== ESTADO ACTUAL DEL EQUIPO ===\n", team_block, "\n\n",
    "=== TOTALES CATAPULT PRE-CALCULADOS (usa para rankings y totales) ===\n", catapult_rankings, "\n\n",
    "=== DETALLE DIARIO CATAPULT — 1 fila por jugador por día (últimos 30 días) ===\n",
    paste(catapult_daily, collapse = "\n"), "\n\n",
    "=== DETALLE DIARIO A:C (EWMA) — últimos 30 días ===\n",
    paste(ac_daily, collapse = "\n"), "\n\n",
    "=== DETALLE DIARIO BIENESTAR — últimos 30 días ===\n",
    paste(rec_daily, collapse = "\n"), "\n\n",
    "PREGUNTA: ", question
  )
}

