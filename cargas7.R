# Load necessary libraries
library(tidyverse)
library(readxl)
library(gt)

# Leer datos ----

# datos <- read_xlsx("data/Sessions_micro01.xlsx")

datos <- read_excel(file.path("data", "Sessions_micro01.xlsx"))
datos$date <- as.Date(datos$date)

datos <-  datos |>
  mutate(player = case_when(
    player == "kevin alvarez" ~ "Kevin Álvarez",
    player == "Erick Sanchez" ~ "Erick Sánchez",
    player == "Brian Rodriguez" ~ "Brian Rodríguez",
    player == "Victor Davila" ~ "Víctor Dávila",
    player == "Miguel Ramirez" ~ "Miguel Ramírez",
    player == "Miguel  Vazquez" ~ "Miguel Vázquez",
    player == "Nestor Araujo" ~ "Néstor Araujo",
    # player == "Fidalgo Fidalgo" ~ "Álvaro Fidalgo",
    player == "Jona Dos Santos" ~ "Jonathan Dos Santos",
    player == "Luis Ángel Malagón Velázquez" ~ "Luis Ángel Malagón",
    player == "Alexis Gutierrez" ~ "Alexis Gutiérrez",
    player == "Sebastian Cáceres" ~ "Sebastián Cáceres",
    player == "Isaias Violante" ~ "Isaías Violante",
    player == "Jose Zuniga" ~ "José Raúl Zúñiga",
    # player == "Allan Maximin" ~ "Allan Saint-Maximin",
    player == "Patricio Salas" ~ "Patricio Salas",
    player == "Rodrigo Dourado" ~ "Rodrigo Dourado",
    player == "Aaron Mejia" ~ "Aarón Mejía",
    player == "Thiago Espinosa" ~ "Thiago Espinosa",
    player == "Vinicius Lima" ~ "Vinícius Lima",
    TRUE ~ player
  ),
  date = as.Date(date))

# Velocidades Máximas Jugadores ----

vel_max_lookup <- tibble::tribble(
  ~player,                ~vel_max_hist,
  "Aarón Mejía",               35.51,
  "Alan Cervantes",            33.55,
  "Alejandro Zendejas",        34.00,
  "Alexis Gutiérrez",          33.57,
  # "Allan Saint-Maximin",       35.39,
  "Brian Rodríguez",           35.60,
  "Cristian Borja",            34.00,
  "Dagoberto Espinoza",        35.58,
  "Erick Sánchez",             33.05,
  "Henry Martín",              33.71,
  "Igor Lichnovsky",           34.00,
  "Isaías Violante",           35.00,
  "Israel Reyes",              33.00,
  "Jonathan Dos Santos",       31.47,
  "José Raúl Zúñiga",          34.93,
  "Kevin Álvarez",             35.00,
  "Miguel Vázquez",            34.25,
  "Néstor Araujo",             33.00,
  "Patricio Salas",            35.00,
  "Ramón Juárez",              33.00,
  # "Rodrigo Aguirre",           34.45,
  "Rodrigo Dourado",           30.00,
  "Santiago Naveda",           32.45,
  "Sebastián Cáceres",         35.00,
  "Víctor Dávila",             34.00,
  "Raphael Veiga",             32.00,
  "Vinícius Lima",             31.66,
  "Thiago Espinosa",           34.00
  # "Álvaro Fidalgo",            33.50
)

selected_players <- c("Aarón Mejía", "Alan Cervantes", "Alejandro Zendejas", "Alexis Gutiérrez", "Brian Rodríguez",           
                   "Cristian Borja", "Dagoberto Espinoza", "Erick Sánchez", "Henry Martín", "Isaías Violante",           
                   "Israel Reyes", "Jonathan Dos Santos", "José Raúl Zúñiga", "Kevin Álvarez", "Miguel Vázquez",      
                   "Néstor Araujo", "Patricio Salas", "Ramón Juárez", "Rodrigo Dourado", "Santiago Naveda",           
                   "Sebastián Cáceres", "Víctor Dávila", "Raphael Veiga", "Vinícius Lima", "Thiago Espinosa")

datos <- datos |>
  left_join(vel_max_lookup, by = "player") |>
  filter(player %in% selected_players) |>
  filter(player != "Luis Ángel Malagón")

# -------------------------------------------------------
# Últimos 7 días calendario (no sesiones) ----
# -------------------------------------------------------

end_date_7d   <- max(datos$date, na.rm = TRUE)
start_date_7d <- end_date_7d - days(6)   # 7 días inclusive

last7 <- datos |>
  filter(date >= start_date_7d, date <= end_date_7d) |>
  group_by(player) |>
  summarise(
    HSR_abs_dist_7d     = sum(HSR_abs_dist,     na.rm = TRUE),
    distance_abs_7d     = sum(distance_abs,     na.rm = TRUE),
    sprints_abs_count_7d = sum(sprints_abs_count, na.rm = TRUE),
    .groups = "drop"
  )
