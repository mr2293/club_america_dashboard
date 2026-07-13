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
    player == "Jona Dos Santos" ~ "Jonathan Dos Santos",
    player == "Luis Ángel Malagón Velázquez" ~ "Luis Ángel Malagón",
    player == "Alexis Gutierrez" ~ "Alexis Gutiérrez",
    player == "Sebastian Cáceres" ~ "Sebastián Cáceres",
    player == "Isaias Violante" ~ "Isaías Violante",
    player == "Jose Zuniga" ~ "José Raúl Zúñiga",
    player == "Patricio Salas" ~ "Patricio Salas",
    player == "Rodrigo Dourado" ~ "Rodrigo Dourado",
    player == "Thiago Espinosa" ~ "Thiago Espinosa",
    player == "Dago Espinoza" ~ "Dago Espinoza",
    player == "Ricardo Gonzalez" ~ "Ricardo González",
    player == "Guillermo Cortes" ~ "Guillermo Cortés",
    player == "Adrian Fernandez" ~ "Adrián Fernández",
    player == "Diego Arriaga" ~ "Diego Arriaga",
    TRUE ~ player
  ),
  date = as.Date(date))

# Velocidades Máximas Jugadores ----

vel_max_lookup <- tibble::tribble(
  ~player,                ~vel_max_hist,
  "Alan Cervantes",            33.55,
  "Alejandro Zendejas",        34.00,
  "Alexis Gutiérrez",          33.57,
  "Brian Rodríguez",           35.60,
  "Cristian Borja",            34.00,
  "Dagoberto Espinoza",        35.58,
  "Erick Sánchez",             33.05,
  "Henry Martín",              33.71,
  "Isaías Violante",           35.00,
  "Israel Reyes",              33.00,
  "José Raúl Zúñiga",          34.93,
  "Kevin Álvarez",             35.00,
  "Miguel Vázquez",            34.25,
  "Patricio Salas",            35.00,
  "Ramón Juárez",              33.00,
  "Sebastián Cáceres",         35.00,
  "Víctor Dávila",             34.00,
  "Raphael Veiga",             32.00,
  "Thiago Espinosa",           34.00,
  "Franco Rossano",            33.00,
  "Emilio Lara",               33.00,
  "Dago Espinoza",             33.00,
  "Ricardo González",          33.00,
  "Guillermo Cortés",          33.00,
  "Adrián Fernández",          33.00,
  "Diego Arriaga",             33.00
)

selected_players <- c("Alan Cervantes", "Alejandro Zendejas", "Alexis Gutiérrez", "Brian Rodríguez",           
                   "Cristian Borja", "Dagoberto Espinoza", "Erick Sánchez", "Henry Martín", "Isaías Violante",           
                   "Israel Reyes", "José Raúl Zúñiga", "Kevin Álvarez", "Miguel Vázquez", "Patricio Salas", 
                   "Ramón Juárez","Sebastián Cáceres", "Víctor Dávila", "Raphael Veiga", "Thiago Espinosa",
                   "Franco Rossano", "Emilio Lara", "Dago Espinoza", "Ricardo González", "Adrián Fernández",
                   "Guillermo Cortés", "Diego Arriaga")

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
    sprint_dist_7d     = sum(sprint_dist,     na.rm = TRUE),
    sprint_count_95pct_7d = sum(sprint_count_95pct, na.rm = TRUE),
    .groups = "drop"
  )

jugs <- datos |>
  distinct(player)
