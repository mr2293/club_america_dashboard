# Shiny module wrapper around this app's original app.R, so it can be
# mounted as one tab inside the combined club_america_dashboard app
# (see /app.R at the repo root) instead of running as its own website.
#
# This file intentionally does NOT replace app.R -- app.R still works
# standalone (e.g. for local dev or a fallback deploy of this app alone).
# The module wrapper sources dashboard.R itself (guarded so it only runs
# once even if multiple modules end up in the same R session) and renames
# a few objects that collided with cargas_fisicas_7's globals once both
# apps' code loads into one process: `jugs` -> `dc_jugs`,
# `selected_players` -> `dc_selected_players`, and the AI helper functions
# `call_claude_api` / `build_narrative_prompt` / `build_nl_prompt` get a
# `dc_` prefix (cargas_fisicas_7 defines its own same-named versions).

DASHBOARD_CARGAS_DIR <- "apps/dashboard_cargas"

if (!exists(".dashboard_cargas_globals_loaded", inherits = TRUE)) {
  .dashboard_cargas_globals_loaded <- TRUE

  if (file.exists(file.path(DASHBOARD_CARGAS_DIR, "secrets.R"))) {
    source(file.path(DASHBOARD_CARGAS_DIR, "secrets.R"))
  }

  # dashboard.R uses relative paths ("data/...", "micros/..."); run it with
  # the app's own subdirectory as the working directory, then restore.
  .dc_prev_wd <- getwd()
  setwd(DASHBOARD_CARGAS_DIR)
  tryCatch(
    source("dashboard.R", local = FALSE, encoding = "UTF-8"),
    error = function(e) stop("Failed to source dashboard.R: ", conditionMessage(e)),
    finally = setwd(.dc_prev_wd)
  )

  if (!exists("ACWR_MISSING_Y", inherits = TRUE)) {
    ACWR_MISSING_Y <- 0.65
  }

  # --- collision-avoidance renames (see file header) ---
  dc_jugs <- jugs
  dc_selected_players <- selected_players
  dc_call_claude_api <- call_claude_api
  dc_build_narrative_prompt <- build_narrative_prompt
  dc_build_nl_prompt <- build_nl_prompt
}

dc_player_info <- tibble::tibble(
  player = c("Brian Rodríguez", "Sebastián Cáceres", "Alan Cervantes",
             "Rodolfo Cota", "Erick Sánchez", "Henry Martín", "Israel Reyes",
             "Kevin Álvarez", "Luis Ángel Malagón", "Miguel Vázquez",
             "Ramón Juárez", "Alejandro Zendejas", "Cristian Borja",
             "Dagoberto Espinoza", "Víctor Dávila", "Alexis Gutiérrez", "Isaías Violante",
             "José Raúl Zúñiga", "Patricio Salas", "Raphael Veiga", "Fernando Tapia",
             "Emilio Lara", "Franco Rossano", "Santiago Naveda",
             "Alejandro Cárdenas", "Adrián Fernández", "Guillermo Cortéz", "Ícaro da Conceicao",
             "Ricardo González", "Diego Arriaga", "Óscar Perea", "Edwin Cerrillo"
             ),
  image = c("brian.jpg", "caceres.jpg", "cervantes.jpg", "cota.jpg",
            "erick_sanchez.jpg", "henry.jpg", "israel_reyes.jpg",
            "kevin.jpg", "malagon.jpg", "miguel_vazquez.jpg", "ramon.jpg",
            "zendejas.jpg", "borja.jpg", "dagoberto.jpg", "davila.jpg",
            "alexis_gtz.jpg", "violante.jpg", "zuniga.jpg", "pato_salas.jpg", "veiga.jpg",
            "tapia.jpg", "pelon.jpg", "rossano.jpg", "naveda.jpg",
            "coco.jpg", "chiquis.jpg", "cortez.jpg", "icaro.jpg", "rica.jpg", "arriaga.jpg",
            "perea.webp", "cerrillo.webp"
            ),
  age = c("20/05/2000", "18/08/1999", "17/01/1998", "03/07/1987", "27/09/1999",
          "18/11/1992", "23/05/2000", "15/01/1999", "02/03/1997", "07/02/2004", "09/05/2001",
          "07/02/1998", "18/02/1993", "17/04/2004", "04/11/1997",
          "26/02/2000", "20/10/2003", "13/07/1994", "17/02/2004",
          "19/06/1995", "17/06/2001",
          "18/05/2002", "27/07/2005", "16/04/2001", "28/07/2006", "05/05/2006",
          "17/02/2007", "23/05/2007", "14/04/2009", "30/04/2004", "27/09/2005",
          "03/10/2000"
          ),
  height = c("1.75 m", "1.80 m", "1.81 m", "1.83 m", "1.67 m",
            "1.77 m", "1.79 m", "1.76 m",
            "1.82 m", "1.85 m", "1.82 m", "1.70 m", "1.79 m", "1.80 m",
            "1.73 m", "1.75 m", "1.73 m", "1.80 m", "1.85 m", "1.76m",
            "1.85 m", "1.87m", "1.79m", "1.78m", "1.89m", "1.69m",
            "1.72m", "1.75m", "1.72m", "1.74m", "1.74m", "1.75m"
            )
)

.meses_es_dc <- c("enero","febrero","marzo","abril","mayo","junio",
                   "julio","agosto","septiembre","octubre","noviembre","diciembre")
.dias_es_dc  <- c("domingo","lunes","martes","miércoles","jueves","viernes","sábado")
.cap_dc      <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
.today_es_dc <- paste0(
  .cap_dc(.dias_es_dc[as.integer(format(Sys.Date(), "%w")) + 1L]), " ",
  format(Sys.Date(), "%d"), " de ",
  .cap_dc(.meses_es_dc[as.integer(format(Sys.Date(), "%m"))]), " de ",
  format(Sys.Date(), "%Y")
)
.phase_dc    <- if (exists("current_md_phase")) current_md_phase else "—"
.md_label_dc <- switch(.phase_dc,
  "MD"    = "MD",
  "No"    = "No MD",
  "Rehab" = "Rehabilitación",
  ">-5"   = "MD >-5",
  "Other" = "Entrenamiento",
  "NA"    = "—",
  .phase_dc
)

mod_dashboard_cargas_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML(paste0(
      "#", ns("btn_narrative"), " { color: #0B1B4A !important; font-weight: bold !important; }\n",
      "#", ns("btn_query"),     " { color: #C1121F !important; font-weight: bold !important; }\n"
    ))),

    tags$div(
      style = paste0(
        "background: linear-gradient(135deg, #1a237e 0%, #1565c0 100%);",
        "color: white; padding: 10px 24px; margin-bottom: 16px;",
        "border-radius: 6px; display: flex; justify-content: space-between; align-items: center;"
      ),
      tags$span(.today_es_dc, style = "font-size: 15px; opacity: 0.85;"),
      tags$span(paste0("Entreno de ayer: ", .md_label_dc), style = "font-size: 17px; font-weight: bold;")
    ),

    fluidRow(
      column(
        10,
        tabsetPanel(
          id = ns("team_top_tabs"),
          tabPanel("Recuperación",
                   plotlyOutput(ns("acwr_scatter"), height = "500px")),
          tabPanel("Descanso",
                   plotlyOutput(ns("acwr_rest_scatter"), height = "500px")),
          tabPanel("Dolor Muscular",
                   plotlyOutput(ns("acwr_pain_scatter"), height = "500px")),
          tabPanel("RPE",
                   plotlyOutput(ns("acwr_rpe_scatter"), height = "500px")),
          tabPanel("Análisis IA",
            tags$div(style = "padding: 20px 8px 8px 8px;",
              fluidRow(
                column(5,
                  tags$div(style = "padding: 16px; border: 1px solid #dee2e6; border-radius:6px;",
                    tags$h4("Narrativa del equipo", style = "font-weight:700; color:#1565c0; margin-top:0;"),
                    tags$p("Resumen automático del estado de recuperación y carga del equipo.",
                           style = "color:#6b7280; font-size:13px;"),
                    actionButton(ns("btn_narrative"), "Generar Narrativa",
                                 style = paste0("background-color:#1565c0; color:white; font-weight:bold;",
                                                " border:none; border-radius:4px; padding:8px 18px;")),
                    tags$div(
                      style = paste0("margin-top:14px; padding:14px; background:#f8f9fa;",
                                     " border-left:4px solid #1565c0; border-radius:4px;",
                                     " min-height:80px; font-size:14px; line-height:1.7;"),
                      textOutput(ns("narrative_out"))
                    )
                  )
                ),
                column(7,
                  tags$div(style = "padding: 16px; border: 1px solid #dee2e6; border-radius:6px;",
                    tags$h4("Consulta en Lenguaje Natural", style = "font-weight:700; color:#C1121F; margin-top:0;"),
                    tags$p("Haz cualquier pregunta sobre los datos de carga y bienestar.",
                           style = "color:#6b7280; font-size:13px;"),
                    fluidRow(
                      column(9,
                        textInput(ns("nl_query"), NULL, width = "100%",
                                  placeholder = "¿Quién tiene el ACWR más alto? ¿Quién durmió peor esta semana?")
                      ),
                      column(3,
                        tags$div(style = "padding-top:1px;",
                          actionButton(ns("btn_query"), "Consultar",
                                       style = paste0("background-color:#C1121F; color:white; font-weight:bold;",
                                                      " border:none; border-radius:4px; padding:8px 18px; width:100%;"))
                        )
                      )
                    ),
                    tags$div(
                      style = paste0("margin-top:14px; padding:14px; background:#f8f9fa;",
                                     " border-left:4px solid #C1121F; border-radius:4px;",
                                     " min-height:80px; font-size:14px; line-height:1.7;"),
                      textOutput(ns("nl_answer_out"))
                    )
                  )
                )
              )
            )
          )
        )
      ),
      column(
        2,
        selectInput(
          inputId = ns("player_select"),
          label   = "Buscar jugador",
          choices = sort(dc_player_info$player),
          selected = NULL
        ),
        uiOutput(ns("player_info_box"))
      )
    ),

    fluidRow(
      style = "margin: 10px 0;",
      column(3,
             tags$div(
               style = "border: 1px solid #dee2e6; border-radius: 6px; padding: 12px; height: 100%;",
               tags$h4("Sin Respuesta Hoy", style = "font-weight:bold; text-align:center; margin-top:0;"),
               uiOutput(ns("missing_respondents"))
             )
      ),
      column(9,
             tags$h4("Resumen del Equipo", style = "text-align:center; font-weight:bold;"),
             DTOutput(ns("team_summary_table"))
      )
    ),

    fluidRow(
      column(12,
             h3(NULL, align = "center"),
             plotlyOutput(ns("survey_plot"), height = "600px")
      )
    ),

    fluidRow(
      column(12,
             tabsetPanel(
               tabPanel("HSR (>21 km/h)", plotlyOutput(ns("hsr_plot"), height = "500px")),
               tabPanel("Carga Aguda vs Crónica (A:C)", plotlyOutput(ns("ac_plot"), height = "500px"))
             )
      )
    ),

    fluidRow(
      column(12,
             h3("Interpretación de Métricas", align = "center"),
             tags$div(style = "margin: 10px 25px;",
                      tags$details(
                        tags$summary("Estado del jugador", style = "font-weight:bold; font-size:16px;"),
                        "Si el jugador está en verde, tiene un score de recuperación de 6 o mayor y su índice de carga esta en rango.",
                        tags$br(),
                        "Si el jugador está en amarillo, uno de los valores, ya sea score de recuperación o índice de carga, es menor a 6 o está fuera de rango.",
                        tags$br(),
                        "Si el jugador está en rojo, tiene un score de recuperación menor a 6 y su índice de carga esta fuera rango."
                      ),
                      tags$details(
                        tags$summary("Interpretación Índice de Carga", style = "font-weight:bold; font-size:16px;"),
                        "El índice de carga es el cociente de la carga aguda dividida por la carga crónica.",
                        tags$br(),
                        "La carga aguda es la suma de la distancia absoluta de High Speed Running (HSR_abs_dist) y la carga del jugador (player_load), ambos datos capturados por Catapult.",
                        tags$br(),
                        "La carga crónica es la media rodante de los valores de carga aguda, calculada tomando en cuenta los últimos siete días de carga aguda.",
                        tags$br(),
                        "Esta carga aguda se divide por la carga crónica para así tener el A:C ratio, en este caso interpretado como Índice de Carga."
                      ),
                      tags$details(
                        tags$summary("Interpretación Estatus de Carga", style = "font-weight:bold; font-size:16px;"),
                        "El valor del índice de carga, idealmente, debe de estar en un rango de 0.8 a 1.3.",
                        tags$br(),
                        "Debajo de 0.8: 'Carga Baja'. Sobre 1.3: 'Sobrecargado'."
                      ),
                      tags$details(
                        tags$summary("Interpretación Score de Recuperación", style = "font-weight:bold; font-size:16px;"),
                        "Compuesto por fatiga, sueño, calidad de sueño y dolor muscular..."
                      ),
                      tags$details(
                        tags$summary("Interpretación Estatus de Recuperación", style = "font-weight:bold; font-size:16px;"),
                        "Si un jugador presenta un score de recuperación de 6 o mayor, su estatus será 'Recuperado'.",
                        tags$br(),
                        "Si es menor a 6, su estatus será 'Fatigado'."
                      )
             ))
    )
  )
}

mod_dashboard_cargas_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    selected        <- reactiveVal(NULL)
    sorted_table_df <- reactiveVal(NULL)

    to_plotly <- function(p, src = NULL) {
      if (inherits(p, "ggplot")) p <- ggplotly(p, tooltip = "text")
      if (!inherits(p, "plotly")) stop("Plot must be a ggplot or plotly object.")
      if (!is.null(src)) p$x$source <- src
      p
    }

    add_competition_banner <- function(p, player, up_to_date) {
      if (exists("last4_md_80_flag", inherits = TRUE) && last4_md_80_flag(player, up_to_date)) {
        p <- p |>
          layout(
            annotations = list(list(
              x = 0.5, xref = "paper", xanchor = "center",
              y = 1.08, yref = "paper", yanchor = "top",
              text = "<b>Carga Alta de Competencia</b>",
              showarrow = FALSE,
              bgcolor = "#C62828", bordercolor = "#C62828",
              font = list(color = "white", size = 16),
              opacity = 0.95
            ))
          )
      }
      p
    }

    empty_plot <- function(msg) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = msg, size = 6) +
        theme_void()
    }

    scatter_df_local <- reactive({
      roster <- dc_player_info$player

      latest_pain2_obj <- get("latest_pain2", inherits = TRUE)

      rings_auth <- latest_pain2_obj |>
        dplyr::filter(pain_flag) |>
        dplyr::distinct(player, .keep_all = TRUE) |>
        dplyr::select(player, zona_adolorida, pain_flag)

      ac_last <- get("micros_individual", inherits = TRUE) |>
        dplyr::mutate(date = as.Date(date)) |>
        dplyr::filter(player %in% roster) |>
        dplyr::group_by(player) |>
        dplyr::slice_max(order_by = date, n = 1, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::select(player, ac_ratio)

      recuperacion_df <- get("recuperacion_df", inherits = TRUE)

      rec_last <- recuperacion_df |>
        dplyr::mutate(date = as.Date(`Marca temporal`)) |>
        dplyr::filter(Nombre %in% roster) |>
        dplyr::group_by(Nombre) |>
        dplyr::slice_max(order_by = date, n = 1, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::transmute(
          player      = Nombre,
          recovery_score,
          latest_date = date
        )

      rec_last |>
        dplyr::left_join(ac_last, by = "player") |>
        dplyr::left_join(rings_auth, by = "player") |>
        dplyr::mutate(
          pain_flag = tidyr::replace_na(pain_flag, FALSE),
          y_plot = dplyr::if_else(is.na(ac_ratio), ACWR_MISSING_Y, ac_ratio),
          recovery_status = dplyr::if_else(recovery_score >= 6, "Recuperado", "Fatigado"),
          load_status = dplyr::case_when(
            is.na(ac_ratio) ~ "Sin Catapult (sin ACWR)",
            ac_ratio < 0.8 ~ "Carga Baja",
            ac_ratio > 1.3 ~ "Carga Alta",
            TRUE ~ "Carga Óptima"
          ),
          color_status = dplyr::case_when(
            is.na(ac_ratio) & recovery_score >= 6 ~ "green",
            is.na(ac_ratio) & recovery_score < 6  ~ "red",

            ac_ratio >= 0.8 & ac_ratio <= 1.3 & recovery_score >= 6 ~ "green",
            (ac_ratio >= 0.8 & ac_ratio <= 1.3 & recovery_score < 6) |
              (recovery_score >= 6 & (ac_ratio < 0.8 | ac_ratio > 1.3)) ~ "yellow",
            TRUE ~ "red"
          ),
          hover_text = paste0(
            "Jugador: ", player,
            "<br>Fecha: ", latest_date,
            "<br>Score de Recuperación: ", recovery_score,
            ifelse(is.na(ac_ratio),
                   "<br>ACWR: (Sin Catapult)",
                   paste0("<br>Índice de Carga: ", round(ac_ratio, 2))),
            "<br>Estatus de Recuperación: ", recovery_status,
            "<br>Estatus de Carga: ", load_status,
            ifelse(pain_flag & !is.na(zona_adolorida),
                   paste0("<br>Zona Adolorida: ", zona_adolorida), "")
          )
        ) |>
        dplyr::filter(!is.na(recovery_score)) |>
        dplyr::distinct(player, .keep_all = TRUE)
    })

    observe({
      df <- scatter_df_local()
      if (nrow(df) == 0) return()

      choices <- sort(unique(df$player))
      curr <- isolate(selected())
      sel  <- if (!is.null(curr) && curr %in% choices) curr else choices[1]

      updateSelectInput(session, "player_select", choices = choices, selected = sel)
      if (is.null(curr)) selected(sel)
    })

    observeEvent(input$player_select, {
      selected(input$player_select)
    }, ignoreInit = TRUE)

    src_prefix <- session$ns("")

    observeEvent(event_data("plotly_click", source = session$ns("acwr_scatter")), {
      cd <- event_data("plotly_click", source = session$ns("acwr_scatter"))
      if (!is.null(cd) && !is.null(cd$customdata)) {
        selected(cd$customdata)
        updateSelectInput(session, "player_select", selected = cd$customdata)
      }
    }, ignoreInit = TRUE)

    observeEvent(event_data("plotly_click", source = session$ns("acwr_rest_scatter")), {
      cd <- event_data("plotly_click", source = session$ns("acwr_rest_scatter"))
      if (!is.null(cd) && !is.null(cd$customdata)) {
        selected(cd$customdata)
        updateSelectInput(session, "player_select", selected = cd$customdata)
      }
    }, ignoreInit = TRUE)

    observeEvent(event_data("plotly_click", source = session$ns("acwr_pain_scatter")), {
      cd <- event_data("plotly_click", source = session$ns("acwr_pain_scatter"))
      if (!is.null(cd) && !is.null(cd$customdata)) {
        selected(cd$customdata)
        updateSelectInput(session, "player_select", selected = cd$customdata)
      }
    }, ignoreInit = TRUE)

    observeEvent(event_data("plotly_click", source = session$ns("acwr_rpe_scatter")), {
      cd <- event_data("plotly_click", source = session$ns("acwr_rpe_scatter"))
      if (!is.null(cd) && !is.null(cd$customdata)) {
        selected(cd$customdata)
        updateSelectInput(session, "player_select", selected = cd$customdata)
      }
    }, ignoreInit = TRUE)

    output$acwr_scatter <- renderPlotly({
      df <- scatter_df_local()
      req(nrow(df) > 0, selected())

      df <- df |> dplyr::mutate(selected_flag = player == selected())

      rings_df <- df |>
        dplyr::filter(pain_flag == TRUE) |>
        dplyr::distinct(player, .keep_all = TRUE)

      p <- ggplot(df, aes(x = recovery_score, y = y_plot)) +
        geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
        geom_hline(yintercept = ACWR_MISSING_Y, linetype = "dotted", color = "gray60", alpha = 0.7) +
        geom_point(aes(fill = color_status, text = hover_text, customdata = player),
                   shape = 21, size = 6, alpha = 0.35, color = "black") +
        geom_point(data = rings_df,
                   aes(x = recovery_score, y = y_plot),
                   inherit.aes = FALSE,
                   shape = 21, size = 10, stroke = 1.2, fill = NA, color = "#d62728") +
        geom_point(data = dplyr::filter(df, selected_flag),
                   aes(fill = color_status, text = hover_text, customdata = player),
                   shape = 21, size = 8, stroke = 1.2, color = "black") +
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

      ggplotly(p, tooltip = "text", source = session$ns("acwr_scatter")) |>
        layout(
          margin = list(b = 80),
          annotations = list(
            list(
              x = 0.90, y = -0.14, xref = "paper", yref = "paper",
              text = "<b>Círculo rojo = Dolor muscular</b>",
              showarrow = FALSE, xanchor = "center", yanchor = "top",
              font = list(size = 12)
            ),
            list(
              x = 0.10, y = -0.14, xref = "paper", yref = "paper",
              text = "<b>Línea punteada = Sin Catapult (sin ACWR)</b>",
              showarrow = FALSE, xanchor = "left", yanchor = "top",
              font = list(size = 12)
            )
          )
        )
    })

    output$acwr_rest_scatter <- renderPlotly({
      req(exists("rest_scatter_df", inherits = TRUE))
      req(selected())

      df <- get("rest_scatter_df", inherits = TRUE)

      if (!"y_plot" %in% names(df)) {
        df <- df |> mutate(y_plot = if_else(is.na(ac_ratio), ACWR_MISSING_Y, ac_ratio))
      }

      df <- df |>
        dplyr::mutate(
          selected_flag = player == selected(),
          hover_text = paste0(
            "Jugador: ", player,
            "<br>Fecha: ", latest_date,
            "<br>Score de Descanso: ", rest_score,
            ifelse(is.na(ac_ratio),
                   "<br>ACWR: (Sin Catapult)",
                   paste0("<br>Índice de Carga: ", round(ac_ratio, 2))),
            "<br>Estatus de Descanso: ", rest_status,
            "<br>Estatus de Carga: ", load_status
          )
        )

      p <- ggplot(df, aes(x = rest_score, y = y_plot)) +
        geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
        geom_hline(yintercept = ACWR_MISSING_Y, linetype = "dotted", color = "gray60", alpha = 0.7) +
        geom_point(aes(fill = color_status_rest, text = hover_text, customdata = player),
                   shape = 21, size = 6, alpha = 0.30, color = "black") +
        geom_point(data = dplyr::filter(df, selected_flag),
                   aes(fill = color_status_rest, text = hover_text, customdata = player),
                   shape = 21, size = 8, stroke = 2, color = "black") +
        scale_fill_manual(values = c(green = "#2ca02c", yellow = "#ffbf00", red = "#d62728")) +
        labs(
          x = "Score de Descanso",
          y = "Índice de Carga (ACWR)",
          title = "ACWR & Descanso: Resumen del equipo de hoy"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank()
        )

      ggplotly(p, tooltip = "text", source = session$ns("acwr_rest_scatter"))
    })

    output$acwr_pain_scatter <- renderPlotly({
      req(exists("pain_scatter_df", inherits = TRUE))
      req(selected())

      df <- get("pain_scatter_df", inherits = TRUE)

      if (!"y_plot" %in% names(df)) {
        df <- df |> mutate(y_plot = if_else(is.na(ac_ratio), ACWR_MISSING_Y, ac_ratio))
      }

      df <- df |>
        dplyr::mutate(
          pain_flag     = tidyr::replace_na(pain_flag, FALSE),
          selected_flag = player == selected(),
          hover_text = paste0(
            "Jugador: ", player,
            "<br>Fecha: ", latest_date,
            "<br>Score de Dolor Muscular: ", pain_score,
            ifelse(is.na(ac_ratio),
                   "<br>ACWR: (Sin Catapult)",
                   paste0("<br>Índice de Carga: ", round(ac_ratio, 2))),
            "<br>Estatus de Dolor Muscular: ", pain_status,
            "<br>Estatus de Carga: ", load_status,
            ifelse(pain_flag, paste0("<br>Zona Adolorida: ", zona_adolorida), "")
          )
        )

      rings_df <- df |>
        dplyr::filter(pain_flag) |>
        dplyr::distinct(player, .keep_all = TRUE)

      p <- ggplot(df, aes(x = pain_score, y = y_plot)) +
        geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
        geom_hline(yintercept = ACWR_MISSING_Y, linetype = "dotted", color = "gray60", alpha = 0.7) +
        geom_point(
          aes(fill = color_status_pain, text = hover_text, customdata = player),
          shape = 21, size = 6, alpha = 0.30, color = "black", stroke = 0.7
        ) +
        geom_point(
          data = dplyr::filter(df, selected_flag),
          aes(fill = color_status_pain, text = hover_text, customdata = player),
          shape = 21, size = 8, stroke = 2, color = "black"
        ) +
        geom_point(
          data = rings_df,
          aes(x = pain_score, y = y_plot),
          inherit.aes = FALSE,
          shape = 21, size = 10, stroke = 1.2, fill = NA, color = "#d62728"
        ) +
        scale_fill_manual(values = c(green = "#2ca02c", yellow = "#ffbf00", red = "#d62728")) +
        labs(
          x = "Score de Dolor Muscular",
          y = "Índice de Carga (ACWR)",
          title = "ACWR & Dolor Muscular: Resumen del equipo de hoy"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank()
        )

      ggplotly(p, tooltip = "text", source = session$ns("acwr_pain_scatter"))
    })

    output$acwr_rpe_scatter <- renderPlotly({
      req(selected())
      req(exists("scatter_df_rpe", inherits = TRUE))

      df <- get("scatter_df_rpe", inherits = TRUE)

      if (!"y_plot" %in% names(df)) {
        df <- df |> mutate(y_plot = if_else(is.na(ac_ratio), ACWR_MISSING_Y, ac_ratio))
      }

      df <- df |>
        dplyr::mutate(
          selected_flag = player == selected(),
          hover_text = paste0(
            "Jugador: ", player,
            if ("rpe_date" %in% names(df)) paste0("<br>Fecha (RPE): ", rpe_date) else "",
            if ("acwr_date" %in% names(df) && !all(is.na(df$acwr_date))) {
              ifelse(is.na(ac_ratio), "", paste0("<br>Fecha (ACWR): ", acwr_date))
            } else "",
            "<br>RPE: ", rpe_val,
            ifelse(is.na(ac_ratio),
                   "<br>ACWR: (Sin Catapult)",
                   paste0("<br>Índice de Carga (ACWR): ", round(ac_ratio, 2))),
            if ("load_status" %in% names(df)) paste0("<br>Estatus de Carga: ", load_status) else ""
          )
        ) |>
        dplyr::filter(!is.na(rpe_val))

      p <- ggplot(df, aes(x = rpe_val, y = y_plot)) +
        geom_hline(yintercept = c(0.8, 1.3), linetype = "dashed", color = "gray50") +
        geom_hline(yintercept = ACWR_MISSING_Y, linetype = "dotted", color = "gray60", alpha = 0.7) +
        geom_point(
          aes(fill = color_status_rpe, text = hover_text, customdata = player),
          shape = 21, size = 6, alpha = 0.30, color = "black", stroke = 0.7
        ) +
        geom_point(
          data = dplyr::filter(df, selected_flag),
          aes(fill = color_status_rpe, text = hover_text, customdata = player),
          shape = 21, size = 8, stroke = 2, color = "black"
        ) +
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

      ggplotly(p, tooltip = "text", source = session$ns("acwr_rpe_scatter"))
    })

    output$survey_plot <- renderPlotly({
      req(selected())
      ggplotly(plot_player_recuperacion(selected()), tooltip = "text")
    })

    output$hsr_plot <- renderPlotly({
      req(selected())
      up_to <- if (exists("micros_hsr", inherits = TRUE)) max(get("micros_hsr", inherits = TRUE)$date, na.rm = TRUE) else Sys.Date()

      p <- to_plotly(plot_individual_hsr(selected()), src = session$ns("hsr_plot"))
      add_competition_banner(p, selected(), up_to)
    })

    output$ac_plot <- renderPlotly({
      req(selected())

      mi <- get("micros_individual", inherits = TRUE)
      if (!selected() %in% unique(mi$player)) {
        return(ggplotly(empty_plot(paste("Sin datos Catapult para", selected())), tooltip = "text"))
      }

      up_to <- max(mi$date, na.rm = TRUE)
      p <- to_plotly(plot_individual_ac(selected()), src = session$ns("ac_plot"))
      add_competition_banner(p, selected(), up_to)
    })

    output$missing_respondents <- renderUI({
      rec_df <- get("recuperacion_df", inherits = TRUE)
      ref_date <- max(rec_df$`Marca temporal`, na.rm = TRUE)

      responded <- rec_df |>
        dplyr::filter(`Marca temporal` == ref_date) |>
        dplyr::pull(Nombre) |>
        unique()

      missing <- sort(setdiff(dc_player_info$player, responded))

      if (length(missing) == 0) {
        tags$p(
          paste0("Todos respondieron (", format(ref_date, "%d/%m/%Y"), ")"),
          style = "color: #2ca02c; font-weight: bold; text-align: center;"
        )
      } else {
        tags$div(
          tags$p(
            paste0(length(missing), " sin respuesta – ", format(ref_date, "%d/%m/%Y")),
            style = "color: #d62728; font-weight: bold; margin-bottom: 6px;"
          ),
          tags$ul(
            style = "padding-left: 18px; margin: 0;",
            lapply(missing, function(p) tags$li(p, style = "font-size: 13px;"))
          )
        )
      }
    })

    output$team_summary_table <- DT::renderDT({
      df <- scatter_df_local() |>
        dplyr::transmute(
          Jugador            = player,
          `Última Respuesta` = latest_date,
          `Score Rec.`       = recovery_score,
          `Est. Recuperación`= recovery_status,
          ACWR               = dplyr::if_else(is.na(ac_ratio), NA_real_, round(ac_ratio, 2)),
          `Est. Carga`       = load_status,
          `Dolor Muscular`   = dplyr::if_else(
            pain_flag,
            dplyr::if_else(!is.na(zona_adolorida), paste0("Sí – ", zona_adolorida), "Sí"),
            "No"
          ),
          status_color       = color_status,
          sort_priority      = dplyr::case_when(
            color_status == "red"    ~ 1L,
            color_status == "yellow" ~ 2L,
            color_status == "green"  ~ 3L,
            TRUE ~ 4L
          )
        ) |>
        dplyr::arrange(sort_priority, `Score Rec.`)

      sorted_table_df(df)

      DT::datatable(
        df,
        options = list(
          pageLength   = 30,
          dom          = "tip",
          order        = list(list(8L, "asc"), list(2L, "asc")),
          columnDefs   = list(list(visible = FALSE, targets = c(7L, 8L)))
        ),
        rownames  = FALSE,
        selection = "single",
        class     = "compact stripe hover"
      ) |>
        DT::formatStyle(
          "status_color",
          target          = "row",
          backgroundColor = DT::styleEqual(
            c("green",   "yellow",  "red"),
            c("#d4edda", "#fff3cd", "#f8d7da")
          )
        ) |>
        DT::formatStyle(
          "ACWR",
          backgroundColor = DT::styleInterval(
            c(0.8, 1.3),
            c("#cce5ff", "#d4edda", "#f8d7da")
          )
        )
    })

    observeEvent(input$team_summary_table_rows_selected, {
      row <- input$team_summary_table_rows_selected
      df  <- sorted_table_df()
      if (!is.null(row) && !is.null(df) && row <= nrow(df)) {
        player_name <- df$Jugador[row]
        selected(player_name)
        updateSelectInput(session, "player_select", selected = player_name)
      }
    })

    output$player_info_box <- renderUI({
      req(selected())
      player_row <- dc_player_info |> filter(player == selected())
      if (nrow(player_row) == 0) return(NULL)
      tags$div(style = "text-align:center;",
               tags$img(
                 src = file.path("dashboard_cargas_www", "player_images", player_row$image),
                 width = "100%", style = "max-width:200px; border-radius:10px; margin-bottom:10px;"
               ),
               tags$h4(player_row$player),
               tags$p(paste("Nacido el:", player_row$age)),
               tags$p(paste("Estatura:", player_row$height))
      )
    })

    narrative_val  <- reactiveVal("")
    nl_answer_val  <- reactiveVal("")

    observeEvent(input$btn_narrative, {
      withProgress(message = "Consultando IA…", value = 0.6, {
        prompt <- dc_build_narrative_prompt(
          scatter_df_local(),
          get("micros_individual",  inherits = TRUE),
          get("recuperacion_df",    inherits = TRUE),
          get("current_md_phase",   inherits = TRUE)
        )
        narrative_val(dc_call_claude_api(prompt, max_tokens = 500))
      })
    })
    output$narrative_out <- renderText(narrative_val())

    observeEvent(input$btn_query, {
      req(nchar(trimws(input$nl_query)) > 0)
      withProgress(message = "Consultando IA…", value = 0.6, {
        prompt <- dc_build_nl_prompt(
          scatter_df_local(),
          get("micros_shiny_comb",  inherits = TRUE),
          get("micros_individual",  inherits = TRUE),
          get("recuperacion_df",    inherits = TRUE),
          get("current_md_phase",   inherits = TRUE),
          input$nl_query
        )
        nl_answer_val(dc_call_claude_api(prompt, max_tokens = 700))
      })
    })
    output$nl_answer_out <- renderText(nl_answer_val())
  })
}
