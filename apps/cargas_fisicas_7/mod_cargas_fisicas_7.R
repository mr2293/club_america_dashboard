# Shiny module wrapper around this app's original app.R, so it can be
# mounted as one tab inside the combined club_america_dashboard app
# (see /app.R at the repo root) instead of running as its own website.
#
# This file does NOT replace app.R -- app.R still works standalone (e.g.
# for local dev or a fallback deploy of this app alone).
#
# app.R itself mixes library()/data-loading/helper-function code (lines
# 1-1413) with its own ui/server definitions (lines 1417 on). Only the
# first part is reusable as a module -- we eval just that prefix here
# (guarded so it runs once even if sourced from multiple places) rather
# than duplicating ~1400 lines of plotting/table-building code by hand.
# This app's globals (call_claude_api, build_narrative_prompt,
# build_nl_prompt, selected_players, jugs, datos, base_theme, ...) are
# kept as-is; dashboard_cargas's same-named globals were the ones renamed
# (dc_ prefix) in mod_dashboard_cargas.R to avoid the collision.

CARGAS_FISICAS_7_DIR <- "apps/cargas_fisicas_7"

if (!exists(".cargas_fisicas_7_globals_loaded", inherits = TRUE)) {
  .cargas_fisicas_7_globals_loaded <- TRUE

  .c7_prev_wd <- getwd()
  setwd(CARGAS_FISICAS_7_DIR)
  tryCatch({
    .c7_app_lines <- readLines("app.R", encoding = "UTF-8", warn = FALSE)
    .c7_ui_marker <- which(.c7_app_lines == "# UI")
    if (length(.c7_ui_marker) == 0) stop("Could not find '# UI' marker in cargas_fisicas_7/app.R")
    # cut two lines before the marker to drop the "# ----" fence above it
    .c7_prefix <- .c7_app_lines[seq_len(.c7_ui_marker - 2)]
    eval(parse(text = .c7_prefix, encoding = "UTF-8"), envir = globalenv())
  }, error = function(e) {
    stop("Failed to load cargas_fisicas_7/app.R globals: ", conditionMessage(e))
  }, finally = setwd(.c7_prev_wd))
}

mod_cargas_fisicas_7_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$style(HTML("
        .comp-toggle-label {
          margin-bottom: 6px;
          font-size: 13px;
          font-weight: 700;
          letter-spacing: 0.04em;
          text-transform: uppercase;
          color: #6b7280;
        }
        .comp-toggle .shiny-options-group {
          display: inline-flex;
          gap: 2px;
          background: #eef0f4;
          border-radius: 999px;
          padding: 3px;
          margin: 0;
        }
        .comp-toggle .radio {
          margin: 0;
        }
        .comp-toggle .radio label {
          display: flex;
          align-items: center;
          justify-content: center;
          min-width: 56px;
          margin: 0;
          padding: 6px 16px;
          border-radius: 999px;
          cursor: pointer;
          font-weight: 700;
          font-size: 14px;
          color: #6b7280;
          transition: background-color .15s ease, color .15s ease, box-shadow .15s ease;
        }
        .comp-toggle .radio input[type='radio'] {
          position: absolute;
          opacity: 0;
          width: 0;
          height: 0;
        }
        .comp-toggle .radio label:hover {
          color: #0B1B4A;
        }
        .comp-toggle .radio:nth-of-type(1) label:has(input:checked) {
          background: #0B1B4A;
          color: #fff;
          box-shadow: 0 1px 4px rgba(11, 27, 74, .35);
        }
        .comp-toggle .radio:nth-of-type(2) label:has(input:checked) {
          background: #C1121F;
          color: #fff;
          box-shadow: 0 1px 4px rgba(193, 18, 31, .35);
        }
      "))
    ),

    tags$div(
      style = "margin-top:-10px; margin-bottom:10px; color:#4b5563; font-size:18px; font-weight:600;",
      "Acumulado por Período Seleccionado"
    ),

    tags$div(
      style = "margin-top:-4px; margin-bottom:8px; color:#111827; font-size:16px; font-weight:700;",
      textOutput(ns("ultima_sesion"))
    ),

    fluidRow(
      column(4,
        dateRangeInput(
          inputId  = ns("date_range"),
          label    = "Periodo:",
          start    = max(datos$date, na.rm = TRUE) - days(6),
          end      = max(datos$date, na.rm = TRUE),
          min      = min(datos$date, na.rm = TRUE),
          max      = max(datos$date, na.rm = TRUE),
          format   = "dd/mm/yyyy",
          language = "es",
          separator = " – "
        )
      ),
      column(3,
        tags$div(class = "comp-toggle-label", "Incluir Compensatorio"),
        tags$div(class = "comp-toggle",
          radioButtons(
            inputId  = ns("incluir_compensatorio"),
            label    = NULL,
            choices  = c("Sí" = "si", "No" = "no"),
            selected = "si",
            inline   = TRUE
          )
        )
      )
    ),

    tabsetPanel(
      tabPanel("Resumen Diario",
               div(style = "overflow-x: auto; padding: 20px;",
                   gt::gt_output(ns("tabla_resumen")))),
      tabPanel("HSR",                   plotOutput(ns("plot_hsr"),         height = "700px")),
      tabPanel("Sprint",                plotOutput(ns("plot_sprint"),       height = "700px")),
      tabPanel("Distancia Total",       plotOutput(ns("plot_distance"),     height = "700px")),
      tabPanel("ACC + DECC",            plotOutput(ns("plot_acc"),          height = "700px")),
      tabPanel("Player Load",           plotOutput(ns("plot_pl"),           height = "700px")),
      tabPanel("% Vel. Máx. Hist", plotOutput(ns("plot_pct_speed"),    height = "700px")),
      tabPanel("Sprints >30 km/h (>95%)", plotOutput(ns("plot_sprints_abs"),  height = "700px")),
      tabPanel("Sprints >25 km/h (>85%)", plotOutput(ns("plot_sprints_rel"),  height = "700px")),
      tabPanel("Partido (MD)",
        fluidRow(
          column(3,
            tags$div(style = "padding: 20px 20px 0 20px;",
              selectInput(
                inputId  = ns("md_date"),
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
                gt::gt_output(ns("tabla_md"))))
        )
      ),
      tabPanel("Perfil Jugador",
        fluidRow(
          column(3,
            tags$div(style = "padding: 20px 20px 0 20px;",
              selectInput(
                inputId  = ns("profile_player"),
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
                gt::gt_output(ns("tabla_perfil")))
          )
        )
      ),
      tabPanel("ACWR",
        fluidRow(
          column(12,
            div(style = "padding: 20px; overflow-x: auto;",
                gt::gt_output(ns("tabla_acwr")))
          )
        )
      ),
      tabPanel("Perfil MD",
        fluidRow(
          column(3,
            tags$div(style = "padding: 20px 20px 0 20px;",
              selectInput(
                inputId  = ns("md_rel_metric"),
                label    = "Métrica:",
                choices  = c(
                  "Distancia Total"  = "distance_m",
                  "HSR"              = "HSR_abs_dist",
                  "Sprint"           = "sprint_dist",
                  "ACC + DECC"       = "acc_plus_decc",
                  "Player Load"      = "player_load",
                  "Sprints >25 km/h (>85%)" = "sprint_count_85pct"
                ),
                selected = "distance_m"
              )
            )
          )
        ),
        fluidRow(
          column(12,
            plotOutput(ns("plot_md_relative"), height = "520px"))
        )
      ),
      tabPanel("Análisis IA",
        fluidRow(
          column(12,
            tags$div(style = "padding: 24px 24px 8px 24px;",
              tags$h4("Narrativa Semanal",
                      style = "font-weight:700; color:#0B1B4A; margin-bottom:4px;"),
              tags$p("Resumen automático sobre la carga de los últimos 7 días.",
                     style = "color:#6b7280; font-size:14px; margin-bottom:12px;"),
              actionButton(ns("btn_narrative"), "Generar Narrativa",
                           style = paste0("background-color:#0B1B4A; color:white; font-weight:bold;",
                                          " border:none; border-radius:4px; padding:8px 18px;")),
              tags$div(
                style = paste0("margin-top:14px; padding:16px; background:#f8f9fa;",
                               " border-left:4px solid #0B1B4A; border-radius:4px; min-height:80px;",
                               " font-size:15px; line-height:1.65;"),
                textOutput(ns("narrative_out"))
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
              tags$p("Haz cualquier pregunta sobre los datos de carga física.",
                     style = "color:#6b7280; font-size:14px; margin-bottom:12px;"),
              fluidRow(
                column(8,
                  textInput(ns("nl_query"), NULL, width = "100%",
                            placeholder = "¿Quién tuvo más HSR la última semana?")
                ),
                column(2,
                  tags$div(style = "padding-top:0px;",
                    actionButton(ns("btn_query"), "Consultar",
                                 style = paste0("background-color:#C1121F; color:white; font-weight:bold;",
                                                " border:none; border-radius:4px; padding:8px 18px;"))
                  )
                )
              ),
              tags$div(
                style = paste0("margin-top:14px; padding:16px; background:#f8f9fa;",
                               " border-left:4px solid #C1121F; border-radius:4px; min-height:80px;",
                               " font-size:15px; line-height:1.65;"),
                textOutput(ns("nl_answer_out"))
              )
            )
          )
        )
      )
    )
  )
}

mod_cargas_fisicas_7_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    datos_base <- reactive({
      if (identical(input$incluir_compensatorio, "no")) {
        datos |> dplyr::filter(!grepl("compensatorio", session_name, ignore.case = TRUE))
      } else {
        datos
      }
    })

    datos_win <- reactive({
      req(input$date_range)
      start <- as.Date(input$date_range[1])
      end   <- as.Date(input$date_range[2])
      validate(need(start <= end, "La fecha de inicio debe ser anterior a la fecha final."))
      datos_base() |> dplyr::filter(date >= start, date <= end)
    })

    output$ultima_sesion <- renderText({
      req(datos_win())
      last_date <- as.Date(max(datos_win()$date, na.rm = TRUE))
      paste0("Última Sesión Considerada: ", format(last_date, "%d/%m/%Y"))
    })

    output$plot_hsr <- renderPlot({
      req(datos_win())
      plot_hsr_7d_with_4w_avg(datos_win(), datos_base())
    })

    output$plot_sprint <- renderPlot({
      req(datos_win())
      plot_sprint_7d_with_4w_avg(datos_win(), datos_base())
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
      plot_sprint_count_95pct(datos_win(), datos_base())
    })

    output$plot_sprints_rel <- renderPlot({
      req(datos_win())
      plot_sprint_count_85pct(datos_win(), datos_base())
    })

    output$tabla_resumen <- gt::render_gt({
      req(datos_win())
      build_resumen_table(datos_win())
    })

    output$tabla_md <- gt::render_gt({
      req(input$md_date)
      build_md_table(datos_base(), input$md_date)
    })

    output$tabla_perfil <- gt::render_gt({
      req(datos_win(), input$profile_player)
      build_player_profile(datos_win(), input$profile_player)
    })

    output$tabla_acwr <- gt::render_gt({
      build_acwr_table(datos_base())
    })

    output$plot_md_relative <- renderPlot({
      req(input$md_rel_metric)
      build_md_relative_plot(datos_base(), input$md_rel_metric)
    })

    narrative_val <- reactiveVal("")
    observeEvent(input$btn_narrative, {
      withProgress(message = "Consultando IA…", value = 0.6, {
        prompt <- build_narrative_prompt(datos_base())
        result <- call_claude_api(prompt, max_tokens = 450)
        narrative_val(result)
      })
    })
    output$narrative_out <- renderText(narrative_val())

    nl_answer_val <- reactiveVal("")
    observeEvent(input$btn_query, {
      req(nchar(trimws(input$nl_query)) > 0)
      withProgress(message = "Consultando IA…", value = 0.6, {
        prompt <- build_nl_prompt(datos_base(), input$nl_query)
        result <- call_claude_api(prompt, max_tokens = 700)
        nl_answer_val(result)
      })
    })
    output$nl_answer_out <- renderText(nl_answer_val())
  })
}
