# Shiny module for a single "activity" row in the session planner.
#
# Each row lets the user pick an activity type and a duration (minutes).
# The module renders its own predicted-metric bars (mean + 95% CI) once
# both an activity type and a match day with data are selected, scaling
# the bars against a session-wide max supplied by the parent so all rows
# share a comparable scale (mirrors the original JS single-page app).

mod_activity_row_ui <- function(id, activity_choices, type = "", duration = 15) {
  ns <- NS(id)

  choices <- c("Seleccionar..." = "", activity_choices)

  div(
    class = "card", id = ns("card"),
    div(
      class = "card-header",
      selectInput(ns("type"), NULL, choices = choices, selected = type, width = "100%"),
      div(
        class = "dur-group",
        actionButton(ns("dur_minus"), "−", class = "dur-btn"),
        numericInput(ns("duration"), NULL, value = duration, min = 1, step = 1),
        actionButton(ns("dur_plus"), "+", class = "dur-btn"),
        span(class = "dur-label", "min")
      ),
      actionButton(ns("remove"), "×", class = "remove-btn")
    ),
    uiOutput(ns("metrics"))
  )
}

mod_activity_row_server <- function(id, match_day, model, metrics_cfg, session_max, on_remove) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$dur_minus, {
      updateNumericInput(session, "duration", value = max(1, (input$duration %||% 1) - 5))
    })
    observeEvent(input$dur_plus, {
      updateNumericInput(session, "duration", value = (input$duration %||% 1) + 5)
    })

    observeEvent(input$remove, {
      removeUI(selector = paste0("#", session$ns("card")))
      on_remove(id)
    }, ignoreInit = TRUE)

    output$metrics <- renderUI({
      act_type <- input$type
      md <- match_day()
      dur <- input$duration
      req(dur)

      if (is.null(act_type) || act_type == "") return(NULL)

      m <- model()
      has_data <- !is.null(m$model[[act_type]]) && !is.null(m$model[[act_type]][[md]])

      if (!has_data) {
        return(div(class = "warning", "⚠ Sin datos para esta actividad en ", md))
      }

      md_model <- m$model[[act_type]][[md]]
      smax <- session_max()

      bars <- lapply(metrics_cfg, function(mc) {
        d <- md_model[[mc$key]]
        if (is.null(d)) return(NULL)

        pred <- d$mean * dur
        ci_l <- d$ci_lower * dur
        ci_h <- d$ci_upper * dur
        max_val <- (smax[[mc$key]] %||% 0) * 1.15
        pct <- if (max_val > 0) min(pred / max_val * 100, 100) else 0
        ci_l_pct <- if (max_val > 0) ci_l / max_val * 100 else 0
        ci_h_pct <- if (max_val > 0) min(ci_h / max_val * 100, 100) else 0

        div(
          class = "metric-row",
          div(
            class = "metric-header",
            span(class = "metric-label", style = paste0("color:", mc$color), mc$label),
            span(
              class = "metric-val",
              tags$strong(fmt_num(pred)),
              span(class = "metric-ci", paste0("(", fmt_num(ci_l), "–", fmt_num(ci_h), ")"))
            )
          ),
          div(
            class = "bar-bg",
            div(class = "bar-ci", style = paste0(
              "left:", ci_l_pct, "%;width:", max(0, ci_h_pct - ci_l_pct), "%;background:", mc$color
            )),
            div(class = "bar-fill", style = paste0("width:", pct, "%;background:", mc$color))
          )
        )
      })

      sample_n <- md_model[["distance"]][["n"]]

      tagList(bars, div(class = "sample-n", paste0("n = ", sample_n, " registros")))
    })

    list(
      type = reactive(input$type),
      duration = reactive(input$duration %||% 1)
    )
  })
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

fmt_num <- function(n) {
  if (is.null(n) || is.na(n)) return("—")
  if (n >= 100) format(round(n), big.mark = ",", scientific = FALSE)
  else formatC(n, format = "f", digits = 1)
}
