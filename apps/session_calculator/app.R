# Calculadora de Sesion -- R / Shiny port
#
# Predictive model of physical training load, based on historical
# Catapult data. Port of the original Python/FastAPI + vanilla-JS app,
# rebuilt as a Shiny app so it lives in the same language/stack as
# related tools.
#
# Usage:
#   Place stats_df.xlsx in data/ (or set the DATA_PATH env var), then:
#   Rscript -e "shiny::runApp()"

library(shiny)

source("R/categories.R")
source("R/engine.R")
source("R/mod_activity_row.R")

DATA_PATH <- Sys.getenv("DATA_PATH", file.path("data", "stats_df.xlsx"))

METRICS_CFG <- list(
  list(key = "distance", label = "Distancia Total", unit = "m", color = "#3B82F6"),
  list(key = "player_load", label = "Player Load", unit = "AU", color = "#EC4899"),
  list(key = "hsr", label = "HSR", unit = "m", color = "#F59E0B"),
  list(key = "sprint", label = "Sprint", unit = "m", color = "#EF4444"),
  list(key = "accel", label = "ACC", unit = "", color = "#10B981"),
  list(key = "decel", label = "DEC", unit = "", color = "#8B5CF6")
)

# Fixed microcycle: only these match days are ever offered, in chronological
# order. This is a historical-aggregate model rather than a single-week
# planner, so all 9 slots are always shown -- how much data lands in each
# one (MD-6 only has data from weeks with a full 7-day gap between games,
# MD+1/MD+2 only from weeks where there was recovery time before the next
# a-block starts, etc.) depends on the real calendar gap between matches.
# "Otro" is appended for sessions tagged with a non-standard DayCode
# (see normalize_match_day() in R/engine.R) that staff couldn't assign to
# the microcycle -- kept visible rather than silently dropped.
PRIORITY_MDS <- c("MD-6", "MD-5", "MD-4", "MD-3", "MD-2", "MD-1", "MD", "MD+1", "MD+2", "Otro")

md_label_full <- function(md) {
  if (md == "MD+1") paste0(md, " · Compensatorio") else md
}

# ── UI ───────────────────────────────────────────────────────────────────────

app_css <- "
  * { box-sizing: border-box; }
  body { font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif; background: #F3F4F6; color: #111827; padding: 16px 12px; }
  .container-app { max-width: 540px; margin: 0 auto; }
  h1.app-title { font-size: 20px; font-weight: 800; letter-spacing: -0.03em; text-align: center; margin-bottom: 2px; }
  .subtitle { font-size: 12px; color: #6B7280; text-align: center; margin-bottom: 20px; }
  .section-label { font-size: 10px; font-weight: 700; color: #6B7280; text-transform: uppercase; letter-spacing: 0.08em; margin-bottom: 5px; display: block; }

  .md-row { display: flex; gap: 4px; flex-wrap: wrap; margin-bottom: 14px; }
  .md-btn { flex: 1 0 auto; min-width: 44px; padding: 6px !important; border-radius: 6px !important; font-size: 11px !important; font-weight: 700 !important; border: 1px solid #D1D5DB !important; background: #fff !important; color: #6B7280 !important; box-shadow: none !important; }
  .md-btn.active { border: 2px solid #2563EB !important; background: #EFF6FF !important; color: #2563EB !important; }
  .md-btn.md-btn-disabled { opacity: 0.4 !important; cursor: not-allowed !important; pointer-events: none; }

  .card { background: #fff; border: 1px solid #E5E7EB; border-radius: 10px; padding: 14px 16px; margin-bottom: 8px; }
  .card-header { display: flex; align-items: center; gap: 8px; flex-wrap: wrap; }
  .card-header .form-group { margin-bottom: 0 !important; flex: 1 1 140px; }
  .card-header select.form-control { padding: 7px 10px !important; border-radius: 6px !important; border: 1px solid #D1D5DB !important; background: #F9FAFB !important; font-size: 13px !important; font-weight: 500 !important; color: #111827 !important; height: auto !important; }
  .dur-group { display: flex; align-items: center; gap: 4px; }
  .dur-group .form-group { margin-bottom: 0 !important; }
  .dur-btn { width: 28px !important; height: 28px !important; padding: 0 !important; border-radius: 6px !important; border: 1px solid #D1D5DB !important; background: #F9FAFB !important; font-size: 15px !important; line-height: 1 !important; }
  .dur-group input[type=number] { width: 56px; text-align: center; padding: 5px 2px !important; border-radius: 6px !important; border: 1px solid #D1D5DB !important; background: #F9FAFB !important; font-size: 14px !important; font-weight: 700 !important; height: auto !important; }
  .dur-label { font-size: 11px; color: #9CA3AF; margin-left: 2px; }
  .remove-btn { width: 28px !important; height: 28px !important; padding: 0 !important; border-radius: 6px !important; border: 1px solid #E5E7EB !important; background: transparent !important; color: #EF4444 !important; font-size: 16px !important; flex-shrink: 0; box-shadow: none !important; }
  .add-btn { width: 100%; padding: 9px !important; border-radius: 8px !important; border: 2px dashed #D1D5DB !important; background: transparent !important; color: #6B7280 !important; font-size: 13px !important; font-weight: 600 !important; box-shadow: none !important; margin-top: 6px; }
  .warning { font-size: 12px; color: #D97706; padding: 6px 0; }
  .sample-n { font-size: 10px; color: #9CA3AF; text-align: right; margin-top: 4px; }

  .metric-row { margin-bottom: 5px; }
  .metric-header { display: flex; justify-content: space-between; font-size: 11px; margin-bottom: 1px; }
  .metric-label { font-weight: 700; letter-spacing: 0.03em; }
  .metric-val strong { color: #374151; }
  .metric-ci { color: #9CA3AF; margin-left: 3px; font-size: 10px; }
  .bar-bg { height: 6px; background: #F3F4F6; border-radius: 3px; position: relative; overflow: hidden; }
  .bar-ci { position: absolute; height: 100%; opacity: 0.15; border-radius: 3px; }
  .bar-fill { height: 100%; border-radius: 3px; transition: width 0.25s ease; }

  .totals { background: #fff; border: 2px solid #2563EB; border-radius: 12px; padding: 16px; margin-bottom: 16px; margin-top: 14px; }
  .totals-header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px; }
  .totals-title { font-size: 14px; font-weight: 800; color: #2563EB; margin: 0; }
  .totals-sub { font-size: 11px; color: #9CA3AF; font-weight: 600; }
  .totals-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 8px; }
  .total-cell { background: #F9FAFB; border-radius: 8px; padding: 8px 6px; text-align: center; }
  .total-metric-label { font-size: 9px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.06em; margin-bottom: 3px; }
  .total-val { font-size: 18px; font-weight: 800; color: #111827; line-height: 1; }
  .total-ci { font-size: 9px; color: #9CA3AF; margin-top: 2px; }
  .footer { font-size: 10px; color: #9CA3AF; text-align: center; line-height: 1.5; padding: 0 12px; }
  .loading, .error-box { text-align: center; padding: 40px; color: #6B7280; }

  .club-logo { position: fixed; top: 12px; right: 16px; width: 56px; height: 56px; z-index: 100; }
"

ui <- fluidPage(
  title = "Calculadora de Sesion",
  tags$head(tags$style(HTML(app_css))),
  tags$img(src = "escudo.png", class = "club-logo", alt = "Club América"),
  div(
    class = "container-app",
    h1(class = "app-title", "Calculadora de Sesión"),
    p(class = "subtitle", "Modelo predictivo · Datos Catapult"),
    uiOutput("body_ui")
  )
)

# ── Server ───────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  model <- reactiveVal(NULL)
  load_error <- reactiveVal(NULL)

  load_model <- function() {
    tryCatch({
      model(build_full_model(DATA_PATH))
      load_error(NULL)
    }, error = function(e) {
      load_error(conditionMessage(e))
    })
  }

  if (file.exists(DATA_PATH)) load_model() else load_error(paste0("No se encontro ", DATA_PATH))

  row_ids <- reactiveVal(character(0))
  row_counter <- reactiveVal(0)
  row_data <- new.env(parent = emptyenv()) # id -> list(type = reactive, duration = reactive)

  match_day <- reactiveVal(NULL)

  # Which of the 9 priority match days actually have data (>=1 activity
  # type) in the loaded dataset -- used to gray out empty slots.
  md_has_data <- reactive({
    m <- model()
    req(m)
    setNames(
      vapply(PRIORITY_MDS, function(md) {
        any(vapply(m$activity_types, function(at) !is.null(m$model[[at$id]][[md]]), logical(1)))
      }, logical(1)),
      PRIORITY_MDS
    )
  })

  activity_choices <- reactive({
    m <- model()
    req(m)
    setNames(
      vapply(m$activity_types, function(x) x$id, character(1)),
      vapply(m$activity_types, function(x) x$display_name, character(1))
    )
  })

  remove_row <- function(id) {
    row_ids(setdiff(row_ids(), id))
    rm(list = id, envir = row_data)
  }

  add_row <- function(type = "", duration = 15) {
    row_counter(row_counter() + 1)
    id <- paste0("row", row_counter())
    insertUI(
      selector = "#activity-rows", where = "beforeEnd",
      ui = mod_activity_row_ui(id, activity_choices(), type = type, duration = duration)
    )
    res <- mod_activity_row_server(
      id, match_day = match_day, model = model, metrics_cfg = METRICS_CFG,
      session_max = session_max, on_remove = remove_row
    )
    assign(id, res, envir = row_data)
    row_ids(c(row_ids(), id))
  }

  all_activities <- reactive({
    ids <- row_ids()
    md <- match_day()
    m <- model()
    if (is.null(m) || is.null(md) || length(ids) == 0) return(list())

    lapply(ids, function(id) {
      mod <- get(id, envir = row_data)
      act_type <- mod$type()
      dur <- mod$duration()
      has <- !is.null(act_type) && act_type != "" &&
        !is.null(m$model[[act_type]]) && !is.null(m$model[[act_type]][[md]])
      list(type = act_type, duration = dur, has_data = has)
    })
  })

  session_max <- reactive({
    m <- model()
    md <- match_day()
    acts <- all_activities()
    smax <- setNames(as.list(rep(0, length(METRICS_CFG))), vapply(METRICS_CFG, function(x) x$key, character(1)))
    if (is.null(m) || is.null(md)) return(smax)

    for (a in acts) {
      if (!isTRUE(a$has_data)) next
      md_model <- m$model[[a$type]][[md]]
      for (mc in METRICS_CFG) {
        d <- md_model[[mc$key]]
        if (!is.null(d)) smax[[mc$key]] <- max(smax[[mc$key]], d$ci_upper * a$duration)
      }
    }
    smax
  })

  totals <- reactive({
    m <- model()
    md <- match_day()
    acts <- all_activities()
    tot <- lapply(METRICS_CFG, function(x) list(pred = 0, ciL = 0, ciH = 0))
    names(tot) <- vapply(METRICS_CFG, function(x) x$key, character(1))
    if (is.null(m) || is.null(md)) return(tot)

    for (a in acts) {
      if (!isTRUE(a$has_data)) next
      md_model <- m$model[[a$type]][[md]]
      for (mc in METRICS_CFG) {
        d <- md_model[[mc$key]]
        if (is.null(d)) next
        tot[[mc$key]]$pred <- tot[[mc$key]]$pred + d$mean * a$duration
        tot[[mc$key]]$ciL <- tot[[mc$key]]$ciL + d$ci_lower * a$duration
        tot[[mc$key]]$ciH <- tot[[mc$key]]$ciH + d$ci_upper * a$duration
      }
    }
    tot
  })

  total_minutes <- reactive({
    acts <- all_activities()
    if (length(acts) == 0) return(0)
    sum(vapply(acts, function(a) a$duration %||% 0, numeric(1)))
  })

  # Bootstrap default activities once the model is ready, mirroring the
  # original app's behaviour of pre-filling a plausible MD-2 session.
  observeEvent(model(), {
    m <- model()
    req(m)

    has_data <- md_has_data()
    default_md <- if (isTRUE(has_data[["MD-2"]])) {
      "MD-2"
    } else {
      available_mds <- PRIORITY_MDS[has_data]
      if (length(available_mds) > 0) available_mds[1] else "MD-2"
    }
    match_day(default_md)

    available <- Filter(function(at) !is.null(m$model[[at$id]][[default_md]]), m$activity_types)
    available <- utils::head(available, 4)

    if (length(available) > 0) {
      for (i in seq_along(available)) {
        add_row(type = available[[i]]$id, duration = if (i == 1) 10 else 15)
      }
    } else {
      add_row()
    }
  }, once = TRUE)

  observeEvent(input$add_activity, add_row())

  output$body_ui <- renderUI({
    if (!is.null(load_error())) {
      return(div(
        class = "error-box",
        style = "color:#EF4444",
        "Error cargando el modelo.", tags$br(),
        tags$span(style = "font-size:12px;color:#6B7280", load_error())
      ))
    }
    if (is.null(model())) return(div(class = "loading", "Cargando modelo..."))

    tagList(
      tags$label(class = "section-label", "Día de partido"),
      uiOutput("md_buttons"),
      div(
        style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:8px",
        tags$label(class = "section-label", style = "margin-bottom:0", "Actividades"),
        tags$span(style = "font-size:11px;color:#9CA3AF", textOutput("total_min_label", inline = TRUE))
      ),
      div(id = "activity-rows"),
      actionButton("add_activity", "+ Agregar actividad", class = "add-btn"),
      uiOutput("totals_panel"),
      div(
        class = "footer",
        "Promedios por jugador · IC 95% (t-distribution) · Los intervalos se estrechan con más datos"
      )
    )
  })

  output$total_min_label <- renderText(paste(total_minutes(), "min total"))

  output$md_buttons <- renderUI({
    current <- match_day()
    has_data <- md_has_data()
    div(
      class = "md-row",
      lapply(PRIORITY_MDS, function(md) {
        enabled <- isTRUE(has_data[[md]])
        actionButton(
          paste0("md_", gsub("[+-]", "_", md)),
          md_label_full(md),
          class = paste(
            "md-btn",
            if (identical(md, current)) "active" else "",
            if (!enabled) "md-btn-disabled" else ""
          ),
          onclick = if (enabled) {
            sprintf("Shiny.setInputValue('md_selected', '%s', {priority: 'event'})", md)
          } else {
            NULL
          }
        )
      })
    )
  })

  observeEvent(input$md_selected, match_day(input$md_selected))

  output$totals_panel <- renderUI({
    tot <- totals()
    md <- match_day()
    div(
      class = "totals",
      div(
        class = "totals-header",
        h2(class = "totals-title", "Carga Total"),
        span(class = "totals-sub", paste0(md_label_full(md), " · ", total_minutes(), " min"))
      ),
      div(
        class = "totals-grid",
        lapply(METRICS_CFG, function(mc) {
          t <- tot[[mc$key]]
          div(
            class = "total-cell",
            div(class = "total-metric-label", style = paste0("color:", mc$color), mc$label),
            div(class = "total-val", fmt_num(t$pred)),
            div(class = "total-ci", paste0(fmt_num(t$ciL), "–", fmt_num(t$ciH)))
          )
        })
      )
    )
  })
}

shinyApp(ui, server)
