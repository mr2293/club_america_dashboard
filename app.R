# Club América -- combined physical-load dashboard.
#
# Mounts the three previously-standalone Shiny apps (apps/dashboard_cargas,
# apps/cargas_fisicas_7, apps/session_calculator) as namespaced modules
# inside one navbarPage, so they live at one URL instead of three. Each
# app's own app.R is untouched and still runs standalone; see
# apps/<name>/mod_<name>.R for the module wrapper that makes this possible.

if (file.exists("secrets.R")) source("secrets.R")

library(shiny)
library(plotly)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(lubridate)
library(scales)
library(readxl)
library(gt)
library(httr)
library(jsonlite)
library(tidyverse)
library(zoo)
library(reshape2)
library(ggrepel)

source("apps/dashboard_cargas/mod_dashboard_cargas.R", encoding = "UTF-8")
source("apps/cargas_fisicas_7/mod_cargas_fisicas_7.R", encoding = "UTF-8")
source("apps/session_calculator/mod_session_calculator.R", encoding = "UTF-8")

# Each app's www/ folder is only auto-served by Shiny when it sits next to
# the running app.R (i.e. at the repo root) -- since these apps now live
# in subfolders, their static assets need explicit resource-path mounts
# with app-specific prefixes so dashboard_cargas's and session_calculator's
# images can't collide, and img src="..." references in each module were
# updated to match these prefixes.
addResourcePath("dashboard_cargas_www", file.path("apps", "dashboard_cargas", "www"))
addResourcePath("session_calculator_www", file.path("apps", "session_calculator", "www"))

# Global chrome styling: the navbar's default shinytheme("cerulean") light
# blue didn't match the app's actual accent color, used elsewhere (e.g. the
# Bienestar tab's date/MD-phase bar) as a linear-gradient(135deg, #1a237e,
# #1565c0). Restyled the navbar to the same gradient, and layered in a
# shared font stack + softer page background across all three tabs for a
# more cohesive look, without touching any tab's own internal CSS/layout.
app_shell_css <- "
  body { font-family: -apple-system, BlinkMacSystemFont, 'Inter', 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; background: #F7F8FA; }
  /* Scoped to body's own direct-child wrapper only -- Bootstrap's navbar
     markup also uses an inner .container-fluid to wrap its own content,
     and an unscoped rule here paints over the navbar gradient since that
     inner one exactly fills the navbar's box. */
  body > .container-fluid { background: #F7F8FA; }

  .navbar-default {
    background: linear-gradient(135deg, #1a237e 0%, #1565c0 100%) !important;
    border: none !important;
    box-shadow: 0 1px 4px rgba(0,0,0,0.15);
  }
  .navbar-default .navbar-brand {
    color: #ffffff;
    font-weight: 700;
    letter-spacing: -0.01em;
  }
  .navbar-default .navbar-nav > li > a {
    color: rgba(255,255,255,0.82);
    font-weight: 500;
    transition: color 0.15s ease, background-color 0.15s ease;
  }
  .navbar-default .navbar-nav > li > a:hover,
  .navbar-default .navbar-nav > li > a:focus {
    color: #ffffff;
    background-color: rgba(255,255,255,0.08);
  }
  .navbar-default .navbar-nav > .active > a,
  .navbar-default .navbar-nav > .active > a:hover,
  .navbar-default .navbar-nav > .active > a:focus {
    color: #ffffff !important;
    background-color: rgba(255,255,255,0.18) !important;
    box-shadow: none;
  }

  .nav-tabs { border-bottom-color: #E2E5EA; }
  .nav-tabs > li > a { color: #1565c0; }
  .nav-tabs > li.active > a,
  .nav-tabs > li.active > a:hover,
  .nav-tabs > li.active > a:focus {
    color: #1a237e;
    font-weight: 700;
    border-color: #E2E5EA #E2E5EA transparent;
  }

  .btn-default, .btn { border-radius: 6px; }
  .form-control, .selectize-input { border-radius: 6px; }
"

ui <- navbarPage(
  title = "Club América — Cargas Físicas",
  theme = shinytheme("cerulean"),
  header = tagList(
    tags$style(HTML(app_shell_css)),
    tags$style(HTML(
      ".club-logo-corner { position: fixed; top: 4px; right: 8px; width: 40px; height: 40px; z-index: 1100; }"
    )),
    tags$img(src = "session_calculator_www/escudo.png", class = "club-logo-corner", alt = "Club América")
  ),
  tabPanel("Bienestar", mod_dashboard_cargas_ui("cargas")),
  tabPanel("Cargas Físicas", mod_cargas_fisicas_7_ui("cargas7")),
  tabPanel("Calculadora de Sesión", mod_session_calculator_ui("calc"))
)

server <- function(input, output, session) {
  mod_dashboard_cargas_server("cargas")
  mod_cargas_fisicas_7_server("cargas7")
  mod_session_calculator_server("calc")
}

shinyApp(ui = ui, server = server)
