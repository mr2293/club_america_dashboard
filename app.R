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

ui <- navbarPage(
  title = "Club América — Cargas Físicas",
  theme = shinytheme("cerulean"),
  tabPanel("Cargas", mod_dashboard_cargas_ui("cargas")),
  tabPanel("Cargas Físicas 7", mod_cargas_fisicas_7_ui("cargas7")),
  tabPanel("Calculadora de Sesión", mod_session_calculator_ui("calc"))
)

server <- function(input, output, session) {
  mod_dashboard_cargas_server("cargas")
  mod_cargas_fisicas_7_server("cargas7")
  mod_session_calculator_server("calc")
}

shinyApp(ui = ui, server = server)
