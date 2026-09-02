#!/usr/bin/env Rscript
# Session Load Calculator -- Quick Start
#
# Usage:
#   1. Place your stats_df.xlsx in the data/ folder
#   2. Run: Rscript run.R
#   3. It opens automatically in your browser at http://localhost:8000

data_path <- file.path("data", "stats_df.xlsx")

if (!file.exists(data_path)) {
  cat("❌ No se encontro stats_df.xlsx\n")
  cat(sprintf("   Coloca tu archivo en: %s\n", normalizePath(data_path, mustWork = FALSE)))
  quit(status = 1)
}

cat("\U0001F680 Iniciando Calculadora de Sesion...\n")
cat("   Abriendo http://localhost:8000 en tu navegador...\n\n")

shiny::runApp(".", host = "0.0.0.0", port = 8000, launch.browser = TRUE)
