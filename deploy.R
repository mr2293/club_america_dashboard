if (requireNamespace("renv", quietly = TRUE)) {
  renv::deactivate()
}

library(rsconnect)

shiny_acc <- Sys.getenv("SHINY_ACC_NAME")
shiny_token <- Sys.getenv("TOKEN")
shiny_secret <- Sys.getenv("SECRET")

rsconnect::setAccountInfo(
  name   = shiny_acc,
  token  = shiny_token,
  secret = shiny_secret
)

options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Write secrets.R from environment variable so the API key reaches the deployed app
api_key <- Sys.getenv("ANTHROPIC_API_KEY")
if (nchar(trimws(api_key)) > 0) {
  writeLines(paste0('Sys.setenv(ANTHROPIC_API_KEY = "', api_key, '")'), "secrets.R")
}

app_files <- c("app.R", "cargas7.R", "data/Sessions_micro01.xlsx")
if (file.exists("secrets.R")) app_files <- c(app_files, "secrets.R")

rsconnect::deployApp(
  appDir         = ".",
  appFiles       = app_files,
  appName        = "cargas_fisicas_7",
  account        = shiny_acc,
  server         = "shinyapps.io",
  forceUpdate    = TRUE,
  launch.browser = FALSE
)
