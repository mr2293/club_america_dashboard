library(rsconnect)

shiny_acc    <- Sys.getenv("SHINY_ACC_NAME")
shiny_token  <- Sys.getenv("TOKEN")
shiny_secret <- Sys.getenv("SECRET")
anthropic_key <- Sys.getenv("ANTHROPIC_API_KEY")

cat("Shiny account:", shiny_acc, "\n")
cat("Token length:", nchar(shiny_token), "\n")
cat("Secret length:", nchar(shiny_secret), "\n")
cat("Anthropic key set:", nchar(anthropic_key) > 0, "\n")

rsconnect::setAccountInfo(
  name   = shiny_acc,
  token  = shiny_token,
  secret = shiny_secret
)

if (requireNamespace("renv", quietly = TRUE)) {
  renv::deactivate()
}

options(
  repos                  = c(CRAN = "https://cran.rstudio.com/"),
  rsconnect.http.timeout = 300
)

# Write secrets.R so the key is bundled into the deployed app
if (nchar(anthropic_key) > 0) {
  writeLines(
    paste0('Sys.setenv(ANTHROPIC_API_KEY = "', anthropic_key, '")'),
    "secrets.R"
  )
  cat("secrets.R written for deployment\n")
}

rsconnect::deployApp(
  appDir        = ".",
  appName       = "dashboard_cargas",
  account       = shiny_acc,
  server        = "shinyapps.io",
  forceUpdate   = TRUE,
  launch.browser = FALSE
)

cat("Deployment complete. Set ANTHROPIC_API_KEY manually in shinyapps.io dashboard if needed.\n")