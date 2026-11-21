# =====================================================================
# deploy.R  -  Publica a DEMO no shinyapps.io ou Posit Connect
#
# Antes (uma vez): rsconnect::setAccountInfo(name, token, secret)   # shinyapps.io
#             ou:  rsconnect::addServer(url); rsconnect::connectUser() # Posit Connect
#
#   Rscript deploy.R                 # usa a conta configurada
#   APP_NAME=ppge-credenciamento Rscript deploy.R
#
# Os dois bancos NAO estao no git: sao gerados aqui e enviados junto com o app.
#   banco_edital.db  <- Knit do banco_edital.Rmd (precisa de dados/*.csv)
#   app.db           <- R/semear_demo.R (docentes ficticios; senhas so como hash)
# =====================================================================

if (!file.exists("banco_edital.db")) {
  stopifnot("Coloque revista.csv, issn.csv e percentil.csv em dados/" =
              all(file.exists(file.path("dados", c("revista.csv", "issn.csv", "percentil.csv")))))
  rmarkdown::render("banco_edital.Rmd", quiet = TRUE)
}
if (!file.exists("app.db")) source("R/semear_demo.R", encoding = "UTF-8")

arquivos <- c("app.R", "banco_edital.db", "app.db",
              list.files("R", pattern = "\\.R$", full.names = TRUE))
arquivos <- setdiff(arquivos, c("R/testar_artigo_y.R", "R/testar_app.R"))

rsconnect::deployApp(
  appDir   = ".",
  appFiles = arquivos,
  appName  = Sys.getenv("APP_NAME", "ppge-credenciamento"),
  appTitle = "Credenciamento Docente PPGE (demo)",
  forceUpdate = TRUE,
  launch.browser = FALSE,
  lint = FALSE
)
