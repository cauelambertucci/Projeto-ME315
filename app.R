# =====================================================================
# app.R  -  Credenciamento Docente PPGE: avaliação pelo currículo Lattes
#
#   shiny::runApp()      # na raiz do projeto
#
# Sem login e sem gravar nada: a pessoa envia o Lattes (.zip/.xml), escolhe os
# artigos, informa licenças (Artigo Y) e vê o resultado do Artigo X.
# Precisa só do banco_edital.db (somente leitura). Se faltar, R/bootstrap.R o
# obtém ao iniciar (Knit dos dados/*.csv ou download do Drive).
# Variável opcional: PPGE_BANCO_EDITAL (caminho do banco_edital.db).
# =====================================================================

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(DT); library(DBI); library(RSQLite)
})

for (f in c("funcoes_banco", "periodo_artigo_y", "modulo_periodo", "avaliacao", "importar_artigos",
            "ui_componentes", "mod_avaliacao", "mod_lattes", "mod_revistas", "mod_regras", "bootstrap")) {
  source(file.path("R", paste0(f, ".R")), encoding = "UTF-8", local = FALSE)
}

preparar_banco()
ARQ_BANCO_EDITAL <- Sys.getenv("PPGE_BANCO_EDITAL", "banco_edital.db")

# arquivos enviados: até 30 MB (currículos grandes)
options(shiny.maxRequestSize = 30 * 1024^2)

ui <- page_navbar(
  id = "nav", lang = "pt-BR", fillable = FALSE, theme = tema_ppge(),
  bg = "#1F3A5F", inverse = TRUE,
  title = span(icon("graduation-cap"), " PPGE · Credenciamento"),
  window_title = "Credenciamento Docente PPGE",
  header = tags$head(tags$script(HTML(
    "Shiny.addCustomMessageHandler('ppge_rolar', function(id) {
       var el = document.getElementById(id); if (el) el.scrollIntoView({behavior: 'smooth'});
     });"))),
  nav_panel("Avaliar meu Lattes", value = "lattes", icon = icon("file-code"), lattes_ui("lat")),
  nav_panel("Revistas", value = "revistas", icon = icon("book-open"), revistas_ui("rev")),
  nav_panel("Regras e premissas", value = "regras", icon = icon("scale-balanced"), regras_ui("regras"))
)

server <- function(input, output, session) {
  # UMA conexão por sessão (tmp_artigo é por conexão), fechada no fim
  con_sjr <- conectar_banco(ARQ_BANCO_EDITAL)
  onStop(function() try(DBI::dbDisconnect(con_sjr), silent = TRUE))

  lattes_server("lat", con_sjr)
  revistas_server("rev", list(con_sjr = con_sjr))
  regras_server("regras", list(con_sjr = con_sjr))
}

shinyApp(ui, server)
