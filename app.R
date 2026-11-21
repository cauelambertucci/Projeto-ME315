# =====================================================================
# app.R  -  Credenciamento Docente PPGE (DEMO, dados ficticios)
#
#   shiny::runApp()      # na raiz do projeto
#
# Precisa de:  banco_edital.db (Knit do banco_edital.Rmd; somente leitura)
#              app.db          (Rscript R/semear_demo.R; gravavel)
# Variaveis de ambiente opcionais:
#   PPGE_BANCO_EDITAL  caminho do banco_edital.db  (padrao: banco_edital.db)
#   PPGE_APP_DB        caminho do app.db           (padrao: app.db)
# =====================================================================

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(DT); library(DBI); library(RSQLite)
  library(shinymanager); library(jsonlite); library(ggplot2)
})

for (f in c("funcoes_banco", "periodo_artigo_y", "modulo_periodo", "banco_app", "importar_artigos",
            "ui_componentes", "mod_avaliacao", "mod_docente", "mod_revistas", "mod_regras", "mod_comissao", "bootstrap")) {
  source(file.path("R", paste0(f, ".R")), encoding = "UTF-8", local = FALSE)
}

preparar_bancos()   # servidor novo: obtem banco_edital.db e semeia app.db (R/bootstrap.R)
ARQ_BANCO_EDITAL <- Sys.getenv("PPGE_BANCO_EDITAL", "banco_edital.db")

# Login: confere usuario/senha no app.db (hash scrypt). Conexao curta, so leitura do usuario.
checar_credenciais <- function(user, password) {
  con <- abrir_app_db(); on.exit(DBI::dbDisconnect(con))
  u <- autenticar_usuario(con, user, password)
  if (is.null(u)) return(list(result = FALSE))
  list(result = TRUE, user_info = list(user = u$login, papel = u$papel,
                                       docente_id = if (is.na(u$docente_id)) NA_integer_ else u$docente_id))
}

ABAS_DOCENTE  <- c("doc_sol", "doc_avaliacao", "doc_periodo", "doc_artigos")
ABAS_COMISSAO <- c("com_lista", "com_detalhe", "com_painel")

ui_app <- page_navbar(
  id = "nav", lang = "pt-BR", fillable = FALSE, theme = tema_ppge(),
  bg = "#1F3A5F", inverse = TRUE,
  title = span(icon("graduation-cap"), " PPGE · Credenciamento"),
  window_title = "Credenciamento Docente PPGE",
  header = div(class = "faixa-demo", role = "note",
               icon("flask"), " Demonstração: todos os docentes, artigos e licenças são fictícios."),
  nav_panel("Minhas solicitações", value = "doc_sol", icon = icon("folder-open"), doc_solicitacoes_ui("doc")),
  nav_panel("Avaliação", value = "doc_avaliacao", icon = icon("gauge"), doc_avaliacao_ui("doc")),
  nav_panel("Período e Artigo Y", value = "doc_periodo", icon = icon("calendar-days"), doc_periodo_ui("doc")),
  nav_panel("Artigos", value = "doc_artigos", icon = icon("file-lines"), doc_artigos_ui("doc")),
  nav_panel("Solicitações", value = "com_lista", icon = icon("list-check"), com_lista_ui("com")),
  nav_panel("Detalhe", value = "com_detalhe", icon = icon("magnifying-glass-chart"), com_detalhe_ui("com")),
  nav_panel("Painel", value = "com_painel", icon = icon("chart-column"), com_painel_ui("com")),
  nav_panel("Revistas", value = "revistas", icon = icon("book-open"), revistas_ui("rev")),
  nav_panel("Regras e premissas", value = "regras", icon = icon("scale-balanced"), regras_ui("regras")),
  nav_spacer(),
  nav_item(uiOutput("quem"))
)

ui <- secure_app(
  ui_app, language = "pt-BR", enable_admin = FALSE, fab_position = "bottom-right",
  theme = tema_ppge(),
  tags_top = tags$div(
    tags$h1(class = "h4 text-center mt-2", style = "color:#1F3A5F", "Credenciamento Docente · PPGE"),
    tags$p(class = "text-center small text-secondary", "Demonstração com dados fictícios")),
  tags_bottom = tags$p(class = "small text-secondary text-center mt-2",
    "Usuários de demonstração: peça as credenciais à equipe (arquivo fora do repositório).")
)

server <- function(input, output, session) {
  res_auth <- secure_server(check_credentials = checar_credenciais)

  # UMA conexao de cada banco por sessao (tmp_artigo e por conexao), fechadas no fim
  con_sjr <- conectar_banco(ARQ_BANCO_EDITAL)
  con_app <- abrir_app_db()
  onStop(function() {
    try(DBI::dbDisconnect(con_sjr), silent = TRUE)
    try(DBI::dbDisconnect(con_app), silent = TRUE)
  })

  ctx <- list(con_sjr = con_sjr, con_app = con_app,
              ir_para = function(aba) nav_select("nav", aba, session = session),
              versao_global = reactiveVal(0))

  observeEvent(res_auth$user, once = TRUE, {
    papel <- res_auth$papel
    output$quem <- renderUI(span(class = "navbar-text small text-white-50",
      icon(if (papel == "comissao") "users" else "user"), " ", res_auth$user,
      if (papel == "comissao") " (comissão)" else " (docente)"))
    if (identical(papel, "docente")) {
      for (a in ABAS_COMISSAO) nav_remove("nav", a, session = session)
      did <- as.integer(res_auth$docente_id)
      docente_server("doc", ctx, reactive(did))
      nav_select("nav", "doc_sol", session = session)
    } else if (identical(papel, "comissao")) {
      for (a in ABAS_DOCENTE) nav_remove("nav", a, session = session)
      comissao_server("com", ctx, reactive(TRUE))
      nav_select("nav", "com_lista", session = session)
    }
    revistas_server("rev", ctx)
    regras_server("regras", ctx)
  })
}

shinyApp(ui, server)
