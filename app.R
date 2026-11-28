# =====================================================================
# app.R  -  Credenciamento Docente PPGE: avaliacao pelo curriculo Lattes
#   shiny::runApp()
# O docente envia o Lattes, informa licencas (Artigo Y) e ve o resultado
# do Artigo X com o ranking dos artigos. Nada e gravado.
# =====================================================================

library(shiny)
library(bslib)
library(DT)

source("R/funcoes_banco.R", encoding = "UTF-8")
source("R/periodo_artigo_y.R", encoding = "UTF-8")
source("R/modulo_periodo.R", encoding = "UTF-8")
source("R/lattes.R", encoding = "UTF-8")
source("R/ranking.R", encoding = "UTF-8")
source("R/baixar_banco.R", encoding = "UTF-8")

baixar_banco()   # no servidor o banco_edital.db nao vem do git

num <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",")

# opcoes comuns das tabelas (textos em portugues)
opcoes_dt <- list(pageLength = 25, scrollX = TRUE, language = list(
  search = "Buscar:", info = "_START_ a _END_ de _TOTAL_", infoEmpty = "Nenhum registro",
  infoFiltered = "(de _MAX_)", lengthMenu = "Mostrar _MENU_", zeroRecords = "Nada encontrado",
  paginate = list(previous = "Anterior", `next` = "Próximo")))

# ---------------------------------------------------------------------
# Interface
# ---------------------------------------------------------------------
tema <- bs_theme(version = 5, primary = "#1F3A5F", base_font = font_collection("system-ui", "sans-serif"))

aba_avaliar <- layout_sidebar(
  sidebar = sidebar(width = 340,
    h5("Currículo Lattes"),
    p(class = "small", "No Lattes: Exportar > XML. Envie o .zip baixado."),
    fileInput("arquivo", NULL, accept = c(".zip", ".xml"), buttonLabel = "Escolher..."),
    p(class = "small text-muted", "O arquivo é lido e descartado; nada é gravado."),
    hr(),
    periodo_ui("per")
  ),
  uiOutput("resumo"),
  card(card_header("Ranking dos artigos"), DTOutput("tabela", fill = FALSE))
)

aba_revistas <- card(
  card_header("Ranking de revistas (SJR)"),
  layout_columns(
    selectInput("area", "Área", choices = NULL),
    selectInput("janela", "Período", choices = NULL)
  ),
  p(class = "small text-muted",
    "Todas as áreas: ranking global pela nota p do edital (maior percentil entre áreas e anos); ",
    "empate desfeito pelo maior SJR do período. Uma área: ranking pelo percentil nessa área."),
  DTOutput("revistas", fill = FALSE)
)

aba_regras <- card(card_header("Regras do edital e premissas"), card_body(
  h5("Artigo X"),
  p("Soma de p de até 4 artigos, contando só revistas com p ≥ 0,25; meta: soma ≥ 1,25 (máximo 4,0)."),
  p("p = maior percentil da revista entre todas as áreas e anos do período, em escala 0 a 1 ",
    "(percentil 87,3 vira p = 0,873)."),
  h5("Artigo Y"),
  p("Parentalidade: +2 anos por licença. PcD: +1 ano. Licença médica: + a duração. ",
    "As situações somam e o total é arredondado para cima em anos inteiros. A base SJR vai de 2020 a 2025, ",
    "então o período máximo é 6 anos."),
  h5("Premissas ainda não confirmadas"),
  tags$ol(
    tags$li("Maior percentil = melhor revista."),
    tags$li("A meta 1,25 vale para qualquer tamanho de periodo."),
    tags$li("Só conta artigo publicado dentro do período."),
    tags$li("Artigo e revista são ligados pelo ISSN e, se não achar, pelo nome exato da revista. ",
            "ISSN de mais de uma revista: usa a de menor código e avisa."),
    tags$li("Ano final 2025 (último ano completo do SJR)."),
    tags$li("Ranking global: ordena pela nota p e, no empate, pelo maior SJR do período.")
  ),
  tableOutput("parametros")
))

ui <- page_navbar(
  title = "PPGE · Credenciamento", theme = tema, lang = "pt-BR", fillable = FALSE,
  bg = "#1F3A5F", inverse = TRUE,
  nav_panel("Avaliar meu Lattes", aba_avaliar),
  nav_panel("Revistas", aba_revistas),
  nav_panel("Regras", aba_regras)
)

# ---------------------------------------------------------------------
# Servidor
# ---------------------------------------------------------------------
server <- function(input, output, session) {
  con <- conectar_banco()                       # uma conexao por sessao
  onStop(function() DBI::dbDisconnect(con))

  periodo <- periodo_server("per", con)          # Artigo Y (modulo do grupo)

  artigos <- reactive({
    req(input$arquivo)
    tryCatch(ler_lattes(input$arquivo$datapath),
             error = function(e) validate(conditionMessage(e)))
  })

  resultado <- reactive({
    per <- periodo()
    req(per$valido)
    avaliar_lattes(con, artigos(), per$n_anos)
  })

  output$resumo <- renderUI({
    if (is.null(input$arquivo)) {
      return(card(card_body("Envie o currículo Lattes para ver a avaliação.")))
    }
    r <- resultado()$resumo
    det <- resultado()$detalhe
    pontos <- det$pontos[det$pontos > 0]
    conta <- if (length(pontos)) paste(num(pontos), collapse = " + ") else "0"
    cor <- if (r$aprovado) "success" else "danger"
    card(card_body(
      h4(attr(artigos(), "nome")),
      p(sprintf("Período: %d a %d (%d anos) · %d artigos no Lattes, %d no período",
                r$ano_ini, r$ano_fim, r$n_anos, r$n_artigos_lattes, r$n_no_periodo)),
      div(class = "progress position-relative", style = "height: 1.4rem",
          div(class = paste0("progress-bar bg-", cor), style = sprintf("width: %.1f%%", 100 * r$soma / 4),
              num(r$soma)),
          div(title = "meta 1,25", style = "position:absolute; left:31.25%; top:0; bottom:0; width:3px; background:#1D2733")),
      p(class = "small text-muted mt-1", "Escala de 0 a 4,0; a linha preta marca a meta 1,25"),
      h5(sprintf("Soma = %s = %s %s 1,25", conta, num(r$soma), if (r$aprovado) "≥" else "<")),
      h4(class = paste0("text-", cor),
         if (r$aprovado) "Atinge a meta" else sprintf("Faltam %s para a meta", num(r$meta - r$soma))),
      if (r$n_ambiguos > 0) p(class = "text-warning",
         sprintf("Atenção: %d artigo(s) com ISSN ligado a mais de uma revista (coluna Aviso).", r$n_ambiguos))
    ))
  })

  output$tabela <- renderDT({
    d <- resultado()$detalhe
    validate(need(nrow(d) > 0, "Nenhum artigo publicado encontrado no currículo."))
    total <- resultado()$resumo$total_global
    tab <- data.frame(
      `#` = ifelse(d$selecionado == 1, d$pos, ""),
      Título = d$titulo, Ano = d$ano,
      Revista = ifelse(is.na(d$revista_sjr), d$revista, d$revista_sjr),
      `Melhor área` = d$melhor_area,
      `Posição na área` = ifelse(is.na(d$rank_area), "",
                                 sprintf("%d de %d (%d)", d$rank_area, d$n_area, d$ano_max)),
      `Ranking global` = ifelse(is.na(d$rank_global), "", sprintf("%d de %d", d$rank_global, total)),
      `Percentil (0-100)` = d$percentil_max, `p (0-1)` = round(d$p, 3), Pontos = round(d$pontos, 3),
      Situação = d$motivo,
      Aviso = ifelse(d$n_candidatos > 1, sprintf("ISSN de %d revistas", d$n_candidatos), ""),
      check.names = FALSE)
    datatable(tab, rownames = FALSE, selection = "none", options = c(opcoes_dt, ordering = FALSE)) |>
      formatRound("Percentil (0-100)", 1, dec.mark = ",") |>
      formatRound(c("p (0-1)", "Pontos"), 3, dec.mark = ",") |>
      formatStyle("Situação", target = "row",
                  fontWeight = styleEqual("Entra na soma", "bold"),
                  color = styleEqual(c("Fora do período", "Revista não encontrada no SJR",
                                       "Revista sem classificação no período"), rep("#7A8594", 3)))
  })

  # aba Revistas
  janelas <- listar_janelas(con)
  updateSelectInput(session, "janela", selected = 4,
                    choices = setNames(janelas$n_anos, sprintf("%d-%d", janelas$ano_ini, janelas$ano_fim)))
  updateSelectInput(session, "area", choices = c("Todas as áreas (global)" = "", listar_areas(con)$area))
  output$revistas <- renderDT({
    req(input$janela)
    d <- ranking_revistas(con, as.integer(input$janela), input$area)
    names(d) <- c("Posição", "Revista", "Área", "Percentil (0-100)", "p (0-1)", "SJR")
    datatable(d, rownames = FALSE, selection = "none", options = opcoes_dt) |>
      formatRound("Percentil (0-100)", 1, dec.mark = ",") |>
      formatRound(c("p (0-1)", "SJR"), 3, dec.mark = ",")
  })

  output$parametros <- renderTable(
    DBI::dbGetQuery(con, "SELECT chave AS Parâmetro, valor AS Valor, descricao AS Descrição FROM parametro"))
}

shinyApp(ui, server)
