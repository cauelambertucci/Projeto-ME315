# =====================================================================
# modulo_periodo.R  -  Caixinha de tela (Shiny) do Artigo Y
# Pede as licencas, mostra o periodo calculado e devolve um reactive.
#
# Precisa de: shiny, R/funcoes_banco.R e R/periodo_artigo_y.R carregados.
#
# No app:
#   ui:      periodo_ui("per")
#   server:  con <- conectar_banco("sjr.db")            # uma conexao por sessao
#            onStop(function() DBI::dbDisconnect(con))
#            per <- periodo_server("per", con)           # reactive
#            observeEvent(input$avaliar, {
#              p <- per()
#              if (!p$valido) return()                   # a caixinha ja mostra o erro
#              res <- avaliar_artigos(con, artigos, n_anos = p$n_anos)
#              ...
#            })
# =====================================================================

periodo_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Ampliação do período (Artigo Y)"),
    shiny::numericInput(ns("n_parentalidade"),
      "Licenças de parentalidade (maternidade, paternidade, adoção): quantas?",
      value = 0, min = 0, step = 1),
    shiny::checkboxInput(ns("pcd"),
      "Docente com deficiência, ou cônjuge/dependente com deficiência ou condição que exige cuidados contínuos",
      value = FALSE),
    shiny::numericInput(ns("meses_medica"),
      "Licença médica: duração total (em meses)",
      value = 0, min = 0, step = 1),
    shiny::uiOutput(ns("resumo"))
  )
}

periodo_server <- function(id, con) {
  shiny::moduleServer(id, function(input, output, session) {

    periodo <- shiny::reactive({
      calcular_periodo(con,
                       n_parentalidade      = input$n_parentalidade,
                       pcd                  = input$pcd,
                       meses_licenca_medica = input$meses_medica)
    })

    output$resumo <- shiny::renderUI({
      p <- periodo()
      if (!p$valido) {
        return(shiny::tags$div(style = "color:#b00020;", lapply(p$erros, shiny::tags$p)))
      }
      itens <- lapply(seq_len(nrow(p$detalhe)), function(i) {
        shiny::tags$li(sprintf("%s: %s anos", p$detalhe$motivo[i],
                               format(p$detalhe$anos[i], decimal.mark = ",")))
      })
      shiny::tagList(
        shiny::tags$p(shiny::tags$strong(
          sprintf("Período de análise: %d anos (%d a %d)", p$n_anos, p$ano_ini, p$ano_fim))),
        shiny::tags$ul(itens),
        lapply(p$avisos, function(a) shiny::tags$p(style = "color:#8a6d00;", a))
      )
    })

    periodo
  })
}
