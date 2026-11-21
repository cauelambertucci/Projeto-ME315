# =====================================================================
# mod_lattes.R  -  Pagina "Avaliar meu Lattes" (sem login, nada e gravado)
#   1. envia o .zip/.xml exportado da Plataforma Lattes
#   2. escolhe os artigos (pre-selecionados: os de 2020 em diante)
#   3. informa licencas do Artigo Y (periodo_ui/periodo_server)
#   4. ve o resultado (avaliar_lattes + mod_avaliacao)
# =====================================================================

lattes_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_columns(col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)), fill = FALSE,
      bslib::card(
        bslib::card_header("1. Envie seu currículo Lattes"),
        bslib::card_body(fillable = FALSE,
          shiny::tags$ol(class = "small mb-2",
            shiny::tags$li("Entre em ", shiny::tags$a(href = "https://lattes.cnpq.br", target = "_blank",
                           rel = "noopener", "lattes.cnpq.br"), " e abra seu currículo (Atualizar currículo)."),
            shiny::tags$li("No menu, clique em ", shiny::strong("Exportar"), " e escolha ", shiny::strong("XML"),
                           ". O navegador baixa um arquivo .zip."),
            shiny::tags$li("Envie o .zip aqui (ou o curriculo.xml de dentro dele).")),
          shiny::fileInput(ns("arquivo"), NULL, accept = c(".zip", ".xml"), buttonLabel = "Escolher arquivo...",
                           placeholder = "curriculo.zip ou curriculo.xml", width = "100%"),
          shiny::p(class = "small-muted mb-0", shiny::icon("user-shield"),
            " O arquivo é lido na hora e descartado: nada é gravado no servidor."))),
      bslib::card(
        bslib::card_header("3. Licenças (Artigo Y)"),
        bslib::card_body(fillable = FALSE,
          shiny::p(class = "small-muted", "Só preencha se tiver licença nos últimos anos; elas ampliam o período de análise."),
          periodo_ui(ns("per")),
          shiny::tags$details(
            shiny::tags$summary(class = "small", "Calcular os meses de licença médica pelas datas"),
            shiny::dateRangeInput(ns("med_datas"), NULL, start = NULL, end = NULL, language = "pt-BR",
                                  format = "dd/mm/yyyy", separator = "até"),
            shiny::actionButton(ns("med_usar"), "Usar estas datas", class = "btn-sm btn-outline-primary"))))),
    shiny::uiOutput(ns("passo2")),
    shiny::uiOutput(ns("botao_calcular")),
    shiny::div(id = ns("resultado"), avaliacao_ui(ns("av")))
  )
}

lattes_server <- function(id, con_sjr) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    avisar <- function(msg, tipo = "message") shiny::showNotification(msg, type = tipo, duration = 8)
    jan <- listar_janelas(con_sjr)
    ano_ini_base <- min(jan$ano_ini)

    lattes   <- shiny::reactiveVal(NULL)   # artigos lidos + previa SJR
    conf     <- shiny::reactiveVal(NULL)   # sourceid confirmado por artigo (NA = automatico)
    resultado <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$arquivo, {
      resultado(NULL)
      df <- tryCatch(ler_lattes_xml(input$arquivo$datapath),
                     error = function(e) { avisar(conditionMessage(e), "error"); NULL })
      unlink(input$arquivo$datapath)
      if (is.null(df)) return(lattes(NULL))
      if (!nrow(df)) { avisar("Nenhum artigo publicado encontrado neste currículo.", "warning"); return(lattes(NULL)) }
      prev <- previsualizar_artigos(con_sjr, df, n_anos = max(jan$n_anos))
      attr(prev, "docente") <- attr(df, "docente")
      conf(rep(NA_real_, nrow(prev)))
      lattes(prev)
    })

    output$passo2 <- shiny::renderUI({
      d <- lattes(); if (is.null(d)) return(NULL)
      inf <- attr(d, "docente")
      atual <- if (nchar(inf$atualizado_em) == 8)
        sprintf(" · atualizado em %s/%s/%s", substr(inf$atualizado_em, 1, 2), substr(inf$atualizado_em, 3, 4),
                substr(inf$atualizado_em, 5, 8)) else ""
      bslib::card(
        bslib::card_header("2. Confira os artigos"),
        bslib::card_body(fillable = FALSE,
          shiny::p(class = "mb-1", shiny::strong(if (nzchar(inf$nome)) inf$nome else "Currículo Lattes"), atual),
          shiny::p(class = "small-muted",
            sprintf("%d artigo(s) publicado(s); %d com revista encontrada no SJR. ", nrow(d), sum(d$sjr != "não encontrada")),
            sprintf("Já vêm marcados os de %d em diante (a base SJR começa em %d). Clique nas linhas para marcar ou desmarcar.",
                    ano_ini_base, ano_ini_base)),
          DT::DTOutput(ns("tab"), fill = FALSE)))
    })
    output$tab <- DT::renderDT({
      d <- lattes(); shiny::req(d)
      sel <- which(suppressWarnings(as.integer(d$ano)) >= ano_ini_base)
      df <- data.frame(`Título` = d$titulo, Ano = d$ano, `Revista (no Lattes)` = d$revista,
        ISSN = ifelse(nzchar(d$issn), fmt_issn(normalizar_issn(d$issn)), "–"),
        `No SJR?` = d$sjr, `p máx. 2020–2025 (0–1)` = fmt_p(d$p), check.names = FALSE)
      tabela_dt(df, selecao = list(mode = "multiple", selected = sel), pagina = 10)
    })
    output$botao_calcular <- shiny::renderUI({
      shiny::req(lattes())
      n <- length(input$tab_rows_selected)
      shiny::div(class = "my-3",
        shiny::actionButton(ns("calcular"), sprintf("Calcular avaliação com %d artigo(s)", n),
                            icon = shiny::icon("calculator"), class = "btn-primary btn-lg"))
    })

    per <- periodo_server("per", con_sjr)
    shiny::observeEvent(input$med_usar, {
      d <- input$med_datas
      m <- meses_entre_datas(d[1], d[2])
      if (length(m) != 1 || is.na(m)) return(avisar("Informe início e fim válidos (fim depois do início).", "warning"))
      shiny::updateNumericInput(session, "per-meses_medica", value = m)
      avisar(sprintf("Licença médica: %d mês(es).", as.integer(m)))
    })

    calcular <- function() {
      d <- lattes(); i <- input$tab_rows_selected
      p <- per()
      if (is.null(d)) return(avisar("Envie o currículo primeiro.", "warning"))
      if (!length(i)) return(avisar("Marque ao menos um artigo.", "warning"))
      if (!p$valido) return(avisar(paste(p$erros, collapse = " "), "error"))
      r <- tryCatch(avaliar_lattes(con_sjr, d[i, c("titulo", "ano", "revista", "issn")],
                                   n_parentalidade = input[["per-n_parentalidade"]],
                                   pcd = input[["per-pcd"]],
                                   meses_medica = input[["per-meses_medica"]],
                                   sourceid_conf = conf()[i]),
                    error = function(e) { avisar(conditionMessage(e), "error"); NULL })
      if (is.null(r)) return()
      # artigo_id da avaliacao = posicao entre os selecionados -> linha do Lattes
      attr(r, "linhas") <- i
      resultado(r)
      session$sendCustomMessage("ppge_rolar", ns("resultado"))
    }
    shiny::observeEvent(input$calcular, calcular())

    avaliacao_server("av", list(con_sjr = con_sjr, ao_confirmar = function(artigo_id, sourceid) {
      r <- resultado(); linhas <- attr(r, "linhas")
      cf <- conf(); cf[linhas[artigo_id]] <- sourceid; conf(cf)
      calcular()
      avisar("Revista confirmada; avaliação recalculada.")
    }), resultado, shiny::reactive(TRUE))
  })
}
