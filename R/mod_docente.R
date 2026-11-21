# =====================================================================
# mod_docente.R  -  Telas do DOCENTE (um modulo, varias abas, mesmo id)
#   doc_solicitacoes_ui / doc_avaliacao_ui / doc_periodo_ui / doc_artigos_ui
#   docente_server(id, ctx, docente_id)
# =====================================================================

doc_banner <- function(ns, nome) shiny::uiOutput(ns(nome))

# Mostra o conteudo so quando a solicitacao ativa esta em rascunho
so_rascunho <- function(ns, ..., aviso = TRUE) {
  shiny::tagList(
    shiny::conditionalPanel("output.editavel", ns = ns, ...),
    if (aviso) shiny::conditionalPanel("!output.editavel", ns = ns,
      shiny::div(class = "alert alert-secondary", shiny::icon("lock"),
        " Esta solicitação já foi enviada: não é possível alterar. Para mudar algo, peça à comissão que a devolva ou crie uma nova solicitação.")))
}

doc_solicitacoes_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      bslib::card_header(class = "d-flex flex-wrap justify-content-between align-items-center gap-2",
        shiny::span("Minhas solicitações"),
        shiny::div(class = "d-flex flex-wrap gap-2",
          shiny::actionButton(ns("nova"), "Nova solicitação", icon = shiny::icon("plus"), class = "btn-primary btn-sm"),
          shiny::actionButton(ns("enviar"), "Enviar para a comissão", icon = shiny::icon("paper-plane"), class = "btn-outline-primary btn-sm"),
          shiny::actionButton(ns("excluir_sol"), "Excluir rascunho", icon = shiny::icon("trash"), class = "btn-outline-danger btn-sm"))),
      bslib::card_body(fillable = FALSE, 
        shiny::p(class = "small-muted mb-1", "Clique em uma linha para torná-la a solicitação ativa nas outras abas."),
        DT::DTOutput(ns("tab_sol"), fill = FALSE))),
    bslib::card(
      bslib::card_header("Histórico de avaliações da solicitação ativa"),
      bslib::card_body(fillable = FALSE, 
        shiny::p(class = "small-muted mb-1",
          "Cada linha é um retrato: guarda o resultado exatamente como foi calculado naquele dia, ",
          "mesmo que o SJR seja atualizado depois. Clique em uma linha para abrir o retrato."),
        DT::DTOutput(ns("tab_hist"), fill = FALSE)))
  )
}

doc_avaliacao_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    doc_banner(ns, "banner_av"),
    shiny::div(class = "d-flex flex-wrap gap-2 align-items-center mb-3",
      so_rascunho(ns, shiny::actionButton(ns("calcular"), "Calcular avaliação agora", icon = shiny::icon("calculator"), class = "btn-primary"), aviso = FALSE),
      shiny::uiOutput(ns("info_retrato"), inline = TRUE)),
    avaliacao_ui(ns("av"))
  )
}

doc_periodo_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    doc_banner(ns, "banner_per"),
    bslib::layout_columns(col_widths = c(12, 12), fill = FALSE,
      bslib::layout_columns(col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
        bslib::card(
          bslib::card_header("1. Informe as licenças"),
          bslib::card_body(fillable = FALSE, so_rascunho(ns,
            shiny::radioButtons(ns("lic_tipo"), "Situação", inline = FALSE,
              choices = c("Licença de parentalidade (maternidade, paternidade, adoção)" = "parentalidade",
                          "Licença médica" = "medica",
                          "Docente, cônjuge ou dependente com deficiência (PcD) ou que exige cuidados contínuos" = "pcd")),
            shiny::dateInput(ns("lic_ini"), "Data de início", value = NULL, language = "pt-BR", format = "dd/mm/yyyy"),
            shiny::conditionalPanel(sprintf("input['%s'] != 'pcd'", ns("lic_tipo")),
              shiny::dateInput(ns("lic_fim"), "Data de término", value = NULL, language = "pt-BR", format = "dd/mm/yyyy")),
            shiny::conditionalPanel(sprintf("input['%s'] == 'pcd'", ns("lic_tipo")),
              shiny::p(class = "small-muted", "Para PcD a data de término é opcional (situação contínua).")),
            shiny::textAreaInput(ns("lic_obs"), "Comprovação (descreva o documento; não há envio de arquivo)",
                                 rows = 2, placeholder = "Ex.: declaração do RH nº ..."),
            shiny::actionButton(ns("lic_add"), "Adicionar licença", icon = shiny::icon("plus"), class = "btn-primary")))),
        bslib::card(
          bslib::card_header("Licenças cadastradas"),
          bslib::card_body(fillable = FALSE, 
            DT::DTOutput(ns("tab_lic"), fill = FALSE),
            so_rascunho(ns, shiny::actionButton(ns("lic_del"), "Excluir licença selecionada", icon = shiny::icon("trash"),
                                class = "btn-outline-danger btn-sm mt-2"), aviso = FALSE)))),
      bslib::layout_columns(col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
        bslib::card(
          bslib::card_header("2. Período calculado (Artigo Y)"),
          bslib::card_body(fillable = FALSE, 
            shiny::p(class = "small-muted",
              "Preenchido automaticamente a partir das licenças cadastradas. ",
              "Licenças médicas: soma dos meses entre as datas de cada licença."),
            shiny::tags$fieldset(disabled = NA, `aria-describedby` = ns("per_ajuda"), periodo_ui(ns("per"))),
            shiny::p(id = ns("per_ajuda"), class = "small-muted",
              "Campos bloqueados: para mudar, cadastre ou exclua licenças ao lado."))),
        bslib::card(
          bslib::card_header("Motivo / anos"),
          bslib::card_body(fillable = FALSE, shiny::uiOutput(ns("tab_motivo")))))
    )
  )
}

doc_artigos_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    doc_banner(ns, "banner_art"),
    so_rascunho(ns, bslib::navset_card_tab(id = ns("aba_cad"), title = "Cadastrar artigos",
      bslib::nav_panel("Manual", icon = shiny::icon("keyboard"),
        bslib::layout_columns(col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
          shiny::div(
            shiny::textInput(ns("m_titulo"), "Título do artigo", width = "100%"),
            shiny::numericInput(ns("m_ano"), "Ano de publicação", value = NA, min = 1950, max = 2100, step = 1),
            shiny::textInput(ns("m_revista"), "Revista (nome como no SJR)", width = "100%"),
            shiny::textInput(ns("m_issn"), "ISSN (opcional, recomendado)", placeholder = "1234-5678"),
            shiny::actionButton(ns("m_add"), "Adicionar artigo", icon = shiny::icon("plus"), class = "btn-primary")),
          shiny::div(
            shiny::h3(class = "h6", "Conferência no SJR"),
            shiny::p(class = "small-muted", "Enquanto você digita a revista, mostramos as revistas parecidas do SJR."),
            shiny::uiOutput(ns("m_preview"))))),
      bslib::nav_panel("Colar lista", icon = shiny::icon("paste"),
        shiny::p(class = "small-muted", "Um artigo por linha: ", shiny::code("Título; Ano; Revista; ISSN"),
                 ". Também aceita TAB (colado de planilha)."),
        shiny::textAreaInput(ns("lista"), NULL, rows = 6, width = "100%",
          placeholder = "Modelos de mistura em alta dimensão; 2024; Journal of Applied Statistics; 0266-4763"),
        shiny::actionButton(ns("lista_add"), "Importar lista", icon = shiny::icon("file-import"), class = "btn-primary")),
      bslib::nav_panel("Arquivo CSV", icon = shiny::icon("file-csv"),
        shiny::p(class = "small-muted", "Colunas: ", shiny::code("titulo, ano, revista, issn"),
                 " (separador ; ou ,; UTF-8)."),
        shiny::fileInput(ns("csv"), NULL, accept = c(".csv", "text/csv"), buttonLabel = "Escolher CSV...",
                         placeholder = "nenhum arquivo"),
        shiny::actionButton(ns("csv_add"), "Importar CSV", icon = shiny::icon("file-import"), class = "btn-primary")),
      bslib::nav_panel("Currículo Lattes", icon = shiny::icon("file-code"),
        shiny::tags$ol(class = "small mb-2",
          shiny::tags$li("Abra seu currículo em ", shiny::tags$a(href = "https://lattes.cnpq.br", target = "_blank",
                         rel = "noopener", "lattes.cnpq.br"), " (Atualizar currículo)."),
          shiny::tags$li("No menu, use ", shiny::strong("Exportar"), " e escolha o formato ", shiny::strong("XML"),
                         ". O navegador baixa um arquivo .zip."),
          shiny::tags$li("Envie o .zip aqui (ou o .xml de dentro dele). Só os ", shiny::em("artigos publicados"),
                         " são lidos; você escolhe quais cadastrar.")),
        shiny::div(class = "alert alert-warning small py-2", shiny::icon("user-shield"),
          " Demonstração: o arquivo é lido na hora e não é guardado. Ficam gravados só os artigos que você cadastrar ",
          "(título, ano, revista, ISSN). Não envie currículo de outra pessoa."),
        shiny::fileInput(ns("xml"), NULL, accept = c(".zip", ".xml"), buttonLabel = "Escolher arquivo...",
                         placeholder = "curriculo.zip ou curriculo.xml"),
        shiny::uiOutput(ns("xml_info")),
        DT::DTOutput(ns("xml_prev"), fill = FALSE),
        shiny::uiOutput(ns("xml_botao")))
    )),
    shiny::uiOutput(ns("import_res")),
    bslib::card(
      bslib::card_header("Artigos cadastrados nesta solicitação"),
      bslib::card_body(fillable = FALSE, 
        shiny::p(class = "small-muted mb-1",
          "Duplicatas (mesmo título normalizado e mesmo ano) não são cadastradas duas vezes."),
        DT::DTOutput(ns("tab_art"), fill = FALSE),
        so_rascunho(ns, shiny::actionButton(ns("art_del"), "Excluir artigos selecionados", icon = shiny::icon("trash"),
                            class = "btn-outline-danger btn-sm mt-2"), aviso = FALSE)))
  )
}

# ---------------------------------------------------------------------
docente_server <- function(id, ctx, docente_id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    app <- ctx$con_app; sjr <- ctx$con_sjr
    versao <- shiny::reactiveVal(0)
    mexeu  <- function() versao(versao() + 1)
    avisar <- function(msg, tipo = "message") shiny::showNotification(msg, type = tipo, duration = 6)
    tentar <- function(expr, ok = NULL) {
      tryCatch({ force(expr); if (!is.null(ok)) avisar(ok); mexeu(); TRUE },
               error = function(e) { avisar(conditionMessage(e), "error"); FALSE })
    }

    sols <- shiny::reactive({ versao(); listar_solicitacoes(app, docente_id()) })
    sol_ativa <- shiny::reactiveVal(NULL)
    retrato_id <- shiny::reactiveVal(NULL)

    # escolhe a solicitacao ativa padrao (a mais recente)
    shiny::observe({
      s <- sols()
      if (is.null(sol_ativa()) || !sol_ativa() %in% s$id) sol_ativa(if (nrow(s)) s$id[1] else NULL)
    })
    sol <- shiny::reactive({
      versao(); shiny::req(sol_ativa())
      s <- obter_solicitacao(app, sol_ativa())
      shiny::req(nrow(s) == 1, s$docente_id == docente_id())   # so as proprias
      s
    })
    editavel <- shiny::reactive(isTRUE(sol()$status == "rascunho"))
    output$editavel <- shiny::reactive(editavel())
    shiny::outputOptions(output, "editavel", suspendWhenHidden = FALSE)

    banner <- function() {
      s <- sols()
      if (!nrow(s)) return(shiny::div(class = "alert alert-info",
        "Você ainda não tem solicitações. Vá em ", shiny::strong("Minhas solicitações"), " e clique em ",
        shiny::strong("Nova solicitação"), "."))
      x <- sol()
      shiny::div(class = "sol-ativa", role = "status",
        shiny::strong(sprintf("Solicitação #%d", x$id)), badge_status(x$status),
        shiny::span(class = "small-muted", sprintf("pedido em %s · ano final da base: %d", fmt_data(x$data_pedido), x$ano_fim_base)),
        if (!editavel()) shiny::span(class = "small text-secondary", shiny::icon("lock"),
                                      " Enviada: somente leitura."))
    }
    output$banner_av  <- shiny::renderUI(banner())
    output$banner_per <- shiny::renderUI(banner())
    output$banner_art <- shiny::renderUI(banner())

    # ---- Minhas solicitacoes ----------------------------------------
    output$tab_sol <- DT::renderDT({
      s <- sols()
      df <- data.frame(
        `Nº` = s$id, `Pedido` = fmt_data(s$data_pedido), Status = html_status(s$status),
        Artigos = s$n_artigos, `Licenças` = s$n_licencas,
        `Última soma (p)` = fmt_p(s$soma), `Situação` = html_situacao(s$situacao),
        `Calculada em` = ifelse(is.na(s$calculada_em), "–", fmt_data_hora(s$calculada_em)),
        check.names = FALSE)
      sel <- which(s$id == shiny::isolate(sol_ativa()))
      tabela_dt(df, escape = FALSE, selecao = list(mode = "single", selected = sel))
    })
    shiny::observeEvent(input$tab_sol_rows_selected, {
      s <- sols(); i <- input$tab_sol_rows_selected
      if (length(i)) { sol_ativa(s$id[i]); retrato_id(NULL) }
    })
    shiny::observeEvent(input$nova, {
      ano_fim <- as.integer(ler_parametros(sjr)$ano_fim)
      if (any(sols()$status == "rascunho")) {
        avisar("Você já tem um rascunho aberto; foi criada outra solicitação mesmo assim.", "warning")
      }
      tentar({ id <- criar_solicitacao(app, docente_id(), ano_fim); sol_ativa(id); retrato_id(NULL) },
             "Solicitação criada (rascunho).")
    })
    shiny::observeEvent(input$excluir_sol, {
      shiny::req(sol())
      if (!editavel()) return(avisar("Só é possível excluir rascunhos.", "warning"))
      shiny::showModal(shiny::modalDialog(title = "Excluir rascunho?",
        sprintf("A solicitação #%d e seus artigos, licenças e retratos serão apagados.", sol()$id),
        footer = shiny::tagList(shiny::modalButton("Cancelar"),
                                shiny::actionButton(ns("excluir_ok"), "Excluir", class = "btn-danger"))))
    })
    shiny::observeEvent(input$excluir_ok, {
      shiny::removeModal()
      tentar({ excluir_solicitacao(app, sol()$id); sol_ativa(NULL) }, "Rascunho excluído.")
    })
    shiny::observeEvent(input$enviar, {
      shiny::req(sol())
      if (!editavel()) return(avisar("Esta solicitação já foi enviada.", "warning"))
      if (sol_n_artigos() == 0) return(avisar("Cadastre ao menos um artigo antes de enviar.", "warning"))
      shiny::showModal(shiny::modalDialog(title = "Enviar para a comissão?",
        "Antes de enviar, o sistema calcula um retrato atualizado da avaliação. ",
        "Depois do envio, artigos e licenças não podem mais ser alterados.",
        footer = shiny::tagList(shiny::modalButton("Cancelar"),
                                shiny::actionButton(ns("enviar_ok"), "Calcular e enviar", class = "btn-primary"))))
    })
    sol_n_artigos <- shiny::reactive({ versao(); nrow(listar_artigos(app, sol()$id)) })
    shiny::observeEvent(input$enviar_ok, {
      shiny::removeModal()
      tentar({ avaliar_solicitacao(app, sjr, sol()$id); mudar_status(app, sol()$id, "enviada"); retrato_id(NULL) },
             "Solicitação enviada para a comissão.")
    })

    hist <- shiny::reactive({ versao(); shiny::req(sol()); listar_avaliacoes(app, sol()$id) })
    output$tab_hist <- DT::renderDT({
      h <- hist()
      df <- data.frame(`Calculada em` = fmt_data_hora(h$calculada_em),
        `Janela` = sprintf("%d–%d (%d anos)", h$ano_ini, h$ano_fim, h$n_anos),
        `Soma (p, 0–4)` = fmt_p(h$soma), Meta = fmt_p(h$meta), Resultado = html_situacao(h$situacao),
        Tipo = ifelse(h$oficial == 1, "Oficial (comissão)", "Simulação do docente"),
        `Base SJR` = h$versao_banco, check.names = FALSE)
      tabela_dt(df, escape = FALSE, pagina = 5)
    })
    shiny::observeEvent(input$tab_hist_rows_selected, {
      i <- input$tab_hist_rows_selected
      if (length(i)) { retrato_id(hist()$id[i]); ctx$ir_para("doc_avaliacao") }
    })

    # ---- Avaliacao ---------------------------------------------------
    retrato <- shiny::reactive({
      versao(); shiny::req(sol())
      if (!is.null(retrato_id())) {
        r <- obter_retrato(app, retrato_id())
        if (!is.null(r) && r$solicitacao_id == sol()$id) return(r)
      }
      ultimo_retrato(app, sol()$id)
    })
    output$info_retrato <- shiny::renderUI({
      r <- tryCatch(retrato(), error = function(e) NULL)
      ult <- tryCatch(hist(), error = function(e) NULL)
      if (!editavel()) return(shiny::span(class = "small-muted", shiny::icon("lock"),
        " Solicitação enviada: o cálculo fica com a comissão."))
      if (!is.null(r) && !is.null(ult) && nrow(ult) && r$avaliacao_id != ult$id[1]) {
        return(shiny::span(class = "badge text-bg-warning", "Você está vendo um retrato antigo"))
      }
      shiny::span(class = "small-muted", "Cada cálculo grava um novo retrato no histórico.")
    })
    shiny::observeEvent(input$calcular, {
      shiny::req(sol())
      if (!editavel()) return(avisar("Solicitação enviada: não é possível recalcular.", "warning"))
      tentar({ avaliar_solicitacao(app, sjr, sol()$id); retrato_id(NULL) }, "Avaliação calculada e gravada.")
    })
    avaliacao_server("av", list(con_sjr = sjr, con_app = app, ao_confirmar = function(art_id, sid) {
      tentar({
        confirmar_revista(app, art_id, sid)
        avaliar_solicitacao(app, sjr, sol()$id); retrato_id(NULL)
      }, "Revista confirmada; avaliação recalculada.")
    }), retrato, editavel)

    # ---- Periodo / Artigo Y -----------------------------------------
    lics <- shiny::reactive({ versao(); shiny::req(sol()); listar_licencas(app, sol()$id) })
    per <- periodo_server("per", sjr)
    shiny::observe({
      rl <- resumo_licencas(lics())
      shiny::updateNumericInput(session, "per-n_parentalidade", value = rl$n_parentalidade)
      shiny::updateCheckboxInput(session, "per-pcd", value = rl$pcd)
      shiny::updateNumericInput(session, "per-meses_medica", value = sum(rl$meses_licenca_medica, na.rm = TRUE))
    })
    output$tab_lic <- DT::renderDT({
      l <- lics()
      df <- data.frame(Tipo = unname(ROTULO_LICENCA[l$tipo]), `Início` = fmt_data(l$data_inicio),
        Fim = ifelse(is.na(l$data_fim), "–", fmt_data(l$data_fim)),
        Meses = ifelse(l$tipo == "medica", meses_entre_datas(l$data_inicio, l$data_fim), NA),
        `Comprovação` = ifelse(is.na(l$observacao), "–", l$observacao), check.names = FALSE)
      tabela_dt(df, pagina = 5)
    })
    shiny::observeEvent(input$lic_add, {
      fim <- if (input$lic_tipo == "pcd") NA else input$lic_fim
      tentar(adicionar_licenca(app, sol()$id, input$lic_tipo, input$lic_ini,
                               if (length(fim)) fim else NA, input$lic_obs), "Licença adicionada.")
    })
    shiny::observeEvent(input$lic_del, {
      i <- input$tab_lic_rows_selected
      if (!length(i)) return(avisar("Selecione uma licença na tabela.", "warning"))
      tentar(excluir_licenca(app, lics()$id[i]), "Licença excluída.")
    })
    output$tab_motivo <- shiny::renderUI({
      p <- per()
      if (!p$valido) return(shiny::div(class = "alert alert-danger", lapply(p$erros, shiny::p)))
      d <- p$detalhe
      shiny::tagList(
        tabela_html(data.frame(Motivo = d$motivo, Anos = formatC(d$anos, format = "fg", decimal.mark = ","),
                               check.names = FALSE), num = "Anos"),
        shiny::p(class = "mt-3 fw-semibold",
          sprintf("Período de análise usado: %d anos (%d a %d).", p$n_anos, p$ano_ini, p$ano_fim)),
        lapply(p$avisos, function(a) shiny::div(class = "alert alert-warning py-2", shiny::icon("triangle-exclamation"), " ", a)),
        shiny::p(class = "small-muted",
          "Regras em uso (pendentes de confirmação): +2 anos por licença de parentalidade, +1 ano para PcD, ",
          "licença médica pelo tempo da licença; as situações somam, sem teto, e o total é arredondado ",
          "para cima em anos inteiros. A base SJR só cobre 2020–2025 (máximo 6 anos)."))
    })

    # ---- Artigos -----------------------------------------------------
    arts <- shiny::reactive({ versao(); shiny::req(sol()); listar_artigos(app, sol()$id) })
    output$tab_art <- DT::renderDT({
      a <- arts()
      df <- data.frame(`Título` = a$titulo, Ano = a$ano,
        `Revista informada` = ifelse(is.na(a$revista_digitada), "–", a$revista_digitada),
        ISSN = ifelse(is.na(a$issn), "–", fmt_issn(a$issn)),
        `Revista confirmada` = ifelse(a$confirmado_manual == 1, sprintf("sim (sourceid %.0f)", a$sourceid), "–"),
        check.names = FALSE)
      tabela_dt(df, selecao = "multiple")
    })
    resultado_import <- shiny::reactiveVal(NULL)
    output$import_res <- shiny::renderUI({
      r <- resultado_import(); if (is.null(r)) return(NULL)
      shiny::div(class = paste("alert", if (r$inseridos > 0) "alert-success" else "alert-warning"), role = "status",
        sprintf("%d artigo(s) cadastrados · %d duplicado(s) ignorado(s) · %d inválido(s).",
                r$inseridos, r$duplicados, r$invalidos),
        if (nrow(r$rejeitados)) shiny::tags$ul(class = "small mb-0",
          lapply(seq_len(nrow(r$rejeitados)), function(i)
            shiny::tags$li(sprintf("Linha %d (%s): %s", r$rejeitados$linha[i],
                                   substr(r$rejeitados$titulo[i], 1, 60), r$rejeitados$motivo[i])))))
    })
    importar <- function(df) {
      if (!editavel()) return(avisar("Solicitação enviada: não é possível alterar.", "warning"))
      if (!nrow(df)) return(avisar("Nenhum artigo encontrado no conteúdo enviado.", "warning"))
      tentar(resultado_import(adicionar_artigos(app, sol()$id, df)))
    }
    shiny::observeEvent(input$m_add, {
      importar(data.frame(titulo = input$m_titulo, ano = input$m_ano %|na|% NA,
                          revista = input$m_revista, issn = input$m_issn, stringsAsFactors = FALSE))
      if (isTRUE(resultado_import()$inseridos == 1)) {
        shiny::updateTextInput(session, "m_titulo", value = "")
        shiny::updateTextInput(session, "m_issn", value = "")
      }
    })
    shiny::observeEvent(input$lista_add, importar(ler_lista_colada(input$lista)))
    shiny::observeEvent(input$csv_add, {
      shiny::req(input$csv)
      df <- tryCatch(ler_csv_artigos(input$csv$datapath),
                     error = function(e) { avisar(paste("CSV ilegível:", conditionMessage(e)), "error"); NULL })
      if (!is.null(df)) importar(df)
    })
    # ---- Lattes: le, mostra a previa e importa so os selecionados ----
    lattes <- shiny::reactiveVal(NULL)
    jan_max <- listar_janelas(sjr)
    ano_ini_max <- min(jan_max$ano_ini)
    shiny::observeEvent(input$xml, {
      df <- tryCatch(ler_lattes_xml(input$xml$datapath),
                     error = function(e) { avisar(conditionMessage(e), "error"); NULL })
      if (is.null(df)) return(lattes(NULL))
      prev <- previsualizar_artigos(sjr, df, n_anos = max(jan_max$n_anos))
      attr(prev, "docente") <- attr(df, "docente")
      lattes(prev)
    })
    output$xml_info <- shiny::renderUI({
      d <- lattes(); shiny::req(d)
      inf <- attr(d, "docente")
      shiny::div(class = "small mb-2",
        shiny::strong(if (nzchar(inf$nome)) inf$nome else "Currículo Lattes"),
        if (nzchar(inf$lattes_id)) sprintf(" · ID Lattes %s", inf$lattes_id),
        if (nzchar(inf$atualizado_em) && nchar(inf$atualizado_em) == 8)
          sprintf(" · atualizado em %s/%s/%s", substr(inf$atualizado_em, 1, 2), substr(inf$atualizado_em, 3, 4),
                  substr(inf$atualizado_em, 5, 8)),
        shiny::br(),
        sprintf("%d artigo(s) publicado(s) no currículo; %d encontrado(s) no SJR. ", nrow(d), sum(d$sjr != "não encontrada")),
        sprintf("Pré-selecionados: os de %d em diante (a base SJR começa em %d). Clique nas linhas para mudar.",
                ano_ini_max, ano_ini_max))
    })
    output$xml_prev <- DT::renderDT({
      d <- lattes(); shiny::req(d)
      sel <- which(suppressWarnings(as.integer(d$ano)) >= ano_ini_max)
      df <- data.frame(`Título` = d$titulo, Ano = d$ano, `Revista (Lattes)` = d$revista,
        ISSN = ifelse(nzchar(d$issn), fmt_issn(normalizar_issn(d$issn)), "–"),
        `No SJR?` = d$sjr, `p máx. 2020–2025 (0–1)` = fmt_p(d$p), check.names = FALSE)
      tabela_dt(df, selecao = list(mode = "multiple", selected = sel), pagina = 10)
    })
    output$xml_botao <- shiny::renderUI({
      shiny::req(lattes())
      n <- length(input$xml_prev_rows_selected)
      shiny::actionButton(ns("xml_add"), sprintf("Cadastrar %d artigo(s) selecionado(s)", n),
                          icon = shiny::icon("file-import"), class = "btn-primary mt-2")
    })
    shiny::observeEvent(input$xml_add, {
      d <- lattes(); i <- input$xml_prev_rows_selected
      if (is.null(d) || !length(i)) return(avisar("Selecione ao menos um artigo.", "warning"))
      importar(d[i, c("titulo", "ano", "revista", "issn")])
    })
    shiny::observeEvent(input$art_del, {
      i <- input$tab_art_rows_selected
      if (!length(i)) return(avisar("Selecione artigos na tabela.", "warning"))
      tentar(for (id in arts()$id[i]) excluir_artigo(app, id), sprintf("%d artigo(s) excluído(s).", length(i)))
    })
    revista_digitada <- shiny::debounce(shiny::reactive(input$m_revista), 400)
    output$m_preview <- shiny::renderUI({
      txt <- revista_digitada(); issn <- normalizar_issn(input$m_issn)
      res <- if (nzchar(issn)) {
        DBI::dbGetQuery(sjr, "SELECT v.titulo, v.percentil_max, v.p, v.melhor_area FROM v_docente v
                              JOIN issn i ON i.sourceid = v.sourceid WHERE i.issn = ?", params = list(issn))
      } else if (nchar(trimws(txt)) >= 3) buscar_revista(sjr, txt, limite = 5) else data.frame()
      if (!nrow(res)) return(shiny::p(class = "small-muted",
        if (nzchar(issn) || nchar(trimws(txt)) >= 3) "Nenhuma revista encontrada no SJR." else "Digite ao menos 3 letras ou um ISSN."))
      tabela_html(data.frame(Revista = res$titulo, `Percentil (0–100)` = fmt_pct(res$percentil_max),
                             `p (0–1)` = fmt_p(res$p), `Melhor área` = ifelse(is.na(res$melhor_area), "–", res$melhor_area), check.names = FALSE),
                  num = c("Percentil (0–100)", "p (0–1)"))
    })
  })
}
