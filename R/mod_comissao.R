# =====================================================================
# mod_comissao.R  -  Telas da COMISSAO (lista, detalhe, painel, exportacao)
#   com_lista_ui / com_detalhe_ui / com_painel_ui ; comissao_server(id, ctx)
# =====================================================================

com_lista_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(fillable = FALSE,
    sidebar = bslib::sidebar(title = "Filtros", width = 280, open = "desktop",
      shiny::checkboxGroupInput(ns("f_status"), "Status", choices = c("Rascunho" = "rascunho",
        "Enviada" = "enviada", "Avaliada" = "avaliada"), selected = c("enviada", "avaliada")),
      shiny::checkboxGroupInput(ns("f_sit"), "Situação (último retrato)",
        choices = c("Apto" = "apto", "Quase apto (1,00–1,25)" = "quase apto", "Não apto" = "não apto",
                    "Sem avaliação" = "sem avaliação"),
        selected = c("apto", "quase apto", "não apto", "sem avaliação")),
      shiny::selectInput(ns("f_area"), "Área de atuação", choices = c("Todas" = ""), multiple = FALSE),
      shiny::selectInput(ns("f_ano"), "Ano do pedido", choices = c("Todos" = "")),
      shiny::hr(),
      shiny::downloadButton(ns("csv"), "Exportar CSV", class = "btn-outline-primary btn-sm w-100 mb-2"),
      shiny::downloadButton(ns("csv_art"), "CSV por artigo", class = "btn-outline-primary btn-sm w-100 mb-2"),
      shiny::downloadButton(ns("pdf"), "Exportar PDF", class = "btn-outline-primary btn-sm w-100")),
    bslib::card(
      bslib::card_header("Solicitações de todos os docentes"),
      bslib::card_body(fillable = FALSE, 
        shiny::uiOutput(ns("resumo_filtro")),
        shiny::p(class = "small-muted mb-1", "Clique em uma linha para abrir o detalhe."),
        DT::DTOutput(ns("tab"), fill = FALSE)))
  )
}

com_detalhe_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("cab")),
    bslib::layout_columns(col_widths = bslib::breakpoints(sm = 12, lg = c(8, 4)),
      avaliacao_ui(ns("av")),
      shiny::div(
        bslib::card(bslib::card_header("Ações da comissão"),
          bslib::card_body(fillable = FALSE, 
            shiny::actionButton(ns("oficial"), "Registrar avaliação oficial", icon = shiny::icon("stamp"),
                                class = "btn-primary w-100 mb-2"),
            shiny::p(class = "small-muted", "Recalcula com a base atual, grava um retrato oficial e marca a solicitação como avaliada."),
            shiny::actionButton(ns("devolver"), "Devolver ao docente", icon = shiny::icon("rotate-left"),
                                class = "btn-outline-secondary w-100 mb-2"),
            shiny::p(class = "small-muted mb-0", "Volta para rascunho para o docente corrigir (o histórico é mantido)."))),
        bslib::card(bslib::card_header("Histórico de retratos"),
          bslib::card_body(fillable = FALSE, DT::DTOutput(ns("hist"), fill = FALSE))))))
}

com_painel_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::p(class = "small-muted", "Considera o último retrato de cada solicitação enviada ou avaliada."),
    bslib::layout_columns(col_widths = bslib::breakpoints(sm = 6, lg = 3), fill = FALSE,
      bslib::value_box("Solicitações", shiny::textOutput(ns("vb_n")), showcase = shiny::icon("folder-open"), theme = "primary"),
      bslib::value_box("Aptos", shiny::textOutput(ns("vb_apto")), showcase = shiny::icon("circle-check"), theme = "success"),
      bslib::value_box("Quase aptos", shiny::textOutput(ns("vb_quase")), showcase = shiny::icon("circle-exclamation"), theme = "warning"),
      bslib::value_box("Não aptos", shiny::textOutput(ns("vb_nao")), showcase = shiny::icon("circle-xmark"), theme = "danger")),
    bslib::layout_columns(col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      bslib::card(full_screen = TRUE, bslib::card_header("Ranking de áreas de atuação"),
        bslib::card_body(fillable = FALSE, shiny::plotOutput(ns("g_area"), height = "300px"),
                         shiny::uiOutput(ns("t_area")))),
      bslib::card(full_screen = TRUE, bslib::card_header("Distribuição das somas"),
        bslib::card_body(fillable = FALSE, shiny::plotOutput(ns("g_soma"), height = "300px"),
                         shiny::p(class = "small-muted", "Linha tracejada: meta de 1,25. Faixa clara: “quase apto” (1,00–1,25).")))))
}

# ---------------------------------------------------------------------
comissao_server <- function(id, ctx, eh_comissao) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    app <- ctx$con_app; sjr <- ctx$con_sjr
    versao <- shiny::reactiveVal(0); mexeu <- function() versao(versao() + 1)
    avisar <- function(msg, tipo = "message") shiny::showNotification(msg, type = tipo, duration = 6)

    todas <- shiny::reactive({ versao(); ctx$versao_global(); shiny::req(eh_comissao()); listar_solicitacoes(app) })
    shiny::observe({
      t <- todas()
      shiny::updateSelectInput(session, "f_area", choices = c("Todas" = "", sort(unique(t$area_atuacao))),
                               selected = shiny::isolate(input$f_area))
      shiny::updateSelectInput(session, "f_ano", choices = c("Todos" = "", sort(unique(substr(t$data_pedido, 1, 4)), decreasing = TRUE)),
                               selected = shiny::isolate(input$f_ano))
    })
    filtradas <- shiny::reactive({
      t <- todas()
      t <- t[t$status %in% input$f_status & t$situacao %in% input$f_sit, , drop = FALSE]
      if (nzchar(input$f_area %||% "")) t <- t[t$area_atuacao == input$f_area, , drop = FALSE]
      if (nzchar(input$f_ano %||% ""))  t <- t[substr(t$data_pedido, 1, 4) == input$f_ano, , drop = FALSE]
      t
    })
    tabela_export <- function(t) data.frame(
      solicitacao = t$id, docente = t$docente, area_atuacao = t$area_atuacao, data_pedido = t$data_pedido,
      status = t$status, n_artigos = t$n_artigos, n_licencas = t$n_licencas,
      janela = ifelse(is.na(t$n_anos), NA, sprintf("%d-%d", t$ano_ini, t$ano_fim)),
      n_anos = t$n_anos, soma_p = round(t$soma, 4), meta = t$meta, situacao = t$situacao,
      retrato_em = t$calculada_em, retrato_oficial = ifelse(is.na(t$oficial), NA, t$oficial == 1),
      stringsAsFactors = FALSE)

    output$resumo_filtro <- shiny::renderUI({
      t <- filtradas()
      shiny::p(class = "small", sprintf("%d solicitação(ões) · %d apto(s) · %d quase apto(s) · %d não apto(s) · %d sem avaliação",
        nrow(t), sum(t$situacao == "apto"), sum(t$situacao == "quase apto"),
        sum(t$situacao == "não apto"), sum(t$situacao == "sem avaliação")))
    })
    output$tab <- DT::renderDT({
      t <- filtradas()
      df <- data.frame(`Nº` = t$id, Docente = t$docente, `Área` = t$area_atuacao, Pedido = fmt_data(t$data_pedido),
        Status = html_status(t$status), Artigos = t$n_artigos, `Licenças` = t$n_licencas,
        Janela = ifelse(is.na(t$n_anos), "–", sprintf("%d–%d", t$ano_ini, t$ano_fim)),
        `Soma (p)` = fmt_p(t$soma), `Situação` = html_situacao(t$situacao), check.names = FALSE)
      tabela_dt(df, escape = FALSE, pagina = 15)
    })
    sol_id <- shiny::reactiveVal(NULL)
    retrato_id <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$tab_rows_selected, {
      i <- input$tab_rows_selected
      if (length(i)) { sol_id(filtradas()$id[i]); retrato_id(NULL); ctx$ir_para("com_detalhe") }
    })

    # ---- exportacao -------------------------------------------------
    output$csv <- shiny::downloadHandler(
      filename = function() sprintf("solicitacoes_ppge_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) exportar_csv(tabela_export(filtradas()), file))
    output$csv_art <- shiny::downloadHandler(
      filename = function() sprintf("artigos_avaliados_ppge_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        t <- filtradas()
        linhas <- lapply(t$avaliacao_id[!is.na(t$avaliacao_id)], function(a) {
          r <- obter_retrato(app, a); d <- r$detalhe
          if (!nrow(d)) return(NULL)
          data.frame(solicitacao = r$solicitacao_id, docente = r$solicitacao$docente, retrato_em = r$calculada_em,
            titulo = d$titulo, ano = d$ano, revista_informada = d$revista, issn = d$issn,
            revista_sjr = d$revista_sjr, sourceid = sprintf("%.0f", d$sourceid), metodo = d$metodo,
            percentil_0_100 = d$percentil_max, p_0_1 = round(d$p, 4), melhor_area = d$melhor_area,
            ano_do_maximo = d$ano_percentil_max, situacao = d$situacao, pontos = round(d$pontos, 4),
            motivo = d$motivo, stringsAsFactors = FALSE)
        })
        exportar_csv(do.call(rbind, linhas) %||% data.frame(), file)
      })
    output$pdf <- shiny::downloadHandler(
      filename = function() sprintf("solicitacoes_ppge_%s.pdf", format(Sys.Date(), "%Y%m%d")),
      content = function(file) exportar_pdf(tabela_export(filtradas()), file,
        filtros = sprintf("Status: %s · Área: %s · Ano: %s", paste(input$f_status, collapse = ", "),
                          if (nzchar(input$f_area %||% "")) input$f_area else "todas",
                          if (nzchar(input$f_ano %||% "")) input$f_ano else "todos"),
        versao = versao_banco_edital(sjr)))

    # ---- detalhe ----------------------------------------------------
    sol <- shiny::reactive({ versao(); shiny::req(eh_comissao(), sol_id()); obter_solicitacao(app, sol_id()) })
    hist <- shiny::reactive({ versao(); shiny::req(sol()); listar_avaliacoes(app, sol()$id) })
    output$cab <- shiny::renderUI({
      if (is.null(sol_id())) return(shiny::div(class = "alert alert-info",
        "Escolha uma solicitação na aba ", shiny::strong("Solicitações"), "."))
      s <- sol()
      shiny::div(class = "sol-ativa",
        shiny::strong(sprintf("Solicitação #%d — %s", s$id, s$docente)), badge_status(s$status),
        shiny::span(class = "small-muted", sprintf("%s · pedido em %s · ano final da base %d", s$area_atuacao,
                                                    fmt_data(s$data_pedido), s$ano_fim_base)),
        shiny::span(class = "small text-secondary", shiny::icon("eye"), " Visão somente leitura"))
    })
    retrato <- shiny::reactive({
      versao(); shiny::req(sol())
      if (!is.null(retrato_id())) { r <- obter_retrato(app, retrato_id()); if (!is.null(r)) return(r) }
      ultimo_retrato(app, sol()$id)
    })
    avaliacao_server("av", list(con_sjr = sjr, con_app = app, ao_confirmar = function(...) NULL),
                     retrato, shiny::reactive(FALSE))
    output$hist <- DT::renderDT({
      h <- hist()
      tabela_dt(data.frame(Quando = fmt_data_hora(h$calculada_em), Soma = fmt_p(h$soma),
        `Situação` = html_situacao(h$situacao), Tipo = ifelse(h$oficial == 1, "oficial", "docente"),
        check.names = FALSE), escape = FALSE, pagina = 5)
    })
    shiny::observeEvent(input$hist_rows_selected, {
      i <- input$hist_rows_selected; if (length(i)) retrato_id(hist()$id[i])
    })
    shiny::observeEvent(input$oficial, {
      shiny::req(sol())
      if (sol()$status == "rascunho") return(avisar("Rascunho: o docente ainda não enviou.", "warning"))
      tryCatch({
        avaliar_solicitacao(app, sjr, sol()$id, oficial = TRUE)
        if (sol()$status == "enviada") mudar_status(app, sol()$id, "avaliada")
        retrato_id(NULL); mexeu(); avisar("Avaliação oficial registrada.")
      }, error = function(e) avisar(conditionMessage(e), "error"))
    })
    shiny::observeEvent(input$devolver, {
      shiny::req(sol())
      tryCatch({
        if (sol()$status == "avaliada") mudar_status(app, sol()$id, "enviada")
        mudar_status(app, sol()$id, "rascunho"); mexeu(); avisar("Solicitação devolvida ao docente.")
      }, error = function(e) avisar(conditionMessage(e), "error"))
    })

    # ---- painel -----------------------------------------------------
    base_painel <- shiny::reactive({
      t <- todas(); t[t$status %in% c("enviada", "avaliada") & !is.na(t$soma), , drop = FALSE]
    })
    output$vb_n     <- shiny::renderText(nrow(base_painel()))
    output$vb_apto  <- shiny::renderText(sum(base_painel()$situacao == "apto"))
    output$vb_quase <- shiny::renderText(sum(base_painel()$situacao == "quase apto"))
    output$vb_nao   <- shiny::renderText(sum(base_painel()$situacao == "não apto"))
    ranking <- shiny::reactive({
      b <- base_painel(); shiny::req(nrow(b))
      a <- do.call(rbind, lapply(split(b, b$area_atuacao), function(g) data.frame(
        area = g$area_atuacao[1], n = nrow(g), aptos = sum(g$situacao == "apto"),
        media = mean(g$soma), stringsAsFactors = FALSE)))
      a$taxa <- a$aptos / a$n
      a[order(-a$taxa, -a$media), ]
    })
    output$g_area <- shiny::renderPlot({
      a <- ranking()
      a$area <- factor(a$area, levels = rev(a$area))
      ggplot2::ggplot(a, ggplot2::aes(x = taxa, y = area)) +
        ggplot2::geom_col(fill = "#1F3A5F", width = 0.6) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%d de %d", aptos, n)), hjust = -0.15, size = 3.8, colour = "#1D2733") +
        ggplot2::scale_x_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, 1.15),
                                    breaks = seq(0, 1, .25), expand = c(0, 0)) +
        ggplot2::labs(x = "Aptos (%)", y = NULL) + tema_grafico()
    }, alt = function() {
      a <- ranking(); paste("Ranking de áreas por proporção de aptos:",
                            paste(sprintf("%s %d de %d", a$area, a$aptos, a$n), collapse = "; "))
    })
    output$t_area <- shiny::renderUI({
      a <- ranking()
      tabela_html(data.frame(`Área` = a$area, Pedidos = a$n, Aptos = a$aptos,
        `Soma média (p)` = fmt_p(a$media), check.names = FALSE), num = c("Pedidos", "Aptos", "Soma média (p)"))
    })
    output$g_soma <- shiny::renderPlot({
      b <- base_painel(); shiny::req(nrow(b))
      ggplot2::ggplot(b, ggplot2::aes(x = soma)) +
        ggplot2::annotate("rect", xmin = LIMIAR_QUASE_APTO, xmax = 1.25, ymin = -Inf, ymax = Inf, fill = "#FFF0D1") +
        ggplot2::geom_histogram(binwidth = 0.25, boundary = 0, fill = "#1F3A5F", colour = "white", linewidth = 0.6) +
        ggplot2::geom_vline(xintercept = 1.25, linetype = "dashed", linewidth = 0.8, colour = "#1D2733") +
        ggplot2::annotate("text", x = 1.27, y = Inf, label = "meta 1,25", hjust = 0, vjust = 1.5, size = 3.8) +
        ggplot2::scale_x_continuous(limits = c(0, 4), breaks = 0:4, labels = function(x) paste0(x, ",0")) +
        ggplot2::scale_y_continuous(breaks = function(l) seq(0, ceiling(l[2]), by = 1), expand = ggplot2::expansion(c(0, .1))) +
        ggplot2::labs(x = "Soma de p (0 a 4,0)", y = "Solicitações") + tema_grafico()
    }, alt = function() {
      b <- base_painel(); sprintf("Histograma das somas de %d solicitações; %d atingem a meta de 1,25.",
                                  nrow(b), sum(b$situacao == "apto"))
    })
  })
}

tema_grafico <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(colour = "#E4E9EF"),
                   axis.text = ggplot2::element_text(colour = "#3A4756"),
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA))
}

# CSV em padrao brasileiro (; e virgula decimal), UTF-8 com BOM para o Excel
exportar_csv <- function(df, file) {
  con <- file(file, open = "wb"); on.exit(close(con))
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  utils::write.table(df, con, sep = ";", dec = ",", row.names = FALSE, na = "", fileEncoding = "UTF-8", qmethod = "double")
}

# PDF sem LaTeX: grid + gridExtra, paginado
exportar_pdf <- function(df, file, filtros = "", versao = "") {
  if (isTRUE(capabilities("cairo"))) {
    grDevices::cairo_pdf(file, width = 11.69, height = 8.27, onefile = TRUE)
  } else {
    grDevices::pdf(file, width = 11.69, height = 8.27, onefile = TRUE, title = "Credenciamento Docente PPGE")
  }
  on.exit(grDevices::dev.off())
  tab <- data.frame(`Nº` = df$solicitacao, Docente = df$docente, `Área` = df$area_atuacao,
    Pedido = fmt_data(df$data_pedido), Status = df$status, Janela = ifelse(is.na(df$janela), "-", df$janela),
    `Soma (p)` = fmt_p(df$soma_p), `Situação` = df$situacao, check.names = FALSE)
  por_pag <- 22
  pags <- max(1, ceiling(nrow(tab) / por_pag))
  tema <- gridExtra::ttheme_minimal(base_size = 9,
    core = list(fg_params = list(hjust = 0, x = 0.03)), colhead = list(fg_params = list(hjust = 0, x = 0.03, fontface = "bold")))
  for (p in seq_len(pags)) {
    grid::grid.newpage()
    grid::grid.text("Credenciamento Docente PPGE - solicitações (DEMO, dados fictícios)", x = 0.04, y = 0.95, just = "left",
                    gp = grid::gpar(fontsize = 15, fontface = "bold", col = "#1F3A5F"))
    grid::grid.text(sprintf("Gerado em %s · %s · Base: %s", format(Sys.time(), "%d/%m/%Y %H:%M"), filtros, versao),
                    x = 0.04, y = 0.91, just = "left", gp = grid::gpar(fontsize = 8.5, col = "#5B6B7F"))
    if (p == 1) grid::grid.text(sprintf("%d solicitações · %d aptas · %d quase aptas · %d não aptas · %d sem avaliação",
                    nrow(df), sum(df$situacao == "apto"), sum(df$situacao == "quase apto"),
                    sum(df$situacao == "não apto"), sum(df$situacao == "sem avaliação")),
                    x = 0.04, y = 0.875, just = "left", gp = grid::gpar(fontsize = 10))
    linhas <- tab[seq((p - 1) * por_pag + 1, min(p * por_pag, nrow(tab))), , drop = FALSE]
    if (nrow(tab)) {
      g <- gridExtra::tableGrob(linhas, rows = NULL, theme = tema)
      grid::pushViewport(grid::viewport(x = 0.04, y = 0.84, width = 0.92, height = 0.78, just = c("left", "top")))
      g$vp <- grid::viewport(x = 0, y = 1, just = c("left", "top"), width = sum(g$widths), height = sum(g$heights))
      grid::grid.draw(g); grid::popViewport()
    } else grid::grid.text("Nenhuma solicitação com estes filtros.", y = 0.7)
    grid::grid.text(sprintf("Página %d de %d · soma de p em escala 0–1 por artigo (máx. 4,0); meta 1,25", p, pags),
                    x = 0.96, y = 0.03, just = "right", gp = grid::gpar(fontsize = 8, col = "#5B6B7F"))
  }
}
