# =====================================================================
# mod_avaliacao.R  -  Mostra o resultado de avaliar_lattes(): medidor,
# cartoes dos artigos, periodo (Artigo Y) e confirmacao de revista ambigua.
# =====================================================================

avaliacao_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("corpo"))
}

# retrato: reactive com a saida de avaliar_lattes() (ou NULL)
# editavel: reactive logico (mostra o seletor de revista ambigua)
# ctx: list(con_sjr, ao_confirmar = function(artigo_id, sourceid))
avaliacao_server <- function(id, ctx, retrato, editavel = shiny::reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$corpo <- shiny::renderUI({
      r <- retrato()
      if (is.null(r)) {
        return(bslib::card(bslib::card_body(
          shiny::p(shiny::icon("circle-info"), " O resultado aparece aqui."),
          shiny::p(class = "small-muted", "Envie o currículo, confira os artigos e, se for o caso, as licenças (Artigo Y); ",
                   "depois clique em ", shiny::strong("Calcular avaliação"), "."))))
      }
      det <- r$detalhe
      res <- r$resumo
      per <- r$periodo
      cab <- shiny::div(class = "small-muted mb-2",
        sprintf("Calculado em %s", fmt_data_hora(r$calculada_em)), shiny::br(),
        sprintf("Janela de análise: %d–%d (%d anos)", per$ano_ini, per$ano_fim, per$n_anos),
        " · Base: ", r$versao_banco)

      # alertas -----------------------------------------------------------
      alertas <- list()
      if (length(per$avisos)) {
        alertas <- c(alertas, lapply(per$avisos, function(a)
          shiny::div(class = "alert alert-warning py-2", role = "alert", shiny::icon("triangle-exclamation"), " ", a)))
      }
      if (nrow(det)) {
        amb <- det[det$n_candidatos > 1 & det$confirmado_manual == 0, , drop = FALSE]
        for (i in seq_len(nrow(amb))) alertas[[length(alertas) + 1]] <- alerta_ambiguo(amb[i, ], per$n_anos)
        nsem <- sum(det$metodo == "sem_match")
        if (nsem) alertas[[length(alertas) + 1]] <- shiny::div(class = "alert alert-secondary py-2",
          shiny::icon("link-slash"), sprintf(" %d artigo(s) com revista não encontrada no SJR: não pontuam. ", nsem),
          "Confira o ISSN ou o nome exato da revista no buscador.")
      }

      cartoes <- if (nrow(det)) lapply(seq_len(nrow(det)), function(i) cartao_artigo(det[i, ])) else
        list(shiny::p("Nenhum artigo cadastrado nesta solicitação."))

      tab_per <- per$detalhe
      if (is.data.frame(tab_per) && nrow(tab_per)) {
        tab_per <- data.frame(Motivo = tab_per$motivo,
                              Anos = formatC(tab_per$anos, format = "fg", decimal.mark = ","),
                              check.names = FALSE)
      }
      lic <- r$licencas

      shiny::tagList(
        bslib::layout_columns(col_widths = c(12, 12, 12), fill = FALSE,
          bslib::card(
            bslib::card_header("Resultado"),
            bslib::card_body(cab, medidor_soma(res$soma, res$meta),
              shiny::div(class = "small-muted",
                sprintf("%d artigo(s) avaliados · %d no período · %d selecionados (até 4) · %d sem revista no SJR",
                        res$n_artigos_lattes, res$n_no_periodo, res$n_selecionados, res$n_sem_revista)),
              alertas)),
          bslib::card(
            bslib::card_header("Artigos",
              shiny::span(class = "small-muted fw-normal ms-2",
                "em destaque: os que entram na soma; em cinza: os que não entram, com o motivo")),
            bslib::card_body(shiny::div(class = "grade-artigos", cartoes))),
          bslib::layout_columns(col_widths = c(6, 6),
            bslib::card(bslib::card_header("Período (Artigo Y)"),
              bslib::card_body(
                if (is.data.frame(tab_per) && nrow(tab_per)) tabela_html(tab_per, num = "Anos"),
                shiny::p(class = "mt-2 mb-0", sprintf("Período usado: %d anos (%d–%d).", per$n_anos, per$ano_ini, per$ano_fim)),
                if (isTRUE(per$limitado)) shiny::p(class = "text-warning-emphasis small",
                  sprintf("Pedido: %d anos. A base só cobre até 6 anos.", per$n_anos_pedido)))),
            bslib::card(bslib::card_header("Licenças informadas"),
              bslib::card_body(
                if (is.data.frame(lic) && nrow(lic)) tabela_html(data.frame(
                  `Situação` = lic$situacao, Informado = lic$informado, check.names = FALSE))
                else shiny::p(class = "small-muted", "Nenhuma licença informada."))))
        )
      )
    })

    alerta_ambiguo <- function(d, n_anos) {
      cands <- candidatos_revista(ctx$con_sjr, d$issn, d$revista, n_anos)
      sel_id <- ns(paste0("amb_", d$artigo_id))
      opcoes <- lapply(seq_len(nrow(cands)), function(k) shiny::tags$option(
        value = sprintf("%.0f", cands$sourceid[k]),
        selected = if (!is.na(d$sourceid) && cands$sourceid[k] == d$sourceid) NA,
        sprintf("%s — percentil %s · p %s (%s)", cands$titulo[k], fmt_pct(cands$percentil_max[k]),
                fmt_p(cands$p[k]), cands$melhor_area[k] %|na|% "sem classificação")))
      js <- sprintf("Shiny.setInputValue('%s', {artigo: %d, sourceid: document.getElementById('%s').value, t: Date.now()}, {priority: 'event'})",
                    ns("confirmar"), as.integer(d$artigo_id), sel_id)
      shiny::div(class = "alert alert-warning", role = "alert",
        shiny::strong(shiny::icon("code-fork"), sprintf(" O ISSN/título de \"%s\" corresponde a %d revistas.", d$titulo, d$n_candidatos)),
        shiny::p(class = "mb-2 small", "O cálculo usou a de menor código (sourceid). Escolha a revista certa e recalcule."),
        if (isTRUE(editavel())) shiny::div(class = "d-flex flex-wrap gap-2 align-items-end",
          shiny::div(class = "flex-grow-1",
            shiny::tags$label(`for` = sel_id, class = "form-label small mb-1", "Revista em que o artigo foi publicado"),
            shiny::tags$select(id = sel_id, class = "form-select form-select-sm", opcoes)),
          shiny::tags$button(type = "button", class = "btn btn-sm btn-primary", onclick = js,
                             shiny::icon("check"), " Confirmar revista"))
        else NULL
      )
    }

    shiny::observeEvent(input$confirmar, {
      if (!isTRUE(editavel())) return()
      x <- input$confirmar
      ctx$ao_confirmar(as.integer(x$artigo), as.numeric(x$sourceid))
    })
  })
}
