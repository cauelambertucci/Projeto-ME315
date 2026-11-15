# =====================================================================
# mod_revistas.R  -  Buscador de revistas e "onde publicar"
# =====================================================================

revistas_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_card_tab(id = ns("aba"),
    bslib::nav_panel("Buscar revista", icon = shiny::icon("magnifying-glass"),
      bslib::layout_columns(col_widths = bslib::breakpoints(sm = 12, lg = c(8, 4)),
        shiny::textInput(ns("q"), "Nome da revista ou ISSN", width = "100%",
                         placeholder = "ex.: statistics  ou  0266-4763"),
        shiny::selectInput(ns("n_anos"), "Janela", choices = NULL)),
      shiny::p(class = "small-muted",
        "Percentil em escala 0–100 (exibição). p em escala 0–1 (é o que entra na soma; corte 0,25). ",
        "Clique em uma revista para ver a nota em cada área."),
      DT::DTOutput(ns("res"), fill = FALSE),
      shiny::uiOutput(ns("areas"))),
    bslib::nav_panel("Onde publicar", icon = shiny::icon("compass"),
      bslib::layout_columns(col_widths = bslib::breakpoints(sm = 12, lg = c(8, 4)),
        shiny::selectizeInput(ns("area"), "Área do SJR", choices = NULL, width = "100%"),
        shiny::selectInput(ns("n_anos2"), "Janela", choices = NULL)),
      shiny::p(class = "small-muted",
        "Revistas da área com p ≥ 0,25 (contam pontos), da maior para a menor. ",
        "“p na área” é a nota nesta área; “p da revista” é o maior p entre todas as áreas (o que vale no edital)."),
      DT::DTOutput(ns("pub"), fill = FALSE))
  )
}

revistas_server <- function(id, ctx) {
  shiny::moduleServer(id, function(input, output, session) {
    sjr <- ctx$con_sjr
    jan <- listar_janelas(sjr)
    escolhas_jan <- stats::setNames(jan$n_anos, sprintf("%d–%d (%d anos)%s", jan$ano_ini, jan$ano_fim, jan$n_anos,
                                                        ifelse(jan$padrao == 1, " – padrão", "")))
    padrao <- jan$n_anos[jan$padrao == 1][1]
    shiny::updateSelectInput(session, "n_anos", choices = escolhas_jan, selected = padrao)
    shiny::updateSelectInput(session, "n_anos2", choices = escolhas_jan, selected = padrao)
    areas <- listar_areas(sjr)
    shiny::updateSelectizeInput(session, "area", choices = areas$area, server = TRUE,
                                selected = areas$area[which.max(areas$n_revistas)])
    corte <- ler_parametros(sjr)$corte_p

    q <- shiny::debounce(shiny::reactive(input$q), 400)
    res <- shiny::reactive({
      txt <- q(); n <- as.integer(input$n_anos)
      shiny::req(n, nchar(trimws(txt %||% "")) >= 3)
      issn <- normalizar_issn(txt)
      if (nzchar(issn) && grepl("^[0-9 xX-]+$", txt)) {
        DBI::dbGetQuery(sjr, "SELECT v.sourceid, v.titulo, v.p, v.percentil_max, v.quartil, v.melhor_area,
                                     v.classificada, v.conta_pontos
                              FROM v_revista_nota v JOIN issn i ON i.sourceid = v.sourceid
                              WHERE i.issn = ? AND v.n_anos = ?", params = list(issn, n))
      } else buscar_revista(sjr, txt, n_anos = n, limite = 50)
    })
    output$res <- DT::renderDT({
      r <- res()
      df <- data.frame(Revista = r$titulo, `Percentil (0–100)` = round(r$percentil_max, 1),
        `p (0–1)` = round(r$p, 3), `Melhor área` = ifelse(is.na(r$melhor_area), "sem classificação", r$melhor_area),
        `Conta pontos?` = ifelse(r$conta_pontos == 1, "sim (p ≥ 0,25)", "não"), check.names = FALSE)
      tabela_dt(df, ordem = list(list(2, "desc"))) |>
        DT::formatRound(2, 1, dec.mark = ",", mark = ".") |> DT::formatRound(3, 3, dec.mark = ",", mark = ".")
    })
    output$areas <- shiny::renderUI({
      i <- input$res_rows_selected; shiny::req(length(i))
      r <- res()[i, , drop = FALSE]
      shiny::req(nrow(r) == 1, !is.na(r$sourceid))
      a <- notas_por_area(sjr, r$sourceid, as.integer(input$n_anos))
      issns <- DBI::dbGetQuery(sjr, "SELECT issn FROM issn WHERE sourceid = ?", params = list(r$sourceid))$issn
      bslib::card(class = "mt-3",
        bslib::card_header(r$titulo),
        bslib::card_body(fillable = FALSE, 
          shiny::p(class = "small-muted", sprintf("sourceid %.0f · ISSN %s", r$sourceid,
                   if (length(issns)) paste(sub("(.{4})(.{4})", "\\1-\\2", issns), collapse = ", ") else "–")),
          if (nrow(a)) tabela_html(data.frame(`Área` = a$area, `Percentil (0–100)` = fmt_pct(a$percentil_max),
            `p (0–1)` = fmt_p(a$p), `Ano do máximo` = a$ano_percentil_max,
            `Melhor?` = ifelse(a$e_melhor_area == 1, "★ melhor área", ""), check.names = FALSE),
            num = c("Percentil (0–100)", "p (0–1)"))
          else shiny::p("Sem percentil nesta janela: a revista não pontua.")))
    })

    output$pub <- DT::renderDT({
      shiny::req(input$area, input$n_anos2)
      r <- DBI::dbGetQuery(sjr,
        "SELECT r.titulo, a.percentil_max, a.p, a.ano_percentil_max, n.p AS p_rev, n.melhor_area,
                (SELECT group_concat(substr(i.issn,1,4) || '-' || substr(i.issn,5,4), ', ')
                   FROM issn i WHERE i.sourceid = a.sourceid) AS issn
         FROM nota_revista_area a
         JOIN revista r      ON r.sourceid = a.sourceid
         JOIN nota_revista n ON n.sourceid = a.sourceid AND n.n_anos = a.n_anos
         WHERE a.area = ? AND a.n_anos = ? AND a.p >= ?
         ORDER BY a.percentil_max DESC, r.titulo LIMIT 500",
        params = list(input$area, as.integer(input$n_anos2), corte))
      df <- data.frame(Revista = r$titulo, ISSN = ifelse(is.na(r$issn), "–", r$issn),
        `Percentil na área (0–100)` = round(r$percentil_max, 1), `p na área (0–1)` = round(r$p, 3),
        `Ano` = r$ano_percentil_max, `p da revista (0–1)` = round(r$p_rev, 3),
        `Melhor área` = r$melhor_area, check.names = FALSE)
      tabela_dt(df, selecao = "none", pagina = 15) |>
        DT::formatRound(3, 1, dec.mark = ",", mark = ".") |> DT::formatRound(c(4, 6), 3, dec.mark = ",", mark = ".")
    })
  })
}
