# =====================================================================
# ui_componentes.R  -  Tema, formatacao e componentes visuais do app
# =====================================================================

tema_ppge <- function() {
  bslib::bs_theme(
    version = 5,
    bg = "#FFFFFF", fg = "#1D2733",
    primary = "#1F3A5F", secondary = "#5B6B7F",
    success = "#1E6B3A", warning = "#8A5300", danger = "#A4262C", info = "#1F5F8B",
    base_font = bslib::font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto",
                                       "Helvetica Neue", "Arial", "sans-serif"),
    heading_font = bslib::font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto",
                                          "Helvetica Neue", "Arial", "sans-serif"),
    "border-radius" = "0.4rem",
    "navbar-bg" = "#1F3A5F"
  ) |>
    bslib::bs_add_rules(CSS_PPGE)
}

CSS_PPGE <- "
:root { --ppge-azul: #1F3A5F; --ppge-cinza: #F3F5F8; --ppge-borda: #D5DCE4; }
body { background: var(--ppge-cinza); }
.navbar { box-shadow: 0 1px 0 rgba(0,0,0,.08); }
.navbar-brand { font-weight: 600; letter-spacing: .01em; }
.card { border-color: var(--ppge-borda); }
.bslib-card:not(.bslib-full-screen) { max-height: none !important; height: auto !important; overflow: visible; }
.bslib-card .tab-content, .bslib-card .tab-content > .tab-pane, .bslib-card .card-body { overflow: visible; max-height: none; }
.dataTables_wrapper .dataTables_filter input { margin-left:.4rem; display:inline-block; width:auto; }
.dataTables_wrapper { font-size:.92rem; }
.dataTables_wrapper::after { content:''; display:block; clear:both; }
.dataTables_wrapper .dataTables_paginate { float:none; text-align:left; margin-top:.4rem; }
table.dataTable tbody tr.selected > * { box-shadow: inset 0 0 0 9999px rgba(31,58,95,.14) !important; color: inherit !important; }
.card-header { background: #fff; font-weight: 600; }
.faixa-demo { background: #FFF4D6; color: #5C3D00; border-bottom: 1px solid #E8D19A;
  font-size: .85rem; padding: .3rem 1rem; text-align: center; }
.sol-ativa { background:#fff; border:1px solid var(--ppge-borda); border-radius:.4rem;
  padding:.5rem .9rem; margin-bottom:1rem; display:flex; flex-wrap:wrap; gap:.5rem 1rem; align-items:center; }
.badge-status { font-weight: 600; }
.st-rascunho { background:#E7EBF0; color:#2B3A4A; }
.st-enviada  { background:#DCE9F7; color:#123E66; }
.st-avaliada { background:#DFF0E4; color:#14522B; }
.sit-apto { background:#DFF0E4; color:#14522B; }
.sit-quase { background:#FFF0D1; color:#5C3D00; }
.sit-nao { background:#F8E1E2; color:#7A1B20; }
.sit-sem { background:#E7EBF0; color:#2B3A4A; }

/* medidor 0-4 */
.medidor { margin: .25rem 0 1.25rem; }
.medidor-topo { display:flex; justify-content:space-between; align-items:baseline; flex-wrap:wrap; gap:.25rem 1rem; }
.medidor-valor { font-size: 2.2rem; font-weight: 700; line-height: 1; }
.medidor-trilho { position:relative; height: 22px; background:#E4E9EF; border-radius: 11px; margin-top:.6rem; }
.medidor-barra { position:absolute; left:0; top:0; bottom:0; border-radius: 11px; }
.medidor-meta { position:absolute; top:-8px; bottom:-8px; width:3px; background:#1D2733; border-radius:2px; }
.medidor-meta-rot { position:absolute; top:-1.6rem; transform:translateX(-50%); font-size:.8rem; font-weight:600; white-space:nowrap; }
.medidor-escala { position:relative; height:1.2rem; font-size:.75rem; color:#5B6B7F; margin-top:.2rem; }
.medidor-escala span { position:absolute; transform:translateX(-50%); }
.cor-apto { background:#2E7D4F; } .cor-quase { background:#C98A1B; } .cor-nao { background:#B3383E; }

/* cartoes de artigo */
.grade-artigos { display:grid; grid-template-columns: repeat(auto-fill, minmax(280px, 1fr)); gap: .9rem; }
.cartao-art { background:#fff; border:1px solid var(--ppge-borda); border-radius:.5rem; padding:.85rem 1rem;
  display:flex; flex-direction:column; gap:.35rem; }
.cartao-art.conta { border:2px solid var(--ppge-azul); box-shadow: 0 2px 6px rgba(31,58,95,.12); }
.cartao-art.cinza { background:#F6F7F9; color:#4A5664; }
.cartao-art h3 { font-size:1rem; margin:0; font-weight:600; }
.cartao-art .meta { font-size:.82rem; color:#5B6B7F; }
.cartao-art .escalas { display:flex; gap:.4rem; flex-wrap:wrap; }
.escala { font-size:.78rem; border:1px solid var(--ppge-borda); border-radius: 999px; padding:.05rem .55rem; background:#fff; }
.escala b { font-weight:700; }
.cartao-art .motivo { font-size:.85rem; font-weight:600; }
.cartao-art.conta .motivo { color:#14522B; }
.pos-badge { font-size:.75rem; font-weight:700; background:var(--ppge-azul); color:#fff; border-radius:999px; padding:.05rem .55rem; }
.tabela-simples { width:100%; font-size:.9rem; }
.tabela-simples td, .tabela-simples th { padding:.3rem .5rem; border-bottom:1px solid #E4E9EF; }
.tabela-simples th { font-weight:600; color:#2B3A4A; }
.tabela-simples td.num { text-align:right; font-variant-numeric: tabular-nums; }
.pendencia { border-left:4px solid #C98A1B; background:#fff; padding:.6rem .9rem; margin-bottom:.6rem; border-radius:.3rem; }
.pendencia h3 { font-size:1rem; margin:0 0 .25rem; }
.small-muted { font-size:.82rem; color:#5B6B7F; overflow-wrap:anywhere; }
:focus-visible { outline: 3px solid #F2B705 !important; outline-offset: 2px; }
@media (max-width: 576px) { .medidor-valor { font-size: 1.8rem; } }
"

# ---- formatacao pt-BR -------------------------------------------------
fmt_p   <- function(x) ifelse(is.na(x), "–", formatC(x, format = "f", digits = 2, decimal.mark = ","))
fmt_pct <- function(x) ifelse(is.na(x), "–", formatC(x, format = "f", digits = 1, decimal.mark = ","))
fmt_data_hora <- function(x) {
  d <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S")
  ifelse(is.na(d), as.character(x), format(d, "%d/%m/%Y %H:%M"))
}
fmt_issn <- function(x) ifelse(is.na(x) | nchar(x) != 8, x, paste0(substr(x, 1, 4), "-", substr(x, 5, 8)))
fmt_data <- function(x) {
  d <- as.Date(x)
  ifelse(is.na(d), "–", format(d, "%d/%m/%Y"))
}
`%|na|%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

ROTULO_STATUS <- c(rascunho = "Rascunho", enviada = "Enviada", avaliada = "Avaliada")
ROTULO_LICENCA <- c(parentalidade = "Parentalidade", medica = "Licença médica", pcd = "PcD")
ROTULO_METODO <- c(issn = "casada por ISSN", titulo = "casada pelo título", manual = "confirmada pelo docente",
                   sem_match = "não encontrada no SJR")

badge_status <- function(st) {
  htmltools::tags$span(class = paste("badge badge-status", paste0("st-", st)), ROTULO_STATUS[[st]] %|na|% st)
}
classe_situacao <- function(sit) {
  switch(sit, "apto" = "sit-apto", "quase apto" = "sit-quase", "não apto" = "sit-nao", "sit-sem")
}
icone_situacao <- function(sit) {
  switch(sit, "apto" = "circle-check", "quase apto" = "circle-exclamation", "não apto" = "circle-xmark", "circle")
}
badge_situacao <- function(sit) {
  htmltools::tags$span(class = paste("badge", classe_situacao(sit)),
                       shiny::icon(icone_situacao(sit)), " ", tools::toTitleCase(sit))
}
# versao HTML (texto) para usar dentro de DT
html_status   <- function(st)  vapply(st, function(s) as.character(badge_status(s)), "")
html_situacao <- function(sit) vapply(sit, function(s) as.character(badge_situacao(s)), "")

# ---- medidor 0 a 4,0 com a meta marcada -------------------------------
medidor_soma <- function(soma, meta = 1.25, maximo = 4) {
  sit <- situacao_soma(soma, meta)
  cor <- switch(sit, "apto" = "cor-apto", "quase apto" = "cor-quase", "cor-nao")
  pct <- function(v) sprintf("%.2f%%", 100 * min(max(v, 0), maximo) / maximo)
  diff <- round(soma - meta, 6)
  frase <- if (diff >= 0) {
    sprintf("Passa: a soma %s atinge a meta de %s (sobra %s).", fmt_p(soma), fmt_p(meta), fmt_p(diff))
  } else {
    sprintf("Falta %s para a meta de %s.", fmt_p(-diff), fmt_p(meta))
  }
  htmltools::tags$div(class = "medidor",
    htmltools::tags$div(class = "medidor-topo",
      htmltools::tags$div(
        htmltools::tags$div(class = "small-muted", "Soma de p (escala 0–1 por artigo; máximo 4,0)"),
        htmltools::tags$span(class = "medidor-valor", fmt_p(soma)),
        htmltools::tags$span(class = "small-muted", " de 4,00")),
      badge_situacao(sit)),
    htmltools::tags$div(class = "medidor-trilho", role = "meter",
      `aria-valuemin` = 0, `aria-valuemax` = maximo, `aria-valuenow` = round(soma, 2),
      `aria-valuetext` = sprintf("Soma %s de 4,00. Meta %s. %s", fmt_p(soma), fmt_p(meta), frase),
      `aria-label` = "Soma da pontuação",
      htmltools::tags$div(class = paste("medidor-barra", cor), style = sprintf("width:%s", pct(soma))),
      htmltools::tags$div(class = "medidor-meta", style = sprintf("left:%s", pct(meta)),
        htmltools::tags$span(class = "medidor-meta-rot", sprintf("meta %s", fmt_p(meta))))),
    htmltools::tags$div(class = "medidor-escala", `aria-hidden` = "true",
      lapply(0:4, function(v) htmltools::tags$span(style = sprintf("left:%s", pct(v)), paste0(v, ",0")))),
    htmltools::tags$p(class = "fw-semibold mb-0", frase)
  )
}

# ---- cartao de artigo ---------------------------------------------------
cartao_artigo <- function(d) {
  conta <- isTRUE(d$conta)
  sel   <- d$situacao == "selecionado"
  revista_txt <- if (!is.na(d$revista_sjr %|na|% NA)) d$revista_sjr else
    (if (nzchar(d$revista %|na|% "")) d$revista else "(revista não informada)")
  htmltools::tags$article(class = paste("cartao-art", if (conta) "conta" else "cinza"),
    `aria-label` = paste("Artigo:", d$titulo),
    htmltools::tags$div(class = "d-flex justify-content-between gap-2 align-items-start",
      htmltools::tags$h3(d$titulo),
      if (sel) htmltools::tags$span(class = "pos-badge", title = "Posição entre os 4 selecionados",
                                    paste0("#", d$pos))),
    htmltools::tags$div(class = "meta",
      sprintf("%d · %s", d$ano, revista_txt),
      if (!is.na(d$issn %|na|% NA) && nzchar(d$issn)) sprintf(" · ISSN %s", fmt_issn(d$issn))),
    htmltools::tags$div(class = "meta",
      shiny::icon(if (d$metodo == "sem_match") "link-slash" else "link"), " Revista ",
      ROTULO_METODO[[d$metodo]] %|na|% d$metodo,
      if (isTRUE(d$n_candidatos > 1)) htmltools::tags$span(class = "badge text-bg-warning ms-1",
        sprintf("%d candidatas", d$n_candidatos))),
    htmltools::tags$div(class = "escalas",
      htmltools::tags$span(class = "escala", "Percentil (0–100): ", htmltools::tags$b(fmt_pct(d$percentil_max))),
      htmltools::tags$span(class = "escala", "p (0–1): ", htmltools::tags$b(fmt_p(d$p))),
      if (sel) htmltools::tags$span(class = "escala", "Pontos: ", htmltools::tags$b(fmt_p(d$pontos)))),
    htmltools::tags$div(class = "small", d$explicacao),
    htmltools::tags$div(class = "motivo",
      shiny::icon(if (conta) "check" else "circle-info"), " ", d$motivo)
  )
}

# DT com textos em portugues (sem depender de CDN)
DT_PT <- list(
  sEmptyTable = "Nenhum registro", sInfo = "_START_ a _END_ de _TOTAL_",
  sInfoEmpty = "0 registros", sInfoFiltered = "(filtrado de _MAX_)", sLengthMenu = "Mostrar _MENU_",
  sLoadingRecords = "Carregando...", sProcessing = "Processando...", sZeroRecords = "Nada encontrado",
  sSearch = "Buscar:", oPaginate = list(sNext = "Próximo", sPrevious = "Anterior",
                                        sFirst = "Primeiro", sLast = "Último"))

tabela_dt <- function(df, ..., selecao = "single", pagina = 10, escape = TRUE, ordem = list()) {
  DT::datatable(df, rownames = FALSE, selection = selecao, escape = escape, style = "auto",
                class = "table table-sm table-hover align-middle", fillContainer = FALSE,
                options = list(language = DT_PT, pageLength = pagina, autoWidth = FALSE,
                               scrollX = TRUE, order = ordem, dom = "ftip"), ...)
}

tabela_html <- function(df, num = character(0)) {
  htmltools::tags$div(class = "table-responsive", htmltools::tags$table(class = "tabela-simples",
    htmltools::tags$thead(htmltools::tags$tr(lapply(names(df), htmltools::tags$th, scope = "col"))),
    htmltools::tags$tbody(lapply(seq_len(nrow(df)), function(i)
      htmltools::tags$tr(lapply(names(df), function(n)
        htmltools::tags$td(class = if (n %in% num) "num", as.character(df[[n]][i]))))))))
}
