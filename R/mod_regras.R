# =====================================================================
# mod_regras.R  -  "Regras e premissas": parametros e decisoes pendentes
# =====================================================================

PENDENCIAS <- list(
  list(t = "1. Maior percentil = melhor revista",
       x = "Assumimos que, no SJR, percentil mais alto indica revista melhor (percentil = (N − posição + 1) / N × 100). Se for o contrário, a fórmula precisa ser invertida no tratamento dos dados."),
  list(t = "2. Meta de 1,25 vale para qualquer janela",
       x = "A meta não muda quando o período é ampliado pelo Artigo Y (5 ou 6 anos continuam exigindo 1,25)."),
  list(t = "3. Como o Artigo Y amplia o período",
       x = "Parentalidade: +2 anos por licença (50% de 4 anos). PcD: +1 ano (o edital diz 50%, mas “até 5 anos”). Licença médica: + a duração. As situações somam, sem teto, e o total é arredondado para cima em anos inteiros (o SJR é anual). Como a base só tem 2020–2025, o máximo possível é 6 anos; acima disso o sistema usa 6 e avisa."),
  list(t = "4. Só conta artigo publicado dentro da janela",
       x = "Artigos com ano fora do período aparecem como “fora do período” e não entram no top 4."),
  list(t = "5. Como o artigo é ligado à revista",
       x = "Primeiro pelo ISSN; se não achar, pelo nome da revista normalizado (sem acento, minúsculo) e idêntico. Se um ISSN ou nome corresponder a mais de uma revista, o cálculo usa a de menor código e avisa. Nesta demo, quando o docente confirma a revista certa, a escolha dele passa a valer no cálculo — confirmar se a comissão concorda."),
  list(t = "6. Qual é o “último ano” da janela",
       x = "O ano final está fixo em 2025 (último ano completo do SJR). Hoje é 2026, e “quatro anos antes da solicitação” pode ser lido como 2022–2025 ou 2023–2026. A demo usa 2022–2025 e grava o ano final em cada solicitação.")
)

regras_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
    shiny::div(
      bslib::card(bslib::card_header("A regra do edital (Artigo X)"),
        bslib::card_body(
          shiny::p("Só contam artigos em revistas indexadas no SJR. Nos 4 anos anteriores à solicitação, ",
                   "o docente precisa somar pelo menos ", shiny::strong("1,25"), " pontos com até ",
                   shiny::strong("4 artigos"), ":"),
          shiny::p(class = "text-center fs-5", shiny::HTML("&sum;<sub>i=1..4</sub> p<sub>i</sub> &middot; I(p<sub>i</sub> &ge; 0,25) &ge; 1,25")),
          shiny::p(shiny::strong("p"), " é o maior percentil da revista entre todas as áreas e todos os anos da janela, ",
                   "em escala 0 a 1. Revista com p abaixo de 0,25 não pontua. O máximo possível é 4,0."),
          shiny::div(class = "alert alert-info small mb-0",
            shiny::strong("Duas escalas: "), "o ", shiny::strong("percentil"), " aparece de 0 a 100 (ex.: 62,0) ",
            "e o ", shiny::strong("p"), " de 0 a 1 (ex.: 0,62). Corte (0,25) e meta (1,25) estão na escala de p."))),
      bslib::card(bslib::card_header("Parâmetros em uso (banco_edital.db)"),
        bslib::card_body(shiny::uiOutput(ns("params")))),
      bslib::card(bslib::card_header("Classificação nas telas da comissão"),
        bslib::card_body(shiny::tags$ul(class = "mb-0",
          shiny::tags$li(badge_situacao("apto"), " soma ≥ 1,25"),
          shiny::tags$li(badge_situacao("quase apto"), " soma entre 1,00 e 1,25 (só informativo)"),
          shiny::tags$li(badge_situacao("não apto"), " soma < 1,00")))),
      bslib::card(bslib::card_header("Sobre esta demonstração"),
        bslib::card_body(class = "small",
          shiny::p("Todos os docentes, artigos e licenças são fictícios. As revistas e percentis vêm do SJR (Scimago)."),
          shiny::p(class = "mb-0", "Cada avaliação é gravada como um retrato: se o SJR for atualizado, ",
                   "os resultados antigos não mudam sozinhos.")))),
    bslib::card(bslib::card_header(shiny::icon("triangle-exclamation"), " Decisões pendentes (confirmar com o professor)"),
      bslib::card_body(
        shiny::p(class = "small-muted", "O sistema funciona com as premissas abaixo, mas elas ainda não foram confirmadas."),
        lapply(PENDENCIAS, function(p) shiny::div(class = "pendencia", shiny::h3(p$t), shiny::p(class = "mb-0", p$x)))))
  )
}

regras_server <- function(id, ctx) {
  shiny::moduleServer(id, function(input, output, session) {
    output$params <- shiny::renderUI({
      p <- DBI::dbGetQuery(ctx$con_sjr, "SELECT chave, valor, descricao FROM parametro ORDER BY chave")
      j <- listar_janelas(ctx$con_sjr)
      shiny::tagList(
        tabela_html(data.frame(`Parâmetro` = p$chave,
          Valor = formatC(p$valor, format = "fg", decimal.mark = ","), `Descrição` = p$descricao,
          check.names = FALSE), num = "Valor"),
        shiny::p(class = "small-muted mt-2 mb-0", "Janelas disponíveis: ",
          paste(sprintf("%d anos (%d–%d)", j$n_anos, j$ano_ini, j$ano_fim), collapse = "; "), ". ",
          "Base: ", versao_banco_edital(ctx$con_sjr)))
    })
  })
}
