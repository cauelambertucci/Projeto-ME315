# =====================================================================
# semear_demo.R  -  Cria o app.db de DEMONSTRACAO com ~15 docentes FICTICIOS
#
#   Rscript R/semear_demo.R            # cria app.db (para se ja tiver dados)
#   Rscript R/semear_demo.R --forcar   # apaga e recria app.db
#   ou, no R:  source("R/semear_demo.R")
#
# - Nomes, areas e titulos de artigos sao INVENTADOS (nenhum dado pessoal real).
# - As REVISTAS sao reais: sorteadas do banco_edital.db por faixa de percentil,
#   para cobrir os casos: apto, quase apto, nao apto, ISSN ambiguo, artigo fora
#   do periodo, revista sem classificacao, duplicata e Artigo Y (parentalidade,
#   PcD, licenca medica, periodo limitado a 6 anos).
# - Senhas: geradas aleatoriamente e gravadas FORA do repositorio, em
#   Sys.getenv("PPGE_CREDENCIAIS", "~/.ppge/credenciais_demo.csv").
#   Se esse arquivo ja existir, as senhas dele sao reaproveitadas.
#   O app.db guarda so o hash (scrypt).
# =====================================================================

suppressPackageStartupMessages({ library(DBI); library(RSQLite) })
if (!exists("conectar_banco"))    source("R/funcoes_banco.R", encoding = "UTF-8")
if (!exists("calcular_periodo"))  source("R/periodo_artigo_y.R", encoding = "UTF-8")
if (!exists("abrir_app_db"))      source("R/banco_app.R", encoding = "UTF-8")

caminho_credenciais <- function() {
  path.expand(Sys.getenv("PPGE_CREDENCIAIS", "~/.ppge/credenciais_demo.csv"))
}

gerar_senha <- function(n = 12) {
  chars <- c(letters[-c(12, 15)], LETTERS[-c(9, 15)], 2:9)   # sem l, o, I, O, 0, 1
  paste(sample(chars, n, replace = TRUE), collapse = "")
}

DOCENTES_FICTICIOS <- data.frame(
  login = sprintf("docente%02d", 1:15),
  nome = c("Ana Beatriz Toledo", "Bruno Carvalhal", "Camila Restrepo Vaz", "Diego Monteval",
           "Elisa Fagundes Prado", "Fábio Nogueira Lins", "Gabriela Sardinha", "Henrique Bastião",
           "Isadora Quintela", "João Pedro Marinho", "Larissa Amarantes", "Marcos Vilaça Teles",
           "Natália Corsino", "Otávio Rebouças", "Paula Siqueira Dorneles"),
  area = c("Probabilidade", "Inferência Estatística", "Bioestatística", "Séries Temporais",
           "Aprendizado Estatístico", "Bioestatística", "Probabilidade", "Estatística Aplicada",
           "Inferência Estatística", "Séries Temporais", "Aprendizado Estatístico",
           "Estatística Aplicada", "Bioestatística", "Probabilidade", "Inferência Estatística"),
  stringsAsFactors = FALSE
)

# Revistas reais do banco_edital.db, separadas por faixa de p (janela de 4 anos).
# Usa so revistas com titulo e ISSN sem ambiguidade (exceto a faixa "ambigua").
sortear_revistas <- function(sjr) {
  base <- dbGetQuery(sjr, "
    WITH tit_unico AS (SELECT titulo_norm FROM revista GROUP BY titulo_norm HAVING COUNT(*) = 1),
         issn_unico AS (SELECT issn, MIN(sourceid) AS sourceid FROM issn
                        GROUP BY issn HAVING COUNT(DISTINCT sourceid) = 1)
    SELECT v4.sourceid, v4.titulo, v4.p AS p4, v6.p AS p6, v4.classificada AS c4,
           v6.classificada AS c6, MIN(iu.issn) AS issn
    FROM v_revista_nota v4
    JOIN v_revista_nota v6 ON v6.sourceid = v4.sourceid AND v6.n_anos = 6
    JOIN tit_unico t ON t.titulo_norm = v4.titulo_norm
    LEFT JOIN issn_unico iu ON iu.sourceid = v4.sourceid
    WHERE v4.n_anos = 4
    GROUP BY v4.sourceid")
  base <- base[!is.na(base$issn), ]
  faixa <- function(cond, n) {
    x <- base[cond, , drop = FALSE]
    if (nrow(x) < n) stop("banco_edital.db tem poucas revistas para a faixa pedida.")
    x[sample(nrow(x), n), ]
  }
  amb <- dbGetQuery(sjr, "SELECT issn FROM issn GROUP BY issn HAVING COUNT(DISTINCT sourceid) > 1")
  list(
    alta   = faixa(base$c4 == 1 & base$p4 >= 0.85, 14),
    media  = faixa(base$c4 == 1 & base$p4 >= 0.45 & base$p4 <= 0.70, 12),
    baixa  = faixa(base$c4 == 1 & base$p4 >= 0.25 & base$p4 <= 0.40, 8),
    abaixo = faixa(base$c4 == 1 & base$p4 > 0 & base$p4 < 0.25, 6),
    sem    = faixa(base$c6 == 0, 2),
    antiga = if (any(base$c4 == 0 & base$c6 == 1)) faixa(base$c4 == 0 & base$c6 == 1, 1) else NULL,
    issn_ambiguo = if (nrow(amb)) amb$issn[sample(nrow(amb), 1)] else NA_character_
  )
}

# Titulos de artigo inventados
titulo_ficticio <- local({
  temas <- c("Estimação robusta", "Modelos de mistura", "Inferência bayesiana", "Testes de permutação",
             "Regressão quantílica", "Processos pontuais", "Modelos hierárquicos", "Bootstrap paramétrico",
             "Séries com mudança de regime", "Florestas aleatórias", "Dados funcionais", "Cópulas",
             "Amostragem complexa", "Análise de sobrevivência", "Modelos espaço-temporais",
             "Seleção de variáveis", "Redes bayesianas", "Dados composicionais")
  objetos <- c("para dados censurados", "em ensaios clínicos simulados", "com covariáveis latentes",
               "aplicada a dados de saúde fictícios", "em alta dimensão", "com observações faltantes",
               "para contagens superdispersas", "em painéis curtos", "sob dependência espacial",
               "com erros de medição")
  function() paste(sample(temas, 1), sample(objetos, 1))
})

semear_demo <- function(app, sjr, credenciais = caminho_credenciais(), semente = 315) {
  set.seed(semente)
  if (nrow(dbGetQuery(app, "SELECT id FROM docente LIMIT 1"))) {
    stop("app.db já tem dados. Use --forcar (ou apague o arquivo) para recriar.")
  }
  rv  <- sortear_revistas(sjr)
  prm <- ler_parametros(sjr)
  ano_fim <- as.integer(prm$ano_fim)

  # senhas: reaproveita o arquivo, se existir
  cred <- if (file.exists(credenciais)) utils::read.csv(credenciais, stringsAsFactors = FALSE) else
    data.frame(login = character(0), senha = character(0), papel = character(0))
  senha_de <- function(login) {
    s <- cred$senha[cred$login == login]
    if (length(s)) s[1] else gerar_senha()
  }

  r <- function(faixa, i) rv[[faixa]][i, ]
  art <- function(sol, rev, ano, por = c("issn", "titulo", "ambos"), titulo = titulo_ficticio()) {
    por <- match.arg(por)
    adicionar_artigo(app, sol, titulo, ano,
                     revista = if (por != "issn") rev$titulo else "",
                     issn    = if (por != "titulo") rev$issn else "")
  }
  lic <- function(sol, tipo, ini, fim = NA, obs = NA) adicionar_licenca(app, sol, tipo, ini, fim, obs)

  ids <- list(); logins <- list()
  dbWithTransaction(app, {
    s_com <- senha_de("comissao")
    criar_usuario(app, "comissao", s_com, "comissao")
    logins$comissao <- c(s_com, "comissao")
    for (i in seq_len(nrow(DOCENTES_FICTICIOS))) {
      d <- DOCENTES_FICTICIOS[i, ]
      ids[[d$login]] <- criar_docente(app, d$nome, d$area)
      s <- senha_de(d$login)
      criar_usuario(app, d$login, s, "docente", ids[[d$login]])
      logins[[d$login]] <- c(s, "docente")
    }
  })

  sol <- function(login, data) criar_solicitacao(app, ids[[login]], ano_fim, data_pedido = data)
  aval <- function(s, oficial = FALSE) avaliar_solicitacao(app, sjr, s, oficial = oficial)
  y <- function(k) ano_fim - k     # 0 = 2025, 1 = 2024, ...

  # 01 Apto com folga: 4 revistas de topo + 1 extra (fora do top 4)
  s <- sol("docente01", "2026-03-02")
  for (k in 1:4) art(s, r("alta", k), y(k %% 4))
  art(s, r("media", 1), y(2)); aval(s); mudar_status(app, s, "enviada"); aval(s, TRUE)
  mudar_status(app, s, "avaliada")

  # 02 Apto justo: alta + media + baixa (por titulo da revista, sem ISSN)
  s <- sol("docente02", "2026-04-15")
  art(s, r("alta", 5), y(1), "titulo"); art(s, r("media", 2), y(2), "titulo"); art(s, r("baixa", 1), y(3))
  aval(s); mudar_status(app, s, "enviada")

  # 03 Quase apto (soma entre 1,00 e 1,25): combina media + baixas ate cair na faixa
  s <- sol("docente03", "2026-05-20")
  cand <- rbind(rv$media[3:6, ], rv$baixa[2:5, ])
  melhor <- NULL
  for (a in 1:(nrow(cand) - 1)) for (b in (a + 1):nrow(cand)) for (c in c(0, setdiff(seq_len(nrow(cand)), c(a, b)))) {
    pp <- cand$p4[c(a, b, if (c) c)]
    if (sum(pp) >= 1.02 && sum(pp) < 1.22) { melhor <- c(a, b, if (c) c); break }
  }
  if (is.null(melhor)) melhor <- 1:2
  for (k in seq_along(melhor)) art(s, cand[melhor[k], ], y(k - 1))
  aval(s); mudar_status(app, s, "enviada")

  # 04 Nao apto: uma baixa + duas abaixo do corte (selecionadas, mas nao pontuam)
  s <- sol("docente04", "2026-02-10")
  art(s, r("baixa", 6), y(0)); art(s, r("abaixo", 1), y(1)); art(s, r("abaixo", 2), y(2))
  aval(s); mudar_status(app, s, "enviada"); aval(s, TRUE); mudar_status(app, s, "avaliada")

  # 05 ISSN ambiguo (o docente ainda nao confirmou a revista) + uma alta
  s <- sol("docente05", "2026-06-01")
  if (!is.na(rv$issn_ambiguo)) {
    adicionar_artigo(app, s, titulo_ficticio(), y(1), revista = "", issn = rv$issn_ambiguo)
  }
  art(s, r("alta", 6), y(2)); art(s, r("media", 7), y(0))
  aval(s)                                         # fica em rascunho para o docente confirmar

  # 06 Artigos fora do periodo (anteriores a 2022) sem licenca: nao apto
  s <- sol("docente06", "2026-03-30")
  art(s, r("alta", 7), y(5)); art(s, r("alta", 8), y(4)); art(s, r("media", 8), y(9)); art(s, r("baixa", 7), y(1))
  aval(s); mudar_status(app, s, "enviada")

  # 07 Revista sem classificacao + revista que nao existe no SJR
  s <- sol("docente07", "2026-07-07")
  art(s, r("sem", 1), y(1)); art(s, r("media", 9), y(2))
  adicionar_artigo(app, s, titulo_ficticio(), y(0), revista = "Revista Inexistente de Estudos Fictícios", issn = "")
  aval(s)

  # 08 Duplicata: o mesmo artigo cadastrado duas vezes (com outra grafia) conta uma vez
  s <- sol("docente08", "2026-08-12")
  t <- titulo_ficticio()
  art(s, r("alta", 9), y(1), titulo = t)
  dup <- art(s, r("alta", 9), y(1), titulo = paste0(toupper(t), " ."))   # rejeitado: duplicado
  art(s, r("media", 10), y(2)); art(s, r("baixa", 8), y(3))
  aval(s); mudar_status(app, s, "enviada")

  # 09 Artigo Y - parentalidade (1 licenca = 6 anos): artigos de 2020-2021 passam a contar
  s <- sol("docente09", "2026-04-02")
  art(s, r("alta", 10), y(5)); art(s, r("alta", 11), y(4)); art(s, r("baixa", 2), y(1))
  lic(s, "parentalidade", sprintf("%d-03-01", y(3)), sprintf("%d-08-28", y(3)),
      "Licença-maternidade (declaração do RH - texto fictício)")
  aval(s); mudar_status(app, s, "enviada"); aval(s, TRUE); mudar_status(app, s, "avaliada")

  # 10 Artigo Y - PcD (+1 ano = 5 anos): artigo de 2021 entra
  s <- sol("docente10", "2026-05-05")
  art(s, r("alta", 12), y(4)); art(s, r("media", 11), y(1)); art(s, r("abaixo", 3), y(0))
  lic(s, "pcd", "2015-01-10", NA, "Dependente com deficiência (laudo - texto fictício)")
  aval(s); mudar_status(app, s, "enviada")

  # 11 Artigo Y - licenca medica de 8 meses (arredonda para +1 ano)
  s <- sol("docente11", "2026-06-18")
  art(s, r("alta", 13), y(4)); art(s, r("media", 12), y(2))
  lic(s, "medica", sprintf("%d-02-01", y(2)), sprintf("%d-09-30", y(2)), "Atestado (texto fictício)")
  aval(s)

  # 12 Artigo Y - 2 parentalidades (pediria 8 anos; a base limita a 6 e avisa)
  s <- sol("docente12", "2026-01-20")
  art(s, r("alta", 14), y(5)); art(s, r("media", 4), y(3)); art(s, r("baixa", 3), y(0))
  if (!is.null(rv$antiga)) art(s, rv$antiga, y(5))
  lic(s, "parentalidade", sprintf("%d-05-01", y(6)), sprintf("%d-10-27", y(6)), "Adoção (texto fictício)")
  lic(s, "parentalidade", sprintf("%d-09-01", y(2)), sprintf("%d-12-30", y(2)), "Licença-paternidade (texto fictício)")
  aval(s); mudar_status(app, s, "enviada")

  # 13 Muitos artigos: 7 cadastrados, so 4 entram (fora do top 4)
  s <- sol("docente13", "2026-09-09")
  for (k in 1:3) art(s, r("media", k + 4), y(k - 1))
  for (k in 4:5) art(s, r("baixa", k), y(k %% 4))
  for (k in 4:5) art(s, r("abaixo", k), y(k %% 4))
  aval(s); mudar_status(app, s, "enviada")

  # 14 Historico: uma solicitacao antiga avaliada + uma nova em rascunho, sem artigos
  s <- sol("docente14", "2025-10-01")
  art(s, r("media", 1), y(1)); aval(s)
  art(s, r("alta", 1), y(2)); aval(s)            # 2o retrato: soma mudou porque entrou um artigo
  mudar_status(app, s, "enviada"); aval(s, TRUE); mudar_status(app, s, "avaliada")
  sol("docente14", "2026-10-20")

  # 15 Recem-cadastrada: rascunho sem nada (fluxo do zero)
  sol("docente15", "2026-11-03")

  # grava as credenciais FORA do repositorio
  dir.create(dirname(credenciais), recursive = TRUE, showWarnings = FALSE)
  out <- data.frame(login = names(logins), senha = vapply(logins, `[`, "", 1),
                    papel = vapply(logins, `[`, "", 2), stringsAsFactors = FALSE)
  utils::write.csv(out, credenciais, row.names = FALSE)
  Sys.chmod(credenciais, "600")
  invisible(list(docentes = length(ids), credenciais = credenciais, duplicata_rejeitada = !dup$ok))
}

# ---- execucao direta ----------------------------------------------------
if (!isTRUE(getOption("ppge.semear.nao_rodar"))) {
  local({
    forcar <- "--forcar" %in% commandArgs(trailingOnly = TRUE) || isTRUE(getOption("ppge.semear.forcar"))
    arq <- caminho_app_db()
    if (forcar && file.exists(arq)) unlink(paste0(arq, c("", "-wal", "-shm")))
    sjr <- conectar_banco()
    app <- abrir_app_db(arq)
    on.exit({ dbDisconnect(app); dbDisconnect(sjr) })
    res <- semear_demo(app, sjr)
    message(sprintf("app.db semeado: %d docentes fictícios + 1 usuário da comissão.", res$docentes))
    message("Credenciais de demonstração (fora do repositório): ", res$credenciais)
  })
}
