# =====================================================================
# funcoes_banco.R  -  Funcoes que o APP (Shiny) usa para falar com o banco
# source("R/funcoes_banco.R")
# Pacotes: DBI, RSQLite, stringi
# =====================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

DIR_SQL <- "sql"   # pasta com os .sql (relativa a raiz do projeto)

# Consulta da COMISSAO (casa artigos com revistas, escolhe os melhores, soma e compara com a meta).
# Fica aqui dentro para o app NAO depender da pasta sql/. {{N_ANOS}} e trocado por 4, 5 ou 6.
# Espera a tabela temporaria tmp_artigo (criada por avaliar_artigos()).
SQL_AVALIAR_ARTIGOS <- "
WITH
param AS (
    SELECT (SELECT valor FROM parametro WHERE chave = 'corte_p')                     AS corte,
           (SELECT valor FROM parametro WHERE chave = 'meta')                        AS meta,
           (SELECT CAST(valor AS INTEGER) FROM parametro WHERE chave = 'max_artigos') AS max_art,
           (SELECT ano_ini FROM janela WHERE n_anos = {{N_ANOS}})                    AS ano_ini,
           (SELECT ano_fim FROM janela WHERE n_anos = {{N_ANOS}})                    AS ano_fim
),
cand AS (
    SELECT a.id_artigo, i.sourceid, 'issn' AS metodo, 1 AS prioridade
    FROM tmp_artigo a
    JOIN issn i ON i.issn = a.issn_norm
    UNION ALL
    SELECT a.id_artigo, r.sourceid, 'titulo', 2
    FROM tmp_artigo a
    JOIN revista r ON r.titulo_norm = a.revista_norm
    WHERE a.revista_norm <> ''
),
melhor AS (
    SELECT id_artigo, sourceid, metodo,
           ROW_NUMBER() OVER (PARTITION BY id_artigo ORDER BY prioridade, sourceid)  AS rn,
           COUNT(*)     OVER (PARTITION BY id_artigo, prioridade)                    AS n_cand
    FROM cand
),
base AS (
    SELECT a.id_artigo, a.titulo, a.ano, a.revista,
           m.sourceid,
           COALESCE(m.metodo, 'sem_match')  AS metodo,
           COALESCE(m.n_cand, 0)            AS n_candidatos,
           COALESCE(n.p, 0)                 AS p,
           n.percentil_max, n.melhor_area, n.quartil,
           CASE WHEN a.ano BETWEEN pr.ano_ini AND pr.ano_fim THEN 1 ELSE 0 END AS no_periodo,
           pr.corte, pr.meta, pr.max_art, pr.ano_ini, pr.ano_fim
    FROM tmp_artigo a
    CROSS JOIN param pr
    LEFT JOIN melhor m       ON m.id_artigo = a.id_artigo AND m.rn = 1
    LEFT JOIN nota_revista n ON n.sourceid = m.sourceid AND n.n_anos = {{N_ANOS}}
),
ordenado AS (
    SELECT b.*,
           ROW_NUMBER() OVER (PARTITION BY no_periodo
                              ORDER BY p DESC, ano DESC, id_artigo) AS rn_periodo
    FROM base b
),
marcado AS (
    SELECT o.*,
           CASE WHEN no_periodo = 1 THEN rn_periodo END AS pos,
           CASE WHEN no_periodo = 1 AND rn_periodo <= max_art THEN 1 ELSE 0 END AS selecionado
    FROM ordenado o
),
pontuado AS (
    SELECT m.*,
           CASE WHEN selecionado = 1 AND p >= corte THEN p ELSE 0 END AS pontos
    FROM marcado m
)
SELECT id_artigo, titulo, ano, revista, sourceid, metodo, n_candidatos,
       melhor_area, quartil,
       ROUND(percentil_max, 2) AS percentil_max,
       p, no_periodo, pos, selecionado, pontos,
       CASE WHEN no_periodo = 0 THEN 'fora_do_periodo'
            WHEN selecionado = 1 THEN 'selecionado'
            ELSE 'nao_selecionado' END AS situacao,
       ROUND(SUM(pontos) OVER (), 6) AS soma,
       meta,
       CASE WHEN ROUND(SUM(pontos) OVER (), 6) >= meta THEN 1 ELSE 0 END AS aprovado,
       ano_ini, ano_fim
FROM pontuado
ORDER BY selecionado DESC, pos, no_periodo DESC, p DESC, id_artigo
"

# ---------------------------------------------------------------------
# Normalizacao (MESMA funcao no build e no app, senao o casamento falha)
# ---------------------------------------------------------------------

# "Revista Brasileira de Estatística & Cia." -> "revista brasileira de estatistica and cia"
normalizar_texto <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- stringi::stri_trans_tolower(x)
  x <- stringi::stri_replace_all_fixed(x, "&", " and ")
  x <- stringi::stri_replace_all_regex(x, "[^a-z0-9]+", " ")
  stringi::stri_trim_both(x)
}

# "1234-567x" -> "1234567X". Devolve "" se nao der um ISSN de 8 caracteres.
# Repoe zeros a esquerda quando o ISSN foi lido como numero e perdeu zeros.
normalizar_issn <- function(x) {
  x <- toupper(gsub("[^0-9Xx]", "", as.character(x)))
  x[is.na(x)] <- ""
  curto <- nchar(x) >= 5 & nchar(x) < 8
  x[curto] <- stringi::stri_pad_left(x[curto], 8, "0")
  x[nchar(x) != 8] <- ""
  x
}

# ---------------------------------------------------------------------
# Execucao de arquivos .sql (usado no build)
# Tira comentarios "--" e executa comando a comando (divide em ";").
# ---------------------------------------------------------------------
executar_sql_arquivo <- function(con, caminho) {
  linhas <- readLines(caminho, encoding = "UTF-8", warn = FALSE)
  linhas <- sub("--.*$", "", linhas)
  cmds <- trimws(strsplit(paste(linhas, collapse = "\n"), ";", fixed = TRUE)[[1]])
  cmds <- cmds[nzchar(cmds)]
  for (cmd in cmds) DBI::dbExecute(con, cmd)
  invisible(length(cmds))
}

# le uma consulta (um unico SELECT) de um arquivo, sem o ";" final
ler_consulta <- function(caminho) {
  txt <- paste(readLines(caminho, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  sub(";\\s*$", "", txt)
}

# ---------------------------------------------------------------------
# Conexao
# ---------------------------------------------------------------------
# Somente leitura (o app nunca altera o banco). bigint = "numeric" faz o
# sourceid voltar como numero comum (alguns passam de 2^31 e NAO cabem em
# integer: nunca use as.integer() nele).
conectar_banco <- function(caminho = "banco_edital.db", somente_leitura = TRUE) {
  if (!file.exists(caminho)) {
    stop("Banco '", caminho, "' nao encontrado. Gere-o abrindo banco_edital.Rmd no RStudio ",
         "e clicando em Knit (ou rode rmarkdown::render('banco_edital.Rmd') na raiz do projeto).")
  }
  flags <- if (somente_leitura) RSQLite::SQLITE_RO else RSQLite::SQLITE_RW
  DBI::dbConnect(RSQLite::SQLite(), caminho, flags = flags, bigint = "numeric")
}

# regras do edital como lista: n_anos_padrao, ano_fim, corte_p, meta, max_artigos
ler_parametros <- function(con) {
  p <- DBI::dbGetQuery(con, "SELECT chave, valor FROM parametro")
  as.list(stats::setNames(p$valor, p$chave))
}

# janelas disponiveis: n_anos, ano_ini, ano_fim, padrao
listar_janelas <- function(con) {
  DBI::dbGetQuery(con, "SELECT n_anos, ano_ini, ano_fim, padrao FROM janela ORDER BY n_anos")
}

listar_areas <- function(con) {
  DBI::dbGetQuery(con, "SELECT area, n_revistas FROM area ORDER BY area")
}

# ---------------------------------------------------------------------
# VISAO DO DOCENTE
# ---------------------------------------------------------------------

# Autocompletar: todas as palavras digitadas precisam aparecer no titulo.
# Devolve: sourceid, titulo, n_anos, p, percentil_max, quartil, melhor_area,
#          classificada, conta_pontos
buscar_revista <- function(con, texto, n_anos = NULL, limite = 15) {
  n_anos <- as.integer(n_anos %||% ler_parametros(con)$n_anos_padrao)
  q <- normalizar_texto(texto)
  palavras <- strsplit(q, " ", fixed = TRUE)[[1]]
  palavras <- palavras[nzchar(palavras)]
  if (length(palavras) == 0) return(data.frame())
  cond <- paste(rep("titulo_norm LIKE ?", length(palavras)), collapse = " AND ")
  sql <- sprintf(
    "SELECT sourceid, titulo, n_anos, p, percentil_max, quartil, melhor_area,
            classificada, conta_pontos
     FROM v_revista_nota
     WHERE n_anos = %d AND %s
     ORDER BY CASE WHEN titulo_norm = ? THEN 0 WHEN titulo_norm LIKE ? THEN 1 ELSE 2 END,
              p DESC, titulo
     LIMIT %d", n_anos, cond, as.integer(limite))
  params <- c(as.list(paste0("%", palavras, "%")), list(q), list(paste0(q, "%")))
  DBI::dbGetQuery(con, sql, params = params)
}

# Nota da revista em CADA area (+ qual e a melhor).
# Devolve: area, percentil_max (0-100), p (0-1), ano_percentil_max, e_melhor_area
notas_por_area <- function(con, sourceid, n_anos = NULL) {
  n_anos <- as.integer(n_anos %||% ler_parametros(con)$n_anos_padrao)
  DBI::dbGetQuery(con,
    "SELECT area, percentil_max, p, ano_percentil_max, e_melhor_area
     FROM v_revista_area
     WHERE sourceid = ? AND n_anos = ?
     ORDER BY p DESC, area",
    params = list(sourceid, n_anos))
}

# ---------------------------------------------------------------------
# VISAO DA COMISSAO
# ---------------------------------------------------------------------

# artigos: data.frame com colunas  titulo, ano, revista, issn
# (e o que o parser do Lattes deve entregar).
preparar_artigos <- function(artigos) {
  falta <- setdiff(c("titulo", "ano", "revista", "issn"), names(artigos))
  if (length(falta)) stop("Faltam colunas em 'artigos': ", paste(falta, collapse = ", "))
  out <- data.frame(
    id_artigo    = seq_len(nrow(artigos)),
    titulo       = as.character(artigos$titulo),
    ano          = suppressWarnings(as.integer(artigos$ano)),
    revista      = as.character(artigos$revista),
    revista_norm = normalizar_texto(artigos$revista),
    issn_norm    = normalizar_issn(artigos$issn),
    stringsAsFactors = FALSE
  )
  # mesmo artigo repetido no Lattes (mesmo titulo normalizado e ano) conta uma vez
  tit <- normalizar_texto(out$titulo)
  out[!(duplicated(paste(tit, out$ano)) & nzchar(tit)), , drop = FALSE]
}

# Aplica a regra do edital: 4 melhores artigos, soma dos p >= corte, compara com a meta.
# n_anos = tamanho do periodo (4 padrao, 5 ou 6 se o Artigo Y estender).
# Devolve list(detalhe = data.frame (1 linha por artigo), resumo = list(...))
avaliar_artigos <- function(con, artigos, n_anos = NULL, dir_sql = DIR_SQL) {
  prm <- ler_parametros(con)
  n_anos <- as.integer(n_anos %||% prm$n_anos_padrao)
  jan <- listar_janelas(con)
  if (!n_anos %in% jan$n_anos) {
    stop("Periodo de ", n_anos, " anos indisponivel. Disponiveis: ",
         paste(jan$n_anos, collapse = ", "),
         " (a base so tem dados desde ", min(jan$ano_ini), ").")
  }
  j <- jan[jan$n_anos == n_anos, ]

  art <- preparar_artigos(artigos)
  DBI::dbWriteTable(con, "tmp_artigo", art, temporary = TRUE, overwrite = TRUE)
  on.exit(try(DBI::dbRemoveTable(con, "tmp_artigo"), silent = TRUE), add = TRUE)

  sql <- SQL_AVALIAR_ARTIGOS   # (dir_sql nao e mais usado; mantido por compatibilidade)
  sql <- gsub("{{N_ANOS}}", n_anos, sql, fixed = TRUE)   # n_anos e inteiro ja validado acima
  det <- DBI::dbGetQuery(con, sql)

  soma <- if (nrow(det)) det$soma[1] else 0
  list(
    detalhe = det,
    resumo = list(
      n_anos = n_anos, ano_ini = j$ano_ini, ano_fim = j$ano_fim,
      soma = soma, meta = prm$meta, aprovado = isTRUE(round(soma, 6) >= prm$meta),
      n_artigos_lattes = nrow(art),
      n_no_periodo = if (nrow(det)) sum(det$no_periodo) else 0,
      n_selecionados = if (nrow(det)) sum(det$selecionado) else 0,
      n_sem_revista = if (nrow(det)) sum(det$metodo == "sem_match") else 0,
      n_ambiguos = if (nrow(det)) sum(det$n_candidatos > 1) else 0
    )
  )
}
