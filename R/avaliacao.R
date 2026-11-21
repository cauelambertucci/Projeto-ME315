# =====================================================================
# avaliacao.R  -  Avalia os artigos lidos do Lattes (tudo em memoria;
# nada e gravado). Usa as funcoes prontas de funcoes_banco.R e
# periodo_artigo_y.R sobre o banco_edital.db (somente leitura).
#
#   source("R/funcoes_banco.R"); source("R/periodo_artigo_y.R"); source("R/avaliacao.R")
#   sjr <- conectar_banco()
#   r <- avaliar_lattes(sjr, artigos, n_parentalidade = 1)
#   r$resumo$soma; r$detalhe$motivo
# =====================================================================

# Quando o docente confirma a revista de um artigo ambiguo (ISSN/titulo ligado a
# mais de uma revista), a escolha dele vale no calculo, antes do ISSN e do titulo.
# Decisao sinalizada em "Regras e premissas"; para desligar, FALSE.
USAR_CONFIRMACAO_MANUAL <- TRUE

# Faixa "quase apto" (so informativa): soma entre 1,00 e 1,25.
LIMIAR_QUASE_APTO <- 1.00

agora_txt <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

na_se_vazio <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- as.character(x)
  x[!is.na(x) & !nzchar(trimws(x))] <- NA_character_
  x
}

# Revistas do SJR que casam com um artigo (para o seletor de ambiguidade).
candidatos_revista <- function(con_sjr, issn, revista, n_anos) {
  issn_n <- normalizar_issn(na_se_vazio(issn))
  rev_n  <- normalizar_texto(na_se_vazio(revista))
  sql <- "SELECT v.sourceid, v.titulo, v.p, v.percentil_max, v.melhor_area, v.classificada, ? AS via
          FROM v_revista_nota v WHERE v.n_anos = ? AND v.sourceid IN (%s) ORDER BY v.sourceid"
  out <- data.frame()
  if (nzchar(issn_n)) {
    out <- DBI::dbGetQuery(con_sjr, sprintf(sql, "SELECT sourceid FROM issn WHERE issn = ?"),
                           params = list("ISSN", as.integer(n_anos), issn_n))
  }
  if (!nrow(out) && nzchar(rev_n)) {
    out <- DBI::dbGetQuery(con_sjr, sprintf(sql, "SELECT sourceid FROM revista WHERE titulo_norm = ?"),
                           params = list("título", as.integer(n_anos), rev_n))
  }
  out
}

# ---------------------------------------------------------------------
# AVALIACAO
# ---------------------------------------------------------------------

# Igual a avaliar_artigos(), mas respeita a revista confirmada manualmente.
# Reaproveita a PROPRIA avaliar_artigos() (mesmo corpo, mesma consulta) trocando
# apenas: (1) a tabela temporaria ganha a coluna sourceid_conf; (2) a consulta
# ganha um ramo de prioridade 0 ('manual') antes do ISSN e do titulo.
avaliar_artigos_app <- function(con, artigos, n_anos) {
  base_cols <- artigos[, c("titulo", "ano", "revista", "issn"), drop = FALSE]
  conf <- if ("sourceid_conf" %in% names(artigos)) artigos$sourceid_conf else rep(NA_real_, nrow(artigos))
  if (!USAR_CONFIRMACAO_MANUAL || all(is.na(conf))) {
    return(avaliar_artigos(con, base_cols, n_anos = n_anos))
  }
  ancora <- "cand AS (\n"
  if (!grepl(ancora, SQL_AVALIAR_ARTIGOS, fixed = TRUE)) {
    stop("SQL_AVALIAR_ARTIGOS mudou: revise avaliar_artigos_app() em R/banco_app.R.")
  }
  sql_manual <- sub(ancora, paste0(ancora,
    "    SELECT a.id_artigo, a.sourceid_conf AS sourceid, 'manual' AS metodo, 0 AS prioridade\n",
    "    FROM tmp_artigo a WHERE a.sourceid_conf IS NOT NULL\n",
    "    UNION ALL\n"), SQL_AVALIAR_ARTIGOS, fixed = TRUE)
  prep_com_conf <- function(a) {
    out <- preparar_artigos(a)
    out$sourceid_conf <- as.numeric(conf[out$id_artigo])
    out
  }
  f <- avaliar_artigos
  environment(f) <- list2env(list(preparar_artigos = prep_com_conf,
                                  SQL_AVALIAR_ARTIGOS = sql_manual),
                             parent = environment(avaliar_artigos))
  f(con, base_cols, n_anos = n_anos)
}

# Identifica a versao do banco_edital.db (vai no retrato).
.cache_versao <- new.env()
versao_banco_edital <- function(con_sjr) {
  arq <- tryCatch(con_sjr@dbname, error = function(e) "")
  chave <- paste(arq, if (nzchar(arq) && file.exists(arq)) file.mtime(arq) else "")
  if (!is.null(.cache_versao[[chave]])) return(.cache_versao[[chave]])
  info <- DBI::dbGetQuery(con_sjr,
    "SELECT MIN(ano) AS ini, MAX(ano) AS fim, (SELECT COUNT(*) FROM revista) AS n FROM percentil")
  md5 <- if (nzchar(arq) && file.exists(arq)) substr(unname(tools::md5sum(arq)), 1, 8) else "?"
  v <- sprintf("SJR %d-%d | %d revistas | md5 %s", info$ini, info$fim, info$n, md5)
  .cache_versao[[chave]] <- v
  v
}

situacao_soma <- function(soma, meta = 1.25, quase = LIMIAR_QUASE_APTO) {
  meta[is.na(meta)] <- 1.25
  ifelse(is.na(soma), "sem avaliação",
         ifelse(round(soma, 6) >= meta, "apto",
                ifelse(round(soma, 6) >= quase, "quase apto", "não apto")))
}

fmt_num <- function(x, d = 2) formatC(x, format = "f", digits = d, decimal.mark = ",")

# Motivo e explicacao de cada artigo (texto que vai no cartao e no retrato).
explicar_detalhe <- function(det, corte) {
  n <- nrow(det)
  motivo <- character(n); explic <- character(n); conta <- logical(n)
  for (i in seq_len(n)) {
    d <- det[i, ]
    tem_nota <- !is.na(d$percentil_max) && isTRUE(d$classificada == 1)
    explic[i] <- if (d$metodo == "sem_match") {
      "Revista não encontrada no SJR (nem por ISSN nem pelo título)."
    } else if (!tem_nota) {
      sprintf("Revista encontrada no SJR, mas sem percentil entre %d e %d.", d$ano_ini, d$ano_fim)
    } else {
      sprintf("p = %s (percentil %s): melhor área %s, ano %s.",
              fmt_num(d$p), fmt_num(d$percentil_max, 1), d$melhor_area, d$ano_percentil_max)
    }
    conta[i] <- d$situacao == "selecionado" && d$pontos > 0
    motivo[i] <- if (d$situacao == "fora_do_periodo") {
      sprintf("Fora do período (publicado em %d; janela %d–%d).", d$ano, d$ano_ini, d$ano_fim)
    } else if (d$metodo == "sem_match" || !tem_nota) {
      "Revista sem classificação no SJR."
    } else if (d$situacao == "nao_selecionado") {
      "Fora do top 4: há outros artigos com p maior."
    } else if (d$p < corte) {
      sprintf("Selecionado, mas p = %s está abaixo do corte de %s.", fmt_num(d$p), fmt_num(corte))
    } else {
      sprintf("Conta %s ponto(s) na soma.", fmt_num(d$pontos))
    }
  }
  det$explicacao <- explic
  det$motivo <- motivo
  det$conta <- conta
  det
}


# ---------------------------------------------------------------------
# avaliar_lattes: avalia artigos (contrato de preparar_artigos) com o Artigo Y.
#   artigos          data.frame(titulo, ano, revista, issn) - ex.: ler_lattes_xml()
#   n_parentalidade  numero de licencas de parentalidade
#   pcd              TRUE/FALSE
#   meses_medica     meses de licenca medica (vetor soma; meses_entre_datas() ajuda)
#   sourceid_conf    vetor alinhado a `artigos`: revista confirmada (NA = automatico)
# Devolve list(calculada_em, versao_banco, parametros, periodo, licencas, resumo,
#              detalhe, areas). Nada e gravado.
# ---------------------------------------------------------------------
avaliar_lattes <- function(con_sjr, artigos, n_parentalidade = 0, pcd = FALSE, meses_medica = 0,
                           sourceid_conf = NULL) {
  prm <- ler_parametros(con_sjr)
  per <- calcular_periodo(con_sjr, n_parentalidade, pcd, meses_medica)
  if (!per$valido) stop(paste(per$erros, collapse = " "))

  artigos <- as.data.frame(artigos, stringsAsFactors = FALSE)
  if (is.null(sourceid_conf)) sourceid_conf <- rep(NA_real_, nrow(artigos))
  limpa <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
  entrada <- data.frame(titulo = limpa(artigos$titulo), ano = artigos$ano,
                        revista = limpa(artigos$revista), issn = limpa(artigos$issn),
                        sourceid_conf = as.numeric(sourceid_conf), stringsAsFactors = FALSE)
  av  <- avaliar_artigos_app(con_sjr, entrada, n_anos = per$n_anos)
  det <- av$detalhe

  if (nrow(det)) {
    det$artigo_id <- det$id_artigo                      # linha do data.frame de entrada
    det$confirmado_manual <- as.integer(!is.na(entrada$sourceid_conf[det$id_artigo]))
    det$issn <- normalizar_issn(entrada$issn[det$id_artigo])
    extra <- data.frame(sourceid = numeric(0), revista_sjr = character(0),
                        ano_percentil_max = integer(0), n_areas = integer(0), classificada = integer(0))
    ids <- unique(det$sourceid[!is.na(det$sourceid)])
    if (length(ids)) {
      extra <- DBI::dbGetQuery(con_sjr, sprintf(
        "SELECT sourceid, titulo AS revista_sjr, ano_percentil_max, n_areas, classificada
         FROM v_revista_nota WHERE n_anos = %d AND sourceid IN (%s)",
        per$n_anos, paste(sprintf("%.0f", ids), collapse = ",")))
    }
    det <- merge(det, extra, by = "sourceid", all.x = TRUE, sort = FALSE)
    det$classificada[is.na(det$classificada)] <- 0
    det <- det[order(-det$selecionado, ifelse(is.na(det$pos), 999, det$pos), -det$p, det$id_artigo), ]
    det <- explicar_detalhe(det, corte = prm$corte_p)
    rownames(det) <- NULL
  }
  ids <- unique(det$sourceid[!is.na(det$sourceid)])
  areas <- stats::setNames(lapply(ids, function(s) notas_por_area(con_sjr, s, per$n_anos)),
                           sprintf("%.0f", ids))

  lic <- data.frame(
    situacao = c("Licenças de parentalidade", "PcD (docente, cônjuge ou dependente)", "Licença médica (meses)"),
    informado = c(as.character(sum(as.numeric(n_parentalidade), na.rm = TRUE)),
                  if (isTRUE(as.logical(pcd))) "sim" else "não",
                  as.character(sum(as.numeric(meses_medica), na.rm = TRUE))),
    stringsAsFactors = FALSE)

  list(
    calculada_em = agora_txt(),
    versao_banco = versao_banco_edital(con_sjr),
    parametros = prm,
    periodo = list(n_anos = per$n_anos, n_anos_pedido = per$n_anos_pedido, ano_ini = per$ano_ini,
                   ano_fim = per$ano_fim, limitado = per$limitado, avisos = per$avisos,
                   detalhe = per$detalhe),
    licencas = lic,
    resumo = av$resumo,
    detalhe = det,
    areas = areas
  )
}
