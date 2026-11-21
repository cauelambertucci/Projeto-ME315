# =====================================================================
# importar_artigos.R  -  Leitores de artigos (lista colada, CSV, Lattes XML)
# Todos devolvem o CONTRATO de preparar_artigos():
#   data.frame(titulo, ano, revista, issn)  (texto; ano como texto/numero)
# =====================================================================

.vazio_artigos <- function() data.frame(titulo = character(0), ano = character(0),
                                        revista = character(0), issn = character(0),
                                        stringsAsFactors = FALSE)

# Renomeia colunas usuais (titulo/título/title, ano/year, revista/periodico/journal, issn)
padronizar_colunas <- function(df) {
  nm <- normalizar_texto(names(df))
  mapa <- c(titulo = "titulo", title = "titulo", titulo_do_artigo = "titulo", artigo = "titulo",
            ano = "ano", year = "ano", ano_do_artigo = "ano",
            revista = "revista", periodico = "revista", journal = "revista", source_title = "revista",
            titulo_do_periodico = "revista", issn = "issn", eissn = "issn")
  nm <- gsub(" ", "_", nm)
  novo <- ifelse(nm %in% names(mapa), mapa[nm], nm)
  names(df) <- novo
  for (c in c("titulo", "ano", "revista", "issn")) if (!c %in% names(df)) df[[c]] <- ""
  df <- df[, c("titulo", "ano", "revista", "issn")]
  df[] <- lapply(df, function(x) { x <- as.character(x); x[is.na(x)] <- ""; trimws(x) })
  df[nzchar(df$titulo), , drop = FALSE]
}

# Uma linha por artigo: "Titulo; Ano; Revista; ISSN"  (separador ; ou TAB ou |)
ler_lista_colada <- function(texto) {
  linhas <- trimws(strsplit(texto %||% "", "\r?\n")[[1]])
  linhas <- linhas[nzchar(linhas)]
  if (!length(linhas)) return(.vazio_artigos())
  partes <- lapply(linhas, function(l) {
    sep <- if (grepl("\t", l)) "\t" else if (grepl(";", l)) ";" else "|"
    p <- trimws(strsplit(l, sep, fixed = TRUE)[[1]]); length(p) <- 4; p
  })
  df <- as.data.frame(do.call(rbind, partes), stringsAsFactors = FALSE)
  names(df) <- c("titulo", "ano", "revista", "issn")
  if (normalizar_texto(df$titulo[1]) %in% c("titulo", "title")) df <- df[-1, , drop = FALSE]
  padronizar_colunas(df)
}

# CSV com cabecalho (titulo, ano, revista, issn). Detecta ; ou , ; UTF-8 com ou sem BOM.
ler_csv_artigos <- function(caminho) {
  primeira <- readLines(caminho, n = 1, warn = FALSE, encoding = "UTF-8")
  sep <- if (lengths(regmatches(primeira, gregexpr(";", primeira))) >=
             lengths(regmatches(primeira, gregexpr(",", primeira)))) ";" else ","
  df <- utils::read.csv(caminho, sep = sep, colClasses = "character", fileEncoding = "UTF-8-BOM",
                        check.names = FALSE, stringsAsFactors = FALSE)
  padronizar_colunas(df)
}

# ---------------------------------------------------------------------
# Curriculo Lattes. Aceita o que a Plataforma Lattes entrega:
#   - o .zip baixado em "Exportar > XML" (contem curriculo.xml), ou
#   - o .xml solto.
# O arquivo real vem em ISO-8859-1 (declarado no cabecalho); xml2 respeita.
# Le so ARTIGO-PUBLICADO (artigos aceitos/no prelo ficam de fora, como no edital).
# Devolve o contrato data.frame(titulo, ano, revista, issn) com atributos:
#   attr(, "docente") = list(nome, lattes_id, atualizado_em)
#   attr(, "extra")   = data.frame(doi, natureza) alinhado as linhas
# ---------------------------------------------------------------------
abrir_xml_lattes <- function(caminho) {
  cab <- readBin(caminho, "raw", n = 4)
  if (length(cab) >= 2 && identical(cab[1:2], as.raw(c(0x50, 0x4B)))) {       # "PK" = zip
    dir <- tempfile("lattes_"); dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    arqs <- utils::unzip(caminho, list = TRUE)$Name
    xmls <- arqs[grepl("\\.xml$", arqs, ignore.case = TRUE)]
    if (!length(xmls)) stop("O .zip não contém nenhum arquivo .xml do Lattes.")
    utils::unzip(caminho, files = xmls[1], exdir = dir, junkpaths = TRUE)
    caminho <- file.path(dir, basename(xmls[1]))
  }
  doc <- tryCatch(xml2::read_xml(caminho, options = c("RECOVER", "NOBLANKS", "HUGE")),
                  error = function(e) NULL)
  if (is.null(doc)) {
    # 2a tentativa: corrige defeitos comuns de exportacao ("&" solto, caracteres de controle)
    bruto <- readBin(caminho, "raw", file.info(caminho)$size)
    txt <- rawToChar(bruto[bruto != as.raw(0)])
    txt <- gsub("&(?!(amp|lt|gt|quot|apos|#[0-9]+|#x[0-9A-Fa-f]+);)", "&amp;", txt, perl = TRUE, useBytes = TRUE)
    txt <- gsub("[\\x01-\\x08\\x0B\\x0C\\x0E-\\x1F]", "", txt, perl = TRUE, useBytes = TRUE)
    f2 <- tempfile(fileext = ".xml"); on.exit(unlink(f2), add = TRUE)
    writeBin(charToRaw(txt), f2)
    doc <- tryCatch(xml2::read_xml(f2, options = c("RECOVER", "NOBLANKS", "HUGE")),
                    error = function(e) stop("Não consegui ler o XML do Lattes: ", conditionMessage(e)))
  }
  if (!identical(xml2::xml_name(doc), "CURRICULO-VITAE")) {
    stop("Este arquivo não parece um currículo Lattes (falta o elemento CURRICULO-VITAE).")
  }
  doc
}

ler_lattes_xml <- function(caminho) {
  doc  <- abrir_xml_lattes(caminho)
  raiz <- doc
  dg   <- xml2::xml_find_first(doc, "./DADOS-GERAIS")
  info <- list(
    nome          = xml2::xml_attr(dg, "NOME-COMPLETO") %|v|% "",
    lattes_id     = xml2::xml_attr(raiz, "NUMERO-IDENTIFICADOR") %|v|% "",
    atualizado_em = xml2::xml_attr(raiz, "DATA-ATUALIZACAO") %|v|% "")
  arts <- xml2::xml_find_all(doc, ".//PRODUCAO-BIBLIOGRAFICA/ARTIGOS-PUBLICADOS/ARTIGO-PUBLICADO")
  if (!length(arts)) arts <- xml2::xml_find_all(doc, ".//ARTIGO-PUBLICADO")
  pega <- function(xp, attr) {
    v <- xml2::xml_attr(xml2::xml_find_first(arts, xp), attr)
    v[is.na(v)] <- ""
    trimws(gsub("\\s+", " ", v))
  }
  df <- data.frame(
    titulo  = pega("./DADOS-BASICOS-DO-ARTIGO", "TITULO-DO-ARTIGO"),
    ano     = pega("./DADOS-BASICOS-DO-ARTIGO", "ANO-DO-ARTIGO"),
    revista = pega("./DETALHAMENTO-DO-ARTIGO", "TITULO-DO-PERIODICO-OU-REVISTA"),
    issn    = pega("./DETALHAMENTO-DO-ARTIGO", "ISSN"),
    stringsAsFactors = FALSE)
  extra <- data.frame(doi = pega("./DADOS-BASICOS-DO-ARTIGO", "DOI"),
                      natureza = pega("./DADOS-BASICOS-DO-ARTIGO", "NATUREZA"),
                      stringsAsFactors = FALSE)
  manter <- nzchar(df$titulo)
  out <- padronizar_colunas(df)
  attr(out, "docente") <- info
  attr(out, "extra") <- extra[manter, , drop = FALSE]
  out
}

`%|v|%` <- function(a, b) if (length(a) == 0 || is.na(a[1]) || !nzchar(a[1])) b else a[1]

# Pre-visualizacao: para cada artigo, se a revista existe no SJR (por ISSN, depois
# titulo normalizado - a mesma ordem de avaliar_artigos) e o p na janela pedida.
# So informativo: a nota que vale e a calculada na avaliacao.
previsualizar_artigos <- function(con_sjr, df, n_anos = 4) {
  if (!nrow(df)) return(cbind(df, sjr = character(0), revista_sjr = character(0), p = numeric(0)))
  issn_n <- normalizar_issn(df$issn)
  rev_n  <- normalizar_texto(df$revista)
  tmp <- data.frame(i = seq_len(nrow(df)), issn_n = issn_n, rev_n = rev_n, stringsAsFactors = FALSE)
  nome <- paste0("tmp_prev_", as.integer(stats::runif(1, 1, 1e9)))
  DBI::dbWriteTable(con_sjr, nome, tmp, temporary = TRUE)
  on.exit(try(DBI::dbRemoveTable(con_sjr, nome), silent = TRUE), add = TRUE)
  m <- DBI::dbGetQuery(con_sjr, sprintf("
    WITH c AS (
      SELECT t.i, i.sourceid, 1 AS prio FROM %1$s t JOIN issn i ON i.issn = t.issn_n WHERE t.issn_n <> ''
      UNION ALL
      SELECT t.i, r.sourceid, 2 FROM %1$s t JOIN revista r ON r.titulo_norm = t.rev_n WHERE t.rev_n <> ''),
    m AS (SELECT i, sourceid, prio, ROW_NUMBER() OVER (PARTITION BY i ORDER BY prio, sourceid) AS rn FROM c)
    SELECT m.i, m.prio, v.titulo AS revista_sjr, v.p, v.classificada
    FROM m JOIN v_revista_nota v ON v.sourceid = m.sourceid AND v.n_anos = %2$d
    WHERE m.rn = 1", nome, as.integer(n_anos)))
  out <- df
  out$sjr <- "não encontrada"
  out$revista_sjr <- NA_character_
  out$p <- NA_real_
  out$sjr[m$i] <- ifelse(m$prio == 1, "sim (ISSN)", "sim (título)")
  out$revista_sjr[m$i] <- m$revista_sjr
  out$p[m$i] <- ifelse(m$classificada == 1, m$p, 0)
  out
}
