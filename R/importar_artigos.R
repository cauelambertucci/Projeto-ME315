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

# Curriculo Lattes (XML exportado da Plataforma Lattes): ARTIGO-PUBLICADO
ler_lattes_xml <- function(caminho) {
  doc <- xml2::read_xml(caminho)
  arts <- xml2::xml_find_all(doc, ".//ARTIGO-PUBLICADO")
  if (!length(arts)) return(.vazio_artigos())
  pega <- function(no, xp, attr) {
    n <- xml2::xml_find_first(no, xp)
    v <- xml2::xml_attr(n, attr)
    ifelse(is.na(v), "", v)
  }
  df <- data.frame(
    titulo  = vapply(arts, pega, "", "./DADOS-BASICOS-DO-ARTIGO", "TITULO-DO-ARTIGO"),
    ano     = vapply(arts, pega, "", "./DADOS-BASICOS-DO-ARTIGO", "ANO-DO-ARTIGO"),
    revista = vapply(arts, pega, "", "./DETALHAMENTO-DO-ARTIGO", "TITULO-DO-PERIODICO-OU-REVISTA"),
    issn    = vapply(arts, pega, "", "./DETALHAMENTO-DO-ARTIGO", "ISSN"),
    stringsAsFactors = FALSE)
  padronizar_colunas(df)
}
