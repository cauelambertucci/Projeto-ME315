# =====================================================================
# lattes.R  -  Le o curriculo exportado da Plataforma Lattes
# Aceita o .zip (Exportar > XML) ou o curriculo.xml de dentro dele.
# Devolve o contrato de preparar_artigos(): titulo, ano, revista, issn
# (e o nome do docente em attr(, "nome")).
# =====================================================================

ler_lattes <- function(caminho) {
  # o .zip comeca com os bytes "PK": extrai o primeiro .xml
  if (identical(readBin(caminho, "raw", 2), charToRaw("PK"))) {
    pasta <- tempfile()
    xml <- grep("\\.xml$", utils::unzip(caminho, list = TRUE)$Name, value = TRUE)[1]
    if (is.na(xml)) stop("O .zip nao tem nenhum arquivo .xml.")
    caminho <- utils::unzip(caminho, files = xml, exdir = pasta)
  }
  doc <- tryCatch(xml2::read_xml(caminho),
                  error = function(e) stop("Nao consegui ler o XML. Envie o arquivo exportado pelo Lattes."))
  if (xml2::xml_name(doc) != "CURRICULO-VITAE") stop("O arquivo nao e um curriculo Lattes.")

  # so artigos publicados (os aceitos ficam de fora, como no edital)
  artigos <- xml2::xml_find_all(doc, ".//ARTIGOS-PUBLICADOS/ARTIGO-PUBLICADO")
  campo <- function(no, atributo) {
    x <- xml2::xml_attr(xml2::xml_find_first(artigos, no), atributo)
    trimws(gsub("\\s+", " ", ifelse(is.na(x), "", x)))
  }
  df <- data.frame(
    titulo  = campo("./DADOS-BASICOS-DO-ARTIGO", "TITULO-DO-ARTIGO"),
    ano     = campo("./DADOS-BASICOS-DO-ARTIGO", "ANO-DO-ARTIGO"),
    revista = campo("./DETALHAMENTO-DO-ARTIGO", "TITULO-DO-PERIODICO-OU-REVISTA"),
    issn    = campo("./DETALHAMENTO-DO-ARTIGO", "ISSN"),
    stringsAsFactors = FALSE
  )
  attr(df, "nome") <- xml2::xml_attr(xml2::xml_find_first(doc, "./DADOS-GERAIS"), "NOME-COMPLETO")
  df
}
