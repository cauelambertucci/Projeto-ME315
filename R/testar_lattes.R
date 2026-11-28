# =====================================================================
# testar_lattes.R  -  Confere a leitura do Lattes e o ranking
# Chamado no fim de R/testar_artigo_y.R. Monta um curriculo no formato da
# Plataforma Lattes (ISO-8859-1, dentro de um .zip) com revistas do banco.
# =====================================================================

source("R/lattes.R", encoding = "UTF-8")
source("R/ranking.R", encoding = "UTF-8")

con <- conectar_banco()

# duas revistas com titulo e ISSN sem ambiguidade e p alto
rev <- dbGetQuery(con, "
  SELECT v.titulo, MIN(i.issn) AS issn FROM v_docente v JOIN issn i ON i.sourceid = v.sourceid
  WHERE v.p >= 0.6
    AND v.titulo_norm IN (SELECT titulo_norm FROM revista GROUP BY titulo_norm HAVING COUNT(*) = 1)
    AND i.issn IN (SELECT issn FROM issn GROUP BY issn HAVING COUNT(DISTINCT sourceid) = 1)
  GROUP BY v.sourceid ORDER BY v.p DESC LIMIT 2")

artigo <- function(titulo, ano, revista, issn) sprintf(
  '<ARTIGO-PUBLICADO><DADOS-BASICOS-DO-ARTIGO TITULO-DO-ARTIGO="%s" ANO-DO-ARTIGO="%s"/>
   <DETALHAMENTO-DO-ARTIGO TITULO-DO-PERIODICO-OU-REVISTA="%s" ISSN="%s"/></ARTIGO-PUBLICADO>',
  titulo, ano, revista, issn)
xml <- paste0('<?xml version="1.0" encoding="ISO-8859-1"?><CURRICULO-VITAE>',
  '<DADOS-GERAIS NOME-COMPLETO="Pessoa de Teste"/><PRODUCAO-BIBLIOGRAFICA><ARTIGOS-PUBLICADOS>',
  artigo("Estimação  robusta", 2024, rev$titulo[1], ""),             # casa pelo nome da revista
  artigo("Outro artigo", 2023, "nome qualquer", rev$issn[2]),        # casa pelo ISSN
  artigo("Artigo de 2020", 2020, rev$titulo[2], ""),                 # fora do periodo de 4 anos
  artigo("Revista que nao existe", 2025, "Boletim Inexistente", ""),
  '</ARTIGOS-PUBLICADOS><ARTIGOS-ACEITOS-PARA-PUBLICACAO><ARTIGO-ACEITO-PARA-PUBLICACAO>',
  '<DADOS-BASICOS-DO-ARTIGO TITULO-DO-ARTIGO="Aceito" ANO-DO-ARTIGO="2026"/>',
  '</ARTIGO-ACEITO-PARA-PUBLICACAO></ARTIGOS-ACEITOS-PARA-PUBLICACAO>',
  '</PRODUCAO-BIBLIOGRAFICA></CURRICULO-VITAE>')
pasta <- tempfile(); dir.create(pasta)
writeBin(iconv(xml, "UTF-8", "latin1", toRaw = TRUE)[[1]], file.path(pasta, "curriculo.xml"))

message("Leitura do Lattes:")
lx <- ler_lattes(file.path(pasta, "curriculo.xml"))
chk("le so os artigos publicados", nrow(lx) == 4)
chk("acentos (ISO-8859-1) e espacos duplos", lx$titulo[1] == "Estimação robusta")
chk("le o nome do docente", attr(lx, "nome") == "Pessoa de Teste")
zip_ok <- tryCatch(utils::zip(file.path(pasta, "curriculo.zip"), file.path(pasta, "curriculo.xml"),
                              flags = "-jq") == 0, error = function(e) FALSE, warning = function(w) FALSE)
if (isTRUE(zip_ok)) chk("aceita o .zip", identical(ler_lattes(file.path(pasta, "curriculo.zip"))$titulo, lx$titulo))
writeLines("<outra/>", file.path(pasta, "outro.xml"))
chk("recusa arquivo que nao e Lattes", inherits(try(ler_lattes(file.path(pasta, "outro.xml")), silent = TRUE), "try-error"))

message("Ranking:")
r <- avaliar_lattes(con, lx, n_anos = 4)
d <- r$detalhe
chk("casa por nome e por ISSN", all(d$metodo[d$ano %in% c(2023, 2024)] != "sem_match"))
chk("2020 fora do periodo de 4 anos", d$motivo[d$ano == 2020] == "Fora do período")
chk("revista inexistente nao pontua", d$motivo[d$ano == 2025] == "Revista não encontrada no SJR")
chk("posicao na area e global preenchidas", all(!is.na(d$rank_area[d$ano == 2024])) &&
    all(d$rank_global[d$ano == 2024] <= r$resumo$total_global))
chk("soma = soma dos pontos", abs(sum(d$pontos) - r$resumo$soma) < 1e-6)
d6 <- avaliar_lattes(con, lx, n_anos = 6)$detalhe
chk("com 6 anos (Artigo Y) o artigo de 2020 entra", d6$no_periodo[d6$ano == 2020] == 1)
g <- ranking_revistas(con, 4)
chk("ranking global comeca em 1 e esta em ordem de p", g$posicao[1] == 1 && !is.unsorted(rev(g$p)))
a <- ranking_revistas(con, 4, g$area[1])
chk("ranking de uma area so tem essa area", all(a$area == g$area[1]) && a$posicao[1] == 1)

dbDisconnect(con)
unlink(pasta, recursive = TRUE)
