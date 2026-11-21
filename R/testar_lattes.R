# =====================================================================
# testar_lattes.R  -  Testes da leitura do Lattes e da avaliacao em memoria
# Chamado no fim de R/testar_artigo_y.R; tambem roda sozinho:
#   source("R/testar_lattes.R")
# Monta um curriculo no FORMATO REAL da Plataforma Lattes (ISO-8859-1,
# compactado em .zip) com revistas sorteadas do banco_edital.db.
# =====================================================================

suppressPackageStartupMessages({ library(DBI); library(RSQLite) })
source("R/funcoes_banco.R", encoding = "UTF-8")
source("R/periodo_artigo_y.R", encoding = "UTF-8")
source("R/avaliacao.R", encoding = "UTF-8")
source("R/importar_artigos.R", encoding = "UTF-8")

local({
  sjr <- conectar_banco(Sys.getenv("PPGE_BANCO_EDITAL", "banco_edital.db"))
  dir_l <- tempfile("lattes_teste_"); dir.create(dir_l)
  on.exit({ dbDisconnect(sjr); unlink(dir_l, recursive = TRUE) })
  chk <- function(nome, cond) {
    if (!isTRUE(cond)) stop("FALHOU: ", nome, call. = FALSE)
    message("  ok  ", nome)
  }

  # revistas com titulo e ISSN sem ambiguidade, p alto na janela de 4 anos
  rv <- dbGetQuery(sjr, "
    SELECT v.titulo, MIN(i.issn) AS issn, v.p FROM v_docente v JOIN issn i ON i.sourceid = v.sourceid
    WHERE v.p >= 0.6
      AND v.titulo_norm IN (SELECT titulo_norm FROM revista GROUP BY titulo_norm HAVING COUNT(*) = 1)
      AND i.issn IN (SELECT issn FROM issn GROUP BY issn HAVING COUNT(DISTINCT sourceid) = 1)
    GROUP BY v.sourceid ORDER BY v.p DESC LIMIT 3")
  chk("achei revistas de teste no banco", nrow(rv) == 3)
  xml_esc <- function(x) gsub("\"", "&quot;", gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE), fixed = TRUE)
  art_xml <- function(seq, tit, ano, rev, issn) sprintf(paste0(
    '<ARTIGO-PUBLICADO SEQUENCIA-PRODUCAO="%d" ORDEM-IMPORTANCIA="">',
    '<DADOS-BASICOS-DO-ARTIGO NATUREZA="COMPLETO" TITULO-DO-ARTIGO="%s" ANO-DO-ARTIGO="%s" PAIS-DE-PUBLICACAO="Brasil" ',
    'IDIOMA="Português" MEIO-DE-DIVULGACAO="DIGITAL" HOME-PAGE-DO-TRABALHO="" FLAG-RELEVANCIA="NAO" DOI="" ',
    'TITULO-DO-ARTIGO-INGLES="" FLAG-DIVULGACAO-CIENTIFICA="NAO"/>',
    '<DETALHAMENTO-DO-ARTIGO TITULO-DO-PERIODICO-OU-REVISTA="%s" ISSN="%s" VOLUME="12" FASCICULO="3" SERIE="" ',
    'PAGINA-INICIAL="1" PAGINA-FINAL="20" LOCAL-DE-PUBLICACAO=""/>',
    '<AUTORES NOME-COMPLETO-DO-AUTOR="Pessoa de Teste" NOME-PARA-CITACAO="TESTE, P." ORDEM-DE-AUTORIA="1"/>',
    '</ARTIGO-PUBLICADO>'), seq, xml_esc(tit), ano, xml_esc(rev), issn)
  xml_txt <- paste0(
    '<?xml version="1.0" encoding="ISO-8859-1" standalone="no" ?>\n',
    '<CURRICULO-VITAE SISTEMA-ORIGEM-XML="LATTES_OFFLINE" NUMERO-IDENTIFICADOR="0000000000000000" ',
    'DATA-ATUALIZACAO="10112026" HORA-ATUALIZACAO="101500">',
    '<DADOS-GERAIS NOME-COMPLETO="Pessoa de Teste" NACIONALIDADE="B"/>',
    '<PRODUCAO-BIBLIOGRAFICA><ARTIGOS-PUBLICADOS>',
    art_xml(1, "Estimação em séries com ruído & tendência", 2024, rv$titulo[1], ""),            # casa pelo titulo
    art_xml(2, "Um artigo  com espaços\n duplos", 2023, "nome diferente da revista", rv$issn[2]), # casa pelo ISSN
    art_xml(3, "Artigo de 2020", 2020, toupper(rv$titulo[3]), ""),                             # fora em 4 anos
    art_xml(4, "Artigo antigo", 2012, rv$titulo[1], ""),
    art_xml(5, "Artigo em revista fora do SJR", 2023, "Boletim Inexistente de Testes", "99999999"),
    art_xml(6, "ESTIMAÇÃO EM SÉRIES COM RUÍDO & TENDÊNCIA!", 2024, rv$titulo[1], ""),          # duplicata
    '</ARTIGOS-PUBLICADOS><ARTIGOS-ACEITOS-PARA-PUBLICACAO><ARTIGO-ACEITO-PARA-PUBLICACAO SEQUENCIA-PRODUCAO="9">',
    '<DADOS-BASICOS-DO-ARTIGO TITULO-DO-ARTIGO="Aceito, ainda não publicado" ANO-DO-ARTIGO="2026"/>',
    '<DETALHAMENTO-DO-ARTIGO TITULO-DO-PERIODICO-OU-REVISTA="X" ISSN="12345678"/>',
    '</ARTIGO-ACEITO-PARA-PUBLICACAO></ARTIGOS-ACEITOS-PARA-PUBLICACAO></PRODUCAO-BIBLIOGRAFICA></CURRICULO-VITAE>')
  xml_l <- file.path(dir_l, "curriculo.xml")
  writeBin(iconv(xml_txt, "UTF-8", "latin1", toRaw = TRUE)[[1]], xml_l)

  message("Leitura do Lattes")
  lx <- ler_lattes_xml(xml_l)
  chk("lê só ARTIGO-PUBLICADO (aceitos ficam de fora)", nrow(lx) == 6)
  chk("contrato titulo/ano/revista/issn", identical(names(lx), c("titulo", "ano", "revista", "issn")))
  chk("ISO-8859-1: acentos e & preservados; espaços normalizados",
      lx$titulo[1] == "Estimação em séries com ruído & tendência" && lx$titulo[2] == "Um artigo com espaços duplos")
  chk("nome, ID e data do currículo lidos",
      attr(lx, "docente")$nome == "Pessoa de Teste" && attr(lx, "docente")$lattes_id == "0000000000000000" &&
      attr(lx, "docente")$atualizado_em == "10112026")
  old <- setwd(dir_l)
  zip_ok <- tryCatch(utils::zip("curriculo.zip", "curriculo.xml", flags = "-q") == 0,
                     error = function(e) FALSE, warning = function(w) FALSE)
  setwd(old)
  if (isTRUE(zip_ok)) {
    chk("aceita o .zip baixado da plataforma", identical(ler_lattes_xml(file.path(dir_l, "curriculo.zip"))$titulo, lx$titulo))
  } else message("  (sem utilitário zip nesta máquina: teste do .zip pulado)")
  ruim <- file.path(dir_l, "ruim.xml")
  writeBin(charToRaw(paste0('<?xml version="1.0" encoding="UTF-8"?><CURRICULO-VITAE><DADOS-GERAIS NOME-COMPLETO="A"/>',
    '<PRODUCAO-BIBLIOGRAFICA><ARTIGOS-PUBLICADOS><ARTIGO-PUBLICADO><DADOS-BASICOS-DO-ARTIGO ',
    'TITULO-DO-ARTIGO="Ciência & Dados" ANO-DO-ARTIGO="2024"/><DETALHAMENTO-DO-ARTIGO ',
    'TITULO-DO-PERIODICO-OU-REVISTA="R & S" ISSN=""/></ARTIGO-PUBLICADO></ARTIGOS-PUBLICADOS>',
    '</PRODUCAO-BIBLIOGRAFICA></CURRICULO-VITAE>')), ruim)
  chk("tolera '&' sem escape (defeito comum de exportação)", ler_lattes_xml(ruim)$titulo == "Ciência & Dados")
  chk("arquivo que não é Lattes dá erro claro", {
    f <- file.path(dir_l, "outro.xml"); writeLines("<outra/>", f)
    grepl("currículo Lattes", tryCatch(ler_lattes_xml(f), error = conditionMessage))
  })

  message("Prévia do casamento com o SJR")
  pv <- previsualizar_artigos(sjr, lx, n_anos = 6)
  chk("casa pelo título, pelo ISSN e marca revista fora do SJR",
      pv$sjr[1] == "sim (título)" && pv$sjr[2] == "sim (ISSN)" && pv$sjr[5] == "não encontrada" && !is.na(pv$p[1]))

  message("Avaliação em memória (Artigo X + Artigo Y)")
  r4 <- avaliar_lattes(sjr, lx)
  d4 <- r4$detalhe
  chk("janela padrão 2022–2025", r4$periodo$n_anos == 4 && r4$periodo$ano_ini == 2022 && r4$periodo$ano_fim == 2025)
  chk("duplicata (mesmo título normalizado + ano) conta uma vez", nrow(d4) == 5)
  chk("2012 e 2020 ficam fora do período", all(d4$situacao[d4$ano %in% c(2012, 2020)] == "fora_do_periodo"))
  chk("revista inexistente = sem_match, sem pontos", d4$metodo[d4$ano == 2023 & grepl("fora do SJR", d4$titulo)] == "sem_match")
  chk("soma = soma dos pontos; até 4 selecionados",
      abs(sum(d4$pontos) - r4$resumo$soma) < 1e-6 && sum(d4$selecionado) <= 4)
  chk("cada artigo tem motivo e explicação", all(nzchar(d4$motivo)) && all(nzchar(d4$explicacao)))
  chk("explicação mostra p e percentil (duas escalas)", any(grepl("^p = .*percentil", d4$explicacao)))
  r6 <- avaliar_lattes(sjr, lx, n_parentalidade = 1)
  chk("1 licença de parentalidade: 6 anos e o artigo de 2020 entra",
      r6$periodo$n_anos == 6 && r6$detalhe$no_periodo[r6$detalhe$ano == 2020] == 1)
  r5 <- avaliar_lattes(sjr, lx, meses_medica = meses_entre_datas("2023-02-01", "2023-09-30"))
  chk("licença médica de 8 meses (pelas datas): 5 anos", r5$periodo$n_anos == 5)
  rl <- avaliar_lattes(sjr, lx, n_parentalidade = 2)
  chk("2 parentalidades: pede 8, usa 6 e avisa", rl$periodo$n_anos == 6 && isTRUE(rl$periodo$limitado) &&
      length(rl$periodo$avisos) >= 1)
  chk("licença inválida não quebra: erro claro", grepl("parentalidade",
      tryCatch(avaliar_lattes(sjr, lx, n_parentalidade = -1), error = conditionMessage)))
  chk("nenhum artigo: soma 0, não apto", { r0 <- avaliar_lattes(sjr, lx[0, ]); r0$resumo$soma == 0 && !r0$resumo$aprovado })
  chk("situacao_soma: 1,25 apto; 1,10 quase; 0,99 não; NA sem",
      identical(situacao_soma(c(1.25, 1.10, 0.99, NA)), c("apto", "quase apto", "não apto", "sem avaliação")))

  message("Revista ambígua e escolha do docente")
  amb <- dbGetQuery(sjr, "SELECT issn, MIN(sourceid) AS menor, MAX(sourceid) AS maior FROM issn
                          GROUP BY issn HAVING COUNT(DISTINCT sourceid) > 1 LIMIT 1")
  if (nrow(amb)) {
    a <- data.frame(titulo = "Artigo ambíguo", ano = 2024, revista = "", issn = amb$issn)
    ra <- avaliar_lattes(sjr, a)
    chk("ISSN ambíguo: usa o menor sourceid e marca n_candidatos > 1",
        ra$detalhe$sourceid == amb$menor && ra$detalhe$n_candidatos > 1)
    chk("candidatos_revista lista as revistas do ISSN", nrow(candidatos_revista(sjr, amb$issn, "", 4)) >= 2)
    rc <- avaliar_lattes(sjr, a, sourceid_conf = amb$maior)
    chk("com a escolha do docente, usa a revista escolhida (método manual)",
        rc$detalhe$sourceid == amb$maior && rc$detalhe$metodo == "manual" && rc$detalhe$confirmado_manual == 1)
  } else message("  (banco sem ISSN ambíguo: testes de ambiguidade pulados)")
})
