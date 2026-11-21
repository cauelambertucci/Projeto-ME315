# =====================================================================
# testar_app.R  -  Testes do banco de credenciamento (app.db) e do RETRATO
# Chamado no fim de R/testar_artigo_y.R; tambem roda sozinho:
#   source("R/testar_app.R")
# Usa um app.db TEMPORARIO (nao mexe no app.db da demo) e abre o
# banco_edital.db somente leitura.
# =====================================================================

suppressPackageStartupMessages({ library(DBI); library(RSQLite) })
source("R/funcoes_banco.R", encoding = "UTF-8")
source("R/periodo_artigo_y.R", encoding = "UTF-8")
source("R/banco_app.R", encoding = "UTF-8")
source("R/importar_artigos.R", encoding = "UTF-8")

local({
  arq_app <- tempfile(fileext = ".db")
  sjr <- conectar_banco()
  app <- abrir_app_db(arq_app)
  on.exit({ dbDisconnect(app); dbDisconnect(sjr); unlink(paste0(arq_app, c("", "-wal", "-shm"))) })

  chk <- function(nome, cond) {
    if (!isTRUE(cond)) stop("FALHOU: ", nome, call. = FALSE)
    message("  ok  ", nome)
  }
  falha <- function(expr) inherits(try(expr, silent = TRUE), "try-error")

  message("app.db: esquema")
  tabs <- dbListTables(app)
  chk("tabelas usuario, docente, solicitacao, licenca, artigo, avaliacao existem",
      all(c("usuario", "docente", "solicitacao", "licenca", "artigo", "avaliacao") %in% tabs))
  chk("chaves estrangeiras ligadas (PRAGMA foreign_keys = 1)",
      dbGetQuery(app, "PRAGMA foreign_keys")[[1]] == 1)
  idx <- dbGetQuery(app, "SELECT name FROM sqlite_master WHERE type = 'index'")$name
  chk("índices em docente_id e solicitacao_id",
      all(c("idx_solicitacao_docente", "idx_artigo_solic", "idx_licenca_solic", "idx_avaliacao_solic") %in% idx))
  chk("criar o esquema de novo não quebra (IF NOT EXISTS)", !falha(criar_esquema_app(app)))
  chk("banco_edital.db continua somente leitura",
      falha(dbExecute(sjr, "CREATE TABLE teste_escrita (x INTEGER)")))

  message("app.db: CRUD")
  d1 <- criar_docente(app, "Docente Teste Um", "Probabilidade")
  d2 <- criar_docente(app, "Docente Teste Dois", "Bioestatística", lattes_id = "")
  chk("docente criado e lido", obter_docente(app, d1)$nome == "Docente Teste Um")
  chk("lattes_id vazio vira NULL", is.na(obter_docente(app, d2)$lattes_id))
  atualizar_docente(app, d2, "Docente Teste 2", "Inferência")
  chk("docente atualizado", obter_docente(app, d2)$area_atuacao == "Inferência")

  criar_usuario(app, "teste_doc", "senha-de-teste-1", "docente", d1)
  criar_usuario(app, "teste_com", "senha-de-teste-2", "comissao")
  h <- dbGetQuery(app, "SELECT senha_hash FROM usuario WHERE login = 'teste_doc'")$senha_hash
  chk("senha guardada só como hash", !grepl("senha-de-teste-1", h, fixed = TRUE))
  chk("login com senha certa", !is.null(autenticar_usuario(app, "teste_doc", "senha-de-teste-1")))
  chk("login com senha errada falha", is.null(autenticar_usuario(app, "teste_doc", "errada")))
  chk("login inexistente falha", is.null(autenticar_usuario(app, "ninguem", "x")))
  chk("papel fora de docente/comissao é rejeitado",
      falha(dbExecute(app, "INSERT INTO usuario (login, senha_hash, papel) VALUES ('a', 'h', 'admin')")))
  chk("usuário docente sem docente_id é rejeitado", falha(criar_usuario(app, "x2", "12345678", "docente")))
  chk("login repetido é rejeitado", falha(criar_usuario(app, "teste_doc", "12345678", "comissao")))

  s1 <- criar_solicitacao(app, d1, 2025, "2026-03-01")
  chk("solicitação nasce como rascunho", obter_solicitacao(app, s1)$status == "rascunho")
  chk("status inválido é rejeitado pelo CHECK",
      falha(dbExecute(app, "UPDATE solicitacao SET status = 'aprovada' WHERE id = ?", params = list(s1))))
  chk("solicitação de docente inexistente é rejeitada (FK)", falha(criar_solicitacao(app, 9999, 2025)))

  chk("licença médica sem data de término é rejeitada", falha(adicionar_licenca(app, s1, "medica", "2023-01-01")))
  chk("término antes do início é rejeitado", falha(adicionar_licenca(app, s1, "parentalidade", "2023-05-01", "2023-01-01")))
  chk("tipo de licença inválido é rejeitado", falha(adicionar_licenca(app, s1, "ferias", "2023-01-01", "2023-02-01")))
  l_pcd <- adicionar_licenca(app, s1, "pcd", "2010-01-01", NA, "laudo fictício")
  chk("PcD aceita data_fim NULL", is.na(listar_licencas(app, s1)$data_fim[1]))
  excluir_licenca(app, l_pcd)
  chk("licença excluída", nrow(listar_licencas(app, s1)) == 0)

  message("app.db: artigos e duplicatas")
  rev <- dbGetQuery(sjr, "
    SELECT v.titulo, v.p FROM v_docente v
    WHERE v.p >= 0.6 AND v.titulo_norm IN (SELECT titulo_norm FROM revista GROUP BY titulo_norm HAVING COUNT(*) = 1)
    ORDER BY v.p DESC LIMIT 2")
  a1 <- adicionar_artigo(app, s1, "Um Artigo de Teste", 2024, rev$titulo[1], "")
  chk("artigo inserido", isTRUE(a1$ok))
  a1b <- adicionar_artigo(app, s1, "  UM ARTIGO, de teste!! ", 2024, rev$titulo[1], "")
  chk("mesmo título normalizado + ano não entra de novo", !a1b$ok && grepl("Duplicado", a1b$motivo))
  chk("mesmo título com outro ano é outro artigo", adicionar_artigo(app, s1, "Um artigo de teste", 2023, rev$titulo[2], "")$ok)
  chk("UNIQUE (solicitacao, titulo_norm, ano) também no banco",
      falha(dbExecute(app, "INSERT INTO artigo (solicitacao_id, titulo, titulo_norm, ano) VALUES (?, 'x', 'um artigo de teste', 2024)",
                      params = list(s1))))
  chk("ano inválido não entra", !adicionar_artigo(app, s1, "Sem ano", NA, "R", "")$ok)
  r <- adicionar_artigos(app, s1, data.frame(titulo = c("Outro", "outro", "Mais um"), ano = c(2022, 2022, 2025),
                                              revista = "Revista Que Não Existe", issn = c("", "", "1234-567X")))
  chk("importação em lote conta inseridos e duplicados", r$inseridos == 2 && r$duplicados == 1)
  chk("ISSN gravado normalizado (texto, sem hífen)",
      dbGetQuery(app, "SELECT issn FROM artigo WHERE titulo = 'Mais um'")$issn == "1234567X")
  chk("artigo de solicitação inexistente é rejeitado (FK)",
      falha(dbExecute(app, "INSERT INTO artigo (solicitacao_id, titulo, titulo_norm, ano) VALUES (9999, 'x', 'x', 2024)")))

  message("Retrato de avaliação")
  ret1 <- avaliar_solicitacao(app, sjr, s1)
  av <- listar_avaliacoes(app, s1)
  chk("avaliação gravada em `avaliacao`", nrow(av) == 1 && av$id[1] == ret1$avaliacao_id)
  chk("retrato tem janela padrão 2022-2025", ret1$periodo$n_anos == 4 && ret1$periodo$ano_ini == 2022)
  chk("soma do retrato = soma dos pontos por artigo", abs(sum(ret1$detalhe$pontos) - ret1$resumo$soma) < 1e-6)
  chk("colunas soma/meta/aprovado iguais ao detalhe_json",
      abs(av$soma[1] - ret1$resumo$soma) < 1e-9 && av$meta[1] == 1.25 && av$aprovado[1] == as.integer(ret1$resumo$aprovado))
  chk("retrato traz o detalhe de cada artigo com motivo e explicação",
      nrow(ret1$detalhe) == 4 && all(nzchar(ret1$detalhe$motivo)) && all(nzchar(ret1$detalhe$explicacao)))
  chk("artigo de 2023 'Um artigo de teste' não foi tratado como duplicata do de 2024",
      sum(normalizar_texto(ret1$detalhe$titulo) == "um artigo de teste") == 2)
  chk("revista inexistente aparece como sem_match", any(ret1$detalhe$metodo == "sem_match"))
  chk("retrato guarda a versão do banco_edital", grepl("^SJR 2020-2025", ret1$versao_banco))
  chk("retrato guarda notas por área das revistas casadas", length(ret1$areas) >= 1)
  chk("retrato não pode ser alterado (trigger)",
      falha(dbExecute(app, "UPDATE avaliacao SET soma = 4 WHERE id = ?", params = list(ret1$avaliacao_id))))

  soma_antes <- ret1$resumo$soma
  adicionar_artigo(app, s1, "Artigo novo depois do retrato", 2025, rev$titulo[2], "")
  dbExecute(app, "DELETE FROM artigo WHERE titulo = 'Um Artigo de Teste'")
  velho <- obter_retrato(app, ret1$avaliacao_id)
  chk("mudar os artigos NÃO altera o retrato antigo",
      velho$resumo$soma == soma_antes && nrow(velho$detalhe) == 4)
  ret2 <- avaliar_solicitacao(app, sjr, s1)
  chk("novo cálculo gera um novo retrato (histórico com 2)", nrow(listar_avaliacoes(app, s1)) == 2)
  chk("último retrato é o novo", ultimo_retrato(app, s1)$avaliacao_id == ret2$avaliacao_id)
  chk("retrato é lido sem consultar o banco_edital (JSON autossuficiente)", {
    j <- dbGetQuery(app, "SELECT detalhe_json FROM avaliacao WHERE id = ?", params = list(ret1$avaliacao_id))[[1]]
    x <- jsonlite::fromJSON(j); x$resumo$soma == soma_antes && x$periodo$ano_fim == 2025
  })

  message("Retrato com Artigo Y (licenças do app.db)")
  s2 <- criar_solicitacao(app, d2, 2025)
  adicionar_artigo(app, s2, "Artigo de 2020", 2020, rev$titulo[1], "")
  adicionar_artigo(app, s2, "Artigo de 2024", 2024, rev$titulo[1], "")
  r0 <- avaliar_solicitacao(app, sjr, s2)
  chk("sem licença o artigo de 2020 fica fora do período",
      r0$detalhe$situacao[r0$detalhe$ano == 2020] == "fora_do_periodo")
  adicionar_licenca(app, s2, "medica", "2023-02-01", "2023-09-30", "atestado fictício")
  r5 <- avaliar_solicitacao(app, sjr, s2)
  chk("licença médica de 8 meses (meses_entre_datas) = janela de 5 anos", r5$periodo$n_anos == 5)
  adicionar_licenca(app, s2, "parentalidade", "2021-01-01", "2021-06-30", "fictício")
  r6 <- avaliar_solicitacao(app, sjr, s2)
  chk("+ parentalidade: pedido 7 anos, usa 6, com aviso",
      r6$periodo$n_anos == 6 && r6$periodo$n_anos_pedido == 7 && isTRUE(r6$periodo$limitado) && length(r6$periodo$avisos) >= 1)
  chk("com 6 anos o artigo de 2020 entra", r6$detalhe$no_periodo[r6$detalhe$ano == 2020] == 1)
  chk("retrato guarda as licenças usadas", nrow(r6$licencas) == 2)

  message("Revista ambígua e confirmação manual")
  amb <- dbGetQuery(sjr, "SELECT issn, MIN(sourceid) AS menor, MAX(sourceid) AS maior FROM issn
                          GROUP BY issn HAVING COUNT(DISTINCT sourceid) > 1 LIMIT 1")
  if (nrow(amb)) {
    s3 <- criar_solicitacao(app, d1, 2025)
    a <- adicionar_artigo(app, s3, "Artigo ambíguo", 2024, "", amb$issn)
    ra <- avaliar_solicitacao(app, sjr, s3)
    chk("ISSN ambíguo: usa o menor sourceid e marca n_candidatos > 1",
        ra$detalhe$sourceid == amb$menor && ra$detalhe$n_candidatos > 1)
    chk("candidatos_revista lista as revistas do ISSN",
        nrow(candidatos_revista(sjr, amb$issn, "", 4)) >= 2)
    confirmar_revista(app, a$id, amb$maior)
    chk("confirmação grava sourceid e confirmado_manual",
        with(listar_artigos(app, s3), sourceid == amb$maior && confirmado_manual == 1))
    rc <- avaliar_solicitacao(app, sjr, s3)
    chk("após confirmar, o cálculo usa a revista escolhida (metodo = manual)",
        rc$detalhe$sourceid == amb$maior && rc$detalhe$metodo == "manual" && rc$detalhe$n_candidatos == 1)
  } else message("  (banco sem ISSN ambíguo: testes de ambiguidade pulados)")

  message("Fluxo de status")
  mudar_status(app, s1, "enviada")
  chk("enviada: não aceita novos artigos", falha(adicionar_artigo(app, s1, "Tarde demais", 2025, "R", "")))
  chk("enviada: não aceita licenças", falha(adicionar_licenca(app, s1, "pcd", "2020-01-01")))
  chk("enviada: comissão ainda pode registrar retrato oficial", isTRUE(avaliar_solicitacao(app, sjr, s1, oficial = TRUE)$oficial))
  chk("transição inválida (enviada -> enviada) é recusada", falha(mudar_status(app, s1, "enviada")))
  mudar_status(app, s1, "avaliada")
  chk("listar_solicitacoes traz último retrato e situação",
      with(listar_solicitacoes(app)[listar_solicitacoes(app)$id == s1, ], oficial == 1 && situacao %in% c("apto", "quase apto", "não apto")))
  chk("situacao_soma: 1,25 apto; 1,10 quase apto; 0,99 não apto; NA sem avaliação",
      identical(situacao_soma(c(1.25, 1.10, 0.99, NA)), c("apto", "quase apto", "não apto", "sem avaliação")))
  s4 <- criar_solicitacao(app, d2, 2025)
  r_vazio <- avaliar_solicitacao(app, sjr, s4)
  chk("solicitação sem artigos não quebra (soma 0)", r_vazio$resumo$soma == 0 && !r_vazio$resumo$aprovado)
  excluir_solicitacao(app, s4)
  chk("excluir rascunho apaga em cascata o retrato",
      nrow(dbGetQuery(app, "SELECT * FROM avaliacao WHERE solicitacao_id = ?", params = list(s4))) == 0)
  excluir_docente(app, d2)
  chk("excluir docente apaga solicitações, artigos e licenças em cascata",
      nrow(dbGetQuery(app, "SELECT * FROM solicitacao WHERE docente_id = ?", params = list(d2))) == 0 &&
      nrow(dbGetQuery(app, "SELECT * FROM licenca WHERE solicitacao_id = ?", params = list(s2))) == 0)

  message("Importadores")
  li <- ler_lista_colada("Título; Ano; Revista; ISSN\nA; 2024; R; 0266-4763\nB\t2023\tS\t")
  chk("lista colada (; e TAB, cabeçalho ignorado)", nrow(li) == 2 && li$issn[1] == "0266-4763")
  csv <- tempfile(fileext = ".csv")
  writeLines(c("titulo;ano;revista;issn", "Artigo CSV;2024;Revista X;00987654"), csv, useBytes = TRUE)
  cc <- ler_csv_artigos(csv)
  chk("CSV com ; e ISSN com zero à esquerda (texto)", nrow(cc) == 1 && cc$issn == "00987654")
  xml <- tempfile(fileext = ".xml")
  writeLines(c('<?xml version="1.0" encoding="UTF-8"?>', '<CURRICULO-VITAE><PRODUCAO-BIBLIOGRAFICA><ARTIGOS-PUBLICADOS>',
    '<ARTIGO-PUBLICADO><DADOS-BASICOS-DO-ARTIGO TITULO-DO-ARTIGO="Artigo Lattes" ANO-DO-ARTIGO="2023"/>',
    '<DETALHAMENTO-DO-ARTIGO TITULO-DO-PERIODICO-OU-REVISTA="Revista L" ISSN="12345678"/></ARTIGO-PUBLICADO>',
    '</ARTIGOS-PUBLICADOS></PRODUCAO-BIBLIOGRAFICA></CURRICULO-VITAE>'), xml)
  lx <- ler_lattes_xml(xml)
  chk("XML do Lattes vira o contrato titulo/ano/revista/issn",
      identical(names(lx), c("titulo", "ano", "revista", "issn")) && lx$titulo == "Artigo Lattes" && lx$ano == "2023")

  message("Currículo Lattes no formato real (.zip, ISO-8859-1)")
  rv_l <- dbGetQuery(sjr, "SELECT v.titulo, MIN(i.issn) AS issn, v.p FROM v_docente v JOIN issn i ON i.sourceid = v.sourceid
                           WHERE v.p >= 0.5 AND v.titulo_norm IN (SELECT titulo_norm FROM revista GROUP BY titulo_norm HAVING COUNT(*) = 1)
                             AND i.issn IN (SELECT issn FROM issn GROUP BY issn HAVING COUNT(DISTINCT sourceid) = 1)
                           GROUP BY v.sourceid ORDER BY v.p DESC LIMIT 2")
  art_xml <- function(seq, tit, ano, rev, issn) sprintf(paste0(
    '<ARTIGO-PUBLICADO SEQUENCIA-PRODUCAO="%d" ORDEM-IMPORTANCIA="">',
    '<DADOS-BASICOS-DO-ARTIGO NATUREZA="COMPLETO" TITULO-DO-ARTIGO="%s" ANO-DO-ARTIGO="%s" PAIS-DE-PUBLICACAO="Brasil" ',
    'IDIOMA="Português" MEIO-DE-DIVULGACAO="DIGITAL" HOME-PAGE-DO-TRABALHO="" FLAG-RELEVANCIA="NAO" DOI="10.0000/fict.%d" ',
    'TITULO-DO-ARTIGO-INGLES="" FLAG-DIVULGACAO-CIENTIFICA="NAO"/>',
    '<DETALHAMENTO-DO-ARTIGO TITULO-DO-PERIODICO-OU-REVISTA="%s" ISSN="%s" VOLUME="12" FASCICULO="3" SERIE="" ',
    'PAGINA-INICIAL="1" PAGINA-FINAL="20" LOCAL-DE-PUBLICACAO=""/>',
    '<AUTORES NOME-COMPLETO-DO-AUTOR="Pessoa Fictícia" NOME-PARA-CITACAO="FICTICIA, P." ORDEM-DE-AUTORIA="1" NRO-ID-CNPQ=""/>',
    '<PALAVRAS-CHAVE PALAVRA-CHAVE-1="estatística"/></ARTIGO-PUBLICADO>'), seq, tit, ano, seq, rev, issn)
  xml_txt <- paste0(
    '<?xml version="1.0" encoding="ISO-8859-1" standalone="no" ?>\n',
    '<CURRICULO-VITAE SISTEMA-ORIGEM-XML="LATTES_OFFLINE" NUMERO-IDENTIFICADOR="0000000000000000" ',
    'DATA-ATUALIZACAO="10112026" HORA-ATUALIZACAO="101500">',
    '<DADOS-GERAIS NOME-COMPLETO="Pessoa Fictícia de Teste" NOME-EM-CITACOES-BIBLIOGRAFICAS="FICTICIA, P." ',
    'NACIONALIDADE="B" PAIS-DE-NASCIMENTO="Brasil"/>',
    '<PRODUCAO-BIBLIOGRAFICA><ARTIGOS-PUBLICADOS>',
    art_xml(1, "Estimação em séries com ruído &amp; tendência", 2024, rv_l$titulo[1], ""),
    art_xml(2, "Um artigo  com espaços\n duplos", 2021, "nome diferente da revista", rv_l$issn[2]),
    art_xml(3, "Artigo antigo", 2012, rv_l$titulo[1], ""),
    art_xml(4, "Artigo em revista fora do SJR", 2023, "Boletim Inexistente", "99999999"),
    '</ARTIGOS-PUBLICADOS><ARTIGOS-ACEITOS-PARA-PUBLICACAO><ARTIGO-ACEITO-PARA-PUBLICACAO SEQUENCIA-PRODUCAO="9">',
    '<DADOS-BASICOS-DO-ARTIGO TITULO-DO-ARTIGO="Aceito, ainda não publicado" ANO-DO-ARTIGO="2026"/>',
    '<DETALHAMENTO-DO-ARTIGO TITULO-DO-PERIODICO-OU-REVISTA="X" ISSN="12345678"/>',
    '</ARTIGO-ACEITO-PARA-PUBLICACAO></ARTIGOS-ACEITOS-PARA-PUBLICACAO></PRODUCAO-BIBLIOGRAFICA></CURRICULO-VITAE>')
  dir_l <- tempfile("lattes_teste_"); dir.create(dir_l)
  xml_l <- file.path(dir_l, "curriculo.xml")
  writeBin(iconv(xml_txt, "UTF-8", "latin1", toRaw = TRUE)[[1]], xml_l)
  lx2 <- ler_lattes_xml(xml_l)
  chk("Lattes ISO-8859-1: lê só ARTIGO-PUBLICADO (aceitos ficam de fora)", nrow(lx2) == 4)
  chk("Lattes: acentos e & preservados, espaços normalizados",
      lx2$titulo[1] == "Estimação em séries com ruído & tendência" && lx2$titulo[2] == "Um artigo com espaços duplos")
  chk("Lattes: nome e ID do currículo lidos", attr(lx2, "docente")$nome == "Pessoa Fictícia de Teste" &&
      attr(lx2, "docente")$lattes_id == "0000000000000000")
  zip_l <- file.path(dir_l, "curriculo.zip")
  old_wd <- setwd(dir_l)
  zip_ok <- tryCatch(utils::zip("curriculo.zip", "curriculo.xml", flags = "-q") == 0,
                     error = function(e) FALSE, warning = function(w) FALSE)
  setwd(old_wd)
  if (isTRUE(zip_ok) && file.exists(zip_l)) {
    chk("Lattes: aceita o .zip baixado da plataforma", identical(ler_lattes_xml(zip_l)$titulo, lx2$titulo))
  } else message("  (sem utilitário zip nesta máquina: teste do .zip pulado)")
  chk("arquivo que não é Lattes dá erro claro", {
    f <- tempfile(fileext = ".xml"); writeLines("<outra/>", f)
    grepl("currículo Lattes", tryCatch(ler_lattes_xml(f), error = conditionMessage))
  })
  pv <- previsualizar_artigos(sjr, lx2, n_anos = 6)
  chk("prévia: casa por título e por ISSN, e marca revista fora do SJR",
      pv$sjr[1] == "sim (título)" && pv$sjr[2] == "sim (ISSN)" && pv$sjr[4] == "não encontrada" && !is.na(pv$p[1]))
  s5 <- criar_solicitacao(app, d1, 2025)
  ri <- adicionar_artigos(app, s5, lx2)
  r5 <- avaliar_solicitacao(app, sjr, s5)
  chk("Lattes importado e avaliado: 2012 fora do período, revista inexistente sem_match",
      ri$inseridos == 4 && r5$detalhe$situacao[r5$detalhe$ano == 2012] == "fora_do_periodo" &&
      r5$detalhe$metodo[r5$detalhe$ano == 2023] == "sem_match")
  unlink(dir_l, recursive = TRUE)

  message("Semente da demonstração")
  app2 <- abrir_app_db(":memory:")
  cred <- tempfile(fileext = ".csv")
  options(ppge.semear.nao_rodar = TRUE)
  source("R/semear_demo.R", encoding = "UTF-8", local = TRUE)
  options(ppge.semear.nao_rodar = NULL)
  sd <- semear_demo(app2, sjr, credenciais = cred)
  ls <- listar_solicitacoes(app2)
  chk("semente: 15 docentes fictícios e 16 usuários",
      nrow(listar_docentes(app2)) == 15 && nrow(listar_usuarios(app2)) == 16)
  chk("semente cobre apto, quase apto e não apto", all(c("apto", "quase apto", "não apto") %in% ls$situacao))
  chk("semente cobre os 3 status", all(c("rascunho", "enviada", "avaliada") %in% ls$status))
  chk("semente cobre parentalidade, médica e PcD",
      all(c("parentalidade", "medica", "pcd") %in% dbGetQuery(app2, "SELECT DISTINCT tipo FROM licenca")$tipo))
  chk("semente: duplicata foi rejeitada", isTRUE(sd$duplicata_rejeitada))
  chk("semente: credenciais gravadas fora do banco e conferem com o hash", {
    cr <- utils::read.csv(cred, stringsAsFactors = FALSE)
    nrow(cr) == 16 && !is.null(autenticar_usuario(app2, cr$login[1], cr$senha[1]))
  })
  dbDisconnect(app2)
})
