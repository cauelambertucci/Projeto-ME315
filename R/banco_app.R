# =====================================================================
# banco_app.R  -  Banco de CREDENCIAMENTO (app.db): esquema + CRUD + retrato
#
# app.db e GRAVAVEL e separado do banco_edital.db (que e sempre somente
# leitura). Nada aqui grava no banco_edital.db.
#
#   source("R/funcoes_banco.R"); source("R/periodo_artigo_y.R"); source("R/banco_app.R")
#   app <- abrir_app_db("app.db")          # cria o esquema se nao existir
#   sjr <- conectar_banco()                # banco_edital.db, somente leitura
#   d   <- criar_docente(app, "Docente Ficticio", "Estatistica")
#   s   <- criar_solicitacao(app, d, ano_fim_base = 2025)
#   adicionar_artigo(app, s, "Titulo", 2024, "Revista X", "1234-5678")
#   r   <- avaliar_solicitacao(app, sjr, s)  # calcula e grava o RETRATO
#
# Pacotes: DBI, RSQLite, jsonlite, scrypt (hash de senha)
# =====================================================================

# Ao confirmar manualmente a revista de um artigo ambiguo, o sourceid escolhido
# pelo docente passa a valer no calculo (antes do ISSN e do titulo).
# Decisao sinalizada na tela "Regras e premissas"; para desligar, FALSE.
USAR_CONFIRMACAO_MANUAL <- TRUE

# Faixa "quase apto" usada nas telas da comissao (soma entre 1,00 e 1,25).
LIMIAR_QUASE_APTO <- 1.00

STATUS_SOLICITACAO <- c("rascunho", "enviada", "avaliada")
TIPOS_LICENCA      <- c("parentalidade", "medica", "pcd")

agora_txt <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# ---------------------------------------------------------------------
# Conexao e esquema
# ---------------------------------------------------------------------

SQL_ESQUEMA_APP <- c(
"CREATE TABLE IF NOT EXISTS docente (
    id           INTEGER PRIMARY KEY,
    nome         TEXT NOT NULL CHECK (length(trim(nome)) > 0),
    lattes_id    TEXT,
    area_atuacao TEXT NOT NULL,
    criado_em    TEXT NOT NULL
)",
"CREATE TABLE IF NOT EXISTS usuario (
    id         INTEGER PRIMARY KEY,
    login      TEXT NOT NULL UNIQUE,
    senha_hash TEXT NOT NULL,
    papel      TEXT NOT NULL CHECK (papel IN ('docente', 'comissao')),
    docente_id INTEGER REFERENCES docente (id) ON DELETE CASCADE,
    CHECK (papel = 'comissao' OR docente_id IS NOT NULL)
)",
"CREATE TABLE IF NOT EXISTS solicitacao (
    id           INTEGER PRIMARY KEY,
    docente_id   INTEGER NOT NULL REFERENCES docente (id) ON DELETE CASCADE,
    data_pedido  TEXT NOT NULL,
    status       TEXT NOT NULL DEFAULT 'rascunho'
                 CHECK (status IN ('rascunho', 'enviada', 'avaliada')),
    ano_fim_base INTEGER NOT NULL
)",
"CREATE TABLE IF NOT EXISTS licenca (
    id             INTEGER PRIMARY KEY,
    solicitacao_id INTEGER NOT NULL REFERENCES solicitacao (id) ON DELETE CASCADE,
    tipo           TEXT NOT NULL CHECK (tipo IN ('parentalidade', 'medica', 'pcd')),
    data_inicio    TEXT NOT NULL,
    data_fim       TEXT,
    observacao     TEXT,
    CHECK (tipo = 'pcd' OR data_fim IS NOT NULL),
    CHECK (data_fim IS NULL OR data_fim >= data_inicio)
)",
# titulo_norm = normalizar_texto(titulo): a UNIQUE abaixo reflete a regra de
# duplicata de preparar_artigos() (mesmo titulo normalizado + ano conta uma vez).
"CREATE TABLE IF NOT EXISTS artigo (
    id                INTEGER PRIMARY KEY,
    solicitacao_id    INTEGER NOT NULL REFERENCES solicitacao (id) ON DELETE CASCADE,
    titulo            TEXT NOT NULL,
    titulo_norm       TEXT NOT NULL CHECK (length(titulo_norm) > 0),
    ano               INTEGER NOT NULL,
    revista_digitada  TEXT,
    issn              TEXT,
    sourceid          INTEGER,
    confirmado_manual INTEGER NOT NULL DEFAULT 0 CHECK (confirmado_manual IN (0, 1)),
    UNIQUE (solicitacao_id, titulo_norm, ano)
)",
# RETRATO: uma linha por calculo, nunca alterada (trigger abaixo). O SJR muda a
# cada ano; o historico guarda o resultado como foi visto naquele dia.
"CREATE TABLE IF NOT EXISTS avaliacao (
    id             INTEGER PRIMARY KEY,
    solicitacao_id INTEGER NOT NULL REFERENCES solicitacao (id) ON DELETE CASCADE,
    calculada_em   TEXT NOT NULL,
    n_anos         INTEGER NOT NULL,
    ano_ini        INTEGER NOT NULL,
    ano_fim        INTEGER NOT NULL,
    soma           REAL NOT NULL,
    meta           REAL NOT NULL,
    aprovado       INTEGER NOT NULL CHECK (aprovado IN (0, 1)),
    versao_banco   TEXT NOT NULL,
    detalhe_json   TEXT NOT NULL,
    oficial        INTEGER NOT NULL DEFAULT 0 CHECK (oficial IN (0, 1))
)",
"CREATE TRIGGER IF NOT EXISTS avaliacao_imutavel
 BEFORE UPDATE ON avaliacao
 BEGIN SELECT RAISE(ABORT, 'avaliacao e um retrato e nao pode ser alterada'); END",
"CREATE INDEX IF NOT EXISTS idx_usuario_docente     ON usuario (docente_id)",
"CREATE INDEX IF NOT EXISTS idx_solicitacao_docente ON solicitacao (docente_id)",
"CREATE INDEX IF NOT EXISTS idx_licenca_solic       ON licenca (solicitacao_id)",
"CREATE INDEX IF NOT EXISTS idx_artigo_solic        ON artigo (solicitacao_id)",
"CREATE INDEX IF NOT EXISTS idx_avaliacao_solic     ON avaliacao (solicitacao_id, calculada_em)"
)

caminho_app_db <- function() Sys.getenv("PPGE_APP_DB", "app.db")

# Abre (e cria, se preciso) o app.db. Uma conexao por sessao do Shiny.
abrir_app_db <- function(caminho = caminho_app_db()) {
  if (identical(normalizePath(caminho, mustWork = FALSE),
                normalizePath("banco_edital.db", mustWork = FALSE))) {
    stop("app.db nao pode ser o banco_edital.db.")
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), caminho, bigint = "numeric")
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
  if (caminho != ":memory:") DBI::dbGetQuery(con, "PRAGMA journal_mode = WAL")
  criar_esquema_app(con)
  con
}

criar_esquema_app <- function(con) {
  DBI::dbWithTransaction(con, for (s in SQL_ESQUEMA_APP) DBI::dbExecute(con, s))
  invisible(TRUE)
}

# helpers ---------------------------------------------------------------
na_se_vazio <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- as.character(x)
  x[!is.na(x) & !nzchar(trimws(x))] <- NA_character_
  x
}
inserir <- function(con, sql, params) {
  DBI::dbExecute(con, sql, params = params)
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
}
garantir_rascunho <- function(con, solicitacao_id) {
  st <- DBI::dbGetQuery(con, "SELECT status FROM solicitacao WHERE id = ?",
                        params = list(solicitacao_id))$status
  if (!length(st)) stop("Solicitação ", solicitacao_id, " não existe.")
  if (st != "rascunho") stop("A solicitação já foi enviada e não pode mais ser alterada.")
  invisible(TRUE)
}

# ---------------------------------------------------------------------
# DOCENTE
# ---------------------------------------------------------------------
criar_docente <- function(con, nome, area_atuacao, lattes_id = NA) {
  inserir(con, "INSERT INTO docente (nome, lattes_id, area_atuacao, criado_em) VALUES (?, ?, ?, ?)",
          list(nome, na_se_vazio(lattes_id), area_atuacao, agora_txt()))
}
obter_docente <- function(con, id) {
  DBI::dbGetQuery(con, "SELECT * FROM docente WHERE id = ?", params = list(id))
}
listar_docentes <- function(con) DBI::dbGetQuery(con, "SELECT * FROM docente ORDER BY nome")
atualizar_docente <- function(con, id, nome, area_atuacao, lattes_id = NA) {
  DBI::dbExecute(con, "UPDATE docente SET nome = ?, area_atuacao = ?, lattes_id = ? WHERE id = ?",
                 params = list(nome, area_atuacao, na_se_vazio(lattes_id), id))
}
excluir_docente <- function(con, id) {
  DBI::dbExecute(con, "DELETE FROM docente WHERE id = ?", params = list(id))
}

# ---------------------------------------------------------------------
# USUARIO (senha guardada so como hash scrypt)
# ---------------------------------------------------------------------
criar_usuario <- function(con, login, senha, papel, docente_id = NA) {
  if (nchar(senha) < 8) stop("A senha precisa de pelo menos 8 caracteres.")
  inserir(con, "INSERT INTO usuario (login, senha_hash, papel, docente_id) VALUES (?, ?, ?, ?)",
          list(login, scrypt::hashPassword(senha), papel,
               if (is.na(docente_id)) NA_integer_ else docente_id))
}
# Devolve a linha do usuario (sem o hash) se login e senha conferem; senao NULL.
autenticar_usuario <- function(con, login, senha) {
  u <- DBI::dbGetQuery(con, "SELECT * FROM usuario WHERE login = ?", params = list(login))
  if (nrow(u) != 1 || !isTRUE(scrypt::verifyPassword(u$senha_hash, senha))) return(NULL)
  u$senha_hash <- NULL
  u
}
listar_usuarios <- function(con) {
  DBI::dbGetQuery(con, "SELECT u.id, u.login, u.papel, u.docente_id, d.nome
                        FROM usuario u LEFT JOIN docente d ON d.id = u.docente_id ORDER BY u.login")
}

# ---------------------------------------------------------------------
# SOLICITACAO
# ---------------------------------------------------------------------
criar_solicitacao <- function(con, docente_id, ano_fim_base, data_pedido = Sys.Date()) {
  inserir(con, "INSERT INTO solicitacao (docente_id, data_pedido, status, ano_fim_base)
                VALUES (?, ?, 'rascunho', ?)",
          list(docente_id, format(as.Date(data_pedido)), as.integer(ano_fim_base)))
}
obter_solicitacao <- function(con, id) {
  DBI::dbGetQuery(con,
    "SELECT s.*, d.nome AS docente, d.area_atuacao, d.lattes_id
     FROM solicitacao s JOIN docente d ON d.id = s.docente_id WHERE s.id = ?",
    params = list(id))
}

# Lista solicitacoes (de um docente ou de todos) com o ULTIMO retrato de cada uma.
listar_solicitacoes <- function(con, docente_id = NULL) {
  sql <- "
    WITH ult AS (
      SELECT a.*, ROW_NUMBER() OVER (PARTITION BY solicitacao_id
                                     ORDER BY calculada_em DESC, id DESC) AS rn
      FROM avaliacao a)
    SELECT s.id, s.docente_id, d.nome AS docente, d.area_atuacao, s.data_pedido, s.status,
           s.ano_fim_base,
           (SELECT COUNT(*) FROM artigo  ar WHERE ar.solicitacao_id = s.id) AS n_artigos,
           (SELECT COUNT(*) FROM licenca li WHERE li.solicitacao_id = s.id) AS n_licencas,
           (SELECT COUNT(*) FROM avaliacao av WHERE av.solicitacao_id = s.id) AS n_avaliacoes,
           u.id AS avaliacao_id, u.calculada_em, u.n_anos, u.ano_ini, u.ano_fim,
           u.soma, u.meta, u.aprovado, u.oficial
    FROM solicitacao s
    JOIN docente d ON d.id = s.docente_id
    LEFT JOIN ult u ON u.solicitacao_id = s.id AND u.rn = 1"
  if (is.null(docente_id)) {
    out <- DBI::dbGetQuery(con, paste(sql, "ORDER BY s.data_pedido DESC, s.id DESC"))
  } else {
    out <- DBI::dbGetQuery(con, paste(sql, "WHERE s.docente_id = ? ORDER BY s.data_pedido DESC, s.id DESC"),
                           params = list(docente_id))
  }
  out$situacao <- situacao_soma(out$soma, out$meta)
  out
}

# rascunho -> enviada (docente) ; enviada -> avaliada (comissao)
# avaliada -> enviada (comissao reabre)  ; enviada -> rascunho (devolver ao docente)
TRANSICOES_STATUS <- list(rascunho = "enviada", enviada = c("avaliada", "rascunho"),
                          avaliada = "enviada")
mudar_status <- function(con, id, novo) {
  novo <- match.arg(novo, STATUS_SOLICITACAO)
  atual <- obter_solicitacao(con, id)$status
  if (!length(atual)) stop("Solicitação ", id, " não existe.")
  if (!novo %in% TRANSICOES_STATUS[[atual]]) {
    stop(sprintf("Não é possível passar de '%s' para '%s'.", atual, novo))
  }
  DBI::dbExecute(con, "UPDATE solicitacao SET status = ? WHERE id = ?", params = list(novo, id))
}
excluir_solicitacao <- function(con, id) {
  garantir_rascunho(con, id)
  DBI::dbExecute(con, "DELETE FROM solicitacao WHERE id = ?", params = list(id))
}

# ---------------------------------------------------------------------
# LICENCA (Artigo Y). Comprovacao = texto livre em `observacao` (sem upload).
# ---------------------------------------------------------------------
validar_licenca <- function(tipo, data_inicio, data_fim) {
  erros <- character(0)
  if (!tipo %in% TIPOS_LICENCA) erros <- c(erros, "Tipo de licença inválido.")
  ini <- suppressWarnings(as.Date(na_se_vazio(data_inicio), optional = TRUE))
  fim <- suppressWarnings(as.Date(na_se_vazio(data_fim), optional = TRUE))
  if (is.na(ini)) erros <- c(erros, "Informe a data de início.")
  if (tipo != "pcd" && is.na(fim)) erros <- c(erros, "Informe a data de término.")
  if (!is.na(ini) && !is.na(fim) && fim < ini) erros <- c(erros, "O término é anterior ao início.")
  erros
}
adicionar_licenca <- function(con, solicitacao_id, tipo, data_inicio, data_fim = NA, observacao = NA) {
  garantir_rascunho(con, solicitacao_id)
  erros <- validar_licenca(tipo, data_inicio, data_fim)
  if (length(erros)) stop(paste(erros, collapse = " "))
  inserir(con, "INSERT INTO licenca (solicitacao_id, tipo, data_inicio, data_fim, observacao)
                VALUES (?, ?, ?, ?, ?)",
          list(solicitacao_id, tipo, format(as.Date(data_inicio)),
               if (is.na(na_se_vazio(data_fim))) NA_character_ else format(as.Date(data_fim)),
               na_se_vazio(observacao)))
}
listar_licencas <- function(con, solicitacao_id) {
  DBI::dbGetQuery(con, "SELECT * FROM licenca WHERE solicitacao_id = ? ORDER BY data_inicio, id",
                  params = list(solicitacao_id))
}
excluir_licenca <- function(con, id) {
  sid <- DBI::dbGetQuery(con, "SELECT solicitacao_id FROM licenca WHERE id = ?", params = list(id))$solicitacao_id
  if (length(sid)) garantir_rascunho(con, sid)
  DBI::dbExecute(con, "DELETE FROM licenca WHERE id = ?", params = list(id))
}

# Converte as licencas cadastradas nos 3 argumentos de calcular_periodo().
# Licenca medica: meses via meses_entre_datas() (uma entrada por licenca; somam).
resumo_licencas <- function(lic) {
  med <- lic[lic$tipo == "medica", , drop = FALSE]
  list(
    n_parentalidade      = sum(lic$tipo == "parentalidade"),
    pcd                  = any(lic$tipo == "pcd"),
    meses_licenca_medica = if (nrow(med)) meses_entre_datas(med$data_inicio, med$data_fim) else 0
  )
}

# ---------------------------------------------------------------------
# ARTIGO
# ---------------------------------------------------------------------
# Insere um artigo. Devolve list(ok, id, motivo). Duplicata (mesmo titulo
# normalizado + ano na mesma solicitacao) NAO e inserida: motivo = "duplicado".
adicionar_artigo <- function(con, solicitacao_id, titulo, ano, revista = NA, issn = NA) {
  garantir_rascunho(con, solicitacao_id)
  titulo <- trimws(as.character(titulo))
  tn  <- normalizar_texto(titulo)
  ano <- suppressWarnings(as.integer(ano))
  if (!nzchar(tn))            return(list(ok = FALSE, id = NA, motivo = "Título vazio."))
  if (is.na(ano) || ano < 1900 || ano > 2100)
                              return(list(ok = FALSE, id = NA, motivo = "Ano inválido."))
  dup <- DBI::dbGetQuery(con,
    "SELECT id, titulo FROM artigo WHERE solicitacao_id = ? AND titulo_norm = ? AND ano = ?",
    params = list(solicitacao_id, tn, ano))
  if (nrow(dup)) {
    return(list(ok = FALSE, id = dup$id[1],
                motivo = sprintf("Duplicado: já existe \"%s\" (%d) nesta solicitação.", dup$titulo[1], ano)))
  }
  issn_n <- normalizar_issn(na_se_vazio(issn))
  id <- inserir(con,
    "INSERT INTO artigo (solicitacao_id, titulo, titulo_norm, ano, revista_digitada, issn)
     VALUES (?, ?, ?, ?, ?, ?)",
    list(solicitacao_id, titulo, tn, ano, na_se_vazio(revista),
         if (nzchar(issn_n)) issn_n else na_se_vazio(issn)))
  list(ok = TRUE, id = id, motivo = NA_character_)
}

# Insere varios (data.frame com titulo, ano, revista, issn). Devolve contagens
# e a lista do que nao entrou, para mostrar na tela.
adicionar_artigos <- function(con, solicitacao_id, df) {
  falta <- setdiff(c("titulo", "ano"), names(df))
  if (length(falta)) stop("Faltam colunas: ", paste(falta, collapse = ", "))
  if (!"revista" %in% names(df)) df$revista <- NA
  if (!"issn" %in% names(df))    df$issn <- NA
  res <- lapply(seq_len(nrow(df)), function(i)
    adicionar_artigo(con, solicitacao_id, df$titulo[i], df$ano[i], df$revista[i], df$issn[i]))
  ok <- vapply(res, `[[`, logical(1), "ok")
  mot <- vapply(res, function(r) as.character(r$motivo), "")
  list(inseridos = sum(ok),
       duplicados = sum(!ok & startsWith(mot, "Duplicado")),
       invalidos = sum(!ok & !startsWith(mot, "Duplicado")),
       rejeitados = data.frame(linha = which(!ok), titulo = as.character(df$titulo[!ok]),
                               motivo = mot[!ok], stringsAsFactors = FALSE))
}
listar_artigos <- function(con, solicitacao_id) {
  DBI::dbGetQuery(con, "SELECT * FROM artigo WHERE solicitacao_id = ? ORDER BY ano DESC, id",
                  params = list(solicitacao_id))
}
excluir_artigo <- function(con, id) {
  sid <- DBI::dbGetQuery(con, "SELECT solicitacao_id FROM artigo WHERE id = ?", params = list(id))$solicitacao_id
  if (length(sid)) garantir_rascunho(con, sid)
  DBI::dbExecute(con, "DELETE FROM artigo WHERE id = ?", params = list(id))
}
# O docente escolhe qual revista do SJR e a certa (ISSN/titulo ambiguo).
confirmar_revista <- function(con, artigo_id, sourceid) {
  sid <- DBI::dbGetQuery(con, "SELECT solicitacao_id FROM artigo WHERE id = ?", params = list(artigo_id))$solicitacao_id
  if (!length(sid)) stop("Artigo não existe.")
  garantir_rascunho(con, sid)
  DBI::dbExecute(con, "UPDATE artigo SET sourceid = ?, confirmado_manual = 1 WHERE id = ?",
                 params = list(as.numeric(sourceid), artigo_id))
}
desfazer_confirmacao <- function(con, artigo_id) {
  DBI::dbExecute(con, "UPDATE artigo SET sourceid = NULL, confirmado_manual = 0 WHERE id = ?",
                 params = list(artigo_id))
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

# Calcula a avaliacao de uma solicitacao e GRAVA o retrato em `avaliacao`.
#  1. le artigos e licencas do app.db
#  2. janela com calcular_periodo() (meses de licenca medica via meses_entre_datas)
#  3. avaliar_artigos() (via avaliar_artigos_app, que respeita confirmacao manual)
#  4. enriquece cada artigo (ano do maximo, titulo no SJR, notas por area, motivo)
#  5. grava tudo em detalhe_json; devolve o retrato ja lido
avaliar_solicitacao <- function(con_app, con_sjr, solicitacao_id, oficial = FALSE) {
  sol <- obter_solicitacao(con_app, solicitacao_id)
  if (!nrow(sol)) stop("Solicitação ", solicitacao_id, " não existe.")
  art <- listar_artigos(con_app, solicitacao_id)
  lic <- listar_licencas(con_app, solicitacao_id)
  prm <- ler_parametros(con_sjr)

  rl  <- resumo_licencas(lic)
  per <- calcular_periodo(con_sjr, rl$n_parentalidade, rl$pcd, rl$meses_licenca_medica)
  if (!per$valido) stop("Licenças inválidas: ", paste(per$erros, collapse = " "))

  entrada <- data.frame(titulo = art$titulo, ano = art$ano,
                        revista = ifelse(is.na(art$revista_digitada), "", art$revista_digitada),
                        issn = ifelse(is.na(art$issn), "", art$issn),
                        sourceid_conf = ifelse(art$confirmado_manual == 1, art$sourceid, NA_real_),
                        stringsAsFactors = FALSE)
  av  <- avaliar_artigos_app(con_sjr, entrada, n_anos = per$n_anos)
  det <- av$detalhe

  if (nrow(det)) {
    det$artigo_id <- art$id[det$id_artigo]
    det$confirmado_manual <- art$confirmado_manual[det$id_artigo]
    det$issn <- entrada$issn[det$id_artigo]
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
  areas <- lapply(unique(det$sourceid[!is.na(det$sourceid)]), function(s)
    notas_por_area(con_sjr, s, per$n_anos))
  names(areas) <- sprintf("%.0f", unique(det$sourceid[!is.na(det$sourceid)]))

  resumo <- av$resumo
  quando <- agora_txt()
  versao <- versao_banco_edital(con_sjr)
  retrato <- list(
    versao_formato = 1L,
    calculada_em = quando,
    versao_banco = versao,
    oficial = isTRUE(oficial),
    solicitacao = as.list(sol[1, c("id", "docente", "area_atuacao", "data_pedido", "ano_fim_base")]),
    parametros = prm,
    periodo = list(n_anos = per$n_anos, n_anos_pedido = per$n_anos_pedido, ano_ini = per$ano_ini,
                   ano_fim = per$ano_fim, limitado = per$limitado, avisos = per$avisos,
                   detalhe = per$detalhe, entradas = rl),
    licencas = lic,
    resumo = resumo,
    detalhe = det,
    areas = areas
  )
  json <- jsonlite::toJSON(retrato, auto_unbox = TRUE, digits = NA, na = "null",
                           dataframe = "rows", null = "null")
  id <- inserir(con_app,
    "INSERT INTO avaliacao (solicitacao_id, calculada_em, n_anos, ano_ini, ano_fim, soma, meta,
                            aprovado, versao_banco, detalhe_json, oficial)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    list(solicitacao_id, quando, per$n_anos, per$ano_ini, per$ano_fim, resumo$soma, resumo$meta,
         as.integer(resumo$aprovado), versao, as.character(json), as.integer(isTRUE(oficial))))
  obter_retrato(con_app, id)
}

listar_avaliacoes <- function(con, solicitacao_id) {
  out <- DBI::dbGetQuery(con,
    "SELECT id, solicitacao_id, calculada_em, n_anos, ano_ini, ano_fim, soma, meta, aprovado,
            oficial, versao_banco
     FROM avaliacao WHERE solicitacao_id = ? ORDER BY calculada_em DESC, id DESC",
    params = list(solicitacao_id))
  out$situacao <- situacao_soma(out$soma, out$meta)
  out
}

# Le um retrato gravado. NAO recalcula nada: e exatamente o que foi visto.
obter_retrato <- function(con, avaliacao_id) {
  a <- DBI::dbGetQuery(con, "SELECT * FROM avaliacao WHERE id = ?", params = list(avaliacao_id))
  if (!nrow(a)) return(NULL)
  r <- jsonlite::fromJSON(a$detalhe_json, simplifyDataFrame = TRUE)
  if (!is.data.frame(r$detalhe)) r$detalhe <- data.frame()
  if (!is.data.frame(r$licencas)) r$licencas <- data.frame()
  r$avaliacao_id <- a$id
  r$solicitacao_id <- a$solicitacao_id
  r
}
ultimo_retrato <- function(con, solicitacao_id) {
  id <- DBI::dbGetQuery(con,
    "SELECT id FROM avaliacao WHERE solicitacao_id = ? ORDER BY calculada_em DESC, id DESC LIMIT 1",
    params = list(solicitacao_id))$id
  if (!length(id)) NULL else obter_retrato(con, id)
}
