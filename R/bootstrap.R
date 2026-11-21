# =====================================================================
# bootstrap.R  -  Prepara os bancos quando o app sobe num servidor novo
# (Posit Connect Cloud, shinyapps.io etc.), onde os .db nao vem do git.
#
#  banco_edital.db  faltando -> Knit do banco_edital.Rmd se dados/*.csv existirem;
#                               senao baixa do Drive (BANCO_EDITAL_DRIVE_ID).
#  app.db           faltando -> semeia os docentes ficticios com a senha
#                               PPGE_SENHA_DEMO (variavel secreta do servidor).
# Se a pasta do app nao for gravavel, usa a pasta temporaria.
# =====================================================================

BANCO_EDITAL_DRIVE_ID_PADRAO <- "1vTkiAwa4cqzWkFP5N-nHuqZ5P9EI0sRW"

eh_sqlite <- function(arq) {
  file.exists(arq) && identical(readBin(arq, "raw", 15), charToRaw("SQLite format 3"))
}

preparar_bancos <- function() {
  if (file.access(".", 2) != 0) {                       # pasta do app somente leitura
    if (!nzchar(Sys.getenv("PPGE_BANCO_EDITAL")) && !file.exists("banco_edital.db"))
      Sys.setenv(PPGE_BANCO_EDITAL = file.path(tempdir(), "banco_edital.db"))
    if (!nzchar(Sys.getenv("PPGE_APP_DB")) && !file.exists("app.db"))
      Sys.setenv(PPGE_APP_DB = file.path(tempdir(), "app.db"))
  }
  arq_ed <- Sys.getenv("PPGE_BANCO_EDITAL", "banco_edital.db")

  if (!eh_sqlite(arq_ed)) {
    csvs <- file.path("dados", c("revista.csv", "issn.csv", "percentil.csv"))
    if (all(file.exists(csvs)) && file.access(".", 2) == 0) {
      message("[bootstrap] gerando banco_edital.db pelo banco_edital.Rmd")
      knitr::knit("banco_edital.Rmd", output = file.path(tempdir(), "banco_edital.md"),
                  quiet = TRUE, envir = new.env())
      if (normalizePath(arq_ed, mustWork = FALSE) != normalizePath("banco_edital.db", mustWork = FALSE))
        file.copy("banco_edital.db", arq_ed, overwrite = TRUE)
    } else {
      id  <- Sys.getenv("BANCO_EDITAL_DRIVE_ID", BANCO_EDITAL_DRIVE_ID_PADRAO)
      url <- sprintf("https://drive.usercontent.google.com/download?id=%s&export=download&confirm=t", id)
      message("[bootstrap] baixando banco_edital.db do Drive")
      tmp <- tempfile(fileext = ".db")
      old <- options(timeout = max(600, getOption("timeout"))); on.exit(options(old), add = TRUE)
      ok <- tryCatch(utils::download.file(url, tmp, mode = "wb", quiet = TRUE) == 0, error = function(e) FALSE)
      if (!ok || !eh_sqlite(tmp)) {
        stop("Não consegui obter o banco_edital.db: o link do Drive precisa estar compartilhado ",
             "como 'qualquer pessoa com o link', ou suba dados/*.csv no repositório.")
      }
      file.copy(tmp, arq_ed, overwrite = TRUE); unlink(tmp)
    }
  }

  arq_app <- caminho_app_db()
  if (!file.exists(arq_app)) {
    if (nchar(Sys.getenv("PPGE_SENHA_DEMO")) < 8) {
      stop("Defina a variável secreta PPGE_SENHA_DEMO (8+ caracteres) no servidor: ",
           "é a senha dos logins da demonstração.")
    }
    message("[bootstrap] criando app.db com os docentes fictícios")
    old <- options(ppge.semear.nao_rodar = TRUE); on.exit(options(old), add = TRUE)
    source("R/semear_demo.R", encoding = "UTF-8", local = TRUE)
    sjr <- conectar_banco(arq_ed); app <- abrir_app_db(arq_app)
    on.exit({ DBI::dbDisconnect(app); DBI::dbDisconnect(sjr) }, add = TRUE)
    semear_demo(app, sjr, credenciais = file.path(tempdir(), "credenciais_demo.csv"))
  }
  invisible(TRUE)
}
