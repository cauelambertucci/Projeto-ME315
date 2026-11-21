# =====================================================================
# bootstrap.R  -  Garante o banco_edital.db quando o app sobe num servidor
# novo (Posit Connect Cloud etc.), onde o .db nao vem do git:
#   1. se dados/revista.csv, issn.csv e percentil.csv existirem: Knit do
#      banco_edital.Rmd;
#   2. senao: baixa do Drive (BANCO_EDITAL_DRIVE_ID; link compartilhado).
# Se a pasta do app nao for gravavel, usa a pasta temporaria.
# =====================================================================

BANCO_EDITAL_DRIVE_ID_PADRAO <- "1vTkiAwa4cqzWkFP5N-nHuqZ5P9EI0sRW"

eh_sqlite <- function(arq) {
  file.exists(arq) && identical(readBin(arq, "raw", 15), charToRaw("SQLite format 3"))
}

preparar_banco <- function() {
  arq <- Sys.getenv("PPGE_BANCO_EDITAL", "banco_edital.db")
  if (eh_sqlite(arq)) return(invisible(arq))
  if (file.access(".", 2) != 0 && !nzchar(Sys.getenv("PPGE_BANCO_EDITAL"))) {
    arq <- file.path(tempdir(), "banco_edital.db")
    Sys.setenv(PPGE_BANCO_EDITAL = arq)
    if (eh_sqlite(arq)) return(invisible(arq))
  }

  csvs <- file.path("dados", c("revista.csv", "issn.csv", "percentil.csv"))
  if (all(file.exists(csvs)) && file.access(".", 2) == 0) {
    message("[bootstrap] gerando banco_edital.db pelo banco_edital.Rmd")
    knitr::knit("banco_edital.Rmd", output = file.path(tempdir(), "banco_edital.md"),
                quiet = TRUE, envir = new.env())
    if (normalizePath(arq, mustWork = FALSE) != normalizePath("banco_edital.db", mustWork = FALSE)) {
      file.copy("banco_edital.db", arq, overwrite = TRUE)
    }
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
    file.copy(tmp, arq, overwrite = TRUE); unlink(tmp)
  }
  invisible(arq)
}
