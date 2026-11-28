# =====================================================================
# baixar_banco.R  -  Se o banco_edital.db nao existir (servidor novo),
# baixa a copia compartilhada no Drive. Localmente, gere com o Knit do
# banco_edital.Rmd.
# =====================================================================

ID_DRIVE <- "1vTkiAwa4cqzWkFP5N-nHuqZ5P9EI0sRW"

baixar_banco <- function(arquivo = "banco_edital.db") {
  if (file.exists(arquivo)) return(invisible(arquivo))
  url <- paste0("https://drive.usercontent.google.com/download?export=download&confirm=t&id=", ID_DRIVE)
  options(timeout = 600)
  utils::download.file(url, arquivo, mode = "wb", quiet = TRUE)
  invisible(arquivo)
}
