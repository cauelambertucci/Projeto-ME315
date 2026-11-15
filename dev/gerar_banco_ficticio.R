# =====================================================================
# dev/gerar_banco_ficticio.R  -  SOMENTE PARA DESENVOLVIMENTO / CI
#
# Gera CSVs com revistas FICTICIAS no mesmo formato de dados/*.csv e roda o
# banco_edital.Rmd sobre eles, produzindo um banco_edital.db pequeno (~1 MB)
# com o MESMO esquema do real. Serve para testar o app e rodar os testes sem
# os 39 MB do SJR.
#
#   Rscript dev/gerar_banco_ficticio.R            # cria ./banco_edital.db se nao existir
#   Rscript dev/gerar_banco_ficticio.R --forcar   # sobrescreve
#
# NAO use este banco na demo publicada: la vale o banco gerado pelo Knit do
# banco_edital.Rmd com os CSVs reais (README, passo 2).
#
# Casos cobertos de proposito:
#   - sourceid acima de 2^31
#   - ISSN com zero a esquerda e com X; ISSN com hifen no CSV
#   - ISSN ligado a DUAS revistas (casamento ambiguo)
#   - dois titulos que normalizam igual (ambiguo por titulo)
#   - revistas sem nenhum percentil (sem classificacao)
#   - revistas que so tem percentil em 2020-2021 (so contam com janela de 6 anos)
# =====================================================================

args   <- commandArgs(trailingOnly = TRUE)
forcar <- "--forcar" %in% args
destino <- "banco_edital.db"
if (file.exists(destino) && !forcar) {
  message("banco_edital.db ja existe; nada a fazer (use --forcar para recriar).")
  quit(save = "no")
}

set.seed(315)
anos  <- 2020:2025
areas <- c("Statistics and Probability", "Statistics, Probability and Uncertainty",
           "Economics and Econometrics", "Applied Mathematics", "Epidemiology",
           "Computer Science Applications", "Public Health, Environmental and Occupational Health")

prefixo <- c("Journal of", "Annals of", "Review of", "Letters in", "Bulletin of",
             "Advances in", "Studies in", "Archives of", "Frontiers of", "Transactions on")
adjetivo <- c("Fictional", "Imaginary", "Hypothetical", "Synthetic", "Simulated",
              "Notional", "Invented", "Placeholder", "Mock", "Example")
campo <- c("Statistics", "Probability", "Econometrics", "Biostatistics", "Data Science",
           "Applied Mathematics", "Epidemiology", "Survey Methods", "Stochastic Models",
           "Computational Statistics", "Health Metrics", "Quantitative Economics")

combos <- expand.grid(p = prefixo, a = adjetivo, c = campo, stringsAsFactors = FALSE)
combos <- combos[sample(nrow(combos), 150), ]
titulos <- paste(combos$p, combos$a, combos$c)
# alguns titulos em portugues, com acento (testa normalizar_texto)
titulos[1:4] <- c("Revista Brasileira Fictícia de Estatística",
                  "Cadernos Imaginários de Saúde Pública",
                  "Estudos Hipotéticos em Econometria & Finanças",
                  "Boletim Simulado de Matemática Aplicada")
# dois titulos que normalizam igual (ambiguo por titulo)
titulos[150] <- paste0(titulos[149], ".")

n_rev <- length(titulos)
sourceid <- sample(12000:99999, n_rev)
sourceid[c(10, 20, 30)] <- c(21100855437, 21101042107, 21100223311)   # > 2^31

revista <- data.frame(sourceid = sourceid, titulo = titulos, tipo = "journal",
                      stringsAsFactors = FALSE)

# ISSN: 8 caracteres, alguns com X, um com zero a esquerda, escritos com hifen
gera_issn <- function() {
  d <- sprintf("%07d", sample(1e6:9999999, 1))
  paste0(d, sample(c(0:9, "X"), 1))
}
issn_v <- vapply(seq_len(n_rev), function(i) gera_issn(), "")
issn_v[5] <- "0098765X"
issn <- data.frame(sourceid = sourceid, issn = issn_v, stringsAsFactors = FALSE)
# segunda ISSN (eletronica) para metade das revistas
seg <- sample(n_rev, n_rev %/% 2)
issn <- rbind(issn, data.frame(sourceid = sourceid[seg],
                               issn = vapply(seg, function(i) gera_issn(), ""),
                               stringsAsFactors = FALSE))
# ISSN compartilhado por duas revistas (casamento ambiguo)
issn <- rbind(issn, data.frame(sourceid = sourceid[8], issn = issn_v[7],
                               stringsAsFactors = FALSE))
issn$issn <- paste0(substr(issn$issn, 1, 4), "-", substr(issn$issn, 5, 8))

# revistas sem classificacao (nenhum percentil)
sem_classif <- c(140, 141, 142)
# revistas que so tem dados em 2020-2021
so_antigas <- c(130, 131, 132)

qualidade <- runif(n_rev)
qualidade[c(1, 2)] <- c(0.97, 0.55)           # ancoras para os casos de teste
linhas <- list()
for (i in setdiff(seq_len(n_rev), sem_classif)) {
  ar <- sample(areas, sample(1:3, 1, prob = c(.4, .4, .2)))
  an <- if (i %in% so_antigas) 2020:2021 else anos
  for (a in ar) for (y in an) {
    linhas[[length(linhas) + 1]] <- data.frame(
      sourceid = sourceid[i], area = a, ano = y,
      sjr = round(pmax(0.05, qualidade[i] * 3 + rnorm(1, 0, 0.25)), 3),
      stringsAsFactors = FALSE)
  }
}
percentil <- do.call(rbind, linhas)
percentil <- do.call(rbind, lapply(split(percentil, list(percentil$area, percentil$ano), drop = TRUE),
  function(g) {
    g <- g[order(-g$sjr, g$sourceid), ]
    g$rank <- seq_len(nrow(g))
    g$n <- nrow(g)
    g$percentil <- round((g$n - g$rank + 1) / g$n * 100, 6)
    g
  }))
rownames(percentil) <- NULL

# ---- roda o Rmd numa pasta temporaria ---------------------------------
tmp <- tempfile("banco_ficticio_")
dir.create(file.path(tmp, "dados"), recursive = TRUE)
dir.create(file.path(tmp, "R"))
write.csv(revista,   file.path(tmp, "dados", "revista.csv"),   row.names = FALSE, fileEncoding = "UTF-8")
write.csv(issn,      file.path(tmp, "dados", "issn.csv"),      row.names = FALSE, fileEncoding = "UTF-8")
write.csv(percentil, file.path(tmp, "dados", "percentil.csv"), row.names = FALSE, fileEncoding = "UTF-8")
file.copy("banco_edital.Rmd", tmp)
file.copy("R/funcoes_banco.R", file.path(tmp, "R"))

old <- setwd(tmp)
suppressMessages(knitr::knit("banco_edital.Rmd", output = "banco_edital.md", quiet = TRUE,
                             envir = new.env()))
setwd(old)
ok <- file.copy(file.path(tmp, "banco_edital.db"), destino, overwrite = TRUE)
if (!ok) stop("Falhou ao copiar o banco gerado.")
message(sprintf("banco_edital.db FICTICIO criado: %d revistas, %d linhas de percentil.",
                nrow(revista), nrow(percentil)))
