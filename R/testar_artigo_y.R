# =====================================================================
# testar_artigo_y.R  -  Confere se o Artigo Y esta funcionando
# Rodar da RAIZ do projeto (depois de gerar o sjr.db):
#   source("R/testar_artigo_y.R")
# Se tudo estiver certo, termina com "TODOS OS TESTES PASSARAM".
# Se algum falhar, para e diz qual (copie a mensagem e mande).
# =====================================================================

suppressPackageStartupMessages({ library(DBI); library(RSQLite) })
source("R/funcoes_banco.R", encoding = "UTF-8")
source("R/periodo_artigo_y.R", encoding = "UTF-8")

arq <- c("banco_edital.db", "sjr.db")
arq <- arq[file.exists(arq)][1]
if (is.na(arq)) stop("Nao achei banco_edital.db nem sjr.db nesta pasta. Gere o banco primeiro.")
message("Usando o banco: ", arq)
con <- conectar_banco(arq)

chk <- function(nome, cond) {
  if (!isTRUE(cond)) {
    dbDisconnect(con)
    stop("FALHOU: ", nome, call. = FALSE)
  }
  message("  ok  ", nome)
}
per <- function(...) calcular_periodo(con, ...)

message("Periodo (base 2020-2025):")
chk("sem licença = 4 anos (2022-2025)",
    per()$n_anos == 4 && per()$ano_ini == 2022 && per()$ano_fim == 2025 && per()$valido)
chk("1 licença de parentalidade = 6 anos (2020-2025)",
    per(n_parentalidade = 1)$n_anos == 6 && per(n_parentalidade = 1)$ano_ini == 2020)
x <- per(n_parentalidade = 2)
chk("2 licenças de parentalidade: pedido 8 anos, usa 6 e avisa",
    x$n_anos_pedido == 8 && x$n_anos == 6 && x$limitado && length(x$avisos) >= 1)
chk("PcD = 5 anos", per(pcd = TRUE)$n_anos == 5)
chk("licença médica de 1 mês = 5 anos (arredonda p/ cima)", per(meses_licenca_medica = 1)$n_anos == 5)
chk("licença médica de 12 meses = 5 anos", per(meses_licenca_medica = 12)$n_anos == 5)
chk("licença médica de 13 meses = 6 anos", per(meses_licenca_medica = 13)$n_anos == 6)
chk("várias licenças médicas somam (6 + 6 meses = 5 anos)", per(meses_licenca_medica = c(6, 6))$n_anos == 5)
x <- per(n_parentalidade = 1, meses_licenca_medica = 12)
chk("parentalidade + 12 meses de licença médica: pedido 7, usa 6",
    x$n_anos_pedido == 7 && x$n_anos == 6 && x$limitado)
chk("campo vazio (NA/NULL) vale 0", per(n_parentalidade = NA, meses_licenca_medica = NULL)$n_anos == 4)
x <- per(n_parentalidade = -1)
chk("valor negativo não quebra: valido = FALSE e período padrão",
    !x$valido && x$n_anos == 4 && length(x$erros) > 0)
chk("1,5 licença de parentalidade é inválido", !per(n_parentalidade = 1.5)$valido)
chk("licença médica negativa é inválida", !per(meses_licenca_medica = -3)$valido)
chk("a tabela detalhe fecha com o total", {
  d <- per(n_parentalidade = 1, meses_licenca_medica = 8)$detalhe
  d$anos[nrow(d)] == 7 || d$anos[nrow(d)] == 6   # pedido 7 (2 + 8 meses = 2,67 -> 3 extras), usado 6
})

message("meses_entre_datas:")
chk("01/01 a 31/01 = 1 mês", meses_entre_datas("2023-01-01", "2023-01-31") == 1)
chk("01/01 a 31/12 = 12 meses", meses_entre_datas("2023-01-01", "2023-12-31") == 12)
chk("data inválida = NA", is.na(meses_entre_datas("abc", "2023-01-31")))
chk("fim antes do início = NA", is.na(meses_entre_datas("2023-02-01", "2023-01-01")))

message("Avaliação de artigos com período ampliado:")
# revista de teste: p alto e titulo unico no banco (evita ambiguidade)
rev <- dbGetQuery(con,
  "SELECT titulo FROM v_docente
   WHERE p >= 0.9 AND titulo_norm IN
         (SELECT titulo_norm FROM revista GROUP BY titulo_norm HAVING COUNT(*) = 1)
   ORDER BY p DESC LIMIT 1")$titulo
chk("achei uma revista de teste no banco", length(rev) == 1)
art <- data.frame(
  titulo  = c("Artigo de 2020", "Artigo de 2024"),
  ano     = c(2020, 2024),
  revista = c(rev, rev),
  issn    = c("", ""),
  stringsAsFactors = FALSE
)
r4 <- avaliar_com_licencas(con, art)
chk("sem licença o artigo de 2020 fica fora do período",
    r4$detalhe$situacao[r4$detalhe$titulo == "Artigo de 2020"] == "fora_do_periodo")
chk("sem licença só 1 artigo conta e não atinge a meta (soma < 1,25)",
    r4$resumo$soma < 1.25 && !r4$resumo$aprovado)
r6 <- avaliar_com_licencas(con, art, n_parentalidade = 1)
chk("com 1 licença de parentalidade (6 anos) o artigo de 2020 entra",
    r6$detalhe$no_periodo[r6$detalhe$titulo == "Artigo de 2020"] == 1 && r6$resumo$n_anos == 6)
chk("e agora a soma passa de 1,25 e aprova", r6$resumo$soma > 1.25 && r6$resumo$aprovado)
chk("entrada inválida não avalia nem quebra",
    {
      r <- avaliar_com_licencas(con, art, n_parentalidade = -2)
      !r$periodo$valido && is.null(r$resumo)
    })
chk("sem artigos não quebra",
    {
      r <- avaliar_com_licencas(con, art[0, ], n_parentalidade = 1)
      r$resumo$soma == 0 && !r$resumo$aprovado
    })

dbDisconnect(con)

# Testes do app (app.db, CRUD e retrato de avaliacao)
source("R/testar_app.R", encoding = "UTF-8")

message("\nTODOS OS TESTES PASSARAM")
