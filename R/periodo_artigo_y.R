# =====================================================================
# periodo_artigo_y.R  -  Artigo Y: periodo de analise AMPLIADO
#
# Uso (depois de carregar o banco):
#   source("R/funcoes_banco.R"); source("R/periodo_artigo_y.R")
#   con <- conectar_banco("sjr.db")
#   per <- calcular_periodo(con, n_parentalidade = 1, pcd = FALSE, meses_licenca_medica = 8)
#   res <- avaliar_artigos(con, artigos, n_anos = per$n_anos)
# ou direto:
#   res <- avaliar_com_licencas(con, artigos, n_parentalidade = 1, meses_licenca_medica = 8)
#
# Se aparecer caractere estranho nas mensagens, use  source(..., encoding = "UTF-8").
# =====================================================================

# Regras (valores ficam na tabela `parametro`. Se o banco for antigo e a chave
# nao existir, valem os padroes abaixo):
#   periodo padrao ............ 4 anos                     (n_anos_padrao)
#   parentalidade ............. +2 anos POR licenca (50%)   (ext_parentalidade_anos)
#   deficiencia (PcD) ......... +1 ano, uma vez             (ext_pcd_anos)
#                               (edital diz 50% mas "ate 5 anos": pendencia 5)
#   licenca medica ............ + duracao da licenca, qualquer duracao
#   varias situacoes .......... somam, sem teto
# Os rankings do SJR sao ANUAIS, entao o total de anos extras e arredondado
# PARA CIMA (6 meses de licenca medica = +1 ano). Um "ano" inteiro e a menor
# unidade possivel.

# ---------------------------------------------------------------------
# Meses de uma licenca a partir de datas (ex.: pausa na carreira do Lattes).
# Conta o dia inicial e o final e arredonda para cima, com tolerancia de ~1,5
# dia para que 01/01 a 31/01 seja 1 mes e 01/01 a 31/12 seja 12 meses.
# Data invalida ou fim antes do inicio devolve NA.
# ---------------------------------------------------------------------
meses_entre_datas <- function(inicio, fim) {
  conv <- function(x) {
    tryCatch(as.Date(x), error = function(e) as.Date(rep(NA, length(x))))
  }
  inicio <- conv(inicio)
  fim    <- conv(fim)
  dias   <- as.numeric(fim - inicio) + 1
  meses  <- ceiling(dias / 30.4375 - 0.05)
  meses[is.na(dias) | dias < 1] <- NA_real_
  meses
}

# ---------------------------------------------------------------------
# calcular_periodo: transforma as licencas em numero de anos do periodo.
#   n_parentalidade      quantas licencas de parentalidade (maternidade,
#                        paternidade, adocao). NAO e a duracao: cada uma
#                        amplia o periodo em 50%, qualquer que seja o tempo.
#   pcd                  TRUE/FALSE (docente, conjuge ou dependente PcD)
#   meses_licenca_medica duracao TOTAL em meses (pode ser um vetor: soma)
# Campo vazio (NA ou NULL) vale 0. Nunca da erro: entrada invalida volta com
# valido = FALSE e a lista de motivos em $erros (e n_anos = periodo padrao).
#
# Devolve lista:
#   valido, erros, avisos
#   n_anos_base, anos_extras_exatos, anos_extras (inteiro, arredondado p/ cima)
#   n_anos_pedido  periodo que o edital pediria
#   n_anos         periodo USADO (limitado ao que a base tem de dados)
#   limitado       TRUE se n_anos_pedido passou do que a base cobre
#   ano_ini, ano_fim do periodo usado
#   detalhe        data.frame(motivo, anos) para mostrar na tela
# ---------------------------------------------------------------------
calcular_periodo <- function(con, n_parentalidade = 0, pcd = FALSE, meses_licenca_medica = 0) {
  prm      <- ler_parametros(con)
  base     <- as.integer(prm$n_anos_padrao %||% 4)
  ext_par  <- as.numeric(prm$ext_parentalidade_anos %||% 2)
  ext_pcd  <- as.numeric(prm$ext_pcd_anos %||% 1)
  jan      <- listar_janelas(con)
  max_disp <- max(jan$n_anos)
  ano_fim  <- max(jan$ano_fim)

  como_numero <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    x[is.na(x)] <- 0
    x
  }
  par_v   <- como_numero(n_parentalidade)
  med_v   <- como_numero(meses_licenca_medica)
  n_par   <- sum(par_v)
  meses   <- sum(med_v)
  tem_pcd <- isTRUE(as.logical(pcd))

  erros <- character(0)
  if (any(par_v < 0) || abs(n_par - round(n_par)) > 1e-9) {
    erros <- c(erros, "A quantidade de licenças de parentalidade deve ser um número inteiro, zero ou maior.")
  }
  if (any(med_v < 0)) {
    erros <- c(erros, "A duração da licença médica não pode ser negativa.")
  }
  if (length(erros) > 0) {
    return(list(
      valido = FALSE, erros = erros, avisos = character(0),
      n_anos_base = base, anos_extras_exatos = NA_real_, anos_extras = NA_integer_,
      n_anos_pedido = NA_integer_, n_anos = base, limitado = FALSE,
      ano_ini = jan$ano_ini[jan$n_anos == base], ano_fim = ano_fim,
      detalhe = data.frame(motivo = character(0), anos = numeric(0), stringsAsFactors = FALSE)
    ))
  }

  extra_meses <- n_par * ext_par * 12 + (if (tem_pcd) ext_pcd * 12 else 0) + meses
  anos_extras <- as.integer(ceiling(round(extra_meses, 6) / 12))
  pedido      <- base + anos_extras
  usado       <- min(pedido, max_disp)
  limitado    <- pedido > max_disp
  ano_ini_usado <- jan$ano_ini[jan$n_anos == usado]

  avisos <- character(0)
  if (limitado) {
    avisos <- c(avisos, sprintf(
      "O período ampliado seria de %d anos (desde %d), mas a base só tem dados de %d a %d (no máximo %d anos). A análise usa %d anos e artigos anteriores a %d não podem ser avaliados.",
      as.integer(pedido), as.integer(ano_fim - pedido + 1), as.integer(ano_ini_usado),
      as.integer(ano_fim), as.integer(max_disp), as.integer(usado), as.integer(ano_ini_usado)))
  }
  if (abs(anos_extras - extra_meses / 12) > 1e-9) {
    avisos <- c(avisos, "O tempo extra foi arredondado para cima até anos inteiros, porque os rankings do SJR são anuais.")
  }

  linhas <- data.frame(
    motivo = c("Período padrão do edital",
               "Licença(s) de parentalidade",
               "Docente ou dependente com deficiência (PcD)",
               "Licença médica",
               "Arredondamento para anos inteiros"),
    anos = c(base, n_par * ext_par, if (tem_pcd) ext_pcd else 0, meses / 12,
             anos_extras - extra_meses / 12),
    stringsAsFactors = FALSE
  )
  linhas$anos <- round(linhas$anos, 2)
  linhas <- linhas[linhas$anos != 0, , drop = FALSE]
  linhas <- rbind(linhas, data.frame(motivo = "Total do período pedido", anos = pedido,
                                     stringsAsFactors = FALSE))
  rownames(linhas) <- NULL

  list(
    valido = TRUE, erros = character(0), avisos = avisos,
    n_anos_base = base, anos_extras_exatos = extra_meses / 12, anos_extras = anos_extras,
    n_anos_pedido = as.integer(pedido), n_anos = as.integer(usado), limitado = limitado,
    ano_ini = ano_ini_usado, ano_fim = ano_fim,
    detalhe = linhas
  )
}

# ---------------------------------------------------------------------
# avaliar_com_licencas: calcula o periodo e ja avalia os artigos nele.
# Devolve o mesmo que avaliar_artigos() mais $periodo (saida de calcular_periodo).
# Se $periodo$valido for FALSE, nada e avaliado ($detalhe e $resumo = NULL):
# mostre $periodo$erros na tela.
# ---------------------------------------------------------------------
avaliar_com_licencas <- function(con, artigos, n_parentalidade = 0, pcd = FALSE,
                                 meses_licenca_medica = 0) {
  per <- calcular_periodo(con, n_parentalidade, pcd, meses_licenca_medica)
  if (!per$valido) return(list(periodo = per, detalhe = NULL, resumo = NULL))
  av <- avaliar_artigos(con, artigos, n_anos = per$n_anos)
  av$periodo <- per
  av
}
