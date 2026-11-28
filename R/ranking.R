# =====================================================================
# ranking.R  -  Avaliacao dos artigos do Lattes + rankings de revistas
#   avaliar_lattes(con, artigos, n_anos)  regra do edital + posicao das revistas
#   ranking_revistas(con, n_anos, area)    ranking global ou de uma area
# Usa so o banco_edital.db (somente leitura) e as funcoes de funcoes_banco.R.
# =====================================================================

# Ranking global: todas as revistas classificadas, pela nota p do edital
# (maior percentil entre todas as areas e anos da janela). Como a 1a revista de
# cada area tem percentil 100, o empate e desfeito pelo maior SJR da janela.
SQL_RANK_GLOBAL <- "
  SELECT n.sourceid, MAX(pc.sjr) AS sjr,
         RANK() OVER (ORDER BY n.p DESC, MAX(pc.sjr) DESC) AS rank_global
  FROM nota_revista n
  JOIN janela j ON j.n_anos = n.n_anos
  JOIN percentil pc ON pc.sourceid = n.sourceid AND pc.ano BETWEEN j.ano_ini AND j.ano_fim
  WHERE n.n_anos = %d AND n.classificada = 1
  GROUP BY n.sourceid, n.p"

avaliar_lattes <- function(con, artigos, n_anos) {
  res <- avaliar_artigos(con, artigos, n_anos = n_anos)
  det <- res$detalhe
  if (nrow(det) == 0) return(res)

  # posicao da revista na melhor area (no ano do maximo) e no ranking global
  ids <- paste(sprintf("%.0f", unique(na.omit(det$sourceid))), collapse = ",")
  if (ids == "") ids <- "NULL"   # nenhuma revista casada: a consulta volta vazia
  pos <- DBI::dbGetQuery(con, sprintf("
      SELECT n.sourceid, r.titulo AS revista_sjr, n.ano_percentil_max AS ano_max,
             pc.rank AS rank_area, pc.n AS n_area, g.rank_global
      FROM nota_revista n
      JOIN revista r ON r.sourceid = n.sourceid
      LEFT JOIN percentil pc ON pc.sourceid = n.sourceid AND pc.area = n.melhor_area
                            AND pc.ano = n.ano_percentil_max
      LEFT JOIN (%s) g ON g.sourceid = n.sourceid
      WHERE n.n_anos = %d AND n.sourceid IN (%s)",
      sprintf(SQL_RANK_GLOBAL, n_anos), n_anos, ids))
  det <- merge(det, pos, by = "sourceid", all.x = TRUE)

  # por que o artigo entra (ou nao) na soma
  det$motivo <- ifelse(det$situacao == "fora_do_periodo", "Fora do período",
                ifelse(det$metodo == "sem_match", "Revista não encontrada no SJR",
                ifelse(is.na(det$percentil_max), "Revista sem classificação no período",
                ifelse(det$situacao == "nao_selecionado", "Fora dos 4 melhores",
                ifelse(det$p < 0.25, "p abaixo de 0,25", "Entra na soma")))))

  res$detalhe <- det[order(-det$selecionado, -det$p, -det$ano), ]
  res$resumo$total_global <- DBI::dbGetQuery(con, sprintf(
    "SELECT COUNT(*) AS n FROM nota_revista WHERE n_anos = %d AND classificada = 1", n_anos))$n
  res
}

# area = "" -> ranking global (todas as areas); senao, ranking dentro da area
ranking_revistas <- function(con, n_anos, area = "") {
  if (area == "") {
    sql <- sprintf("
      SELECT g.rank_global AS posicao, r.titulo, n.melhor_area AS area,
             n.percentil_max, n.p, g.sjr
      FROM (%s) g
      JOIN nota_revista n ON n.sourceid = g.sourceid AND n.n_anos = %d
      JOIN revista r ON r.sourceid = g.sourceid
      ORDER BY posicao, r.titulo", sprintf(SQL_RANK_GLOBAL, n_anos), n_anos)
    return(DBI::dbGetQuery(con, sql))
  }
  DBI::dbGetQuery(con, "
    SELECT RANK() OVER (ORDER BY a.percentil_max DESC, pc.sjr DESC) AS posicao, r.titulo,
           a.area, a.percentil_max, a.p, pc.sjr
    FROM nota_revista_area a
    JOIN revista r ON r.sourceid = a.sourceid
    JOIN percentil pc ON pc.sourceid = a.sourceid AND pc.area = a.area AND pc.ano = a.ano_percentil_max
    WHERE a.n_anos = ? AND a.area = ?
    ORDER BY posicao", params = list(as.integer(n_anos), area))
}
