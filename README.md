# Credenciamento Docente PPGE — avaliação pelo currículo Lattes (ME315)

Aplicativo **R + Shiny** que aplica as regras do edital de credenciamento (Artigo X e Artigo Y) aos artigos de um **currículo Lattes real**, usando o ranking **SJR** (Scimago).

- **Avaliar meu Lattes:** a pessoa envia o `.zip`/`.xml` exportado da Plataforma Lattes, confere os artigos (com a revista encontrada no SJR e o p de cada uma), informa licenças do Artigo Y e vê o resultado: medidor 0–4,0 com a meta 1,25, os artigos que entram na soma e o motivo dos que não entram.
- **Revistas:** buscador (nota em cada área) e "onde publicar" por área.
- **Regras e premissas:** parâmetros em uso e decisões ainda pendentes.

Sem login. **Nada é gravado:** o currículo é lido na hora e descartado.

Estrutura de dados: [`DICIONARIO.md`](DICIONARIO.md).

```
app.R                    # interface (bslib + DT)
banco_edital.Rmd         # gera banco_edital.db (SJR + regras) — somente leitura no app
R/funcoes_banco.R        # Artigo X (pronto)
R/periodo_artigo_y.R     # Artigo Y (pronto)
R/modulo_periodo.R       # caixinha Shiny do Artigo Y (pronto)
R/importar_artigos.R     # leitor do Lattes (.zip/.xml, ISO-8859-1) e prévia no SJR
R/avaliacao.R            # avaliar_lattes(): cálculo completo em memória
R/mod_lattes.R, mod_avaliacao.R, mod_revistas.R, mod_regras.R, ui_componentes.R
R/bootstrap.R            # obtém o banco_edital.db ao iniciar num servidor novo
R/testar_artigo_y.R      # testes (chama também R/testar_lattes.R)
```

## Rodar na sua máquina

1. Pacotes (R ≥ 4.1):

   ```r
   install.packages(c("shiny", "bslib", "DT", "DBI", "RSQLite", "xml2", "stringi",
                      "bit64", "readr", "dplyr", "knitr", "rmarkdown"))
   ```

2. `banco_edital.db`: coloque `revista.csv`, `issn.csv` e `percentil.csv` em `dados/` e dê **Knit** no `banco_edital.Rmd` (ou `rmarkdown::render("banco_edital.Rmd")`). Sem os CSVs, o app baixa o `.db` do Drive ao iniciar (link compartilhado; ID em `R/bootstrap.R` ou na variável `BANCO_EDITAL_DRIVE_ID`). O arquivo tem ~39 MB e não vai para o git.

3. `shiny::runApp()`

## Testes

```r
source("R/testar_artigo_y.R")
```

Roda os 25 testes do Artigo Y e `R/testar_lattes.R` (currículo no formato real da Plataforma Lattes — ISO-8859-1, `.zip`, artigos aceitos ignorados —, prévia no SJR, avaliação com e sem licenças, duplicatas, revista ambígua). Termina com **TODOS OS TESTES PASSARAM**. No GitHub, o workflow **Testes** roda tudo sobre o banco SJR real a cada mudança no código e confere que o app sobe.

## Publicar (Posit Connect Cloud)

1. Em <https://connect.posit.cloud>: **Publish → From GitHub → Shiny**.
2. Repositório `cauelambertucci/Projeto-ME315`, branch `main`, arquivo principal `app.R`.
3. **Publish**. Com *Automatically publish on push*, cada mudança na `main` atualiza o site.

O `manifest.json` (pacotes e versões que o Connect Cloud exige) é gerado e commitado pelo workflow **Gerar manifest** quando o código muda. No plano gratuito o repositório precisa ser público. Não há variáveis a configurar.

## Usar com o seu Lattes

Na Plataforma Lattes, abra o currículo, clique em **Exportar → XML** (baixa um `.zip`) e envie o `.zip` na aba **Avaliar meu Lattes**. São lidos só os **artigos publicados**; os de 2020 em diante vêm marcados (a base SJR começa em 2020). Se o ISSN de um artigo corresponder a mais de uma revista, o resultado mostra um seletor para escolher a certa.

## Decisões pendentes (aparecem no app em "Regras e premissas")

1. Maior percentil = melhor revista (assumido).
2. Meta 1,25 fixa em qualquer janela.
3. Artigo Y: parentalidade +2 anos por licença, PcD +1 ano, soma sem teto, arredondando para anos inteiros; base 2020–2025 ⇒ máximo 6 anos.
4. Só conta artigo publicado dentro da janela.
5. Casamento por ISSN, depois por título normalizado exato; a revista escolhida pelo docente no seletor prevalece (`USAR_CONFIRMACAO_MANUAL` em `R/avaliacao.R`).
6. Ano final fixo em 2025 (hoje é 2026): "quatro anos antes da solicitação" pode ser 2022–2025 ou 2023–2026.

"Quase apto" (soma entre 1,00 e 1,25) é só informativo.
