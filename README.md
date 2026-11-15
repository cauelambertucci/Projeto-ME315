# Credenciamento Docente PPGE — demo (ME315)

Aplicativo **R + Shiny** que aplica as regras do edital de credenciamento (Artigo X e Artigo Y) sobre o ranking **SJR**. É uma **demonstração**: todos os docentes, artigos e licenças são fictícios; as revistas e percentis são reais (Scimago).

- **Docente:** solicitações e histórico, avaliação com medidor 0–4,0 e cartões dos artigos, período ampliado (Artigo Y), cadastro de artigos (manual, lista colada, CSV, XML do Lattes), buscador de revistas e "onde publicar".
- **Comissão:** lista de todas as solicitações com filtros e situação (apto / quase apto / não apto), detalhe somente leitura, painel, exportação CSV e PDF.
- **Regras e premissas:** parâmetros em uso e as decisões ainda pendentes.

Estrutura de dados: veja [`DICIONARIO.md`](DICIONARIO.md).

```
app.R                     # interface (bslib + DT + shinymanager)
banco_edital.Rmd          # gera banco_edital.db (SJR + regras)  — somente leitura no app
dados/                    # revista.csv, issn.csv, percentil.csv (entrada do Rmd)
R/funcoes_banco.R         # regras do Artigo X (pronto)
R/periodo_artigo_y.R      # Artigo Y (pronto)
R/modulo_periodo.R        # caixinha Shiny do Artigo Y (pronto)
R/banco_app.R             # app.db: esquema, CRUD, retrato de avaliação
R/semear_demo.R           # 15 docentes fictícios
R/importar_artigos.R      # lista colada, CSV, Lattes XML
R/mod_*.R, ui_componentes.R
R/testar_artigo_y.R       # testes (chama também R/testar_app.R)
deploy.R                  # publicação
dev/gerar_banco_ficticio.R  # banco_edital.db pequeno e FICTÍCIO, só para desenvolvimento/CI
```

## 1. Instalar os pacotes

R ≥ 4.1.

```r
install.packages(c(
  "shiny", "bslib", "DT", "shinymanager", "DBI", "RSQLite", "jsonlite", "scrypt",
  "ggplot2", "gridExtra", "xml2", "stringi", "bit64",
  "readr", "dplyr", "knitr", "rmarkdown",   # para gerar o banco_edital.db
  "rsconnect"                               # para publicar
))
```

## 2. Gerar o `banco_edital.db`

Coloque `revista.csv`, `issn.csv` e `percentil.csv` em `dados/`, abra `banco_edital.Rmd` no RStudio e clique em **Knit** (ou, no console, na raiz do projeto):

```r
rmarkdown::render("banco_edital.Rmd")
```

O arquivo tem ~39 MB e **não vai para o git** (`.gitignore`); ele é gerado em cada máquina e no deploy.

> Sem os CSVs? `Rscript dev/gerar_banco_ficticio.R` cria um `banco_edital.db` pequeno com revistas **inventadas** e o mesmo esquema, só para testar. Não use na demo publicada.

## 3. Criar e semear o `app.db`

```bash
Rscript R/semear_demo.R
```

- Cria `app.db` (gravável, separado do `banco_edital.db`) e 15 docentes fictícios + 1 usuário `comissao`.
- Os docentes cobrem: apto, quase apto, não apto, ISSN ambíguo, artigo fora do período, revista sem classificação, duplicata e Artigo Y (parentalidade, PcD, licença médica, período limitado a 6 anos).
- **Credenciais de demonstração** ficam **fora do repositório**, em `~/.ppge/credenciais_demo.csv` (ou no caminho de `PPGE_CREDENCIAIS`). As senhas são sorteadas na primeira vez e reaproveitadas depois; o banco guarda só o hash (scrypt). Para escolher as senhas, crie esse CSV antes (`login,senha,papel`, senhas com 8+ caracteres).
- Para recriar do zero: `Rscript R/semear_demo.R --forcar`.

## 4. Rodar o app

```r
shiny::runApp()
```

Entre com um login do arquivo de credenciais (`comissao`, `docente01` … `docente15`).

Variáveis opcionais: `PPGE_BANCO_EDITAL` (caminho do banco_edital.db) e `PPGE_APP_DB` (caminho do app.db).

## 5. Testes

```r
source("R/testar_artigo_y.R")
```

Roda os 25 testes do Artigo Y e, em seguida, `R/testar_app.R` (esquema e CRUD do app.db, chaves estrangeiras, duplicatas, fluxo de status, retrato imutável, Artigo Y a partir das licenças, revista ambígua e confirmação manual, importadores e semente). Termina com **TODOS OS TESTES PASSARAM**. Usa um app.db temporário; não mexe no da demo.

## 6. Publicar a demo

O app precisa dos dois `.db` ao lado do `app.R`. Eles não estão no git: são gerados e enviados no deploy.

### shinyapps.io

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "<conta>", token = "<token>", secret = "<secret>")  # painel do shinyapps.io > Tokens
source("deploy.R")
```

`deploy.R` gera o `banco_edital.db` (se faltar), semeia o `app.db` (se faltar) e publica só os arquivos necessários (`app.R`, `R/*.R` do app e os dois bancos). As senhas continuam só no seu `~/.ppge/credenciais_demo.csv`.

Observações:
- No shinyapps.io o disco é temporário: o que os usuários gravarem no `app.db` volta ao estado semeado quando a instância reinicia. Para uma demo isso é suficiente.
- O plano gratuito aceita o pacote de ~40 MB; se o upload ficar lento, gere o banco com `Rscript dev/gerar_banco_ficticio.R` só para testar o deploy.

### Posit Connect

```r
rsconnect::addServer("https://<seu-connect>/", name = "connect")
rsconnect::connectUser(server = "connect")
source("deploy.R")
```

No Connect o `app.db` também pode ficar num caminho persistente: defina `PPGE_APP_DB` nas variáveis de ambiente do conteúdo.

## Decisões pendentes (aparecem no app em "Regras e premissas")

1. Maior percentil = melhor revista (assumido).
2. Meta 1,25 fixa em qualquer janela.
3. Artigo Y: parentalidade +2 anos por licença, PcD +1 ano, soma sem teto, arredondando para anos inteiros; base 2020–2025 ⇒ máximo 6 anos.
4. Só conta artigo publicado dentro da janela.
5. Casamento por ISSN, depois por título normalizado exato. **Na demo, a revista confirmada manualmente pelo docente prevalece** (`USAR_CONFIRMACAO_MANUAL` em `R/banco_app.R`).
6. Ano final fixo em 2025 (hoje é 2026): "quatro anos antes da solicitação" pode ser 2022–2025 ou 2023–2026.

Também é convenção desta demo (não do edital): "quase apto" = soma entre 1,00 e 1,25.
