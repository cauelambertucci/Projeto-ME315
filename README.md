# Credenciamento Docente PPGE — demo (ME315)

Aplicativo **R + Shiny** que aplica as regras do edital de credenciamento (Artigo X e Artigo Y) sobre o ranking **SJR**. É uma **demonstração**: todos os docentes, artigos e licenças são fictícios; as revistas e percentis são reais (Scimago).

- **Docente:** solicitações e histórico, avaliação com medidor 0–4,0 e cartões dos artigos, período ampliado (Artigo Y), cadastro de artigos (manual, lista colada, CSV e o .zip/.xml exportado do Lattes, com prévia), buscador de revistas e "onde publicar".
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

## 6. Publicar a demo (site público)

### Posit Connect Cloud (recomendado)

Publica direto do GitHub. Ao iniciar num servidor novo, o app prepara os bancos sozinho (`R/bootstrap.R`): baixa o `banco_edital.db` do Drive (ou faz o Knit, se `dados/*.csv` estiverem no repositório) e cria os docentes fictícios. O `manifest.json` (pacotes e versões, exigido pelo Connect Cloud) é gerado e commitado pelo workflow *Gerar manifest* a cada mudança no código.

1. Em <https://connect.posit.cloud>, entre com a conta do GitHub e clique em **Publish → From GitHub → Shiny**.
2. Repositório `cauelambertucci/Projeto-ME315`, branch `main`, arquivo principal `app.R`.
3. Em **Advanced settings → Configure variables**, adicione `PPGE_SENHA_DEMO` (8+ caracteres) marcada como **secret**: é a senha de todos os logins (`comissao`, `docente01` … `docente15`).
4. **Publish**. A primeira abertura leva ~1 minuto (download do banco). O endereço fica em `https://connect.posit.cloud/...` e pode ser compartilhado.

No plano gratuito o repositório precisa ser **público**. O disco é temporário: o que for gravado volta ao estado semeado quando o app reinicia.

### shinyapps.io, pelo GitHub Actions

O workflow [`.github/workflows/publicar.yml`](.github/workflows/publicar.yml) publica no **shinyapps.io** quando você roda *Actions → Publicar demo → Run workflow*. Ele instala os pacotes, obtém o `banco_edital.db` (Knit dos `dados/*.csv`, se estiverem no repositório; senão baixa o `.db` do Drive pelo link compartilhado), roda os testes, semeia o `app.db` e publica.

Uma vez só:
1. Crie uma conta gratuita em <https://www.shinyapps.io> e escolha o nome da conta (vira o endereço `https://<conta>.shinyapps.io/ppge-credenciamento/`).
2. No shinyapps.io: *Account → Tokens → Show → Show secret*.
3. No GitHub: *Settings → Secrets and variables → Actions → New repository secret* e crie:
   - `SHINYAPPS_ACCOUNT` (nome da conta), `SHINYAPPS_TOKEN`, `SHINYAPPS_SECRET`;
   - `PPGE_SENHA_DEMO`: a senha (8+ caracteres) de todos os logins da demo (`comissao`, `docente01` … `docente15`).
4. *Actions → Publicar demo → Run workflow*. O endereço aparece no resumo da execução.

Observações:
- No shinyapps.io o disco é temporário: o que for gravado no `app.db` volta ao estado semeado quando a instância reinicia ou a cada novo deploy. Para uma demo isso basta.
- O arquivo do Lattes enviado é lido na hora e não é guardado; só os artigos cadastrados ficam no `app.db`.

### Manual, da sua máquina

```r
rsconnect::setAccountInfo(name = "<conta>", token = "<token>", secret = "<secret>")
source("deploy.R")
```

`deploy.R` gera o `banco_edital.db` (se faltar), semeia o `app.db` (se faltar) e publica só os arquivos necessários. Para **Posit Connect**, troque o `setAccountInfo` por `rsconnect::addServer(...)` + `rsconnect::connectUser(...)`; lá dá para apontar `PPGE_APP_DB` para um caminho persistente.

## 7. Usar com o seu currículo Lattes

Em *Artigos → Currículo Lattes*: na Plataforma Lattes, *Exportar → XML* baixa um `.zip`; envie o `.zip` (ou o `curriculo.xml`). O app lê só os **artigos publicados**, mostra quais revistas estão no SJR e o p de cada uma, pré-seleciona os de 2020 em diante e cadastra os que você marcar. Depois, *Avaliação → Calcular avaliação agora*.

## Decisões pendentes (aparecem no app em "Regras e premissas")

1. Maior percentil = melhor revista (assumido).
2. Meta 1,25 fixa em qualquer janela.
3. Artigo Y: parentalidade +2 anos por licença, PcD +1 ano, soma sem teto, arredondando para anos inteiros; base 2020–2025 ⇒ máximo 6 anos.
4. Só conta artigo publicado dentro da janela.
5. Casamento por ISSN, depois por título normalizado exato. **Na demo, a revista confirmada manualmente pelo docente prevalece** (`USAR_CONFIRMACAO_MANUAL` em `R/banco_app.R`).
6. Ano final fixo em 2025 (hoje é 2026): "quatro anos antes da solicitação" pode ser 2022–2025 ou 2023–2026.

Também é convenção desta demo (não do edital): "quase apto" = soma entre 1,00 e 1,25.
