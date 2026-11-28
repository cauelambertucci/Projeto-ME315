# Credenciamento Docente PPGE (ME315)

Aplicativo em R + Shiny que avalia um currículo Lattes pelas regras do edital de credenciamento (Artigo X e Artigo Y), usando o ranking SJR (Scimago).

- **Avaliar meu Lattes:** o docente envia o `.zip` exportado do Lattes e informa as licenças do Artigo Y. O app mostra a soma com a conta da fórmula e o ranking dos artigos. Para cada artigo: ano, revista, melhor área, posição da revista na área, posição no ranking global, percentil, p, pontos e se entra na soma.
- **Revistas:** ranking global de revistas (todas as áreas) ou ranking dentro de uma área.
- **Regras:** as regras do edital e as premissas ainda não confirmadas.

Não há login e nada é gravado.

## Arquivos

| Arquivo | O que faz |
|---|---|
| `app.R` | interface e servidor |
| `R/lattes.R` | lê o currículo Lattes (`.zip` ou `.xml`) |
| `R/ranking.R` | avaliação dos artigos e rankings de revistas |
| `R/baixar_banco.R` | baixa o `banco_edital.db` se ele não existir |
| `R/funcoes_banco.R`, `R/periodo_artigo_y.R`, `R/modulo_periodo.R` | regras do edital (feitos antes pelo grupo) |
| `R/testar_artigo_y.R`, `R/testar_lattes.R` | testes |
| `banco_edital.Rmd` | gera o `banco_edital.db` a partir de `dados/*.csv` |

Estrutura do banco: [`DICIONARIO.md`](DICIONARIO.md).

## Rodar

```r
install.packages(c("shiny", "bslib", "DT", "DBI", "RSQLite", "xml2", "stringi"))
shiny::runApp()
```

Se o `banco_edital.db` não estiver na pasta, o app baixa a cópia do Drive. Para gerar o banco do zero, coloque os CSVs em `dados/` e dê Knit no `banco_edital.Rmd`.

## Testes

```r
source("R/testar_artigo_y.R")
```

Termina com `TODOS OS TESTES PASSARAM`. No GitHub, o workflow **Testes** roda os testes sobre o banco real a cada mudança.

## Publicar (Posit Connect Cloud)

**Publish → From GitHub → Shiny**, repositório `cauelambertucci/Projeto-ME315`, arquivo `app.R`. O `manifest.json` é gerado pelo workflow **Gerar manifest**.

## Como baixar o Lattes

Em lattes.cnpq.br, abra o currículo e use **Exportar → XML** (ou, na página pública, **Baixar currículo**). Envie o `.zip` sem descompactar.
