# Dicionário dos bancos (ME315)

Dois bancos SQLite, **separados**:

| Banco | Para quê | Acesso no app | Versionado? |
|---|---|---|---|
| `banco_edital.db` | SJR (revistas, ISSN, percentis) + regras do edital | **somente leitura** (`conectar_banco()`) | não (gerado pelo Knit) |
| `app.db` | credenciamento: usuários, docentes, solicitações, licenças, artigos, retratos de avaliação | leitura e escrita (`abrir_app_db()`) | não (gerado por `R/semear_demo.R`) |

---

## Parte 1 — `banco_edital.db` (pronto; não alterar o esquema)

### Como gerar o banco

**Caminho principal: `banco_edital.Rmd`** (tudo num arquivo: esquema, carga dos CSVs, variáveis derivadas e auditoria).

```
projeto/
  banco_edital.Rmd
  dados/   revista.csv  issn.csv  percentil.csv
  R/       funcoes_banco.R  periodo_artigo_y.R  modulo_periodo.R  testar_artigo_y.R
  banco_edital.db        (gerado)
```

1. Pacotes: `install.packages(c("DBI","RSQLite","readr","dplyr","stringi","bit64","knitr","rmarkdown"))`.
2. Abra `banco_edital.Rmd` no RStudio e clique em **Knit**. O `.db` é criado na mesma pasta.
3. Se a auditoria do final reprovar, o Knit para com a mensagem.

O arquivo `banco_edital.db` nasce do `dbConnect()` do primeiro chunk: se não existir, o SQLite cria. Cada Knit **recria o banco do zero**.

(A consulta da comissão está embutida em `R/funcoes_banco.R`: o projeto **não usa** pasta `sql/`.)

O app abre o banco com `conectar_banco()` (padrão: `"banco_edital.db"`).

### Regras do edital → onde estão no banco

| Regra | Implementação |
|---|---|
| Soma de `p_i · I(p_i ≥ 0,25)` ≥ 1,25 sobre até 4 artigos | `avaliar_artigos()` (consulta em `R/funcoes_banco.R`); valores em `parametro` (`corte_p`, `meta`, `max_artigos`) |
| `p_i` = maior percentil entre todas as áreas e todos os anos do período | `nota_revista_area` (máximo por área) e `nota_revista` (máximo entre áreas) |
| Período 2022–2025 | `janela` com `n_anos = 4` (`ano_fim` em `parametro`) |
| Artigo Y: período ampliado | `janela` com `n_anos` = 5 e 6 (a base vai de 2020 a 2025, então 6 é o teto) |
| Revista sem classificação = 0 | `nota_revista` tem uma linha para **toda** revista; sem nota → `p = 0`, `classificada = 0` |
| Mostrar nota em cada área e a melhor | `v_revista_area` e `nota_revista.melhor_area` |

Para mudar qualquer valor (corte, meta, ano final, nº de artigos), edite o `INSERT` de `parametro` no `banco_edital.Rmd` e dê Knit de novo.

### Escalas (a principal armadilha)

- `percentil_max`: **0 a 100**, para exibir ("87,3").
- `p`: **0 a 1**, para a fórmula do edital (`0,25` e `1,25` estão nessa escala).
- Nunca compare `percentil_max` com `0,25` nem some `percentil_max` para comparar com `1,25`.

### Tabelas base (vêm dos CSVs)

**`revista`**: uma linha por revista. `sourceid` (INTEGER, chave), `titulo`, `tipo`, `titulo_norm` (minúsculo, sem acento, só `a-z0-9` e espaço; usado na busca e no casamento com o Lattes).

**`issn`**: `sourceid`, `issn` (8 caracteres, sem hífen, maiúsculo, **texto**). Uma revista pode ter vários.

**`percentil`**: uma linha por revista × área × ano. `sourceid`, `area`, `ano`, `sjr`, `rank`, `n`, `percentil` (0–100, arredondado em 6 casas, fórmula `((N − Rank + 1)/N)·100`).

**`parametro`**: `chave`, `valor`, `descricao`. Chaves: `n_anos_padrao` (4), `ano_fim` (2025), `corte_p` (0,25), `meta` (1,25), `max_artigos` (4), `ext_parentalidade_anos` (2), `ext_pcd_anos` (1).

### Tabelas derivadas (prontas para o app)

**`janela`**: períodos possíveis, sempre terminando em `ano_fim`. `n_anos` (4, 5, 6), `ano_ini`, `ano_fim`, `padrao` (1 = janela do edital).

**`area`**: `area`, `n_revistas`. Para menus e filtros.

**`nota_revista_area`**: maior percentil da revista em cada área dentro da janela. `sourceid`, `n_anos`, `area`, `percentil_max` (0–100), `p` (0–1), `ano_percentil_max`.

**`nota_revista`**: uma linha por revista × janela, para todas as revistas.

| Coluna | Significado |
|---|---|
| `p` | maior `p` entre as áreas (0 se não classificada) |
| `percentil_max` | o mesmo em 0–100 |
| `melhor_area` | área do máximo (empate: ordem alfabética); `NULL` se não classificada |
| `ano_percentil_max` | ano do máximo; `NULL` se não classificada |
| `n_areas` | quantas áreas têm dado na janela |
| `classificada` | 1 se tem percentil na janela |
| `conta_pontos` | 1 se `p ≥ corte_p` (entra na soma do edital) |
| `quartil` | Q1 (≥75), Q2 (≥50), Q3 (≥25), Q4 (<25) sobre `percentil_max`; convenção do grupo, não é o quartil oficial do SCImago |

### Views

- `v_revista_nota`: `revista` + `nota_revista` (todas as janelas; filtre por `n_anos`).
- `v_revista_area`: nota em cada área (`area`, `percentil_max`, `p`, `ano_percentil_max`, `e_melhor_area`).
- `v_docente`: atalho de `v_revista_nota` na janela padrão.

### Funções R (`R/funcoes_banco.R`, `R/periodo_artigo_y.R`, `R/modulo_periodo.R`)

| Função | Para quê |
|---|---|
| `conectar_banco()` | conexão somente leitura (`sourceid` volta como número) |
| `ler_parametros(con)` / `listar_janelas(con)` / `listar_areas(con)` | regras, períodos e áreas |
| `buscar_revista(con, texto, n_anos)` | autocompletar por título |
| `notas_por_area(con, sourceid, n_anos)` | nota em cada área + melhor |
| `preparar_artigos(df)` | normaliza o que vem do Lattes (e remove duplicatas título+ano) |
| `avaliar_artigos(con, artigos, n_anos)` | casa artigos × revistas, escolhe os 4 melhores, soma e compara com a meta |
| `normalizar_texto()` / `normalizar_issn()` | **use as mesmas** em qualquer lugar que compare título/ISSN |
| `calcular_periodo(con, n_parentalidade, pcd, meses_licenca_medica)` | Artigo Y: `n_anos`, `ano_ini`, `ano_fim`, `detalhe` (motivo/anos), `avisos`, `limitado`, `valido`, `erros` |
| `avaliar_com_licencas(...)` | calcula o período e avalia |
| `meses_entre_datas(inicio, fim)` | datas de licença → meses |
| `periodo_ui(id)` / `periodo_server(id, con)` | caixinha Shiny do Artigo Y |

`avaliar_artigos()` espera `titulo`, `ano`, `revista`, `issn` e devolve `$detalhe` (uma linha por artigo: `sourceid`, `metodo` — `issn`, `titulo` ou `sem_match` —, `n_candidatos`, `melhor_area`, `quartil`, `percentil_max`, `p`, `no_periodo`, `pos`, `selecionado`, `pontos`, `situacao`, `soma`, `meta`, `aprovado`) e `$resumo`.

### Artigo Y (regras em uso, pendentes de confirmação)

Parentalidade +2 anos **por licença**; PcD +1 ano (uma vez); licença médica + a duração (várias somam). As situações **somam, sem teto**, e o total extra é arredondado **para cima** em anos inteiros. A base vai de 2020 a 2025: acima de 6 anos o sistema usa 6 e avisa.

### Armadilhas já tratadas (não refazer no app)

- `sourceid` passa de 2³¹: **não use `as.integer()`**; use `conectar_banco()` (`bigint = "numeric"`).
- ISSN é **texto** (`"09700358"` perderia o zero se virasse número).
- Percentil arredondado a 6 casas e soma comparada com 6 casas (caso exato `p = 0,25`).
- Tabela temporária `tmp_artigo` é **por conexão**: em Shiny, uma conexão por sessão, fechada com `onStop`.

---

## Parte 2 — `app.db` (banco de credenciamento, gravável)

Criado por `abrir_app_db()` (`R/banco_app.R`) com `CREATE TABLE IF NOT EXISTS`. Sempre com `PRAGMA foreign_keys = ON`, `busy_timeout` e `journal_mode = WAL`. Caminho: `Sys.getenv("PPGE_APP_DB", "app.db")`.

| Tabela | Colunas | Regras |
|---|---|---|
| `docente` | `id`, `nome`, `lattes_id` (NULL), `area_atuacao`, `criado_em` | nome não vazio |
| `usuario` | `id`, `login` (único), `senha_hash` (scrypt), `papel`, `docente_id` (NULL) | `papel IN ('docente','comissao')`; docente exige `docente_id`; FK → `docente` (cascata) |
| `solicitacao` | `id`, `docente_id`, `data_pedido`, `status`, `ano_fim_base` | `status IN ('rascunho','enviada','avaliada')`; FK → `docente` (cascata) |
| `licenca` | `id`, `solicitacao_id`, `tipo`, `data_inicio`, `data_fim`, `observacao` | `tipo IN ('parentalidade','medica','pcd')`; `data_fim` obrigatória exceto PcD; fim ≥ início; comprovação = texto |
| `artigo` | `id`, `solicitacao_id`, `titulo`, `titulo_norm`, `ano`, `revista_digitada`, `issn`, `sourceid` (NULL), `confirmado_manual` (0/1) | `UNIQUE(solicitacao_id, titulo_norm, ano)` = regra de duplicata do `preparar_artigos()` |
| `avaliacao` | `id`, `solicitacao_id`, `calculada_em`, `n_anos`, `ano_ini`, `ano_fim`, `soma`, `meta`, `aprovado`, `versao_banco`, `detalhe_json`, `oficial` (0/1) | **retrato imutável** (trigger bloqueia `UPDATE`) |

Índices: `usuario(docente_id)`, `solicitacao(docente_id)`, `licenca(solicitacao_id)`, `artigo(solicitacao_id)`, `avaliacao(solicitacao_id, calculada_em)`.

**Fluxo de status:** rascunho → enviada (docente) → avaliada (comissão). A comissão pode devolver (enviada → rascunho) ou reabrir (avaliada → enviada). Só rascunho aceita mudanças em artigos e licenças.

**Retrato (`avaliacao.detalhe_json`):** JSON com tudo o que foi visto no cálculo — parâmetros, período (tabela motivo/anos, avisos, entradas), licenças, resumo, detalhe por artigo (com `revista_sjr`, `ano_percentil_max`, `motivo`, `explicacao`) e notas por área de cada revista casada, além de `versao_banco` (`SJR 2020-2025 | N revistas | md5 …`). Ler com `obter_retrato()` **não** consulta o `banco_edital.db`: o histórico não muda quando o SJR for atualizado.

**Avaliação de uma solicitação** (`avaliar_solicitacao(con_app, con_sjr, id)`): lê artigos e licenças → `resumo_licencas()` (meses de licença médica via `meses_entre_datas`) → `calcular_periodo()` → `avaliar_artigos()` → enriquece → grava o retrato.

**Confirmação manual de revista:** quando `n_candidatos > 1`, o docente escolhe a revista; `confirmar_revista()` grava `sourceid` e `confirmado_manual = 1`. Com `USAR_CONFIRMACAO_MANUAL = TRUE` (padrão, em `R/banco_app.R`), o cálculo usa essa revista (método `manual`) antes do ISSN e do título; para isso `avaliar_artigos_app()` reaproveita a própria `avaliar_artigos()` com um ramo extra na consulta. **Decisão pendente** (ver tela "Regras e premissas").

Principais funções: `criar_docente`, `criar_usuario`, `autenticar_usuario`, `criar_solicitacao`, `listar_solicitacoes`, `mudar_status`, `adicionar_licenca`, `listar_licencas`, `adicionar_artigo(s)`, `listar_artigos`, `confirmar_revista`, `candidatos_revista`, `avaliar_solicitacao`, `listar_avaliacoes`, `obter_retrato`, `ultimo_retrato`, `situacao_soma`.
