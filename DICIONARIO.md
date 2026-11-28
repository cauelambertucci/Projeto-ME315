# Dicionário dos bancos (ME315)

Um banco SQLite, `banco_edital.db` (SJR + regras do edital), aberto **somente leitura** com `conectar_banco()`. Não é versionado: é gerado pelo Knit do `banco_edital.Rmd` ou baixado do Drive pelo `R/baixar_banco.R`.

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

## Parte 2 — O app

O app não grava nada. Fluxo:

1. `ler_lattes(caminho)` (`R/lattes.R`) lê os artigos publicados do currículo (`.zip` ou `.xml`) e devolve `titulo, ano, revista, issn`.
2. `periodo_server()` calcula a janela do Artigo Y (`calcular_periodo()`).
3. `avaliar_lattes(con, artigos, n_anos)` (`R/ranking.R`) chama `avaliar_artigos()` e acrescenta, para cada revista:
   - `rank_area` de `n_area`: posição na melhor área, no ano do maior percentil (tabela `percentil`);
   - `rank_global`: posição entre todas as revistas classificadas na janela, ordenadas por `p` e, no empate, pelo maior SJR da janela;
   - `motivo`: por que o artigo entra ou não na soma.
4. `ranking_revistas(con, n_anos, area)` monta o ranking global (`area = ""`) ou o ranking de uma área (`nota_revista_area`, desempate pelo SJR).
