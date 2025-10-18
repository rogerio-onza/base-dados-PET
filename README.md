# 📷 Base de dados do Parque Estadual do Turvo

## 📝 Descrição

Este repositório reúne **scripts em R** para organizar e revisar vídeos de **armadilhas fotográficas** usados no monitoramento do Parque Estadual do Turvo.  

Há dois fluxos principais:

1. **Separação por espécie e armadilha (trap)** a partir de uma planilha mestre (`CSV`) — com *dry-run*, deduplicação e relatórios.  
2. **Pós-processamento por estação (Sxx A/B)** usando as planilhas do **Addax** (`results_SxxA.xlsx` / `results_SxxB.xlsx`) — movendo vídeos para verificação manual ou excluindo casos de alta confiança de pessoa/veículo.

## 🎯 Objetivos

- Estruturar vídeos em pastas por **espécie** e nomear como **`<TrapID>_<file_base>.<ext>`**.  
- Detectar e reportar **faltantes**, **duplicatas** e **conflitos no destino**.  
- Automatizar a triagem pós-Addax:
  - **Mover** vídeos com **baixa confiança** de pessoa/veículo e **sem detecção** para revisão manual.  
  - **Excluir** vídeos com **confiança alta** de pessoa/veículo.  
- Produzir **relatórios** para auditoria e reprodutibilidade.


## 🗂️ Estrutura do Repositório


├── R/

│ ├── process_videos.R # v0.3.3 — separa por espécie/trap (TEST/COPY/MOVE)

│ ├── station_postprocess.R # pós-processamento Addax (dry_run)

│ ├── verify_copies.R # (opcional) checa se o plano foi copiado

├── README.md

├── .gitignore

└── LICENSE

## 📦 Dependências

Instale os pacotes abaixo antes de rodar:

```r
install.packages(c(
  "readr","readxl",     # I/O (CSV/Excel)
  "dplyr","stringr","tibble","tidyr","purrr",  # manipulação
  "fs"                  # operações de arquivos e pastas
))
```

## 🚀 Como Usar
#### 1) Separação por espécie/trap — R/process_videos.R (v0.3.3)

- Lê um CSV com as colunas: Nome do Arquivo, Ponto ID (ex.: T01A, S02B…), Nome Comum.

- Procura .mp4/.avi em duas estruturas por trap:

- base_dir/TRAP → .../Campanha/T01A

- base_dir/prefixo/sufixo → .../Campanha/T01/A

- Gera plano de destino (uma pasta por espécie) e deduplica por Ponto ID + file_base.

- **Modo action="test" não altera nada**; salva relatórios (plan.csv, faltantes, duplicatas, conflitos).


```r
source("R/process_videos.R")

base_dir  <- "F:/Campanha 03"
dest_dir  <- "C:/Users/roger/OneDrive/.../videos_sep_campanha_3"
csv_path  <- "C:/Users/roger/OneDrive/.../monitoramento_turvo_rogerio_2021_2022.csv"

# Opção 1: gerar via helper (se usar R/make_traps.R)
# source("R/make_traps.R")
# selected_traps <- c(make_traps("T",1,14), make_traps("S",1,12), make_traps("C",2,8))

# Opção 2: lista explícita
selected_traps <- c(
  "T01A","T01B","T02A","T02B","T03A","T03B","T04A","T04B","T05A","T05B","T06A","T06B",
  "T07A","T07B","T08A","T08B","T09A","T09B","T10A","T10B","T11A","T11B","T12A","T12B",
  "T13A","T13B","T14A","T14B",
  "S01A","S01B","S02A","S02B","S03A","S03B","S04A","S04B","S05A","S05B","S06A","S06B",
  "S07A","S07B","S08A","S08B","S09A","S09B","S10A","S10B","S11A","S11B","S12A","S12B",
  "C02A","C02B","C03A","C03B","C04A","C04B","C05A","C05B","C06A","C06B","C07A","C07B",
  "C08A","C08B"
)

# 1) TEST RUN (não altera nada; cria relatórios)
process_videos(
  csv_path, base_dir, dest_dir, selected_traps,
  action = "test",
  report_dir = file.path(dest_dir, "_relatorios_test_run")
)

# 2) Execução real
# process_videos(csv_path, base_dir, dest_dir, selected_traps,
#                action = "copy", overwrite = FALSE,
#                report_dir = file.path(dest_dir, "_relatorios_copy"))
# OU
# process_videos(csv_path, base_dir, dest_dir, selected_traps,
#                action = "move", overwrite = FALSE,
#                report_dir = file.path(dest_dir, "_relatorios_move"))
```

Saídas

 - plan.csv (TEST RUN): mapeia src_file → dest_file.

- Relatórios: missing.csv, duplicates_in_sheet.csv, destination_conflicts.csv, species_counts_test_run.csv.

- Em copy/move: copied_plan.csv ou moved_plan.csv.

#### 2) Pós-processamento por estação — R/station_postprocess.R

- Pré-requisito de pastas

```css
Campanha 02/
  S01/
    A/
    B/
  S01 - Addax/
    results_S01A.xlsx
    results_S01B.xlsx
  S01 - Manual_check/
    A/
    B/
  ...
```
#### O que faz?

- Para cada Sxx e lado A/B, lê detections e files nas planilhas results_SxxA/B.xlsx.
- Move para Sxx - Manual_check/<A|B>:
  - pessoa/veículo com confiança < 0.80
  - vídeos sem detecção

- Exclui:
  - pessoa/veículo com confiança ≥ 0.80

- Tolera relative_path com subpastas; se não achar, tenta o basename().
- dry_run = TRUE para simular.

#### Exemplo de uso
```r
source("R/station_postprocess.R")

# Edite no topo do script:
# root_path <- "C:/Users/roger/OneDrive/Monitoramento_PET_Campanhas/Campanha 02"
# dry_run   <- TRUE

# Executar:
# source("R/station_postprocess.R")
```
#### Relatórios impressos

- Sumário geral (movidos / excluídos) e detalhes por estação.

- Marca “Não movido/Não excluído (origem inexistente)” quando o arquivo esperado não está na origem.

## 📥 Dados de Entrada

#### process_videos.R:
- CSV com colunas:

  - Nome do Arquivo (número base; será str_pad(..., width=8)),

  - Ponto ID (trap: TxxA, SxxB, CxxA…),

  - Nome Comum (usado para nome da pasta de espécie).

#### station_postprocess.R:
- Excel results_SxxA.xlsx/results_SxxB.xlsx com abas:

  - detections: colunas label (person/vehicle), confidence (numérico), relative_path

  - files: colunas n_detections (numérico), relative_path


## 📤 Dados de Saída

- Pastas por espécie (sanitizadas) dentro de dest_dir.

- Arquivos nomeados como <PontoID>_<file_base>.<ext> (preferência .mp4, fallback .avi).

- Relatórios .csv (TEST RUN / execução) para auditoria.

## ✅ Resultados

- Ao término do TEST RUN, você terá um plano completo e contagens por espécie (totais vs únicos).
- Após copy/move, os arquivos estarão organizados em dest_dir/<NOME_COMUM>/... com os nomes padronizados.

## ⚙️ Configurações Avançadas

- action = c("test","copy","move") — simular / copiar / mover.

- exts = c("mp4","avi") — ordem de preferência de extensões.

- overwrite = FALSE — não sobrescreve no destino (gera conflicts se já existir).

- Matching por trap + nome evita ambiguidade quando o mesmo número aparece em traps diferentes.

- Deduplicação por trap + file_base garante uma única cópia por vídeo.

## 🤝 Contribuições

- Sinta-se livre para abrir issues e pull requests com melhorias, correções e novas ideias.
- Sugestões são bem-vindas!

## 📄 Licença

- Este projeto está sob a licença MIT (veja LICENSE).

## 👤 Autor

Rogério Nunes Oliveira

Script desenvolvido como parte de pesquisa de doutorado

Data de criação: Junho/2025

## 📚 Citação

Se você usar este código em sua pesquisa, por favor cite:

```bibtex
@software{oliveira2025armadilhas,
  author = {Oliveira, Rogério Nunes},
  title  = {Organização e Pós-processamento de Vídeos de Armadilhas Fotográficas},
  year   = {2025},
  url    = {https://github.com/rogerio-onza/monitoramento-pet-scripts}
}
```
