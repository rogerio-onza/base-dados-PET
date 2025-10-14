# =============================== #
# Separar vídeos por espécie e trap — v0.3.3
# - Indexa por (trap, filename) -> sem ambiguidade entre traps
# - DRY-RUN / COPY / MOVE
# - Resumo por espécie: totais (todas as linhas localizadas) vs únicos (1 por trap+nome)
# =============================== #

process_videos <- function(
    csv_path,
    base_dir,
    dest_dir,
    selected_traps,
    exts        = c("mp4","avi"),     # ordem de preferência
    action      = c("test","copy","move"),
    overwrite   = FALSE,              # TRUE permite sobrescrever destino
    report_dir  = NULL,               # se definido, salva CSVs de relatório
    stop_if_missing   = TRUE,         # aborta se houver faltantes (copy/move)
    stop_if_conflicts = TRUE,         # aborta se houver conflitos (copy/move)
    verbose     = TRUE
) {
  # ---- pacotes ----
  suppressPackageStartupMessages({
    library(readr); library(dplyr); library(stringr); library(tidyr); library(tibble); library(purrr)
  })
  action <- match.arg(action)
  
  # ---- helpers ----
  msg_info <- function(...) cat("[INFO] ", ..., "\n", sep = "")
  msg_warn <- function(...) cat("[WARN] ", ..., "\n", sep = "")
  msg_ok   <- function(...) cat("[OK]   ", ..., "\n", sep = "")
  
  sanitize_fs <- function(x){
    x |>
      str_replace_all("[\\\\/:*?\"<>|]", "_") |>
      str_replace_all("\\s+", " ") |>
      str_trim()
  }
  
  # lista arquivos de UMA TRAP e devolve tibble (trap, full_path, name_upper)
  list_trap_index <- function(base_dir, trap, exts){
    pat <- paste0("\\.(", paste0(exts, collapse="|"), ")$")
    # (1) base_dir/TRAP
    p1 <- file.path(base_dir, trap)
    # (2) base_dir/prefixo/sufixo (ex.: C02/A)
    prefix <- substr(trap, 1, nchar(trap)-1)
    suffix <- substr(trap, nchar(trap), nchar(trap))
    p2 <- file.path(base_dir, prefix, suffix)
    paths <- unique(c(p1, p2)); paths <- paths[dir.exists(paths)]
    if (!length(paths)) return(tibble(trap = character(), full_path = character(), name_upper = character()))
    files <- unlist(lapply(paths, function(p){
      list.files(p, pattern = pat, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    }), use.names = FALSE)
    if (!length(files)) return(tibble(trap = character(), full_path = character(), name_upper = character()))
    tibble(trap = trap, full_path = files, name_upper = toupper(basename(files)))
  }
  
  make_key <- function(trap, name) paste0(trap, "||", name)
  
  # ---- lê CSV e prepara lookups ----
  if (verbose) msg_info("Lendo planilha e preparando lookups...")
  df <- read_csv(csv_path, col_types = cols(
    `Nome do Arquivo` = col_character(),
    `Ponto ID`        = col_character(),
    `Nome Comum`      = col_character()
  )) |>
    filter(`Ponto ID` %in% selected_traps) |>
    mutate(file_base = str_pad(`Nome do Arquivo`, width = 8, side = "left", pad = "0"))
  
  # colunas de lookup para cada extensão
  for (ext in tolower(exts)) {
    df[[paste0("lookup_", ext)]] <- toupper(paste0(df$file_base, ".", toupper(ext)))
  }
  
  # ---- índice de arquivos por (trap, filename) ----
  if (verbose) msg_info("Indexando arquivos de vídeo no disco (por TRAP)...")
  idx_list   <- lapply(unique(selected_traps), list_trap_index, base_dir = base_dir, exts = tolower(exts))
  file_index <- bind_rows(idx_list)
  if (!nrow(file_index)) stop("Nenhum arquivo .mp4 ou .avi encontrado nas pastas das traps.")
  
  # ambiguidade agora checada por (trap, name_upper)
  ambig <- file_index |> add_count(trap, name_upper, name = "hits") |> filter(hits > 1)
  if (nrow(ambig) > 0) {
    msg_warn("Mesmo nome de arquivo aparece em múltiplos locais DENTRO da mesma TRAP (usarei o 1º encontrado dessa TRAP).")
    amb_tbl <- file_index |> semi_join(ambig, by = c("trap","name_upper")) |> arrange(trap, name_upper)
    if (!is.null(report_dir)) {
      dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)
      write_csv(amb_tbl, file.path(report_dir, "ambiguous_sources_within_trap.csv"))
    }
    if (verbose) print(amb_tbl, n = min(20, nrow(amb_tbl)))
  }
  
  # reduz para primeira ocorrência por (trap, name_upper)
  tmp_map  <- file_index |> distinct(trap, name_upper, .keep_all = TRUE)
  index_map <- setNames(tmp_map$full_path, make_key(tmp_map$trap, tmp_map$name_upper))
  
  # ---- associa cada linha ao arquivo encontrado respeitando a TRAP ----
  for (ext in tolower(exts)) {
    key_vec <- make_key(df$`Ponto ID`, df[[paste0("lookup_", ext)]])
    df[[paste0("src_", ext)]] <- index_map[key_vec]
  }
  # primeira não-NA na ordem de preferência
  df$src_file <- NA_character_
  for (ext in tolower(exts)) {
    vec <- df[[paste0("src_", ext)]]
    idx <- is.na(df$src_file) & !is.na(vec)
    df$src_file[idx] <- vec[idx]
  }
  # extensão usada
  df$ext_used <- ifelse(is.na(df$src_file),
                        NA_character_,
                        paste0(".", tolower(tools::file_ext(df$src_file))))
  
  # ---- faltantes & duplicatas na planilha ----
  pre_missing <- df |>
    filter(is.na(src_file) | !file.exists(src_file)) |>
    select(`Ponto ID`, file_base, starts_with("lookup_")) |>
    distinct()
  
  dup_counts <- df |>
    count(`Ponto ID`, file_base, name = "times") |>
    filter(times > 1)
  
  # ---- destino (sem tocar no disco) ----
  df_plan <- df |>
    distinct(`Ponto ID`, file_base, .keep_all = TRUE) |>
    mutate(
      species_folder = sanitize_fs(`Nome Comum`),
      target_folder  = file.path(dest_dir, species_folder),
      dest_file      = file.path(target_folder, paste0(`Ponto ID`, "_", file_base, ext_used))
    )
  
  # conflitos no destino
  dest_conflicts <- df_plan |>
    mutate(exists = file.exists(dest_file)) |>
    filter(exists & !overwrite) |>
    select(`Ponto ID`, file_base, dest_file)
  
  # ---- DRY-RUN ----
  if (action == "test") {
    msg_ok("TEST RUN (nenhum arquivo será copiado/movido).")
    cat("\nResumo:\n", sep = "")
    cat(sprintf("  • Registros (traps selecionadas): %d\n", nrow(df)))
    cat(sprintf("  • Registros únicos (por trap + nome): %d\n", nrow(df_plan)))
    cat(sprintf("  • Arquivos localizados: %d\n", sum(!is.na(df$src_file))))
    cat(sprintf("  • Faltantes: %d\n", nrow(pre_missing)))
    cat(sprintf("  • Entradas duplicadas na planilha: %d\n", nrow(dup_counts)))
    cat(sprintf("  • Conflitos no destino (overwrite=FALSE): %d\n", nrow(dest_conflicts)))
    
    if (nrow(pre_missing) > 0) {
      cat("\nVídeos não encontrados (verifique numeração/planilha):\n", sep = "")
      # monta string dos lookups de forma robusta
      if (any(grepl("^lookup_", names(pre_missing)))) {
        lookup_cols <- grep("^lookup_", names(pre_missing), value = TRUE)
        pre_missing <- pre_missing |>
          unite("lookups", all_of(lookup_cols), sep = " / ", remove = FALSE, na.rm = TRUE)
      } else {
        pre_missing$lookups <- NA_character_
      }
      pre_missing |>
        arrange(`Ponto ID`, file_base) |>
        mutate(.msg = paste0(`Ponto ID`, ": ", file_base, " (", lookups, ")")) |>
        pull(.msg) |>
        (\(v) { cat(paste0("  • ", v, "\n"), sep = "") })()
    }
    
    if (nrow(dup_counts) > 0) {
      cat("\nEntradas duplicadas na planilha:\n", sep = "")
      dup_counts |>
        arrange(`Ponto ID`, file_base) |>
        mutate(.msg = paste0(`Ponto ID`, ": ", file_base, " — ", times, " vezes")) |>
        pull(.msg) |>
        (\(v) { cat(paste0("  • ", v, "\n"), sep = "") })()
    }
    
    # --- Resumo por espécie (totais vs únicos) ---
    cat("\nResumo por espécie (totais vs únicos):\n", sep = "")
    plan_counts  <- df_plan |> count(`Nome Comum`, name = "n_unique")
    total_counts <- df |> filter(!is.na(src_file)) |> count(`Nome Comum`, name = "n_total")
    
    summary_counts <- total_counts |>
      full_join(plan_counts, by = "Nome Comum") |>
      mutate(across(c(n_total, n_unique), ~replace_na(., 0L))) |>
      arrange(desc(n_total), desc(n_unique))
    
    for (i in seq_len(nrow(summary_counts))) {
      cat(sprintf("  • %s: %d totais | %d únicos\n",
                  summary_counts$`Nome Comum`[i],
                  summary_counts$n_total[i],
                  summary_counts$n_unique[i]))
    }
    
    if (!is.null(report_dir)) {
      dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)
      write_csv(df_plan,        file.path(report_dir, "plan.csv"))
      write_csv(pre_missing,    file.path(report_dir, "missing.csv"))
      write_csv(dup_counts,     file.path(report_dir, "duplicates_in_sheet.csv"))
      write_csv(dest_conflicts, file.path(report_dir, "destination_conflicts.csv"))
      write_csv(summary_counts, file.path(report_dir, "species_counts_test_run.csv"))
      if (nrow(ambig) > 0) {
        amb_tbl <- file_index |> semi_join(ambig, by = c("trap","name_upper")) |> arrange(trap, name_upper)
        write_csv(amb_tbl, file.path(report_dir, "ambiguous_sources_within_trap.csv"))
      }
      msg_ok(sprintf("Relatórios salvos em: %s", normalizePath(report_dir)))
    }
    
    msg_ok("Fim do TEST RUN. Rode com action='copy' ou 'move' quando estiver tudo certo.")
    return(invisible(list(plan = df_plan, missing = pre_missing,
                          duplicates = dup_counts, conflicts = dest_conflicts,
                          species_counts = summary_counts)))
  }
  
  # ---- em modo copy/move, aborta diante de problemas (se configurado) ----
  if (stop_if_missing && nrow(pre_missing) > 0) {
    stop("Existem vídeos faltantes. Rode primeiro com action='test' e corrija a planilha.")
  }
  if (!overwrite && stop_if_conflicts && nrow(dest_conflicts) > 0) {
    stop("Existem conflitos no destino (arquivos já existem). Ajuste overwrite=TRUE ou limpe o destino.")
  }
  
  # ---- execução (cópia ou move) ----
  msg_ok(sprintf("Iniciando %s de vídeos únicos...",
                 if (action == "copy") "cópia" else "movimentação"))
  
  # garante pastas de espécie
  dirs_to_make <- unique(df_plan$target_folder)
  invisible(lapply(dirs_to_make, function(p) if (!dir.exists(p)) dir.create(p, recursive = TRUE)))
  
  # contadores por espécie
  sp_levels  <- unique(df_plan$`Nome Comum`)
  copy_count <- setNames(integer(length(sp_levels)), sp_levels)
  
  move_file <- function(src, dest, overwrite){
    if (overwrite && file.exists(dest)) unlink(dest)
    ok <- suppressWarnings(file.rename(src, dest))
    if (!ok) { ok <- file.copy(src, dest, overwrite = overwrite); if (ok) unlink(src) }
    ok
  }
  
  for (i in seq_len(nrow(df_plan))) {
    src  <- df_plan$src_file[i]
    dest <- df_plan$dest_file[i]
    sp   <- df_plan$`Nome Comum`[i]
    if (is.na(src) || !file.exists(src)) next
    ok <- if (action == "copy") file.copy(src, dest, overwrite = overwrite) else move_file(src, dest, overwrite)
    if (ok) copy_count[sp] <- copy_count[sp] + 1
  }
  
  cat("Arquivos processados por espécie:\n", sep = "")
  for (sp in names(copy_count)) {
    cat(sprintf("  • %d para %s\n", copy_count[sp], sp))
  }
  
  # --- Resumo por espécie (totais vs únicos) após a execução ---
  cat("\nResumo por espécie (totais vs únicos):\n", sep = "")
  plan_counts  <- df_plan |> count(`Nome Comum`, name = "n_unique")
  total_counts <- df |> filter(!is.na(src_file)) |> count(`Nome Comum`, name = "n_total")
  
  summary_counts <- total_counts |>
    full_join(plan_counts, by = "Nome Comum") |>
    mutate(across(c(n_total, n_unique), ~replace_na(., 0L))) |>
    arrange(desc(n_total), desc(n_unique))
  
  for (i in seq_len(nrow(summary_counts))) {
    cat(sprintf("  • %s: %d totais | %d únicos\n",
                summary_counts$`Nome Comum`[i],
                summary_counts$n_total[i],
                summary_counts$n_unique[i]))
  }
  
  msg_ok(paste0("Processo concluído para armadilhas: ",
                paste(unique(df_plan$`Ponto ID`), collapse = ", ")))
  
  if (!is.null(report_dir)) {
    dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)
    write_csv(df_plan, file.path(report_dir, if (action=="copy") "copied_plan.csv" else "moved_plan.csv"))
    write_csv(summary_counts, file.path(report_dir,
                                        if (action=="copy") "species_counts_after_copy.csv" else "species_counts_after_move.csv"))
  }
  
  invisible(list(done = copy_count, plan = df_plan, species_counts = summary_counts))
}


# =============================== #
# EXEMPLO DE USO — Campanha 03
# =============================== #

base_dir  <- c("Campanha 1/","Campanha 02/","Campanha 03/")
dest_dir  <- "Campanhas - Identificados/"
csv_path  <- "monitoramento_turvo_rogerio_2021_2022.csv"
selected_traps <- c(
  "T01A","T01B","T02A","T02B","T03A","T03B","T04A","T04B","T05A","T05B","T06A","T06B",
  "T07A","T07B","T08A","T08B","T09A","T09B","T10A","T10B","T11A","T11B","T12A","T12B",
  "T13A","T13B","T14A","T14B",
  "S01A","S01B","S02A","S02B","S03A","S03B","S04A","S04B","S05A","S05B","S06A","S06B",
  "S07A","S07B","S08A","S08B","S09A","S09B","S10A","S10B","S11A","S11B","S12A","S12B",
  "C02A","C02B","C03A","C03B","C04A","C04B","C05A","C05B","C06A","C06B","C07A","C07B",
  "C08A","C08B"
)

# 1) Teste (não altera nada):
process_videos(
  csv_path, base_dir, dest_dir, selected_traps,
  action = "copy",
  report_dir = file.path(dest_dir, "_relatorios_test_run")
)

# 2) Depois rode de fato:
# process_videos(csv_path, base_dir, dest_dir, selected_traps, action = "copy", overwrite = FALSE)
# process_videos(csv_path, base_dir, dest_dir, selected_traps, action = "move", overwrite = FALSE)
