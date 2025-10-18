# --- CONFIGURAÇÃO INICIAL ---

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(fs)
  library(stringr)
  library(tidyr)
  library(tibble)
  library(purrr)
})

# Use "/" mesmo no Windows
root_path <- "C:/Users/roger/OneDrive/Monitoramento_PET_Campanhas/Campanha 02"

# DRY RUN: TRUE = não move/deleta, só simula
dry_run <- TRUE  # recomendo TRUE no primeiro run

# Estações a processar (S01..S08, S10..S11)
stations <- paste0("S", sprintf("%02d", c(1:8, 10:11)))

# --- INÍCIO DO SCRIPT ---

cat("\n🚀 Iniciando o script de organização de vídeos de armadilhas fotográficas...\n")
if (dry_run) {
  cat("🟠 MODO DE SEGURANÇA (DRY RUN) ATIVADO. Nenhuma alteração será feita nos arquivos.\n\n")
} else {
  cat("🔴 MODO DE EXECUÇÃO REAL ATIVADO. Arquivos serão movidos e deletados.\n\n")
}

# Dataframe de log (armazenado via lista e depois combinado → mais rápido)
report_rows <- list()

# helpers
push_log <- function(station_id, side, action, reason, file_name) {
  report_rows[[length(report_rows) + 1]] <<- tibble(
    station_id = station_id,
    side = side,
    action = action,
    reason = reason,
    file_name = file_name
  )
}

sheet_exists <- function(path, sheet) {
  tolower(sheet) %in% tolower(readxl::excel_sheets(path))
}

# Loop principal
for (station_id in stations) {

  cat("=========================================================\n")
  cat(paste0("▶️  Estação: ", station_id, "\n"))

  # loop A/B
  for (side in c("A", "B")) {
    cat(paste0("\n-- Lado: ", side, " --\n"))

    # nomes/paths
    excel_file_name <- paste0("results_", station_id, side, ".xlsx")
    addax_candidates <- list.files(root_path, pattern = paste0("^", station_id, " - Addax$"), full.names = FALSE)
    if (length(addax_candidates) == 0) {
      cat("   ⚠️  Pasta '", station_id, " - Addax' não encontrada. Pulando lado ", side, ".\n", sep = "")
      next
    }
    if (length(addax_candidates) > 1) {
      cat("   ⚠️  Múltiplas pastas '", station_id, " - Addax' encontradas: ",
          paste(addax_candidates, collapse = ", "), ". Usando a primeira.\n", sep = "")
    }
    addax_folder_name <- addax_candidates[[1]]

    excel_path        <- file.path(root_path, addax_folder_name, excel_file_name)
    video_source_path <- file.path(root_path, station_id, side)
    manual_check_path <- file.path(root_path, paste0(station_id, " - Manual_check"), side)

    if (!file_exists(excel_path)) {
      cat("   ⚠️  Planilha '", excel_file_name, "' não encontrada. Pulando lado ", side, ".\n", sep = "")
      next
    }
    cat("   ✔️  Planilha encontrada: ", excel_file_name, "\n", sep = "")

    # valida abas
    if (!sheet_exists(excel_path, "detections") || !sheet_exists(excel_path, "files")) {
      cat("   ⚠️  A planilha não contém abas 'detections' e/ou 'files'. Pulando lado ", side, ".\n", sep = "")
      next
    }

    detections_df <- read_excel(excel_path, sheet = "detections", .name_repair = "unique")
    files_df      <- read_excel(excel_path, sheet = "files",      .name_repair = "unique")

    # normaliza colunas que podem vir com NA
    if (!"label" %in% names(detections_df)) detections_df$label <- NA_character_
    if (!"confidence" %in% names(detections_df)) detections_df$confidence <- NA_real_
    if (!"relative_path" %in% names(detections_df)) detections_df$relative_path <- NA_character_
    if (!"n_detections" %in% names(files_df)) files_df$n_detections <- NA_real_
    if (!"relative_path" %in% names(files_df)) files_df$relative_path <- NA_character_

    detections_df <- detections_df %>%
      mutate(confidence = coalesce(as.numeric(confidence), 0),
             label = as.character(label),
             relative_path = as.character(relative_path))

    files_df <- files_df %>%
      mutate(n_detections = coalesce(as.numeric(n_detections), 0),
             relative_path = as.character(relative_path))

    # LÓGICA: detections
    detections_to_check        <- detections_df %>% filter(label %in% c("person", "vehicle"))
    files_to_delete            <- detections_to_check %>% filter(confidence >= 0.80) %>% pull(relative_path) %>% unique()
    files_to_move_low_conf     <- detections_to_check %>% filter(confidence < 0.80) %>% pull(relative_path) %>% unique()

    # LÓGICA: files
    files_to_move_no_detection <- files_df %>% filter(n_detections == 0) %>% pull(relative_path) %>% unique()

    # COMBINAR MOVES
    all_files_to_move <- unique(c(files_to_move_low_conf, files_to_move_no_detection))

    # --- mover arquivos para manual_check ---
    if (length(all_files_to_move) > 0) {
      if (!dry_run) dir_create(manual_check_path, recurse = TRUE)

      for (file_rel_path in all_files_to_move) {
        # tenta com subpastas; se não achar, cai para basename
        candidate1 <- file.path(video_source_path, file_rel_path)
        candidate2 <- file.path(video_source_path, basename(file_rel_path))
        source_file <- if (file_exists(candidate1)) candidate1 else candidate2
        dest_file   <- file.path(manual_check_path, basename(source_file))

        reason_move <- ifelse(file_rel_path %in% files_to_move_no_detection,
                              "Nenhuma Detecção", "Baixa Confiança")

        if (!file_exists(source_file)) {
          cat("   ⚠️  NÃO ENCONTRADO (para mover): ", basename(source_file), "\n", sep = "")
          push_log(station_id, side, "Não movido", "Origem inexistente", basename(file_rel_path))
          next
        }

        cat("   ➡️  Movendo (", reason_move, "): ", basename(source_file), "\n", sep = "")
        if (!dry_run) {
          dir_create(dirname(dest_file), recurse = TRUE)
          file_move(source_file, dest_file)
        }
        push_log(station_id, side, "Movido", reason_move, basename(file_rel_path))
      }
    } else {
      cat("   ℹ️  Nenhum arquivo para mover para verificação manual neste lado.\n")
    }

    # --- deletar arquivos (alta confiança de pessoa/veículo) ---
    if (length(files_to_delete) > 0) {
      for (file_rel_path in files_to_delete) {
        candidate1 <- file.path(video_source_path, file_rel_path)
        candidate2 <- file.path(video_source_path, basename(file_rel_path))
        source_file <- if (file_exists(candidate1)) candidate1 else candidate2

        if (!file_exists(source_file)) {
          cat("   ⚠️  NÃO ENCONTRADO (para excluir): ", basename(source_file), "\n", sep = "")
          push_log(station_id, side, "Não excluído", "Origem inexistente", basename(file_rel_path))
          next
        }

        cat("   ❌  Excluindo arquivo: ", basename(source_file), "\n", sep = "")
        if (!dry_run) file_delete(source_file)
        push_log(station_id, side, "Excluído", "Confiança >= 0.80", basename(file_rel_path))
      }
    } else {
      cat("   ℹ️  Nenhum arquivo para excluir neste lado.\n")
    }
  }
}

# --- GERAÇÃO DO RELATÓRIO FINAL ---

cat("\n=========================================================\n")
cat("📊 RELATÓRIO FINAL DA OPERAÇÃO\n")
cat("=========================================================\n")

report_data <- if (length(report_rows)) bind_rows(report_rows) else
  tibble(station_id = character(), side = character(), action = character(),
         reason = character(), file_name = character())

if (nrow(report_data) == 0) {
  cat("Nenhuma ação de exclusão ou movimentação foi registrada.\n")
} else {
  # 1) SUMÁRIO GERAL
  summary_total <- report_data %>% count(action, name = "total")
  total_movido   <- summary_total %>% filter(action == "Movido") %>% pull(total) %>% { if (length(.)==0) 0 else . }
  total_excluido <- summary_total %>% filter(action == "Excluído") %>% pull(total) %>% { if (length(.)==0) 0 else . }

  cat("--- SUMÁRIO GERAL ---\n")
  cat(sprintf("➡️  Total de arquivos movidos para verificação manual: %d\n", total_movido))
  cat(sprintf("❌  Total de arquivos excluídos: %d\n\n", total_excluido))

  # 2) DETALHES POR ESTAÇÃO
  cat("--- DETALHES POR ESTAÇÃO ---\n")
  summary_by_station <- report_data %>%
    count(station_id, action, name = "count")

  for (station in stations) {
    station_summary <- summary_by_station %>% filter(station_id == station)
    if (nrow(station_summary) > 0) {
      cat(station, ":\n", sep = "")
      n_movidos   <- station_summary %>% filter(action == "Movido")       %>% pull(count) %>% { if (length(.)==0) 0 else . }
      n_excluidos <- station_summary %>% filter(action == "Excluído")     %>% pull(count) %>% { if (length(.)==0) 0 else . }
      n_nao_mov   <- station_summary %>% filter(action == "Não movido")   %>% pull(count) %>% { if (length(.)==0) 0 else . }
      n_nao_exc   <- station_summary %>% filter(action == "Não excluído") %>% pull(count) %>% { if (length(.)==0) 0 else . }

      cat(sprintf("  - Movidos: %d | Excluídos: %d | Não movidos (origem ausente): %d | Não excluídos (origem ausente): %d\n",
                  n_movidos, n_excluidos, n_nao_mov, n_nao_exc))
    }
  }
}

cat("=========================================================\n")
cat("✅ Script finalizado.\n")
