# Verifica se os arquivos planejados foram copiados/movidos
# Use com: plan.csv (TEST RUN), copied_plan.csv ou moved_plan.csv

verify_copies <- function(plan_csv, dest_root = NULL, check_size = TRUE, report = TRUE) {
  suppressPackageStartupMessages({
    library(readr); library(dplyr); library(stringr); library(tibble)
  })
  stopifnot(file.exists(plan_csv))
  plan <- readr::read_csv(plan_csv, show_col_types = FALSE)

  # opcional: reescrever raiz do destino
  if (!is.null(dest_root)) {
    plan <- plan %>%
      mutate(dest_file = file.path(dest_root, basename(dirname(dest_file)), basename(dest_file)))
  }

  # 1) presença no destino
  plan <- plan %>% mutate(dest_exists = file.exists(dest_file))
  missing <- plan %>% filter(!dest_exists)

  # 2) checagem de tamanho (opcional)
  size_mismatch <- tibble()
  if (check_size) {
    ok_idx <- which(plan$dest_exists & !is.na(plan$src_file) & file.exists(plan$src_file))
    if (length(ok_idx)) {
      finfo_src  <- file.info(plan$src_file[ok_idx])
      finfo_dest <- file.info(plan$dest_file[ok_idx])
      size_mismatch <- tibble(
        `Ponto ID`   = plan$`Ponto ID`[ok_idx],
        `Nome Comum` = plan$`Nome Comum`[ok_idx],
        file_base    = plan$file_base[ok_idx],
        src_file     = plan$src_file[ok_idx],
        dest_file    = plan$dest_file[ok_idx],
        src_bytes    = finfo_src$size,
        dest_bytes   = finfo_dest$size
      ) %>% filter(!is.na(src_bytes) & !is.na(dest_bytes) & src_bytes != dest_bytes)
    }
  }

  # 3) extras no destino
  dest_dir <- unique(dirname(plan$dest_file))
  all_in_dest <- unlist(lapply(unique(dest_dir), function(p) {
    if (dir.exists(p)) list.files(p, recursive = TRUE, full.names = TRUE) else character()
  }), use.names = FALSE)
  expected <- normalizePath(plan$dest_file, winslash = "\\", mustWork = FALSE)
  extras   <- setdiff(normalizePath(all_in_dest, winslash = "\\", mustWork = FALSE),
                      expected[!is.na(expected)])

  # resumo
  cat("\n--- Verificação de cópias/movimentos ---\n")
  cat(sprintf("Total planejado (linhas únicas): %d\n", nrow(plan)))
  cat(sprintf("Encontrados no destino: %d\n", sum(plan$dest_exists)))
  cat(sprintf("Faltantes no destino:  %d\n", nrow(missing)))
  if (check_size) cat(sprintf("Tamanho divergente (src vs dest): %d\n", nrow(size_mismatch)))
  cat(sprintf("Arquivos extras detectados no destino: %d\n", length(extras)))

  if (nrow(missing)) {
    cat("\nExemplos de faltantes (até 10):\n")
    print(missing %>% select(`Ponto ID`, `Nome Comum`, file_base, dest_file) %>% head(10), n = 10)
  }
  if (check_size && nrow(size_mismatch)) {
    cat("\nExemplos de divergência de tamanho (até 10):\n")
    print(size_mismatch %>% head(10), n = 10)
  }
  if (length(extras)) {
    cat("\nExemplos de extras no destino (até 10):\n")
    print(head(extras, 10))
  }

  # relatórios
  out <- list(plan = plan, missing = missing, size_mismatch = size_mismatch, extras = extras)
  if (isTRUE(report)) {
    out_dir <- file.path(dirname(plan_csv), "_verify_reports")
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    readr::write_csv(missing,       file.path(out_dir, "missing_in_destination.csv"))
    if (check_size) readr::write_csv(size_mismatch, file.path(out_dir, "size_mismatch.csv"))
    if (length(extras)) writeLines(extras, con = file.path(out_dir, "extras_in_destination.txt"))
    cat(sprintf("\nRelatórios salvos em: %s\n", normalizePath(out_dir)))
  }

  invisible(out)
}
