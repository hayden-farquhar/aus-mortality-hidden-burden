# =============================================================================
# 01_inspect_data.R
# Purpose: Inspect all downloaded ABS and AIHW data files
# Run this first, then share the console output back so we can plan cleaning
# =============================================================================

# Install packages if needed (only runs once)
if (!require("readxl"))   install.packages("readxl", repos = "https://cran.r-project.org")
if (!require("tidyverse")) install.packages("tidyverse", repos = "https://cran.r-project.org")

library(readxl)
library(tidyverse)

# --- SET YOUR BASE PATH HERE ---
# This should point to your 'data' folder
base_path <- "data"  # assumes your RStudio project is in the 'Analysis' folder

# If that doesn't work, uncomment and use the full path:
# base_path <- "data"  # (alternative: already set above)

# =============================================================================
# 1. List all files
# =============================================================================

cat("===========================================================\n")
cat("LISTING ALL DATA FILES\n")
cat("===========================================================\n\n")

abs_files <- list.files(file.path(base_path, "abs_cod"), pattern = "\\.xlsx$", full.names = TRUE)
diag_files <- list.files(file.path(base_path, "aihw_diagnosis"), pattern = "\\.xlsx$", full.names = TRUE)
proc_files <- list.files(file.path(base_path, "aihw_procedures"), pattern = "\\.xlsx$", full.names = TRUE)

cat("ABS Causes of Death files:", length(abs_files), "\n")
for (f in abs_files) cat("  ", basename(f), "\n")

cat("\nAIHW Diagnosis files:", length(diag_files), "\n")
for (f in diag_files) cat("  ", basename(f), "\n")

cat("\nAIHW Procedures files:", length(proc_files), "\n")
for (f in proc_files) cat("  ", basename(f), "\n")

# =============================================================================
# 2. Inspect ABS Causes of Death files — sheet names and first rows
# =============================================================================

cat("\n\n===========================================================\n")
cat("ABS CAUSES OF DEATH — SHEET STRUCTURES\n")
cat("===========================================================\n")

for (f in abs_files) {
  cat("\n-----------------------------------------------------------\n")
  cat("FILE:", basename(f), "\n")
  cat("-----------------------------------------------------------\n")

  sheets <- excel_sheets(f)
  cat("Sheets:", length(sheets), "\n")
  for (s in sheets) cat("  ", s, "\n")

  # Peek at the first data sheet (skip any sheet called "Contents" or "Notes")
  data_sheets <- sheets[!grepl("content|note|info|about|source", sheets, ignore.case = TRUE)]

  if (length(data_sheets) > 0) {
    # Look at the first data sheet
    s <- data_sheets[1]
    cat("\nPeeking at sheet:", s, "\n")
    tryCatch({
      # Read without col_names to see the raw structure (ABS files often have header rows)
      peek <- read_excel(f, sheet = s, col_names = FALSE, n_max = 10, .name_repair = "minimal")
      cat("  Dimensions:", nrow(peek), "x", ncol(peek), "(first 10 rows)\n")
      cat("  First few cells of each row:\n")
      for (i in 1:min(nrow(peek), 8)) {
        row_vals <- as.character(peek[i, 1:min(ncol(peek), 5)])
        row_vals[is.na(row_vals)] <- "NA"
        cat("    Row", i, ":", paste(row_vals, collapse = " | "), "\n")
      }
    }, error = function(e) {
      cat("  ERROR reading sheet:", e$message, "\n")
    })
  }
}

# =============================================================================
# 3. Inspect the MULTIPLE CAUSES file in detail (this is our key file)
# =============================================================================

cat("\n\n===========================================================\n")
cat("DETAILED LOOK: MULTIPLE CAUSES OF DEATH (Data Cube 10)\n")
cat("===========================================================\n")

mcod_file <- abs_files[grepl("Multiple", abs_files)]
if (length(mcod_file) > 0) {
  sheets <- excel_sheets(mcod_file[1])
  cat("File:", basename(mcod_file[1]), "\n")
  cat("All sheets:\n")
  for (s in sheets) cat("  ", s, "\n")

  # Peek at each sheet
  for (s in sheets) {
    if (grepl("content|note|info|about|source", s, ignore.case = TRUE)) next
    cat("\n  Sheet:", s, "\n")
    tryCatch({
      peek <- read_excel(mcod_file[1], sheet = s, col_names = FALSE, n_max = 12, .name_repair = "minimal")
      cat("    Dimensions:", nrow(peek), "x", ncol(peek), "(first 12 rows)\n")
      for (i in 1:min(nrow(peek), 10)) {
        row_vals <- as.character(peek[i, 1:min(ncol(peek), 6)])
        row_vals[is.na(row_vals)] <- "NA"
        cat("      Row", i, ":", paste(row_vals, collapse = " | "), "\n")
      }
    }, error = function(e) {
      cat("    ERROR:", e$message, "\n")
    })
  }
} else {
  cat("Multiple causes file not found — check file names.\n")
}

# =============================================================================
# 4. Inspect one AIHW Procedures file
# =============================================================================

cat("\n\n===========================================================\n")
cat("AIHW PROCEDURES — SAMPLE FILE STRUCTURE\n")
cat("===========================================================\n")

if (length(proc_files) > 0) {
  f <- proc_files[length(proc_files)]  # most recent file
  cat("File:", basename(f), "\n")

  sheets <- excel_sheets(f)
  cat("Sheets:", length(sheets), "\n")
  for (s in sheets) cat("  ", s, "\n")

  # Peek at each sheet
  for (s in sheets) {
    cat("\n  Sheet:", s, "\n")
    tryCatch({
      peek <- read_excel(f, sheet = s, col_names = FALSE, n_max = 10, .name_repair = "minimal")
      cat("    Dimensions:", nrow(peek), "x", ncol(peek), "(first 10 rows)\n")
      for (i in 1:min(nrow(peek), 6)) {
        row_vals <- as.character(peek[i, 1:min(ncol(peek), 6)])
        row_vals[is.na(row_vals)] <- "NA"
        cat("      Row", i, ":", paste(row_vals, collapse = " | "), "\n")
      }
    }, error = function(e) {
      cat("    ERROR:", e$message, "\n")
    })
  }
}

# =============================================================================
# 5. Inspect one AIHW Principal Diagnosis file
# =============================================================================

cat("\n\n===========================================================\n")
cat("AIHW PRINCIPAL DIAGNOSIS — SAMPLE FILE STRUCTURE\n")
cat("===========================================================\n")

if (length(diag_files) > 0) {
  f <- diag_files[length(diag_files)]  # most recent file
  cat("File:", basename(f), "\n")

  sheets <- excel_sheets(f)
  cat("Sheets:", length(sheets), "\n")
  for (s in sheets) cat("  ", s, "\n")

  # Peek at each sheet
  for (s in sheets[1:min(length(sheets), 3)]) {
    cat("\n  Sheet:", s, "\n")
    tryCatch({
      peek <- read_excel(f, sheet = s, col_names = FALSE, n_max = 10, .name_repair = "minimal")
      cat("    Dimensions:", nrow(peek), "x", ncol(peek), "(first 10 rows)\n")
      for (i in 1:min(nrow(peek), 6)) {
        row_vals <- as.character(peek[i, 1:min(ncol(peek), 6)])
        row_vals[is.na(row_vals)] <- "NA"
        cat("      Row", i, ":", paste(row_vals, collapse = " | "), "\n")
      }
    }, error = function(e) {
      cat("    ERROR:", e$message, "\n")
    })
  }
}

cat("\n\n===========================================================\n")
cat("INSPECTION COMPLETE\n")
cat("Copy all output above and share it back.\n")
cat("===========================================================\n")
