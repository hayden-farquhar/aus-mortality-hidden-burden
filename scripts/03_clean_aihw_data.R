# =============================================================================
# 03_clean_aihw_data.R
# Purpose: Clean AIHW Procedures and Principal Diagnosis data cubes
# Inputs:  data/aihw_procedures/*.xlsx, data/aihw_diagnosis/*.xlsx
# Outputs: outputs/hospital_procedures_by_year.csv
#          outputs/hospital_diagnoses_by_year.csv
# =============================================================================

library(readxl)
library(tidyverse)

base_path <- "data"
dir.create("outputs", showWarnings = FALSE)

# =============================================================================
# PART 1: AIHW Procedures Data
# =============================================================================

cat("=== PART 1: Cleaning AIHW Procedures data ===\n\n")

proc_files <- list.files(file.path(base_path, "aihw_procedures"), 
                          pattern = "\\.xlsx$", full.names = TRUE)

read_procedures_cube <- function(filepath) {
  
  fname <- basename(filepath)
  # Extract year from filename like "Procedures-cube-2023-24.xlsx"
  year_str <- str_extract(fname, "\\d{4}-\\d{2}")
  
  cat("  Reading", fname, "...\n")
  
  # Find the data sheet — name varies across years
  sheets <- excel_sheets(filepath)
  data_sheet <- sheets[grepl("Procedure.*Count.*Data|Procedure.*Data", sheets, ignore.case = TRUE)]
  
  if (length(data_sheet) == 0) {
    # Fallback: pick any sheet that isn't "Explanatory" or "Summary"
    data_sheet <- sheets[!grepl("Explanatory|Summary|Notes", sheets, ignore.case = TRUE)]
  }
  
  if (length(data_sheet) == 0) {
    cat("    WARNING: No suitable data sheet found. Sheets:", paste(sheets, collapse = ", "), "\n")
    return(NULL)
  }
  
  data_sheet <- data_sheet[1]
  cat("    Using sheet:", data_sheet, "\n")
  
  # Read — skip header rows (4 rows)
  raw <- read_excel(
    filepath, 
    sheet = data_sheet, 
    col_names = FALSE, 
    skip = 4,
    .name_repair = "minimal"
  )
  
  # First row after skip should be column headers
  # Based on inspection: chapter, sub-chapter, block, code, age group, sex, same-day, count
  headers <- as.character(raw[1, ])
  cat("    Column headers:", paste(headers, collapse = " | "), "\n")
  cat("    Raw rows (excl header):", nrow(raw) - 1, "\n")
  
  # Remove header row
  data <- raw[-1, ]
  
  # Name columns based on what we saw in inspection
  ncols <- ncol(data)
  if (ncols == 8) {
    colnames(data) <- c("proc_chapter", "proc_subchapter", "proc_block", 
                         "proc_code", "age_group", "sex", "same_day_flag", 
                         "separations")
  } else if (ncols == 7) {
    colnames(data) <- c("proc_chapter", "proc_subchapter", "proc_block", 
                         "proc_code", "age_group", "sex", "separations")
  } else {
    cat("    WARNING: Unexpected number of columns:", ncols, "\n")
    colnames(data) <- paste0("col_", 1:ncols)
  }
  
  data <- data %>%
    mutate(
      year = year_str,
      separations = suppressWarnings(as.numeric(separations))
    )
  
  return(data)
}

# Read all procedure files
all_proc <- list()
for (i in seq_along(proc_files)) {
  tryCatch({
    all_proc[[i]] <- read_procedures_cube(proc_files[i])
  }, error = function(e) {
    cat("  ERROR with", basename(proc_files[i]), ":", e$message, "\n")
  })
}

proc_raw <- bind_rows(compact(all_proc))
cat("\n  Combined raw procedures:", format(nrow(proc_raw), big.mark = ","), "rows\n")

# --- Aggregate to chapter level by year (for linking to mortality) ---
cat("\n  Aggregating to chapter level by year...\n")

proc_by_chapter_year <- proc_raw %>%
  filter(!is.na(separations)) %>%
  group_by(year, proc_chapter) %>%
  summarise(
    total_separations = sum(separations, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, desc(total_separations))

cat("  Chapter-year combinations:", nrow(proc_by_chapter_year), "\n")

# Preview
cat("\n  Top procedure chapters (most recent year):\n")
proc_by_chapter_year %>%
  filter(year == max(year)) %>%
  head(10) %>%
  mutate(total_separations = format(total_separations, big.mark = ",")) %>%
  print(n = 10, width = 100)

# Also aggregate by chapter + sex + year
proc_by_chapter_sex_year <- proc_raw %>%
  filter(!is.na(separations)) %>%
  group_by(year, proc_chapter, sex) %>%
  summarise(
    total_separations = sum(separations, na.rm = TRUE),
    .groups = "drop"
  )

# Save both
write_csv(proc_by_chapter_year, "outputs/hospital_procedures_by_year.csv")
write_csv(proc_by_chapter_sex_year, "outputs/hospital_procedures_by_year_sex.csv")
cat("\n  Saved: outputs/hospital_procedures_by_year.csv\n")
cat("  Saved: outputs/hospital_procedures_by_year_sex.csv\n")


# =============================================================================
# PART 2: AIHW Principal Diagnosis Data
# =============================================================================

cat("\n\n=== PART 2: Cleaning AIHW Principal Diagnosis data ===\n\n")

diag_files <- list.files(file.path(base_path, "aihw_diagnosis"), 
                          pattern = "\\.xlsx$", full.names = TRUE)

read_diagnosis_cube <- function(filepath) {
  
  fname <- basename(filepath)
  year_str <- str_extract(fname, "\\d{4}-\\d{2}")
  
  cat("  Reading", fname, "...\n")
  
  # Use the full-year sheet "PDx Counts Data July-June"
  # Some older files might have different sheet names
  sheets <- excel_sheets(filepath)
  
  # Find the full-year data sheet
  full_year_sheet <- sheets[grepl("July-June|July-Jun", sheets) & grepl("Data", sheets)]
  
  if (length(full_year_sheet) == 0) {
    # Try alternative names
    full_year_sheet <- sheets[grepl("Data", sheets)]
    if (length(full_year_sheet) > 1) {
      # Pick the last one (likely full year)
      full_year_sheet <- full_year_sheet[length(full_year_sheet)]
    }
    cat("    Using sheet:", full_year_sheet, "\n")
  } else {
    full_year_sheet <- full_year_sheet[1]
    cat("    Using sheet:", full_year_sheet, "\n")
  }
  
  raw <- read_excel(
    filepath, 
    sheet = full_year_sheet, 
    col_names = FALSE, 
    skip = 4,
    .name_repair = "minimal"
  )
  
  # First row = headers
  headers <- as.character(raw[1, ])
  cat("    Column headers:", paste(headers[1:min(length(headers), 6)], collapse = " | "), "...\n")
  cat("    Raw rows (excl header):", nrow(raw) - 1, "\n")
  
  data <- raw[-1, ]
  
  ncols <- ncol(data)
  if (ncols == 10) {
    colnames(data) <- c("diag_chapter", "diag_subchapter", "diag_3digit", 
                         "diag_4digit", "diag_5digit", "age_group", "sex", 
                         "same_day_flag", "separations", "patient_days")
  } else if (ncols == 9) {
    colnames(data) <- c("diag_chapter", "diag_subchapter", "diag_3digit", 
                         "diag_4digit", "diag_5digit", "age_group", "sex", 
                         "separations", "patient_days")
  } else if (ncols == 8) {
    colnames(data) <- c("diag_chapter", "diag_subchapter", "diag_3digit", 
                         "diag_4digit", "diag_5digit", "age_group", "sex", 
                         "separations")
  } else {
    cat("    WARNING: Unexpected number of columns:", ncols, "\n")
    colnames(data) <- paste0("col_", 1:ncols)
  }
  
  data <- data %>%
    mutate(
      year = year_str,
      separations = suppressWarnings(as.numeric(separations))
    )
  
  if ("patient_days" %in% colnames(data)) {
    data <- data %>%
      mutate(patient_days = suppressWarnings(as.numeric(patient_days)))
  }
  
  return(data)
}

# Read all diagnosis files
all_diag <- list()
for (i in seq_along(diag_files)) {
  tryCatch({
    all_diag[[i]] <- read_diagnosis_cube(diag_files[i])
  }, error = function(e) {
    cat("  ERROR with", basename(diag_files[i]), ":", e$message, "\n")
  })
}

diag_raw <- bind_rows(all_diag)
cat("\n  Combined raw diagnoses:", format(nrow(diag_raw), big.mark = ","), "rows\n")

# --- Aggregate to chapter level by year ---
cat("\n  Aggregating to chapter level by year...\n")

diag_by_chapter_year <- diag_raw %>%
  filter(!is.na(separations)) %>%
  group_by(year, diag_chapter) %>%
  summarise(
    total_separations = sum(separations, na.rm = TRUE),
    total_patient_days = if ("patient_days" %in% names(.)) sum(patient_days, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  arrange(year, desc(total_separations))

cat("  Chapter-year combinations:", nrow(diag_by_chapter_year), "\n")

# Preview
cat("\n  Top diagnosis chapters (most recent year):\n")
diag_by_chapter_year %>%
  filter(year == max(year)) %>%
  head(10) %>%
  mutate(total_separations = format(total_separations, big.mark = ",")) %>%
  print(n = 10, width = 120)

# Save
write_csv(diag_by_chapter_year, "outputs/hospital_diagnoses_by_year.csv")
cat("\n  Saved: outputs/hospital_diagnoses_by_year.csv\n")


# =============================================================================
# PART 3: Create a concordance preview — procedure chapters vs ICD-10 chapters
# =============================================================================

cat("\n\n=== PART 3: Concordance preview ===\n\n")

cat("Procedure chapters available:\n")
proc_by_chapter_year %>%
  filter(year == max(year)) %>%
  distinct(proc_chapter) %>%
  pull(proc_chapter) %>%
  sort() %>%
  cat(sep = "\n")

cat("\n\nDiagnosis (ICD-10) chapters available:\n")
diag_by_chapter_year %>%
  filter(year == max(year)) %>%
  distinct(diag_chapter) %>%
  pull(diag_chapter) %>%
  sort() %>%
  cat(sep = "\n")

cat("\n\n=== All AIHW cleaning complete ===\n")
cat("Output files:\n")
cat("  outputs/hospital_procedures_by_year.csv\n")
cat("  outputs/hospital_procedures_by_year_sex.csv\n")
cat("  outputs/hospital_diagnoses_by_year.csv\n")
cat("\nNext step: Build the concordance table and start Phase 3 exploratory analysis.\n")
