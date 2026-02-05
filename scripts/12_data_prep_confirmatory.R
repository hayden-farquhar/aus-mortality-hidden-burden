# ============================================================================
# Script 12: Data Preparation for Confirmatory & Exploratory Analyses
# (REWRITE — Feb 2026)
#
# This script replaces the earlier skeleton version. It:
#   1. Discovers the internal structure of every downloaded data source
#   2. Extracts and parses the data needed for confirmatory hypotheses (H1, H2, H3)
#   3. Extracts and parses data needed for exploratory analyses (E1–E7)
#   4. Outputs clean, analysis-ready CSVs
#
# PRE-REGISTERED ANALYSIS — follows OSF pre-registration exactly.
#
# FOLDER LAYOUT ASSUMED (from screenshot, 5 Feb 2026):
#   <PROJECT_ROOT>/
#     └── Data from sources/
#         ├── 1A ABS Causes of Death Data/
#         │   ├── 2024_01 Underlying causes of death (Australia).xlsx
#         │   ├── 2024_02 ... (New South Wales).xlsx  ... through 2024_09 (ACT)
#         │   ├── 2024_10 Multiple causes of death (Australia).xlsx    ← KEY
#         │   ├── 2024_11 Intentional self-harm (suicide)(Australia).xlsx
#         │   ├── 2024_12 Deaths of Aboriginal and Torres Strait Islander people.xlsx
#         │   ├── 2024_13 Drug and alcohol-induced deaths (Australia).xlsx
#         │   ├── 2024_14 Causes of death by year of occurrence (Australia).xlsx
#         │   └── 2024_15 External causes of death (Australia).xlsx
#         ├── 1B AIHW Potentially Avoidable Deaths Classification/
#         │   └── AIHW-PHE-229-report-supplementary-tables.xlsx
#         ├── 2A PBS Prescribing Statistics/
#         │   ├── AIHW-HWE-098-PBS-ATC1-prescriptions-monthly-data.xlsx  (103 MB)
#         │   └── AIHW-HWE-098-PBS-ATC2-prescriptions-monthly-data.xlsx  (389 MB)
#         ├── 2B AIHW Mental Health Services/
#         │   └── Data tables_ED states and territories 2023-24/
#         │       ├── ED_State_Sex_Age_Qtr_2324.csv
#         │       ├── ED_Tables_Jurisdictions_2324.xlsx
#         │       └── ... (other ED CSVs)
#         ├── 2C AIHW Burden of Disease/
#         │   └── Supplementary-Data-tables-ABDS-2018-State-and-territory-estimates-for-Australia.xlsx
#         ├── 2E ABS Population Estimates/
#         │   └── ABS Population Estimates Data.csv
#         ├── 3A AIHW Hospital Procedures Data Cubes/
#         │   ├── Procedures-cube-2019-20.xlsx  ... through 2023-24
#         ├── 3B AIHW Principal Diagnosis Data Cubes/
#         │   ├── Principal-Diagnosis-Cube-2019-20.xlsx  ... through 2023-24
#         └── 3C AIHW Admitted Patient Data/
#             ├── 2-admitted-patient-care-2023-24-tables-activity.xlsx
#             ├── 4-admitted-patient-care-2023-24-tables-reasons-for-care.xlsx
#             └── 6-admitted-patient-care-2023-24-tables-procedures.xlsx
#
# OUTPUT FILES:
#   outputs/confirmatory/
#     confirmatory_h1_data.csv        – year-level hypertension MUR by sex
#     confirmatory_h2_data.csv        – condition CVs with avoidability labels
#     confirmatory_h3_data.csv        – year-level mental health MUR by sex
#   outputs/exploratory/
#     deaths_underlying_national.csv  – national underlying cause data
#     deaths_underlying_by_state.csv  – state-level underlying cause data
#     deaths_multiple_causes.csv      – multiple cause of death data
#     avoidable_deaths_classification.csv
#     pbs_prescriptions.csv
#     mental_health_ed_by_state.csv
#     burden_of_disease_dalys.csv
#     population_estimates.csv
#     hospital_procedures.csv
#     hospital_diagnoses.csv
#   outputs/data_structure_report.txt – diagnostic report of all file structures
#
# ============================================================================

# ---- Load libraries --------------------------------------------------------

cat("============================================================\n")
cat("Script 12: Data Preparation (Rewrite — Feb 2026)\n")
cat("============================================================\n\n")

required_packages <- c("tidyverse", "readxl", "writexl")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(tidyverse)
library(readxl)

# ---- Configuration ---------------------------------------------------------

# Set this to YOUR project root (the "Confirmatory exploration" folder).
# The script expects a "Data from sources" subfolder inside it.
# Adjust if your layout is different.

# Auto-detect: if "Data from sources" exists in the working directory, use it.
# Otherwise, try one level up, or prompt the user.

DATA_ROOT <- "../Confirmatory exploration/Data from sources"

# Define all subfolder paths
PATHS <- list(
  abs_cod     = file.path(DATA_ROOT, "1A ABS Causes of Death Data"),
  aihw_avoid  = file.path(DATA_ROOT, "1B AIHW Potentially Avoidable Deaths Classification"),
  pbs         = file.path(DATA_ROOT, "2A PBS Prescribing Statistics"),
  mental_health = file.path(DATA_ROOT, "2B AIHW Mental Health Services",
                            "Data tables_ED states and territories 2023-24"),
  burden      = file.path(DATA_ROOT, "2C AIHW Burden of Disease"),
  population  = file.path(DATA_ROOT, "2E ABS Population Estimates"),
  hosp_proc   = file.path(DATA_ROOT, "3A AIHW Hospital Procedures Data Cubes"),
  hosp_diag   = file.path(DATA_ROOT, "3B AIHW Principal Diagnosis Data Cubes"),
  hosp_admit  = file.path(DATA_ROOT, "3C AIHW Admitted Patient Data")
)

# Create output directories
dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/confirmatory", showWarnings = FALSE)
dir.create("outputs/exploratory", showWarnings = FALSE)

# Open a log file for the structure report
log_file <- file("outputs/data_structure_report.txt", open = "wt")
write_log <- function(...) {
  msg <- paste0(...)
  cat(msg, "\n")
  writeLines(msg, log_file)
}


# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Profile an Excel file: list sheets, dimensions, and column names
profile_excel <- function(filepath, max_preview_rows = 10) {
  if (!file.exists(filepath)) {
    return(list(exists = FALSE, error = paste("File not found:", filepath)))
  }
  
  result <- list(exists = TRUE, filepath = filepath, sheets = list())
  
  tryCatch({
    sheet_names <- excel_sheets(filepath)
    result$n_sheets <- length(sheet_names)
    result$sheet_names <- sheet_names
    
    for (s in sheet_names) {
      tryCatch({
        # Read a small preview to get structure
        preview <- read_excel(filepath, sheet = s, n_max = max_preview_rows,
                              col_names = FALSE, .name_repair = "minimal")
        
        # Also try reading with headers to detect column names
        with_headers <- tryCatch(
          read_excel(filepath, sheet = s, n_max = 5, .name_repair = "unique"),
          error = function(e) NULL
        )
        
        result$sheets[[s]] <- list(
          n_rows_preview = nrow(preview),
          n_cols = ncol(preview),
          raw_first_rows = as.data.frame(preview[1:min(5, nrow(preview)), ]),
          col_names_detected = if (!is.null(with_headers)) names(with_headers) else NULL
        )
      }, error = function(e) {
        result$sheets[[s]] <- list(error = conditionMessage(e))
      })
    }
  }, error = function(e) {
    result$error <- conditionMessage(e)
  })
  
  return(result)
}

#' Profile a CSV file
profile_csv <- function(filepath, max_preview_rows = 10) {
  if (!file.exists(filepath)) {
    return(list(exists = FALSE, error = paste("File not found:", filepath)))
  }
  
  tryCatch({
    df <- read_csv(filepath, n_max = max_preview_rows, show_col_types = FALSE)
    full_nrow <- nrow(read_csv(filepath, show_col_types = FALSE, lazy = TRUE))
    list(
      exists = TRUE,
      n_rows = full_nrow,
      n_cols = ncol(df),
      col_names = names(df),
      col_types = sapply(df, class),
      preview = as.data.frame(head(df, 5))
    )
  }, error = function(e) {
    list(exists = FALSE, error = conditionMessage(e))
  })
}

#' Safely read an Excel sheet, skipping metadata rows at top if needed
#' ABS/AIHW files often have title rows before the actual data header.
#' This function tries to auto-detect where the real header starts.
read_excel_smart <- function(filepath, sheet = 1, max_skip = 15) {
  # Try reading raw first to find the header row
  raw <- tryCatch(
    read_excel(filepath, sheet = sheet, col_names = FALSE, 
               .name_repair = "minimal", n_max = max_skip + 5),
    error = function(e) return(NULL)
  )
  
  if (is.null(raw)) return(NULL)
  
  # Heuristic: the header row is the first row where most cells are non-empty
  # and look like column names (not numbers, not long sentences)
  best_skip <- 0
  best_score <- 0
  
  for (i in seq_len(min(max_skip, nrow(raw)))) {
    row_vals <- as.character(raw[i, ])
    non_empty <- sum(!is.na(row_vals) & row_vals != "" & row_vals != "NA")
    # Penalise rows where values are very long (likely title/subtitle)
    avg_len <- mean(nchar(row_vals[!is.na(row_vals) & row_vals != ""]), na.rm = TRUE)
    if (is.nan(avg_len)) avg_len <- 100
    
    score <- non_empty * (1 / max(1, avg_len / 30))
    
    if (score > best_score && non_empty >= 2) {
      best_score <- score
      best_skip <- i - 1  # skip is 0-indexed (skip 0 = first row is header)
    }
  }
  
  tryCatch(
    read_excel(filepath, sheet = sheet, skip = best_skip, .name_repair = "unique"),
    error = function(e) {
      cat("  ⚠ Failed to read", basename(filepath), "sheet:", sheet, 
          "with skip =", best_skip, "\n")
      cat("    Error:", conditionMessage(e), "\n")
      return(NULL)
    }
  )
}

#' Clean column names to snake_case
clean_names <- function(df) {
  names(df) <- names(df) %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "") %>%
    str_replace_all("__+", "_")
  df
}


# ============================================================================
# SECTION 1: DATA DISCOVERY — Profile every data source
# ============================================================================

write_log("============================================================")
write_log("SECTION 1: DATA DISCOVERY")
write_log("============================================================")
write_log("")

# --- 1A: ABS Causes of Death ---
write_log("--- 1A: ABS Causes of Death Data ---")

if (dir.exists(PATHS$abs_cod)) {
  abs_files <- list.files(PATHS$abs_cod, pattern = "\\.xlsx$", full.names = TRUE)
  write_log(paste("  Found", length(abs_files), "Excel files"))
  
  # Profile each file
  abs_profiles <- list()
  for (f in abs_files) {
    fname <- basename(f)
    write_log(paste("  Profiling:", fname))
    prof <- profile_excel(f)
    abs_profiles[[fname]] <- prof
    if (!is.null(prof$sheet_names)) {
      write_log(paste("    Sheets:", paste(prof$sheet_names, collapse = " | ")))
    }
  }
  
  # Specifically profile the key file: Data Cube 10 (Multiple Causes)
  cube10_file <- abs_files[str_detect(basename(abs_files), "2024_10")]
  if (length(cube10_file) > 0) {
    write_log("")
    write_log("  ★ KEY FILE: Data Cube 10 — Multiple Causes of Death")
    p10 <- abs_profiles[[basename(cube10_file[1])]]
    write_log(paste("    Sheets:", paste(p10$sheet_names, collapse = " | ")))
    for (s in p10$sheet_names) {
      info <- p10$sheets[[s]]
      if (!is.null(info$col_names_detected)) {
        write_log(paste("    Sheet '", s, "' columns:", 
                        paste(head(info$col_names_detected, 10), collapse = ", ")))
      }
    }
  }
} else {
  write_log(paste("  ✗ Folder not found:", PATHS$abs_cod))
}

# --- 1B: Avoidable Deaths ---
write_log("")
write_log("--- 1B: AIHW Potentially Avoidable Deaths Classification ---")

if (dir.exists(PATHS$aihw_avoid)) {
  avoid_files <- list.files(PATHS$aihw_avoid, pattern = "\\.xlsx$", full.names = TRUE)
  for (f in avoid_files) {
    write_log(paste("  Profiling:", basename(f)))
    prof <- profile_excel(f)
    if (!is.null(prof$sheet_names)) {
      write_log(paste("    Sheets:", paste(prof$sheet_names, collapse = " | ")))
    }
  }
} else {
  write_log(paste("  ✗ Folder not found:", PATHS$aihw_avoid))
}

# --- 2A: PBS ---
write_log("")
write_log("--- 2A: PBS Prescribing Statistics ---")

if (dir.exists(PATHS$pbs)) {
  pbs_files <- list.files(PATHS$pbs, pattern = "\\.xlsx$", full.names = TRUE)
  write_log(paste("  Found", length(pbs_files), "files"))
  for (f in pbs_files) {
    fsize <- file.info(f)$size / 1e6
    write_log(paste("  ", basename(f), sprintf("(%.1f MB)", fsize)))
    # Only profile sheets (don't read full data — files are very large)
    tryCatch({
      sheets <- excel_sheets(f)
      write_log(paste("    Sheets:", paste(head(sheets, 5), collapse = " | "),
                       if (length(sheets) > 5) paste("... +", length(sheets) - 5, "more")))
    }, error = function(e) {
      write_log(paste("    ⚠ Could not read sheet names:", conditionMessage(e)))
    })
  }
} else {
  write_log(paste("  ✗ Folder not found:", PATHS$pbs))
}

# --- 2B: Mental Health ---
write_log("")
write_log("--- 2B: AIHW Mental Health Services (ED data) ---")

if (dir.exists(PATHS$mental_health)) {
  mh_files <- list.files(PATHS$mental_health, full.names = TRUE)
  write_log(paste("  Found", length(mh_files), "files"))
  for (f in mh_files) {
    write_log(paste("  ", basename(f)))
    if (str_detect(f, "\\.csv$")) {
      prof <- profile_csv(f)
      if (!is.null(prof$col_names)) {
        write_log(paste("    Columns:", paste(head(prof$col_names, 10), collapse = ", ")))
        write_log(paste("    Rows:", prof$n_rows))
      }
    } else if (str_detect(f, "\\.xlsx$")) {
      prof <- profile_excel(f)
      if (!is.null(prof$sheet_names)) {
        write_log(paste("    Sheets:", paste(prof$sheet_names, collapse = " | ")))
      }
    }
  }
} else {
  write_log(paste("  ✗ Folder not found:", PATHS$mental_health))
}

# --- 2C: Burden of Disease ---
write_log("")
write_log("--- 2C: AIHW Burden of Disease ---")

if (dir.exists(PATHS$burden)) {
  bod_files <- list.files(PATHS$burden, pattern = "\\.xlsx$", full.names = TRUE)
  for (f in bod_files) {
    write_log(paste("  Profiling:", basename(f)))
    prof <- profile_excel(f)
    if (!is.null(prof$sheet_names)) {
      write_log(paste("    Sheets (", length(prof$sheet_names), "):", 
                       paste(head(prof$sheet_names, 10), collapse = " | "),
                       if (length(prof$sheet_names) > 10) "..."))
    }
  }
} else {
  write_log(paste("  ✗ Folder not found:", PATHS$burden))
}

# --- 2E: Population ---
write_log("")
write_log("--- 2E: ABS Population Estimates ---")

if (dir.exists(PATHS$population)) {
  pop_files <- list.files(PATHS$population, full.names = TRUE)
  for (f in pop_files) {
    write_log(paste("  ", basename(f)))
    if (str_detect(f, "\\.csv$")) {
      prof <- profile_csv(f)
      if (!is.null(prof$col_names)) {
        write_log(paste("    Columns:", paste(head(prof$col_names, 12), collapse = ", ")))
        write_log(paste("    Rows:", prof$n_rows))
      }
    }
  }
} else {
  write_log(paste("  ✗ Folder not found:", PATHS$population))
}

# --- 3A: Hospital Procedures ---
write_log("")
write_log("--- 3A: AIHW Hospital Procedures Data Cubes ---")

if (dir.exists(PATHS$hosp_proc)) {
  proc_files <- list.files(PATHS$hosp_proc, pattern = "\\.xlsx$", full.names = TRUE)
  write_log(paste("  Found", length(proc_files), "files"))
  # Profile just the first one as representative
  if (length(proc_files) > 0) {
    write_log(paste("  Profiling representative file:", basename(proc_files[1])))
    prof <- profile_excel(proc_files[1])
    if (!is.null(prof$sheet_names)) {
      write_log(paste("    Sheets:", paste(head(prof$sheet_names, 10), collapse = " | ")))
    }
  }
} else {
  write_log(paste("  ✗ Folder not found:", PATHS$hosp_proc))
}

# --- 3B: Principal Diagnosis ---
write_log("")
write_log("--- 3B: AIHW Principal Diagnosis Data Cubes ---")

if (dir.exists(PATHS$hosp_diag)) {
  diag_files <- list.files(PATHS$hosp_diag, pattern = "\\.xlsx$", full.names = TRUE)
  write_log(paste("  Found", length(diag_files), "files"))
  if (length(diag_files) > 0) {
    write_log(paste("  Profiling representative file:", basename(diag_files[1])))
    prof <- profile_excel(diag_files[1])
    if (!is.null(prof$sheet_names)) {
      write_log(paste("    Sheets:", paste(head(prof$sheet_names, 10), collapse = " | ")))
    }
  }
} else {
  write_log(paste("  ✗ Folder not found:", PATHS$hosp_diag))
}

# --- 3C: Admitted Patient Data ---
write_log("")
write_log("--- 3C: AIHW Admitted Patient Data ---")

if (dir.exists(PATHS$hosp_admit)) {
  admit_files <- list.files(PATHS$hosp_admit, pattern = "\\.xlsx$", full.names = TRUE)
  write_log(paste("  Found", length(admit_files), "files"))
  for (f in admit_files) {
    write_log(paste("  Profiling:", basename(f)))
    prof <- profile_excel(f)
    if (!is.null(prof$sheet_names)) {
      write_log(paste("    Sheets:", paste(head(prof$sheet_names, 8), collapse = " | ")))
    }
  }
} else {
  write_log(paste("  ✗ Folder not found:", PATHS$hosp_admit))
}

write_log("")
write_log("Data discovery complete. See full details above.")
write_log("============================================================\n")


# ============================================================================
# SECTION 2: PRIORITY 1A — ABS Multiple Causes of Death (H1, H3)
# ============================================================================
#
# The key file is Data Cube 10: "Multiple causes of death (Australia)"
# This cube typically contains:
#   - Counts of deaths where each condition was listed as ANY cause
#   - Counts where each condition was the UNDERLYING cause
#   - Broken down by sex, year, and ICD-10 chapter/condition
#
# We extract:
#   (a) Hypertensive diseases (I10–I15) — for H1
#   (b) Mental & behavioural disorders (F00–F99, F10–F19) — for H3
# ============================================================================

cat("\n============================================================\n")
cat("SECTION 2: Extracting Multiple Causes of Death (Cube 10)\n")
cat("============================================================\n\n")

cube10_path <- list.files(PATHS$abs_cod, 
                          pattern = "2024_10.*[Mm]ultiple.*\\.xlsx$", 
                          full.names = TRUE)

if (length(cube10_path) > 0) {
  cube10_path <- cube10_path[1]
  cat("Found:", basename(cube10_path), "\n")
  
  # Get all sheet names
  sheets_10 <- excel_sheets(cube10_path)
  cat("Sheets:", paste(sheets_10, collapse = " | "), "\n\n")
  
  # Read and profile every sheet to find the data we need
  cube10_data <- list()
  
  for (s in sheets_10) {
    cat("  Reading sheet:", s, "... ")
    df <- tryCatch({
      read_excel_smart(cube10_path, sheet = s)
    }, error = function(e) {
      cat("FAILED (", conditionMessage(e), ")\n")
      NULL
    })
    
    if (!is.null(df) && nrow(df) > 0) {
      df <- clean_names(df)
      cube10_data[[s]] <- df
      cat("OK —", nrow(df), "rows,", ncol(df), "cols\n")
      cat("    Columns:", paste(head(names(df), 8), collapse = ", "), "\n")
    } else if (!is.null(df)) {
      cat("Empty sheet\n")
    }
  }
  
  # ------------------------------------------------------------------
  # Strategy: Look for sheets/columns containing hypertension (I10-I15)
  # and mental health (F00-F99) data with year + sex breakdowns.
  #
  # ABS Cube 10 format varies by release. Common patterns:
  #   Pattern A: Long format with columns like
  #     [Year, Sex, Cause_of_death, Type(underlying/multiple), Count]
  #   Pattern B: Wide format with separate columns for underlying vs multiple
  #   Pattern C: Separate sheets per condition group
  #
  # We search all sheets for relevant data.
  # ------------------------------------------------------------------
  
  cat("\n  Searching for hypertension and mental health data across sheets...\n")
  
  # Helper: search for ICD codes or condition names in a dataframe
  find_condition_data <- function(df, patterns) {
    # Search all character columns for the patterns
    char_cols <- names(df)[sapply(df, is.character)]
    for (col in char_cols) {
      matches <- str_detect(df[[col]], regex(paste(patterns, collapse = "|"), 
                                              ignore_case = TRUE))
      if (any(matches, na.rm = TRUE)) {
        return(list(column = col, n_matches = sum(matches, na.rm = TRUE)))
      }
    }
    return(NULL)
  }
  
  hypertension_patterns <- c("I10", "I11", "I12", "I13", "I14", "I15",
                              "[Hh]ypertens")
  mental_health_patterns <- c("F00", "F01", "F10", "F19", "F20", "F99",
                               "[Mm]ental.*behav", "[Ss]ubstance")
  
  hyp_found_in <- list()
  mh_found_in <- list()
  
  for (s in names(cube10_data)) {
    df <- cube10_data[[s]]
    
    hyp_result <- find_condition_data(df, hypertension_patterns)
    if (!is.null(hyp_result)) {
      hyp_found_in[[s]] <- hyp_result
      cat("    ★ Hypertension data found in sheet '", s, "' — column '", 
          hyp_result$column, "' (", hyp_result$n_matches, " rows)\n")
    }
    
    mh_result <- find_condition_data(df, mental_health_patterns)
    if (!is.null(mh_result)) {
      mh_found_in[[s]] <- mh_result
      cat("    ★ Mental health data found in sheet '", s, "' — column '", 
          mh_result$column, "' (", mh_result$n_matches, " rows)\n")
    }
  }
  
  # ------------------------------------------------------------------
  # Now attempt to extract and structure the data.
  # We look for columns that represent: year, sex, cause type, count.
  # ------------------------------------------------------------------
  
  extract_mur_data <- function(sheets_data, condition_name, icd_pattern, 
                                found_sheets) {
    if (length(found_sheets) == 0) {
      cat("  ✗ No sheets found containing", condition_name, "data.\n")
      return(NULL)
    }
    
    all_results <- list()
    
    for (s in names(found_sheets)) {
      df <- sheets_data[[s]] %>% clean_names()
      info <- found_sheets[[s]]
      cause_col <- info$column
      
      # Detect year, sex, and count columns
      year_col <- names(df)[str_detect(names(df), "year|period|time|date")][1]
      sex_col <- names(df)[str_detect(names(df), "sex|gender")][1]
      
      # Count columns: look for numeric columns that might be counts
      num_cols <- names(df)[sapply(df, is.numeric)]
      
      # Also look for "underlying" and "multiple/associated/all cause" indicators
      type_col <- names(df)[str_detect(names(df), 
                                        "type|cause_type|underlying|multiple|associated")][1]
      
      cat("  Sheet '", s, "': year=", 
          ifelse(is.na(year_col), "NOT FOUND", year_col), ", sex=",
          ifelse(is.na(sex_col), "NOT FOUND", sex_col), ", type=",
          ifelse(is.na(type_col), "NOT FOUND", type_col), "\n")
      cat("    Numeric columns:", paste(num_cols, collapse = ", "), "\n")
      
      # Filter to our condition
      condition_rows <- df %>%
        filter(str_detect(!!sym(cause_col), regex(icd_pattern, ignore_case = TRUE)))
      
      if (nrow(condition_rows) > 0) {
        cat("    Found", nrow(condition_rows), "rows matching", condition_name, "\n")
        
        # Store the filtered data — we'll structure it in the next step
        all_results[[s]] <- list(
          data = condition_rows,
          year_col = year_col,
          sex_col = sex_col,
          type_col = type_col,
          cause_col = cause_col,
          num_cols = num_cols
        )
      }
    }
    
    return(all_results)
  }
  
  # Extract hypertension data
  cat("\n  --- Extracting Hypertension (I10–I15) Data ---\n")
  hyp_raw <- extract_mur_data(
    cube10_data, "Hypertensive diseases", 
    "I1[0-5]|[Hh]ypertens", hyp_found_in
  )
  
  # Extract mental health data
  cat("\n  --- Extracting Mental Health (F00–F99) Data ---\n")
  mh_raw <- extract_mur_data(
    cube10_data, "Mental & behavioural disorders",
    "F[0-9]|[Mm]ental.*behav|[Ss]ubstance", mh_found_in
  )
  
  # ------------------------------------------------------------------
  # Save raw extracted data for manual inspection
  # The user may need to adjust the structuring below.
  # ------------------------------------------------------------------
  
  if (length(hyp_raw) > 0) {
    for (s in names(hyp_raw)) {
      outfile <- paste0("outputs/confirmatory/raw_hypertension_", 
                         str_replace_all(s, "[^a-zA-Z0-9]", "_"), ".csv")
      write_csv(hyp_raw[[s]]$data, outfile)
      cat("  → Saved raw extract:", outfile, "\n")
    }
  }
  
  if (length(mh_raw) > 0) {
    for (s in names(mh_raw)) {
      outfile <- paste0("outputs/confirmatory/raw_mental_health_", 
                         str_replace_all(s, "[^a-zA-Z0-9]", "_"), ".csv")
      write_csv(mh_raw[[s]]$data, outfile)
      cat("  → Saved raw extract:", outfile, "\n")
    }
  }
  
  # ------------------------------------------------------------------
  # Attempt to compute MUR (Multiple-to-Underlying Ratio) 
  # This depends on how the ABS structures the data.
  # Common pattern: separate count columns for "underlying" and "multiple".
  # ------------------------------------------------------------------
  
  build_mur_from_extract <- function(raw_extract, condition_label) {
    if (is.null(raw_extract) || length(raw_extract) == 0) return(NULL)
    
    # Use the first (or most data-rich) sheet
    best_sheet <- names(raw_extract)[which.max(
      sapply(raw_extract, function(x) nrow(x$data))
    )]
    info <- raw_extract[[best_sheet]]
    df <- info$data
    
    cat("  Using sheet '", best_sheet, "' (", nrow(df), " rows)\n")
    
    # Strategy 1: If there are columns named like "underlying" and "multiple"
    underlying_col <- info$num_cols[str_detect(tolower(info$num_cols), 
                                                "underlying|ucod")][1]
    multiple_col <- info$num_cols[str_detect(tolower(info$num_cols), 
                                              "multiple|all.*cause|mention|mcod|total")][1]
    
    if (!is.na(underlying_col) && !is.na(multiple_col)) {
      cat("  Found underlying col:", underlying_col, ", multiple col:", multiple_col, "\n")
      
      result <- df %>%
        rename(underlying_count = !!sym(underlying_col),
               multiple_count = !!sym(multiple_col))
      
      if (!is.na(info$year_col)) {
        result <- result %>% rename(year = !!sym(info$year_col))
      }
      if (!is.na(info$sex_col)) {
        result <- result %>% rename(sex = !!sym(info$sex_col))
      }
      
      result <- result %>%
        mutate(
          underlying_count = as.numeric(underlying_count),
          multiple_count = as.numeric(multiple_count),
          mur = multiple_count / underlying_count,
          condition = condition_label
        )
      
      return(result)
    }
    
    # Strategy 2: If there's a type column distinguishing underlying vs multiple
    if (!is.na(info$type_col)) {
      cat("  Found type column:", info$type_col, "\n")
      cat("  Unique values:", paste(head(unique(df[[info$type_col]]), 10), collapse = ", "), "\n")
      
      # This requires pivoting — save the raw data for manual processing
      cat("  ⚠ Type-based extraction requires manual structuring.\n")
      cat("  The raw data has been saved. Please check and adjust.\n")
      return(NULL)
    }
    
    # Strategy 3: Columns might be structured differently
    cat("  ⚠ Could not auto-detect underlying vs multiple count columns.\n")
    cat("  Available numeric columns:", paste(info$num_cols, collapse = ", "), "\n")
    cat("  Please check the raw extract and manually specify column mappings.\n")
    return(NULL)
  }
  
  cat("\n  --- Building H1 MUR (Hypertension) ---\n")
  h1_data <- build_mur_from_extract(hyp_raw, "Hypertensive diseases (I10-I15)")
  
  if (!is.null(h1_data)) {
    write_csv(h1_data, "outputs/confirmatory/confirmatory_h1_data.csv")
    cat("  ✓ Saved: outputs/confirmatory/confirmatory_h1_data.csv\n")
    cat("    ", nrow(h1_data), "rows\n")
  }
  
  cat("\n  --- Building H3 MUR (Mental Health) ---\n")
  h3_data <- build_mur_from_extract(mh_raw, "Mental and behavioural disorders (F00-F99)")
  
  if (!is.null(h3_data)) {
    write_csv(h3_data, "outputs/confirmatory/confirmatory_h3_data.csv")
    cat("  ✓ Saved: outputs/confirmatory/confirmatory_h3_data.csv\n")
    cat("    ", nrow(h3_data), "rows\n")
  }
  
} else {
  cat("✗ Data Cube 10 (Multiple Causes) not found in:", PATHS$abs_cod, "\n")
  cat("  Expected filename pattern: 2024_10*Multiple*.xlsx\n")
}


# ============================================================================
# SECTION 3: ABS Underlying Causes by State (H2, exploratory)
# ============================================================================
#
# Read Data Cubes 1–9 (Australia + 8 states/territories) to build a
# state × cause matrix of death counts / rates.
# ============================================================================

cat("\n============================================================\n")
cat("SECTION 3: Extracting Underlying Causes by State\n")
cat("============================================================\n\n")

state_files <- list.files(PATHS$abs_cod,
                          pattern = "2024_0[1-9].*[Uu]nderlying.*\\.xlsx$",
                          full.names = TRUE)

state_map <- c(
  "01" = "Australia",
  "02" = "NSW", "03" = "VIC", "04" = "QLD",
  "05" = "SA",  "06" = "WA",  "07" = "TAS",
  "08" = "NT",  "09" = "ACT"
)

if (length(state_files) > 0) {
  cat("Found", length(state_files), "underlying cause files.\n")
  
  all_state_data <- list()
  
  for (f in state_files) {
    fname <- basename(f)
    # Extract the cube number (01–09) to identify the state
    cube_num <- str_extract(fname, "(?<=2024_)0[1-9]")
    state_name <- state_map[cube_num]
    
    cat("  Reading:", fname, "→", state_name, "... ")
    
    # Get sheet names
    sheets <- tryCatch(excel_sheets(f), error = function(e) character(0))
    
    if (length(sheets) == 0) {
      cat("FAILED (no sheets)\n")
      next
    }
    
    # ABS underlying cause cubes often have a main data sheet.
    # Look for sheets that contain actual data (not "Contents", "Notes", etc.)
    data_sheets <- sheets[!str_detect(tolower(sheets), 
                                       "content|note|glossary|explain|method")]
    
    if (length(data_sheets) == 0) data_sheets <- sheets
    
    # Read the first substantive sheet
    df <- read_excel_smart(f, sheet = data_sheets[1])
    
    if (!is.null(df) && nrow(df) > 0) {
      df <- clean_names(df) %>%
        mutate(.state = state_name, .source_file = fname)
      all_state_data[[state_name]] <- df
      cat("OK —", nrow(df), "rows,", ncol(df), "cols\n")
      cat("    Columns:", paste(head(names(df), 8), collapse = ", "), "\n")
    } else {
      cat("Empty or unreadable\n")
    }
  }
  
  # Combine all states (where column structures match)
  if (length(all_state_data) > 0) {
    # Check if all dataframes have the same columns
    all_colnames <- lapply(all_state_data, names)
    common_cols <- Reduce(intersect, all_colnames)
    
    if (length(common_cols) > 2) {
      cat("\n  Combining state data on", length(common_cols), "common columns...\n")
      
      combined_states <- bind_rows(
        lapply(all_state_data, function(df) df %>% select(any_of(common_cols)))
      )
      
      write_csv(combined_states, "outputs/exploratory/deaths_underlying_by_state.csv")
      cat("  ✓ Saved: outputs/exploratory/deaths_underlying_by_state.csv\n")
      cat("    ", nrow(combined_states), "rows across", 
          n_distinct(combined_states$.state), "jurisdictions\n")
    } else {
      cat("\n  ⚠ Column structures differ across state files. Saving separately.\n")
      for (state_name in names(all_state_data)) {
        outfile <- paste0("outputs/exploratory/deaths_underlying_", 
                           str_to_lower(state_name), ".csv")
        write_csv(all_state_data[[state_name]], outfile)
        cat("  → Saved:", outfile, "\n")
      }
    }
  }
  
  # Also save the national file (Cube 01) separately for convenience
  if ("Australia" %in% names(all_state_data)) {
    write_csv(all_state_data[["Australia"]], 
              "outputs/exploratory/deaths_underlying_national.csv")
    cat("  ✓ Also saved: outputs/exploratory/deaths_underlying_national.csv\n")
  }
  
} else {
  cat("✗ No underlying cause files found in:", PATHS$abs_cod, "\n")
}


# ============================================================================
# SECTION 4: Additional ABS Death Data Cubes
# ============================================================================
#
# Extract drug/alcohol-induced deaths (Cube 13), suicide (Cube 11),
# and year-of-occurrence data (Cube 14) for enrichment.
# ============================================================================

cat("\n============================================================\n")
cat("SECTION 4: Additional ABS Death Data Cubes\n")
cat("============================================================\n\n")

# --- Cube 13: Drug and alcohol-induced deaths ---
cube13_path <- list.files(PATHS$abs_cod,
                          pattern = "2024_13.*[Dd]rug.*\\.xlsx$",
                          full.names = TRUE)

if (length(cube13_path) > 0) {
  cat("  Reading Cube 13 (Drug/alcohol deaths):", basename(cube13_path[1]), "\n")
  sheets_13 <- excel_sheets(cube13_path[1])
  cat("  Sheets:", paste(sheets_13, collapse = " | "), "\n")
  
  # Read all substantive sheets
  data_sheets_13 <- sheets_13[!str_detect(tolower(sheets_13), 
                                           "content|note|glossary|explain")]
  
  cube13_all <- list()
  for (s in data_sheets_13) {
    df <- read_excel_smart(cube13_path[1], sheet = s)
    if (!is.null(df) && nrow(df) > 0) {
      cube13_all[[s]] <- clean_names(df)
      cat("    Sheet '", s, "':", nrow(df), "rows\n")
    }
  }
  
  if (length(cube13_all) > 0) {
    # Save each sheet as a separate CSV (they likely have different structures)
    for (s in names(cube13_all)) {
      safe_name <- str_replace_all(s, "[^a-zA-Z0-9]", "_") %>% str_to_lower()
      outfile <- paste0("outputs/exploratory/deaths_drug_alcohol_", safe_name, ".csv")
      write_csv(cube13_all[[s]], outfile)
    }
    cat("  ✓ Saved drug/alcohol death tables to outputs/exploratory/\n")
  }
}

# --- Cube 11: Suicide ---
cube11_path <- list.files(PATHS$abs_cod,
                          pattern = "2024_11.*[Ss]uicide.*\\.xlsx$",
                          full.names = TRUE)

if (length(cube11_path) > 0) {
  cat("\n  Reading Cube 11 (Suicide):", basename(cube11_path[1]), "\n")
  sheets_11 <- excel_sheets(cube11_path[1])
  cat("  Sheets:", paste(sheets_11, collapse = " | "), "\n")
  
  data_sheets_11 <- sheets_11[!str_detect(tolower(sheets_11), 
                                           "content|note|glossary|explain")]
  for (s in data_sheets_11) {
    df <- read_excel_smart(cube11_path[1], sheet = s)
    if (!is.null(df) && nrow(df) > 0) {
      safe_name <- str_replace_all(s, "[^a-zA-Z0-9]", "_") %>% str_to_lower()
      write_csv(clean_names(df), 
                paste0("outputs/exploratory/deaths_suicide_", safe_name, ".csv"))
      cat("    Sheet '", s, "':", nrow(df), "rows → saved\n")
    }
  }
}

# --- Cube 14: Year of occurrence ---
cube14_path <- list.files(PATHS$abs_cod,
                          pattern = "2024_14.*[Yy]ear.*occurrence.*\\.xlsx$",
                          full.names = TRUE)

if (length(cube14_path) > 0) {
  cat("\n  Reading Cube 14 (Year of occurrence):", basename(cube14_path[1]), "\n")
  sheets_14 <- excel_sheets(cube14_path[1])
  cat("  Sheets:", paste(head(sheets_14, 10), collapse = " | "), "\n")
  
  # This file is 2.8 MB — may have many sheets. Read selectively.
  data_sheets_14 <- sheets_14[!str_detect(tolower(sheets_14), 
                                           "content|note|glossary|explain")]
  cat("  Data sheets:", length(data_sheets_14), "\n")
  
  for (s in head(data_sheets_14, 5)) {  # Read first 5 data sheets
    df <- read_excel_smart(cube14_path[1], sheet = s)
    if (!is.null(df) && nrow(df) > 0) {
      safe_name <- str_replace_all(s, "[^a-zA-Z0-9]", "_") %>% str_to_lower()
      write_csv(clean_names(df),
                paste0("outputs/exploratory/deaths_by_occurrence_", safe_name, ".csv"))
      cat("    Sheet '", s, "':", nrow(df), "rows → saved\n")
    }
  }
}


# ============================================================================
# SECTION 5: PRIORITY 1B — AIHW Avoidability Classification (H2)
# ============================================================================

cat("\n============================================================\n")
cat("SECTION 5: AIHW Avoidability Classification\n")
cat("============================================================\n\n")

avoid_file <- list.files(PATHS$aihw_avoid, pattern = "\\.xlsx$", full.names = TRUE)

if (length(avoid_file) > 0) {
  avoid_file <- avoid_file[1]
  cat("Found:", basename(avoid_file), "\n")
  
  sheets_avoid <- excel_sheets(avoid_file)
  cat("Sheets:", paste(sheets_avoid, collapse = " | "), "\n\n")
  
  # Read all sheets and look for the classification table
  avoid_all <- list()
  for (s in sheets_avoid) {
    df <- read_excel_smart(avoid_file, sheet = s)
    if (!is.null(df) && nrow(df) > 0) {
      avoid_all[[s]] <- clean_names(df)
      cat("  Sheet '", s, "':", nrow(df), "rows,", ncol(df), "cols\n")
      cat("    Columns:", paste(head(names(clean_names(df)), 8), collapse = ", "), "\n")
    }
  }
  
  # Search for sheets containing ICD codes and avoidability info
  for (s in names(avoid_all)) {
    df <- avoid_all[[s]]
    # Check if any column contains ICD-10 codes
    has_icd <- any(sapply(df, function(col) {
      if (is.character(col)) any(str_detect(col, "[A-Z][0-9]{2}"), na.rm = TRUE)
      else FALSE
    }))
    # Check if any column contains "avoidable" or "preventable" or "treatable"
    has_avoid <- any(sapply(df, function(col) {
      if (is.character(col)) any(str_detect(tolower(col), 
                                             "avoidable|preventable|treatable"), na.rm = TRUE)
      else FALSE
    }))
    
    if (has_icd || has_avoid) {
      cat("  ★ Sheet '", s, "' appears to contain classification data\n")
      outfile <- paste0("outputs/exploratory/avoidable_classification_", 
                         str_replace_all(s, "[^a-zA-Z0-9]", "_"), ".csv")
      write_csv(df, outfile)
      cat("    → Saved:", outfile, "\n")
      
      # Also save as the expected filename for downstream scripts
      write_csv(df, "outputs/exploratory/avoidable_deaths_classification.csv")
      cat("    → Also saved as: outputs/exploratory/avoidable_deaths_classification.csv\n")
    }
  }
  
  # Also look for sheets with death rates by state (for H2 geographic analysis)
  for (s in names(avoid_all)) {
    df <- avoid_all[[s]]
    has_state <- any(sapply(df, function(col) {
      if (is.character(col)) {
        any(str_detect(col, "NSW|VIC|QLD|SA|WA|TAS|NT|ACT|New South Wales|Victoria"), 
            na.rm = TRUE)
      } else FALSE
    }))
    
    if (has_state) {
      cat("  ★ Sheet '", s, "' contains state-level data\n")
      outfile <- paste0("outputs/exploratory/avoidable_deaths_by_state_", 
                         str_replace_all(s, "[^a-zA-Z0-9]", "_"), ".csv")
      write_csv(df, outfile)
      cat("    → Saved:", outfile, "\n")
    }
  }
  
} else {
  cat("✗ AIHW avoidable deaths file not found in:", PATHS$aihw_avoid, "\n")
}


# ============================================================================
# SECTION 6: H2 — Compute Geographic CVs and Merge Avoidability
# ============================================================================

cat("\n============================================================\n")
cat("SECTION 6: Building H2 Data (Geographic CVs + Avoidability)\n")
cat("============================================================\n\n")

# Check if we have state-level death data from Section 3
state_data_file <- "outputs/exploratory/deaths_underlying_by_state.csv"
avoid_class_file <- "outputs/exploratory/avoidable_deaths_classification.csv"

if (file.exists(state_data_file)) {
  cat("Loading state-level death data...\n")
  state_deaths <- read_csv(state_data_file, show_col_types = FALSE)
  
  cat("  Columns:", paste(names(state_deaths), collapse = ", "), "\n")
  cat("  Rows:", nrow(state_deaths), "\n\n")
  
  # Try to compute CVs — need to identify state, cause, and rate columns
  state_col <- names(state_deaths)[str_detect(tolower(names(state_deaths)), 
                                               "state|territory|jurisdiction|\\.state")][1]
  cause_col <- names(state_deaths)[str_detect(tolower(names(state_deaths)), 
                                               "cause|condition|icd|chapter|disease")][1]
  
  # Look for age-standardised rate, or count, or rate column
  rate_col <- names(state_deaths)[str_detect(tolower(names(state_deaths)), 
                                              "asr|age_stand|standard.*rate")][1]
  if (is.na(rate_col)) {
    rate_col <- names(state_deaths)[str_detect(tolower(names(state_deaths)), 
                                                "rate|per_100")][1]
  }
  if (is.na(rate_col)) {
    # Fall back to count columns
    rate_col <- names(state_deaths)[sapply(state_deaths, is.numeric)][1]
  }
  
  cat("  Detected: state=", ifelse(is.na(state_col), "?", state_col),
      ", cause=", ifelse(is.na(cause_col), "?", cause_col),
      ", rate=", ifelse(is.na(rate_col), "?", rate_col), "\n\n")
  
  if (!is.na(state_col) && !is.na(cause_col) && !is.na(rate_col)) {
    # Exclude "Australia" from state-level analysis
    cv_data <- state_deaths %>%
      rename(state = !!sym(state_col),
             cause = !!sym(cause_col),
             rate = !!sym(rate_col)) %>%
      filter(!str_detect(tolower(state), "australia|total|all")) %>%
      mutate(rate = as.numeric(rate)) %>%
      filter(!is.na(rate), rate > 0) %>%
      group_by(cause) %>%
      summarise(
        n_states = n_distinct(state),
        mean_rate = mean(rate, na.rm = TRUE),
        sd_rate = sd(rate, na.rm = TRUE),
        cv = (sd_rate / mean_rate) * 100,
        min_rate = min(rate, na.rm = TRUE),
        max_rate = max(rate, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(n_states >= 6)
    
    cat("  Computed CVs for", nrow(cv_data), "conditions.\n")
    
    # Merge with avoidability classification if available
    if (file.exists(avoid_class_file)) {
      avoid_class <- read_csv(avoid_class_file, show_col_types = FALSE)
      cat("  Loaded avoidability classification:", nrow(avoid_class), "rows\n")
      cat("  Columns:", paste(names(avoid_class), collapse = ", "), "\n")
      
      # Attempt fuzzy matching between condition names
      # First try exact join on the first column
      avoid_col1 <- names(avoid_class)[1]
      
      h2_data <- cv_data %>%
        left_join(avoid_class, by = c("cause" = avoid_col1))
      
      # Check match rate
      avoidability_cols <- names(avoid_class)[str_detect(tolower(names(avoid_class)), 
                                                          "avoid|prevent|treat|categ|class")]
      if (length(avoidability_cols) > 0) {
        n_matched <- sum(!is.na(h2_data[[avoidability_cols[1]]]))
        cat("  Matched", n_matched, "of", nrow(cv_data), "conditions.\n")
        
        if (n_matched < 5) {
          cat("  ⚠ Low match rate. Condition names likely differ between sources.\n")
          cat("  Saving unmatched data — you'll need to create a manual concordance.\n")
          
          # Save both for manual matching
          write_csv(cv_data, "outputs/confirmatory/h2_condition_cvs.csv")
          write_csv(avoid_class, "outputs/confirmatory/h2_avoidability_lookup.csv")
          cat("  → Saved: outputs/confirmatory/h2_condition_cvs.csv\n")
          cat("  → Saved: outputs/confirmatory/h2_avoidability_lookup.csv\n")
        }
      }
      
      write_csv(h2_data, "outputs/confirmatory/confirmatory_h2_data.csv")
      cat("  ✓ Saved: outputs/confirmatory/confirmatory_h2_data.csv\n")
      
    } else {
      cat("  Avoidability classification not yet available — saving CVs only.\n")
      write_csv(cv_data, "outputs/confirmatory/h2_condition_cvs.csv")
      cat("  → Saved: outputs/confirmatory/h2_condition_cvs.csv\n")
    }
    
  } else {
    cat("  ⚠ Could not auto-detect required columns for CV calculation.\n")
    cat("  Please check the state-level death data and adjust column names.\n")
  }
  
} else {
  cat("  ⚠ State-level death data not yet available.\n")
  cat("  This will be generated after Section 3 runs successfully.\n")
}


# ============================================================================
# SECTION 7: PRIORITY 2 — Exploratory Data Sources
# ============================================================================

cat("\n============================================================\n")
cat("SECTION 7: Exploratory Data Sources\n")
cat("============================================================\n\n")

# --- 7A: PBS Prescribing Data ---
cat("--- 7A: PBS Prescribing Statistics ---\n\n")

pbs_files <- list.files(PATHS$pbs, pattern = "\\.xlsx$", full.names = TRUE)

if (length(pbs_files) > 0) {
  # These files are VERY large (103 MB and 389 MB).
  # We only need ATC2 level data for antihypertensives (C02-C09).
  # Read sheet names first, then selectively read relevant sheets.
  
  for (f in pbs_files) {
    fsize <- file.info(f)$size / 1e6
    cat("  File:", basename(f), sprintf("(%.0f MB)\n", fsize))
    
    tryCatch({
      sheets <- excel_sheets(f)
      cat("  Sheets:", paste(head(sheets, 10), collapse = " | "), "\n")
      
      # Look for sheets containing ATC2 codes or "C0" prefix
      # The ATC2 file should have antihypertensive data at the 2-char level
      if (str_detect(basename(f), "ATC2")) {
        cat("  → This is the ATC2-level file (more granular — has C02-C09)\n")
        
        # Read just a preview to understand structure
        cat("  Reading preview (first sheet, first 20 rows)...\n")
        preview <- tryCatch(
          read_excel(f, sheet = sheets[1], n_max = 20, .name_repair = "unique"),
          error = function(e) {
            cat("  ⚠ Failed to read preview:", conditionMessage(e), "\n")
            NULL
          }
        )
        
        if (!is.null(preview)) {
          preview <- clean_names(preview)
          cat("  Columns:", paste(names(preview), collapse = ", "), "\n")
          cat("  Preview (first 3 rows):\n")
          print(head(preview, 3))
          
          # Try to find and filter for antihypertensive ATC codes
          cat("\n  Attempting to extract antihypertensive data (ATC C02-C09)...\n")
          cat("  ⚠ NOTE: This file is very large. Reading may take several minutes.\n")
          
          # Read the full sheet
          pbs_full <- tryCatch({
            read_excel(f, sheet = sheets[1], .name_repair = "unique") %>%
              clean_names()
          }, error = function(e) {
            cat("  ✗ Failed to read full file:", conditionMessage(e), "\n")
            NULL
          })
          
          if (!is.null(pbs_full)) {
            cat("  Read", nrow(pbs_full), "rows\n")
            
            # Find the ATC code column
            atc_col <- names(pbs_full)[str_detect(tolower(names(pbs_full)), 
                                                   "atc|code|drug|class|category")][1]
            
            if (!is.na(atc_col)) {
              # Filter for C02-C09 (antihypertensives)
              pbs_antihyp <- pbs_full %>%
                filter(str_detect(!!sym(atc_col), "^C0[2-9]"))
              
              if (nrow(pbs_antihyp) > 0) {
                write_csv(pbs_antihyp, "outputs/exploratory/pbs_antihypertensives.csv")
                cat("  ✓ Extracted", nrow(pbs_antihyp), "antihypertensive rows\n")
                cat("  → Saved: outputs/exploratory/pbs_antihypertensives.csv\n")
              } else {
                cat("  No rows matched ATC C02-C09 in column '", atc_col, "'\n")
                cat("  Unique values (first 20):", 
                    paste(head(unique(pbs_full[[atc_col]]), 20), collapse = ", "), "\n")
              }
            } else {
              cat("  ⚠ Could not identify ATC code column.\n")
              cat("  Columns:", paste(names(pbs_full), collapse = ", "), "\n")
            }
            
            # Also save a summary of all ATC codes available
            if (!is.na(atc_col)) {
              atc_summary <- pbs_full %>%
                count(!!sym(atc_col), name = "n_records") %>%
                arrange(desc(n_records))
              write_csv(atc_summary, "outputs/exploratory/pbs_atc_code_summary.csv")
              cat("  → Also saved ATC code summary\n")
            }
          }
        }
      } else {
        cat("  → This is the ATC1-level file (broader categories)\n")
        # Read preview only
        preview <- tryCatch(
          read_excel(f, sheet = sheets[1], n_max = 20, .name_repair = "unique"),
          error = function(e) NULL
        )
        if (!is.null(preview)) {
          cat("  Columns:", paste(names(clean_names(preview)), collapse = ", "), "\n")
        }
      }
    }, error = function(e) {
      cat("  ✗ Error:", conditionMessage(e), "\n")
    })
    cat("\n")
  }
} else {
  cat("  ✗ No PBS files found in:", PATHS$pbs, "\n\n")
}

# --- 7B: Mental Health ED Data ---
cat("--- 7B: Mental Health ED Data by State ---\n\n")

if (dir.exists(PATHS$mental_health)) {
  # The key file for state-level analysis is ED_State_Sex_Age_Qtr_2324.csv
  ed_state_file <- file.path(PATHS$mental_health, "ED_State_Sex_Age_Qtr_2324.csv")
  ed_juris_file <- file.path(PATHS$mental_health, "ED_Tables_Jurisdictions_2324.xlsx")
  
  if (file.exists(ed_state_file)) {
    cat("  Reading:", basename(ed_state_file), "\n")
    ed_state <- tryCatch(
      read_csv(ed_state_file, show_col_types = FALSE),
      error = function(e) { cat("  ✗ Error:", conditionMessage(e), "\n"); NULL }
    )
    
    if (!is.null(ed_state)) {
      cat("  Rows:", nrow(ed_state), "\n")
      cat("  Columns:", paste(names(ed_state), collapse = ", "), "\n")
      
      ed_state_clean <- clean_names(ed_state)
      write_csv(ed_state_clean, "outputs/exploratory/mental_health_ed_state_sex_age.csv")
      cat("  ✓ Saved: outputs/exploratory/mental_health_ed_state_sex_age.csv\n\n")
    }
  }
  
  if (file.exists(ed_juris_file)) {
    cat("  Reading:", basename(ed_juris_file), "\n")
    sheets_juris <- excel_sheets(ed_juris_file)
    cat("  Sheets:", paste(sheets_juris, collapse = " | "), "\n")
    
    for (s in sheets_juris) {
      df <- read_excel_smart(ed_juris_file, sheet = s)
      if (!is.null(df) && nrow(df) > 0) {
        safe_name <- str_replace_all(s, "[^a-zA-Z0-9]", "_") %>% str_to_lower()
        write_csv(clean_names(df),
                  paste0("outputs/exploratory/mental_health_ed_jurisdictions_", 
                         safe_name, ".csv"))
        cat("    Sheet '", s, "':", nrow(df), "rows → saved\n")
      }
    }
  }
  
  # Also read any other CSV files in the folder
  other_csvs <- list.files(PATHS$mental_health, pattern = "\\.csv$", full.names = TRUE)
  other_csvs <- other_csvs[!str_detect(basename(other_csvs), "State_Sex_Age")]  # skip already read
  
  for (f in other_csvs) {
    cat("  Reading:", basename(f), "... ")
    df <- tryCatch(read_csv(f, show_col_types = FALSE), error = function(e) NULL)
    if (!is.null(df) && nrow(df) > 0) {
      safe_name <- str_replace_all(basename(f), "\\.csv$", "") %>%
        str_replace_all("[^a-zA-Z0-9]", "_") %>% str_to_lower()
      write_csv(clean_names(df),
                paste0("outputs/exploratory/mental_health_", safe_name, ".csv"))
      cat(nrow(df), "rows → saved\n")
    } else {
      cat("empty or failed\n")
    }
  }
  
} else {
  cat("  ✗ Mental health data folder not found:", PATHS$mental_health, "\n")
}
cat("\n")

# --- 7C: Burden of Disease ---
cat("--- 7C: AIHW Burden of Disease (DALY Estimates) ---\n\n")

bod_files <- list.files(PATHS$burden, pattern = "\\.xlsx$", full.names = TRUE)

if (length(bod_files) > 0) {
  bod_file <- bod_files[1]
  cat("  Reading:", basename(bod_file), "\n")
  
  sheets_bod <- excel_sheets(bod_file)
  cat("  Sheets (", length(sheets_bod), "):", 
      paste(head(sheets_bod, 10), collapse = " | "), "\n\n")
  
  # Read all sheets and save
  for (s in sheets_bod) {
    df <- read_excel_smart(bod_file, sheet = s)
    if (!is.null(df) && nrow(df) > 1) {
      safe_name <- str_replace_all(s, "[^a-zA-Z0-9]", "_") %>% str_to_lower()
      outfile <- paste0("outputs/exploratory/burden_of_disease_", safe_name, ".csv")
      write_csv(clean_names(df), outfile)
      cat("    Sheet '", s, "':", nrow(df), "rows → saved\n")
    }
  }
  
  # Look specifically for DALY data with state breakdowns
  for (s in sheets_bod) {
    df <- read_excel_smart(bod_file, sheet = s)
    if (!is.null(df) && nrow(df) > 0) {
      df_clean <- clean_names(df)
      has_daly <- any(str_detect(tolower(names(df_clean)), "daly|yld|yll|burden"))
      has_state <- any(sapply(df_clean, function(col) {
        if (is.character(col)) any(str_detect(col, "NSW|VIC|QLD"), na.rm = TRUE)
        else FALSE
      }))
      
      if (has_daly && has_state) {
        cat("  ★ Sheet '", s, "' has DALY data with state breakdowns\n")
        write_csv(df_clean, "outputs/exploratory/burden_of_disease_dalys_by_state.csv")
        cat("  → Saved: outputs/exploratory/burden_of_disease_dalys_by_state.csv\n")
      } else if (has_daly) {
        cat("  ★ Sheet '", s, "' has DALY data (national level)\n")
        write_csv(df_clean, "outputs/exploratory/burden_of_disease_dalys.csv")
        cat("  → Saved: outputs/exploratory/burden_of_disease_dalys.csv\n")
      }
    }
  }
  
} else {
  cat("  ✗ No burden of disease files found in:", PATHS$burden, "\n")
}
cat("\n")

# --- 7D: Population Estimates ---
cat("--- 7D: ABS Population Estimates ---\n\n")

pop_file <- file.path(PATHS$population, "ABS Population Estimates Data.csv")

if (file.exists(pop_file)) {
  cat("  Reading:", basename(pop_file), "\n")
  pop <- tryCatch(
    read_csv(pop_file, show_col_types = FALSE),
    error = function(e) { cat("  ✗ Error:", conditionMessage(e), "\n"); NULL }
  )
  
  if (!is.null(pop)) {
    pop_clean <- clean_names(pop)
    cat("  Rows:", nrow(pop_clean), "\n")
    cat("  Columns:", paste(names(pop_clean), collapse = ", "), "\n")
    
    write_csv(pop_clean, "outputs/exploratory/population_estimates.csv")
    cat("  ✓ Saved: outputs/exploratory/population_estimates.csv\n")
  }
} else {
  cat("  ✗ Population file not found:", pop_file, "\n")
}
cat("\n")


# ============================================================================
# SECTION 8: Hospital Data (Procedures + Diagnoses)
# ============================================================================

cat("============================================================\n")
cat("SECTION 8: Hospital Procedures & Diagnosis Data\n")
cat("============================================================\n\n")

# --- 8A: Procedures Cubes ---
cat("--- 8A: Hospital Procedures Cubes ---\n\n")

proc_files <- list.files(PATHS$hosp_proc, pattern = "\\.xlsx$", full.names = TRUE)

if (length(proc_files) > 0) {
  cat("  Found", length(proc_files), "procedure cube files.\n")
  
  # Profile the first file to understand structure
  cat("  Profiling:", basename(proc_files[1]), "\n")
  prof <- profile_excel(proc_files[1])
  if (!is.null(prof$sheet_names)) {
    cat("  Sheets:", paste(prof$sheet_names, collapse = " | "), "\n\n")
  }
  
  # Read all procedure cubes and combine
  all_proc <- list()
  
  for (f in proc_files) {
    fname <- basename(f)
    # Extract the year from filename (e.g., "2019-20")
    year_range <- str_extract(fname, "\\d{4}-\\d{2}")
    
    cat("  Reading:", fname, "(", year_range, ")... ")
    
    sheets <- tryCatch(excel_sheets(f), error = function(e) character(0))
    # Use the first data sheet (skip contents/notes)
    data_sheets <- sheets[!str_detect(tolower(sheets), "content|note|about|glossary")]
    if (length(data_sheets) == 0) data_sheets <- sheets
    
    df <- read_excel_smart(f, sheet = data_sheets[1])
    
    if (!is.null(df) && nrow(df) > 0) {
      df <- clean_names(df) %>%
        mutate(.year_range = year_range, .source_file = fname)
      all_proc[[year_range]] <- df
      cat("OK —", nrow(df), "rows\n")
    } else {
      cat("empty/failed\n")
    }
  }
  
  if (length(all_proc) > 0) {
    # Check if column structures match across years
    all_proc_cols <- lapply(all_proc, names)
    common_proc_cols <- Reduce(intersect, all_proc_cols)
    
    if (length(common_proc_cols) > 2) {
      combined_proc <- bind_rows(
        lapply(all_proc, function(df) df %>% select(any_of(common_proc_cols)))
      )
      write_csv(combined_proc, "outputs/exploratory/hospital_procedures_combined.csv")
      cat("  ✓ Saved combined:", nrow(combined_proc), "rows across", 
          length(all_proc), "years\n")
      cat("  → outputs/exploratory/hospital_procedures_combined.csv\n")
    } else {
      cat("  ⚠ Column structures differ across years. Saving separately.\n")
      for (yr in names(all_proc)) {
        outfile <- paste0("outputs/exploratory/hospital_procedures_", 
                           str_replace_all(yr, "-", "_"), ".csv")
        write_csv(all_proc[[yr]], outfile)
        cat("  → Saved:", outfile, "\n")
      }
    }
  }
  
} else {
  cat("  ✗ No procedure cube files found in:", PATHS$hosp_proc, "\n")
}
cat("\n")

# --- 8B: Principal Diagnosis Cubes ---
cat("--- 8B: Principal Diagnosis Cubes ---\n\n")

diag_files <- list.files(PATHS$hosp_diag, pattern = "\\.xlsx$", full.names = TRUE)

if (length(diag_files) > 0) {
  cat("  Found", length(diag_files), "diagnosis cube files.\n")
  
  all_diag <- list()
  
  for (f in diag_files) {
    fname <- basename(f)
    year_range <- str_extract(fname, "\\d{4}-\\d{2}")
    
    cat("  Reading:", fname, "(", year_range, ")... ")
    
    sheets <- tryCatch(excel_sheets(f), error = function(e) character(0))
    data_sheets <- sheets[!str_detect(tolower(sheets), "content|note|about|glossary")]
    if (length(data_sheets) == 0) data_sheets <- sheets
    
    df <- read_excel_smart(f, sheet = data_sheets[1])
    
    if (!is.null(df) && nrow(df) > 0) {
      df <- clean_names(df) %>%
        mutate(.year_range = year_range, .source_file = fname)
      all_diag[[year_range]] <- df
      cat("OK —", nrow(df), "rows\n")
    } else {
      cat("empty/failed\n")
    }
  }
  
  if (length(all_diag) > 0) {
    all_diag_cols <- lapply(all_diag, names)
    common_diag_cols <- Reduce(intersect, all_diag_cols)
    
    if (length(common_diag_cols) > 2) {
      combined_diag <- bind_rows(
        lapply(all_diag, function(df) df %>% select(any_of(common_diag_cols)))
      )
      write_csv(combined_diag, "outputs/exploratory/hospital_diagnoses_combined.csv")
      cat("  ✓ Saved combined:", nrow(combined_diag), "rows across", 
          length(all_diag), "years\n")
      cat("  → outputs/exploratory/hospital_diagnoses_combined.csv\n")
    } else {
      cat("  ⚠ Column structures differ across years. Saving separately.\n")
      for (yr in names(all_diag)) {
        outfile <- paste0("outputs/exploratory/hospital_diagnoses_", 
                           str_replace_all(yr, "-", "_"), ".csv")
        write_csv(all_diag[[yr]], outfile)
        cat("  → Saved:", outfile, "\n")
      }
    }
  }
  
} else {
  cat("  ✗ No diagnosis cube files found in:", PATHS$hosp_diag, "\n")
}
cat("\n")

# --- 8C: Admitted Patient Data ---
cat("--- 8C: Admitted Patient Care Data ---\n\n")

admit_files <- list.files(PATHS$hosp_admit, pattern = "\\.xlsx$", full.names = TRUE)

if (length(admit_files) > 0) {
  for (f in admit_files) {
    cat("  Reading:", basename(f), "\n")
    sheets <- tryCatch(excel_sheets(f), error = function(e) character(0))
    cat("  Sheets:", paste(head(sheets, 8), collapse = " | "), "\n")
    
    data_sheets <- sheets[!str_detect(tolower(sheets), 
                                       "content|note|about|glossary|source|method")]
    
    for (s in data_sheets) {
      df <- read_excel_smart(f, sheet = s)
      if (!is.null(df) && nrow(df) > 1) {
        safe_name <- str_replace_all(
          paste0(tools::file_path_sans_ext(basename(f)), "_", s),
          "[^a-zA-Z0-9]", "_"
        ) %>% str_to_lower()
        outfile <- paste0("outputs/exploratory/admitted_patient_", safe_name, ".csv")
        write_csv(clean_names(df), outfile)
        cat("    Sheet '", s, "':", nrow(df), "rows → saved\n")
      }
    }
    cat("\n")
  }
} else {
  cat("  ✗ No admitted patient files found in:", PATHS$hosp_admit, "\n")
}


# ============================================================================
# SECTION 9: DATA READINESS SUMMARY
# ============================================================================

cat("\n")
cat("============================================================\n")
cat("FINAL DATA READINESS SUMMARY\n")
cat("============================================================\n\n")

write_log("")
write_log("============================================================")
write_log("FINAL DATA READINESS SUMMARY")
write_log("============================================================")
write_log("")

check_and_report <- function(filepath, label) {
  if (file.exists(filepath)) {
    nrows <- nrow(read_csv(filepath, show_col_types = FALSE))
    msg <- paste("  ✓", label, "—", nrows, "rows")
    write_log(msg)
  } else {
    msg <- paste("  ✗", label, "— NOT YET AVAILABLE")
    write_log(msg)
  }
}

write_log("CONFIRMATORY (Priority 1):")
check_and_report("outputs/confirmatory/confirmatory_h1_data.csv",
                 "H1: Hypertension MUR by year × sex")
check_and_report("outputs/confirmatory/confirmatory_h2_data.csv",
                 "H2: Geographic CVs with avoidability")
check_and_report("outputs/confirmatory/confirmatory_h3_data.csv",
                 "H3: Mental health MUR by year × sex")

write_log("")
write_log("EXPLORATORY (Priority 2):")
check_and_report("outputs/exploratory/deaths_underlying_national.csv",
                 "National underlying cause of death")
check_and_report("outputs/exploratory/deaths_underlying_by_state.csv",
                 "State-level underlying cause of death")
check_and_report("outputs/exploratory/deaths_multiple_causes.csv",
                 "Multiple causes of death (parsed)")
check_and_report("outputs/exploratory/avoidable_deaths_classification.csv",
                 "AIHW avoidability classification")
check_and_report("outputs/exploratory/pbs_antihypertensives.csv",
                 "PBS antihypertensive prescriptions")
check_and_report("outputs/exploratory/mental_health_ed_state_sex_age.csv",
                 "Mental health ED data by state")
check_and_report("outputs/exploratory/burden_of_disease_dalys.csv",
                 "Burden of disease (DALYs)")
check_and_report("outputs/exploratory/population_estimates.csv",
                 "ABS population estimates")
check_and_report("outputs/exploratory/hospital_procedures_combined.csv",
                 "Hospital procedures (combined)")
check_and_report("outputs/exploratory/hospital_diagnoses_combined.csv",
                 "Hospital diagnoses (combined)")

write_log("")
write_log("RAW EXTRACTS (may need manual structuring):")

raw_confirms <- list.files("outputs/confirmatory", pattern = "^raw_", full.names = TRUE)
if (length(raw_confirms) > 0) {
  for (f in raw_confirms) {
    nrows <- nrow(read_csv(f, show_col_types = FALSE))
    write_log(paste("  →", basename(f), "—", nrows, "rows"))
  }
} else {
  write_log("  (none)")
}

write_log("")
write_log("============================================================")
write_log("NEXT STEPS")
write_log("============================================================")
write_log("")
write_log("1. Review outputs/data_structure_report.txt for file structures.")
write_log("2. Check any raw_* files in outputs/confirmatory/ — these may need")
write_log("   manual column mapping if auto-detection didn't find the right columns.")
write_log("3. If H1/H3 data couldn't be auto-extracted, the raw extracts contain")
write_log("   the relevant rows from Cube 10. You'll need to identify which columns")
write_log("   represent underlying vs multiple cause counts and restructure.")
write_log("4. For H2, if the avoidability classification didn't auto-match,")
write_log("   create a manual concordance between condition names.")
write_log("5. Once confirmatory data is ready, run Scripts 13–16 in order.")
write_log("")

# Close log file
close(log_file)

cat("\n✓ Script 12 complete.\n")
cat("  Full structure report saved to: outputs/data_structure_report.txt\n")
cat("  All extracted data saved to: outputs/confirmatory/ and outputs/exploratory/\n")
