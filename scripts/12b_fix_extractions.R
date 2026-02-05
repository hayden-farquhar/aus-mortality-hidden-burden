###############################################################################
# Script 12b: Fix H1/H5 extraction + PBS data
# Run AFTER Script 12 has completed successfully
###############################################################################

library(tidyverse)
library(readxl)

# --- PATH CONFIGURATION ---
# Update this to match your setup (include trailing space if your folder has one)
DATA_ROOT <- "../Confirmatory exploration/Data from sources"
OUTPUT_DIR <- "outputs"

cat("============================================================\n")
cat("Script 12b: Structuring H1, H5, and PBS data\n")
cat("============================================================\n\n")

###############################################################################
# PART 1: Structure H1 (Hypertension MUR) from raw Table 10.2 extract
###############################################################################

cat("--- PART 1: Structuring H1 (Hypertension MUR) ---\n\n")

h1_file <- file.path(OUTPUT_DIR, "confirmatory", "raw_hypertension_Table_10_2.csv")

if (file.exists(h1_file)) {
  
  # Read raw extract — no header row (column names are generic)
  h1_raw <- read_csv(h1_file, col_names = FALSE, show_col_types = FALSE)
  
  # Assign proper column names based on Table 10.2 structure
  colnames(h1_raw) <- c("cause", "unit", 
                         "underlying_male", "underlying_female", "underlying_persons",
                         "multiple_male", "multiple_female", "multiple_persons",
                         "ratio")
  
  # Convert counts to numeric (some may have formatting issues)
  h1_raw <- h1_raw %>%
    mutate(across(c(underlying_male:multiple_persons), ~ as.numeric(gsub("[^0-9.]", "", as.character(.)))),
           ratio = as.numeric(ifelse(ratio == "—" | ratio == "\u2014", NA, ratio)))
  
  # Extract the totals row and hypertensive diseases rows
  h1_totals <- h1_raw %>% filter(str_detect(cause, "^total_deaths|^Total"))
  h1_hypertension <- h1_raw %>% filter(str_detect(cause, "I1[0-5]|[Hh]ypertens"))
  
  # Compute MUR by sex for hypertensive diseases (I10-I15 aggregate)
  h1_main <- h1_hypertension %>%
    filter(str_detect(cause, "I10-I15|Hypertensive diseases")) %>%
    select(cause, starts_with("underlying"), starts_with("multiple"), ratio) %>%
    mutate(
      mur_male   = multiple_male / underlying_male,
      mur_female = multiple_female / underlying_female,
      mur_persons = multiple_persons / underlying_persons,
      data_year  = 2023  # ABS 2024 release covers deaths registered in 2023
    )
  
  # Also compute MUR for each sub-condition
  h1_detail <- h1_hypertension %>%
    filter(!str_detect(cause, "^total")) %>%
    mutate(
      mur_male   = ifelse(underlying_male > 0, multiple_male / underlying_male, NA),
      mur_female = ifelse(underlying_female > 0, multiple_female / underlying_female, NA),
      mur_persons = ifelse(underlying_persons > 0, multiple_persons / underlying_persons, NA),
      data_year  = 2023
    )
  
  # Save structured output
  write_csv(h1_detail, file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h1_data.csv"))
  
  cat("  ✓ H1 data structured:\n")
  cat("    Key finding — Hypertensive diseases (I10-I15) overall MUR:\n")
  if (nrow(h1_main) > 0) {
    cat(sprintf("      Males:   %.1f (underlying: %s, multiple: %s)\n", 
                h1_main$mur_male[1], 
                format(h1_main$underlying_male[1], big.mark = ","),
                format(h1_main$multiple_male[1], big.mark = ",")))
    cat(sprintf("      Females: %.1f (underlying: %s, multiple: %s)\n", 
                h1_main$mur_female[1],
                format(h1_main$underlying_female[1], big.mark = ","),
                format(h1_main$multiple_female[1], big.mark = ",")))
    cat(sprintf("      Persons: %.1f (underlying: %s, multiple: %s)\n", 
                h1_main$mur_persons[1],
                format(h1_main$underlying_persons[1], big.mark = ","),
                format(h1_main$multiple_persons[1], big.mark = ",")))
  }
  cat("    Saved to: outputs/confirmatory/confirmatory_h1_data.csv\n\n")
  
  # Print sub-condition MURs
  cat("    Sub-condition MURs:\n")
  h1_detail %>%
    filter(str_detect(cause, "I1[0-5]\\)")) %>%
    select(cause, mur_persons) %>%
    mutate(cause = str_extract(cause, "^[^(]+")) %>%
    print(n = 20)
  cat("\n")
  
} else {
  cat("  ✗ Raw H1 extract not found. Re-run Script 12 first.\n\n")
}


###############################################################################
# PART 2: Structure H5 (Mental Health MUR) from raw Table 10.2 extract
###############################################################################

cat("--- PART 2: Structuring H5 (Mental Health MUR) ---\n\n")

h5_file <- file.path(OUTPUT_DIR, "confirmatory", "raw_mental_health_Table_10_2.csv")

if (file.exists(h5_file)) {
  
  h5_raw <- read_csv(h5_file, col_names = FALSE, show_col_types = FALSE)
  
  colnames(h5_raw) <- c("cause", "unit",
                         "underlying_male", "underlying_female", "underlying_persons",
                         "multiple_male", "multiple_female", "multiple_persons",
                         "ratio")
  
  h5_raw <- h5_raw %>%
    mutate(across(c(underlying_male:multiple_persons), ~ as.numeric(gsub("[^0-9.]", "", as.character(.)))),
           ratio = as.numeric(ifelse(ratio == "—" | ratio == "\u2014", NA, ratio)))
  
  # Chapter-level mental health MUR
  h5_chapter <- h5_raw %>%
    filter(str_detect(cause, "CHAPTER V|F00-F99")) %>%
    mutate(
      mur_male   = multiple_male / underlying_male,
      mur_female = multiple_female / underlying_female,
      mur_persons = multiple_persons / underlying_persons,
      data_year  = 2023
    )
  
  # Sub-group MURs (F00-F09, F10-F19, F20-F29, F30-F39, F40-F48, etc.)
  h5_subgroups <- h5_raw %>%
    filter(str_detect(cause, "^[A-Z].*\\(F\\d{2}-F\\d{2}\\)")) %>%
    mutate(
      mur_male   = ifelse(underlying_male > 0, multiple_male / underlying_male, NA),
      mur_female = ifelse(underlying_female > 0, multiple_female / underlying_female, NA),
      mur_persons = ifelse(underlying_persons > 0, multiple_persons / underlying_persons, NA),
      icd_range  = str_extract(cause, "F\\d{2}-F\\d{2}"),
      condition_group = str_extract(cause, "^[^(]+"),
      data_year  = 2023
    )
  
  # Full detail (all F-codes)
  h5_detail <- h5_raw %>%
    filter(str_detect(cause, "\\(F\\d{2}")) %>%
    mutate(
      mur_male   = ifelse(underlying_male > 0, multiple_male / underlying_male, NA),
      mur_female = ifelse(underlying_female > 0, multiple_female / underlying_female, NA),
      mur_persons = ifelse(underlying_persons > 0, multiple_persons / underlying_persons, NA),
      icd_code   = str_extract(cause, "F\\d{2}(-F\\d{2})?"),
      data_year  = 2023
    )
  
  write_csv(h5_detail, file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h5_data.csv"))
  
  cat("  ✓ H5 data structured:\n")
  cat("    Chapter V Mental & Behavioural Disorders (F00-F99) overall MUR:\n")
  if (nrow(h5_chapter) > 0) {
    cat(sprintf("      Males:   %.1f (underlying: %s, multiple: %s)\n",
                h5_chapter$mur_male[1],
                format(h5_chapter$underlying_male[1], big.mark = ","),
                format(h5_chapter$multiple_male[1], big.mark = ",")))
    cat(sprintf("      Females: %.1f (underlying: %s, multiple: %s)\n",
                h5_chapter$mur_female[1],
                format(h5_chapter$underlying_female[1], big.mark = ","),
                format(h5_chapter$multiple_female[1], big.mark = ",")))
    cat(sprintf("      Persons: %.1f (underlying: %s, multiple: %s)\n",
                h5_chapter$mur_persons[1],
                format(h5_chapter$underlying_persons[1], big.mark = ","),
                format(h5_chapter$multiple_persons[1], big.mark = ",")))
  }
  cat("    Saved to: outputs/confirmatory/confirmatory_h5_data.csv\n\n")
  
  # Print sub-group MURs
  cat("    Sub-group MURs (persons):\n")
  h5_subgroups %>%
    select(condition_group, icd_range, underlying_persons, multiple_persons, mur_persons) %>%
    arrange(desc(mur_persons)) %>%
    print(n = 20)
  cat("\n")
  
} else {
  cat("  ✗ Raw H5 extract not found. Re-run Script 12 first.\n\n")
}


###############################################################################
# PART 3: Fix PBS Antihypertensive Extraction
###############################################################################

cat("--- PART 3: PBS Antihypertensive Data ---\n\n")

pbs_atc2_file <- file.path(DATA_ROOT, "2A PBS Prescribing Statistics",
                           "AIHW-HWE-098-PBS-ATC2-prescriptions-monthly-data.xlsx")

if (file.exists(pbs_atc2_file)) {
  
  cat("  Reading PBS ATC2 file sheet names...\n")
  pbs_sheets <- excel_sheets(pbs_atc2_file)
  cat("  Sheets:", paste(pbs_sheets, collapse = " | "), "\n")
  
  # The Contents sheet is just metadata. The actual data is in Table 1-6.
  # Table 1 = national data (most likely what we need)
  # Let's read the first few rows of Table 1 to find the header row
  
  cat("  Reading Table 1 preview to find header row...\n")
  preview <- read_excel(pbs_atc2_file, sheet = "Table 1", n_max = 15, col_names = FALSE)
  
  cat("  First 15 rows of Table 1:\n")
  for (i in 1:min(15, nrow(preview))) {
    row_vals <- paste(preview[i, 1:min(6, ncol(preview))], collapse = " | ")
    cat(sprintf("    Row %2d: %s\n", i, row_vals))
  }
  cat("\n")
  
  # Find the header row (look for a row containing "ATC" or "Year" or "Month")
  header_row <- NA
  for (i in 1:15) {
    row_text <- paste(unlist(preview[i, ]), collapse = " ")
    if (str_detect(tolower(row_text), "atc|year|month|medicine|prescriptions")) {
      header_row <- i
      cat(sprintf("  Detected header at row %d: %s\n", i, row_text))
      break
    }
  }
  
  if (!is.na(header_row)) {
    cat(sprintf("  Reading full Table 1 (skipping %d rows)...\n", header_row))
    cat("  ⚠ This may take several minutes for this large file.\n")
    
    pbs_data <- read_excel(pbs_atc2_file, sheet = "Table 1", skip = header_row)
    
    cat(sprintf("  Read %s rows × %d columns\n", format(nrow(pbs_data), big.mark = ","), ncol(pbs_data)))
    cat("  Column names:", paste(names(pbs_data)[1:min(10, ncol(pbs_data))], collapse = " | "), "\n")
    
    # Show unique ATC values to find the right column
    atc_cols <- names(pbs_data)[str_detect(tolower(names(pbs_data)), "atc|medicine|group|category")]
    if (length(atc_cols) > 0) {
      cat("  ATC-related columns:", paste(atc_cols, collapse = ", "), "\n")
      for (col in atc_cols[1:min(2, length(atc_cols))]) {
        unique_vals <- unique(pbs_data[[col]])
        cat(sprintf("    '%s' has %d unique values. First 20:\n", col, length(unique_vals)))
        cat("   ", paste(head(unique_vals, 20), collapse = "\n    "), "\n")
      }
    }
    
    # Try to filter for antihypertensives (ATC codes C02-C09)
    # Search all character columns for C02, C03, ... C09 patterns
    antihypertensive_codes <- paste0("C0", 2:9)
    found <- FALSE
    
    for (col in names(pbs_data)) {
      if (is.character(pbs_data[[col]]) || is.factor(pbs_data[[col]])) {
        matches <- pbs_data %>%
          filter(str_detect(as.character(.data[[col]]), 
                            paste(antihypertensive_codes, collapse = "|")))
        if (nrow(matches) > 0) {
          cat(sprintf("\n  ✓ Found %s rows matching C02-C09 in column '%s'\n", 
                      format(nrow(matches), big.mark = ","), col))
          
          write_csv(matches, file.path(OUTPUT_DIR, "exploratory", "pbs_antihypertensives.csv"))
          cat("    Saved to: outputs/exploratory/pbs_antihypertensives.csv\n")
          
          # Show a summary
          cat("    ATC code breakdown:\n")
          matches %>%
            mutate(atc2 = str_extract(as.character(.data[[col]]), "C0[2-9]")) %>%
            count(atc2) %>%
            print(n = 20)
          
          found <- TRUE
          break
        }
      }
    }
    
    if (!found) {
      cat("\n  ⚠ No C02-C09 codes found. Saving column structure for manual inspection.\n")
      # Save a sample for manual review
      write_csv(head(pbs_data, 100), file.path(OUTPUT_DIR, "exploratory", "pbs_table1_sample.csv"))
      cat("    Saved first 100 rows to: outputs/exploratory/pbs_table1_sample.csv\n")
    }
    
  } else {
    cat("  ⚠ Could not auto-detect header row. Saving raw preview for manual inspection.\n")
    write_csv(preview, file.path(OUTPUT_DIR, "exploratory", "pbs_table1_preview.csv"))
  }
  
} else {
  cat("  ✗ PBS ATC2 file not found at expected path.\n")
}


###############################################################################
# PART 4: Check Mental Health ED folder
###############################################################################

cat("\n--- PART 4: Mental Health ED Data ---\n\n")

mh_base <- file.path(DATA_ROOT, "2B AIHW Mental Health Services")

if (dir.exists(mh_base)) {
  cat("  Found base folder. Contents:\n")
  mh_contents <- list.files(mh_base, recursive = FALSE)
  for (f in mh_contents) {
    cat(sprintf("    %s\n", f))
  }
  
  # Try to find the ED subfolder with fuzzy matching
  ed_folders <- mh_contents[str_detect(tolower(mh_contents), "ed|emergency")]
  if (length(ed_folders) > 0) {
    cat(sprintf("\n  Found ED-related subfolder(s): %s\n", paste(ed_folders, collapse = ", ")))
    
    for (ed_folder in ed_folders) {
      ed_path <- file.path(mh_base, ed_folder)
      if (dir.exists(ed_path)) {
        ed_files <- list.files(ed_path)
        cat(sprintf("  Contents of '%s':\n", ed_folder))
        for (f in ed_files) {
          cat(sprintf("    %s\n", f))
        }
      }
    }
  } else {
    cat("  No ED-specific subfolder found. You may need to check the folder structure.\n")
  }
} else {
  cat("  ✗ Mental health services folder not found at:", mh_base, "\n")
}


###############################################################################
# PART 5: Summary and temporal data note
###############################################################################

cat("\n============================================================\n")
cat("IMPORTANT NOTE ON TEMPORAL DATA\n")
cat("============================================================\n\n")
cat("The ABS 2024 release of Cube 10 (Multiple Causes of Death) contains\n")
cat("data for a SINGLE YEAR only. To test H1 and H5 as temporal trends\n")
cat("(MUR increasing over time), you would need to either:\n\n")
cat("  Option A: Download historical Cube 10 files from prior ABS releases\n")
cat("            (2023 release, 2022 release, etc.) from the ABS website.\n")
cat("            Each year's release should have a Cube 10 with that year's\n")
cat("            multiple cause data.\n\n")
cat("  Option B: Reframe H1 and H5 as cross-sectional hypotheses:\n")
cat("            e.g., 'Hypertension has a higher MUR than cardiovascular\n")
cat("            diseases overall' or 'Mental health MUR varies by sex'\n\n")
cat("  Option C: Use Cube 14 (year of occurrence) which may have multi-year\n")
cat("            data, though it tracks underlying cause only.\n\n")
cat("Current data allows: MUR by sex for a single year (2023 deaths).\n")
cat("This is still valuable for a cross-sectional analysis.\n\n")

cat("============================================================\n")
cat("Script 12b complete.\n")
cat("============================================================\n")
