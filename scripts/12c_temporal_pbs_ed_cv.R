###############################################################################
# Script 12c: Temporal data (Cube 14), PBS fix, ED fix, full CV MUR
# Run AFTER Scripts 12 and 12b
###############################################################################

library(tidyverse)
library(readxl)

DATA_ROOT <- "../Confirmatory exploration/Data from sources"
OUTPUT_DIR <- "outputs"

cat("============================================================\n")
cat("Script 12c: Temporal + PBS + ED + Comparative MUR\n")
cat("============================================================\n\n")


###############################################################################
# PART 1: Cube 14 — Temporal underlying cause data (2014–2024)
###############################################################################

cat("--- PART 1: Cube 14 — Temporal Underlying Cause Data ---\n\n")

cube14_file <- file.path(DATA_ROOT, "1A ABS Causes of Death Data",
                         "2024_14 Causes of death by year of occurrence (Australia).xlsx")

if (file.exists(cube14_file)) {
  
  # ---- 1A: Read Table 14.1 structure ----
  # Row 5 = years, Row 6 = sex headers, Row 7+ = data
  # Columns: cause_description, unit, then 3 columns per year (M, F, P)
  
  cat("  Reading Table 14.1...\n")
  raw <- read_excel(cube14_file, sheet = "Table 14.1", col_names = FALSE)
  
  # Find the year row (contains "2014", "2015", etc.)
  year_row <- NULL
  sex_row <- NULL
  for (i in 1:10) {
    row_text <- paste(unlist(raw[i, ]), collapse = " ")
    if (str_detect(row_text, "2014.*2015.*2016")) {
      year_row <- i
      sex_row <- i + 1
      cat(sprintf("  Found year headers at row %d, sex headers at row %d\n", i, i + 1))
      break
    }
  }
  
  if (!is.null(year_row)) {
    
    # Extract year and sex labels
    years_raw <- unlist(raw[year_row, ])
    sex_raw   <- unlist(raw[sex_row, ])
    
    # Build column names: cause, unit, then year_sex combinations
    # Years appear at the start of each 3-column group, NAs for the 2nd and 3rd
    years <- c()
    current_year <- NA
    for (j in 3:length(years_raw)) {
      if (!is.na(years_raw[j])) current_year <- years_raw[j]
      years <- c(years, current_year)
    }
    
    sexes <- sex_raw[3:length(sex_raw)]
    
    col_names <- c("cause", "unit")
    for (j in seq_along(years)) {
      sex_label <- tolower(as.character(sexes[j]))
      sex_label <- case_when(
        str_detect(sex_label, "female")  ~ "female",
        str_detect(sex_label, "person")  ~ "persons",
        str_detect(sex_label, "male")    ~ "male",
        TRUE ~ sex_label
      )
      col_names <- c(col_names, paste0("y", years[j], "_", sex_label))
    }
    
    # Safety check: ensure no duplicate column names
    if (any(duplicated(col_names))) {
      cat("  ⚠ Duplicate column names detected, making unique\n")
      col_names <- make.unique(col_names)
    }
    
    # Read data rows (start after sex header row)
    data_start <- sex_row + 1
    data_rows <- raw[data_start:nrow(raw), ]
    
    # Assign column names (handle length mismatch)
    if (ncol(data_rows) >= length(col_names)) {
      data_rows <- data_rows[, 1:length(col_names)]
      colnames(data_rows) <- col_names
    } else {
      col_names <- col_names[1:ncol(data_rows)]
      colnames(data_rows) <- col_names
    }
    
    # Clean: remove empty rows and pure header rows
    data_rows <- data_rows %>%
      filter(!is.na(cause), 
             cause != "Causes of death",
             !str_detect(cause, "^Cause of death"))
    
    # Keep only cause rows (those with "no." in unit column or numeric data)
    data_rows <- data_rows %>%
      filter(unit == "no." | !is.na(unit))
    
    cat(sprintf("  Parsed %d cause rows × %d years (2014–2024)\n", nrow(data_rows), 11))
    
    # ---- 1B: Pivot to long format ----
    cube14_long <- data_rows %>%
      pivot_longer(cols = starts_with("y"), 
                   names_to = "year_sex", 
                   values_to = "deaths") %>%
      mutate(
        year = as.integer(str_extract(year_sex, "\\d{4}")),
        sex  = str_extract(year_sex, "(?<=_).*"),
        deaths = as.numeric(deaths),
        icd_code = str_extract(cause, "\\([A-Z]\\d{2}(-[A-Z]\\d{2})?\\)"),
        icd_code = str_remove_all(icd_code, "[()]")
      ) %>%
      select(cause, icd_code, year, sex, deaths) %>%
      filter(!is.na(year))
    
    cat(sprintf("  Long format: %s rows\n", format(nrow(cube14_long), big.mark = ",")))
    
    # Save full dataset
    write_csv(cube14_long, file.path(OUTPUT_DIR, "exploratory", "cube14_temporal_all_causes.csv"))
    cat("  Saved: outputs/exploratory/cube14_temporal_all_causes.csv\n\n")
    
    # ---- 1C: Extract H1c — Hypertension temporal trend ----
    h1c <- cube14_long %>%
      filter(str_detect(cause, "I1[0-5]|[Hh]ypertens"))
    
    if (nrow(h1c) > 0) {
      write_csv(h1c, file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h1c_temporal.csv"))
      cat("  ✓ H1c (Hypertension temporal): ", nrow(h1c), "rows\n")
      
      # Quick summary
      h1c_summary <- h1c %>%
        filter(str_detect(cause, "I10-I15"), sex == "persons") %>%
        arrange(year)
      
      if (nrow(h1c_summary) > 0) {
        cat("    Hypertensive diseases (I10-I15) as underlying cause, persons:\n")
        for (i in 1:nrow(h1c_summary)) {
          cat(sprintf("      %d: %s deaths\n", h1c_summary$year[i], 
                      format(h1c_summary$deaths[i], big.mark = ",")))
        }
      }
    } else {
      cat("  ⚠ No hypertension rows found in Cube 14\n")
    }
    
    # ---- 1D: Extract H5c — Mental health temporal trend ----
    h5c <- cube14_long %>%
      filter(str_detect(cause, "\\(F\\d{2}|F00-F99|Mental and behav"))
    
    if (nrow(h5c) > 0) {
      write_csv(h5c, file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h5c_temporal.csv"))
      cat("\n  ✓ H5c (Mental health temporal): ", nrow(h5c), "rows\n")
      
      h5c_summary <- h5c %>%
        filter(str_detect(cause, "F00-F99|CHAPTER V"), sex == "persons") %>%
        arrange(year)
      
      if (nrow(h5c_summary) > 0) {
        cat("    Chapter V Mental & Behavioural (F00-F99) as underlying cause, persons:\n")
        for (i in 1:nrow(h5c_summary)) {
          cat(sprintf("      %d: %s deaths\n", h5c_summary$year[i],
                      format(h5c_summary$deaths[i], big.mark = ",")))
        }
      }
    } else {
      cat("  ⚠ No mental health rows found in Cube 14\n")
    }
    
    # ---- 1E: Also extract IHD and cerebrovascular for comparison ----
    cv_temporal <- cube14_long %>%
      filter(str_detect(cause, "I2[0-5]|I6[0-9]|[Ii]schaemic heart|[Cc]erebrovascular|I00-I99"))
    
    if (nrow(cv_temporal) > 0) {
      write_csv(cv_temporal, file.path(OUTPUT_DIR, "exploratory", "cube14_cv_temporal.csv"))
      cat("\n  ✓ CV comparison temporal data:", nrow(cv_temporal), "rows\n")
    }
    
  } else {
    cat("  ⚠ Could not find year headers in Table 14.1\n")
  }
  
  # ---- 1F: Check other tables in Cube 14 ----
  cat("\n  Checking other Cube 14 tables...\n")
  sheets <- excel_sheets(cube14_file)
  for (s in sheets[str_detect(sheets, "14\\.")]) {
    preview <- read_excel(cube14_file, sheet = s, n_max = 5, col_names = FALSE)
    desc <- as.character(preview[1, 1])
    cat(sprintf("    %s: %s\n", s, substr(desc, 1, 100)))
  }
  
} else {
  cat("  ✗ Cube 14 file not found\n")
}


###############################################################################
# PART 2: Full Cardiovascular MUR from Cube 10 (for H1a comparisons)
###############################################################################

cat("\n\n--- PART 2: Full Cardiovascular MUR from Cube 10 ---\n\n")

cube10_file <- file.path(DATA_ROOT, "1A ABS Causes of Death Data",
                         "2024_10 Multiple causes of death (Australia).xlsx")

if (file.exists(cube10_file)) {
  
  cat("  Reading Table 10.2 (underlying + multiple counts by sex)...\n")
  
  # Read full Table 10.2
  raw10 <- read_excel(cube10_file, sheet = "Table 10.2", col_names = FALSE)
  
  # Find the data start (look for "Total deaths" or "total" row)
  data_start <- NULL
  for (i in 1:20) {
    val <- as.character(raw10[i, 1])
    if (is.na(val)) next
    val <- tolower(val)
    if (str_detect(val, "total.?death|^total")) {
      data_start <- i
      break
    }
  }
  
  if (is.null(data_start)) {
    # Try finding header row instead
    for (i in 1:15) {
      val <- paste(na.omit(unlist(raw10[i, ])), collapse = " ")
      if (str_detect(tolower(val), "male.*female.*person")) {
        data_start <- i + 1
        break
      }
    }
  }
  
  if (!is.null(data_start)) {
    # Read from data start
    t10_2 <- raw10[data_start:nrow(raw10), ]
    
    # Assign column names: cause, unit, und_m, und_f, und_p, mult_m, mult_f, mult_p, ratio
    if (ncol(t10_2) >= 9) {
      colnames(t10_2) <- c("cause", "unit", 
                           "underlying_male", "underlying_female", "underlying_persons",
                           "multiple_male", "multiple_female", "multiple_persons",
                           "ratio")
    } else if (ncol(t10_2) >= 8) {
      colnames(t10_2) <- c("cause", "unit",
                           "underlying_male", "underlying_female", "underlying_persons",
                           "multiple_male", "multiple_female", "multiple_persons")
      t10_2$ratio <- NA
    }
    
    # Clean and convert
    t10_2 <- t10_2 %>%
      filter(!is.na(cause), cause != "") %>%
      mutate(across(c(underlying_male:multiple_persons), 
                    ~ as.numeric(gsub("[^0-9.]", "", as.character(.)))),
             ratio = as.numeric(ifelse(ratio %in% c("—", "\u2014", ".."), NA, ratio)),
             icd_code = str_extract(cause, "\\([A-Z]\\d{2}[^)]*\\)"),
             icd_code = str_remove_all(icd_code, "[()]"))
    
    # ---- 2A: Extract Chapter IX — Circulatory system (I00-I99) ----
    # Find rows between Chapter IX header and Chapter X header
    ch9_start <- which(str_detect(t10_2$cause, "CHAPTER IX|I00-I99"))
    ch10_start <- which(str_detect(t10_2$cause, "CHAPTER X|J00-J99"))
    
    if (length(ch9_start) > 0) {
      end_row <- ifelse(length(ch10_start) > 0, ch10_start[1] - 1, nrow(t10_2))
      
      cv_mur <- t10_2[ch9_start[1]:end_row, ] %>%
        filter(!is.na(underlying_persons) | !is.na(multiple_persons)) %>%
        mutate(
          mur_male = ifelse(underlying_male > 0, multiple_male / underlying_male, NA),
          mur_female = ifelse(underlying_female > 0, multiple_female / underlying_female, NA),
          mur_persons = ifelse(underlying_persons > 0, multiple_persons / underlying_persons, NA)
        )
      
      write_csv(cv_mur, file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h1a_cv_mur.csv"))
      
      cat(sprintf("  ✓ Extracted %d cardiovascular condition rows\n", nrow(cv_mur)))
      cat("    Key MUR comparisons (persons):\n\n")
      
      # Show the major CV conditions
      key_conditions <- cv_mur %>%
        filter(str_detect(cause, "I00-I99|I10-I15|I20-I25|I60-I69|I26|I30-I52|I70-I79")) %>%
        select(cause, underlying_persons, multiple_persons, mur_persons) %>%
        arrange(desc(mur_persons))
      
      for (i in 1:nrow(key_conditions)) {
        cat(sprintf("      %-55s  Und: %6s  Mult: %6s  MUR: %5.1f\n",
                    substr(key_conditions$cause[i], 1, 55),
                    format(key_conditions$underlying_persons[i], big.mark = ","),
                    format(key_conditions$multiple_persons[i], big.mark = ","),
                    key_conditions$mur_persons[i]))
      }
    } else {
      cat("  ⚠ Could not locate Chapter IX in Table 10.2\n")
    }
    
    # ---- 2B: Extract full dataset for broader MUR analysis ----
    # Get chapter-level summaries for all chapters
    chapter_rows <- t10_2 %>%
      filter(str_detect(cause, "^CHAPTER"))
    
    if (nrow(chapter_rows) > 0) {
      chapter_mur <- chapter_rows %>%
        mutate(
          mur_male = ifelse(underlying_male > 0, multiple_male / underlying_male, NA),
          mur_female = ifelse(underlying_female > 0, multiple_female / underlying_female, NA),
          mur_persons = ifelse(underlying_persons > 0, multiple_persons / underlying_persons, NA)
        ) %>%
        arrange(desc(mur_persons))
      
      write_csv(chapter_mur, file.path(OUTPUT_DIR, "exploratory", "cube10_chapter_mur_summary.csv"))
      
      cat("\n    All-chapter MUR summary (persons, sorted by MUR):\n\n")
      for (i in 1:nrow(chapter_mur)) {
        cat(sprintf("      %-65s MUR: %5.1f\n",
                    substr(chapter_mur$cause[i], 1, 65),
                    chapter_mur$mur_persons[i]))
      }
    }
    
    # ---- 2C: Save full Table 10.2 parsed ----
    full_mur <- t10_2 %>%
      mutate(
        mur_male = ifelse(underlying_male > 0, multiple_male / underlying_male, NA),
        mur_female = ifelse(underlying_female > 0, multiple_female / underlying_female, NA),
        mur_persons = ifelse(underlying_persons > 0, multiple_persons / underlying_persons, NA)
      )
    
    write_csv(full_mur, file.path(OUTPUT_DIR, "exploratory", "cube10_full_mur_table.csv"))
    cat(sprintf("\n  ✓ Full MUR table saved: %d rows\n", nrow(full_mur)))
    cat("    → outputs/exploratory/cube10_full_mur_table.csv\n")
    
  } else {
    cat("  ⚠ Could not find data start row in Table 10.2\n")
  }
  
} else {
  cat("  ✗ Cube 10 file not found\n")
}


###############################################################################
# PART 3: PBS Antihypertensive Prescriptions (fixed)
###############################################################################

cat("\n\n--- PART 3: PBS Prescriptions (fixed extraction) ---\n\n")

pbs_file <- file.path(DATA_ROOT, "2A PBS Prescribing Statistics",
                      "AIHW-HWE-098-PBS-ATC2-prescriptions-monthly-data.xlsx")

if (file.exists(pbs_file)) {
  
  cat("  Reading Table 1 (skipping title row)...\n")
  cat("  ⚠ This is a 389 MB file — may take several minutes.\n")
  
  pbs <- read_excel(pbs_file, sheet = "Table 1", skip = 1)
  
  cat(sprintf("  Read %d rows × %d columns\n", nrow(pbs), ncol(pbs)))
  cat(sprintf("  Columns: %s\n", paste(names(pbs)[1:5], collapse = " | ")))
  
  # The first column is the drug class name (not ATC code)
  drug_col <- names(pbs)[1]  # "Type of script"
  cat(sprintf("  Drug class column: '%s'\n", drug_col))
  
  # List unique drug classes
  unique_drugs <- unique(pbs[[drug_col]])
  cat(sprintf("  Found %d unique drug classes:\n", length(unique_drugs)))
  for (d in sort(unique_drugs)) {
    cat(sprintf("    %s\n", d))
  }
  
  # Antihypertensive-related classes at ATC2 level:
  # C02 = Antihypertensives
  # C03 = Diuretics  
  # C07 = Beta blocking agents
  # C08 = Calcium channel blockers
  # C09 = Agents acting on the renin-angiotensin system
  # Also relevant: C10 = Lipid modifying agents (for CV context)
  
  # Search by name patterns
  cv_patterns <- c(
    "antihypertensive", "diuretic", "beta.?block", "calcium channel",
    "renin.?angiotensin", "lipid.?modify", "cardiac", "vasodilat",
    "peripheral vasodilator"
  )
  
  cv_regex <- paste(cv_patterns, collapse = "|")
  
  cv_drugs <- pbs %>%
    filter(str_detect(tolower(.data[[drug_col]]), cv_regex))
  
  if (nrow(cv_drugs) > 0) {
    cat(sprintf("\n  ✓ Found %d rows matching cardiovascular drug classes\n", nrow(cv_drugs)))
    
    matched_classes <- unique(cv_drugs[[drug_col]])
    cat("    Matched classes:\n")
    for (d in matched_classes) {
      n_rows <- sum(cv_drugs[[drug_col]] == d)
      cat(sprintf("      %s (%d rows)\n", d, n_rows))
    }
    
    write_csv(cv_drugs, file.path(OUTPUT_DIR, "exploratory", "pbs_cardiovascular.csv"))
    cat("    Saved: outputs/exploratory/pbs_cardiovascular.csv\n")
    
  } else {
    cat("\n  ⚠ No cardiovascular drug classes matched.\n")
    cat("    Saving all unique drug class names for manual matching.\n")
  }
  
  # Also extract the renin-angiotensin system data specifically
  ras_drugs <- pbs %>%
    filter(str_detect(tolower(.data[[drug_col]]), "renin|angiotensin"))
  
  if (nrow(ras_drugs) > 0) {
    cat(sprintf("\n  ✓ Renin-angiotensin system (C09): %d rows\n", nrow(ras_drugs)))
    write_csv(ras_drugs, file.path(OUTPUT_DIR, "exploratory", "pbs_antihypertensives.csv"))
    cat("    Saved: outputs/exploratory/pbs_antihypertensives.csv\n")
    
    # Quick summary: total scripts per year (from monthly columns)
    month_cols <- names(ras_drugs)[str_detect(names(ras_drugs), "^[A-Z]{3}-\\d{4}$")]
    if (length(month_cols) > 0) {
      # Get national total (age = Total, value = Scripts per ERP or similar)
      ras_national <- ras_drugs %>%
        filter(.data[["Age group"]] == "Total",
               str_detect(.data[["Value"]], "Scripts"))
      
      if (nrow(ras_national) > 0) {
        cat("    National scripts per ERP (total, selected months):\n")
        sample_months <- month_cols[seq(1, length(month_cols), by = 12)]
        for (m in sample_months) {
          val <- ras_national[[m]][1]
          cat(sprintf("      %s: %.4f\n", m, as.numeric(val)))
        }
      }
    }
  }
  
} else {
  cat("  ✗ PBS ATC2 file not found\n")
}


###############################################################################
# PART 4: Mental Health ED Data (fixed path with em-dash)
###############################################################################

cat("\n\n--- PART 4: Mental Health ED Data ---\n\n")

# The folder uses an em-dash, not a hyphen
# Try both to be safe
mh_base <- file.path(DATA_ROOT, "2B AIHW Mental Health Services")

if (dir.exists(mh_base)) {
  # Find the ED subfolder (fuzzy match)
  subfolders <- list.files(mh_base)
  ed_folder <- subfolders[str_detect(tolower(subfolders), "ed.*states|states.*ed")]
  
  if (length(ed_folder) > 0) {
    ed_path <- file.path(mh_base, ed_folder[1])
    cat(sprintf("  Found ED folder: %s\n", ed_folder[1]))
    
    # Look for the state-level CSV
    ed_files <- list.files(ed_path, full.names = TRUE)
    state_file <- ed_files[str_detect(ed_files, "State_Sex_Age")]
    
    if (length(state_file) > 0) {
      cat(sprintf("  Reading: %s\n", basename(state_file[1])))
      
      ed_data <- read_csv(state_file[1], show_col_types = FALSE)
      cat(sprintf("  Rows: %d, Columns: %d\n", nrow(ed_data), ncol(ed_data)))
      cat(sprintf("  Column names: %s\n", paste(names(ed_data), collapse = " | ")))
      
      # Preview
      cat("  First 5 rows:\n")
      print(head(ed_data, 5), width = Inf)
      
      write_csv(ed_data, file.path(OUTPUT_DIR, "exploratory", "mental_health_ed_state.csv"))
      cat("\n  ✓ Saved: outputs/exploratory/mental_health_ed_state.csv\n")
      
    } else {
      cat("  ⚠ State-level ED CSV not found. Available files:\n")
      for (f in basename(ed_files)) cat(sprintf("    %s\n", f))
    }
    
    # Also read the main Excel tables file if present
    xlsx_file <- ed_files[str_detect(ed_files, "Tables.*\\.xlsx")]
    if (length(xlsx_file) > 0) {
      cat(sprintf("\n  Also found: %s\n", basename(xlsx_file[1])))
      sheets <- excel_sheets(xlsx_file[1])
      cat(sprintf("  Sheets: %s\n", paste(sheets, collapse = " | ")))
    }
    
  } else {
    cat("  ⚠ No ED subfolder found. Available subfolders:\n")
    for (f in subfolders) cat(sprintf("    %s\n", f))
  }
  
} else {
  cat("  ✗ Mental health services folder not found\n")
}


###############################################################################
# PART 5: Updated data readiness summary
###############################################################################

cat("\n\n============================================================\n")
cat("UPDATED DATA READINESS SUMMARY\n")
cat("============================================================\n\n")

check_file <- function(path, label) {
  if (file.exists(path)) {
    n <- nrow(read_csv(path, show_col_types = FALSE))
    cat(sprintf("  ✓ %-50s — %s rows\n", label, format(n, big.mark = ",")))
  } else {
    cat(sprintf("  ✗ %-50s — NOT AVAILABLE\n", label))
  }
}

cat("CONFIRMATORY:\n")
check_file(file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h1_data.csv"),
           "H1a: Hypertension MUR (cross-sectional)")
check_file(file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h1a_cv_mur.csv"),
           "H1a: CV comparison MURs")
check_file(file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h1c_temporal.csv"),
           "H1c: Hypertension temporal trend")
check_file(file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h4_data.csv"),
           "H4: Geographic CVs + avoidability")
check_file(file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h5_data.csv"),
           "H5a: Mental health MUR (cross-sectional)")
check_file(file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h5c_temporal.csv"),
           "H5c: Mental health temporal trend")

cat("\nEXPLORATORY:\n")
check_file(file.path(OUTPUT_DIR, "exploratory", "deaths_underlying_national.csv"),
           "National underlying cause of death")
check_file(file.path(OUTPUT_DIR, "exploratory", "deaths_underlying_by_state.csv"),
           "State-level underlying cause of death")
check_file(file.path(OUTPUT_DIR, "exploratory", "cube10_full_mur_table.csv"),
           "Full MUR table (all conditions)")
check_file(file.path(OUTPUT_DIR, "exploratory", "cube10_chapter_mur_summary.csv"),
           "Chapter-level MUR summary")
check_file(file.path(OUTPUT_DIR, "exploratory", "cube14_temporal_all_causes.csv"),
           "Temporal all-cause data (2014-2024)")
check_file(file.path(OUTPUT_DIR, "exploratory", "avoidable_deaths_classification.csv"),
           "AIHW avoidability classification")
check_file(file.path(OUTPUT_DIR, "exploratory", "pbs_cardiovascular.csv"),
           "PBS cardiovascular prescriptions")
check_file(file.path(OUTPUT_DIR, "exploratory", "pbs_antihypertensives.csv"),
           "PBS antihypertensives (C09)")
check_file(file.path(OUTPUT_DIR, "exploratory", "mental_health_ed_state.csv"),
           "Mental health ED data by state")
check_file(file.path(OUTPUT_DIR, "exploratory", "burden_of_disease_dalys.csv"),
           "Burden of disease (DALYs)")
check_file(file.path(OUTPUT_DIR, "exploratory", "population_estimates.csv"),
           "ABS population estimates")
check_file(file.path(OUTPUT_DIR, "exploratory", "hospital_procedures_combined.csv"),
           "Hospital procedures (combined)")
check_file(file.path(OUTPUT_DIR, "exploratory", "hospital_diagnoses_combined.csv"),
           "Hospital diagnoses (combined)")

cat("\n============================================================\n")
cat("Script 12c complete.\n")
cat("============================================================\n")
