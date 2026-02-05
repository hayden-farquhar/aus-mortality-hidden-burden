# =============================================================================
# 02_clean_abs_data.R
# Purpose: Clean ABS Causes of Death data cubes into analysis-ready CSVs
# Inputs:  data/abs_cod/*.xlsx
# Outputs: outputs/deaths_underlying_by_state.csv
#          outputs/deaths_multiple_causes.csv
#          outputs/deaths_underlying_vs_multiple.csv
# =============================================================================

library(readxl)
library(tidyverse)

base_path <- "data"

# Create output directories
dir.create("outputs", showWarnings = FALSE)
dir.create("scripts", showWarnings = FALSE)

# =============================================================================
# PART 1: State-level underlying cause of death (Data cubes 01-09, Table X.1)
# =============================================================================

cat("=== PART 1: Cleaning state-level underlying cause data ===\n\n")

# State mapping from cube number to state name
state_map <- tibble(
  cube = sprintf("%02d", 1:9),
  state = c("Australia", "New South Wales", "Victoria", "Queensland",
            "South Australia", "Western Australia", "Tasmania",
            "Northern Territory", "Australian Capital Territory"),
  state_abbr = c("AUS", "NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")
)

# Function to read one state's Table X.1
read_underlying_cause <- function(filepath, state_info) {
  
  cube_num <- state_info$cube
  sheet_name <- paste0("Table ", as.integer(cube_num), ".1")
  
  cat("  Reading", state_info$state, "from sheet", sheet_name, "...\n")
  
  # Read raw — skip the metadata rows, use row 5-6 as guidance for headers
  # Based on inspection: rows 1-4 are metadata, rows 5-6 are headers, data from row 7
  raw <- read_excel(
    filepath, 
    sheet = sheet_name, 
    col_names = FALSE, 
    skip = 6,  # skip rows 1-6 (metadata + headers)
    .name_repair = "minimal"
  )
  
  # The columns are (no unit column in state files):
  # 1: Cause of death and ICD-10 code
  # 2: Number Males
  # 3: Number Females
  # 4: Number Persons
  # 5: Crude Rate Males
  # 6: Crude Rate Females
  # 7: Crude Rate Persons
  # 8: Age-std Rate Males
  # 9: Age-std Rate Females
  # 10: Age-std Rate Persons
  # 11: YPLL Males
  # 12: YPLL Females
  # 13: YPLL Persons
  
  colnames(raw) <- c("cause_icd10",
                      "n_male", "n_female", "n_persons",
                      "crude_rate_male", "crude_rate_female", "crude_rate_persons",
                      "asr_male", "asr_female", "asr_persons",
                      "ypll_male", "ypll_female", "ypll_persons")[1:ncol(raw)]
  
  # If there are more columns, capture them
  if (ncol(raw) > 13) {
    colnames(raw)[14:ncol(raw)] <- paste0("extra_", 1:(ncol(raw) - 13))
  }
  
  # Clean up
  cleaned <- raw %>%
    # Remove rows that are section headers, notes, or empty
    filter(!is.na(cause_icd10)) %>%
    filter(!cause_icd10 %in% c("Causes of death", "")) %>%
    # Remove footnote rows (often start with numbers or special chars at the end)
    filter(!str_detect(cause_icd10, "^\\d+\\.\\s|^Source:|^Note:")) %>%
    # Add state info
    mutate(
      state = state_info$state,
      state_abbr = state_info$state_abbr,
      year = 2024
    ) %>%
    # Convert numeric columns
    mutate(across(c(n_male, n_female, n_persons, 
                    crude_rate_male, crude_rate_female, crude_rate_persons,
                    asr_male, asr_female, asr_persons), 
                  ~suppressWarnings(as.numeric(.x)))) %>%
    # Parse ICD-10 code from the cause description
    mutate(
      icd10_code = str_extract(cause_icd10, "\\([A-Z][0-9].*?\\)"),
      icd10_code = str_remove_all(icd10_code, "[()]"),
      cause_name = str_trim(str_remove(cause_icd10, "\\(.*\\)$")),
      # Identify hierarchy level
      is_chapter = str_detect(cause_icd10, "^CHAPTER"),
      is_total = cause_icd10 == "Total deaths"
    )
  
  return(cleaned)
}

# Read all state files
abs_files <- list.files(file.path(base_path, "abs_cod"), pattern = "\\.xlsx$", full.names = TRUE)

all_states <- list()
for (i in 1:nrow(state_map)) {
  # Find the matching file
  pattern <- paste0("_", state_map$cube[i], " ")
  matching_file <- abs_files[str_detect(abs_files, pattern)]
  
  if (length(matching_file) == 1) {
    tryCatch({
      all_states[[i]] <- read_underlying_cause(matching_file, state_map[i, ])
    }, error = function(e) {
      cat("  ERROR reading", state_map$state[i], ":", e$message, "\n")
    })
  } else {
    cat("  WARNING: Could not find file for", state_map$state[i], "\n")
  }
}

# Combine all states
deaths_by_state <- bind_rows(all_states)

cat("\n  Combined dataset:", nrow(deaths_by_state), "rows\n")
cat("  States:", paste(unique(deaths_by_state$state_abbr), collapse = ", "), "\n")

# Quick sanity check — total deaths by state
cat("\n  Sanity check — Total deaths by state:\n")
deaths_by_state %>%
  filter(is_total) %>%
  select(state_abbr, n_persons) %>%
  print(n = 10)

# Save
write_csv(deaths_by_state, "outputs/deaths_underlying_by_state.csv")
cat("\n  Saved: outputs/deaths_underlying_by_state.csv\n")


# =============================================================================
# PART 2: Multiple causes of death (Data cube 10)
# =============================================================================

cat("\n\n=== PART 2: Cleaning multiple causes of death data ===\n\n")

mcod_file <- abs_files[grepl("Multiple", abs_files)]

# --- Table 10.2: Underlying vs Multiple cause counts ---
cat("  Reading Table 10.2 (underlying vs multiple cause counts)...\n")

raw_10_2 <- read_excel(
  mcod_file, 
  sheet = "Table 10.2", 
  col_names = FALSE, 
  skip = 6,
  .name_repair = "minimal"
)

# Based on inspection:
# Col 1: Cause of death and ICD-10 code
# Col 2: Unit
# Col 3: Underlying cause Males
# Col 4: Underlying cause Females
# Col 5: Underlying cause Persons
# Col 6: Multiple causes Males
# Col 7: Multiple causes Females
# Col 8: Multiple causes Persons
# Col 9: (possibly ratio or additional)

cat("  Raw dimensions:", nrow(raw_10_2), "x", ncol(raw_10_2), "\n")

colnames(raw_10_2) <- c("cause_icd10", "unit",
                         "underlying_male", "underlying_female", "underlying_persons",
                         "multiple_male", "multiple_female", "multiple_persons")[1:min(ncol(raw_10_2), 8)]

if (ncol(raw_10_2) > 8) {
  colnames(raw_10_2)[9:ncol(raw_10_2)] <- paste0("extra_", 1:(ncol(raw_10_2) - 8))
}

underlying_vs_multiple <- raw_10_2 %>%
  filter(!is.na(cause_icd10)) %>%
  filter(!cause_icd10 %in% c("Causes of death", "")) %>%
  filter(!str_detect(cause_icd10, "^Source:|^Note:|^\\.")) %>%
  mutate(across(c(underlying_male, underlying_female, underlying_persons,
                  multiple_male, multiple_female, multiple_persons),
                ~suppressWarnings(as.numeric(.x)))) %>%
  mutate(
    icd10_code = str_extract(cause_icd10, "\\([A-Z][0-9].*?\\)"),
    icd10_code = str_remove_all(icd10_code, "[()]"),
    cause_name = str_trim(str_remove(cause_icd10, "\\(.*\\)$")),
    is_chapter = str_detect(cause_icd10, "^CHAPTER"),
    is_total = cause_icd10 == "Total deaths"
  ) %>%
  # Calculate the key ratio: multiple mentions / underlying cause count
  mutate(
    ratio_persons = ifelse(underlying_persons > 0, 
                           multiple_persons / underlying_persons, 
                           NA_real_),
    ratio_male = ifelse(underlying_male > 0, 
                        multiple_male / underlying_male, 
                        NA_real_),
    ratio_female = ifelse(underlying_female > 0, 
                          multiple_female / underlying_female, 
                          NA_real_)
  )

cat("  Cleaned:", nrow(underlying_vs_multiple), "rows\n")

# Preview the most interesting finding — conditions with highest ratio
cat("\n  Top 20 conditions by multiple/underlying ratio (hidden burden):\n")
cat("  (Ratio > 1 means condition appears more often as contributing cause than underlying)\n\n")
underlying_vs_multiple %>%
  filter(!is_chapter, !is_total, !is.na(ratio_persons)) %>%
  filter(underlying_persons >= 50) %>%  # minimum threshold for meaningful ratio
  arrange(desc(ratio_persons)) %>%
  select(cause_name, icd10_code, underlying_persons, multiple_persons, ratio_persons) %>%
  head(20) %>%
  mutate(ratio_persons = round(ratio_persons, 1)) %>%
  print(n = 20, width = 120)

write_csv(underlying_vs_multiple, "outputs/deaths_underlying_vs_multiple.csv")
cat("\n  Saved: outputs/deaths_underlying_vs_multiple.csv\n")


# --- Table 10.3: Multiple cause counts with age-standardised rates ---
cat("\n  Reading Table 10.3 (multiple cause counts + ASR)...\n")

raw_10_3 <- read_excel(
  mcod_file, 
  sheet = "Table 10.3", 
  col_names = FALSE, 
  skip = 7,  # extra header row in this table
  .name_repair = "minimal"
)

colnames(raw_10_3) <- c("cause_icd10", "unit",
                         "mc_count_male", "mc_count_female", "mc_count_persons",
                         "mc_asr_male", "mc_asr_female", "mc_asr_persons")[1:min(ncol(raw_10_3), 8)]

multiple_cause_rates <- raw_10_3 %>%
  filter(!is.na(cause_icd10)) %>%
  filter(!cause_icd10 %in% c("Causes of death", "")) %>%
  filter(!str_detect(cause_icd10, "^Source:|^Note:|^\\.")) %>%
  mutate(across(c(mc_count_male, mc_count_female, mc_count_persons,
                  mc_asr_male, mc_asr_female, mc_asr_persons),
                ~suppressWarnings(as.numeric(.x)))) %>%
  mutate(
    icd10_code = str_extract(cause_icd10, "\\([A-Z][0-9].*?\\)"),
    icd10_code = str_remove_all(icd10_code, "[()]"),
    cause_name = str_trim(str_remove(cause_icd10, "\\(.*\\)$")),
    is_chapter = str_detect(cause_icd10, "^CHAPTER"),
    is_total = cause_icd10 == "Total deaths"
  )

cat("  Cleaned:", nrow(multiple_cause_rates), "rows\n")

write_csv(multiple_cause_rates, "outputs/deaths_multiple_cause_rates.csv")
cat("  Saved: outputs/deaths_multiple_cause_rates.csv\n")


# --- Table 10.1: Deaths by number of co-occurring causes ---
cat("\n  Reading Table 10.1 (deaths by number of causes reported)...\n")

raw_10_1 <- read_excel(
  mcod_file, 
  sheet = "Table 10.1", 
  col_names = FALSE, 
  skip = 6,
  .name_repair = "minimal"
)

colnames(raw_10_1) <- c("cause_icd10", "unit",
                         "reported_alone", "with_1_other", "with_2_other",
                         "with_3_other", "with_4_other", "with_5_plus")[1:min(ncol(raw_10_1), 8)]

deaths_by_ncauses <- raw_10_1 %>%
  filter(!is.na(cause_icd10)) %>%
  filter(!cause_icd10 %in% c("Causes of death", "")) %>%
  filter(!str_detect(cause_icd10, "^Source:|^Note:|^\\.")) %>%
  mutate(across(c(reported_alone, with_1_other, with_2_other,
                  with_3_other, with_4_other, with_5_plus),
                ~suppressWarnings(as.numeric(.x)))) %>%
  mutate(
    icd10_code = str_extract(cause_icd10, "\\([A-Z][0-9].*?\\)"),
    icd10_code = str_remove_all(icd10_code, "[()]"),
    cause_name = str_trim(str_remove(cause_icd10, "\\(.*\\)$")),
    is_chapter = str_detect(cause_icd10, "^CHAPTER"),
    is_total = cause_icd10 == "Total deaths"
  )

cat("  Cleaned:", nrow(deaths_by_ncauses), "rows\n")

write_csv(deaths_by_ncauses, "outputs/deaths_by_num_causes.csv")
cat("  Saved: outputs/deaths_by_num_causes.csv\n")


# =============================================================================
# PART 3: Quick summary statistics
# =============================================================================

cat("\n\n=== SUMMARY ===\n\n")

# Total deaths nationally
total <- deaths_by_state %>% filter(is_total, state_abbr == "AUS") %>% pull(n_persons)
cat("Total deaths Australia 2024:", format(total, big.mark = ","), "\n")

# How many deaths reported alone vs with other causes
totals_10_1 <- deaths_by_ncauses %>% filter(is_total)
if (nrow(totals_10_1) > 0) {
  alone <- totals_10_1$reported_alone[1]
  pct_alone <- round(100 * alone / total, 1)
  cat("Deaths reported with only one cause:", format(alone, big.mark = ","), 
      "(", pct_alone, "% )\n")
  cat("Deaths reported with 2+ causes:", format(total - alone, big.mark = ","),
      "(", round(100 - pct_alone, 1), "% )\n")
}

# Overall multiple/underlying ratio
overall_ratio <- underlying_vs_multiple %>% 
  filter(is_total) %>% 
  pull(ratio_persons)
if (length(overall_ratio) > 0) {
  cat("Overall ratio (total multiple cause mentions / total deaths):", 
      round(overall_ratio, 2), "\n")
  cat("  This means on average each death has", round(overall_ratio, 1), 
      "conditions listed on the certificate.\n")
}

cat("\n=== Output files created ===\n")
cat("  outputs/deaths_underlying_by_state.csv\n")
cat("  outputs/deaths_underlying_vs_multiple.csv\n")
cat("  outputs/deaths_multiple_cause_rates.csv\n")
cat("  outputs/deaths_by_num_causes.csv\n")
cat("\nDone! Share the console output back and we'll move on to AIHW data.\n")
