###############################################################################
# Script 12d: Fix H4 rate conversion, H5c/H1c population adjustment,
#             H1 obstetric code filtering, H4 avoidability merge
#
# Run AFTER Scripts 12, 12b, and 12c have completed successfully.
# Working directory should be Analysis/ (the RStudio project root).
#
# Fixes three issues identified during data verification:
#   1. CRITICAL: H4 geographic CVs used raw death COUNTS instead of rates.
#      The coefficient of variation was reflecting population size differences
#      between states, not actual geographic variation in mortality patterns.
#   2. MODERATE: H5c and H1c temporal trends are raw counts without
#      population denominators. Cannot distinguish real change from
#      demographic growth (~14% over 2014-2024).
#   3. MINOR: H1 data includes obstetric codes O10-O16 which have
#      near-zero counts and should be filtered for cleanliness.
#   4. CRITICAL: H4 avoidability classification merge produced all NAs.
#      Rebuilds classification from AIHW NHA definitions and re-merges.
#
# OUTPUTS (overwrites existing files in outputs/confirmatory/):
#   confirmatory_h4_data.csv      - CVs on ASR (not counts) + avoidability
#   confirmatory_h5c_temporal.csv - with crude rate per 100,000 column
#   confirmatory_h1c_temporal.csv - with crude rate per 100,000 column
#   confirmatory_h1_data.csv      - obstetric codes removed
###############################################################################

library(tidyverse)
library(readxl)

OUTPUT_DIR <- "outputs"
DATA_ROOT <- "../Confirmatory exploration/Data from sources"

cat("============================================================\n")
cat("Script 12d: Fix H4 rates, H5c/H1c population, H1 obstetric\n")
cat("============================================================\n\n")


###############################################################################
# PART 0: Load and prepare population estimates
###############################################################################

cat("--- PART 0: Loading Population Estimates ---\n\n")

pop_file <- file.path(OUTPUT_DIR, "exploratory", "population_estimates.csv")

if (!file.exists(pop_file)) {
  stop("Population estimates file not found at: ", pop_file,
       "\n  Run Script 12 first.")
}

# The CSV has duplicate column names; read_csv disambiguates them.
# Columns of interest (by position after read_csv):
#   sex (col 7-8): code + name. 3 = Persons
#   age (col 9-10): code + name. e.g. A40 = 40-44
#   region (col 11-12): code + name. 1=NSW,...8=ACT, AUS=Australia
#   time_period (col 15-16): e.g. 2023-Q2
#   obs_value (col 17): population count
pop_raw <- read_csv(pop_file, show_col_types = FALSE)

# Rename columns by position for clarity
pop_colnames <- c("structure", "structure_id", "structure_name", "action",
                   "measure_code", "measure_name", "sex_code", "sex_name",
                   "age_code", "age_name", "region_code", "region_name",
                   "freq_code", "freq_name", "time_period", "time_period_name",
                   "obs_value", "obs_value_name", "unit_code", "unit_name",
                   "obs_status", "obs_status_name", "obs_comment", "obs_comment_name")

if (ncol(pop_raw) >= length(pop_colnames)) {
  names(pop_raw)[1:length(pop_colnames)] <- pop_colnames
}

pop <- pop_raw %>%
  select(sex_name, age_code, age_name, region_code, region_name,
         time_period, obs_value) %>%
  mutate(
    obs_value = as.numeric(obs_value),
    year = as.integer(str_extract(time_period, "^\\d{4}")),
    quarter = str_extract(time_period, "Q\\d$")
  ) %>%
  filter(!is.na(obs_value), !is.na(year))

cat(sprintf("  Loaded %s population records, years %d-%d\n",
            format(nrow(pop), big.mark = ","),
            min(pop$year), max(pop$year)))

# --- 0A: National annual population (mid-year Q2) ---
# Sum all age groups for Australia, using Q2 as mid-year estimate
pop_national_annual <- pop %>%
  filter(region_code == "AUS", quarter == "Q2") %>%
  group_by(year) %>%
  summarise(population = sum(obs_value, na.rm = TRUE), .groups = "drop") %>%
  arrange(year)

cat("  National mid-year population (Q2, all ages):\n")
pop_national_annual %>%
  filter(year >= 2014, year <= 2024) %>%
  mutate(pop_fmt = format(population, big.mark = ",")) %>%
  {for (i in 1:nrow(.)) {
    cat(sprintf("    %d: %s\n", .$year[i], .$pop_fmt[i]))
  }}
cat("\n")

# --- 0B: State-level annual population (mid-year Q2) ---
# Build a state lookup: region_code -> abbreviated state name
state_lookup <- tibble(
  region_code = c("1", "2", "3", "4", "5", "6", "7", "8"),
  state_abbr = c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")
)

pop_state_annual <- pop %>%
  filter(region_code %in% state_lookup$region_code, quarter == "Q2") %>%
  left_join(state_lookup, by = "region_code") %>%
  group_by(year, state_abbr) %>%
  summarise(population = sum(obs_value, na.rm = TRUE), .groups = "drop") %>%
  arrange(year, state_abbr)

cat("  State populations for 2023 (Q2):\n")
pop_state_annual %>%
  filter(year == 2023) %>%
  mutate(pop_fmt = format(population, big.mark = ",")) %>%
  {for (i in 1:nrow(.)) {
    cat(sprintf("    %s: %s\n", .$state_abbr[i], .$pop_fmt[i]))
  }}
cat("\n")


###############################################################################
# PART 1: Fix H1 — Remove obstetric codes (MINOR)
###############################################################################

cat("--- PART 1: Remove obstetric codes from H1 data ---\n\n")

h1_file <- file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h1_data.csv")

if (file.exists(h1_file)) {
  h1 <- read_csv(h1_file, show_col_types = FALSE)
  n_before <- nrow(h1)

  # Remove O10-O16 obstetric hypertension codes
  h1_clean <- h1 %>%
    filter(!str_detect(cause, "\\(O1[0-6]\\)|O10-O16"))

  n_removed <- n_before - nrow(h1_clean)

  write_csv(h1_clean, h1_file)
  cat(sprintf("  Removed %d obstetric rows (O10-O16) from %d total.\n", n_removed, n_before))
  cat(sprintf("  Remaining: %d rows. Saved to: %s\n\n", nrow(h1_clean), h1_file))

  # Print remaining conditions
  cat("  Retained conditions:\n")
  for (i in 1:nrow(h1_clean)) {
    cat(sprintf("    %s  (MUR persons: %s)\n",
                h1_clean$cause[i],
                ifelse(is.na(h1_clean$mur_persons[i]), "NA",
                       sprintf("%.1f", h1_clean$mur_persons[i]))))
  }
  cat("\n")

} else {
  cat("  WARNING: H1 data file not found at:", h1_file, "\n\n")
}


###############################################################################
# PART 2: Fix H4 — Recalculate CVs using rates, rebuild avoidability merge
###############################################################################

cat("--- PART 2: Fix H4 geographic variation (rates + avoidability) ---\n\n")

state_file <- file.path(OUTPUT_DIR, "exploratory", "deaths_underlying_by_state.csv")

if (file.exists(state_file)) {

  state_deaths <- read_csv(state_file, show_col_types = FALSE)
  cat(sprintf("  Loaded state death data: %d rows, %d columns\n",
              nrow(state_deaths), ncol(state_deaths)))
  cat(sprintf("  Columns: %s\n", paste(names(state_deaths), collapse = ", ")))
  cat(sprintf("  States: %s\n\n",
              paste(unique(state_deaths$.state), collapse = ", ")))

  # The column structure from ABS underlying cause cubes:
  #   cause_of_death_and_icd_10_code = condition name with ICD code
  #   persons_4  = death count (persons)
  #   persons_7  = crude rate per 100,000 (persons)
  #   persons_10 = age-standardised rate per 100,000 (persons, ASR)
  #   .state     = state abbreviation
  #
  # ABS uses "—" and "np" for suppressed values. Convert to numeric (NA).

  # --- 2A: Use ASR for CV calculation where available ---

  # Rename for clarity and convert
  cv_input <- state_deaths %>%
    rename(
      cause = cause_of_death_and_icd_10_code,
      death_count = persons_4,
      crude_rate = persons_7,
      asr = persons_10,
      state = .state
    ) %>%
    # Exclude national total
    filter(!str_detect(tolower(state), "australia")) %>%
    # Convert to numeric (handles "—", "np", ".." etc.)
    mutate(
      death_count = suppressWarnings(as.numeric(death_count)),
      crude_rate = suppressWarnings(as.numeric(crude_rate)),
      asr = suppressWarnings(as.numeric(asr))
    ) %>%
    # Remove non-data rows (e.g., "Causes of death" header row)
    filter(!is.na(cause),
           cause != "Causes of death",
           !str_detect(cause, "^Total deaths$"))

  cat(sprintf("  After filtering: %d condition-state rows across %d states\n",
              nrow(cv_input), n_distinct(cv_input$state)))

  # Prefer ASR; fall back to crude rate where ASR is missing
  cv_input <- cv_input %>%
    mutate(rate_for_cv = ifelse(!is.na(asr), asr, crude_rate))

  # Compute CVs on RATES (not counts!)
  h4_cv <- cv_input %>%
    filter(!is.na(rate_for_cv), rate_for_cv > 0) %>%
    group_by(cause) %>%
    summarise(
      n_states = n_distinct(state),
      mean_rate = mean(rate_for_cv, na.rm = TRUE),
      sd_rate = sd(rate_for_cv, na.rm = TRUE),
      cv = (sd_rate / mean_rate) * 100,
      min_rate = min(rate_for_cv, na.rm = TRUE),
      max_rate = max(rate_for_cv, na.rm = TRUE),
      total_deaths = sum(death_count, na.rm = TRUE),
      rate_type = ifelse(all(!is.na(asr[rate_for_cv > 0])), "ASR", "crude_rate"),
      .groups = "drop"
    ) %>%
    # Require at least 6 states for meaningful CV
    filter(n_states >= 6) %>%
    # Extract ICD code for matching
    mutate(
      icd_code = str_extract(cause, "\\(([A-Z]\\d{2}[^)]*?)\\)"),
      icd_code = str_remove_all(icd_code, "[()]")
    ) %>%
    arrange(desc(cv))

  cat(sprintf("  Computed CVs for %d conditions (using rates, min 6 states)\n",
              nrow(h4_cv)))

  # Sanity check: show some key conditions
  cat("\n  Sample CVs (should be much lower than before):\n")
  key_checks <- c("I21", "I10", "J45", "G30", "C34")
  for (code in key_checks) {
    row <- h4_cv %>% filter(str_detect(icd_code, code))
    if (nrow(row) > 0) {
      cat(sprintf("    %-50s CV: %5.1f%% (mean ASR: %.1f, %d states)\n",
                  substr(row$cause[1], 1, 50),
                  row$cv[1], row$mean_rate[1], row$n_states[1]))
    }
  }

  # --- 2B: Build AIHW avoidability classification ---
  #
  # The AIHW "potentially avoidable deaths" classification (NHA PI 16)
  # defines conditions as "preventable" or "treatable" for people <75.
  # We build this lookup from the published AIHW definitions.
  #
  # Source: AIHW 2021. National Healthcare Agreement: PI 16.
  # https://meteor.aihw.gov.au/content/740864

  cat("\n  Building AIHW avoidability classification lookup...\n")

  # ICD-10 code ranges for each category
  # Format: each entry is a regex pattern that matches relevant ICD codes
  avoidable_preventable <- tribble(
    ~condition_group, ~icd_pattern, ~avoidability,
    # Infections
    "HIV/AIDS", "^B20|^B21|^B22|^B23|^B24", "preventable",
    "Selected invasive infections", "^A38|^A39|^A40|^A41|^B05|^G00|^J13|^J14|^J15\\.3|^J15\\.4|^J15\\.7|^J15\\.9", "preventable",
    "Hepatitis B and C", "^B16|^B17\\.1|^B18\\.0|^B18\\.1|^B18\\.2", "preventable",
    # Cancer
    "Lip, oral cavity and pharynx cancer", "^C0[0-9]|^C10|^C11|^C12|^C13|^C14", "preventable",
    "Oesophageal cancer", "^C15", "preventable",
    "Stomach cancer", "^C16", "preventable",
    "Liver cancer", "^C22", "preventable",
    "Lung cancer", "^C33|^C34", "preventable",
    "Skin cancer (melanoma)", "^C43", "preventable",
    # Metabolic
    "Diabetes", "^E10|^E11|^E12|^E13|^E14", "preventable",
    # Mental/substance
    "Drug use disorders", "^F11|^F12|^F13|^F14|^F15|^F16|^F18|^F19", "preventable",
    "Alcohol use disorders", "^F10", "preventable",
    # Circulatory
    "Ischaemic heart disease", "^I20|^I21|^I22|^I23|^I24|^I25", "preventable",
    "Cerebrovascular disease", "^I60|^I61|^I62|^I63|^I64|^I65|^I66|^I67|^I68|^I69", "preventable",
    "Hypertensive disease", "^I10|^I11|^I12|^I13", "preventable",
    "Aortic aneurysm", "^I71", "preventable",
    # Respiratory
    "COPD", "^J40|^J41|^J42|^J43|^J44", "preventable",
    "Influenza and pneumonia", "^J09|^J10|^J11|^J12|^J13|^J14|^J15|^J16|^J17|^J18", "preventable",
    # External
    "Transport accidents", "^V[0-9]", "preventable",
    "Accidental poisoning", "^X40|^X41|^X42|^X43|^X44|^X45|^X46|^X47|^X48|^X49", "preventable",
    "Accidental falls", "^W0[0-9]|^W1[0-9]", "preventable",
    "Accidental drowning", "^W65|^W66|^W67|^W68|^W69|^W70|^W73|^W74", "preventable",
    "Fire, burns", "^X0[0-9]|^X1[0-9]", "preventable",
    "Suicide", "^X6[0-9]|^X7[0-9]|^X8[0-4]|^Y87\\.0", "preventable",
    "Assault", "^X85|^X86|^X87|^X88|^X89|^X9[0-9]|^Y0[0-9]", "preventable"
  )

  avoidable_treatable <- tribble(
    ~condition_group, ~icd_pattern, ~avoidability,
    # Cancer
    "Breast cancer", "^C50", "treatable",
    "Cervical cancer", "^C53", "treatable",
    "Testicular cancer", "^C62", "treatable",
    "Bladder cancer", "^C67", "treatable",
    "Thyroid cancer", "^C73", "treatable",
    "Hodgkin disease", "^C81", "treatable",
    "Leukaemia", "^C91\\.0|^C91\\.1", "treatable",
    "Colorectal cancer", "^C18|^C19|^C20|^C21", "treatable",
    "Kidney cancer", "^C64", "treatable",
    "Prostate cancer", "^C61", "treatable",
    # Endocrine
    "Thyroid disorders", "^E0[0-7]", "treatable",
    # Nervous system
    "Epilepsy", "^G40|^G41", "treatable",
    # Circulatory
    "Rheumatic/valvular heart disease", "^I0[1-9]", "treatable",
    "Heart failure", "^I50|^I51", "treatable",
    # Respiratory
    "Asthma", "^J45|^J46", "treatable",
    # Digestive
    "Peptic ulcer disease", "^K25|^K26|^K27", "treatable",
    "Hernia complications", "^K40|^K41|^K42|^K43|^K44|^K45|^K46", "treatable",
    "Cholelithiasis/cholecystitis", "^K80|^K81|^K82|^K83", "treatable",
    # Genitourinary
    "Renal failure", "^N17|^N18|^N19", "treatable",
    # Maternal/infant
    "Complications of perinatal period", "^P0[0-9]|^P[1-9]", "treatable",
    "Congenital malformations (selected)", "^Q20|^Q21|^Q22|^Q23|^Q24|^Q25|^Q26|^Q27|^Q28", "treatable"
  )

  avoidable_all <- bind_rows(avoidable_preventable, avoidable_treatable)

  cat(sprintf("  Classification built: %d preventable + %d treatable = %d total groups\n",
              nrow(avoidable_preventable), nrow(avoidable_treatable), nrow(avoidable_all)))

  # --- 2C: Match conditions to avoidability ---
  # For each condition in the CV data, check if its ICD code matches
  # any avoidability pattern

  classify_condition <- function(icd_code_str) {
    if (is.na(icd_code_str) || icd_code_str == "") return(NA_character_)

    # Handle ranges like "I10-I15" by extracting the base code
    # Also handle single codes like "I21"
    codes <- str_split(icd_code_str, "-")[[1]]
    base_code <- codes[1]

    for (i in 1:nrow(avoidable_all)) {
      if (str_detect(base_code, avoidable_all$icd_pattern[i])) {
        return(avoidable_all$avoidability[i])
      }
    }
    return("non-avoidable")
  }

  classify_group <- function(icd_code_str) {
    if (is.na(icd_code_str) || icd_code_str == "") return(NA_character_)
    codes <- str_split(icd_code_str, "-")[[1]]
    base_code <- codes[1]

    for (i in 1:nrow(avoidable_all)) {
      if (str_detect(base_code, avoidable_all$icd_pattern[i])) {
        return(avoidable_all$condition_group[i])
      }
    }
    return(NA_character_)
  }

  h4_cv <- h4_cv %>%
    mutate(
      avoidability = map_chr(icd_code, classify_condition),
      avoidable_group = map_chr(icd_code, classify_group)
    )

  # Count matches
  avoid_counts <- h4_cv %>% count(avoidability)
  cat("\n  Avoidability classification results:\n")
  for (i in 1:nrow(avoid_counts)) {
    cat(sprintf("    %-15s: %d conditions\n",
                avoid_counts$avoidability[i], avoid_counts$n[i]))
  }

  # Quick check: conditions classified as avoidable
  cat("\n  Sample avoidable conditions (top 10 by CV):\n")
  h4_cv %>%
    filter(avoidability != "non-avoidable") %>%
    arrange(desc(cv)) %>%
    head(10) %>%
    {for (i in 1:nrow(.)) {
      cat(sprintf("    %-45s CV: %5.1f%% (%s)\n",
                  substr(.$cause[i], 1, 45),
                  .$cv[i], .$avoidability[i]))
    }}

  cat("\n  Sample non-avoidable conditions (top 10 by CV):\n")
  h4_cv %>%
    filter(avoidability == "non-avoidable") %>%
    arrange(desc(cv)) %>%
    head(10) %>%
    {for (i in 1:nrow(.)) {
      cat(sprintf("    %-45s CV: %5.1f%%\n",
                  substr(.$cause[i], 1, 45),
                  .$cv[i]))
    }}

  # --- 2D: Preliminary H4 test (for verification) ---
  avoidable_cvs <- h4_cv %>%
    filter(avoidability %in% c("preventable", "treatable")) %>%
    pull(cv)
  nonavoidable_cvs <- h4_cv %>%
    filter(avoidability == "non-avoidable") %>%
    pull(cv)

  cat(sprintf("\n  Preliminary comparison:\n"))
  cat(sprintf("    Avoidable conditions (n=%d):     median CV = %.1f%%\n",
              length(avoidable_cvs), median(avoidable_cvs)))
  cat(sprintf("    Non-avoidable conditions (n=%d): median CV = %.1f%%\n",
              length(nonavoidable_cvs), median(nonavoidable_cvs)))

  if (length(avoidable_cvs) >= 5 && length(nonavoidable_cvs) >= 5) {
    prelim_test <- wilcox.test(avoidable_cvs, nonavoidable_cvs)
    cat(sprintf("    Mann-Whitney U p-value: %.4f\n", prelim_test$p.value))
  }

  # --- 2E: Save corrected H4 data ---
  h4_out <- h4_cv %>%
    select(cause, icd_code, n_states, mean_rate, sd_rate, cv,
           min_rate, max_rate, total_deaths, rate_type,
           avoidability, avoidable_group)

  h4_outfile <- file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h4_data.csv")
  write_csv(h4_out, h4_outfile)
  cat(sprintf("\n  Saved corrected H4 data: %s (%d rows)\n\n", h4_outfile, nrow(h4_out)))

} else {
  cat("  WARNING: State death data not found at:", state_file, "\n\n")
}


###############################################################################
# PART 3: Fix H5c and H1c — Add population-adjusted rates
###############################################################################

cat("--- PART 3: Add population-adjusted rates to temporal data ---\n\n")

# --- 3A: Fix H5c (Mental health temporal) ---
h5c_file <- file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h5c_temporal.csv")

if (file.exists(h5c_file)) {
  h5c <- read_csv(h5c_file, show_col_types = FALSE)
  cat(sprintf("  H5c: loaded %d rows, years %d-%d\n",
              nrow(h5c), min(h5c$year), max(h5c$year)))

  # Merge population
  h5c_adj <- h5c %>%
    left_join(pop_national_annual, by = "year") %>%
    mutate(
      crude_rate_per_100k = (deaths / population) * 100000
    )

  # Check for unmatched years
  n_unmatched <- sum(is.na(h5c_adj$population))
  if (n_unmatched > 0) {
    unmatched_years <- unique(h5c_adj$year[is.na(h5c_adj$population)])
    cat(sprintf("  WARNING: %d rows have no population match (years: %s)\n",
                n_unmatched, paste(unmatched_years, collapse = ", ")))

    # For 2024 and 2025: extrapolate from most recent available year
    # Use simple linear extrapolation from last 3 years
    recent_pop <- pop_national_annual %>%
      filter(year >= 2022, year <= 2024) %>%
      arrange(year)

    if (nrow(recent_pop) >= 2) {
      # Linear model on recent years
      pop_lm <- lm(population ~ year, data = recent_pop)
      for (yr in unmatched_years) {
        extrap_pop <- predict(pop_lm, newdata = data.frame(year = yr))
        h5c_adj <- h5c_adj %>%
          mutate(
            population = ifelse(year == yr & is.na(population),
                                round(extrap_pop), population),
            crude_rate_per_100k = ifelse(year == yr,
                                         (deaths / population) * 100000,
                                         crude_rate_per_100k)
          )
        cat(sprintf("    Extrapolated %d population: %s\n",
                    yr, format(round(extrap_pop), big.mark = ",")))
      }
    }
  }

  write_csv(h5c_adj, h5c_file)
  cat(sprintf("  Saved: %s\n", h5c_file))

  # Show the correction effect
  cat("\n  Mental health (F00-F99) underlying deaths, persons:\n")
  cat("    Year    Deaths     Population    Crude Rate/100k\n")
  h5c_adj %>%
    filter(str_detect(cause, "F00-F99"), sex == "persons") %>%
    arrange(year) %>%
    {for (i in 1:nrow(.)) {
      cat(sprintf("    %d  %7s  %12s    %.1f\n",
                  .$year[i],
                  format(.$deaths[i], big.mark = ","),
                  format(.$population[i], big.mark = ","),
                  .$crude_rate_per_100k[i]))
    }}

  # Check: is the trend still present after adjustment?
  h5c_persons <- h5c_adj %>%
    filter(str_detect(cause, "F00-F99"), sex == "persons", !is.na(crude_rate_per_100k))

  if (nrow(h5c_persons) >= 3) {
    raw_change <- (max(h5c_persons$deaths) / min(h5c_persons$deaths) - 1) * 100
    rate_change <- (max(h5c_persons$crude_rate_per_100k) /
                    min(h5c_persons$crude_rate_per_100k) - 1) * 100
    cat(sprintf("\n  Raw count change: +%.1f%%\n", raw_change))
    cat(sprintf("  Crude rate change: +%.1f%% (after population adjustment)\n", rate_change))
    cat("  Difference reflects population growth + ageing over the period.\n")
  }
  cat("\n")

} else {
  cat("  WARNING: H5c temporal file not found at:", h5c_file, "\n\n")
}

# --- 3B: Fix H1c (Hypertension temporal) ---
h1c_file <- file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h1c_temporal.csv")

if (file.exists(h1c_file)) {
  h1c <- read_csv(h1c_file, show_col_types = FALSE)
  cat(sprintf("  H1c: loaded %d rows, years %d-%d\n",
              nrow(h1c), min(h1c$year), max(h1c$year)))

  h1c_adj <- h1c %>%
    left_join(pop_national_annual, by = "year") %>%
    mutate(
      crude_rate_per_100k = (deaths / population) * 100000
    )

  # Handle unmatched years (same approach)
  n_unmatched <- sum(is.na(h1c_adj$population))
  if (n_unmatched > 0) {
    unmatched_years <- unique(h1c_adj$year[is.na(h1c_adj$population)])
    cat(sprintf("  WARNING: %d rows have no population match (years: %s)\n",
                n_unmatched, paste(unmatched_years, collapse = ", ")))

    recent_pop <- pop_national_annual %>%
      filter(year >= 2022, year <= 2024) %>%
      arrange(year)

    if (nrow(recent_pop) >= 2) {
      pop_lm <- lm(population ~ year, data = recent_pop)
      for (yr in unmatched_years) {
        extrap_pop <- predict(pop_lm, newdata = data.frame(year = yr))
        h1c_adj <- h1c_adj %>%
          mutate(
            population = ifelse(year == yr & is.na(population),
                                round(extrap_pop), population),
            crude_rate_per_100k = ifelse(year == yr,
                                         (deaths / population) * 100000,
                                         crude_rate_per_100k)
          )
        cat(sprintf("    Extrapolated %d population: %s\n",
                    yr, format(round(extrap_pop), big.mark = ",")))
      }
    }
  }

  write_csv(h1c_adj, h1c_file)
  cat(sprintf("  Saved: %s\n", h1c_file))

  # Show
  cat("\n  Hypertension (I10-I15) underlying deaths, persons:\n")
  cat("    Year    Deaths     Population    Crude Rate/100k\n")
  h1c_adj %>%
    filter(str_detect(icd_code, "I10-I15"), sex == "persons") %>%
    arrange(year) %>%
    {for (i in 1:nrow(.)) {
      cat(sprintf("    %d  %7s  %12s    %.2f\n",
                  .$year[i],
                  format(.$deaths[i], big.mark = ","),
                  format(.$population[i], big.mark = ","),
                  .$crude_rate_per_100k[i]))
    }}
  cat("\n")

} else {
  cat("  WARNING: H1c temporal file not found at:", h1c_file, "\n\n")
}


###############################################################################
# PART 4: Validation summary
###############################################################################

cat("============================================================\n")
cat("VALIDATION SUMMARY\n")
cat("============================================================\n\n")

# Check H4
if (file.exists(file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h4_data.csv"))) {
  h4_check <- read_csv(file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h4_data.csv"),
                        show_col_types = FALSE)

  cat("H4 Geographic Variation (FIXED):\n")
  cat(sprintf("  Total conditions with CV: %d\n", nrow(h4_check)))
  cat(sprintf("  Median CV: %.1f%% (was ~88%% with raw counts)\n", median(h4_check$cv)))
  cat(sprintf("  Rate type used: %s\n",
              paste(unique(h4_check$rate_type), collapse = ", ")))

  avoid_n <- sum(h4_check$avoidability %in% c("preventable", "treatable"))
  nonavoid_n <- sum(h4_check$avoidability == "non-avoidable")
  na_n <- sum(is.na(h4_check$avoidability))
  cat(sprintf("  Avoidable: %d, Non-avoidable: %d, Unclassified: %d\n",
              avoid_n, nonavoid_n, na_n))
  cat(sprintf("  (Previously: 19 avoidability columns all NA)\n\n"))
}

# Check H5c
if (file.exists(file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h5c_temporal.csv"))) {
  h5c_check <- read_csv(file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h5c_temporal.csv"),
                          show_col_types = FALSE)
  has_rate <- "crude_rate_per_100k" %in% names(h5c_check)
  cat(sprintf("H5c Temporal (FIXED): crude_rate_per_100k column present = %s\n", has_rate))
  if (has_rate) {
    n_with_rate <- sum(!is.na(h5c_check$crude_rate_per_100k))
    cat(sprintf("  Rows with valid rate: %d of %d\n\n", n_with_rate, nrow(h5c_check)))
  }
}

# Check H1c
if (file.exists(file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h1c_temporal.csv"))) {
  h1c_check <- read_csv(file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h1c_temporal.csv"),
                          show_col_types = FALSE)
  has_rate <- "crude_rate_per_100k" %in% names(h1c_check)
  cat(sprintf("H1c Temporal (FIXED): crude_rate_per_100k column present = %s\n", has_rate))
  if (has_rate) {
    n_with_rate <- sum(!is.na(h1c_check$crude_rate_per_100k))
    cat(sprintf("  Rows with valid rate: %d of %d\n\n", n_with_rate, nrow(h1c_check)))
  }
}

# Check H1
if (file.exists(file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h1_data.csv"))) {
  h1_check <- read_csv(file.path(OUTPUT_DIR, "confirmatory", "confirmatory_h1_data.csv"),
                         show_col_types = FALSE)
  has_obstetric <- any(str_detect(h1_check$cause, "O1[0-6]"))
  cat(sprintf("H1 Data (FIXED): obstetric codes present = %s, rows = %d\n\n",
              has_obstetric, nrow(h1_check)))
}

cat("============================================================\n")
cat("Script 12d complete. All issues addressed.\n")
cat("Ready to proceed to Script 13 (confirmatory tests).\n")
cat("============================================================\n")
