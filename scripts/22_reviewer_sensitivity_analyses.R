# ============================================================================
# Script 21: Reviewer Sensitivity Analyses
#
# Computes three sensitivity analyses requested by reviewers:
#
#   A. Hospital-MUR correlation with and without Eye domain outlier (#4)
#   B. Poisson confidence intervals for MUR in Table 1 (#2)
#   C. Spearman rho on crude rates (not raw counts) for Table 3 (#11)
#
# INPUT: outputs/exploratory/hospital_mortality_linkage.csv
#        outputs/exploratory/mur_ranking_full.csv
#        outputs/exploratory/temporal_high_mur_series.csv
#        outputs/exploratory/population_estimates.csv
#
# OUTPUT: outputs/exploratory/reviewer_sensitivity_results.txt  (console + file)
#
# PACKAGES: tidyverse
# ============================================================================

library(tidyverse)

cat("============================================================\n")
cat("Script 21: Reviewer Sensitivity Analyses\n")
cat("============================================================\n\n")

dir.create("outputs/exploratory", showWarnings = FALSE, recursive = TRUE)

# Open sink to capture all output
sink_file <- "outputs/exploratory/reviewer_sensitivity_results.txt"
sink(sink_file, split = TRUE)  # split = TRUE also prints to console

cat("============================================================\n")
cat("REVIEWER SENSITIVITY ANALYSES\n")
cat(sprintf("Date: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M")))
cat("============================================================\n\n")

# ============================================================================
# A. HOSPITAL-MUR CORRELATION WITH/WITHOUT EYE DOMAIN
# ============================================================================

cat("============================================================\n")
cat("A. HOSPITAL-MUR CORRELATION: SENSITIVITY TO EYE DOMAIN\n")
cat("============================================================\n\n")

hosp <- read_csv("outputs/exploratory/hospital_mortality_linkage.csv",
                  show_col_types = FALSE)

cat("All domains (n =", nrow(hosp), "):\n")
cat(sprintf("  %-20s  MUR = %6.1f  Procs/death = %10.0f\n",
            hosp$clinical_domain, hosp$mur, hosp$procs_per_death), sep = "")

# Full dataset correlations
hosp_full <- hosp %>%
  mutate(log_procs = log10(procs_per_death),
         log_mur   = log10(mur))

spearman_full <- cor.test(hosp_full$log_procs, hosp_full$mur,
                          method = "spearman", exact = FALSE)
pearson_full  <- cor.test(hosp_full$log_procs, hosp_full$log_mur,
                          method = "pearson")

cat("\n--- WITH Eye domain (n =", nrow(hosp_full), ") ---\n")
cat(sprintf("  Spearman rho (log procs/death vs MUR):     rho = %.3f, p = %.4f\n",
            spearman_full$estimate, spearman_full$p.value))
cat(sprintf("  Pearson r (log procs/death vs log MUR):    r   = %.3f, p = %.4f\n",
            pearson_full$estimate, pearson_full$p.value))

# Excluding Eye
hosp_no_eye <- hosp_full %>% filter(clinical_domain != "Eye")

spearman_no_eye <- cor.test(hosp_no_eye$log_procs, hosp_no_eye$mur,
                            method = "spearman", exact = FALSE)
pearson_no_eye  <- cor.test(hosp_no_eye$log_procs, hosp_no_eye$log_mur,
                            method = "pearson")

cat(sprintf("\n--- WITHOUT Eye domain (n = %d) ---\n", nrow(hosp_no_eye)))
cat(sprintf("  Spearman rho (log procs/death vs MUR):     rho = %.3f, p = %.4f\n",
            spearman_no_eye$estimate, spearman_no_eye$p.value))
cat(sprintf("  Pearson r (log procs/death vs log MUR):    r   = %.3f, p = %.4f\n",
            pearson_no_eye$estimate, pearson_no_eye$p.value))

cat("\n--- Summary ---\n")
cat(sprintf("  Eye domain: MUR = %.1f, Procs/death = %s (extreme outlier)\n",
            hosp$mur[hosp$clinical_domain == "Eye"],
            format(hosp$procs_per_death[hosp$clinical_domain == "Eye"], big.mark = ",")))
cat(sprintf("  Removing Eye changes Spearman rho from %.3f to %.3f\n",
            spearman_full$estimate, spearman_no_eye$estimate))
cat(sprintf("  Removing Eye changes Pearson r from %.3f to %.3f\n",
            pearson_full$estimate, pearson_no_eye$estimate))

# ============================================================================
# B. POISSON CONFIDENCE INTERVALS FOR TABLE 1 MUR
# ============================================================================

cat("\n\n============================================================\n")
cat("B. POISSON CONFIDENCE INTERVALS FOR MUR (TABLE 1)\n")
cat("============================================================\n\n")

mur_data <- read_csv("outputs/exploratory/mur_ranking_full.csv",
                      show_col_types = FALSE)

# Top 10 conditions (matching Table 1 in manuscript)
top10 <- mur_data %>% head(10)

cat("Exact Poisson 95% CIs for MUR:\n")
cat("(MUR = Multiple / Underlying; CI based on Poisson uncertainty in\n")
cat(" underlying death count, which is the smaller and more variable quantity)\n\n")

cat(sprintf("%-55s %6s %6s %6s %8s   %s\n",
            "Condition", "U", "M", "MUR", "95% CI", "CI width"))
cat(paste(rep("-", 110), collapse = ""), "\n")

for (i in seq_len(nrow(top10))) {
  row <- top10[i, ]
  U <- row$underlying_persons
  M <- row$multiple_persons

  # Exact Poisson CI for underlying count
  # Lower bound of U -> upper bound of MUR, and vice versa
  U_lower <- qchisq(0.025, 2 * U) / 2
  U_upper <- qchisq(0.975, 2 * (U + 1)) / 2

  mur_lower <- M / U_upper
  mur_upper <- M / U_lower

  cat(sprintf("%-55s %6d %6d %6.1f [%6.1f, %6.1f]   width=%.0f\n",
              str_trunc(row$condition_name, 55),
              U, M, row$mur_persons,
              mur_lower, mur_upper,
              mur_upper - mur_lower))
}

cat("\nInterpretation:\n")
cat("Conditions with few underlying deaths (<50) have extremely wide CIs,\n")
cat("indicating MUR point estimates are unstable. For example, Y83-Y84\n")
cat("(15 underlying deaths) has a CI width spanning hundreds of MUR units.\n")
cat("Conditions with >50 underlying deaths have narrower, more reliable CIs.\n")

# Also compute for key conditions mentioned in text
cat("\n\nKey conditions referenced in manuscript:\n")
key_codes <- c("I10", "J96", "H00-H59")  # hypertension, resp failure, eye
for (code in key_codes) {
  row <- mur_data %>% filter(icd_code == code)
  if (nrow(row) == 0) next
  U <- row$underlying_persons
  M <- row$multiple_persons
  U_lower <- qchisq(0.025, 2 * U) / 2
  U_upper <- qchisq(0.975, 2 * (U + 1)) / 2
  cat(sprintf("  %s (%s): MUR = %.1f [%.1f, %.1f]\n",
              str_trunc(row$condition_name, 40), code,
              row$mur_persons, M / U_upper, M / U_lower))
}

# ============================================================================
# C. SPEARMAN RHO ON CRUDE RATES (TABLE 3)
# ============================================================================

cat("\n\n============================================================\n")
cat("C. SPEARMAN RHO ON CRUDE RATES vs RAW COUNTS (TABLE 3)\n")
cat("============================================================\n\n")

# --- Load temporal series ---
temporal <- read_csv("outputs/exploratory/temporal_high_mur_series.csv",
                     show_col_types = FALSE)

# --- Load population data ---
pop_file <- "outputs/exploratory/population_estimates.csv"
if (!file.exists(pop_file)) {
  cat("ERROR: population_estimates.csv not found.\n")
  cat("Attempting to load from Confirmatory exploration path...\n")
  pop_file <- "../Confirmatory exploration/Data from sources/2E ABS Population Estimates/population_estimates.csv"
}

pop_raw <- read_csv(pop_file, show_col_types = FALSE)

# Population CSV has duplicate column names — use positional access
# Column 11 (region code), 15 (time_period), 17 (obs_value)
pop <- pop_raw %>%
  select(region = 11, time_period = 15, obs_value = 17)

# Extract year from time_period (format: "2014-Q2")
# Use Q2 (mid-year) estimates for annual population
pop_annual <- pop %>%
  filter(grepl("-Q2$", time_period)) %>%
  mutate(year = as.integer(str_extract(time_period, "^\\d{4}"))) %>%
  filter(region == "AUS") %>%  # Australia total
  group_by(year) %>%
  summarise(population = sum(obs_value, na.rm = TRUE), .groups = "drop") %>%
  filter(year >= 2014, year <= 2024)

cat("Population estimates (mid-year, Australia):\n")
for (i in seq_len(nrow(pop_annual))) {
  cat(sprintf("  %d: %s\n", pop_annual$year[i],
              format(pop_annual$population[i], big.mark = ",")))
}

# Handle 2024 — Q2 may not be available yet; use latest available
if (!2024 %in% pop_annual$year) {
  cat("\n  Note: 2024 Q2 not available. Using latest available quarter.\n")
  pop_2024 <- pop %>%
    mutate(year = as.integer(str_extract(time_period, "^\\d{4}"))) %>%
    filter(year == 2024, region == "AUS") %>%
    group_by(year) %>%
    summarise(population = sum(obs_value, na.rm = TRUE), .groups = "drop")
  if (nrow(pop_2024) > 0) {
    pop_annual <- bind_rows(pop_annual, pop_2024)
    cat(sprintf("  2024 (latest Q): %s\n",
                format(pop_2024$population[1], big.mark = ",")))
  }
}

# --- Compute crude rates and re-run Spearman ---
temporal_rates <- temporal %>%
  filter(sex == "persons") %>%
  left_join(pop_annual, by = "year") %>%
  mutate(crude_rate = deaths / population * 100000)

cat("\n\nComparison: Spearman rho on raw counts vs crude rates\n\n")
cat(sprintf("%-42s %8s %8s %8s %8s   %s\n",
            "Condition", "rho_cnt", "p_cnt", "rho_rate", "p_rate", "Changed?"))
cat(paste(rep("-", 100), collapse = ""), "\n")

# Compute for each condition
conditions <- temporal_rates %>%
  distinct(icd_code, cause, mur_persons)

comparison <- data.frame()

for (i in seq_len(nrow(conditions))) {
  code <- conditions$icd_code[i]
  cond <- conditions$cause[i]
  mur  <- conditions$mur_persons[i]

  d <- temporal_rates %>% filter(icd_code == code, !is.na(crude_rate))

  if (nrow(d) < 4) next

  # Spearman on raw counts
  test_counts <- cor.test(d$year, d$deaths, method = "spearman", exact = FALSE)

  # Spearman on crude rates
  test_rates  <- cor.test(d$year, d$crude_rate, method = "spearman", exact = FALSE)

  # Classify trend
  classify <- function(rho, p) {
    if (is.na(rho) || is.na(p)) return("Stable")
    if (rho > 0.3 & p < 0.1) return("Increasing")
    if (rho < -0.3 & p < 0.1) return("Decreasing")
    return("Stable")
  }

  trend_counts <- classify(test_counts$estimate, test_counts$p.value)
  trend_rates  <- classify(test_rates$estimate, test_rates$p.value)
  changed <- ifelse(trend_counts != trend_rates, "YES ***", "no")

  # Short condition name
  short_name <- str_replace(cond, "\\s*\\([A-Z]\\d{2}.*\\)\\s*$", "") %>%
    str_trunc(42)

  cat(sprintf("%-42s %+7.3f  %7.4f  %+7.3f  %7.4f   %s\n",
              short_name,
              test_counts$estimate, test_counts$p.value,
              test_rates$estimate, test_rates$p.value,
              changed))

  comparison <- bind_rows(comparison, tibble(
    icd_code = code,
    condition = short_name,
    mur = mur,
    rho_counts = as.double(test_counts$estimate),
    p_counts = test_counts$p.value,
    trend_counts = trend_counts,
    rho_rates = as.double(test_rates$estimate),
    p_rates = test_rates$p.value,
    trend_rates = trend_rates,
    classification_changed = trend_counts != trend_rates
  ))
}

n_changed <- sum(comparison$classification_changed)
cat(sprintf("\n\nSummary: %d of %d conditions changed trend classification\n",
            n_changed, nrow(comparison)))

if (n_changed > 0) {
  cat("Conditions that changed:\n")
  changed_rows <- comparison %>% filter(classification_changed)
  for (j in seq_len(nrow(changed_rows))) {
    r <- changed_rows[j, ]
    cat(sprintf("  %s [%s]: %s -> %s\n",
                r$condition, r$icd_code, r$trend_counts, r$trend_rates))
  }
}

cat("\nNote: For Spearman (rank-based), if population grows monotonically,\n")
cat("the rank order of years is usually preserved. Differences arise only\n")
cat("when year-to-year death count changes are small relative to population\n")
cat("growth, potentially flipping the rank of adjacent years.\n")

# Also compute the rate change percentages for Table 3
cat("\n\nUpdated crude rate changes (per 100,000):\n\n")
cat(sprintf("%-42s %10s %10s %10s %10s %8s\n",
            "Condition", "Rate 2014", "Rate 2024", "Abs chg", "% chg", "ICD"))
cat(paste(rep("-", 95), collapse = ""), "\n")

for (i in seq_len(nrow(conditions))) {
  code <- conditions$icd_code[i]
  d <- temporal_rates %>% filter(icd_code == code, !is.na(crude_rate))
  if (nrow(d) == 0) next

  rate_first <- d$crude_rate[which.min(d$year)]
  rate_last  <- d$crude_rate[which.max(d$year)]
  pct_chg    <- 100 * (rate_last / rate_first - 1)

  short_name <- str_replace(conditions$cause[i],
                             "\\s*\\([A-Z]\\d{2}.*\\)\\s*$", "") %>%
    str_trunc(42)

  cat(sprintf("%-42s %10.3f %10.3f %10.3f %+9.1f%% %8s\n",
              short_name, rate_first, rate_last,
              rate_last - rate_first, pct_chg, code))
}

cat("\n============================================================\n")
cat("Script 21 complete.\n")
cat("============================================================\n")

sink()

cat("\nResults saved to:", sink_file, "\n")
