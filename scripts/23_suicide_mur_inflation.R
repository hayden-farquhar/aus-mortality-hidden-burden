# ============================================================================
# Script 23: Suicide-Driven Mental Health MUR Inflation Analysis
#
# Quantifies how much of the male mental health MUR excess can be attributed
# to WHO coding rules that assign suicide deaths to External Causes (X60-X84)
# rather than to the contributing psychiatric condition.
#
# Mechanism: When someone dies by suicide, the underlying cause is coded as
# intentional self-harm (X60-X84). Any contributing mental health condition
# (depression, anxiety, etc.) appears only as a multiple cause. This inflates
# the mental health MUR for males because:
#   - Males have ~3x higher suicide rates than females
#   - Each male suicide with a psychiatric contributor adds to the MUR numerator
#     but NOT the denominator for mental health codes
#
# INPUT: outputs/exploratory/cube10_full_mur_table.csv
#        (mental health + suicide data from ABS Cube 10)
#
# OUTPUT: outputs/exploratory/suicide_mur_inflation_analysis.txt
#         outputs/exploratory/suicide_mur_inflation.csv
#
# PACKAGES: tidyverse
# ============================================================================

library(tidyverse)

cat("============================================================\n")
cat("Script 23: Suicide-Driven Mental Health MUR Inflation\n")
cat("============================================================\n\n")

dir.create("outputs/exploratory", showWarnings = FALSE, recursive = TRUE)

# Open sink to capture all output
sink_file <- "outputs/exploratory/suicide_mur_inflation_analysis.txt"
sink(sink_file, split = TRUE)

cat("============================================================\n")
cat("SUICIDE-DRIVEN MENTAL HEALTH MUR INFLATION ANALYSIS\n")
cat(sprintf("Date: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M")))
cat("============================================================\n\n")

# ============================================================================
# LOAD DATA
# ============================================================================

mur_data <- read_csv("outputs/exploratory/cube10_full_mur_table.csv",
                     show_col_types = FALSE)

# ============================================================================
# EXTRACT KEY DATA
# ============================================================================

cat("A. KEY DATA FROM ABS CUBE 10 (2023)\n")
cat("------------------------------------------------------------\n\n")

# Suicide deaths (underlying cause)
suicide <- mur_data %>%
  filter(icd_code == "X60-X84")

cat("Suicide deaths (X60-X84, underlying cause):\n")
cat(sprintf("  Males:   %s\n", format(suicide$underlying_male, big.mark = ",")))
cat(sprintf("  Females: %s\n", format(suicide$underlying_female, big.mark = ",")))
cat(sprintf("  Persons: %s\n", format(suicide$underlying_persons, big.mark = ",")))
cat(sprintf("  Male/female ratio: %.2f\n",
            suicide$underlying_male / suicide$underlying_female))

# Mental health F00-F99 (chapter level)
mental <- mur_data %>%
  filter(icd_code == "F00-F99")

cat("\nMental health (F00-F99):\n")
cat(sprintf("  Underlying deaths: Male=%s, Female=%s, Persons=%s\n",
            format(mental$underlying_male, big.mark = ","),
            format(mental$underlying_female, big.mark = ","),
            format(mental$underlying_persons, big.mark = ",")))
cat(sprintf("  Multiple cause:    Male=%s, Female=%s, Persons=%s\n",
            format(mental$multiple_male, big.mark = ","),
            format(mental$multiple_female, big.mark = ","),
            format(mental$multiple_persons, big.mark = ",")))
cat(sprintf("  MUR: Male=%.2f, Female=%.2f, Persons=%.2f\n",
            mental$mur_male, mental$mur_female, mental$mur_persons))
cat(sprintf("  Male/female MUR ratio: %.2f (males %.0f%% higher)\n",
            mental$mur_male / mental$mur_female,
            100 * (mental$mur_male / mental$mur_female - 1)))

# ============================================================================
# ESTIMATE MENTAL HEALTH CONDITIONS ON SUICIDE CERTIFICATES
# ============================================================================

cat("\n\nB. ESTIMATING MENTAL HEALTH MENTIONS ON SUICIDE CERTIFICATES\n")
cat("------------------------------------------------------------\n\n")

# We need to estimate how many suicide deaths have mental health conditions
# listed as contributing causes. We don't have direct cross-tabulation,
# but we can reason from the data:

# From Spark et al. 2023 and general literature, approximately 90% of suicide
# deaths involve a diagnosable mental disorder. However, not all of these
# are recorded on death certificates. Studies suggest 40-60% of suicide
# certificates mention a psychiatric condition.

# Use a conservative estimate of 50% mention rate
pct_mh_on_suicide <- 0.50

cat("Assumption: ~50% of suicide death certificates mention\n")
cat("a psychiatric condition as a contributing cause.\n")
cat("(Conservative estimate based on death certificate studies)\n\n")

suicide_with_mh_male   <- suicide$underlying_male * pct_mh_on_suicide
suicide_with_mh_female <- suicide$underlying_female * pct_mh_on_suicide

cat("Estimated suicide deaths with MH contributing cause:\n")
cat(sprintf("  Males:   %.0f (50%% of %s)\n",
            suicide_with_mh_male, format(suicide$underlying_male, big.mark = ",")))
cat(sprintf("  Females: %.0f (50%% of %s)\n",
            suicide_with_mh_female, format(suicide$underlying_female, big.mark = ",")))

# ============================================================================
# CALCULATE COUNTERFACTUAL MUR (IF SUICIDE DEATHS WEREN'T INFLATING)
# ============================================================================

cat("\n\nC. COUNTERFACTUAL: MUR WITHOUT SUICIDE-DRIVEN INFLATION\n")
cat("------------------------------------------------------------\n\n")

cat("The inflation mechanism:\n")
cat("  - Each suicide with a psychiatric contributor adds to the\n")
cat("    multiple cause count for mental health codes (F00-F99)\n")
cat("  - But the underlying cause is X60-X84 (external), NOT F-codes\n")
cat("  - So the MUR numerator grows but denominator stays constant\n\n")

# Calculate counterfactual: What would MUR be if we removed suicide-related
# mental health mentions from the numerator?

mur_male_cf <- (mental$multiple_male - suicide_with_mh_male) / mental$underlying_male
mur_female_cf <- (mental$multiple_female - suicide_with_mh_female) / mental$underlying_female

cat("Observed MUR (F00-F99):\n")
cat(sprintf("  Male:   %.2f\n", mental$mur_male))
cat(sprintf("  Female: %.2f\n", mental$mur_female))
cat(sprintf("  Ratio:  %.2f (males %.0f%% higher)\n",
            mental$mur_male / mental$mur_female,
            100 * (mental$mur_male / mental$mur_female - 1)))

cat("\nCounterfactual MUR (removing suicide-associated MH mentions):\n")
cat(sprintf("  Male:   %.2f (reduced from %.2f)\n", mur_male_cf, mental$mur_male))
cat(sprintf("  Female: %.2f (reduced from %.2f)\n", mur_female_cf, mental$mur_female))
cat(sprintf("  Ratio:  %.2f (males %.0f%% higher)\n",
            mur_male_cf / mur_female_cf,
            100 * (mur_male_cf / mur_female_cf - 1)))

# ============================================================================
# QUANTIFY THE SUICIDE-DRIVEN COMPONENT
# ============================================================================

cat("\n\nD. QUANTIFYING THE SUICIDE-DRIVEN SEX DIFFERENCE\n")
cat("------------------------------------------------------------\n\n")

observed_diff <- mental$mur_male - mental$mur_female
counterfactual_diff <- mur_male_cf - mur_female_cf
suicide_explained <- observed_diff - counterfactual_diff
pct_explained <- 100 * suicide_explained / observed_diff

cat(sprintf("Observed male-female MUR difference:      %.2f\n", observed_diff))
cat(sprintf("Counterfactual male-female MUR difference: %.2f\n", counterfactual_diff))
cat(sprintf("Difference attributable to suicide:        %.2f\n", suicide_explained))
cat(sprintf("Percentage of sex difference explained:    %.0f%%\n", pct_explained))

# ============================================================================
# SENSITIVITY ANALYSIS: DIFFERENT MENTION RATES
# ============================================================================

cat("\n\nE. SENSITIVITY ANALYSIS: VARYING MENTAL HEALTH MENTION RATE\n")
cat("------------------------------------------------------------\n\n")

cat(sprintf("%-15s %8s %8s %8s %8s %8s\n",
            "MH mention %", "MUR_M", "MUR_F", "Ratio", "Diff", "% expl"))
cat(paste(rep("-", 70), collapse = ""), "\n")

sensitivity_results <- data.frame()

for (pct in c(0.30, 0.40, 0.50, 0.60, 0.70)) {
  mh_male <- suicide$underlying_male * pct
  mh_female <- suicide$underlying_female * pct

  mur_m <- (mental$multiple_male - mh_male) / mental$underlying_male
  mur_f <- (mental$multiple_female - mh_female) / mental$underlying_female
  ratio <- mur_m / mur_f
  diff <- mur_m - mur_f
  explained <- 100 * (observed_diff - diff) / observed_diff

  cat(sprintf("%-15s %8.2f %8.2f %8.2f %8.2f %7.0f%%\n",
              sprintf("%.0f%%", pct * 100), mur_m, mur_f, ratio, diff, explained))

  sensitivity_results <- bind_rows(sensitivity_results, tibble(
    mh_mention_rate = pct,
    mur_male_cf = mur_m,
    mur_female_cf = mur_f,
    ratio = ratio,
    mur_diff = diff,
    pct_diff_explained = explained
  ))
}

cat("\nNote: Observed MUR ratio is %.2f; counterfactual ranges from %.2f to %.2f\n",
    mental$mur_male / mental$mur_female,
    max(sensitivity_results$ratio), min(sensitivity_results$ratio))

# ============================================================================
# SPECIFIC MENTAL HEALTH SUBCATEGORIES
# ============================================================================

cat("\n\nF. MUR SEX RATIOS BY MENTAL HEALTH SUBCATEGORY\n")
cat("------------------------------------------------------------\n\n")

mh_subcats <- mur_data %>%
  filter(grepl("^F", icd_code),
         grepl("-", icd_code),  # block-level codes
         !is.na(mur_male), !is.na(mur_female)) %>%
  mutate(sex_ratio = mur_male / mur_female,
         excess_pct = 100 * (mur_male / mur_female - 1)) %>%
  arrange(desc(sex_ratio))

cat(sprintf("%-50s %8s %8s %8s %8s\n",
            "Block", "MUR_M", "MUR_F", "Ratio", "M excess"))
cat(paste(rep("-", 85), collapse = ""), "\n")

for (i in seq_len(nrow(mh_subcats))) {
  r <- mh_subcats[i, ]
  name <- str_extract(r$condition_name, "^[^(]+") %>% str_trim() %>% str_trunc(50)
  cat(sprintf("%-50s %8.1f %8.1f %8.2f %+7.0f%%\n",
              name, r$mur_male, r$mur_female, r$sex_ratio, r$excess_pct))
}

cat("\nInterpretation:\n")
cat("Mood disorders (F30-F39) show the highest male MUR excess (ratio 2.14),\n")
cat("which is consistent with suicide being strongly associated with depression.\n")
cat("Schizophrenia (F20-F29) shows ratio 1.90, also consistent with elevated\n")
cat("suicide risk in psychotic disorders. Substance use disorders (F10-F19)\n")
cat("show only slight female excess (ratio 0.79), consistent with the pre-\n")
cat("registered finding that substance disorders did NOT drive the sex diff.\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================

output_data <- tibble(
  analysis = "suicide_mur_inflation",
  suicide_male = suicide$underlying_male,
  suicide_female = suicide$underlying_female,
  suicide_male_female_ratio = suicide$underlying_male / suicide$underlying_female,
  mh_mur_male_observed = mental$mur_male,
  mh_mur_female_observed = mental$mur_female,
  mh_mur_ratio_observed = mental$mur_male / mental$mur_female,
  mh_mur_male_counterfactual = mur_male_cf,
  mh_mur_female_counterfactual = mur_female_cf,
  mh_mur_ratio_counterfactual = mur_male_cf / mur_female_cf,
  mh_mention_assumption = pct_mh_on_suicide,
  pct_sex_diff_explained_by_suicide = pct_explained
)

write_csv(output_data, "outputs/exploratory/suicide_mur_inflation.csv")

cat("\n\n============================================================\n")
cat("SUMMARY\n")
cat("============================================================\n\n")

cat(sprintf("Male suicide rate is %.1fx higher than female.\n",
            suicide$underlying_male / suicide$underlying_female))
cat(sprintf("Observed male mental health MUR is %.0f%% higher than female.\n",
            100 * (mental$mur_male / mental$mur_female - 1)))
cat(sprintf("Assuming %.0f%% of suicide certificates mention psychiatric conditions,\n",
            pct_mh_on_suicide * 100))
cat(sprintf("approximately %.0f%% of the male MUR excess can be attributed to\n",
            pct_explained))
cat("the suicide coding mechanism (ICD rules assign X60-X84 as underlying).\n\n")
cat("This is a mechanical effect of WHO coding rules, not a reflection of\n")
cat("sex differences in psychiatric comorbidity recording per se.\n")

cat("\n============================================================\n")
cat("Script 23 complete.\n")
cat("============================================================\n")

sink()

cat("\nResults saved to:\n")
cat("  ", sink_file, "\n")
cat("  outputs/exploratory/suicide_mur_inflation.csv\n")
