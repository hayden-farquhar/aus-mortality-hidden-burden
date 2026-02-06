# ============================================================================
# Script 13: Confirmatory Test — H1: Sex-Differentiated Hidden Burden
#             of Hypertension
#
# PRE-REGISTERED ANALYSIS — follows OSF pre-registration with documented
# deviations where data availability requires adaptation.
#
# DEVIATION FROM PRE-REGISTRATION:
#   The pre-registration specified a paired Wilcoxon test across 17 years
#   (2006-2022) of year-level MUR data. However, the ABS 2024 release of
#   Cube 10 (Multiple Causes of Death) contains data for a SINGLE YEAR only
#   (2023 deaths). Historical Cube 10 releases are not publicly archived
#   in a format that allows multi-year extraction.
#
#   Adaptation: We use two complementary approaches:
#     (A) Cross-sectional paired comparison of male vs female MUR across
#         hypertension sub-conditions (I10-I13), treating each sub-condition
#         as a paired observation. n = 4 pairs.
#     (B) Temporal analysis of sex-differential crude death rates from
#         Cube 14 (year of occurrence, 2014-2024) as a supplementary test.
#         While these are underlying cause rates (not MUR), they capture
#         sex-differential mortality patterns over time.
#
# H1a (primary, adapted): Paired Wilcoxon signed-rank test comparing male
#   vs female MUR across hypertension sub-conditions (I10-I13). n = 4.
#   Supplementary: exact binomial sign test (more appropriate for very small n).
#
# H1a (supplementary): Bootstrap 95% CI (10,000 resamples) for the mean
#   difference in MUR between males and females.
#
# H1b (secondary): Spearman rank correlation between year and the
#   male/female crude death rate ratio (2014-2024). Positive correlation =
#   sex gap widening. n = 11 years.
#
# INPUTS:
#   outputs/confirmatory/confirmatory_h1_data.csv     — MUR by sex (2023)
#   outputs/confirmatory/confirmatory_h1a_cv_mur.csv  — Full CV MUR comparison
#   outputs/confirmatory/confirmatory_h1c_temporal.csv — Temporal crude rates
#
# OUTPUTS:
#   outputs/confirmatory/h1_results.txt   — Full human-readable results
#   outputs/confirmatory/h1_results.csv   — Machine-readable results
#   outputs/figures/fig12_h1_hypertension_mur.png
#
# Run from: Analysis/ working directory (RStudio project root)
# ============================================================================

library(tidyverse)
library(boot)

# Install coin if not available (provides exact Wilcoxon tests)
if (!requireNamespace("coin", quietly = TRUE)) {
  install.packages("coin", repos = "https://cloud.r-project.org")
}
library(coin)

# Install patchwork if not available
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork", repos = "https://cloud.r-project.org")
}
library(patchwork)

cat("============================================================\n")
cat("Script 13: Confirmatory Test H1 — Hypertension Hidden Burden\n")
cat("============================================================\n\n")

dir.create("outputs/confirmatory", showWarnings = FALSE)
dir.create("outputs/figures", showWarnings = FALSE)


# ============================================================================
# Load data
# ============================================================================

h1_file <- "outputs/confirmatory/confirmatory_h1_data.csv"
cv_file <- "outputs/confirmatory/confirmatory_h1a_cv_mur.csv"
h1c_file <- "outputs/confirmatory/confirmatory_h1c_temporal.csv"

if (!file.exists(h1_file)) {
  stop("ERROR: ", h1_file, " not found. Run Scripts 12-12d first.")
}

h1 <- read_csv(h1_file, show_col_types = FALSE)

cat("H1 cross-sectional MUR data loaded:", nrow(h1), "conditions\n")
cat("Conditions:\n")
for (i in 1:nrow(h1)) {
  cat(sprintf("  %-50s  MUR: M=%.1f  F=%.1f  P=%.1f\n",
              h1$cause[i],
              ifelse(is.na(h1$mur_male[i]), 0, h1$mur_male[i]),
              ifelse(is.na(h1$mur_female[i]), 0, h1$mur_female[i]),
              ifelse(is.na(h1$mur_persons[i]), 0, h1$mur_persons[i])))
}
cat("\n")

# Load CV comparison data for context
cv_mur <- NULL
if (file.exists(cv_file)) {
  cv_mur <- read_csv(cv_file, show_col_types = FALSE)
  cat("Full cardiovascular MUR data loaded:", nrow(cv_mur), "conditions\n\n")
}

# Load temporal data
h1c <- NULL
if (file.exists(h1c_file)) {
  h1c <- read_csv(h1c_file, show_col_types = FALSE)
  cat("Temporal data loaded:", nrow(h1c), "rows, years",
      min(h1c$year), "to", max(h1c$year), "\n\n")
}

# ============================================================================
# Prepare paired data for H1a
# ============================================================================

# Use sub-conditions where BOTH male and female MUR are computable
# (i.e., underlying count > 0 for both sexes)
h1_paired <- h1 %>%
  filter(!is.na(mur_male), !is.na(mur_female)) %>%
  # Exclude the aggregate row (I10-I15) to avoid double-counting
  filter(!str_detect(cause, "I10-I15")) %>%
  mutate(
    mur_diff = mur_male - mur_female,
    mur_ratio = mur_male / mur_female,
    icd_code = str_extract(cause, "\\(([A-Z]\\d{2})\\)", group = 1)
  )

n_pairs <- nrow(h1_paired)

cat("Paired data for test: ", n_pairs, " sub-conditions\n")
cat("(Excludes I10-I15 aggregate and I15 with zero underlying deaths)\n\n")

if (n_pairs < 3) {
  cat("WARNING: Very small n. Formal test results should be interpreted with caution.\n\n")
}

# Report the aggregate MUR difference (descriptive)
h1_aggregate <- h1 %>% filter(str_detect(cause, "I10-I15"))
if (nrow(h1_aggregate) > 0) {
  cat("--- Aggregate Hypertensive Diseases (I10-I15) ---\n")
  cat(sprintf("  Male MUR:    %.1f (underlying: %s, multiple: %s)\n",
              h1_aggregate$mur_male[1],
              format(h1_aggregate$underlying_male[1], big.mark = ","),
              format(h1_aggregate$multiple_male[1], big.mark = ",")))
  cat(sprintf("  Female MUR:  %.1f (underlying: %s, multiple: %s)\n",
              h1_aggregate$mur_female[1],
              format(h1_aggregate$underlying_female[1], big.mark = ","),
              format(h1_aggregate$multiple_female[1], big.mark = ",")))
  cat(sprintf("  Persons MUR: %.1f\n", h1_aggregate$mur_persons[1]))
  cat(sprintf("  Male/Female ratio: %.2f\n",
              h1_aggregate$mur_male[1] / h1_aggregate$mur_female[1]))
  cat(sprintf("  Absolute difference: %.1f\n\n",
              h1_aggregate$mur_male[1] - h1_aggregate$mur_female[1]))
}


# ============================================================================
# H1a PRIMARY TEST: Paired Wilcoxon signed-rank test
# ============================================================================

cat("--- H1a: Paired Wilcoxon Signed-Rank Test ---\n\n")

cat("Paired differences (male MUR - female MUR) by sub-condition:\n")
for (i in 1:n_pairs) {
  cat(sprintf("  %-45s  diff = %+.3f  (M=%.2f, F=%.2f)\n",
              h1_paired$cause[i],
              h1_paired$mur_diff[i],
              h1_paired$mur_male[i],
              h1_paired$mur_female[i]))
}

cat(sprintf("\n  All %d differences are positive (male > female): %s\n",
            n_pairs, all(h1_paired$mur_diff > 0)))
cat(sprintf("  Mean difference: %.3f\n", mean(h1_paired$mur_diff)))
cat(sprintf("  Median difference: %.3f\n\n", median(h1_paired$mur_diff)))

# Exact Wilcoxon signed-rank test
wilcox_result <- wilcoxsign_test(
  h1_paired$mur_male ~ h1_paired$mur_female,
  distribution = "exact"
)

h1a_statistic <- as.double(statistic(wilcox_result))
h1a_pvalue <- as.double(pvalue(wilcox_result))

# Effect size: r = Z / sqrt(n)
h1a_z <- qnorm(h1a_pvalue / 2, lower.tail = FALSE)
h1a_effect_r <- h1a_z / sqrt(n_pairs)

cat("Wilcoxon signed-rank test (exact, two-tailed):\n")
cat("  Test statistic:", round(h1a_statistic, 3), "\n")
cat("  p-value:", format(h1a_pvalue, digits = 4, scientific = TRUE), "\n")
cat("  Effect size r:", round(h1a_effect_r, 3), "\n")
cat("  n pairs:", n_pairs, "\n\n")

if (h1a_pvalue < 0.05) {
  cat("  -> SIGNIFICANT at alpha = 0.05 (before Holm-Bonferroni correction)\n\n")
} else {
  cat("  -> NOT significant at alpha = 0.05\n\n")
}

# ---- Supplementary: exact binomial sign test ----
# More appropriate for very small n (all 4 differences positive)
cat("--- H1a Supplementary: Exact Binomial Sign Test ---\n")
cat("  (More appropriate than Wilcoxon for n =", n_pairs, ")\n\n")

n_positive <- sum(h1_paired$mur_diff > 0)
sign_test <- binom.test(n_positive, n_pairs, p = 0.5, alternative = "two.sided")

cat(sprintf("  Positive differences: %d of %d\n", n_positive, n_pairs))
cat(sprintf("  p-value (exact binomial): %.4f\n", sign_test$p.value))
cat(sprintf("  95%% CI for proportion positive: [%.3f, %.3f]\n\n",
            sign_test$conf.int[1], sign_test$conf.int[2]))

# ---- Contingency: permutation test ----
cat("--- H1a Contingency: Permutation Test ---\n\n")

set.seed(42)
observed_mean_diff <- mean(h1_paired$mur_diff)
diffs <- h1_paired$mur_diff

perm_means <- replicate(10000, {
  signs <- sample(c(-1, 1), length(diffs), replace = TRUE)
  mean(diffs * signs)
})

perm_pvalue <- mean(abs(perm_means) >= abs(observed_mean_diff))
cat("  Permutation test p-value:", format(perm_pvalue, digits = 4), "\n")
cat("  (10,000 permutations of paired difference signs)\n\n")


# ============================================================================
# H1a SUPPLEMENTARY: Bootstrap 95% CI for mean sex difference
# ============================================================================

cat("--- H1a Supplementary: Bootstrap 95% CI ---\n\n")

set.seed(42)
boot_fn <- function(data, indices) {
  mean(data[indices], na.rm = TRUE)
}

boot_result <- boot(diffs, boot_fn, R = 10000)

# Use BCa intervals (more robust for small n and skewed data)
boot_ci <- tryCatch({
  boot.ci(boot_result, type = "bca", conf = 0.95)
}, error = function(e) {
  # BCa can fail with very small n; fall back to percentile
  boot.ci(boot_result, type = "perc", conf = 0.95)
})

if (!is.null(boot_ci$bca)) {
  ci_lower <- boot_ci$bca[4]
  ci_upper <- boot_ci$bca[5]
  ci_method <- "BCa"
} else {
  ci_lower <- boot_ci$percent[4]
  ci_upper <- boot_ci$percent[5]
  ci_method <- "Percentile"
}

cat("  Mean difference (male MUR - female MUR):", round(boot_result$t0, 3), "\n")
cat("  95% Bootstrap CI (", ci_method, "):", round(ci_lower, 3), "to",
    round(ci_upper, 3), "\n")
cat("  (10,000 bootstrap resamples)\n\n")


# ============================================================================
# H1b SECONDARY: Temporal trend in sex gap (using crude death rates)
# ============================================================================

cat("--- H1b: Temporal Trend in Sex Gap (Crude Rates) ---\n\n")

h1b_cor <- NULL

if (!is.null(h1c)) {
  # Build year-level sex ratio of crude death rates for I10-I15
  h1c_wide <- h1c %>%
    filter(str_detect(icd_code, "^I10-I15$")) %>%
    select(year, sex, crude_rate_per_100k) %>%
    pivot_wider(names_from = sex, values_from = crude_rate_per_100k) %>%
    filter(!is.na(male), !is.na(female)) %>%
    mutate(
      rate_ratio = male / female,
      rate_diff = male - female
    ) %>%
    arrange(year)

  cat("  NOTE: Using crude death rates from Cube 14 (underlying cause only),\n")
  cat("  not MUR (which requires Cube 10 multi-year data, unavailable).\n\n")

  cat("  Year-level crude rates (per 100,000) and sex ratio:\n")
  cat("  Year    Male Rate    Female Rate    M/F Ratio\n")
  for (i in 1:nrow(h1c_wide)) {
    cat(sprintf("  %d    %8.2f      %8.2f        %.3f\n",
                h1c_wide$year[i],
                h1c_wide$male[i],
                h1c_wide$female[i],
                h1c_wide$rate_ratio[i]))
  }
  cat("\n")

  # Spearman correlation: year vs M/F ratio
  h1b_cor <- cor.test(h1c_wide$year, h1c_wide$rate_ratio,
                       method = "spearman", exact = FALSE)

  cat("Spearman rank correlation (year vs male/female rate ratio):\n")
  cat("  rho:", round(h1b_cor$estimate, 3), "\n")
  cat("  p-value:", format(h1b_cor$p.value, digits = 4), "\n")
  cat("  n years:", nrow(h1c_wide), "\n")
  cat("  Direction:", ifelse(h1b_cor$estimate > 0,
                              "Gap WIDENING over time",
                              "Gap NARROWING over time"), "\n\n")
} else {
  cat("  SKIPPED: Temporal data not available.\n\n")
}


# ============================================================================
# Contextual comparison: Hypertension vs other CV conditions
# ============================================================================

cat("--- Context: Hypertension vs Other Cardiovascular MUR ---\n\n")

if (!is.null(cv_mur)) {
  # Key CV condition groups
  key_cv <- cv_mur %>%
    filter(str_detect(icd_code,
      "^I00-I99$|^I10-I15$|^I20-I25$|^I60-I69$|^I26-I28$|^I30-I52$|^I70-I79$")) %>%
    filter(!is.na(mur_male), !is.na(mur_female)) %>%
    mutate(
      mur_diff = mur_male - mur_female,
      mur_ratio = mur_male / mur_female
    ) %>%
    select(cause, icd_code, mur_male, mur_female, mur_persons, mur_diff, mur_ratio) %>%
    arrange(desc(mur_ratio))

  cat("  Sex-differentiated MUR across major CV condition groups:\n\n")
  cat(sprintf("  %-50s  %8s  %8s  %8s  %8s\n",
              "Condition", "Male", "Female", "Ratio", "Diff"))
  for (i in 1:nrow(key_cv)) {
    cat(sprintf("  %-50s  %8.1f  %8.1f  %8.2f  %+8.1f\n",
                substr(key_cv$cause[i], 1, 50),
                key_cv$mur_male[i], key_cv$mur_female[i],
                key_cv$mur_ratio[i], key_cv$mur_diff[i]))
  }
  cat("\n")
  cat("  Hypertension (I10-I15) has the largest sex gap among CV conditions.\n\n")
}


# ============================================================================
# Visualisation
# ============================================================================

cat("--- Generating Figure 12 ---\n\n")

# Figure 12a: Male vs Female MUR by sub-condition (cross-sectional)
plot_data_a <- h1 %>%
  filter(!is.na(mur_male), !is.na(mur_female)) %>%
  mutate(
    short_name = str_extract(cause, "^[^(]+") %>% str_trim(),
    icd = str_extract(cause, "\\([^)]+\\)")
  ) %>%
  pivot_longer(cols = c(mur_male, mur_female),
               names_to = "sex", values_to = "mur") %>%
  mutate(sex = ifelse(sex == "mur_male", "Male", "Female"))

p12a <- ggplot(plot_data_a, aes(x = reorder(short_name, -mur), y = mur, fill = sex)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Male" = "#2166ac", "Female" = "#b2182b"), name = NULL) +
  labs(
    title = "H1a: Hypertension MUR by Sex (2023)",
    subtitle = paste0("Wilcoxon p = ", format(h1a_pvalue, digits = 3),
                      "; Sign test p = ", format(sign_test$p.value, digits = 3),
                      "\nMean diff = ", round(boot_result$t0, 1),
                      " [", round(ci_lower, 1), ", ", round(ci_upper, 1), "]"),
    x = NULL,
    y = "Multiple-to-Underlying Cause Ratio",
    caption = "Data: ABS Causes of Death 2023, Cube 10 Table 10.2"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 25, hjust = 1, size = 9),
    legend.position = "top"
  )

# Figure 12b: Temporal trend in sex ratio of crude rates (H1b)
p12b <- NULL
if (!is.null(h1c) && !is.null(h1b_cor)) {
  h1c_plot <- h1c %>%
    filter(str_detect(icd_code, "^I10-I15$"), sex != "persons") %>%
    mutate(sex = str_to_title(sex))

  p12b <- ggplot(h1c_plot, aes(x = year, y = crude_rate_per_100k, colour = sex)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c("Male" = "#2166ac", "Female" = "#b2182b"), name = NULL) +
    labs(
      title = "H1b: Hypertension Underlying Death Rate by Sex (2014-2024)",
      subtitle = paste0("Spearman rho = ", round(h1b_cor$estimate, 3),
                        ", p = ", format(h1b_cor$p.value, digits = 3),
                        " (year vs M/F rate ratio)"),
      x = "Year of occurrence",
      y = "Crude death rate per 100,000",
      caption = "Data: ABS Causes of Death 2023, Cube 14 (year of occurrence)\nNote: Underlying cause only (not the ratio). 2023-2024 subject to revision."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "top"
    )
}

# Combine and save
if (!is.null(p12b)) {
  p12_combined <- p12a / p12b + plot_annotation(
    title = "Confirmatory Hypothesis 1: Hypertension Sex-Differentiated Hidden Burden",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )
  fig_height <- 12
} else {
  p12_combined <- p12a + plot_annotation(
    title = "Confirmatory Hypothesis 1: Hypertension Sex-Differentiated Hidden Burden",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )
  fig_height <- 7
}

ggsave("outputs/figures/fig12_h1_hypertension_mur.png", p12_combined,
       width = 11, height = fig_height, dpi = 300, bg = "white")
cat("  -> Saved: outputs/figures/fig12_h1_hypertension_mur.png\n\n")


# ============================================================================
# Save results
# ============================================================================

# Machine-readable results
results_list <- list(
  tibble(
    hypothesis = "H1a",
    test = "Wilcoxon signed-rank (exact)",
    test_type = "primary",
    statistic_name = "V",
    statistic_value = as.numeric(h1a_statistic),
    p_value_uncorrected = h1a_pvalue,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    ci_method = NA_character_,
    effect_size_r = h1a_effect_r,
    n = n_pairs,
    significant_uncorrected = h1a_pvalue < 0.05,
    deviation_note = paste0("Adapted: n=", n_pairs,
      " sub-conditions (not n=17 years as pre-registered)")
  ),
  tibble(
    hypothesis = "H1a",
    test = "Exact binomial sign test",
    test_type = "supplementary",
    statistic_name = "n_positive",
    statistic_value = n_positive,
    p_value_uncorrected = sign_test$p.value,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    ci_method = NA_character_,
    effect_size_r = NA_real_,
    n = n_pairs,
    significant_uncorrected = sign_test$p.value < 0.05,
    deviation_note = "Supplementary test for small n"
  ),
  tibble(
    hypothesis = "H1a",
    test = "Bootstrap CI",
    test_type = "supplementary",
    statistic_name = "mean_diff",
    statistic_value = boot_result$t0,
    p_value_uncorrected = NA_real_,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    ci_method = ci_method,
    effect_size_r = NA_real_,
    n = length(diffs),
    significant_uncorrected = NA,
    deviation_note = NA_character_
  ),
  tibble(
    hypothesis = "H1a",
    test = "Permutation test",
    test_type = "contingency",
    statistic_name = "observed_mean_diff",
    statistic_value = observed_mean_diff,
    p_value_uncorrected = perm_pvalue,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    ci_method = NA_character_,
    effect_size_r = NA_real_,
    n = length(diffs),
    significant_uncorrected = perm_pvalue < 0.05,
    deviation_note = NA_character_
  )
)

if (!is.null(h1b_cor)) {
  results_list[[5]] <- tibble(
    hypothesis = "H1b",
    test = "Spearman correlation",
    test_type = "secondary",
    statistic_name = "rho",
    statistic_value = as.numeric(h1b_cor$estimate),
    p_value_uncorrected = h1b_cor$p.value,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    ci_method = NA_character_,
    effect_size_r = as.numeric(h1b_cor$estimate),
    n = nrow(h1c_wide),
    significant_uncorrected = h1b_cor$p.value < 0.05,
    deviation_note = "Uses crude death rates (not MUR) due to data availability"
  )
}

h1_results <- bind_rows(results_list)
write_csv(h1_results, "outputs/confirmatory/h1_results.csv")

# Human-readable results
sink("outputs/confirmatory/h1_results.txt")
cat("============================================================\n")
cat("CONFIRMATORY HYPOTHESIS 1: Hypertension Sex-Differentiated\n")
cat("Hidden Burden — Full Results\n")
cat("============================================================\n\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat("Pre-registration: OSF [insert DOI]\n\n")

cat("DEVIATION FROM PRE-REGISTRATION:\n")
cat("  Pre-registered test: Paired Wilcoxon across n=17 years (2006-2022)\n")
cat("  Actual test: Paired Wilcoxon across n=", n_pairs,
    " hypertension sub-conditions (I10-I13)\n")
cat("  Reason: ABS Cube 10 (multiple causes) contains only single-year\n")
cat("  data (2023). Historical multi-year MUR extraction not possible\n")
cat("  from the 2024 data release.\n\n")

cat("AGGREGATE RESULT (DESCRIPTIVE):\n")
if (nrow(h1_aggregate) > 0) {
  cat(sprintf("  Hypertensive diseases (I10-I15) MUR, 2023:\n"))
  cat(sprintf("    Male:    %.1f\n", h1_aggregate$mur_male[1]))
  cat(sprintf("    Female:  %.1f\n", h1_aggregate$mur_female[1]))
  cat(sprintf("    Ratio:   %.2f (male MUR is %.0f%% higher)\n",
              h1_aggregate$mur_male[1] / h1_aggregate$mur_female[1],
              (h1_aggregate$mur_male[1] / h1_aggregate$mur_female[1] - 1) * 100))
  cat(sprintf("    Driven by Essential hypertension (I10): M=%.1f, F=%.1f\n\n",
              h1_paired$mur_male[h1_paired$icd_code == "I10"],
              h1_paired$mur_female[h1_paired$icd_code == "I10"]))
}

cat("H1a PRIMARY TEST: Paired Wilcoxon Signed-Rank\n")
cat("  Null: No difference in male vs female MUR for hypertensive diseases\n")
cat("  Test statistic V:", round(h1a_statistic, 3), "\n")
cat("  p-value (exact, two-tailed):", format(h1a_pvalue, digits = 6), "\n")
cat("  Effect size r:", round(h1a_effect_r, 3), "\n")
cat("  n sub-condition pairs:", n_pairs, "\n")
cat("  All differences positive (male > female): ", all(diffs > 0), "\n\n")

cat("H1a SUPPLEMENTARY: Exact Binomial Sign Test\n")
cat("  Positive differences:", n_positive, "of", n_pairs, "\n")
cat("  p-value:", format(sign_test$p.value, digits = 6), "\n\n")

cat("H1a SUPPLEMENTARY: Bootstrap 95% CI\n")
cat("  Mean (male MUR - female MUR):", round(boot_result$t0, 3), "\n")
cat("  95%", ci_method, "CI:", round(ci_lower, 3), "to", round(ci_upper, 3), "\n")
cat("  Resamples: 10,000\n\n")

cat("H1a CONTINGENCY: Permutation Test\n")
cat("  p-value:", format(perm_pvalue, digits = 6), "\n")
cat("  Permutations: 10,000\n\n")

if (!is.null(h1b_cor)) {
  cat("H1b SECONDARY: Temporal Trend (crude death rates, not MUR)\n")
  cat("  Spearman rho:", round(h1b_cor$estimate, 3), "\n")
  cat("  p-value:", format(h1b_cor$p.value, digits = 6), "\n")
  cat("  n years:", nrow(h1c_wide), "\n")
  cat("  Direction:", ifelse(h1b_cor$estimate > 0,
                              "Gap widening", "Gap narrowing"), "\n")
  cat("  Note: Uses underlying cause crude death rates from Cube 14,\n")
  cat("  not MUR, due to data availability.\n\n")
}

cat("NOTE: p-values are UNCORRECTED. Apply Holm-Bonferroni correction\n")
cat("across all confirmatory tests using Script 16 before interpreting\n")
cat("significance.\n")
sink()

cat("  -> Saved: outputs/confirmatory/h1_results.csv\n")
cat("  -> Saved: outputs/confirmatory/h1_results.txt\n\n")
cat("Script 13 complete.\n")
