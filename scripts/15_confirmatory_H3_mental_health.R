# ============================================================================
# Script 15: Confirmatory Test — H3: Sex-Differentiated Hidden Burden of
#             Mental and Behavioural Disorders
#
# PRE-REGISTERED ANALYSIS — follows OSF pre-registration.
#
# H3a (primary): Paired Wilcoxon signed-rank test comparing male vs female
#   MUR for mental/behavioural disorder sub-conditions (ICD-10 F00-F99).
#
# H3a (supplementary): Exact binomial sign test, permutation test (10,000),
#   bootstrap 95% CI (10,000 resamples).
#
# H3b (secondary): Repeat H3a separately for substance use disorders
#   (F10-F19) and non-substance disorders (F20-F99). If sex difference
#   is significant for F10-F19 but not F20-F99, supports hypothesis that
#   substance use drives the overall sex difference.
#
# H3c (secondary): Temporal trend in sex gap using crude death rates
#   (Spearman correlation, mirroring H1b).
#
# DEVIATION FROM PRE-REGISTRATION:
#   Pre-registered: Paired Wilcoxon across n=17 years (2006-2022).
#   Actual: Paired Wilcoxon across sub-conditions within F00-F99
#   (n=8 ICD-10 block-level pairs with valid male+female MUR).
#   Reason: ABS Cube 10 (multiple causes) contains only single-year
#   data (2023). Historical multi-year MUR not available from the
#   2024 data release.
#   Temporal trend uses crude death rates from Cube 14 (underlying
#   cause only, not MUR).
#
# INPUT: outputs/confirmatory/confirmatory_h3_data.csv   (cross-sectional MUR)
#        outputs/confirmatory/confirmatory_h3c_temporal.csv (crude rates 2014-2024)
#
# OUTPUT: outputs/confirmatory/h3_results.txt
#         outputs/confirmatory/h3_results.csv
#         outputs/figures/fig14_h3_mental_health_mur.png
#
# PACKAGES: tidyverse, boot, coin, patchwork
# ============================================================================

library(tidyverse)
library(boot)

if (!requireNamespace("coin", quietly = TRUE)) {
  install.packages("coin", repos = "https://cloud.r-project.org")
}
library(coin)

if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork", repos = "https://cloud.r-project.org")
}
library(patchwork)

cat("============================================================\n")
cat("Script 15: Confirmatory Test H3 -- Mental Health Hidden Burden\n")
cat("============================================================\n\n")

dir.create("outputs/confirmatory", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# Load data
# ============================================================================

# --- Cross-sectional MUR data (Cube 10, 2023) ---
mur_file <- "outputs/confirmatory/confirmatory_h3_data.csv"
if (!file.exists(mur_file)) {
  stop("ERROR: ", mur_file, " not found. Run Scripts 12/12b first.")
}

h3_raw <- read_csv(mur_file, show_col_types = FALSE)
cat("H3 cross-sectional MUR data loaded:", nrow(h3_raw), "conditions\n")

# --- Temporal crude rate data (Cube 14, 2014-2024) ---
temporal_file <- "outputs/confirmatory/confirmatory_h3c_temporal.csv"
if (!file.exists(temporal_file)) {
  cat("WARNING: Temporal data not found. H3c trend analysis will be skipped.\n")
  h3c_temporal <- NULL
} else {
  h3c_temporal <- read_csv(temporal_file, show_col_types = FALSE)
  cat("Temporal data loaded:", nrow(h3c_temporal), "rows, years",
      min(h3c_temporal$year), "to", max(h3c_temporal$year), "\n")
}

# ============================================================================
# Prepare paired data
# ============================================================================

# Identify the aggregate F00-F99 row
h3_aggregate <- h3_raw %>%
  filter(icd_code == "F00-F99")

# ICD-10 block-level sub-conditions within Chapter V
# These are the major groupings that avoid double-counting
block_codes <- c("F00-F09", "F10-F19", "F20-F29", "F30-F39",
                 "F40-F48", "F50-F59", "F60-F69", "F70-F79",
                 "F80-F89", "F90-F98", "F99-F99")

h3_blocks <- h3_raw %>%
  filter(icd_code %in% block_codes) %>%
  filter(!is.na(mur_male) & !is.na(mur_female)) %>%
  mutate(mur_diff = mur_male - mur_female)

n_pairs <- nrow(h3_blocks)

cat("\nBlock-level sub-conditions with paired male/female MUR:", n_pairs, "\n")
cat("(Excludes blocks with zero underlying deaths for either sex)\n\n")

cat("Conditions used in H3a paired test:\n")
for (i in seq_len(n_pairs)) {
  row <- h3_blocks[i, ]
  cat(sprintf("  %-55s M=%.1f  F=%.1f  diff=%+.1f\n",
              paste0(str_trunc(row$cause, 45), " (", row$icd_code, ")"),
              row$mur_male, row$mur_female, row$mur_diff))
}

# ============================================================================
# Descriptive: Aggregate F00-F99
# ============================================================================

cat("\n--- Aggregate Mental & Behavioural Disorders (F00-F99) ---\n")
if (nrow(h3_aggregate) > 0) {
  cat(sprintf("  Male MUR:    %.1f (underlying: %s, multiple: %s)\n",
              h3_aggregate$mur_male, format(h3_aggregate$underlying_male, big.mark = ","),
              format(h3_aggregate$multiple_male, big.mark = ",")))
  cat(sprintf("  Female MUR:  %.1f (underlying: %s, multiple: %s)\n",
              h3_aggregate$mur_female, format(h3_aggregate$underlying_female, big.mark = ","),
              format(h3_aggregate$multiple_female, big.mark = ",")))
  cat(sprintf("  Persons MUR: %.1f\n", h3_aggregate$mur_persons))
  cat(sprintf("  Male/Female ratio: %.2f\n",
              h3_aggregate$mur_male / h3_aggregate$mur_female))
  cat(sprintf("  Absolute difference: %.1f\n\n",
              h3_aggregate$mur_male - h3_aggregate$mur_female))
}

# ============================================================================
# H3a PRIMARY: Paired Wilcoxon Signed-Rank Test
# ============================================================================

cat("--- H3a: Paired Wilcoxon Signed-Rank Test ---\n\n")

diffs <- h3_blocks$mur_diff
n_positive <- sum(diffs > 0)
n_negative <- sum(diffs < 0)

cat(sprintf("Paired differences (male MUR - female MUR) by block:\n"))
for (i in seq_len(n_pairs)) {
  row <- h3_blocks[i, ]
  direction <- ifelse(row$mur_diff > 0, "male > female",
                      ifelse(row$mur_diff < 0, "female > male", "equal"))
  cat(sprintf("  %-40s diff = %+.1f  (%s)\n",
              row$icd_code, row$mur_diff, direction))
}

cat(sprintf("\n  Positive (male > female): %d of %d\n", n_positive, n_pairs))
cat(sprintf("  Negative (female > male): %d of %d\n", n_negative, n_pairs))
cat(sprintf("  Mean difference: %.3f\n", mean(diffs)))
cat(sprintf("  Median difference: %.3f\n\n", median(diffs)))

# Wilcoxon signed-rank (exact)
wilcox_result <- wilcoxsign_test(
  h3_blocks$mur_male ~ h3_blocks$mur_female,
  distribution = "exact"
)

# Strip coin pvalue class immediately
h3a_statistic <- as.double(statistic(wilcox_result))
h3a_pvalue <- as.double(pvalue(wilcox_result))
h3a_z <- qnorm(h3a_pvalue / 2, lower.tail = FALSE)
h3a_effect_r <- h3a_z / sqrt(n_pairs)

cat("Wilcoxon signed-rank test (exact, two-tailed):\n")
cat("  Test statistic:", round(h3a_statistic, 3), "\n")
cat("  p-value:", format(h3a_pvalue, digits = 4, scientific = TRUE), "\n")
cat("  Effect size r:", round(h3a_effect_r, 3), "\n")
cat("  n pairs:", n_pairs, "\n\n")

if (h3a_pvalue < 0.05) {
  cat("  -> SIGNIFICANT at alpha = 0.05 (before Holm-Bonferroni correction)\n\n")
} else {
  cat("  -> NOT significant at alpha = 0.05\n\n")
}

# ---- H3a SUPPLEMENTARY: Exact Binomial Sign Test ----
cat("--- H3a Supplementary: Exact Binomial Sign Test ---\n\n")

sign_test <- binom.test(n_positive, n_pairs, p = 0.5, alternative = "two.sided")
cat(sprintf("  Positive differences: %d of %d\n", n_positive, n_pairs))
cat(sprintf("  p-value (exact binomial): %.4f\n", sign_test$p.value))
cat(sprintf("  95%% CI for proportion positive: [%.3f, %.3f]\n\n",
            sign_test$conf.int[1], sign_test$conf.int[2]))

# ---- H3a SUPPLEMENTARY: Permutation Test ----
cat("--- H3a Supplementary: Permutation Test ---\n\n")

set.seed(42)
observed_mean_diff <- mean(diffs)

perm_means <- replicate(10000, {
  signs <- sample(c(-1, 1), length(diffs), replace = TRUE)
  mean(diffs * signs)
})

perm_pvalue <- mean(abs(perm_means) >= abs(observed_mean_diff))
cat("  Permutation test p-value:", format(perm_pvalue, digits = 4), "\n")
cat("  (10,000 permutations of paired difference signs)\n\n")

# ---- H3a SUPPLEMENTARY: Bootstrap 95% CI ----
cat("--- H3a Supplementary: Bootstrap 95% CI ---\n\n")

set.seed(42)
boot_fn <- function(d, i) mean(d[i], na.rm = TRUE)
boot_result <- boot(diffs, boot_fn, R = 10000)

# Choose CI method based on skewness
boot_skew <- (mean(boot_result$t) - boot_result$t0) / sd(boot_result$t)
if (abs(boot_skew) > 0.25) {
  ci <- tryCatch(boot.ci(boot_result, type = "bca", conf = 0.95),
                 error = function(e) boot.ci(boot_result, type = "perc", conf = 0.95))
  if (!is.null(ci$bca)) {
    ci_lower <- ci$bca[4]; ci_upper <- ci$bca[5]; ci_method <- "BCa"
  } else {
    ci_lower <- ci$percent[4]; ci_upper <- ci$percent[5]; ci_method <- "Percentile"
  }
} else {
  ci <- boot.ci(boot_result, type = "perc", conf = 0.95)
  ci_lower <- ci$percent[4]; ci_upper <- ci$percent[5]; ci_method <- "Percentile"
}

cat(sprintf("  Mean difference (male MUR - female MUR): %.3f\n", boot_result$t0))
cat(sprintf("  95%% Bootstrap CI (%s): %.3f to %.3f\n", ci_method, ci_lower, ci_upper))
cat("  (10,000 bootstrap resamples)\n\n")

# ============================================================================
# H3b SECONDARY: Substance Use vs Non-Substance
# ============================================================================

cat("=== H3b: Substance Use (F10-F19) vs Other Mental (F20-F99) ===\n\n")

# --- H3b Substance use: individual substance codes within F10-F19 ---
substance_codes <- h3_raw %>%
  filter(str_detect(icd_code, "^F1[0-9]$"),  # F10, F11, ..., F19
         !is.na(mur_male) & !is.na(mur_female)) %>%
  mutate(mur_diff = mur_male - mur_female)

cat("--- H3b: Substance Use Disorders (F10-F19) ---\n\n")
cat("  Sub-conditions with paired MUR:", nrow(substance_codes), "\n")

if (nrow(substance_codes) >= 3) {
  for (i in seq_len(nrow(substance_codes))) {
    row <- substance_codes[i, ]
    cat(sprintf("    %-40s M=%.1f  F=%.1f  diff=%+.1f\n",
                row$icd_code, row$mur_male, row$mur_female, row$mur_diff))
  }
  cat("\n")

  n_sub_positive <- sum(substance_codes$mur_diff > 0)
  n_sub_negative <- sum(substance_codes$mur_diff < 0)
  cat(sprintf("  Male > Female: %d,  Female > Male: %d\n",
              n_sub_positive, n_sub_negative))

  if (nrow(substance_codes) >= 4) {
    sub_wilcox <- wilcoxsign_test(
      substance_codes$mur_male ~ substance_codes$mur_female,
      distribution = "exact"
    )
    h3b_sub_stat <- as.double(statistic(sub_wilcox))
    h3b_sub_p <- as.double(pvalue(sub_wilcox))
    h3b_sub_r <- qnorm(h3b_sub_p / 2, lower.tail = FALSE) / sqrt(nrow(substance_codes))
  } else {
    # Too few for Wilcoxon, use sign test only
    h3b_sub_stat <- NA_real_
    sub_sign <- binom.test(n_sub_positive, nrow(substance_codes), p = 0.5)
    h3b_sub_p <- sub_sign$p.value
    h3b_sub_r <- NA_real_
  }

  cat(sprintf("  p-value: %s\n", format(h3b_sub_p, digits = 4)))
  if (!is.na(h3b_sub_r)) cat(sprintf("  Effect size r: %.3f\n", h3b_sub_r))
  cat(sprintf("  Mean diff: %.1f\n\n", mean(substance_codes$mur_diff)))

  # Note the aggregate direction
  sub_agg <- h3_raw %>% filter(icd_code == "F10-F19")
  if (nrow(sub_agg) > 0) {
    cat(sprintf("  Aggregate F10-F19: M=%.1f, F=%.1f (%s)\n\n",
                sub_agg$mur_male, sub_agg$mur_female,
                ifelse(sub_agg$mur_male > sub_agg$mur_female,
                       "male > female", "female > male")))
  }
} else {
  cat("  Insufficient paired data for substance use analysis.\n\n")
  h3b_sub_stat <- NA_real_
  h3b_sub_p <- NA_real_
  h3b_sub_r <- NA_real_
}

# --- H3b Non-substance: block-level codes F20-F99 ---
non_sub_blocks <- h3_blocks %>%
  filter(!icd_code %in% c("F00-F09", "F10-F19"))  # F20+ blocks only

cat("--- H3b: Non-Substance Mental Disorders (F20-F99) ---\n\n")
cat("  Block-level conditions with paired MUR:", nrow(non_sub_blocks), "\n")

if (nrow(non_sub_blocks) >= 3) {
  for (i in seq_len(nrow(non_sub_blocks))) {
    row <- non_sub_blocks[i, ]
    cat(sprintf("    %-40s M=%.1f  F=%.1f  diff=%+.1f\n",
                row$icd_code, row$mur_male, row$mur_female, row$mur_diff))
  }
  cat("\n")

  n_nonsub_positive <- sum(non_sub_blocks$mur_diff > 0)
  cat(sprintf("  All %d differences positive (male > female): %s\n",
              nrow(non_sub_blocks), n_nonsub_positive == nrow(non_sub_blocks)))

  if (nrow(non_sub_blocks) >= 4) {
    nonsub_wilcox <- wilcoxsign_test(
      non_sub_blocks$mur_male ~ non_sub_blocks$mur_female,
      distribution = "exact"
    )
    h3b_nonsub_stat <- as.double(statistic(nonsub_wilcox))
    h3b_nonsub_p <- as.double(pvalue(nonsub_wilcox))
    h3b_nonsub_r <- qnorm(h3b_nonsub_p / 2, lower.tail = FALSE) /
      sqrt(nrow(non_sub_blocks))
  } else {
    h3b_nonsub_stat <- NA_real_
    nonsub_sign <- binom.test(n_nonsub_positive, nrow(non_sub_blocks), p = 0.5)
    h3b_nonsub_p <- nonsub_sign$p.value
    h3b_nonsub_r <- NA_real_
  }

  cat(sprintf("  p-value: %s\n", format(h3b_nonsub_p, digits = 4)))
  if (!is.na(h3b_nonsub_r)) cat(sprintf("  Effect size r: %.3f\n", h3b_nonsub_r))
  cat(sprintf("  Mean diff: %.1f\n\n", mean(non_sub_blocks$mur_diff)))
} else {
  cat("  Insufficient paired data for non-substance analysis.\n\n")
  h3b_nonsub_stat <- NA_real_
  h3b_nonsub_p <- NA_real_
  h3b_nonsub_r <- NA_real_
}

# H3b interpretation
cat("--- H3b Interpretation ---\n\n")
if (!is.na(h3b_sub_p) & !is.na(h3b_nonsub_p)) {
  if (h3b_sub_p < 0.05 & h3b_nonsub_p >= 0.05) {
    cat("  -> SUPPORTS H3b: Sex difference significant for substance use\n")
    cat("     but not for other mental disorders.\n\n")
  } else if (h3b_sub_p >= 0.05 & h3b_nonsub_p < 0.05) {
    cat("  -> REVERSAL: Sex difference significant for non-substance\n")
    cat("     but NOT for substance use disorders.\n")
    cat("     Substance use does NOT drive the overall sex gap.\n\n")
  } else if (h3b_sub_p < 0.05 & h3b_nonsub_p < 0.05) {
    cat("  -> PARTIALLY SUPPORTS: Sex difference significant for both.\n")
    cat("     Substance use is not the sole driver.\n\n")
  } else {
    cat("  -> Neither subgroup shows significant sex difference.\n\n")
  }
}

# ============================================================================
# H3c SECONDARY: Temporal Trend in Sex Gap (Crude Rates)
# ============================================================================

cat("--- H3c: Temporal Trend in Sex Gap (Crude Rates) ---\n\n")

h3c_cor <- NULL

if (!is.null(h3c_temporal)) {
  cat("  NOTE: Using crude death rates from Cube 14 (underlying cause only),\n")
  cat("  not MUR (which requires Cube 10 multi-year data, unavailable).\n\n")

  # Overall F00-F99 temporal trend
  h3c_overall <- h3c_temporal %>%
    filter(icd_code == "F00-F99", sex != "persons") %>%
    select(year, sex, crude_rate_per_100k) %>%
    pivot_wider(names_from = sex, values_from = crude_rate_per_100k) %>%
    mutate(mf_ratio = male / female)

  cat("  Year-level crude rates (per 100,000) and sex ratio:\n")
  cat("  Year    Male Rate    Female Rate    M/F Ratio\n")
  for (i in seq_len(nrow(h3c_overall))) {
    row <- h3c_overall[i, ]
    cat(sprintf("  %d    %8.2f      %8.2f        %.3f\n",
                row$year, row$male, row$female, row$mf_ratio))
  }

  h3c_cor <- cor.test(h3c_overall$year, h3c_overall$mf_ratio, method = "spearman",
                      exact = FALSE)

  cat(sprintf("\nSpearman rank correlation (year vs male/female rate ratio):\n"))
  cat(sprintf("  rho: %.3f\n", h3c_cor$estimate))
  cat(sprintf("  p-value: %.5f\n", h3c_cor$p.value))
  cat(sprintf("  n years: %d\n", nrow(h3c_overall)))
  direction <- ifelse(h3c_cor$estimate > 0, "NARROWING", "WIDENING")
  cat(sprintf("  Direction: Gap %s over time\n\n", direction))
} else {
  cat("  Temporal data not available. Skipping H3c.\n\n")
}

# ============================================================================
# Visualisation
# ============================================================================

cat("--- Generating Figure 14 ---\n\n")

# Panel A: Block-level MUR by sex (bar chart, like H1)
h3_plot_a <- h3_blocks %>%
  mutate(
    short_name = str_replace(cause, "\\s*\\(.*\\)$", ""),
    short_name = str_trunc(short_name, 35)
  ) %>%
  arrange(desc(mur_male)) %>%
  mutate(short_name = factor(short_name, levels = rev(short_name)))

p14a <- ggplot(h3_plot_a) +
  geom_col(aes(x = short_name, y = mur_female, fill = "Female"),
           width = 0.4, position = position_nudge(x = -0.2)) +
  geom_col(aes(x = short_name, y = mur_male, fill = "Male"),
           width = 0.4, position = position_nudge(x = 0.2)) +
  scale_fill_manual(values = c("Male" = "#2166ac", "Female" = "#b2182b"),
                    name = NULL) +
  coord_flip() +
  labs(
    title = "H3a: Mental Health MUR by Sex (2023)",
    subtitle = paste0("Wilcoxon p = ", format(h3a_pvalue, digits = 3),
                      "; Sign test p = ", format(sign_test$p.value, digits = 3),
                      "\nMean diff = ", round(boot_result$t0, 1),
                      " [", round(ci_lower, 1), ", ", round(ci_upper, 1), "]"),
    x = NULL,
    y = "Multiple-to-Underlying Cause Ratio",
    caption = "Data: ABS Causes of Death 2023, Cube 10 Table 10.2"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "top"
  )

# Panel B: Temporal crude rates by sex
if (!is.null(h3c_temporal)) {
  h3c_plot <- h3c_temporal %>%
    filter(icd_code == "F00-F99", sex != "persons")

  p14b <- ggplot(h3c_plot, aes(x = year, y = crude_rate_per_100k, colour = sex)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    scale_colour_manual(
      values = c("male" = "#2166ac", "female" = "#b2182b"),
      labels = c("Female", "Male"),
      name = NULL
    ) +
    labs(
      title = "H3c: Mental Health Underlying Death Rate by Sex (2014-2024)",
      subtitle = paste0("Spearman rho = ", round(h3c_cor$estimate, 3),
                        ", p = ", format(h3c_cor$p.value, digits = 4),
                        " (year vs M/F rate ratio)"),
      x = "Year of occurrence",
      y = "Crude death rate per 100,000",
      caption = paste0("Data: ABS Causes of Death 2023, Cube 14 (year of occurrence)\n",
                       "Note: Underlying cause only (not the ratio). 2023-2024 subject to revision.")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      legend.position = "top"
    )

  p14_combined <- p14a / p14b +
    plot_annotation(
      title = "Confirmatory Hypothesis 3: Mental Health Sex-Differentiated Hidden Burden",
      theme = theme(plot.title = element_text(face = "bold", size = 14))
    )
  fig_height <- 12
} else {
  p14_combined <- p14a
  fig_height <- 7
}

ggsave("outputs/figures/fig14_h3_mental_health_mur.png", p14_combined,
       width = 11, height = fig_height, dpi = 300, bg = "white")
cat("  -> Saved: outputs/figures/fig14_h3_mental_health_mur.png\n\n")

# ============================================================================
# Save results
# ============================================================================

results_list <- list(
  tibble(
    hypothesis = "H3a",
    test = "Wilcoxon signed-rank (exact)",
    test_type = "primary",
    statistic_name = "V",
    statistic_value = h3a_statistic,
    p_value_uncorrected = h3a_pvalue,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    ci_method = NA_character_,
    effect_size_r = h3a_effect_r,
    n = n_pairs,
    significant_uncorrected = h3a_pvalue < 0.05,
    deviation_note = paste0("Adapted: n=", n_pairs,
      " block-level sub-conditions (not n=17 years as pre-registered)")
  ),
  tibble(
    hypothesis = "H3a",
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
    hypothesis = "H3a",
    test = "Bootstrap CI",
    test_type = "supplementary",
    statistic_name = "mean_diff",
    statistic_value = boot_result$t0,
    p_value_uncorrected = NA_real_,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    ci_method = ci_method,
    effect_size_r = NA_real_,
    n = n_pairs,
    significant_uncorrected = NA,
    deviation_note = NA_character_
  ),
  tibble(
    hypothesis = "H3a",
    test = "Permutation test",
    test_type = "supplementary",
    statistic_name = "observed_mean_diff",
    statistic_value = observed_mean_diff,
    p_value_uncorrected = perm_pvalue,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    ci_method = NA_character_,
    effect_size_r = NA_real_,
    n = n_pairs,
    significant_uncorrected = perm_pvalue < 0.05,
    deviation_note = NA_character_
  ),
  tibble(
    hypothesis = "H3b_substance",
    test = ifelse(is.na(h3b_sub_stat), "Sign test", "Wilcoxon signed-rank (exact)"),
    test_type = "secondary",
    statistic_name = ifelse(is.na(h3b_sub_stat), "n_positive", "V"),
    statistic_value = ifelse(is.na(h3b_sub_stat),
                              sum(substance_codes$mur_diff > 0), h3b_sub_stat),
    p_value_uncorrected = h3b_sub_p,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    ci_method = NA_character_,
    effect_size_r = h3b_sub_r,
    n = nrow(substance_codes),
    significant_uncorrected = ifelse(is.na(h3b_sub_p), NA, h3b_sub_p < 0.05),
    deviation_note = "Substance use sub-conditions (F10-F19 individual codes)"
  ),
  tibble(
    hypothesis = "H3b_non_substance",
    test = ifelse(is.na(h3b_nonsub_stat), "Sign test", "Wilcoxon signed-rank (exact)"),
    test_type = "secondary",
    statistic_name = ifelse(is.na(h3b_nonsub_stat), "n_positive", "V"),
    statistic_value = ifelse(is.na(h3b_nonsub_stat),
                              sum(non_sub_blocks$mur_diff > 0), h3b_nonsub_stat),
    p_value_uncorrected = h3b_nonsub_p,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    ci_method = NA_character_,
    effect_size_r = h3b_nonsub_r,
    n = nrow(non_sub_blocks),
    significant_uncorrected = ifelse(is.na(h3b_nonsub_p), NA, h3b_nonsub_p < 0.05),
    deviation_note = "Non-substance block-level conditions (F20-F99)"
  )
)

# Add H3c if available
if (!is.null(h3c_cor)) {
  results_list[[length(results_list) + 1]] <- tibble(
    hypothesis = "H3c",
    test = "Spearman correlation",
    test_type = "secondary",
    statistic_name = "rho",
    statistic_value = as.numeric(h3c_cor$estimate),
    p_value_uncorrected = h3c_cor$p.value,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    ci_method = NA_character_,
    effect_size_r = as.numeric(h3c_cor$estimate),
    n = nrow(h3c_overall),
    significant_uncorrected = h3c_cor$p.value < 0.05,
    deviation_note = "Uses crude death rates (not MUR) due to data availability"
  )
}

h3_results <- bind_rows(results_list)
write_csv(h3_results, "outputs/confirmatory/h3_results.csv")

# Human-readable results
sink("outputs/confirmatory/h3_results.txt")
cat("============================================================\n")
cat("CONFIRMATORY HYPOTHESIS 3: Mental Health Sex-Differentiated\n")
cat("Hidden Burden -- Full Results\n")
cat("============================================================\n\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat("Pre-registration: OSF [insert DOI]\n\n")

cat("DEVIATION FROM PRE-REGISTRATION:\n")
cat("  Pre-registered test: Paired Wilcoxon across n=17 years (2006-2022)\n")
cat("  Actual test: Paired Wilcoxon across n=", n_pairs,
    " block-level sub-conditions\n")
cat("  Reason: ABS Cube 10 (multiple causes) contains only single-year\n")
cat("  data (2023). Historical multi-year MUR extraction not possible\n")
cat("  from the 2024 data release.\n\n")

cat("AGGREGATE RESULT (DESCRIPTIVE):\n")
if (nrow(h3_aggregate) > 0) {
  cat(sprintf("  Mental & behavioural disorders (F00-F99) MUR, 2023:\n"))
  cat(sprintf("    Male:    %.1f\n", h3_aggregate$mur_male))
  cat(sprintf("    Female:  %.1f\n", h3_aggregate$mur_female))
  cat(sprintf("    Ratio:   %.2f (male MUR is %.0f%% higher)\n",
              h3_aggregate$mur_male / h3_aggregate$mur_female,
              (h3_aggregate$mur_male / h3_aggregate$mur_female - 1) * 100))
}
cat("\n")

cat("H3a PRIMARY: Paired Wilcoxon Signed-Rank\n")
cat("  Null: No difference in male vs female MUR for mental disorders\n")
cat("  Test statistic V:", round(h3a_statistic, 3), "\n")
cat("  p-value (exact, two-tailed):", format(h3a_pvalue, digits = 6), "\n")
cat("  Effect size r:", round(h3a_effect_r, 3), "\n")
cat("  n block-level pairs:", n_pairs, "\n")
cat("  Positive differences (male > female):", n_positive, "of", n_pairs, "\n\n")

cat("H3a SUPPLEMENTARY: Exact Binomial Sign Test\n")
cat("  p-value:", format(sign_test$p.value, digits = 6), "\n\n")

cat("H3a SUPPLEMENTARY: Bootstrap 95% CI\n")
cat("  Mean (male MUR - female MUR):", round(boot_result$t0, 3), "\n")
cat("  95%", ci_method, "CI:", round(ci_lower, 3), "to", round(ci_upper, 3), "\n\n")

cat("H3a SUPPLEMENTARY: Permutation Test\n")
cat("  p-value:", format(perm_pvalue, digits = 6), "\n\n")

cat("H3b SECONDARY: Substance Use (F10-F19)\n")
cat("  n conditions:", nrow(substance_codes), "\n")
cat("  p-value:", format(h3b_sub_p, digits = 6), "\n")
if (!is.na(h3b_sub_r)) cat("  Effect size r:", round(h3b_sub_r, 3), "\n")
cat("  Mean diff:", round(mean(substance_codes$mur_diff), 1), "\n\n")

cat("H3b SECONDARY: Non-Substance (F20-F99)\n")
cat("  n conditions:", nrow(non_sub_blocks), "\n")
cat("  p-value:", format(h3b_nonsub_p, digits = 6), "\n")
if (!is.na(h3b_nonsub_r)) cat("  Effect size r:", round(h3b_nonsub_r, 3), "\n")
cat("  Mean diff:", round(mean(non_sub_blocks$mur_diff), 1), "\n\n")

if (!is.null(h3c_cor)) {
  cat("H3c SECONDARY: Temporal Trend (crude death rates, not MUR)\n")
  cat("  Spearman rho:", round(h3c_cor$estimate, 3), "\n")
  cat("  p-value:", format(h3c_cor$p.value, digits = 6), "\n")
  cat("  n years:", nrow(h3c_overall), "\n")
  cat("  Direction:", ifelse(h3c_cor$estimate > 0, "Gap narrowing", "Gap widening"), "\n\n")
}

cat("NOTE: p-values are UNCORRECTED. Apply Holm-Bonferroni correction\n")
cat("across all confirmatory tests using Script 16 before interpreting\n")
cat("significance.\n")
sink()

cat("  -> Saved: outputs/confirmatory/h3_results.csv\n")
cat("  -> Saved: outputs/confirmatory/h3_results.txt\n\n")
cat("Script 15 complete.\n")
