# ============================================================================
# Script 14: Confirmatory Test — H2: Geographic Variation is Systematically
#             Higher for Preventable Conditions
#
# PRE-REGISTERED ANALYSIS — follows OSF pre-registration.
#
# H2a (primary): Mann-Whitney U test comparing state-level CVs for
#   conditions classified as "potentially avoidable" versus "non-avoidable"
#   under the AIHW National Healthcare Agreement framework.
#
# H2a (supplementary): Permutation test (10,000 permutations) shuffling
#   avoidable/non-avoidable labels.
#
# H2b (secondary): Mann-Whitney U test comparing CVs of "preventable" vs
#   "treatable" subcategories within the avoidable group.
#
# SENSITIVITY: Repeat all tests excluding NT and ACT.
#
# INPUT: outputs/confirmatory/confirmatory_h2_data.csv
#   Columns: cause, icd_code, n_states, mean_rate, sd_rate, cv,
#            min_rate, max_rate, total_deaths, rate_type,
#            avoidability (preventable / treatable / non-avoidable),
#            avoidable_group
#
# OUTPUT: outputs/confirmatory/h2_results.txt
#         outputs/confirmatory/h2_results.csv
#         outputs/figures/fig13_h2_geographic_variation.png
#
# PACKAGES: tidyverse, coin
#
# DEVIATION FROM PRE-REGISTRATION:
#   Pre-registered: CVs computed from age-standardised rates (ASRs).
#   Actual: CVs computed from ASRs where available (8-state conditions),
#   crude rates otherwise (6-7 state conditions where ABS suppressed
#   ASRs for small jurisdictions). Rate type recorded in rate_type column.
# ============================================================================

library(tidyverse)

if (!requireNamespace("coin", quietly = TRUE)) {
  install.packages("coin", repos = "https://cloud.r-project.org")
}
library(coin)

cat("============================================================\n")
cat("Script 14: Confirmatory Test H2 -- Geographic Variation\n")
cat("============================================================\n\n")

dir.create("outputs/confirmatory", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# Load data
# ============================================================================

input_file <- "outputs/confirmatory/confirmatory_h2_data.csv"

if (!file.exists(input_file)) {
  stop(paste0(
    "ERROR: ", input_file, " not found.\n",
    "Run Scripts 12/12d first to generate the H2 dataset."
  ))
}

h2 <- read_csv(input_file, show_col_types = FALSE)

cat("H2 data loaded:", nrow(h2), "conditions\n\n")

# The avoidability column contains: "preventable", "treatable", "non-avoidable"
# Create binary grouping: preventable + treatable = "avoidable"
h2 <- h2 %>%
  mutate(
    avoidable_binary = case_when(
      avoidability %in% c("preventable", "treatable") ~ "avoidable",
      TRUE ~ "non_avoidable"
    )
  )

n_avoidable <- sum(h2$avoidable_binary == "avoidable")
n_non_avoidable <- sum(h2$avoidable_binary == "non_avoidable")
n_preventable <- sum(h2$avoidability == "preventable", na.rm = TRUE)
n_treatable <- sum(h2$avoidability == "treatable", na.rm = TRUE)

cat("Conditions by avoidability:\n")
cat("  Potentially avoidable:", n_avoidable,
    "(preventable:", n_preventable, ", treatable:", n_treatable, ")\n")
cat("  Non-avoidable:", n_non_avoidable, "\n")

# Pre-registration D7: "If fewer than 5 conditions in either category at
# 3-character level, aggregate to chapter level"
if (n_avoidable < 5 | n_non_avoidable < 5) {
  cat("\n  WARNING: Fewer than 5 conditions in one category.\n")
  cat("  Per pre-registration D7, consider aggregating to chapter level.\n")
  cat("  Proceeding with available data but flagging this.\n")
}

cat("\nCV summary by group:\n")
h2 %>%
  group_by(avoidable_binary) %>%
  summarise(
    n = n(),
    median_cv = median(cv, na.rm = TRUE),
    mean_cv = mean(cv, na.rm = TRUE),
    sd_cv = sd(cv, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()
cat("\n")

# ============================================================================
# H2a PRIMARY TEST: Mann-Whitney U test
# ============================================================================

cat("--- H2a: Mann-Whitney U Test ---\n\n")

avoidable_cvs <- h2 %>% filter(avoidable_binary == "avoidable") %>% pull(cv)
non_avoidable_cvs <- h2 %>% filter(avoidable_binary == "non_avoidable") %>% pull(cv)

# Remove NAs for testing
avoidable_cvs <- avoidable_cvs[!is.na(avoidable_cvs)]
non_avoidable_cvs <- non_avoidable_cvs[!is.na(non_avoidable_cvs)]

n1 <- length(avoidable_cvs)
n2 <- length(non_avoidable_cvs)

# Use coin package for exact Mann-Whitney
h2_test_data <- h2 %>%
  filter(!is.na(cv)) %>%
  mutate(avoidable_factor = factor(avoidable_binary,
                                    levels = c("avoidable", "non_avoidable")))

mw_result <- wilcox_test(
  cv ~ avoidable_factor,
  data = h2_test_data,
  distribution = "asymptotic"  # exact infeasible for large n
)

# Strip coin pvalue class immediately
h2a_statistic <- as.double(statistic(mw_result))
h2a_pvalue <- as.double(pvalue(mw_result))

# Effect size: rank-biserial correlation (as specified in pre-registration D6)
U <- as.numeric(wilcox.test(avoidable_cvs, non_avoidable_cvs)$statistic)
h2a_rank_biserial <- 1 - (2 * U) / (n1 * n2)

cat("Mann-Whitney U test (two-tailed):\n")
cat("  Z statistic:", round(h2a_statistic, 3), "\n")
cat("  U:", round(U, 0), "\n")
cat("  p-value:", format(h2a_pvalue, digits = 4, scientific = TRUE), "\n")
cat("  Rank-biserial correlation:", round(h2a_rank_biserial, 3), "\n")
cat("  n avoidable:", n1, ", n non-avoidable:", n2, "\n\n")

if (h2a_pvalue < 0.05) {
  cat("  -> SIGNIFICANT at alpha = 0.05 (before Holm-Bonferroni correction)\n")
  cat("  Median CV avoidable:", round(median(avoidable_cvs), 1), "%\n")
  cat("  Median CV non-avoidable:", round(median(non_avoidable_cvs), 1), "%\n\n")
} else {
  cat("  -> NOT significant at alpha = 0.05\n\n")
}

# ---- H2a SUPPLEMENTARY: Permutation test ----
cat("--- H2a Supplementary: Permutation Test ---\n\n")

set.seed(42)
observed_diff <- median(avoidable_cvs) - median(non_avoidable_cvs)
all_cvs <- c(avoidable_cvs, non_avoidable_cvs)

perm_diffs <- replicate(10000, {
  shuffled <- sample(all_cvs)
  median(shuffled[1:n1]) - median(shuffled[(n1 + 1):(n1 + n2)])
})

h2a_perm_pvalue <- mean(abs(perm_diffs) >= abs(observed_diff))

cat("Permutation test (10,000 permutations):\n")
cat("  Observed median difference:", round(observed_diff, 2), "\n")
cat("  Permutation p-value:", format(h2a_perm_pvalue, digits = 4), "\n\n")

# ============================================================================
# H2b SECONDARY: Preventable vs Treatable
# ============================================================================

cat("--- H2b: Preventable vs Treatable ---\n\n")

preventable_cvs <- h2 %>%
  filter(avoidability == "preventable", !is.na(cv)) %>% pull(cv)
treatable_cvs <- h2 %>%
  filter(avoidability == "treatable", !is.na(cv)) %>% pull(cv)

n_prev <- length(preventable_cvs)
n_treat <- length(treatable_cvs)

if (n_prev >= 3 & n_treat >= 3) {
  h2b_test <- wilcox.test(preventable_cvs, treatable_cvs)

  U_b <- as.numeric(h2b_test$statistic)
  h2b_rank_biserial <- 1 - (2 * U_b) / (n_prev * n_treat)
  h2b_pvalue <- h2b_test$p.value
  h2b_statistic <- U_b

  cat("Mann-Whitney U test (preventable vs treatable):\n")
  cat("  U:", round(U_b, 0), "\n")
  cat("  p-value:", format(h2b_pvalue, digits = 4), "\n")
  cat("  Rank-biserial correlation:", round(h2b_rank_biserial, 3), "\n")
  cat("  Median CV preventable:", round(median(preventable_cvs), 1), "%\n")
  cat("  Median CV treatable:", round(median(treatable_cvs), 1), "%\n")
  cat("  n preventable:", n_prev, ", n treatable:", n_treat, "\n\n")

  if (h2b_pvalue < 0.05) {
    cat("  -> SIGNIFICANT at alpha = 0.05 (before Holm-Bonferroni correction)\n\n")
  } else {
    cat("  -> NOT significant at alpha = 0.05\n\n")
  }
} else {
  cat("Insufficient conditions in preventable/treatable groups for testing.\n")
  cat("  n preventable:", n_prev, ", n treatable:", n_treat, "\n\n")
  h2b_pvalue <- NA_real_
  h2b_statistic <- NA_real_
  h2b_rank_biserial <- NA_real_
}

# ============================================================================
# SENSITIVITY: Exclude NT and ACT
# ============================================================================

cat("--- Sensitivity: Excluding NT and ACT ---\n\n")

# Recompute CVs from raw state-level data excluding NT and ACT
state_file <- "outputs/exploratory/deaths_underlying_by_state.csv"

if (file.exists(state_file)) {
  cat("Recomputing CVs excluding NT and ACT from raw state data...\n\n")

  dbs <- read_csv(state_file, show_col_types = FALSE)

  # Column mapping: persons_7 = crude rate, persons_10 = ASR
  # .state has state names, cause_of_death_and_icd_10_code has condition names
  # Exclude Australia (national aggregate), NT, ACT
  dbs_filtered <- dbs %>%
    filter(
      !.state %in% c("Australia", "Northern Territory",
                      "Australian Capital Territory"),
      !is.na(persons_10) | !is.na(persons_7)
    ) %>%
    mutate(
      # Use ASR where available, crude rate otherwise
      rate = suppressWarnings(as.numeric(persons_10)),
      rate = ifelse(is.na(rate), suppressWarnings(as.numeric(persons_7)), rate)
    ) %>%
    filter(!is.na(rate), rate > 0) %>%
    rename(cause = cause_of_death_and_icd_10_code)

  # Compute CVs by condition (6 states max: NSW, VIC, QLD, SA, WA, TAS)
  cv_excl <- dbs_filtered %>%
    group_by(cause) %>%
    summarise(
      cv_excl = (sd(rate, na.rm = TRUE) / mean(rate, na.rm = TRUE)) * 100,
      n_states_excl = n(),
      .groups = "drop"
    ) %>%
    filter(n_states_excl >= 4)  # require at least 4 of 6 states

  # Merge with avoidability classification
  h2_sens <- h2 %>%
    select(cause, avoidable_binary, avoidability) %>%
    inner_join(cv_excl, by = "cause")

  n_sens <- nrow(h2_sens)
  cat("  Conditions matched for sensitivity:", n_sens, "\n")

  avoidable_sens <- h2_sens %>%
    filter(avoidable_binary == "avoidable") %>% pull(cv_excl)
  non_avoid_sens <- h2_sens %>%
    filter(avoidable_binary == "non_avoidable") %>% pull(cv_excl)

  if (length(avoidable_sens) >= 3 & length(non_avoid_sens) >= 3) {
    sens_test <- wilcox.test(avoidable_sens, non_avoid_sens)
    sens_U <- as.numeric(sens_test$statistic)
    sens_rb <- 1 - (2 * sens_U) / (length(avoidable_sens) * length(non_avoid_sens))

    cat("  H2a sensitivity (excl. NT/ACT):\n")
    cat("    U:", round(sens_U, 0), "\n")
    cat("    p-value:", format(sens_test$p.value, digits = 4), "\n")
    cat("    Rank-biserial:", round(sens_rb, 3), "\n")
    cat("    Median CV avoidable:", round(median(avoidable_sens), 1), "%\n")
    cat("    Median CV non-avoidable:", round(median(non_avoid_sens), 1), "%\n")
    cat("    n avoidable:", length(avoidable_sens),
        ", n non-avoidable:", length(non_avoid_sens), "\n\n")

    sens_pvalue <- sens_test$p.value
    sens_rb_val <- sens_rb
  } else {
    cat("  Insufficient conditions after excluding NT/ACT.\n\n")
    sens_pvalue <- NA_real_
    sens_rb_val <- NA_real_
  }

  # H2b sensitivity: preventable vs treatable (excl. NT/ACT)
  prev_sens <- h2_sens %>%
    filter(avoidability == "preventable") %>% pull(cv_excl)
  treat_sens <- h2_sens %>%
    filter(avoidability == "treatable") %>% pull(cv_excl)

  if (length(prev_sens) >= 3 & length(treat_sens) >= 3) {
    sens_b <- wilcox.test(prev_sens, treat_sens)
    cat("  H2b sensitivity (excl. NT/ACT):\n")
    cat("    p-value:", format(sens_b$p.value, digits = 4), "\n")
    cat("    Median CV preventable:", round(median(prev_sens), 1), "%\n")
    cat("    Median CV treatable:", round(median(treat_sens), 1), "%\n\n")
    sens_b_pvalue <- sens_b$p.value
  } else {
    sens_b_pvalue <- NA_real_
  }
} else {
  cat("State-level data not available for sensitivity analysis.\n")
  cat("  Expected:", state_file, "\n\n")
  sens_pvalue <- NA_real_
  sens_rb_val <- NA_real_
  sens_b_pvalue <- NA_real_
}

# ============================================================================
# Visualisation
# ============================================================================

cat("--- Generating Figure 13 ---\n\n")

# Top conditions in each group for labelling
top_avoidable <- h2 %>%
  filter(avoidable_binary == "avoidable") %>%
  arrange(desc(cv)) %>%
  slice_head(n = 5)
top_non_avoidable <- h2 %>%
  filter(avoidable_binary == "non_avoidable") %>%
  arrange(desc(cv)) %>%
  slice_head(n = 5)

# Extract short condition names (remove ICD codes in parentheses)
h2_plot <- h2 %>%
  filter(!is.na(cv)) %>%
  mutate(
    short_name = str_replace(cause, "\\s*\\(.*\\)$", ""),
    short_name = str_trunc(short_name, 40),
    avoidable_label = ifelse(avoidable_binary == "avoidable",
                              "Potentially\nAvoidable", "Non-\nAvoidable"),
    avoidable_label = factor(avoidable_label,
                              levels = c("Potentially\nAvoidable", "Non-\nAvoidable"))
  )

# Panel A: Boxplot comparison
p13a <- ggplot(h2_plot, aes(x = avoidable_label, y = cv, fill = avoidable_label)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.5) +
  geom_jitter(width = 0.15, size = 1.5, alpha = 0.5) +
  scale_fill_manual(
    values = c("Potentially\nAvoidable" = "#d95f02", "Non-\nAvoidable" = "#7570b3"),
    name = NULL
  ) +
  labs(
    title = "H2a: Geographic CV by Avoidability",
    subtitle = paste0("Mann-Whitney p = ", format(h2a_pvalue, digits = 3),
                      "; rank-biserial r = ", round(h2a_rank_biserial, 3)),
    x = NULL,
    y = "Coefficient of Variation (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "none"
  )

# Panel B: Preventable vs Treatable (if testable)
if (n_prev >= 3 & n_treat >= 3) {
  h2_avoidable <- h2 %>%
    filter(avoidability %in% c("preventable", "treatable"), !is.na(cv)) %>%
    mutate(
      subcat_label = ifelse(avoidability == "preventable", "Preventable", "Treatable"),
      subcat_label = factor(subcat_label, levels = c("Preventable", "Treatable"))
    )

  p13b <- ggplot(h2_avoidable, aes(x = subcat_label, y = cv, fill = subcat_label)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA, width = 0.5) +
    geom_jitter(width = 0.15, size = 1.5, alpha = 0.5) +
    scale_fill_manual(
      values = c("Preventable" = "#e7298a", "Treatable" = "#66a61e"),
      name = NULL
    ) +
    labs(
      title = "H2b: Preventable vs Treatable",
      subtitle = paste0("Mann-Whitney p = ", format(h2b_pvalue, digits = 3),
                        "; rank-biserial r = ", round(h2b_rank_biserial, 3)),
      x = NULL,
      y = "Coefficient of Variation (%)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      legend.position = "none"
    )

  # Combine panels
  if (requireNamespace("patchwork", quietly = TRUE)) {
    library(patchwork)
    p13_combined <- p13a + p13b +
      plot_annotation(
        title = "Confirmatory Hypothesis 4: Geographic Variation x Preventability",
        caption = paste0(
          "Each point = one condition. CV = SD/mean x 100 of state-level rates.\n",
          "Classification: AIHW National Healthcare Agreement PI 16 framework.\n",
          "Data: ABS Causes of Death 2023."
        ),
        theme = theme(
          plot.title = element_text(face = "bold", size = 14),
          plot.caption = element_text(size = 9, hjust = 1)
        )
      )
    fig_width <- 12
    fig_height <- 7
  } else {
    p13_combined <- p13a
    fig_width <- 8
    fig_height <- 7
  }
} else {
  p13_combined <- p13a +
    labs(caption = paste0(
      "Each point = one condition. CV = SD/mean x 100 of state-level rates.\n",
      "Classification: AIHW National Healthcare Agreement PI 16 framework.\n",
      "Data: ABS Causes of Death 2023."
    ))
  fig_width <- 8
  fig_height <- 7
}

ggsave("outputs/figures/fig13_h2_geographic_variation.png", p13_combined,
       width = fig_width, height = fig_height, dpi = 300, bg = "white")
cat("  -> Saved: outputs/figures/fig13_h2_geographic_variation.png\n\n")

# ============================================================================
# Save results
# ============================================================================

results_list <- list(
  tibble(
    hypothesis = "H2a",
    test = "Mann-Whitney U",
    test_type = "primary",
    statistic_name = "Z",
    statistic_value = h2a_statistic,
    p_value_uncorrected = h2a_pvalue,
    effect_size_name = "rank_biserial",
    effect_size = h2a_rank_biserial,
    n_group1 = n1,
    n_group2 = n2,
    significant_uncorrected = h2a_pvalue < 0.05,
    deviation_note = NA_character_
  ),
  tibble(
    hypothesis = "H2a",
    test = "Permutation test",
    test_type = "supplementary",
    statistic_name = "median_diff",
    statistic_value = observed_diff,
    p_value_uncorrected = h2a_perm_pvalue,
    effect_size_name = NA_character_,
    effect_size = NA_real_,
    n_group1 = n1,
    n_group2 = n2,
    significant_uncorrected = h2a_perm_pvalue < 0.05,
    deviation_note = NA_character_
  ),
  tibble(
    hypothesis = "H2b",
    test = "Mann-Whitney U",
    test_type = "secondary",
    statistic_name = "U",
    statistic_value = ifelse(is.na(h2b_statistic), NA_real_, h2b_statistic),
    p_value_uncorrected = ifelse(is.na(h2b_pvalue), NA_real_, h2b_pvalue),
    effect_size_name = "rank_biserial",
    effect_size = ifelse(is.na(h2b_rank_biserial), NA_real_, h2b_rank_biserial),
    n_group1 = n_prev,
    n_group2 = n_treat,
    significant_uncorrected = ifelse(is.na(h2b_pvalue), NA, h2b_pvalue < 0.05),
    deviation_note = NA_character_
  )
)

# Add sensitivity results if available
if (exists("sens_pvalue") && !is.na(sens_pvalue)) {
  results_list[[4]] <- tibble(
    hypothesis = "H2a",
    test = "Mann-Whitney U (excl. NT/ACT)",
    test_type = "sensitivity",
    statistic_name = "U",
    statistic_value = sens_U,
    p_value_uncorrected = sens_pvalue,
    effect_size_name = "rank_biserial",
    effect_size = sens_rb_val,
    n_group1 = length(avoidable_sens),
    n_group2 = length(non_avoid_sens),
    significant_uncorrected = sens_pvalue < 0.05,
    deviation_note = "Sensitivity: NT and ACT excluded from CV computation"
  )
}

if (exists("sens_b_pvalue") && !is.na(sens_b_pvalue)) {
  results_list[[length(results_list) + 1]] <- tibble(
    hypothesis = "H2b",
    test = "Mann-Whitney U (excl. NT/ACT)",
    test_type = "sensitivity",
    statistic_name = "U",
    statistic_value = as.numeric(sens_b$statistic),
    p_value_uncorrected = sens_b_pvalue,
    effect_size_name = NA_character_,
    effect_size = NA_real_,
    n_group1 = length(prev_sens),
    n_group2 = length(treat_sens),
    significant_uncorrected = sens_b_pvalue < 0.05,
    deviation_note = "Sensitivity: NT and ACT excluded from CV computation"
  )
}

h2_results <- bind_rows(results_list)
write_csv(h2_results, "outputs/confirmatory/h2_results.csv")

# Human-readable results
sink("outputs/confirmatory/h2_results.txt")
cat("============================================================\n")
cat("CONFIRMATORY HYPOTHESIS 4: Geographic Variation x\n")
cat("Preventability -- Full Results\n")
cat("============================================================\n\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat("Pre-registration: OSF [insert DOI]\n\n")

cat("DATA SUMMARY:\n")
cat("  Total conditions:", nrow(h2), "\n")
cat("  Potentially avoidable:", n_avoidable,
    "(preventable:", n_preventable, ", treatable:", n_treatable, ")\n")
cat("  Non-avoidable:", n_non_avoidable, "\n\n")

cat("DEVIATION FROM PRE-REGISTRATION:\n")
cat("  CVs use ASRs where ABS published them (n_states = 8),\n")
cat("  crude rates where ASRs were suppressed for small jurisdictions.\n")
cat("  Rate type recorded per condition in rate_type column.\n\n")

cat("H2a PRIMARY: Mann-Whitney U\n")
cat("  Z:", round(h2a_statistic, 3), "\n")
cat("  U:", round(U, 0), "\n")
cat("  p-value:", format(h2a_pvalue, digits = 6), "\n")
cat("  Rank-biserial r:", round(h2a_rank_biserial, 3), "\n")
cat("  Median CV avoidable:", round(median(avoidable_cvs), 1), "%\n")
cat("  Median CV non-avoidable:", round(median(non_avoidable_cvs), 1), "%\n\n")

cat("H2a SUPPLEMENTARY: Permutation test\n")
cat("  Observed median difference:", round(observed_diff, 2), "\n")
cat("  p-value:", format(h2a_perm_pvalue, digits = 6), "\n\n")

cat("H2b SECONDARY: Preventable vs Treatable\n")
if (!is.na(h2b_pvalue)) {
  cat("  U:", round(h2b_statistic, 0), "\n")
  cat("  p-value:", format(h2b_pvalue, digits = 6), "\n")
  cat("  Rank-biserial r:", round(h2b_rank_biserial, 3), "\n")
  cat("  Median CV preventable:", round(median(preventable_cvs), 1), "%\n")
  cat("  Median CV treatable:", round(median(treatable_cvs), 1), "%\n\n")
} else {
  cat("  Not testable (insufficient conditions in subcategories).\n\n")
}

cat("SENSITIVITY: Excluding NT and ACT\n")
if (exists("sens_pvalue") && !is.na(sens_pvalue)) {
  cat("  H2a p-value:", format(sens_pvalue, digits = 6), "\n")
  cat("  Rank-biserial r:", round(sens_rb_val, 3), "\n")
}
if (exists("sens_b_pvalue") && !is.na(sens_b_pvalue)) {
  cat("  H2b p-value:", format(sens_b_pvalue, digits = 6), "\n")
}
cat("\n")

cat("NOTE: p-values are UNCORRECTED. Apply Holm-Bonferroni correction\n")
cat("across all confirmatory tests using Script 16 before interpreting\n")
cat("significance.\n")
sink()

cat("  -> Saved: outputs/confirmatory/h2_results.csv\n")
cat("  -> Saved: outputs/confirmatory/h2_results.txt\n\n")
cat("Script 14 complete.\n")
