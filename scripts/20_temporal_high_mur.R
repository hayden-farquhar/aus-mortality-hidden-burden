# ============================================================================
# Script 20: Temporal Trends in High-MUR Conditions
#
# EXPLORATORY ANALYSIS (E-HM3)
#
# Tracks underlying cause death trends over time (2014-2024) for
# conditions with the highest MUR. Reveals whether conditions with
# large "hidden" mortality burdens are becoming more or less visible
# as underlying causes of death.
#
# If high-MUR conditions show increasing underlying cause deaths, this
# suggests their burden is growing even beyond what the MUR captures.
# If decreasing, it may indicate that these conditions are being
# increasingly "hidden" as contributing (rather than underlying) causes.
#
# Includes:
#   - Top 15 high-MUR conditions tracked over time
#   - ICD-10 chapter-level temporal trends
#   - Mann-Kendall trend tests for each condition
#   - Indexed time series visualisations
#
# INPUT: outputs/exploratory/cube10_full_mur_table.csv
#        outputs/exploratory/cube14_temporal_all_causes.csv
#
# OUTPUT: outputs/exploratory/temporal_high_mur.csv
#         outputs/exploratory/temporal_high_mur.txt
#         outputs/figures/fig18_temporal_high_mur.png
#
# PACKAGES: tidyverse, patchwork
# ============================================================================

library(tidyverse)

if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork", repos = "https://cloud.r-project.org")
}
library(patchwork)

cat("============================================================\n")
cat("Script 20: Temporal Trends in High-MUR Conditions\n")
cat("============================================================\n\n")

dir.create("outputs/exploratory", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# Load MUR data to identify high-MUR conditions
# ============================================================================

mur_file <- "outputs/exploratory/cube10_full_mur_table.csv"
if (!file.exists(mur_file)) {
  stop("ERROR: ", mur_file, " not found. Run earlier scripts first.")
}
mur_raw <- read_csv(mur_file, show_col_types = FALSE)
cat("MUR data loaded:", nrow(mur_raw), "rows\n")

# ============================================================================
# Load temporal data
# ============================================================================

temporal_file <- "outputs/exploratory/cube14_temporal_all_causes.csv"
if (!file.exists(temporal_file)) {
  stop("ERROR: ", temporal_file, " not found. Run earlier scripts first.")
}
temporal_raw <- read_csv(temporal_file, show_col_types = FALSE)
cat("Temporal data loaded:", nrow(temporal_raw), "rows\n")

# Get available ICD codes in temporal data
temporal_codes <- temporal_raw %>%
  filter(!is.na(icd_code)) %>%
  distinct(icd_code) %>%
  pull()
cat("Unique ICD codes in temporal data:", length(temporal_codes), "\n")

# ============================================================================
# Identify top high-MUR conditions available in temporal data
# ============================================================================

# Filter MUR data to conditions that:
# 1. Have a valid MUR (persons)
# 2. Have >= 50 underlying deaths (for stable trends)
# 3. Have a matching ICD code in the temporal data
# 4. Are not chapter headers or totals

# Classify rows
mur_classified <- mur_raw %>%
  filter(
    !is.na(mur_persons),
    is.finite(mur_persons),
    !is.na(icd_code),
    underlying_persons >= 50,
    cause != "Total deaths",
    !str_detect(cause, "^CHAPTER ")
  ) %>%
  # Check which are available in temporal data

  mutate(in_temporal = icd_code %in% temporal_codes) %>%
  arrange(desc(mur_persons))

cat("\nHigh-MUR conditions with temporal data available:\n")
cat("  Total conditions with MUR and >= 50 deaths:", nrow(mur_classified), "\n")
cat("  Available in Cube 14 temporal:", sum(mur_classified$in_temporal), "\n")

# Select top 15 conditions available in temporal data
top_mur <- mur_classified %>%
  filter(in_temporal) %>%
  head(15) %>%
  select(cause, icd_code, mur_persons, underlying_persons, multiple_persons)

cat("\nTop 15 high-MUR conditions for temporal analysis:\n")
for (i in seq_len(nrow(top_mur))) {
  row <- top_mur[i, ]
  cat(sprintf("  %2d. %-45s [%s]  MUR = %.1f\n",
              i, str_trunc(row$cause, 45), row$icd_code, row$mur_persons))
}

# ============================================================================
# Extract temporal data for selected conditions
# ============================================================================

temporal_selected <- temporal_raw %>%
  filter(
    icd_code %in% top_mur$icd_code,
    sex == "persons"
  ) %>%
  left_join(top_mur %>% select(icd_code, mur_persons), by = "icd_code") %>%
  arrange(icd_code, year)

cat("\nTemporal data extracted:", nrow(temporal_selected), "observations\n")
cat("Year range:", min(temporal_selected$year), "-", max(temporal_selected$year), "\n")

# ============================================================================
# Also extract chapter-level trends for context
# ============================================================================

# Get chapter-level ICD codes from temporal data
chapter_codes <- temporal_raw %>%
  filter(str_detect(icd_code, "-")) %>%
  distinct(icd_code) %>%
  pull()

# Get chapter-level MUR
chapter_mur <- mur_raw %>%
  filter(str_detect(cause, "^CHAPTER "), !is.na(mur_persons)) %>%
  select(cause, icd_code, mur_persons) %>%
  mutate(short_name = str_replace(cause, "^CHAPTER [IVXLC]+ ", "") %>%
           str_replace("\\s*\\(.*\\)", ""))

# Top 5 chapters by MUR
top_chapters <- chapter_mur %>%
  filter(icd_code %in% chapter_codes) %>%
  arrange(desc(mur_persons)) %>%
  head(5)

temporal_chapters <- temporal_raw %>%
  filter(
    icd_code %in% top_chapters$icd_code,
    sex == "persons"
  ) %>%
  left_join(top_chapters %>% select(icd_code, mur_persons, short_name),
            by = "icd_code")

# ============================================================================
# Trend tests (Mann-Kendall via Spearman on year)
# ============================================================================

cat("\n============================================================\n")
cat("TREND TESTS (Spearman: year vs deaths)\n")
cat("============================================================\n\n")

trend_results <- temporal_selected %>%
  arrange(icd_code, year) %>%
  group_by(icd_code, cause, mur_persons) %>%
  summarise(
    n_years = n(),
    first_year = min(year),
    last_year = max(year),
    deaths_first = deaths[which.min(year)],
    deaths_last = deaths[which.max(year)],
    rho = if (n() >= 4) {
      as.double(cor.test(year, deaths, method = "spearman", exact = FALSE)$estimate)
    } else NA_real_,
    p_value = if (n() >= 4) {
      cor.test(year, deaths, method = "spearman", exact = FALSE)$p.value
    } else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    pct_change = 100 * (deaths_last / deaths_first - 1),
    direction = case_when(
      rho > 0.3 & p_value < 0.1 ~ "Increasing",
      rho < -0.3 & p_value < 0.1 ~ "Decreasing",
      TRUE ~ "Stable/unclear"
    ),
    # Clean condition name
    condition_name = str_replace(cause, "\\s*\\([A-Z]\\d{2}.*\\)\\s*$", "") %>%
      str_trim()
  ) %>%
  arrange(desc(mur_persons))

for (i in seq_len(nrow(trend_results))) {
  row <- trend_results[i, ]
  sig <- ifelse(!is.na(row$p_value) & row$p_value < 0.05, " *", "")
  cat(sprintf("  %-40s [%s]  MUR=%.1f\n",
              str_trunc(row$condition_name, 40), row$icd_code, row$mur_persons))
  cat(sprintf("    Deaths: %s -> %s (%+.1f%%)  rho=%+.3f  p=%.4f%s  %s\n\n",
              format(row$deaths_first, big.mark = ","),
              format(row$deaths_last, big.mark = ","),
              row$pct_change,
              ifelse(is.na(row$rho), NA, row$rho),
              ifelse(is.na(row$p_value), NA, row$p_value),
              sig, row$direction))
}

# ============================================================================
# Save results
# ============================================================================

write_csv(trend_results, "outputs/exploratory/temporal_high_mur.csv")
cat("Trend results saved: outputs/exploratory/temporal_high_mur.csv\n")

# Also save the full temporal series for selected conditions
write_csv(temporal_selected, "outputs/exploratory/temporal_high_mur_series.csv")

# ============================================================================
# Write summary text
# ============================================================================

sink("outputs/exploratory/temporal_high_mur.txt")

cat("============================================================\n")
cat("TEMPORAL TRENDS IN HIGH-MUR CONDITIONS\n")
cat("Exploratory Analysis E-HM3\n")
cat("============================================================\n\n")
cat(sprintf("Date: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M")))
cat(sprintf("Conditions analysed: %d\n", nrow(trend_results)))
cat(sprintf("Year range: %d-%d\n\n", min(temporal_selected$year),
            max(temporal_selected$year)))

cat("SUMMARY:\n\n")
n_increasing <- sum(trend_results$direction == "Increasing", na.rm = TRUE)
n_decreasing <- sum(trend_results$direction == "Decreasing", na.rm = TRUE)
n_stable <- sum(trend_results$direction == "Stable/unclear", na.rm = TRUE)
cat(sprintf("  Increasing: %d conditions\n", n_increasing))
cat(sprintf("  Decreasing: %d conditions\n", n_decreasing))
cat(sprintf("  Stable/unclear: %d conditions\n\n", n_stable))

cat("CONDITION-LEVEL RESULTS:\n")
cat(sprintf("%-42s %6s %10s %10s %8s %6s %6s %s\n",
            "Condition", "ICD", "First", "Last", "%Change", "Rho", "p", "Trend"))
cat(paste(rep("-", 100), collapse = ""), "\n")

for (i in seq_len(nrow(trend_results))) {
  row <- trend_results[i, ]
  cat(sprintf("%-42s %6s %10s %10s %+7.1f%% %+5.3f %.4f %s\n",
              str_trunc(row$condition_name, 42),
              row$icd_code,
              format(row$deaths_first, big.mark = ","),
              format(row$deaths_last, big.mark = ","),
              row$pct_change,
              ifelse(is.na(row$rho), NA, row$rho),
              ifelse(is.na(row$p_value), NA, row$p_value),
              row$direction))
}

cat("\nINTERPRETATION:\n\n")
cat("High-MUR conditions are those whose mortality burden is most\n")
cat("'hidden' — they appear far more often as contributing causes than\n")
cat("as the underlying cause of death.\n\n")

if (n_increasing > n_decreasing) {
  cat("Most high-MUR conditions show INCREASING underlying cause deaths\n")
  cat("over 2014-2024. This means their total burden (hidden + visible)\n")
  cat("is likely growing even faster, as the MUR multiplier amplifies\n")
  cat("any increase in underlying cause deaths.\n\n")
} else if (n_decreasing > n_increasing) {
  cat("Most high-MUR conditions show DECREASING underlying cause deaths.\n")
  cat("Combined with high MUR, this could indicate these conditions are\n")
  cat("being increasingly attributed as contributing rather than underlying\n")
  cat("causes, further 'hiding' their burden.\n\n")
}

cat("NOTES:\n\n")
cat("1. MUR data: ABS Cube 10 (2023, multiple causes of death).\n")
cat("2. Temporal data: ABS Cube 14 (year of occurrence, underlying cause\n")
cat("   only, 2014-2024). Death counts are raw (not age-standardised).\n")
cat("3. Conditions selected: top 15 by MUR with >= 50 underlying deaths\n")
cat("   and available in Cube 14.\n")
cat("4. Trend direction classified by Spearman rho (>0.3 or <-0.3)\n")
cat("   and p < 0.1.\n")
cat("5. 2024 data may be preliminary/incomplete in some jurisdictions.\n")

sink()

cat("Summary text saved: outputs/exploratory/temporal_high_mur.txt\n")

# ============================================================================
# Figure: Temporal trends for high-MUR conditions
# ============================================================================

# Panel A: Indexed trends for top 8 conditions
top8 <- trend_results %>% head(8)

temporal_top8 <- temporal_selected %>%
  filter(icd_code %in% top8$icd_code) %>%
  group_by(icd_code) %>%
  mutate(
    indexed = 100 * deaths / deaths[which.min(year)],
    label = paste0(str_replace(cause, "\\s*\\([A-Z]\\d{2}.*\\)", ""),
                   " [MUR=", round(mur_persons, 1), "]") %>%
      str_trunc(55)
  ) %>%
  ungroup()

p_a <- ggplot(temporal_top8, aes(x = year, y = indexed,
                                  colour = label, group = label)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = 100, linetype = "dotted", colour = "grey50") +
  scale_colour_brewer(palette = "Set2") +
  labs(
    title = "A. Indexed Trends: Top 8 High-MUR Conditions",
    subtitle = sprintf("Underlying cause deaths indexed to %d = 100",
                       min(temporal_top8$year)),
    x = "Year",
    y = "Index (baseline = 100)",
    colour = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, colour = "grey40"),
    legend.position = "bottom",
    legend.text = element_text(size = 9)
  ) +
  guides(colour = guide_legend(ncol = 2))

# Panel B: Absolute death counts for same conditions
p_b <- ggplot(temporal_top8, aes(x = year, y = deaths,
                                  colour = label, group = label)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_brewer(palette = "Set2") +
  labs(
    title = "B. Absolute Deaths: Top 8 High-MUR Conditions",
    subtitle = "Underlying cause deaths (Cube 14, year of occurrence)",
    x = "Year",
    y = "Deaths (underlying cause)",
    colour = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, colour = "grey40"),
    legend.position = "bottom",
    legend.text = element_text(size = 9)
  ) +
  guides(colour = guide_legend(ncol = 2))

# Panel C: Chapter-level indexed trends
if (nrow(temporal_chapters) > 0) {
  temporal_chapters_indexed <- temporal_chapters %>%
    group_by(icd_code) %>%
    mutate(
      indexed = 100 * deaths / deaths[which.min(year)],
      label = paste0(short_name, " [MUR=", round(mur_persons, 1), "]") %>%
        str_trunc(50)
    ) %>%
    ungroup()

  p_c <- ggplot(temporal_chapters_indexed,
                aes(x = year, y = indexed, colour = label)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_hline(yintercept = 100, linetype = "dotted", colour = "grey50") +
    scale_colour_brewer(palette = "Dark2") +
    labs(
      title = "C. Chapter-Level Trends (Top 5 by MUR)",
      subtitle = sprintf("Indexed to %d = 100", min(temporal_chapters_indexed$year)),
      x = "Year",
      y = "Index (baseline = 100)",
      colour = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, colour = "grey40"),
      legend.position = "bottom",
      legend.text = element_text(size = 10)
    ) +
    guides(colour = guide_legend(ncol = 2))
} else {
  p_c <- ggplot() + theme_void()
}

# Panel D: MUR vs trend direction (bubble chart)
p_d <- ggplot(trend_results,
              aes(x = pct_change, y = mur_persons)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(aes(size = deaths_last, colour = direction), alpha = 0.7) +
  geom_text(aes(label = icd_code), hjust = -0.2, vjust = -0.5,
            size = 3.2, colour = "grey30") +
  scale_colour_manual(values = c(
    "Increasing" = "#d7191c",
    "Decreasing" = "#2c7bb6",
    "Stable/unclear" = "grey60"
  )) +
  scale_size_continuous(range = c(2, 8), labels = scales::comma,
                        name = "Deaths\n(latest year)") +
  labs(
    title = "D. MUR vs Mortality Trend Direction",
    subtitle = "Each point is a high-MUR condition",
    x = "% change in underlying deaths (first to last year)",
    y = "MUR (2023)",
    colour = "Trend"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, colour = "grey40"),
    legend.position = "right",
    legend.text = element_text(size = 10)
  )

# Combine
p_combined <- (p_a | p_b) / (p_c | p_d) +
  plot_annotation(
    title = "Temporal Trends in Underlying Cause Deaths for High-MUR Conditions",
    subtitle = "Conditions with the greatest 'hidden' mortality burden: are they becoming more or less visible?",
    caption = "Sources: ABS Causes of Death 2023 Cube 10 (MUR), ABS Cube 14 (temporal, 2014-2024)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12, colour = "grey40"),
      plot.caption = element_text(size = 10, colour = "grey50")
    )
  )

ggsave("outputs/figures/fig18_temporal_high_mur.png", p_combined,
       width = 16, height = 13, dpi = 300, bg = "white")

cat("Figure saved: outputs/figures/fig18_temporal_high_mur.png\n")

cat("\n============================================================\n")
cat("Script 20 complete.\n")
cat("============================================================\n")
