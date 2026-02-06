# ============================================================================
# Script 17: Full MUR Ranking — Supplementary Table
#
# EXPLORATORY ANALYSIS (E-MUR1)
#
# Creates a comprehensive ranked table of all conditions by their
# Multiple-to-Underlying cause Ratio (MUR). The MUR measures the
# "hidden burden" of mortality — conditions with high MUR appear
# far more often as contributing causes than as the underlying cause
# of death, indicating their burden is under-represented in standard
# cause-of-death statistics.
#
# Includes:
#   - Full ranked table of all conditions with sufficient deaths
#   - Summary statistics by ICD-10 chapter
#   - Top 30 conditions figure (Cleveland dot plot)
#   - Sex-stratified MUR comparison for top conditions
#
# INPUT: outputs/exploratory/cube10_full_mur_table.csv
#        outputs/deaths_underlying_vs_multiple.csv
#
# OUTPUT: outputs/exploratory/mur_ranking_full.csv
#         outputs/exploratory/mur_ranking_summary.txt
#         outputs/figures/fig15_mur_ranking_top30.png
#
# PACKAGES: tidyverse
# ============================================================================

library(tidyverse)

cat("============================================================\n")
cat("Script 17: Full MUR Ranking — Supplementary Table\n")
cat("============================================================\n\n")

dir.create("outputs/exploratory", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# Load data
# ============================================================================

mur_file <- "outputs/exploratory/cube10_full_mur_table.csv"
if (!file.exists(mur_file)) {
  stop("ERROR: ", mur_file, " not found. Run earlier scripts first.")
}

mur_raw <- read_csv(mur_file, show_col_types = FALSE)
cat("MUR data loaded:", nrow(mur_raw), "rows\n")

# Also load the underlying vs multiple file for chapter-level flags
uvm_file <- "outputs/deaths_underlying_vs_multiple.csv"
if (file.exists(uvm_file)) {
  uvm <- read_csv(uvm_file, show_col_types = FALSE)
  cat("Underlying vs multiple data loaded:", nrow(uvm), "rows\n")
} else {
  uvm <- NULL
  cat("WARNING: deaths_underlying_vs_multiple.csv not found.\n")
}

# ============================================================================
# Identify condition levels
# ============================================================================

# Classify each row by its hierarchical level:
#   - "total": the total deaths row
#   - "chapter": ICD-10 chapter headers (CHAPTER I, II, etc.)
#   - "block": ICD-10 block groupings (e.g., A00-A09, I10-I15)
#   - "code": individual 3-character ICD-10 codes (e.g., A41, I10)
#   - "header": section headers with no data (e.g., "Causes of death")

mur_classified <- mur_raw %>%
  mutate(
    level = case_when(
      cause == "Total deaths" ~ "total",
      str_detect(cause, "^CHAPTER ") ~ "chapter",
      is.na(unit) | unit == "NA" ~ "header",
      # Block-level: ICD code contains a dash (range like A00-A09)
      !is.na(icd_code) & str_detect(icd_code, "-") ~ "block",
      # Individual code: single ICD-10 code (e.g., A41, I10)
      !is.na(icd_code) & !str_detect(icd_code, "-") ~ "code",
      TRUE ~ "header"
    ),
    # Extract clean condition name
    condition_name = str_replace(cause, "^CHAPTER [IVXLC]+ ", "") %>%
      str_replace("\\s*\\([A-Z]\\d{2}.*\\)\\s*$", "") %>%
      str_trim(),
    # Map to chapter for grouping
    chapter_code = case_when(
      !is.na(icd_code) ~ str_extract(icd_code, "^[A-Z]"),
      TRUE ~ NA_character_
    ),
    chapter_name = case_when(
      chapter_code %in% c("A", "B") ~ "I: Infectious diseases",
      chapter_code == "C" | (chapter_code == "D" & str_detect(icd_code, "^D[0-4]")) ~ "II: Neoplasms",
      chapter_code == "D" ~ "III: Blood/immune",
      chapter_code == "E" ~ "IV: Endocrine/metabolic",
      chapter_code == "F" ~ "V: Mental/behavioural",
      chapter_code == "G" ~ "VI: Nervous system",
      chapter_code == "H" & str_detect(icd_code, "^H[0-5]") ~ "VII: Eye",
      chapter_code == "H" ~ "VIII: Ear",
      chapter_code == "I" ~ "IX: Circulatory system",
      chapter_code == "J" ~ "X: Respiratory",
      chapter_code == "K" ~ "XI: Digestive",
      chapter_code == "L" ~ "XII: Skin",
      chapter_code == "M" ~ "XIII: Musculoskeletal",
      chapter_code == "N" ~ "XIV: Genitourinary",
      chapter_code == "O" ~ "XV: Obstetric",
      chapter_code == "P" ~ "XVI: Perinatal",
      chapter_code == "Q" ~ "XVII: Congenital",
      chapter_code == "R" ~ "XVIII: Symptoms/signs",
      chapter_code %in% c("S", "T") ~ "XIX: Injury/poisoning",
      chapter_code %in% c("V", "W", "X", "Y") ~ "XX: External causes",
      TRUE ~ NA_character_
    )
  )

cat("Classification counts:\n")
print(table(mur_classified$level))

# ============================================================================
# Filter to rankable conditions
# ============================================================================

# Minimum threshold: 10 underlying deaths (persons) for stability
MIN_DEATHS <- 10

mur_rankable <- mur_classified %>%
  filter(
    level %in% c("block", "code"),
    !is.na(mur_persons),
    is.finite(mur_persons),
    underlying_persons >= MIN_DEATHS
  ) %>%
  arrange(desc(mur_persons))

cat("\nRankable conditions (>= ", MIN_DEATHS, " underlying deaths):",
    nrow(mur_rankable), "\n")

# ============================================================================
# Create full ranking table
# ============================================================================

mur_ranking <- mur_rankable %>%
  mutate(rank = row_number()) %>%
  select(
    rank,
    icd_code,
    condition_name,
    level,
    chapter_name,
    underlying_persons,
    multiple_persons,
    mur_persons,
    mur_male,
    mur_female
  ) %>%
  mutate(
    hidden_deaths = multiple_persons - underlying_persons,
    pct_hidden = round(100 * (1 - underlying_persons / multiple_persons), 1),
    sex_ratio = round(mur_male / mur_female, 2),
    mur_persons = round(mur_persons, 1),
    mur_male = round(mur_male, 1),
    mur_female = round(mur_female, 1)
  )

# Save full ranking
write_csv(mur_ranking, "outputs/exploratory/mur_ranking_full.csv")
cat("Full ranking saved:", nrow(mur_ranking), "conditions\n")

# ============================================================================
# Chapter-level summary
# ============================================================================

chapter_summary <- mur_classified %>%
  filter(level == "chapter", !is.na(mur_persons)) %>%
  arrange(desc(mur_persons)) %>%
  select(
    icd_code, condition_name,
    underlying_persons, multiple_persons,
    mur_persons, mur_male, mur_female
  ) %>%
  mutate(
    hidden_deaths = multiple_persons - underlying_persons,
    pct_hidden = round(100 * (1 - underlying_persons / multiple_persons), 1)
  )

cat("\n--- Chapter-Level MUR Summary ---\n")
for (i in seq_len(nrow(chapter_summary))) {
  row <- chapter_summary[i, ]
  cat(sprintf("  %-50s  MUR = %5.1f  (hidden: %s%%)\n",
              row$condition_name, row$mur_persons, row$pct_hidden))
}

# ============================================================================
# Summary statistics
# ============================================================================

# Top 10 by MUR
top10 <- mur_ranking %>% head(10)

# Bottom 10 (lowest MUR among conditions with enough deaths)
bottom10 <- mur_ranking %>% tail(10)

# Conditions where MUR > 10 (very large hidden burden)
very_high <- mur_ranking %>% filter(mur_persons > 10)

# Sex-stratified: conditions where male MUR >> female (or vice versa)
sex_divergent <- mur_ranking %>%
  filter(!is.na(sex_ratio), is.finite(sex_ratio)) %>%
  filter(sex_ratio > 2 | sex_ratio < 0.5) %>%
  arrange(desc(abs(log(sex_ratio))))

# ============================================================================
# Write summary text
# ============================================================================

sink("outputs/exploratory/mur_ranking_summary.txt")

cat("============================================================\n")
cat("FULL MUR RANKING — SUPPLEMENTARY TABLE SUMMARY\n")
cat("============================================================\n\n")
cat(sprintf("Date: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M")))
cat(sprintf("Total rankable conditions: %d\n", nrow(mur_ranking)))
cat(sprintf("Minimum underlying deaths threshold: %d\n", MIN_DEATHS))
cat(sprintf("Conditions with MUR > 10: %d\n", nrow(very_high)))
cat(sprintf("Median MUR (all conditions): %.1f\n", median(mur_ranking$mur_persons)))
cat(sprintf("Mean MUR (all conditions): %.1f\n", mean(mur_ranking$mur_persons)))
cat("\n")

cat("============================================================\n")
cat("TOP 10 CONDITIONS BY MUR (HIGHEST HIDDEN BURDEN)\n")
cat("============================================================\n\n")

for (i in seq_len(nrow(top10))) {
  row <- top10[i, ]
  cat(sprintf("  %2d. %-45s [%s]\n", row$rank, row$condition_name, row$icd_code))
  cat(sprintf("      MUR = %.1f  |  Underlying: %s  |  Multiple: %s  |  Hidden: %s%%\n",
              row$mur_persons, format(row$underlying_persons, big.mark = ","),
              format(row$multiple_persons, big.mark = ","), row$pct_hidden))
  cat(sprintf("      Male MUR: %.1f  |  Female MUR: %.1f  |  Sex ratio: %.2f\n\n",
              row$mur_male, row$mur_female,
              ifelse(is.finite(row$sex_ratio), row$sex_ratio, NA)))
}

cat("============================================================\n")
cat("BOTTOM 10 CONDITIONS BY MUR (LOWEST HIDDEN BURDEN)\n")
cat("============================================================\n\n")

for (i in seq_len(nrow(bottom10))) {
  row <- bottom10[i, ]
  cat(sprintf("  %3d. %-45s [%s]\n", row$rank, row$condition_name, row$icd_code))
  cat(sprintf("       MUR = %.1f  |  Underlying: %s  |  Multiple: %s\n\n",
              row$mur_persons, format(row$underlying_persons, big.mark = ","),
              format(row$multiple_persons, big.mark = ",")))
}

cat("============================================================\n")
cat("CHAPTER-LEVEL MUR SUMMARY\n")
cat("============================================================\n\n")

for (i in seq_len(nrow(chapter_summary))) {
  row <- chapter_summary[i, ]
  cat(sprintf("  %-55s MUR = %5.1f\n", row$condition_name, row$mur_persons))
  cat(sprintf("    Underlying: %s  |  Multiple: %s  |  Hidden: %s%%\n\n",
              format(row$underlying_persons, big.mark = ","),
              format(row$multiple_persons, big.mark = ","), row$pct_hidden))
}

if (nrow(sex_divergent) > 0) {
  cat("============================================================\n")
  cat("SEX-DIVERGENT CONDITIONS (M/F MUR ratio > 2 or < 0.5)\n")
  cat("============================================================\n\n")
  for (i in seq_len(min(15, nrow(sex_divergent)))) {
    row <- sex_divergent[i, ]
    direction <- ifelse(row$sex_ratio > 1, "male >> female", "female >> male")
    cat(sprintf("  %-45s [%s]\n", row$condition_name, row$icd_code))
    cat(sprintf("    MUR M=%.1f  F=%.1f  Ratio=%.2f  (%s)\n\n",
                row$mur_male, row$mur_female, row$sex_ratio, direction))
  }
}

cat("============================================================\n")
cat("NOTES\n")
cat("============================================================\n\n")
cat("1. MUR = Multiple cause mentions / Underlying cause deaths.\n")
cat("   A MUR of 5.0 means the condition appears 5x more often as any\n")
cat("   cause of death than as the underlying cause.\n\n")
cat("2. 'Hidden deaths' = multiple mentions - underlying deaths.\n")
cat("   These represent deaths where the condition contributed but was\n")
cat("   not selected as the underlying cause.\n\n")
cat("3. Data source: ABS Cube 10, Causes of Death 2023.\n")
cat(sprintf("4. Conditions with < %d underlying deaths excluded for stability.\n", MIN_DEATHS))
cat("5. Sex ratio > 1 indicates higher relative hidden burden in males.\n")

sink()

cat("\nSummary text saved to outputs/exploratory/mur_ranking_summary.txt\n")

# ============================================================================
# Figure: Top 30 conditions by MUR (Cleveland dot plot)
# ============================================================================

top30 <- mur_ranking %>% head(30)

p <- ggplot(top30, aes(x = mur_persons,
                       y = reorder(paste0(condition_name, " [", icd_code, "]"),
                                   mur_persons))) +
  geom_segment(aes(x = 0, xend = mur_persons,
                   y = reorder(paste0(condition_name, " [", icd_code, "]"),
                               mur_persons),
                   yend = reorder(paste0(condition_name, " [", icd_code, "]"),
                                  mur_persons)),
               colour = "grey70", linewidth = 0.6) +
  geom_point(size = 4, colour = "#2c7bb6") +
  geom_text(aes(label = sprintf("%.1f", mur_persons)),
            hjust = -0.3, size = 3.8, colour = "grey30") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Top 30 Conditions by Multiple-to-Underlying Cause Ratio",
    subtitle = paste0("Conditions with the greatest 'hidden' mortality burden | ",
                      "Minimum ", MIN_DEATHS, " underlying deaths"),
    x = "Ratio (multiple / underlying cause)",
    y = NULL,
    caption = "Source: ABS Causes of Death 2023, Cube 10 (multiple causes)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, colour = "grey40"),
    plot.caption = element_text(size = 9, colour = "grey50"),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave("outputs/figures/fig15_mur_ranking_top30.png", p,
       width = 12, height = 13, dpi = 300, bg = "white")

cat("Figure saved: outputs/figures/fig15_mur_ranking_top30.png\n")

# ============================================================================
# Figure: Sex-stratified MUR for top 20 conditions
# ============================================================================

top20_sex <- mur_ranking %>%
  head(20) %>%
  filter(!is.na(mur_male), !is.na(mur_female)) %>%
  select(condition_name, icd_code, mur_male, mur_female) %>%
  pivot_longer(cols = c(mur_male, mur_female),
               names_to = "sex", values_to = "mur") %>%
  mutate(
    sex = ifelse(sex == "mur_male", "Male", "Female"),
    label = paste0(condition_name, " [", icd_code, "]")
  )

if (nrow(top20_sex) > 0) {
  p_sex <- ggplot(top20_sex, aes(x = mur, y = reorder(label, mur),
                                  colour = sex)) +
    geom_point(size = 3.5, position = position_dodge(width = 0.5)) +
    scale_colour_manual(values = c("Male" = "#2c7bb6", "Female" = "#d7191c")) +
    labs(
      title = "Sex-Stratified MUR for Top 20 High-MUR Conditions",
      subtitle = "Comparing male and female hidden burden ratios",
      x = "MUR",
      y = NULL,
      colour = "Sex",
      caption = "Source: ABS Causes of Death 2023, Cube 10"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 11, colour = "grey40"),
      plot.caption = element_text(size = 9, colour = "grey50"),
      axis.text.y = element_text(size = 10),
      legend.position = "top",
      legend.text = element_text(size = 11)
    )

  ggsave("outputs/figures/fig15b_mur_ranking_sex.png", p_sex,
         width = 12, height = 10, dpi = 300, bg = "white")

  cat("Figure saved: outputs/figures/fig15b_mur_ranking_sex.png\n")
}

cat("\n============================================================\n")
cat("Script 17 complete.\n")
cat("============================================================\n")
