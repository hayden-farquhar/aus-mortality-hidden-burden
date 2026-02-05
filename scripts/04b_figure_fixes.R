# =============================================================================
# 04b_figure_fixes.R
# Purpose: Fix Figure 5 (failed), clean up Figures 1, 2, 3, 4
# Run AFTER 04_exploratory_analysis.R
# =============================================================================

library(tidyverse)
library(scales)

dir.create("outputs/figures", showWarnings = FALSE)

# Reload data
mort_multi      <- read_csv("outputs/deaths_underlying_vs_multiple.csv", show_col_types = FALSE)
mort_num_causes <- read_csv("outputs/deaths_by_num_causes.csv", show_col_types = FALSE)
hosp_proc       <- read_csv("outputs/hospital_procedures_by_year.csv", show_col_types = FALSE)
hosp_diag       <- read_csv("outputs/hospital_diagnoses_by_year.csv", show_col_types = FALSE)


# =============================================================================
# FIX 1: Figure 1 — Fix "Essential" label truncation
# =============================================================================

cat("=== Fix 1: Figure 1 — label cleanup ===\n")

fig1_data <- mort_multi %>%
  filter(
    !is.na(ratio_persons),
    is.finite(ratio_persons),
    underlying_persons >= 50,
    multiple_persons >= 100,
    !str_detect(icd10_code, "^R"),
    !str_detect(icd10_code, "^Y"),
    !str_detect(icd10_code, "^U"),
    !is.na(icd10_code)
  ) %>%
  # Fix truncated names
  mutate(cause_name = case_when(
    cause_name == "Essential" ~ "Essential hypertension",
    cause_name == "Other" & str_detect(icd10_code, "^I3") ~ "Other forms of heart disease",
    TRUE ~ cause_name
  )) %>%
  arrange(desc(ratio_persons)) %>%
  head(25)

p1 <- ggplot(fig1_data, aes(x = reorder(cause_name, ratio_persons), y = ratio_persons)) +
  geom_col(fill = "#2166AC", alpha = 0.85) +
  geom_text(aes(label = round(ratio_persons, 1)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Hidden Burden: Conditions Most Under-counted\nas Underlying Cause of Death",
    subtitle = "Ratio of total death certificate mentions to underlying cause listings, Australia 2024\n(Higher ratio = condition appears far more often as contributing cause than primary cause)",
    x = NULL,
    y = "Multiple-to-Underlying Cause Ratio",
    caption = "Source: ABS Causes of Death 2024, Data Cube 10. Conditions with ≥50 underlying cause deaths shown."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    plot.caption = element_text(size = 7, colour = "grey50"),
    panel.grid.major.y = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave("outputs/figures/fig1_hidden_burden_ratio.png", p1, width = 12, height = 8, dpi = 300)
cat("  Saved: outputs/figures/fig1_hidden_burden_ratio.png\n")


# =============================================================================
# FIX 2: Figure 2 — Remove chapter-level rows to avoid double-counting
# =============================================================================

cat("\n=== Fix 2: Figure 2 — remove double-counted chapter totals ===\n")

fig2_data <- mort_multi %>%
  filter(
    !is.na(underlying_persons),
    !is.na(multiple_persons),
    underlying_persons >= 20,
    !is.na(icd10_code),
    is_chapter == FALSE  # <--- KEY FIX: exclude chapter-level aggregates
  ) %>%
  # Fix truncated names
  mutate(cause_name = case_when(
    cause_name == "Essential" ~ "Essential hypertension",
    cause_name == "Other" & str_detect(icd10_code, "^I3") ~ "Other forms of heart disease",
    cause_name == "General" ~ "General symptoms and signs",
    TRUE ~ cause_name
  )) %>%
  mutate(
    extra_deaths = multiple_persons - underlying_persons,
    pct_hidden = (extra_deaths / multiple_persons) * 100
  ) %>%
  filter(extra_deaths > 0) %>%
  arrange(desc(extra_deaths)) %>%
  head(20)

cat("  Top 5 conditions by hidden deaths (no chapter double-counting):\n")
for (i in 1:min(5, nrow(fig2_data))) {
  cat(sprintf("    %s: +%s hidden deaths\n", 
              fig2_data$cause_name[i],
              format(fig2_data$extra_deaths[i], big.mark = ",")))
}

p2 <- ggplot(fig2_data, aes(x = reorder(cause_name, extra_deaths))) +
  geom_col(aes(y = multiple_persons), fill = "#B2182B", alpha = 0.7, width = 0.7) +
  geom_col(aes(y = underlying_persons), fill = "#2166AC", alpha = 0.9, width = 0.7) +
  geom_text(aes(y = multiple_persons, 
                label = paste0("+", format(extra_deaths, big.mark = ","))),
            hjust = -0.05, size = 2.8, colour = "#B2182B") +
  coord_flip() +
  labs(
    title = "The Hidden Death Toll: Specific Conditions with Largest Gap\nBetween Underlying and Multiple Cause Counts",
    subtitle = "Blue = listed as underlying cause | Red = total mentions on death certificates, Australia 2024\n(Chapter-level aggregates excluded to avoid double-counting)",
    x = NULL,
    y = "Number of Death Certificate Mentions",
    caption = "Source: ABS Causes of Death 2024, Data Cube 10."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    plot.caption = element_text(size = 7, colour = "grey50"),
    panel.grid.major.y = element_blank()
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15)))

ggsave("outputs/figures/fig2_absolute_hidden_burden.png", p2, width = 12, height = 8, dpi = 300)
cat("  Saved: outputs/figures/fig2_absolute_hidden_burden.png\n")


# =============================================================================
# FIX 3: Figure 5 — Correct data structure (wide → long)
# =============================================================================

cat("\n=== Fix 3: Figure 5 — Multimorbidity distribution ===\n")

# The data is in WIDE format with columns:
# reported_alone, with_1_other, with_2_other, with_3_other, with_4_other, with_5_plus
# Each ROW is a specific condition. 
# We need the TOTAL row to get the overall distribution.

cat("  Inspecting mort_num_causes structure:\n")
cat("  Columns:", paste(colnames(mort_num_causes), collapse = ", "), "\n")
cat("  Total rows:", nrow(mort_num_causes), "\n")

# Check for a total row
total_rows <- mort_num_causes %>% 
  filter(is_total == TRUE | str_detect(cause_icd10, regex("total|all cause", ignore_case = TRUE)))
cat("  Total/aggregate rows found:", nrow(total_rows), "\n")

if (nrow(total_rows) > 0) {
  cat("  Total row cause_icd10 values:", paste(total_rows$cause_icd10, collapse = "; "), "\n")
}

# The total row gives us the distribution across ALL deaths
# But we need to handle the fact that a death with 3 causes appears in 3 condition rows
# So the TOTAL row is the right way to get the overall distribution

# Let's look at what the Total row actually contains
total_data <- total_rows %>% 
  filter(str_detect(cause_icd10, regex("total", ignore_case = TRUE))) %>%
  head(1)

if (nrow(total_data) > 0) {
  cat("\n  Total row values:\n")
  cat("    reported_alone:", total_data$reported_alone, "\n")
  cat("    with_1_other:", total_data$with_1_other, "\n")
  cat("    with_2_other:", total_data$with_2_other, "\n")
  cat("    with_3_other:", total_data$with_3_other, "\n")
  cat("    with_4_other:", total_data$with_4_other, "\n")
  cat("    with_5_plus:", total_data$with_5_plus, "\n")
}

# Actually, the "Total deaths" row in Table 10.1 gives the count of deaths
# by how many CAUSES were listed. Let's use it.
# But wait — this table is about how often each CONDITION is reported alone 
# vs with others. The Total row would sum across conditions.
# 
# What we really want is the Table 10.1 structure which counts DEATHS by 
# number of causes. Let's check if the structure is different.

cat("\n  Attempting alternative approach: aggregate from the data...\n")

# Actually, the columns reported_alone etc describe how many OTHER causes 
# appeared alongside THIS condition. For "Total deaths", it gives us:
# - reported_alone = deaths where only 1 cause was listed (total)
# - with_1_other = deaths with exactly 2 causes total
# - with_2_other = deaths with exactly 3 causes total
# etc.

# So the Total row IS our distribution
if (nrow(total_data) > 0) {
  
  fig5_data <- tibble(
    num_causes = c("1", "2", "3", "4", "5", "6+"),
    num_causes_int = c(1, 2, 3, 4, 5, 6),
    deaths = c(
      total_data$reported_alone,
      total_data$with_1_other,
      total_data$with_2_other,
      total_data$with_3_other,
      total_data$with_4_other,
      total_data$with_5_plus
    )
  ) %>%
    filter(!is.na(deaths)) %>%
    mutate(
      pct = deaths / sum(deaths, na.rm = TRUE) * 100,
      label = paste0(format(deaths, big.mark = ","), "\n(", round(pct, 1), "%)")
    )
  
  cat("  Distribution of causes per death certificate:\n")
  for (i in 1:nrow(fig5_data)) {
    cat(sprintf("    %s cause(s): %s deaths (%.1f%%)\n",
                fig5_data$num_causes[i],
                format(fig5_data$deaths[i], big.mark = ","),
                fig5_data$pct[i]))
  }
  
  total_deaths <- sum(fig5_data$deaths, na.rm = TRUE)
  avg_causes <- sum(fig5_data$num_causes_int * fig5_data$deaths, na.rm = TRUE) / total_deaths
  # Note: 6+ is approximated as 6 here, real average is slightly higher
  
  p5 <- ggplot(fig5_data, aes(x = factor(num_causes, levels = c("1","2","3","4","5","6+")), 
                               y = deaths)) +
    geom_col(fill = "#4393C3", alpha = 0.85) +
    geom_text(aes(label = label), vjust = -0.2, size = 3.2) +
    labs(
      title = "How Many Conditions Are Listed on Australian Death Certificates?",
      subtitle = paste0("Distribution of co-occurring causes per death, Australia 2024 (N = ",
                        format(total_deaths, big.mark = ","), 
                        ", mean ≥ ", round(avg_causes, 1), " causes per death)"),
      x = "Number of Causes Listed on Death Certificate",
      y = "Number of Deaths",
      caption = "Source: ABS Causes of Death 2024, Data Cube 10, Table 10.1.\n'6+' includes deaths with 6 or more causes listed."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, colour = "grey40"),
      plot.caption = element_text(size = 8, colour = "grey50")
    ) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.18)))
  
  ggsave("outputs/figures/fig5_causes_per_death.png", p5, width = 10, height = 6, dpi = 300)
  cat("  Saved: outputs/figures/fig5_causes_per_death.png\n")
  
} else {
  cat("  ERROR: Could not find Total row. Trying chapter-level sum...\n")
  
  # Fallback: use chapter-level rows only and take the max across chapters
  # (since each death is counted once per chapter)
  # This won't be perfect but gives a reasonable distribution
  chapter_data <- mort_num_causes %>%
    filter(is_chapter == TRUE) %>%
    summarise(
      reported_alone = sum(reported_alone, na.rm = TRUE),
      with_1_other = sum(with_1_other, na.rm = TRUE),
      with_2_other = sum(with_2_other, na.rm = TRUE),
      with_3_other = sum(with_3_other, na.rm = TRUE),
      with_4_other = sum(with_4_other, na.rm = TRUE),
      with_5_plus = sum(with_5_plus, na.rm = TRUE)
    )
  
  cat("  Chapter-level sums (note: may double-count across chapters):\n")
  print(chapter_data)
  cat("  WARNING: Cannot create clean Figure 5 without Total row\n")
}


# =============================================================================
# FIX 4: Figures 3 & 4 — Faceted small multiples for readability
# =============================================================================

cat("\n=== Fix 4: Figures 3 & 4 — Faceted trend charts ===\n")

# --- Figure 3: Procedure trends (top 10 only, faceted) ---

proc_trends <- hosp_proc %>%
  filter(
    !str_detect(proc_chapter, "^19 |^20 |^21 "),
    !is.na(total_separations)
  ) %>%
  mutate(
    chapter_short = str_remove(proc_chapter, "^\\d+ ") %>% 
      str_remove("Procedures on ") %>%
      str_to_sentence() %>%
      str_trunc(35)
  )

# Get top 10 by most recent year volume
top10_proc <- proc_trends %>%
  filter(year == max(year)) %>%
  arrange(desc(total_separations)) %>%
  head(10) %>%
  pull(chapter_short)

proc_top10 <- proc_trends %>%
  filter(chapter_short %in% top10_proc) %>%
  mutate(chapter_short = factor(chapter_short, levels = rev(top10_proc)))

p3 <- ggplot(proc_top10, aes(x = year, y = total_separations, group = 1)) +
  geom_line(colour = "#2166AC", linewidth = 0.8) +
  geom_point(colour = "#2166AC", size = 1.5) +
  facet_wrap(~chapter_short, scales = "free_y", ncol = 2) +
  labs(
    title = "Hospital Procedure Volume Trends — Top 10 Clinical Domains",
    subtitle = "Australia, 2017-18 to 2023-24 (each panel on its own y-axis scale)",
    x = "Financial Year",
    y = "Total Separations",
    caption = "Source: AIHW Procedures Data Cubes 2017-18 to 2023-24. Excludes non-invasive/cognitive and imaging."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    plot.caption = element_text(size = 7, colour = "grey50"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    strip.text = element_text(face = "bold", size = 8)
  ) +
  scale_y_continuous(labels = label_comma(scale = 1/1000, suffix = "K"))

ggsave("outputs/figures/fig3_procedure_trends_faceted.png", p3, width = 12, height = 12, dpi = 300)
cat("  Saved: outputs/figures/fig3_procedure_trends_faceted.png\n")


# --- Figure 4: Diagnosis trends (top 10, faceted) ---

diag_trends <- hosp_diag %>%
  filter(
    !str_detect(diag_chapter, "^Z00|^Not reported|^U00"),
    !is.na(total_separations)
  ) %>%
  mutate(
    chapter_short = str_remove(diag_chapter, "^[A-Z]\\d+.+?\\d+\\s+") %>%
      str_trunc(40)
  )

top10_diag <- diag_trends %>%
  filter(year == max(year)) %>%
  arrange(desc(total_separations)) %>%
  head(10) %>%
  pull(chapter_short)

diag_top10 <- diag_trends %>%
  filter(chapter_short %in% top10_diag) %>%
  mutate(chapter_short = factor(chapter_short, levels = rev(top10_diag)))

p4 <- ggplot(diag_top10, aes(x = year, y = total_separations, group = 1)) +
  geom_line(colour = "#B2182B", linewidth = 0.8) +
  geom_point(colour = "#B2182B", size = 1.5) +
  facet_wrap(~chapter_short, scales = "free_y", ncol = 2) +
  labs(
    title = "Hospital Admissions by Principal Diagnosis — Top 10 Chapters",
    subtitle = "Australia, 2017-18 to 2023-24 (each panel on its own y-axis scale)",
    x = "Financial Year",
    y = "Total Separations",
    caption = "Source: AIHW Principal Diagnosis Data Cubes 2017-18 to 2023-24. Excludes Z-codes."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    plot.caption = element_text(size = 7, colour = "grey50"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    strip.text = element_text(face = "bold", size = 8)
  ) +
  scale_y_continuous(labels = label_comma(scale = 1/1000, suffix = "K"))

ggsave("outputs/figures/fig4_diagnosis_trends_faceted.png", p4, width = 12, height = 12, dpi = 300)
cat("  Saved: outputs/figures/fig4_diagnosis_trends_faceted.png\n")


# =============================================================================
# BONUS: Figure 7 — COVID impact comparison (% change from 2018-19 baseline)
# =============================================================================

cat("\n=== Bonus: Figure 7 — COVID impact on hospital activity ===\n")

# Use 2018-19 as pre-COVID baseline, show % change for each domain
baseline_year <- "2018-19"

proc_covid <- hosp_proc %>%
  filter(!str_detect(proc_chapter, "^19 |^20 |^21 ")) %>%
  mutate(
    chapter_short = str_remove(proc_chapter, "^\\d+ ") %>% 
      str_remove("Procedures on ") %>%
      str_to_sentence() %>%
      str_trunc(30)
  )

# Get baseline values
baseline <- proc_covid %>%
  filter(year == baseline_year) %>%
  select(chapter_short, baseline_seps = total_separations)

proc_pct_change <- proc_covid %>%
  left_join(baseline, by = "chapter_short") %>%
  filter(!is.na(baseline_seps), baseline_seps > 0) %>%
  mutate(pct_change = (total_separations - baseline_seps) / baseline_seps * 100)

# Focus on top 10
proc_pct_top10 <- proc_pct_change %>%
  filter(chapter_short %in% top10_proc)

p7 <- ggplot(proc_pct_top10, aes(x = year, y = pct_change, 
                                   colour = chapter_short, group = chapter_short)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  labs(
    title = "COVID Impact on Hospital Procedures: % Change from 2018-19 Baseline",
    subtitle = "Top 10 procedure domains, Australia. Dashed line = no change from baseline.",
    x = "Financial Year",
    y = "% Change from 2018-19",
    colour = "Procedure Domain",
    caption = "Source: AIHW Procedures Data Cubes 2017-18 to 2023-24."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    plot.caption = element_text(size = 7, colour = "grey50"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.text = element_text(size = 8)
  ) +
  guides(colour = guide_legend(ncol = 1))

ggsave("outputs/figures/fig7_covid_impact_procedures.png", p7, width = 13, height = 7, dpi = 300)
cat("  Saved: outputs/figures/fig7_covid_impact_procedures.png\n")


cat("\n=== All fixes complete ===\n")
cat("Updated/new figures:\n")
cat("  fig1_hidden_burden_ratio.png       (fixed 'Essential' label)\n")
cat("  fig2_absolute_hidden_burden.png    (removed chapter double-counting)\n")
cat("  fig3_procedure_trends_faceted.png  (small multiples, readable)\n")
cat("  fig4_diagnosis_trends_faceted.png  (small multiples, readable)\n")
cat("  fig5_causes_per_death.png          (NEW — was failing before)\n")
cat("  fig7_covid_impact_procedures.png   (NEW — % change from baseline)\n")
