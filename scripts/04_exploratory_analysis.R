# =============================================================================
# 04_exploratory_analysis.R
# Purpose: Build concordance, explore temporal trends, hidden burden patterns
# Inputs:  outputs/ CSV files from scripts 02 and 03
# Outputs: outputs/figures/ (visualisations)
#          outputs/concordance_proc_diag_mort.csv
# =============================================================================

library(tidyverse)
library(scales)

dir.create("outputs/figures", showWarnings = FALSE)

# =============================================================================
# STEP 0: Load all cleaned data
# =============================================================================

cat("=== Loading cleaned datasets ===\n\n")

# ABS mortality
mort_state      <- read_csv("outputs/deaths_underlying_by_state.csv", show_col_types = FALSE)
mort_multi      <- read_csv("outputs/deaths_underlying_vs_multiple.csv", show_col_types = FALSE)
mort_multi_asr  <- read_csv("outputs/deaths_multiple_cause_rates.csv", show_col_types = FALSE)
mort_num_causes <- read_csv("outputs/deaths_by_num_causes.csv", show_col_types = FALSE)

# AIHW hospital
hosp_proc <- read_csv("outputs/hospital_procedures_by_year.csv", show_col_types = FALSE)
hosp_diag <- read_csv("outputs/hospital_diagnoses_by_year.csv", show_col_types = FALSE)

cat("  Mortality (state-level):", format(nrow(mort_state), big.mark = ","), "rows\n")
cat("  Mortality (multiple causes):", format(nrow(mort_multi), big.mark = ","), "rows\n")
cat("  Hospital procedures:", format(nrow(hosp_proc), big.mark = ","), "rows\n")
cat("  Hospital diagnoses:", format(nrow(hosp_diag), big.mark = ","), "rows\n")


# =============================================================================
# STEP 1: Build concordance table
# =============================================================================

cat("\n=== Building concordance table ===\n\n")

# This maps clinical domains across the three classification systems:
# - ICD-10 chapters (mortality + hospital diagnoses)
# - ACHI chapters (hospital procedures)

concordance <- tribble(
  ~clinical_domain,              ~icd10_chapter_pattern,               ~achi_chapter_pattern,
  "Nervous system",              "G00.G99",                            "^01 ",
  "Endocrine/metabolic",         "E00.E89",                            "^02 ",
  "Eye",                         "H00.H59",                            "^03 ",
  "Ear",                         "H60.H95",                            "^04 ",
  "Respiratory",                 "J00.J99",                            "^07 ",
  "Cardiovascular",              "I00.I99",                            "^08 ",
  "Blood/immune",                "D50.D89",                            "^09 ",
  "Digestive",                   "K00.K93",                            "^10 ",
  "Genitourinary",               "N00.N99",                            "^11 ",
  "Musculoskeletal",             "M00.M99",                            "^15 ",
  "Skin",                        "L00.L99",                            "^16 ",
  "Neoplasms",                   "C00.D48",                            "^17|^18",
  "Obstetric",                   "O00.O99",                            "^14 ",
  "Mental/behavioural",          "F00.F99",                            NA_character_,
  "Infectious diseases",         "A00.B99",                            NA_character_,
  "Injury/poisoning",            "S00.T98",                            NA_character_
)

cat("  Concordance table: ", nrow(concordance), " clinical domains\n")
cat("  Domains with procedure linkage: ", sum(!is.na(concordance$achi_chapter_pattern)), "\n")
cat("  Domains without procedure linkage: ", sum(is.na(concordance$achi_chapter_pattern)), "\n\n")

write_csv(concordance, "outputs/concordance_proc_diag_mort.csv")
cat("  Saved: outputs/concordance_proc_diag_mort.csv\n")


# =============================================================================
# STEP 2: FIGURE 1 — Hidden Burden Ratio (top 25 conditions)
# =============================================================================

cat("\n=== Figure 1: Hidden Burden Ratio ===\n")

# Filter to clinically meaningful conditions (exclude "symptoms/signs" R-codes 
# and overly broad chapter-level categories), require minimum 50 underlying deaths
fig1_data <- mort_multi %>%
  filter(
    !is.na(ratio_persons),
    is.finite(ratio_persons),
    underlying_persons >= 50,         # enough underlying deaths to be meaningful
    multiple_persons >= 100,          # enough total mentions
    !str_detect(icd10_code, "^R"),    # exclude symptoms/signs chapter (not specific diagnoses)
    !str_detect(icd10_code, "^Y"),    # exclude external cause supplement codes
    !str_detect(icd10_code, "^U"),    # exclude special purpose codes
    !is.na(icd10_code)
  ) %>%
  arrange(desc(ratio_persons)) %>%
  head(25)

p1 <- ggplot(fig1_data, aes(x = reorder(cause_name, ratio_persons), y = ratio_persons)) +
  geom_col(fill = "#2166AC", alpha = 0.85) +
  geom_text(aes(label = round(ratio_persons, 1)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Hidden Burden: Conditions Most Under-counted as Underlying Cause of Death",
    subtitle = "Ratio of total death certificate mentions to underlying cause listings, Australia 2023\n(Higher ratio = condition appears far more often as contributing cause than primary cause)",
    x = NULL,
    y = "Multiple-to-Underlying Cause Ratio",
    caption = "Source: ABS Causes of Death 2023, Data Cube 10. Conditions with ≥50 underlying cause deaths shown."
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
# STEP 3: FIGURE 2 — Absolute hidden burden (extra deaths revealed)
# =============================================================================

cat("\n=== Figure 2: Absolute Hidden Burden ===\n")

# This shows the CONDITIONS where the gap between multiple and underlying counts
# is largest in absolute terms — i.e., the most "hidden" deaths
fig2_data <- mort_multi %>%
  filter(
    !is.na(underlying_persons),
    !is.na(multiple_persons),
    underlying_persons >= 20,
    !is.na(icd10_code)
  ) %>%
  mutate(
    extra_deaths = multiple_persons - underlying_persons,
    pct_hidden = (extra_deaths / multiple_persons) * 100
  ) %>%
  filter(extra_deaths > 0) %>%
  arrange(desc(extra_deaths)) %>%
  head(20)

p2 <- ggplot(fig2_data, aes(x = reorder(cause_name, extra_deaths))) +
  geom_col(aes(y = multiple_persons), fill = "#B2182B", alpha = 0.7, width = 0.7) +
  geom_col(aes(y = underlying_persons), fill = "#2166AC", alpha = 0.9, width = 0.7) +
  geom_text(aes(y = multiple_persons, 
                label = paste0("+", format(extra_deaths, big.mark = ","))),
            hjust = -0.05, size = 2.8, colour = "#B2182B") +
  coord_flip() +
  labs(
    title = "The Hidden Death Toll: Conditions with Largest Gap Between\nUnderlying and Multiple Cause Counts",
    subtitle = "Blue = listed as underlying cause | Red = total mentions on death certificates, Australia 2023",
    x = NULL,
    y = "Number of Death Certificate Mentions",
    caption = "Source: ABS Causes of Death 2023, Data Cube 10."
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
# STEP 4: FIGURE 3 — Hospital procedure trends over time
# =============================================================================

cat("\n=== Figure 3: Hospital Procedure Trends ===\n")

# Focus on the clinically interesting chapters (exclude ch19 non-invasive which 
# dominates and obscures other trends, and ch20 imaging)
proc_trends <- hosp_proc %>%
  filter(
    !str_detect(proc_chapter, "^19 |^20 |^21 "),
    !is.na(total_separations)
  ) %>%
  # Shorten chapter names for plot labels
  mutate(
    chapter_short = str_remove(proc_chapter, "^\\d+ ") %>% 
      str_remove("Procedures on ") %>%
      str_to_sentence() %>%
      str_trunc(40)
  )

p3 <- ggplot(proc_trends, aes(x = year, y = total_separations, 
                                colour = chapter_short, group = chapter_short)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(
    title = "Hospital Procedure Volume Trends by Clinical Domain",
    subtitle = "Australia, 2017-18 to 2023-24 (excludes non-invasive/cognitive interventions and imaging)",
    x = "Financial Year",
    y = "Total Separations",
    colour = "Procedure Chapter",
    caption = "Source: AIHW Procedures Data Cubes 2017-18 to 2023-24."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    plot.caption = element_text(size = 7, colour = "grey50"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.text = element_text(size = 7)
  ) +
  scale_y_continuous(labels = comma) +
  guides(colour = guide_legend(ncol = 1))

ggsave("outputs/figures/fig3_procedure_trends.png", p3, width = 14, height = 8, dpi = 300)
cat("  Saved: outputs/figures/fig3_procedure_trends.png\n")


# =============================================================================
# STEP 5: FIGURE 4 — Hospital diagnosis trends over time
# =============================================================================

cat("\n=== Figure 4: Hospital Diagnosis Trends ===\n")

diag_trends <- hosp_diag %>%
  filter(
    !str_detect(diag_chapter, "^Z00|^Not reported|^U00"),
    !is.na(total_separations)
  ) %>%
  mutate(
    chapter_short = str_remove(diag_chapter, "^[A-Z]\\d+.+?\\d+\\s+") %>% 
      str_trunc(45)
  )

p4 <- ggplot(diag_trends, aes(x = year, y = total_separations, 
                                colour = chapter_short, group = chapter_short)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(
    title = "Hospital Admissions by Principal Diagnosis Chapter",
    subtitle = "Australia, 2017-18 to 2023-24 (excludes Z-codes: health service contacts)",
    x = "Financial Year",
    y = "Total Separations",
    colour = "Diagnosis Chapter",
    caption = "Source: AIHW Principal Diagnosis Data Cubes 2017-18 to 2023-24."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    plot.caption = element_text(size = 7, colour = "grey50"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.text = element_text(size = 7)
  ) +
  scale_y_continuous(labels = comma) +
  guides(colour = guide_legend(ncol = 1))

ggsave("outputs/figures/fig4_diagnosis_trends.png", p4, width = 14, height = 8, dpi = 300)
cat("  Saved: outputs/figures/fig4_diagnosis_trends.png\n")


# =============================================================================
# STEP 6: FIGURE 5 — Deaths by number of causes on certificate
# =============================================================================

cat("\n=== Figure 5: Multimorbidity at Death ===\n")

# Get the Australia-level data by number of causes
fig5_data <- mort_num_causes %>%
  filter(
    !is.na(cause_icd10),
    str_detect(cause_icd10, "^\\d+ cause|^Total")
  )

# Preview what we have
cat("  Available categories:\n")
cat("  ", paste(unique(fig5_data$cause_icd10), collapse = "\n   "), "\n")

# Try to extract the number of causes and the count
fig5_clean <- fig5_data %>%
  mutate(
    num_causes = case_when(
      str_detect(cause_icd10, "^1 cause") ~ 1L,
      str_detect(cause_icd10, "^2 cause") ~ 2L,
      str_detect(cause_icd10, "^3 cause") ~ 3L,
      str_detect(cause_icd10, "^4 cause") ~ 4L,
      str_detect(cause_icd10, "^5 cause") ~ 5L,
      str_detect(cause_icd10, "^6 cause") ~ 6L,
      str_detect(cause_icd10, "^7 cause") ~ 7L,
      str_detect(cause_icd10, "^8 cause") ~ 8L,
      str_detect(cause_icd10, "^9 cause") ~ 9L,
      str_detect(cause_icd10, "^10") ~ 10L,
      TRUE ~ NA_integer_
    )
  ) %>%
  filter(!is.na(num_causes))

cat("  Found", nrow(fig5_clean), "rows with parseable cause counts\n")

if (nrow(fig5_clean) > 0 && "n_persons" %in% colnames(fig5_clean)) {
  
  # Aggregate if there are sex-specific rows
  fig5_agg <- fig5_clean %>%
    group_by(num_causes) %>%
    summarise(deaths = sum(n_persons, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      pct = deaths / sum(deaths) * 100,
      label = paste0(format(deaths, big.mark = ","), "\n(", round(pct, 1), "%)")
    )
  
  p5 <- ggplot(fig5_agg, aes(x = factor(num_causes), y = deaths)) +
    geom_col(fill = "#4393C3", alpha = 0.85) +
    geom_text(aes(label = label), vjust = -0.2, size = 3) +
    labs(
      title = "How Many Conditions Are Listed on Australian Death Certificates?",
      subtitle = paste0("Distribution of number of causes per death, Australia 2023 (N = ", 
                        format(sum(fig5_agg$deaths), big.mark = ","), ")"),
      x = "Number of Causes Listed on Death Certificate",
      y = "Number of Deaths",
      caption = "Source: ABS Causes of Death 2023, Data Cube 10, Table 10.1."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 9, colour = "grey40"),
      plot.caption = element_text(size = 7, colour = "grey50")
    ) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15)))
  
  ggsave("outputs/figures/fig5_causes_per_death.png", p5, width = 10, height = 6, dpi = 300)
  cat("  Saved: outputs/figures/fig5_causes_per_death.png\n")
  
} else {
  cat("  WARNING: Could not create Figure 5 — check mort_num_causes structure\n")
  cat("  Column names:", paste(colnames(fig5_clean), collapse = ", "), "\n")
}


# =============================================================================
# STEP 7: FIGURE 6 — Linked view: procedures vs mortality by clinical domain
# =============================================================================

cat("\n=== Figure 6: Procedures vs Mortality by Domain ===\n")

# For each concordance domain, get:
# - Total hospital separations (most recent year, from diagnosis data)
# - Total deaths as underlying cause (from mortality)
# - Total deaths as multiple cause (from mortality)

# Extract ICD-10 chapter code range from the diagnosis chapter names
hosp_diag_latest <- hosp_diag %>%
  filter(year == max(year))

# Match each concordance row to the hospital diagnosis data
domain_summary <- concordance %>%
  rowwise() %>%
  mutate(
    # Match diagnosis chapter
    diag_seps = {
      pattern <- str_replace(icd10_chapter_pattern, "\\.", ".")
      # Convert our pattern like "I00.I99" to match "I00–I99" in the chapter name
      first_code <- str_extract(pattern, "^[A-Z]\\d+")
      matching <- hosp_diag_latest %>% 
        filter(str_detect(diag_chapter, fixed(first_code)))
      if (nrow(matching) > 0) sum(matching$total_separations, na.rm = TRUE) else NA_real_
    },
    # Match procedure chapter
    proc_seps = {
      if (!is.na(achi_chapter_pattern)) {
        matching <- hosp_proc %>%
          filter(year == max(year)) %>%
          filter(str_detect(proc_chapter, achi_chapter_pattern))
        if (nrow(matching) > 0) sum(matching$total_separations, na.rm = TRUE) else NA_real_
      } else NA_real_
    }
  ) %>%
  ungroup()

# Match to mortality — get underlying cause deaths by ICD-10 chapter
# Mortality data uses chapter-level rows marked as is_chapter
mort_chapters <- mort_state %>%
  filter(state_abbr == "AUS", is_chapter == TRUE) %>%
  select(cause_name, icd10_code, n_persons, asr_persons)

# Match concordance to mortality chapters
domain_summary <- domain_summary %>%
  rowwise() %>%
  mutate(
    underlying_deaths = {
      first_code <- str_extract(icd10_chapter_pattern, "^[A-Z]\\d+")
      matching <- mort_chapters %>% filter(str_detect(icd10_code, fixed(first_code)))
      if (nrow(matching) > 0) sum(matching$n_persons, na.rm = TRUE) else NA_real_
    }
  ) %>%
  ungroup()

cat("\n  Domain summary:\n")
domain_summary %>%
  select(clinical_domain, diag_seps, proc_seps, underlying_deaths) %>%
  mutate(across(where(is.numeric), ~format(., big.mark = ","))) %>%
  print(n = 20, width = 120)

write_csv(domain_summary, "outputs/domain_summary.csv")
cat("\n  Saved: outputs/domain_summary.csv\n")

# Create the scatter plot — hospital activity vs mortality
fig6_data <- domain_summary %>%
  filter(!is.na(diag_seps), !is.na(underlying_deaths), underlying_deaths > 100)

if (nrow(fig6_data) > 3) {
  p6 <- ggplot(fig6_data, aes(x = diag_seps, y = underlying_deaths)) +
    geom_point(aes(size = proc_seps), colour = "#2166AC", alpha = 0.7) +
    geom_text(aes(label = clinical_domain), vjust = -1, size = 3, check_overlap = TRUE) +
    labs(
      title = "Hospital Burden vs Mortality Burden by Clinical Domain",
      subtitle = "Australia, most recent available year. Point size = procedure volume.",
      x = "Hospital Separations (Principal Diagnosis, 2023-24)",
      y = "Deaths (Underlying Cause, 2023)",
      size = "Procedure\nSeparations",
      caption = "Sources: AIHW Principal Diagnosis & Procedures Data Cubes; ABS Causes of Death 2023."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 9, colour = "grey40"),
      plot.caption = element_text(size = 7, colour = "grey50")
    ) +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    scale_size_continuous(labels = comma)
  
  ggsave("outputs/figures/fig6_hospital_vs_mortality.png", p6, width = 11, height = 8, dpi = 300)
  cat("  Saved: outputs/figures/fig6_hospital_vs_mortality.png\n")
} else {
  cat("  WARNING: Not enough matched domains for Figure 6\n")
}


# =============================================================================
# STEP 8: Summary statistics for the paper
# =============================================================================

cat("\n\n=== KEY FINDINGS SUMMARY ===\n\n")

cat("1. MULTIPLE CAUSE ANALYSIS:\n")
cat("   Total deaths, Australia 2023:", format(187268, big.mark = ","), "\n")

# Top hidden burden conditions (clinically specific, excluding R/Y/U codes)
top_hidden <- mort_multi %>%
  filter(
    underlying_persons >= 100,
    !str_detect(icd10_code, "^R|^Y|^U"),
    !is.na(icd10_code),
    !is.na(ratio_persons)
  ) %>%
  arrange(desc(ratio_persons)) %>%
  head(10)

cat("   Top conditions by hidden burden ratio (≥100 underlying deaths):\n")
for (i in 1:nrow(top_hidden)) {
  cat(sprintf("     %s (%s): ratio %.1f — %s underlying, %s total mentions\n",
              top_hidden$cause_name[i], top_hidden$icd10_code[i],
              top_hidden$ratio_persons[i],
              format(top_hidden$underlying_persons[i], big.mark = ","),
              format(top_hidden$multiple_persons[i], big.mark = ",")))
}

cat("\n2. HOSPITAL TRENDS:\n")
proc_total_by_year <- hosp_proc %>%
  group_by(year) %>%
  summarise(total = sum(total_separations, na.rm = TRUE))
cat("   Total procedures by year:\n")
for (i in 1:nrow(proc_total_by_year)) {
  cat(sprintf("     %s: %s\n", proc_total_by_year$year[i], 
              format(proc_total_by_year$total[i], big.mark = ",")))
}

cat("\n3. COVID IMPACT:\n")
cat("   Look for dips in 2019-20 and 2020-21 procedure volumes\n")

cat("\n=== Analysis complete. Check outputs/figures/ for all plots. ===\n")
