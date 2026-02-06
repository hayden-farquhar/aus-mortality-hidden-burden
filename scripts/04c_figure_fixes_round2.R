# =============================================================================
# 04c_figure_fixes_round2.R
# Purpose: Fix Fig 5 bins, Fig 6 labels, replace Figs 3/4 with focused view
# Run AFTER 04b_figure_fixes.R
# =============================================================================

library(tidyverse)
library(scales)

# Install ggrepel if needed
if (!requireNamespace("ggrepel", quietly = TRUE)) install.packages("ggrepel")
library(ggrepel)

dir.create("outputs/figures", showWarnings = FALSE)

# Reload data
mort_multi      <- read_csv("outputs/deaths_underlying_vs_multiple.csv", show_col_types = FALSE)
mort_num_causes <- read_csv("outputs/deaths_by_num_causes.csv", show_col_types = FALSE)
hosp_proc       <- read_csv("outputs/hospital_procedures_by_year.csv", show_col_types = FALSE)
hosp_diag       <- read_csv("outputs/hospital_diagnoses_by_year.csv", show_col_types = FALSE)


# =============================================================================
# FIX 1: Figure 5 — Correct to 5 bins (1, 2, 3, 4, 5+)
# =============================================================================

cat("=== Fix 1: Figure 5 — correct bin structure ===\n")

# Inspect the Total row to understand what we actually have
total_row <- mort_num_causes %>%
  filter(is_total == TRUE | str_detect(cause_icd10, regex("total", ignore_case = TRUE))) %>%
  head(1)

cat("  Total row columns and values:\n")
cat("    reported_alone:", total_row$reported_alone, "\n")
cat("    with_1_other:  ", total_row$with_1_other, "\n")
cat("    with_2_other:  ", total_row$with_2_other, "\n")
cat("    with_3_other:  ", total_row$with_3_other, "\n")
cat("    with_4_other:  ", total_row$with_4_other, "\n")
cat("    with_5_plus:   ", total_row$with_5_plus, "\n")

# The column with_4_other actually contains "4 or more other causes" = 5+ total
# The with_5_plus column (3.5) is likely an average/ratio, not a count
# Evidence: 36870 + 37313 + 34430 + 26824 + 51831 = 187,268 ✓ (matches total deaths)
# So 5 bins is correct

fig5_data <- tibble(
  num_causes = c("1", "2", "3", "4", "5+"),
  deaths = c(
    total_row$reported_alone,
    total_row$with_1_other,
    total_row$with_2_other,
    total_row$with_3_other,
    total_row$with_4_other     # this is 5+ total causes
  )
) %>%
  filter(!is.na(deaths)) %>%
  mutate(
    pct = deaths / sum(deaths, na.rm = TRUE) * 100,
    label = paste0(format(deaths, big.mark = ","), "\n(", round(pct, 1), "%)"),
    # For colour: highlight that most deaths have multiple causes
    multi = ifelse(num_causes == "1", "Single cause", "Multiple causes")
  )

total_deaths <- sum(fig5_data$deaths, na.rm = TRUE)
multi_pct <- sum(fig5_data$deaths[fig5_data$multi == "Multiple causes"]) / total_deaths * 100

cat("\n  Corrected distribution:\n")
for (i in 1:nrow(fig5_data)) {
  cat(sprintf("    %s cause(s): %s deaths (%.1f%%)\n",
              fig5_data$num_causes[i],
              format(fig5_data$deaths[i], big.mark = ","),
              fig5_data$pct[i]))
}
cat(sprintf("  Total: %s deaths | %.1f%% have multiple causes\n",
            format(total_deaths, big.mark = ","), multi_pct))

p5 <- ggplot(fig5_data, aes(x = num_causes, y = deaths, fill = multi)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = label), vjust = -0.2, size = 3.5) +
  scale_fill_manual(values = c("Single cause" = "#92C5DE", "Multiple causes" = "#2166AC")) +
  labs(
    title = "How Many Conditions Are Listed on Australian Death Certificates?",
    subtitle = paste0("Distribution of co-occurring causes per death, Australia 2024 (N = ",
                      format(total_deaths, big.mark = ","),
                      ") — ", round(multi_pct, 1), "% have multiple causes"),
    x = "Number of Causes Listed on Death Certificate",
    y = "Number of Deaths",
    caption = "Source: ABS Causes of Death 2023, Data Cube 10, Table 10.1.\n'5+' includes deaths listing 5 or more co-occurring causes."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, colour = "grey40"),
    plot.caption = element_text(size = 8, colour = "grey50"),
    legend.position = "none"
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.18)))

ggsave("outputs/figures/fig5_causes_per_death.png", p5, width = 10, height = 6, dpi = 300)
cat("  Saved: outputs/figures/fig5_causes_per_death.png\n")


# =============================================================================
# FIX 2: Figure 6 — Use ggrepel for non-overlapping labels
# =============================================================================

cat("\n=== Fix 2: Figure 6 — ggrepel labels ===\n")

domain_summary <- read_csv("outputs/domain_summary.csv", show_col_types = FALSE)

fig6_data <- domain_summary %>%
  filter(!is.na(diag_seps), !is.na(underlying_deaths), underlying_deaths > 100)

# Add a mortality-to-hospital ratio for interpretation
fig6_data <- fig6_data %>%
  mutate(
    mort_hosp_ratio = underlying_deaths / diag_seps * 1000,  # deaths per 1000 admissions
    quadrant = case_when(
      diag_seps > median(diag_seps) & underlying_deaths > median(underlying_deaths) ~ "High burden\n(both)",
      diag_seps > median(diag_seps) & underlying_deaths <= median(underlying_deaths) ~ "High hospital,\nlow mortality",
      diag_seps <= median(diag_seps) & underlying_deaths > median(underlying_deaths) ~ "Low hospital,\nhigh mortality",
      TRUE ~ "Low burden\n(both)"
    )
  )

p6 <- ggplot(fig6_data, aes(x = diag_seps, y = underlying_deaths)) +
  # Add quadrant dividers at medians
  geom_hline(yintercept = median(fig6_data$underlying_deaths), 
             linetype = "dotted", colour = "grey70", linewidth = 0.5) +
  geom_vline(xintercept = median(fig6_data$diag_seps), 
             linetype = "dotted", colour = "grey70", linewidth = 0.5) +
  # Points sized by procedure volume
  geom_point(aes(size = proc_seps), colour = "#2166AC", alpha = 0.6) +
  # Repelled labels
  geom_text_repel(
    aes(label = clinical_domain),
    size = 3.5,
    fontface = "bold",
    colour = "grey20",
    box.padding = 0.6,
    point.padding = 0.4,
    segment.colour = "grey50",
    segment.size = 0.3,
    max.overlaps = 20,
    seed = 42
  ) +
  # Annotate quadrants
  annotate("text", x = max(fig6_data$diag_seps) * 0.95, 
           y = max(fig6_data$underlying_deaths) * 0.95,
           label = "High hospital burden\nHigh mortality", 
           size = 2.8, colour = "grey50", hjust = 1, fontface = "italic") +
  annotate("text", x = max(fig6_data$diag_seps) * 0.95,
           y = min(fig6_data$underlying_deaths) * 1.5,
           label = "High hospital burden\nLow mortality",
           size = 2.8, colour = "grey50", hjust = 1, fontface = "italic") +
  annotate("text", x = min(fig6_data$diag_seps) * 1.3,
           y = max(fig6_data$underlying_deaths) * 0.95,
           label = "Low hospital burden\nHigh mortality",
           size = 2.8, colour = "grey50", hjust = 0, fontface = "italic") +
  labs(
    title = "Hospital Burden vs Mortality Burden by Clinical Domain",
    subtitle = "Australia, most recent available year. Point size = procedure volume. Dotted lines = medians.",
    x = "Hospital Separations (Principal Diagnosis, 2023-24)",
    y = "Deaths (Underlying Cause, 2024)",
    size = "Procedure\nSeparations",
    caption = "Sources: AIHW Principal Diagnosis & Procedures Data Cubes; ABS Causes of Death 2023.\nDomains above the diagonal line have higher 'lethality' per hospital admission."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    plot.caption = element_text(size = 7, colour = "grey50")
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  scale_size_continuous(labels = comma, range = c(2, 15))

ggsave("outputs/figures/fig6_hospital_vs_mortality.png", p6, width = 12, height = 9, dpi = 300)
cat("  Saved: outputs/figures/fig6_hospital_vs_mortality.png\n")


# =============================================================================
# FIX 3: Replace Figs 3 & 4 with focused domain comparison
# =============================================================================

cat("\n=== Fix 3: Focused domain trend comparison ===\n")

# Instead of showing ALL domains, focus on the 5 domains with the most
# interesting stories for the paper:
# - Cardiovascular (high mortality + procedures, declining?)
# - Respiratory (COVID impact + hidden burden)
# - Mental/behavioural (huge hidden burden, no procedure linkage)
# - Digestive (high hospital burden, moderate mortality)
# - Neoplasms (high mortality, moderate hospital burden)

focus_domains <- tribble(
  ~domain_label,      ~diag_pattern,    ~proc_pattern,
  "Cardiovascular",   "I00",            "^08 ",
  "Respiratory",      "J00",            "^07 ",
  "Digestive",        "K00",            "^10 ",
  "Neoplasms",        "C00",            "^17|^18",
  "Musculoskeletal",  "M00",            "^15 ",
  "Mental health",    "F00",            NA_character_
)

# Build diagnosis trends for focus domains
diag_focus <- map_dfr(1:nrow(focus_domains), function(i) {
  hosp_diag %>%
    filter(str_detect(diag_chapter, fixed(focus_domains$diag_pattern[i]))) %>%
    mutate(domain = focus_domains$domain_label[i])
})

# Build procedure trends for focus domains (where available)
proc_focus <- map_dfr(1:nrow(focus_domains), function(i) {
  if (!is.na(focus_domains$proc_pattern[i])) {
    hosp_proc %>%
      filter(str_detect(proc_chapter, focus_domains$proc_pattern[i])) %>%
      group_by(year) %>%
      summarise(total_separations = sum(total_separations, na.rm = TRUE), .groups = "drop") %>%
      mutate(domain = focus_domains$domain_label[i])
  }
})

# Combine into long format
combined_focus <- bind_rows(
  diag_focus %>% select(year, domain, total_separations) %>% mutate(type = "Hospital admissions"),
  proc_focus %>% select(year, domain, total_separations) %>% mutate(type = "Procedures performed")
)

# Figure 3 replacement: faceted comparison of hospital activity
p3_new <- ggplot(combined_focus, aes(x = year, y = total_separations, 
                                      colour = type, group = type)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~domain, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = c("Hospital admissions" = "#B2182B", 
                                  "Procedures performed" = "#2166AC")) +
  labs(
    title = "Hospital Activity Trends: Admissions vs Procedures for Key Clinical Domains",
    subtitle = "Australia, 2017-18 to 2023-24 (each panel on its own y-axis scale)",
    x = "Financial Year",
    y = "Total Separations",
    colour = NULL,
    caption = "Source: AIHW Principal Diagnosis & Procedures Data Cubes.\nMental health has no directly mapped procedure chapter."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    plot.caption = element_text(size = 7, colour = "grey50"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    strip.text = element_text(face = "bold", size = 9),
    legend.position = "top"
  ) +
  scale_y_continuous(labels = label_comma(scale = 1/1000, suffix = "K"))

ggsave("outputs/figures/fig3_domain_trends_focused.png", p3_new, width = 12, height = 10, dpi = 300)
cat("  Saved: outputs/figures/fig3_domain_trends_focused.png\n")


# =============================================================================
# Figure 4 replacement: % change from baseline (all focus domains together)
# =============================================================================

cat("\n=== Replacement Fig 4: Indexed trends (2017-18 = 100) ===\n")

# Index to first year = 100 for comparability across domains
baseline_year <- "2017-18"

diag_indexed <- diag_focus %>%
  group_by(domain) %>%
  mutate(
    baseline = total_separations[year == baseline_year],
    index = total_separations / baseline * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(index))

p4_new <- ggplot(diag_indexed, aes(x = year, y = index, 
                                     colour = domain, group = domain)) +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "grey60") +
  geom_line(linewidth = 1) +
  geom_point(size = 2.2) +
  # Add labels at the end point
  geom_text_repel(
    data = diag_indexed %>% filter(year == max(year)),
    aes(label = paste0(domain, " (", round(index), ")")),
    hjust = -0.1, size = 3.2, direction = "y", nudge_x = 0.3,
    segment.size = 0.3, segment.colour = "grey50"
  ) +
  scale_colour_manual(values = c(
    "Cardiovascular" = "#E41A1C",
    "Respiratory" = "#377EB8",
    "Digestive" = "#4DAF4A",
    "Neoplasms" = "#984EA3",
    "Musculoskeletal" = "#FF7F00",
    "Mental health" = "#A65628"
  )) +
  labs(
    title = "Hospital Admission Trends by Clinical Domain (Indexed to 2017-18 = 100)",
    subtitle = "Australia. Values > 100 = growth, < 100 = decline relative to 2017-18 baseline.",
    x = "Financial Year",
    y = "Index (2017-18 = 100)",
    caption = "Source: AIHW Principal Diagnosis Data Cubes 2017-18 to 2023-24. Excludes Z-codes."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    plot.caption = element_text(size = 7, colour = "grey50"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  # labels at end of lines instead
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.25))) +
  coord_cartesian(clip = "off")

ggsave("outputs/figures/fig4_diagnosis_indexed_trends.png", p4_new, width = 12, height = 7, dpi = 300)
cat("  Saved: outputs/figures/fig4_diagnosis_indexed_trends.png\n")


# =============================================================================
# Summary: Print mortality-to-hospital ratio table
# =============================================================================

cat("\n=== Mortality-to-Hospital Ratio by Domain ===\n")
cat("  (Deaths per 1,000 hospital admissions — crude measure of 'lethality')\n\n")

ratio_table <- domain_summary %>%
  filter(!is.na(diag_seps), !is.na(underlying_deaths), underlying_deaths > 100) %>%
  mutate(
    deaths_per_1000_admissions = round(underlying_deaths / diag_seps * 1000, 1)
  ) %>%
  arrange(desc(deaths_per_1000_admissions)) %>%
  select(clinical_domain, diag_seps, underlying_deaths, deaths_per_1000_admissions)

for (i in 1:nrow(ratio_table)) {
  cat(sprintf("  %-25s %10s admissions | %7s deaths | %5.1f deaths/1000 admissions\n",
              ratio_table$clinical_domain[i],
              format(ratio_table$diag_seps[i], big.mark = ","),
              format(ratio_table$underlying_deaths[i], big.mark = ","),
              ratio_table$deaths_per_1000_admissions[i]))
}

cat("\n=== All round 2 fixes complete ===\n")
cat("Final figure set for paper:\n")
cat("  fig1_hidden_burden_ratio.png         — Key finding: ratio chart\n")
cat("  fig2_absolute_hidden_burden.png      — Key finding: absolute gap\n")
cat("  fig3_domain_trends_focused.png       — Admissions vs procedures, 6 domains\n")
cat("  fig4_diagnosis_indexed_trends.png    — Indexed growth comparison\n")
cat("  fig5_causes_per_death.png            — Multimorbidity distribution\n")
cat("  fig6_hospital_vs_mortality.png       — Hospital vs mortality scatter\n")
cat("  fig7_covid_impact_procedures.png     — COVID impact % change\n")
