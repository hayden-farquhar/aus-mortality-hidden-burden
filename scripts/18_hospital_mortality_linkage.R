# ============================================================================
# Script 18: Hospital Procedure Activity vs Mortality Hidden Burden
#
# EXPLORATORY ANALYSIS (E-HM1)
#
# Links hospital procedure/diagnosis volumes to mortality hidden burden
# (MUR) at the clinical domain level. This addresses the project's
# original research question: is there a relationship between hospital
# activity and the hidden burden of mortality?
#
# For each clinical domain (e.g., Cardiovascular, Digestive, Respiratory),
# we have:
#   - Hospital procedure separations (from AIHW hospital data)
#   - Hospital diagnosis separations (from AIHW hospital data)
#   - Underlying cause deaths (from ABS Cube 14)
#   - MUR — the ratio of multiple to underlying cause mentions (Cube 10)
#
# We test whether domains with higher hospital activity (relative to
# deaths) have systematically different MUR values.
#
# INPUT: outputs/domain_summary.csv
#        outputs/deaths_underlying_vs_multiple.csv
#
# OUTPUT: outputs/exploratory/hospital_mortality_linkage.csv
#         outputs/exploratory/hospital_mortality_linkage.txt
#         outputs/figures/fig16_hospital_vs_mur.png
#
# PACKAGES: tidyverse, patchwork
# ============================================================================

library(tidyverse)

if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork", repos = "https://cloud.r-project.org")
}
library(patchwork)

cat("============================================================\n")
cat("Script 18: Hospital Activity vs Mortality Hidden Burden\n")
cat("============================================================\n\n")

dir.create("outputs/exploratory", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# Load data
# ============================================================================

# --- Domain summary (hospital activity) ---
domain_file <- "outputs/domain_summary.csv"
if (!file.exists(domain_file)) {
  stop("ERROR: ", domain_file, " not found. Run earlier scripts first.")
}
domains <- read_csv(domain_file, show_col_types = FALSE)
cat("Domain summary loaded:", nrow(domains), "clinical domains\n")

# --- Underlying vs multiple cause data (for chapter-level MUR) ---
uvm_file <- "outputs/deaths_underlying_vs_multiple.csv"
if (!file.exists(uvm_file)) {
  stop("ERROR: ", uvm_file, " not found. Run earlier scripts first.")
}
uvm <- read_csv(uvm_file, show_col_types = FALSE)
cat("Underlying vs multiple data loaded:", nrow(uvm), "rows\n")

# ============================================================================
# Extract chapter-level MUR
# ============================================================================

# Get chapter-level MUR from the underlying vs multiple file
chapter_mur <- uvm %>%
  filter(is_chapter == TRUE) %>%
  select(icd10_code, cause_name, ratio_persons, ratio_male, ratio_female,
         underlying_persons, multiple_persons)

cat("\nChapter-level MUR values:\n")
for (i in seq_len(nrow(chapter_mur))) {
  row <- chapter_mur[i, ]
  cat(sprintf("  %-50s [%s]  MUR = %s\n",
              row$cause_name, row$icd10_code,
              ifelse(is.na(row$ratio_persons), "NA",
                     sprintf("%.1f", row$ratio_persons))))
}

# ============================================================================
# Match domains to chapter-level MUR
# ============================================================================

# The domain_summary has icd10_chapter_pattern like "I00.I99"
# The chapter MUR has icd10_code like "I00-I99"
# Convert domain patterns to match: replace "." with "-"

domains_matched <- domains %>%
  mutate(
    icd10_match = str_replace(icd10_chapter_pattern, "\\.", "-")
  ) %>%
  left_join(
    chapter_mur %>% select(icd10_code, mur = ratio_persons,
                           mur_male = ratio_male, mur_female = ratio_female,
                           uvm_underlying = underlying_persons,
                           uvm_multiple = multiple_persons),
    by = c("icd10_match" = "icd10_code")
  )

cat("\n--- Matched domains ---\n")
for (i in seq_len(nrow(domains_matched))) {
  row <- domains_matched[i, ]
  cat(sprintf("  %-20s  ICD: %-10s  MUR: %s  Proc: %s  Diag: %s  Deaths: %s\n",
              row$clinical_domain,
              ifelse(is.na(row$icd10_match), "NA", row$icd10_match),
              ifelse(is.na(row$mur), "NA", sprintf("%.1f", row$mur)),
              ifelse(is.na(row$proc_seps), "NA", format(row$proc_seps, big.mark = ",")),
              format(row$diag_seps, big.mark = ","),
              ifelse(is.na(row$underlying_deaths), "NA",
                     format(row$underlying_deaths, big.mark = ","))))
}

# ============================================================================
# Compute ratios for analysis
# ============================================================================

# Filter to domains with valid MUR, procedure data, and deaths
analysis_data <- domains_matched %>%
  filter(
    !is.na(mur),
    !is.na(proc_seps),
    !is.na(underlying_deaths),
    underlying_deaths > 0
  ) %>%
  mutate(
    procs_per_death = proc_seps / underlying_deaths,
    diags_per_death = diag_seps / underlying_deaths,
    log_procs_per_death = log10(procs_per_death),
    log_diags_per_death = log10(diags_per_death),
    log_mur = log10(mur)
  )

cat("\nDomains with complete data for analysis:", nrow(analysis_data), "\n")
cat("Domains excluded (no procedure data or no deaths):\n")
excluded <- domains_matched %>%
  filter(is.na(mur) | is.na(proc_seps) | is.na(underlying_deaths) |
           underlying_deaths == 0)
for (i in seq_len(nrow(excluded))) {
  cat(sprintf("  - %s\n", excluded$clinical_domain[i]))
}

# ============================================================================
# Statistical tests
# ============================================================================

cat("\n============================================================\n")
cat("CORRELATION ANALYSES\n")
cat("============================================================\n\n")

# Spearman correlations (non-parametric, robust to small n)
n_domains <- nrow(analysis_data)

# Test 1: Procedures per death vs MUR
cor_proc_mur <- cor.test(analysis_data$log_procs_per_death, analysis_data$mur,
                         method = "spearman", exact = FALSE)
cat(sprintf("Procedures per death vs MUR (Spearman):\n"))
cat(sprintf("  rho = %.3f, p = %.4f, n = %d\n\n",
            cor_proc_mur$estimate, cor_proc_mur$p.value, n_domains))

# Test 2: Diagnosis separations per death vs MUR
cor_diag_mur <- cor.test(analysis_data$log_diags_per_death, analysis_data$mur,
                         method = "spearman", exact = FALSE)
cat(sprintf("Diagnosis separations per death vs MUR (Spearman):\n"))
cat(sprintf("  rho = %.3f, p = %.4f, n = %d\n\n",
            cor_diag_mur$estimate, cor_diag_mur$p.value, n_domains))

# Test 3: Procedure separations (raw volume) vs MUR
cor_rawproc_mur <- cor.test(analysis_data$proc_seps, analysis_data$mur,
                            method = "spearman", exact = FALSE)
cat(sprintf("Raw procedure volume vs MUR (Spearman):\n"))
cat(sprintf("  rho = %.3f, p = %.4f, n = %d\n\n",
            cor_rawproc_mur$estimate, cor_rawproc_mur$p.value, n_domains))

# Test 4: Underlying deaths vs MUR
cor_deaths_mur <- cor.test(analysis_data$underlying_deaths, analysis_data$mur,
                           method = "spearman", exact = FALSE)
cat(sprintf("Underlying deaths vs MUR (Spearman):\n"))
cat(sprintf("  rho = %.3f, p = %.4f, n = %d\n\n",
            cor_deaths_mur$estimate, cor_deaths_mur$p.value, n_domains))

# Pearson correlation on log-transformed values
cor_proc_mur_pearson <- cor.test(analysis_data$log_procs_per_death,
                                 analysis_data$log_mur, method = "pearson")
cat(sprintf("Log(Procs per death) vs Log(MUR) (Pearson):\n"))
cat(sprintf("  r = %.3f, p = %.4f, n = %d\n\n",
            cor_proc_mur_pearson$estimate, cor_proc_mur_pearson$p.value, n_domains))

# ============================================================================
# Save analysis data
# ============================================================================

output_data <- analysis_data %>%
  select(clinical_domain, icd10_chapter_pattern, achi_chapter_pattern,
         diag_seps, proc_seps, underlying_deaths,
         mur, mur_male, mur_female,
         procs_per_death, diags_per_death) %>%
  arrange(desc(mur))

write_csv(output_data, "outputs/exploratory/hospital_mortality_linkage.csv")
cat("Analysis data saved: outputs/exploratory/hospital_mortality_linkage.csv\n")

# ============================================================================
# Write summary text
# ============================================================================

sink("outputs/exploratory/hospital_mortality_linkage.txt")

cat("============================================================\n")
cat("HOSPITAL ACTIVITY vs MORTALITY HIDDEN BURDEN\n")
cat("Exploratory Analysis E-HM1\n")
cat("============================================================\n\n")
cat(sprintf("Date: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M")))
cat(sprintf("Clinical domains analysed: %d\n", n_domains))
cat(sprintf("Domains excluded: %d (missing procedure data or deaths)\n\n",
            nrow(excluded)))

cat("DOMAIN-LEVEL DATA:\n\n")
cat(sprintf("%-20s %12s %12s %10s %6s %10s\n",
            "Domain", "Proc Seps", "Diag Seps", "Deaths", "MUR", "Proc/Death"))
cat(paste(rep("-", 72), collapse = ""), "\n")

for (i in seq_len(nrow(output_data))) {
  row <- output_data[i, ]
  cat(sprintf("%-20s %12s %12s %10s %6.1f %10.0f\n",
              row$clinical_domain,
              format(row$proc_seps, big.mark = ","),
              format(row$diag_seps, big.mark = ","),
              format(row$underlying_deaths, big.mark = ","),
              row$mur,
              row$procs_per_death))
}

cat("\n")
cat("CORRELATION RESULTS:\n\n")
cat(sprintf("1. Procedures per death vs MUR (Spearman):\n"))
cat(sprintf("   rho = %.3f, p = %.4f\n\n", cor_proc_mur$estimate, cor_proc_mur$p.value))
cat(sprintf("2. Diagnosis seps per death vs MUR (Spearman):\n"))
cat(sprintf("   rho = %.3f, p = %.4f\n\n", cor_diag_mur$estimate, cor_diag_mur$p.value))
cat(sprintf("3. Raw procedure volume vs MUR (Spearman):\n"))
cat(sprintf("   rho = %.3f, p = %.4f\n\n", cor_rawproc_mur$estimate, cor_rawproc_mur$p.value))
cat(sprintf("4. Underlying deaths vs MUR (Spearman):\n"))
cat(sprintf("   rho = %.3f, p = %.4f\n\n", cor_deaths_mur$estimate, cor_deaths_mur$p.value))
cat(sprintf("5. Log(Procs/death) vs Log(MUR) (Pearson):\n"))
cat(sprintf("   r = %.3f, p = %.4f\n\n",
            cor_proc_mur_pearson$estimate, cor_proc_mur_pearson$p.value))

cat("INTERPRETATION:\n\n")

if (cor_proc_mur$p.value < 0.05) {
  direction <- ifelse(cor_proc_mur$estimate > 0, "positive", "negative")
  cat(sprintf("A significant %s correlation was found between hospital\n", direction))
  cat("procedure intensity and MUR, suggesting domains with more procedures\n")
  if (cor_proc_mur$estimate > 0) {
    cat("per death also have higher hidden burden.\n\n")
  } else {
    cat("per death have lower hidden burden.\n\n")
  }
} else {
  cat("No significant correlation was found between hospital procedure\n")
  cat("intensity and MUR. This suggests the hidden burden of mortality\n")
  cat("(as measured by MUR) is not systematically related to the level\n")
  cat("of hospital procedural activity within a clinical domain.\n\n")
}

cat("NOTES:\n\n")
cat("1. MUR data from ABS Cube 10 (2023, multiple causes of death).\n")
cat("2. Hospital separations from AIHW hospital statistics (2022-23).\n")
cat("3. Analysis is ecological (domain-level), not individual-level.\n")
cat("4. Small n limits statistical power. Results are exploratory.\n")
cat("5. Domains without ACHI procedure codes (Mental health, Infectious\n")
cat("   diseases, Injury/poisoning) are excluded from procedure analyses.\n")

sink()

cat("Summary text saved: outputs/exploratory/hospital_mortality_linkage.txt\n")

# ============================================================================
# Figure: Hospital activity vs MUR
# ============================================================================

# Panel A: Procedures per death vs MUR (scatter)
p_a <- ggplot(analysis_data, aes(x = procs_per_death, y = mur)) +
  geom_point(aes(size = underlying_deaths), colour = "#2c7bb6", alpha = 0.7) +
  geom_text(aes(label = clinical_domain),
            hjust = -0.1, vjust = -0.5, size = 2.8, colour = "grey30") +
  geom_smooth(method = "lm", se = TRUE, colour = "grey50",
              linetype = "dashed", linewidth = 0.5) +
  scale_x_log10(labels = scales::comma) +
  scale_size_continuous(range = c(2, 8), labels = scales::comma,
                        name = "Underlying\ndeaths") +
  labs(
    title = "A. Hospital Procedures per Death vs MUR",
    subtitle = sprintf("Spearman rho = %.2f, p = %.3f (n = %d domains)",
                       cor_proc_mur$estimate, cor_proc_mur$p.value, n_domains),
    x = "Procedure separations per underlying death (log scale)",
    y = "MUR (Multiple / Underlying ratio)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    legend.position = "right"
  )

# Panel B: Diagnosis separations per death vs MUR
p_b <- ggplot(analysis_data, aes(x = diags_per_death, y = mur)) +
  geom_point(aes(size = underlying_deaths), colour = "#d7191c", alpha = 0.7) +
  geom_text(aes(label = clinical_domain),
            hjust = -0.1, vjust = -0.5, size = 2.8, colour = "grey30") +
  geom_smooth(method = "lm", se = TRUE, colour = "grey50",
              linetype = "dashed", linewidth = 0.5) +
  scale_x_log10(labels = scales::comma) +
  scale_size_continuous(range = c(2, 8), labels = scales::comma,
                        name = "Underlying\ndeaths") +
  labs(
    title = "B. Hospital Diagnoses per Death vs MUR",
    subtitle = sprintf("Spearman rho = %.2f, p = %.3f (n = %d domains)",
                       cor_diag_mur$estimate, cor_diag_mur$p.value, n_domains),
    x = "Diagnosis separations per underlying death (log scale)",
    y = "MUR (Multiple / Underlying ratio)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    legend.position = "right"
  )

# Panel C: Bubble chart — proc volume vs deaths, coloured by MUR
p_c <- ggplot(analysis_data, aes(x = proc_seps, y = underlying_deaths)) +
  geom_point(aes(colour = mur, size = mur), alpha = 0.7) +
  geom_text(aes(label = clinical_domain),
            hjust = -0.1, vjust = -0.5, size = 2.8, colour = "grey30") +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  scale_colour_viridis_c(option = "plasma", name = "MUR") +
  scale_size_continuous(range = c(2, 8), guide = "none") +
  labs(
    title = "C. Clinical Domain Landscape",
    subtitle = "Hospital procedures vs underlying deaths, coloured by MUR",
    x = "Total procedure separations (log scale)",
    y = "Total underlying deaths (log scale)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    legend.position = "right"
  )

# Combine
p_combined <- (p_a | p_b) / p_c +
  plot_annotation(
    title = "Hospital Activity vs Mortality Hidden Burden by Clinical Domain",
    caption = "Source: AIHW hospital statistics (2022-23), ABS Causes of Death 2023 (Cube 10)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.caption = element_text(size = 8, colour = "grey50")
    )
  )

ggsave("outputs/figures/fig16_hospital_vs_mur.png", p_combined,
       width = 16, height = 12, dpi = 300, bg = "white")

cat("Figure saved: outputs/figures/fig16_hospital_vs_mur.png\n")

cat("\n============================================================\n")
cat("Script 18 complete.\n")
cat("============================================================\n")
