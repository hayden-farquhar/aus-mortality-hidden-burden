# ============================================================================
# Script 19: PBS Prescribing Trends vs Hypertension Mortality
#
# EXPLORATORY ANALYSIS (E-HM2)
#
# Compares pharmaceutical prescribing trends for cardiovascular /
# antihypertensive drugs (PBS data, 2015-2024) against temporal trends
# in hypertension mortality (ABS Cube 14, 2014-2024).
#
# Explores whether increasing treatment intensity corresponds to
# changes in hypertension-attributed mortality, and discusses
# implications in the context of hypertension's high MUR (indicating
# that hypertension is frequently a contributing but not underlying
# cause of death).
#
# PBS drug classes examined:
#   - Agents acting on the renin-angiotensin system (RAAS inhibitors)
#   - Antihypertensives
#   - Beta blocking agents
#   - Calcium channel blockers (if available)
#   - Lipid modifying agents (if available)
#
# INPUT: outputs/exploratory/pbs_cardiovascular.csv
#        outputs/exploratory/cube14_temporal_all_causes.csv
#
# OUTPUT: outputs/exploratory/pbs_vs_mortality.csv
#         outputs/exploratory/pbs_vs_mortality.txt
#         outputs/figures/fig17_pbs_vs_mortality.png
#
# PACKAGES: tidyverse, patchwork
# ============================================================================

library(tidyverse)

if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork", repos = "https://cloud.r-project.org")
}
library(patchwork)

cat("============================================================\n")
cat("Script 19: PBS Prescribing vs Hypertension Mortality\n")
cat("============================================================\n\n")

dir.create("outputs/exploratory", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# Load PBS data
# ============================================================================

pbs_file <- "outputs/exploratory/pbs_cardiovascular.csv"
if (!file.exists(pbs_file)) {
  stop("ERROR: ", pbs_file, " not found. Run earlier scripts first.")
}

pbs_raw <- read_csv(pbs_file, show_col_types = FALSE)
cat("PBS data loaded:", nrow(pbs_raw), "rows\n")

# PBS data is in wide format:
# Columns: Type of script, Age group, Value, JAN-2015, FEB-2015, ..., DEC-2025
# Rows: Drug class x Age group x Value type (Benefits per ERP, Scripts per ERP)

# ============================================================================
# Reshape PBS data to long format
# ============================================================================

# Get month columns (those matching MMM-YYYY pattern)
month_cols <- names(pbs_raw)[str_detect(names(pbs_raw), "^[A-Z]{3}-\\d{4}$")]

pbs_long <- pbs_raw %>%
  rename(drug_class = `Type of script`,
         age_group = `Age group`,
         value_type = Value) %>%
  pivot_longer(
    cols = all_of(month_cols),
    names_to = "month_year",
    values_to = "value"
  ) %>%
  mutate(
    month_year = str_trim(month_year),
    date = as.Date(paste0("01-", month_year), format = "%d-%b-%Y"),
    year = year(date),
    month = month(date)
  ) %>%
  filter(!is.na(value), !is.na(date))

cat("PBS data reshaped:", nrow(pbs_long), "observations\n")
cat("Drug classes:", paste(unique(pbs_long$drug_class), collapse = ", "), "\n")
cat("Value types:", paste(unique(pbs_long$value_type), collapse = ", "), "\n")
cat("Age groups:", paste(unique(pbs_long$age_group), collapse = ", "), "\n")

# ============================================================================
# Aggregate PBS to annual totals (Total age group, Scripts per ERP)
# ============================================================================

# Focus on "Total" age group and "Scripts per ERP" as the primary metric
# Scripts per ERP = prescriptions per eligible resident population
# This is already a population-adjusted rate

pbs_annual <- pbs_long %>%
  filter(age_group == "Total", value_type == "Scripts per ERP") %>%
  group_by(drug_class, year) %>%
  summarise(
    annual_scripts_per_erp = sum(value, na.rm = TRUE),
    n_months = n(),
    .groups = "drop"
  ) %>%
  # Only include complete years (12 months of data)
  filter(n_months == 12)

cat("\nPBS annual data (complete years only):\n")
cat("Years available:", paste(sort(unique(pbs_annual$year)), collapse = ", "), "\n")
cat("Drug classes:\n")
for (dc in unique(pbs_annual$drug_class)) {
  yrs <- pbs_annual %>% filter(drug_class == dc) %>% pull(year)
  cat(sprintf("  %-50s %d-%d\n", dc, min(yrs), max(yrs)))
}

# Also get benefits per ERP for context
pbs_benefits <- pbs_long %>%
  filter(age_group == "Total", value_type == "Benefits per ERP") %>%
  group_by(drug_class, year) %>%
  summarise(
    annual_benefits_per_erp = sum(value, na.rm = TRUE),
    n_months = n(),
    .groups = "drop"
  ) %>%
  filter(n_months == 12)

# ============================================================================
# Load temporal mortality data
# ============================================================================

temporal_file <- "outputs/exploratory/cube14_temporal_all_causes.csv"
if (!file.exists(temporal_file)) {
  stop("ERROR: ", temporal_file, " not found. Run earlier scripts first.")
}

temporal_raw <- read_csv(temporal_file, show_col_types = FALSE)
cat("\nTemporal mortality data loaded:", nrow(temporal_raw), "rows\n")

# Extract hypertensive diseases (I10-I15 block) and individual codes
hypertension_deaths <- temporal_raw %>%
  filter(
    icd_code %in% c("I10-I15", "I10", "I11", "I12", "I13"),
    sex == "persons"
  ) %>%
  select(cause, icd_code, year, deaths)

cat("Hypertension temporal data:\n")
for (code in unique(hypertension_deaths$icd_code)) {
  yrs <- hypertension_deaths %>% filter(icd_code == code)
  cat(sprintf("  %-40s [%s]  %d years\n",
              yrs$cause[1], code, nrow(yrs)))
}

# Also get total deaths for rate computation
total_deaths <- temporal_raw %>%
  filter(cause == "Total deaths", sex == "persons") %>%
  select(year, total_deaths = deaths)

# Also get all circulatory system deaths for context
circulatory_deaths <- temporal_raw %>%
  filter(icd_code == "I00-I99", sex == "persons") %>%
  select(year, circ_deaths = deaths)

# ============================================================================
# Merge PBS and mortality data
# ============================================================================

# Focus on key drug classes relevant to hypertension
key_drugs <- c("Agents acting on the renin-angiotensin system",
               "Antihypertensives",
               "Beta blocking agents")

# Get overlapping years
pbs_years <- pbs_annual %>% filter(drug_class %in% key_drugs) %>%
  pull(year) %>% unique()
mort_years <- hypertension_deaths %>% pull(year) %>% unique()
overlap_years <- intersect(pbs_years, mort_years)

cat("\nOverlapping years for analysis:", paste(sort(overlap_years), collapse = ", "), "\n")

# Create merged dataset: hypertension block-level deaths + PBS
htn_block <- hypertension_deaths %>%
  filter(icd_code == "I10-I15", year %in% overlap_years) %>%
  left_join(total_deaths, by = "year") %>%
  left_join(circulatory_deaths, by = "year") %>%
  mutate(
    htn_pct_of_total = 100 * deaths / total_deaths,
    htn_pct_of_circ = 100 * deaths / circ_deaths
  )

# Wide PBS for the key drug classes
pbs_wide <- pbs_annual %>%
  filter(drug_class %in% key_drugs, year %in% overlap_years) %>%
  select(drug_class, year, annual_scripts_per_erp) %>%
  pivot_wider(names_from = drug_class, values_from = annual_scripts_per_erp,
              names_prefix = "pbs_")

# Clean column names
names(pbs_wide) <- names(pbs_wide) %>%
  str_replace_all(" ", "_") %>%
  str_to_lower()

merged <- htn_block %>%
  left_join(pbs_wide, by = "year")

# ============================================================================
# Statistical tests
# ============================================================================

cat("\n============================================================\n")
cat("CORRELATION ANALYSES\n")
cat("============================================================\n\n")

# Get the PBS column names dynamically
pbs_cols <- names(merged)[str_detect(names(merged), "^pbs_")]

results_list <- list()

for (pc in pbs_cols) {
  drug_name <- str_replace(pc, "^pbs_", "") %>%
    str_replace_all("_", " ") %>%
    str_to_title()

  vals <- merged[[pc]]
  if (all(is.na(vals))) next

  # Spearman: PBS scripts vs hypertension deaths
  cor_result <- cor.test(merged[[pc]], merged$deaths,
                         method = "spearman", exact = FALSE)

  cat(sprintf("%s vs Hypertension Deaths (Spearman):\n", drug_name))
  cat(sprintf("  rho = %.3f, p = %.4f, n = %d\n\n",
              cor_result$estimate, cor_result$p.value, nrow(merged)))

  results_list[[pc]] <- tibble(
    drug_class = drug_name,
    test = "Spearman: PBS scripts/ERP vs HTN deaths",
    rho = as.double(cor_result$estimate),
    p_value = cor_result$p.value,
    n = nrow(merged)
  )
}

# Also: hypertension as % of total deaths over time
cor_pct <- cor.test(merged$year, merged$htn_pct_of_total,
                    method = "spearman", exact = FALSE)
cat(sprintf("Year vs Hypertension %% of total deaths (Spearman):\n"))
cat(sprintf("  rho = %.3f, p = %.4f\n", cor_pct$estimate, cor_pct$p.value))
cat(sprintf("  HTN share of total deaths: %.2f%% (%d) to %.2f%% (%d)\n\n",
            merged$htn_pct_of_total[1], min(merged$year),
            merged$htn_pct_of_total[nrow(merged)], max(merged$year)))

results_list[["htn_trend"]] <- tibble(
  drug_class = "Temporal trend",
  test = "Spearman: Year vs HTN % of total deaths",
  rho = as.double(cor_pct$estimate),
  p_value = cor_pct$p.value,
  n = nrow(merged)
)

results_df <- bind_rows(results_list)

# ============================================================================
# Save results
# ============================================================================

write_csv(merged, "outputs/exploratory/pbs_vs_mortality.csv")
cat("Merged data saved: outputs/exploratory/pbs_vs_mortality.csv\n")

# ============================================================================
# Write summary text
# ============================================================================

sink("outputs/exploratory/pbs_vs_mortality.txt")

cat("============================================================\n")
cat("PBS PRESCRIBING vs HYPERTENSION MORTALITY\n")
cat("Exploratory Analysis E-HM2\n")
cat("============================================================\n\n")
cat(sprintf("Date: %s\n", format(Sys.time(), "%Y-%m-%d %H:%M")))
cat(sprintf("Overlap years: %d-%d (%d years)\n\n",
            min(overlap_years), max(overlap_years), length(overlap_years)))

cat("TEMPORAL TRENDS:\n\n")
cat(sprintf("  Hypertension underlying deaths (I10-I15):\n"))
cat(sprintf("    %d: %s deaths (%.2f%% of total)\n",
            min(merged$year), format(merged$deaths[1], big.mark = ","),
            merged$htn_pct_of_total[1]))
cat(sprintf("    %d: %s deaths (%.2f%% of total)\n",
            max(merged$year),
            format(merged$deaths[nrow(merged)], big.mark = ","),
            merged$htn_pct_of_total[nrow(merged)]))
pct_change_deaths <- 100 * (merged$deaths[nrow(merged)] / merged$deaths[1] - 1)
cat(sprintf("    Change: %+.1f%%\n\n", pct_change_deaths))

for (pc in pbs_cols) {
  drug_name <- str_replace(pc, "^pbs_", "") %>%
    str_replace_all("_", " ") %>% str_to_title()
  vals <- merged[[pc]]
  if (all(is.na(vals))) next
  pct_change <- 100 * (vals[length(vals)] / vals[1] - 1)
  cat(sprintf("  %s (Scripts per ERP, annual):\n", drug_name))
  cat(sprintf("    %d: %.3f  |  %d: %.3f  |  Change: %+.1f%%\n\n",
              min(merged$year), vals[1],
              max(merged$year), vals[length(vals)],
              pct_change))
}

cat("CORRELATION RESULTS:\n\n")
for (i in seq_len(nrow(results_df))) {
  row <- results_df[i, ]
  sig <- ifelse(row$p_value < 0.05, " *", "")
  cat(sprintf("  %-35s rho = %+.3f, p = %.4f%s\n",
              paste0(row$drug_class, ":"), row$rho, row$p_value, sig))
}

cat("\nINTERPRETATION:\n\n")
cat("Hypertension (I10-I15) has a MUR of ~8 in 2023 data, meaning\n")
cat("hypertension is mentioned on death certificates ~8 times more\n")
cat("often as a contributing cause than as the underlying cause.\n\n")
cat("Despite increasing prescribing of antihypertensive medications,\n")
cat("hypertension-attributed mortality (as underlying cause) has been\n")
cat("increasing. This is consistent with:\n")
cat("  (a) Population ageing increasing the absolute number of deaths\n")
cat("  (b) Changes in certification practices\n")
cat("  (c) The paradox that treatment may shift deaths from acute\n")
cat("      hypertensive crises (underlying cause) to chronic conditions\n")
cat("      where hypertension is a contributor (increasing MUR)\n\n")

cat("NOTES:\n\n")
cat("1. PBS data: Pharmaceutical Benefits Scheme, Benefits per ERP and\n")
cat("   Scripts per ERP (population-adjusted prescribing rate).\n")
cat("2. Mortality data: ABS Cube 14 (year of occurrence), underlying\n")
cat("   cause only. MUR requires multiple cause data (Cube 10, single\n")
cat("   year only).\n")
cat("3. Ecological analysis — cannot infer individual-level causation.\n")
cat("4. PBS data includes all ages; mortality trends reflect ageing\n")
cat("   population structure (not age-standardised).\n")

sink()

cat("Summary text saved: outputs/exploratory/pbs_vs_mortality.txt\n")

# ============================================================================
# Figure: PBS prescribing vs hypertension mortality
# ============================================================================

# Panel A: Indexed trends (all series indexed to first year = 100)
index_data <- merged %>%
  select(year, htn_deaths = deaths, all_of(pbs_cols)) %>%
  pivot_longer(-year, names_to = "series", values_to = "value") %>%
  group_by(series) %>%
  mutate(
    indexed = 100 * value / first(value),
    series_label = case_when(
      series == "htn_deaths" ~ "Hypertension deaths (I10-I15)",
      str_detect(series, "renin") ~ "RAAS inhibitors (PBS)",
      str_detect(series, "antihypertensives") ~ "Antihypertensives (PBS)",
      str_detect(series, "beta") ~ "Beta blockers (PBS)",
      TRUE ~ series
    )
  ) %>%
  ungroup()

p_a <- ggplot(index_data, aes(x = year, y = indexed,
                               colour = series_label, linetype = series_label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 100, linetype = "dotted", colour = "grey50") +
  scale_colour_manual(values = c(
    "Hypertension deaths (I10-I15)" = "#d7191c",
    "RAAS inhibitors (PBS)" = "#2c7bb6",
    "Antihypertensives (PBS)" = "#1a9641",
    "Beta blockers (PBS)" = "#fdae61"
  )) +
  scale_linetype_manual(values = c(
    "Hypertension deaths (I10-I15)" = "solid",
    "RAAS inhibitors (PBS)" = "dashed",
    "Antihypertensives (PBS)" = "dashed",
    "Beta blockers (PBS)" = "dashed"
  )) +
  labs(
    title = "A. Indexed Trends: Prescribing vs Hypertension Mortality",
    subtitle = sprintf("All series indexed to %d = 100", min(overlap_years)),
    x = "Year",
    y = "Index (baseline = 100)",
    colour = NULL, linetype = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    legend.position = "bottom",
    legend.text = element_text(size = 8)
  ) +
  guides(colour = guide_legend(nrow = 2))

# Panel B: Hypertension as % of total and circulatory deaths
pct_data <- merged %>%
  select(year, htn_pct_of_total, htn_pct_of_circ) %>%
  pivot_longer(-year, names_to = "metric", values_to = "pct") %>%
  mutate(
    metric_label = case_when(
      metric == "htn_pct_of_total" ~ "% of all deaths",
      metric == "htn_pct_of_circ" ~ "% of circulatory deaths"
    )
  )

p_b <- ggplot(pct_data, aes(x = year, y = pct, colour = metric_label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_colour_manual(values = c(
    "% of all deaths" = "#d7191c",
    "% of circulatory deaths" = "#2c7bb6"
  )) +
  labs(
    title = "B. Hypertension Share of Mortality Over Time",
    subtitle = "Hypertension (I10-I15) as underlying cause",
    x = "Year",
    y = "Percentage (%)",
    colour = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    legend.position = "bottom"
  )

# Panel C: Scatter — RAAS prescribing vs hypertension deaths
raas_col <- pbs_cols[str_detect(pbs_cols, "renin")]
if (length(raas_col) > 0) {
  scatter_data <- merged %>%
    select(year, deaths, pbs = all_of(raas_col[1]))

  cor_raas <- cor.test(scatter_data$pbs, scatter_data$deaths,
                       method = "spearman", exact = FALSE)

  p_c <- ggplot(scatter_data, aes(x = pbs, y = deaths)) +
    geom_point(aes(colour = year), size = 3) +
    geom_smooth(method = "lm", se = TRUE, colour = "grey50",
                linetype = "dashed", linewidth = 0.5) +
    geom_text(aes(label = year), hjust = -0.3, size = 2.8) +
    scale_colour_viridis_c() +
    labs(
      title = "C. RAAS Inhibitor Prescribing vs Hypertension Deaths",
      subtitle = sprintf("Spearman rho = %.2f, p = %.3f",
                         cor_raas$estimate, cor_raas$p.value),
      x = "RAAS inhibitor scripts per ERP (annual)",
      y = "Hypertension deaths (underlying cause)",
      colour = "Year"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, colour = "grey40"),
      legend.position = "right"
    )
} else {
  p_c <- ggplot() + theme_void() +
    labs(title = "C. RAAS data not available")
}

# Combine
p_combined <- (p_a | p_b) / p_c +
  plot_annotation(
    title = "PBS Prescribing Trends vs Hypertension Mortality",
    subtitle = "Exploring the paradox: more treatment, but increasing attributed mortality",
    caption = paste0("Sources: PBS Item Reports (2015-2024), ",
                     "ABS Causes of Death Cube 14 (2014-2024)"),
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, colour = "grey40"),
      plot.caption = element_text(size = 8, colour = "grey50")
    )
  )

ggsave("outputs/figures/fig17_pbs_vs_mortality.png", p_combined,
       width = 16, height = 12, dpi = 300, bg = "white")

cat("Figure saved: outputs/figures/fig17_pbs_vs_mortality.png\n")

# ============================================================================
# Supplementary: Age-stratified PBS trends
# ============================================================================

# Show how prescribing differs by age group (65+ vs 19-64)
pbs_by_age <- pbs_long %>%
  filter(
    drug_class == "Agents acting on the renin-angiotensin system",
    value_type == "Scripts per ERP",
    age_group %in% c("19-64", "65+")
  ) %>%
  group_by(age_group, year) %>%
  summarise(annual_scripts = sum(value, na.rm = TRUE),
            n_months = n(), .groups = "drop") %>%
  filter(n_months == 12)

if (nrow(pbs_by_age) > 0) {
  p_age <- ggplot(pbs_by_age, aes(x = year, y = annual_scripts,
                                   colour = age_group)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_colour_manual(values = c("19-64" = "#2c7bb6", "65+" = "#d7191c")) +
    labs(
      title = "RAAS Inhibitor Prescribing by Age Group",
      subtitle = "Annual scripts per eligible resident population",
      x = "Year",
      y = "Scripts per ERP (annual)",
      colour = "Age group"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      legend.position = "top"
    )

  ggsave("outputs/figures/fig17b_pbs_by_age.png", p_age,
         width = 10, height = 6, dpi = 300, bg = "white")
  cat("Figure saved: outputs/figures/fig17b_pbs_by_age.png\n")
}

cat("\n============================================================\n")
cat("Script 19 complete.\n")
cat("============================================================\n")
