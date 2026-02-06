# =============================================================================
# 25_cdc_wonder_mur_validation.R
# Purpose: Validate crude MUR vs age-standardised MUR using US CDC WONDER data
#
# This script computes both crude and directly age-standardised MUR to assess
# whether age confounding materially affects MUR estimates. Since ABS doesn't
# publish age-stratified multiple cause data, we use US CDC WONDER data as
# external validation.
#
# Input:  CDC WONDER exports (XLS format, tab-delimited)
# Output: Validation results CSV, figure, and markdown report
# =============================================================================

library(tidyverse)

# --- US 2000 Standard Population weights for direct age-standardisation ---
# Source: https://www.cdc.gov/nchs/data/statnt/statnt20.pdf
us_2000_std_pop <- tibble(
  age_group = c("< 1 year", "1-4 years", "5-14 years", "15-24 years",
                "25-34 years", "35-44 years", "45-54 years", "55-64 years",
                "65-74 years", "75-84 years", "85+ years"),
  population = c(3795000, 15192000, 39977000, 38077000, 37233000, 44659000,
                 37030000, 23961000, 18136000, 12315000, 4259000)
) %>%
  mutate(weight = population / sum(population))

# --- Cause groups to analyse (using GR113 codes from 113 Cause List) ---
cause_groups <- tibble(
  cause_code = c("GR113-046", "GR113-052", "GR113-069", "GR113-058",
                 "GR113-070", "GR113-076", "GR113-082", "GR113-100"),
  cause_short = c("Diabetes", "Alzheimer", "Hypertension", "Ischaemic heart",
                  "Cerebrovascular", "Influenza/pneumonia", "CLRD", "Renal failure"),
  cause_desc = c("Diabetes mellitus (E10-E14)",
                 "Alzheimer disease (G30)",
                 "Essential hypertension and hypertensive renal disease (I10,I12,I15)",
                 "Ischemic heart diseases (I20-I25)",
                 "Cerebrovascular diseases (I60-I69)",
                 "Influenza and pneumonia (J09-J18)",
                 "Chronic lower respiratory diseases (J40-J47)",
                 "Renal failure (N17-N19)")
)

# =============================================================================
# LOAD CDC WONDER EXPORT
# =============================================================================

load_wonder_export <- function(filepath) {
  # CDC WONDER exports are tab-delimited with a notes section at bottom
  # Read all lines first
  lines <- readLines(filepath, warn = FALSE)

  # Find where data ends (line starting with "---" or containing "Dataset:")
  end_idx <- which(str_detect(lines, '^"---"') | str_detect(lines, "Dataset:"))
  if (length(end_idx) > 0) {
    lines <- lines[1:(end_idx[1] - 1)]
  }

  # Remove empty lines
  lines <- lines[lines != "" & !str_detect(lines, "^\\s*$")]

  # Parse as TSV
  df <- read_tsv(
    paste(lines, collapse = "\n"),
    na = c("Suppressed", "Not Applicable", "Unreliable", "Missing", ""),
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  )

  cat("Loaded", basename(filepath), ":", nrow(df), "rows\n")
  return(df)
}

parse_deaths <- function(x) {
  # Parse death counts, handling suppressed/unreliable values
  x <- str_trim(x)
  x <- ifelse(x %in% c("Suppressed", "Not Applicable", "Unreliable", "Missing", ""),
              NA_character_, x)
  as.numeric(x)
}

# =============================================================================
# MUR COMPUTATION FUNCTIONS
# =============================================================================

compute_crude_mur <- function(mcd_total, ucd_total) {
  # Crude (all-ages pooled) MUR = total MCD mentions / total UCD deaths
  if (is.na(ucd_total) || ucd_total == 0) return(NA_real_)
  mcd_total / ucd_total
}

compute_age_standardised_mur <- function(data_by_age, std_pop) {
  # Direct age-standardisation of MUR
  # Method: Weight age-specific MUR by standard population proportions

  joined <- data_by_age %>%
    inner_join(std_pop, by = "age_group") %>%
    filter(ucd > 0, !is.na(mcd), !is.na(ucd)) %>%
    mutate(age_mur = mcd / ucd)

  if (nrow(joined) == 0) return(NA_real_)

  # Weighted average, rescaled if not all age groups present
  sum(joined$age_mur * joined$weight) / sum(joined$weight)
}

# =============================================================================
# MAIN ANALYSIS
# =============================================================================

run_analysis <- function(mcd_file, ucd_file, output_dir = "outputs/exploratory") {
  cat("=", rep("=", 69), "\n", sep = "")
  cat("CDC WONDER MUR AGE-STANDARDISATION VALIDATION\n")
  cat("=", rep("=", 69), "\n", sep = "")

  # --- Load data ---
  cat("\n--- Loading data ---\n")
  mcd_raw <- load_wonder_export(mcd_file)
  ucd_raw <- load_wonder_export(ucd_file)

  cat("\nMCD columns:", paste(names(mcd_raw), collapse = ", "), "\n")
  cat("UCD columns:", paste(names(ucd_raw), collapse = ", "), "\n")

  # --- Identify columns ---
  # MCD file has "MCD - ICD-10 113 Cause List Code"
  # UCD file has "ICD-10 113 Cause List Code"
  age_col <- "Ten-Year Age Groups"
  mcd_code_col <- names(mcd_raw)[str_detect(names(mcd_raw), "113 Cause List Code")][1]
  ucd_code_col <- names(ucd_raw)[str_detect(names(ucd_raw), "113 Cause List Code")][1]

  cat("\nUsing columns:\n")
  cat("  Age:", age_col, "\n")
  cat("  MCD cause code:", mcd_code_col, "\n")
  cat("  UCD cause code:", ucd_code_col, "\n")

  # --- Process MCD data ---
  mcd <- mcd_raw %>%
    mutate(
      age_group = str_trim(str_remove_all(.data[[age_col]], '"')),
      cause_code = str_trim(str_remove_all(.data[[mcd_code_col]], '"')),
      deaths = parse_deaths(Deaths)
    ) %>%
    filter(
      age_group %in% us_2000_std_pop$age_group,
      cause_code %in% cause_groups$cause_code
    ) %>%
    select(age_group, cause_code, mcd_deaths = deaths)

  # --- Process UCD data ---
  ucd <- ucd_raw %>%
    mutate(
      age_group = str_trim(str_remove_all(.data[[age_col]], '"')),
      cause_code = str_trim(str_remove_all(.data[[ucd_code_col]], '"')),
      deaths = parse_deaths(Deaths)
    ) %>%
    filter(
      age_group %in% us_2000_std_pop$age_group,
      cause_code %in% cause_groups$cause_code
    ) %>%
    select(age_group, cause_code, ucd_deaths = deaths)

  cat("\nFiltered rows: MCD", nrow(mcd), ", UCD", nrow(ucd), "\n")

  # --- Merge MCD and UCD ---
  merged <- full_join(mcd, ucd, by = c("age_group", "cause_code")) %>%
    replace_na(list(mcd_deaths = 0, ucd_deaths = 0))

  cat("Merged dataset:", nrow(merged), "rows\n")

  # --- Compute MUR for each cause ---
  results <- map_dfr(1:nrow(cause_groups), function(i) {
    cg <- cause_groups[i, ]
    code <- cg$cause_code

    # Filter merged data for this cause
    cause_data <- merged %>% filter(cause_code == code)

    # Totals
    total_mcd <- sum(cause_data$mcd_deaths, na.rm = TRUE)
    total_ucd <- sum(cause_data$ucd_deaths, na.rm = TRUE)

    # Crude MUR
    crude_mur <- compute_crude_mur(total_mcd, total_ucd)

    # Age-standardised MUR
    by_age <- cause_data %>%
      group_by(age_group) %>%
      summarise(mcd = sum(mcd_deaths, na.rm = TRUE),
                ucd = sum(ucd_deaths, na.rm = TRUE), .groups = "drop")
    age_std_mur <- compute_age_standardised_mur(by_age, us_2000_std_pop)

    # Differences
    abs_difference <- age_std_mur - crude_mur
    pct_difference <- (abs_difference / crude_mur) * 100
    material <- abs(pct_difference) > 10

    # Count valid age groups
    n_age_groups <- sum(cause_data$ucd_deaths > 0, na.rm = TRUE)

    tibble(
      cause_short = cg$cause_short,
      cause_code = code,
      cause_desc = cg$cause_desc,
      total_mcd = total_mcd,
      total_ucd = total_ucd,
      crude_mur = crude_mur,
      age_std_mur = age_std_mur,
      abs_difference = abs_difference,
      pct_difference = pct_difference,
      material = material,
      n_age_groups = n_age_groups
    )
  })

  # --- Display results ---
  cat("\n", rep("-", 85), "\n", sep = "")
  cat(sprintf("%-20s %10s %10s %9s %9s %8s %10s\n",
              "Cause", "MCD", "UCD", "Crude", "Age-Std", "% Diff", "Material"))
  cat(rep("-", 85), "\n", sep = "")

  for (i in 1:nrow(results)) {
    r <- results[i, ]
    flag <- ifelse(r$material, "YES", "no")
    cat(sprintf("%-20s %10.0f %10.0f %9.3f %9.3f %+7.1f%% %10s\n",
                r$cause_short, r$total_mcd, r$total_ucd,
                r$crude_mur, r$age_std_mur, r$pct_difference, flag))
  }

  # --- Summary ---
  n_material <- sum(results$material, na.rm = TRUE)
  n_concordant <- sum(!results$material, na.rm = TRUE)

  cat("\n")
  if (n_material > 0) {
    cat(sprintf("WARNING: %d of %d causes show >10%% divergence (material age confounding)\n",
                n_material, nrow(results)))
    material_causes <- results %>% filter(material)
    for (i in 1:nrow(material_causes)) {
      cat(sprintf("  - %s: %+.1f%%\n",
                  material_causes$cause_short[i],
                  material_causes$pct_difference[i]))
    }
  } else {
    cat("All causes show <10% divergence - crude MUR is a reliable approximation.\n")
  }

  # --- Save outputs ---
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  results_out <- results %>%
    select(cause_short, cause_code, cause_desc, total_mcd, total_ucd,
           crude_mur, age_std_mur, abs_difference, pct_difference, material)

  write_csv(results_out, file.path(output_dir, "cdc_mur_validation_results.csv"))
  cat("\nResults saved to:", file.path(output_dir, "cdc_mur_validation_results.csv"), "\n")

  # --- Save age-specific detail ---
  age_detail <- merged %>%
    left_join(cause_groups %>% select(cause_code, cause_short), by = "cause_code") %>%
    left_join(us_2000_std_pop %>% select(age_group, weight), by = "age_group") %>%
    mutate(age_specific_mur = ifelse(ucd_deaths > 0, mcd_deaths / ucd_deaths, NA_real_)) %>%
    select(cause_short, cause_code, age_group, mcd_deaths, ucd_deaths,
           age_specific_mur, std_weight = weight) %>%
    arrange(cause_code, age_group)

  write_csv(age_detail, file.path(output_dir, "cdc_mur_validation_age_detail.csv"))
  cat("Age detail saved to:", file.path(output_dir, "cdc_mur_validation_age_detail.csv"), "\n")

  return(results)
}

# =============================================================================
# GENERATE FIGURE
# =============================================================================

generate_validation_figure <- function(results, output_dir = "outputs/figures") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Prepare data
  plot_data <- results %>%
    filter(!is.na(crude_mur), !is.na(age_std_mur)) %>%
    mutate(cause_short = fct_reorder(cause_short, crude_mur))

  # Panel A: Crude vs Age-Std comparison
  p1 <- plot_data %>%
    pivot_longer(cols = c(crude_mur, age_std_mur),
                 names_to = "type", values_to = "mur") %>%
    mutate(type = ifelse(type == "crude_mur", "Crude MUR", "Age-Standardised MUR")) %>%
    ggplot(aes(x = mur, y = cause_short, fill = type)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
    geom_vline(xintercept = 1, linetype = "dotted", color = "grey50", alpha = 0.7) +
    scale_fill_manual(values = c("Crude MUR" = "#4C72B0", "Age-Standardised MUR" = "#DD8452")) +
    labs(x = "Multiple-to-Underlying Ratio", y = NULL, fill = NULL,
         title = "A. Crude vs Age-Standardised MUR") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          panel.grid.major.y = element_blank())

  # Panel B: % Difference with materiality threshold
  p2 <- plot_data %>%
    mutate(fill_color = ifelse(abs(pct_difference) > 10, "Material (>10%)", "Concordant")) %>%
    ggplot(aes(x = pct_difference, y = cause_short, fill = fill_color)) +
    geom_col(alpha = 0.85, width = 0.6) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
    geom_vline(xintercept = c(-10, 10), linetype = "dashed", color = "red", alpha = 0.6) +
    scale_fill_manual(values = c("Concordant" = "#55A868", "Material (>10%)" = "#C44E52")) +
    labs(x = "% Difference (age-standardised minus crude)", y = NULL, fill = NULL,
         title = "B. Materiality of Age Correction") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          panel.grid.major.y = element_blank())

  # Combine
  library(patchwork)
  combined <- p1 + p2 +
    plot_annotation(
      title = "Validation: Age-Standardised vs Crude MUR",
      subtitle = "US CDC WONDER Multiple Cause of Death, 2020",
      theme = theme(plot.title = element_text(face = "bold", size = 14),
                    plot.subtitle = element_text(size = 11, color = "grey40"))
    )

  fig_path <- file.path(output_dir, "fig_cdc_mur_validation.png")
  ggsave(fig_path, combined, width = 14, height = 6, dpi = 150, bg = "white")
  cat("Figure saved to:", fig_path, "\n")

  return(combined)
}

# =============================================================================
# GENERATE REPORT
# =============================================================================

generate_validation_report <- function(results, output_dir = "outputs/exploratory") {

  n_total <- nrow(results %>% filter(!is.na(pct_difference)))
  n_material <- sum(results$material, na.rm = TRUE)
  n_concordant <- n_total - n_material

  material_causes <- results %>% filter(material == TRUE)
  concordant_causes <- results %>% filter(material == FALSE)

  report <- c(
    "# MUR Age-Standardisation Validation Report",
    "",
    "## Data Source",
    "",
    "- **Multiple Cause of Death:** CDC WONDER Database (2018-2023, Single Race), Year 2020",
    "- **Underlying Cause of Death:** CDC WONDER Database (2018-2023, Single Race), Year 2020",
    "- **Age grouping:** Ten-year age groups (< 1 year to 85+)",
    "- **Standard population:** US 2000 Standard Population (Census P25-1130)",
    paste0("- **Analysis date:** ", Sys.Date()),
    "",
    "## Method",
    "",
    "For each cause of death category, we computed:",
    "",
    "1. **Crude MUR** = Total MCD deaths (all ages) / Total UCD deaths (all ages)",
    "2. **Age-specific MUR** = MCD deaths in age group / UCD deaths in age group",
    "3. **Age-standardised MUR** = Sum(age-specific MUR x standard population weight), rescaled",
    "",
    "A divergence of >10% between crude and age-standardised MUR was considered material.",
    "",
    "## Summary Results",
    "",
    "| Cause | Crude MUR | Age-Std MUR | Difference | Material? |",
    "|-------|-----------|-------------|------------|-----------|"
  )

  for (i in 1:nrow(results)) {
    r <- results[i, ]
    if (is.na(r$crude_mur)) next
    flag <- ifelse(r$material, "Yes", "No")
    report <- c(report, sprintf("| %s | %.3f | %.3f | %+.1f%% | %s |",
                                r$cause_short, r$crude_mur, r$age_std_mur,
                                r$pct_difference, flag))
  }

  report <- c(report, "", "## Interpretation", "")

  report <- c(report, sprintf(
    "Of %d cause groups analysed, **%d showed concordant** crude and age-standardised MURs (divergence <=10%%), while **%d showed material divergence** (>10%%).",
    n_total, n_concordant, n_material
  ), "")

  if (n_concordant > 0) {
    report <- c(report, "**Concordant causes** (crude MUR is a reliable approximation):", "")
    for (i in 1:nrow(concordant_causes)) {
      report <- c(report, sprintf("- %s: %+.1f%% difference",
                                  concordant_causes$cause_short[i],
                                  concordant_causes$pct_difference[i]))
    }
    report <- c(report, "")
  }

  if (n_material > 0) {
    report <- c(report, "**Materially divergent causes** (age correction changes the result):", "")
    for (i in 1:nrow(material_causes)) {
      direction <- ifelse(material_causes$pct_difference[i] > 0, "higher", "lower")
      report <- c(report, sprintf("- %s: age-standardised MUR is %.1f%% %s than crude",
                                  material_causes$cause_short[i],
                                  abs(material_causes$pct_difference[i]), direction))
    }
    report <- c(report, "")
  }

  report <- c(report,
    "## Implications for Paper A (Australian MUR Analysis)",
    ""
  )

  if (n_material == 0) {
    report <- c(report,
      "All cause groups showed concordant crude and age-standardised MURs. This supports",
      "the use of crude MUR from Australian ABS data (which lacks age-stratified multiple",
      "cause counts) as a valid approximation. No sensitivity analysis for age structure",
      "is required.",
      ""
    )
  } else {
    report <- c(report,
      sprintf("Most cause groups (%d of %d) showed concordant crude and age-standardised MURs.",
              n_concordant, n_total),
      sprintf("For the %d cause(s) with material divergence, Australian crude MUR findings", n_material),
      "should be interpreted with the caveat that age standardisation may modify the result.",
      "",
      "**Recommended Paper A language:**",
      "",
      sprintf("> \"To assess whether age structure affects MUR estimates, we conducted a validation"),
      sprintf("> analysis using US CDC WONDER data for 2020. Of %d cause groups, %d showed <10%%",
              n_total, n_concordant),
      sprintf("> divergence between crude and directly age-standardised MUR, supporting the use"),
      sprintf("> of crude MUR as a reasonable approximation. For %d cause(s) showing >10%% divergence,",
              n_material),
      "> Australian crude MUR findings should be interpreted with this caveat.\"",
      ""
    )
  }

  report <- c(report,
    "## Methodological Note",
    "",
    "This validation uses US data as a proxy for the age-correction effect. The assumption",
    "is that the direction and approximate magnitude of age-correction bias are transferable",
    "between the US and Australia, given similar disease epidemiology and age structures.",
    ""
  )

  report_path <- file.path(output_dir, "cdc_mur_validation_report.md")
  writeLines(report, report_path)
  cat("Report saved to:", report_path, "\n")
}

# =============================================================================
# ENTRY POINT
# =============================================================================

# File paths (relative to Analysis/ working directory)
mcd_file <- "../Confirmatory exploration/Data from sources/cdc_mcd_by_age.xls"
ucd_file <- "../Confirmatory exploration/Data from sources/cdc_ucd_by_age.xls"

if (file.exists(mcd_file) && file.exists(ucd_file)) {
  cat("Found CDC WONDER exports, running analysis...\n\n")

  results <- run_analysis(mcd_file, ucd_file)

  cat("\n--- Generating outputs ---\n")
  generate_validation_figure(results)
  generate_validation_report(results)

  cat("\n=== CDC WONDER MUR Validation Complete ===\n")

} else {
  cat("ERROR: CDC WONDER data files not found.\n")
  cat("Expected locations:\n")
  cat("  ", mcd_file, "\n")
  cat("  ", ucd_file, "\n")
  cat("\nPlease download data from CDC WONDER following the query guide.\n")
}
