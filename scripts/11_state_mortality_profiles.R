# ============================================================================
# Script 11: State/Territory Mortality Profile Variation
# 
# NOVEL ANALYSIS: Identifies which causes of death show the most geographic 
# variation across Australian states/territories. Conditions with high 
# variation point to state-specific risk factors, healthcare access 
# differences, or coding practice inconsistencies — all publishable findings.
#
# Note: Multiple cause of death data is only available at the national level 
# in the 2024 ABS release. This analysis uses underlying cause by state.
#
# Data source: ABS Causes of Death 2024, Tables 1-9
# ============================================================================

library(tidyverse)

# --- Helper: Smart label shortener for ICD chapter names ---------------------
shorten_chapter <- function(x) {
  x %>%
    str_remove("^CHAPTER [IVXLC]+ ") %>%
    str_replace("Diseases of the ", "") %>%
    str_replace("Certain infectious and parasitic diseases", "Infectious & parasitic diseases") %>%
    str_replace("Endocrine, nutritional and metabolic diseases", "Endocrine & metabolic diseases") %>%
    str_replace("Mental and behavioural disorders", "Mental & behavioural disorders") %>%
    str_replace("Certain conditions originating in the perinatal period", "Perinatal conditions") %>%
    str_replace("Congenital malformations, deformations and chromosomal abnormalities",
                "Congenital malformations") %>%
    str_replace("Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",
                "Symptoms & abnormal findings (NEC)") %>%
    str_replace("External causes of morbidity and mortality", "External causes") %>%
    str_replace("blood and blood-forming organs and certain disorders involving the immune mechanism",
                "Blood & immune system") %>%
    str_replace("musculoskeletal system and connective tissue", "Musculoskeletal system") %>%
    str_replace("skin and subcutaneous tissue", "Skin & subcutaneous tissue") %>%
    str_replace("circulatory system", "Circulatory system") %>%
    str_replace("respiratory system", "Respiratory system") %>%
    str_replace("digestive system", "Digestive system") %>%
    str_replace("nervous system", "Nervous system") %>%
    str_replace("genitourinary system", "Genitourinary system")
}

# --- Helper: Smart label shortener for specific ICD condition names -----------
# Applies targeted abbreviations for verbose ICD-10 names rather than blind
# truncation. Falls back to str_trunc() as a safety net for unmapped names.
shorten_condition <- function(x, max_chars = 55) {
  x %>%
    # --- External causes: accidental poisoning ---
    str_replace("Accidental poisoning by and exposure to ", "Accid. poisoning: ") %>%
    str_replace("antiepileptic, sedative-hypnotic, antiparkinsonism and psychotropic drugs, not elsewhere classified",
                "sedatives/psychotropics (NEC)") %>%
    str_replace("other and unspecified drugs, medicaments and biological substances",
                "other drugs & biologicals") %>%
    str_replace("noxious substances, undetermined intent", "noxious substances (undeterm.)") %>%
    str_replace("narcotics and psychodysleptics \\[hallucinogens\\], not elsewhere classified",
                "narcotics/hallucinogens (NEC)") %>%
    str_replace("noxious subs.*$", "noxious substances") %>%
    # --- External causes: falls ---
    str_replace("Fall on same level from slipping, tripping and stumbling",
                "Fall: slipping/tripping/stumbling") %>%
    # --- External causes: transport ---
    str_replace("Car occupant injured in collision with fixed or stationary object",
                "Car occupant vs fixed object") %>%
    str_replace("Car occupant injured in collision with car, pick-up truck or van",
                "Car occupant vs car/van") %>%
    str_replace("Car occupant injured in transport accident", "Car occupant: transport accident") %>%
    str_replace("Motorcycle rider injured in transport accident", "Motorcycle rider: transport accident") %>%
    str_replace("Pedestrian injured in transport accident", "Pedestrian: transport accident") %>%
    # --- External causes: other ---
    str_replace("Fetus and newborn affected by maternal factors and by complications of pregnancy, labour and delivery",
                "Fetus/newborn: maternal complications") %>%
    str_replace("Other external causes of accidental injury", "Other accidental injury causes") %>%
    str_replace("Other external causes of mortality", "Other external mortality causes") %>%
    str_replace("Other accidental threats to breathing", "Other accidental breathing threats") %>%
    str_replace("Accidental exposure to other and unspecified factors",
                "Accidental exposure: other/unspecified") %>%
    str_replace("Exposure to unspecified factor", "Exposure: unspecified factor") %>%
    str_replace("Accidental drowning and submersion", "Accidental drowning/submersion") %>%
    str_replace("Intentional self-harm by hanging, strangulation and suffocation",
                "Self-harm: hanging/strangulation") %>%
    # --- Cardiovascular ---
    str_replace("Essential.*hypertension", "Essential hypertension") %>%
    str_replace("^Essential$", "Essential hypertension") %>%
    str_replace("Pulmonary heart disease and diseases of pulmonary circulation",
                "Pulmonary heart disease & circulation") %>%
    str_replace("Complications and ill-defined descriptions of heart disease",
                "Complications/ill-defined heart disease") %>%
    # --- Mental/behavioural ---
    str_replace("Mental and behavioural disorders due to (use of )?psychoactive substance(s| use)",
                "Mental/behavioural: psychoactive substances") %>%
    # --- Endocrine/metabolic ---
    str_replace("Non-insulin-dependent diabetes mellitus", "Type 2 diabetes mellitus") %>%
    # --- Other common long names ---
    str_replace("Other disorders of fluid, electrolyte and acid-base balance",
                "Fluid/electrolyte/acid-base disorders") %>%
    str_replace("Sequelae of external causes of morbidity and mortality",
                "Sequelae of external causes") %>%
    str_replace("Osteoporosis without pathological fracture", "Osteoporosis (no fracture)") %>%
    str_replace("Viral pneumonia, not elsewhere classified", "Viral pneumonia (NEC)") %>%
    str_replace("Unspecified acute lower respiratory infection", "Acute lower resp. infection (NOS)") %>%
    str_replace("Other diseases of the respiratory system", "Other respiratory diseases") %>%
    str_replace("Disorders of bone density and structure", "Bone density/structure disorders") %>%
    str_replace("Other ill-defined and unspecified causes of mortality",
                "Ill-defined/unspecified mortality causes") %>%
    str_replace("Ill-defined and unknown causes of mortality", "Ill-defined/unknown mortality causes") %>%
    str_replace("Diseases of oesophagus, stomach and duodenum",
                "Oesophagus/stomach/duodenum diseases") %>%
    str_replace("Pneumonitis due to solids and liquids", "Aspiration pneumonitis") %>%
    str_replace("Atrial fibrillation and flutter", "Atrial fibrillation/flutter") %>%
    str_replace("Other general symptoms and signs", "Other general symptoms/signs") %>%
    str_replace("Disorders of thyroid gland", "Thyroid disorders") %>%
    str_replace("Other forms of heart disease", "Other heart disease forms") %>%
    str_replace("Other peripheral vascular diseases", "Other peripheral vascular dis.") %>%
    str_replace("Other cerebrovascular diseases", "Other cerebrovascular diseases") %>%
    # --- Safety net: truncate cleanly at word boundary if still too long ---
    str_trunc(max_chars, ellipsis = "...")
}

# --- Load data ---------------------------------------------------------------
deaths_by_state <- read_csv("deaths_by_state.csv", show_col_types = FALSE)

# --- Clean and prepare -------------------------------------------------------
state_data <- deaths_by_state %>%
  filter(
    state_abbr %in% c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT"),
    !is_total
  ) %>%
  mutate(
    asr_persons = as.numeric(asr_persons),
    n_persons   = as.numeric(n_persons)
  )

cat("States/territories in data:", paste(sort(unique(state_data$state_abbr)), collapse = ", "), "\n")
cat("Total rows:", nrow(state_data), "\n")

# --- Analysis 1: Coefficient of variation across states ----------------------
state_variation <- state_data %>%
  filter(is_chapter, !is.na(asr_persons), asr_persons > 0) %>%
  group_by(cause_name, icd10_code) %>%
  summarise(
    n_states     = n(),
    mean_asr     = mean(asr_persons, na.rm = TRUE),
    sd_asr       = sd(asr_persons, na.rm = TRUE),
    min_asr      = min(asr_persons, na.rm = TRUE),
    max_asr      = max(asr_persons, na.rm = TRUE),
    range_asr    = max_asr - min_asr,
    cv           = sd_asr / mean_asr * 100,
    min_state    = state_abbr[which.min(asr_persons)],
    max_state    = state_abbr[which.max(asr_persons)],
    total_deaths = sum(n_persons, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_states >= 6, mean_asr >= 1) %>%
  arrange(desc(cv))

cat("\n=== Causes of Death with HIGHEST Geographic Variation (Chapter Level) ===\n")
cat("(Coefficient of Variation across states/territories)\n\n")
state_variation %>%
  select(cause_name, mean_asr, cv, min_state, min_asr, max_state, max_asr, total_deaths) %>%
  mutate(across(where(is.numeric), ~round(., 1))) %>%
  print(n = 20)

# Figure 11a: CV by chapter — with full labels
p11a <- state_variation %>%
  mutate(cause_short = shorten_chapter(cause_name)) %>%
  ggplot(aes(x = reorder(cause_short, cv), y = cv)) +
  geom_col(aes(fill = cv), alpha = 0.85) +
  geom_text(aes(label = sprintf("%.0f%%", cv)), hjust = -0.2, size = 3) +
  coord_flip() +
  scale_fill_viridis_c(option = "rocket", direction = -1, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Geographic Variation in Mortality Across Australian States",
    subtitle = "Higher CV = more variation between states (after age-standardisation)",
    x = NULL,
    y = "Coefficient of Variation (%)",
    caption = "Data: ABS Causes of Death 2024\nAge-standardised rates across 8 states/territories"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave("outputs/figures/fig11a_geographic_variation_cv.png", p11a,
       width = 12, height = 8, dpi = 300, bg = "white")

# --- Analysis 2: State profiles heatmap --------------------------------------
heatmap_data <- state_data %>%
  filter(is_chapter, !is.na(asr_persons), asr_persons > 0) %>%
  group_by(cause_name) %>%
  mutate(
    z_asr = (asr_persons - mean(asr_persons, na.rm = TRUE)) / sd(asr_persons, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    cause_short = shorten_chapter(cause_name),
    # Conditional text colour: white on dark tiles (high absolute z-score)
    text_colour = ifelse(abs(z_asr) > 1.2, "white", "black")
  )

p11b <- heatmap_data %>%
  ggplot(aes(x = state_abbr, y = reorder(cause_short, asr_persons), fill = z_asr)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = round(asr_persons, 1), colour = text_colour), size = 2.8) +
  scale_fill_gradient2(
    low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0,
    name = "Relative rate\n(z-score)"
  ) +
  scale_colour_identity() +
  labs(
    title = "State Mortality Profiles: Age-Standardised Death Rates by Cause",
    subtitle = "Red = above national average; Blue = below; Numbers show ASR per 100,000",
    x = NULL, y = NULL,
    caption = "Data: ABS Causes of Death 2024\nZ-scores calculated across states for each cause"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold"),
    axis.text.x   = element_text(face = "bold"),
    panel.grid     = element_blank()
  )

ggsave("outputs/figures/fig11b_state_mortality_heatmap.png", p11b,
       width = 12, height = 10, dpi = 300, bg = "white")

# --- Analysis 3: NT outlier analysis -----------------------------------------
nt_comparison <- state_data %>%
  filter(is_chapter, !is.na(asr_persons), asr_persons > 0) %>%
  group_by(cause_name) %>%
  mutate(
    national_mean = mean(asr_persons, na.rm = TRUE),
    ratio_to_mean = asr_persons / national_mean
  ) %>%
  ungroup() %>%
  filter(state_abbr == "NT") %>%
  arrange(desc(ratio_to_mean))

cat("\n=== Northern Territory: Ratio to National Average by Cause ===\n")
nt_comparison %>%
  select(cause_name, asr_persons, national_mean, ratio_to_mean) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  print(n = 20)

# --- Analysis 4: Sub-chapter conditions with highest state variation ---------
subchapter_variation <- state_data %>%
  filter(!is_chapter, !is.na(asr_persons), asr_persons > 0) %>%
  group_by(cause_name, icd10_code) %>%
  summarise(
    n_states     = n(),
    mean_asr     = mean(asr_persons, na.rm = TRUE),
    sd_asr       = sd(asr_persons, na.rm = TRUE),
    cv           = sd_asr / mean_asr * 100,
    min_state    = state_abbr[which.min(asr_persons)],
    max_state    = state_abbr[which.max(asr_persons)],
    max_min_ratio = max(asr_persons) / min(asr_persons),
    total_deaths = sum(n_persons, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_states >= 6, mean_asr >= 2, total_deaths >= 100) %>%
  arrange(desc(cv))

cat("\n=== Sub-Chapter Conditions with HIGHEST Geographic Variation ===\n")
cat("(These warrant investigation for state-specific risk factors)\n\n")
subchapter_variation %>%
  select(cause_name, mean_asr, cv, min_state, max_state, max_min_ratio, total_deaths) %>%
  mutate(across(where(is.numeric), ~round(., 1))) %>%
  head(25) %>%
  print(n = 25)

# Figure 11c: Top 15 most geographically variable specific conditions
p11c <- subchapter_variation %>%
  head(15) %>%
  mutate(cause_short = shorten_condition(cause_name, max_chars = 55)) %>%
  ggplot(aes(x = reorder(cause_short, cv), y = cv)) +
  geom_col(aes(fill = max_min_ratio), alpha = 0.85) +
  geom_text(aes(label = sprintf("%.0f%%", cv)), hjust = -0.2, size = 3) +
  coord_flip() +
  scale_fill_viridis_c(name = "Max ÷ Min\nstate ratio", option = "magma") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Specific Conditions with Greatest State-to-State Variation",
    subtitle = "These conditions show the most inconsistent mortality patterns across Australia",
    x = NULL,
    y = "Coefficient of Variation (%)",
    caption = "Data: ABS Causes of Death 2024\nConditions with ≥100 deaths and ≥6 states reporting"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave("outputs/figures/fig11c_specific_condition_variation.png", p11c,
       width = 13, height = 8, dpi = 300, bg = "white")

# --- Save summary tables -----------------------------------------------------
write_csv(state_variation, "outputs/state_variation_chapters.csv")
write_csv(subchapter_variation %>% head(50), "outputs/state_variation_specific.csv")

cat("\n✓ Script 11 complete.\n")
cat("  Figures saved: fig11a, fig11b, fig11c\n")
cat("  Data saved: outputs/state_variation_chapters.csv, outputs/state_variation_specific.csv\n")
