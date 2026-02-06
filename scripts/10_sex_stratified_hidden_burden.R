# ============================================================================
# Script 10: Sex-Stratified Hidden Burden Analysis
# 
# NOVEL ANALYSIS: Compares male vs female "hidden burden" ratios — i.e., the 
# degree to which conditions are undercounted when only the underlying cause 
# of death is used vs multiple causes. Internationally, cardiovascular disease 
# is known to be under-recognised in women; this tests whether that pattern 
# holds in Australian death certification.
#
# Data source: ABS Causes of Death 2023, Table 10.2
# ============================================================================

library(tidyverse)

# Install ggrepel if not available (for non-overlapping scatter labels)
if (!requireNamespace("ggrepel", quietly = TRUE)) install.packages("ggrepel")
library(ggrepel)

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
uvm <- read_csv("underlying_vs_multiple.csv", show_col_types = FALSE)

# --- Focus on conditions with meaningful counts and ratios -------------------
sex_burden <- uvm %>%
  filter(
    !is.na(ratio_persons),
    !is_total,
    underlying_persons >= 50,
    ratio_persons >= 1.5
  ) %>%
  mutate(
    ratio_diff = ratio_male - ratio_female,
    ratio_diff_pct = (ratio_male / ratio_female - 1) * 100,
    more_hidden_in = case_when(
      ratio_male > ratio_female * 1.1 ~ "Males",
      ratio_female > ratio_male * 1.1 ~ "Females",
      TRUE ~ "Similar"
    )
  )

# --- Analysis 1: Overview of sex differences ---------------------------------
cat("=== Sex-Stratified Hidden Burden: Chapter Level ===\n\n")

chapter_sex <- sex_burden %>%
  filter(is_chapter) %>%
  arrange(desc(abs(ratio_diff)))

chapter_sex %>%
  select(cause_name, underlying_persons, ratio_male, ratio_female, 
         ratio_diff, more_hidden_in) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  print(n = 20)

# Figure 10a: Male vs Female hidden burden ratio by ICD chapter
p10a <- chapter_sex %>%
  mutate(cause_short = shorten_chapter(cause_name)) %>%
  pivot_longer(cols = c(ratio_male, ratio_female), 
               names_to = "sex", values_to = "ratio") %>%
  mutate(sex = ifelse(sex == "ratio_male", "Male", "Female")) %>%
  ggplot(aes(x = reorder(cause_short, ratio_persons), y = ratio, fill = sex)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
  coord_flip() +
  scale_fill_manual(values = c("Male" = "#2166ac", "Female" = "#b2182b"), name = NULL) +
  labs(
    title = "Hidden Burden by Sex: Multiple-to-Underlying Cause Ratio",
    subtitle = "Ratio > 1 means the condition is undercounted as underlying cause\nLarger ratios = more hidden burden",
    x = NULL,
    y = "Multiple cause mentions ÷ Underlying cause deaths",
    caption = "Data: ABS Causes of Death 2023, Table 10.2\nICD-10 chapters with ≥50 underlying cause deaths and ratio ≥1.5"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

ggsave("outputs/figures/fig10a_sex_hidden_burden_chapters.png", p10a,
       width = 12, height = 8, dpi = 300, bg = "white")

# --- Analysis 2: Biggest sex differences in specific conditions --------------
cat("\n=== Top 15 Conditions Where Males Are MORE Undercounted ===\n")
sex_burden %>%
  filter(!is_chapter, ratio_diff > 0) %>%
  arrange(desc(ratio_diff)) %>%
  select(cause_name, underlying_persons, ratio_male, ratio_female, ratio_diff) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  head(15) %>%
  print(n = 15)

cat("\n=== Top 15 Conditions Where Females Are MORE Undercounted ===\n")
sex_burden %>%
  filter(!is_chapter, ratio_diff < 0) %>%
  arrange(ratio_diff) %>%
  select(cause_name, underlying_persons, ratio_male, ratio_female, ratio_diff) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  head(15) %>%
  print(n = 15)

# Figure 10b: Conditions with largest sex differences — using ggrepel
top_sex_diff <- sex_burden %>%
  filter(!is_chapter, underlying_persons >= 100) %>%
  arrange(desc(abs(ratio_diff))) %>%
  head(20)

p10b <- top_sex_diff %>%
  mutate(cause_short = shorten_condition(cause_name, max_chars = 55)) %>%
  ggplot(aes(x = ratio_female, y = ratio_male)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.4) +
  geom_point(aes(size = underlying_persons, colour = more_hidden_in), alpha = 0.7) +
  geom_text_repel(aes(label = cause_short), size = 2.8,
                  max.overlaps = 20, box.padding = 0.4, 
                  segment.alpha = 0.3, seed = 42) +
  scale_colour_manual(
    values = c("Males" = "#2166ac", "Females" = "#b2182b", "Similar" = "#636363"),
    name = "More undercounted in"
  ) +
  scale_size_continuous(name = "Underlying\ncause deaths", labels = scales::comma) +
  labs(
    title = "Sex Differences in Hidden Disease Burden",
    subtitle = "Points above diagonal = more undercounted in males; below = more in females",
    x = "Female hidden burden ratio (multiple ÷ underlying)",
    y = "Male hidden burden ratio (multiple ÷ underlying)",
    caption = "Data: ABS Causes of Death 2023, Table 10.2\nConditions with ≥100 underlying cause deaths and ratio ≥1.5"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

ggsave("outputs/figures/fig10b_sex_difference_scatter.png", p10b,
       width = 13, height = 9, dpi = 300, bg = "white")

# --- Analysis 3: Specific clinical focus — cardiovascular & diabetes ---------
cat("\n=== Focus: Cardiovascular Conditions by Sex ===\n")
cv_sex <- uvm %>%
  filter(str_detect(icd10_code, "^I"), !is.na(ratio_persons), underlying_persons >= 50) %>%
  mutate(ratio_diff = ratio_male - ratio_female) %>%
  arrange(desc(abs(ratio_diff)))

cv_sex %>%
  select(cause_name, underlying_persons, ratio_male, ratio_female, ratio_diff) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  print(n = 20)

cat("\n=== Focus: Endocrine/Metabolic Conditions by Sex ===\n")
endo_sex <- uvm %>%
  filter(str_detect(icd10_code, "^E"), !is.na(ratio_persons), underlying_persons >= 50) %>%
  mutate(ratio_diff = ratio_male - ratio_female) %>%
  arrange(desc(abs(ratio_diff)))

endo_sex %>%
  select(cause_name, underlying_persons, ratio_male, ratio_female, ratio_diff) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  print(n = 15)

# Figure 10c: Cardiovascular hidden burden by sex — bar chart
cv_plot_data <- cv_sex %>%
  filter(underlying_persons >= 100) %>%
  head(12) %>%
  mutate(cause_short = shorten_condition(cause_name, max_chars = 50)) %>%
  pivot_longer(cols = c(ratio_male, ratio_female), 
               names_to = "sex", values_to = "ratio") %>%
  mutate(sex = ifelse(sex == "ratio_male", "Male", "Female"))

p10c <- ggplot(cv_plot_data, aes(x = reorder(cause_short, ratio), y = ratio, fill = sex)) +
  geom_col(position = "dodge", alpha = 0.85) +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
  coord_flip() +
  scale_fill_manual(values = c("Male" = "#2166ac", "Female" = "#b2182b"), name = NULL) +
  labs(
    title = "Cardiovascular Hidden Burden by Sex",
    subtitle = "Essential hypertension shows striking sex difference: 41.8× in males vs 21.2× in females",
    x = NULL,
    y = "Multiple cause mentions ÷ Underlying cause deaths",
    caption = "Data: ABS Causes of Death 2023, Table 10.2\nCardiovascular conditions (I00-I99) with ≥100 underlying deaths"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

ggsave("outputs/figures/fig10c_cardiovascular_sex_burden.png", p10c,
       width = 13, height = 8, dpi = 300, bg = "white")

# --- Save summary table ------------------------------------------------------
sex_summary <- sex_burden %>%
  filter(underlying_persons >= 50) %>%
  select(cause_name, icd10_code, is_chapter, underlying_persons,
         underlying_male, underlying_female,
         multiple_male, multiple_female,
         ratio_male, ratio_female, ratio_diff, more_hidden_in) %>%
  arrange(desc(abs(ratio_diff)))

write_csv(sex_summary, "outputs/sex_stratified_hidden_burden.csv")

cat("\n✓ Script 10 complete.\n")
cat("  Figures saved: fig10a, fig10b, fig10c\n")
cat("  Data saved: outputs/sex_stratified_hidden_burden.csv\n")
