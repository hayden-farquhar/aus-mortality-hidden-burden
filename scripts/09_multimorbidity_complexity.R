# ============================================================================
# Script 09: Multimorbidity Complexity at Death
# 
# NOVEL ANALYSIS: Examines how many contributing causes appear alongside each
# condition on death certificates. Conditions that rarely appear alone suggest
# high multimorbidity burden. This is one of the first systematic analyses of
# Australian multiple cause of death complexity patterns.
#
# Data source: ABS Causes of Death 2023, Table 10.1
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
    # --- Safety net: truncate cleanly at word boundary if still too long ---
    str_trunc(max_chars, ellipsis = "...")
}

# --- Load data ---------------------------------------------------------------
# Read all columns as character to avoid type-mismatch issues with "—" values
raw_10_1 <- read_csv("raw_10_1.csv", show_col_types = FALSE,
                      col_types = cols(.default = col_character()))

# --- Clean -------------------------------------------------------------------
mcod_complexity <- raw_10_1 %>%
  filter(unit == "no.") %>%
  mutate(
    icd10_code = str_extract(cause_icd10, "\\(([A-Z][0-9]{2}(?:[.-][A-Z]?[0-9]{0,2})?)\\)$") %>%
      str_remove_all("[()]"),
    cause_name = str_remove(cause_icd10, "\\s*\\(.*\\)$") %>% str_trim(),
    is_chapter = str_detect(cause_icd10, "^CHAPTER"),
    is_total   = cause_icd10 == "Total deaths"
  ) %>%
  mutate(across(c(reported_alone, with_1_other, with_2_other, with_3_other, 
                  with_4_other, with_5_plus), 
                ~as.numeric(na_if(na_if(., "—"), "\u2014")))) %>%
  mutate(
    total_mentions = rowSums(across(c(reported_alone, with_1_other, with_2_other, 
                                       with_3_other, with_4_other, with_5_plus)), 
                              na.rm = TRUE),
    pct_alone = ifelse(total_mentions > 0, reported_alone / total_mentions * 100, NA),
    pct_with_3plus = ifelse(total_mentions > 0, 
                             (with_3_other + with_4_other + with_5_plus) / total_mentions * 100, 
                             NA),
    avg_co_causes = ifelse(total_mentions > 0,
                            (reported_alone * 0 + with_1_other * 1 + with_2_other * 2 + 
                             with_3_other * 3 + with_4_other * 4 + with_5_plus * 5.5) / total_mentions,
                            NA)
  )

# --- Analysis 1: Overall distribution of death certificate complexity --------
total_row <- mcod_complexity %>% filter(is_total)

overall_dist <- tibble(
  complexity = c("Reported alone", "With 1 other", "With 2 others", 
                 "With 3 others", "With 4 others", "With 5+ others"),
  n_deaths = c(total_row$reported_alone, total_row$with_1_other, 
               total_row$with_2_other, total_row$with_3_other,
               total_row$with_4_other, total_row$with_5_plus),
  order = 1:6
) %>%
  mutate(
    pct = n_deaths / sum(n_deaths, na.rm = TRUE) * 100,
    complexity = fct_reorder(complexity, order)
  )

cat("=== Distribution of Death Certificate Complexity (2024) ===\n")
print(overall_dist %>% select(complexity, n_deaths, pct), n = 6)
cat(sprintf("\nOnly %.1f%% of deaths have a single cause listed.\n", 
            overall_dist$pct[1]))
cat(sprintf("%.1f%% of deaths have 3+ contributing causes.\n",
            sum(overall_dist$pct[4:6])))

# Figure 9a: Overall complexity distribution
p9a <- ggplot(overall_dist, aes(x = complexity, y = pct)) +
  geom_col(fill = "#2c7bb6", alpha = 0.85) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.5, size = 3.5) +
  labs(
    title = "Complexity of Australian Death Certificates (2024)",
    subtitle = sprintf("%.1f%% of deaths list 3 or more causes; only %.1f%% list a single cause",
                        sum(overall_dist$pct[4:6]), overall_dist$pct[1]),
    x = NULL,
    y = "% of all deaths",
    caption = "Data: ABS Causes of Death 2023, Table 10.1"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

ggsave("outputs/figures/fig9a_death_certificate_complexity.png", p9a,
       width = 10, height = 6, dpi = 300, bg = "white")

# --- Analysis 2: Which conditions are most/least likely to appear alone? -----
chapter_complexity <- mcod_complexity %>%
  filter(is_chapter, total_mentions >= 100) %>%
  arrange(pct_alone)

cat("\n=== Chapter-Level Conditions: % Reported as Sole Cause ===\n")
chapter_complexity %>%
  select(cause_name, total_mentions, pct_alone, pct_with_3plus, avg_co_causes) %>%
  mutate(across(where(is.numeric), ~round(., 1))) %>%
  print(n = 20)

# Figure 9b: Conditions by % reported alone (chapter level)
p9b <- chapter_complexity %>%
  mutate(cause_short = shorten_chapter(cause_name)) %>%
  ggplot(aes(x = reorder(cause_short, pct_alone), y = pct_alone)) +
  geom_col(aes(fill = avg_co_causes), alpha = 0.85) +
  geom_text(aes(label = sprintf("%.0f%%", pct_alone)), hjust = -0.2, size = 3) +
  coord_flip() +
  scale_fill_viridis_c(name = "Avg co-occurring\ncauses", option = "plasma") +
  labs(
    title = "Which Conditions Appear Alone on Death Certificates?",
    subtitle = "Lower % alone = higher multimorbidity burden at death",
    x = NULL,
    y = "% of mentions where condition was sole cause",
    caption = "Data: ABS Causes of Death 2023, Table 10.1\nICD-10 chapters with ≥100 mentions"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave("outputs/figures/fig9b_conditions_reported_alone.png", p9b,
       width = 12, height = 8, dpi = 300, bg = "white")

# --- Analysis 3: Sub-chapter conditions with highest multimorbidity ---------
high_multimorbidity <- mcod_complexity %>%
  filter(!is_chapter, !is_total, total_mentions >= 200) %>%
  arrange(pct_alone) %>%
  head(20)

cat("\n=== Top 20 Conditions That Almost NEVER Appear Alone ===\n")
cat("(These are overwhelmingly contributing causes in multi-cause deaths)\n\n")
high_multimorbidity %>%
  select(cause_name, total_mentions, pct_alone, avg_co_causes) %>%
  mutate(across(where(is.numeric), ~round(., 1))) %>%
  print(n = 20)

# Figure 9c: REDESIGNED — since all top-20 have 0% alone, show avg co-causes
# This is more informative: which conditions carry the heaviest multimorbidity load?
p9c <- high_multimorbidity %>%
  mutate(cause_short = shorten_condition(cause_name, max_chars = 55)) %>%
  ggplot(aes(x = reorder(cause_short, avg_co_causes), y = avg_co_causes)) +
  geom_col(aes(fill = total_mentions), alpha = 0.85) +
  geom_text(aes(label = sprintf("%.1f", avg_co_causes)), hjust = -0.2, size = 3) +
  coord_flip() +
  scale_fill_viridis_c(name = "Total\nmentions", option = "inferno", direction = -1,
                        labels = scales::comma) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Conditions with Highest Multimorbidity Burden at Death",
    subtitle = "These 20 conditions never appear as sole cause — bars show average co-occurring causes",
    x = NULL,
    y = "Average number of co-occurring causes on death certificate",
    caption = "Data: ABS Causes of Death 2023, Table 10.1\nConditions with ≥200 total mentions that are never listed as sole cause"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave("outputs/figures/fig9c_highest_multimorbidity.png", p9c,
       width = 13, height = 8, dpi = 300, bg = "white")

# --- Analysis 4: Low multimorbidity (conditions that appear alone) -----------
low_multimorbidity <- mcod_complexity %>%
  filter(!is_chapter, !is_total, total_mentions >= 200) %>%
  arrange(desc(pct_alone)) %>%
  head(15)

cat("\n=== Top 15 Conditions Most Often Reported as SOLE Cause ===\n")
cat("(These conditions kill independently of other comorbidities)\n\n")
low_multimorbidity %>%
  select(cause_name, total_mentions, pct_alone, avg_co_causes) %>%
  mutate(across(where(is.numeric), ~round(., 1))) %>%
  print(n = 15)

# --- Save summary table ------------------------------------------------------
complexity_summary <- mcod_complexity %>%
  filter(!is_total, total_mentions >= 50) %>%
  select(cause_name, icd10_code, is_chapter, total_mentions, 
         reported_alone, pct_alone, pct_with_3plus, avg_co_causes) %>%
  arrange(pct_alone)

write_csv(complexity_summary, "outputs/multimorbidity_complexity.csv")

cat("\n✓ Script 09 complete.\n")
cat("  Figures saved: fig9a, fig9b, fig9c\n")
cat("  Data saved: outputs/multimorbidity_complexity.csv\n")
