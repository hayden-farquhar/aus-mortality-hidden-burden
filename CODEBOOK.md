# Codebook: Data Dictionary

**Author:** Hayden Farquhar, MBBS MPHTM | **ORCID:** [0009-0002-6226-440X](https://orcid.org/0009-0002-6226-440X)
**Repository:** [github.com/hayden-farquhar/aus-mortality-hidden-burden](https://github.com/hayden-farquhar/aus-mortality-hidden-burden)

All CSV files use UTF-8 encoding with comma delimiters and standard R/tidyverse quoting conventions.

---

## Top-Level Data (`data/`)

### deaths_underlying_vs_multiple.csv
Core multiple-to-underlying ratio table for all ICD-10 conditions, 2023.

| Column | Type | Description |
|--------|------|-------------|
| cause_icd10 | character | ICD-10 condition name with code in parentheses |
| unit | character | Unit of measurement (always "no.") |
| underlying_male | numeric | Male underlying cause deaths |
| underlying_female | numeric | Female underlying cause deaths |
| underlying_persons | numeric | Total underlying cause deaths |
| multiple_male | numeric | Male multiple cause mentions |
| multiple_female | numeric | Female multiple cause mentions |
| multiple_persons | numeric | Total multiple cause mentions |
| extra_1 | numeric | Average number of causes per death certificate |
| icd10_code | character | ICD-10 code (e.g., "I10-I15"), NA for totals |
| cause_name | character | Cleaned condition name |
| is_chapter | logical | TRUE if row is an ICD-10 chapter-level aggregate |
| is_total | logical | TRUE if row is "Total deaths" |
| ratio_persons | numeric | Ratio for persons (multiple/underlying) |
| ratio_male | numeric | Ratio for males |
| ratio_female | numeric | Ratio for females |

### deaths_underlying_by_state.csv
State-level underlying cause of death data with counts, crude rates, and age-standardised rates.

| Column | Type | Description |
|--------|------|-------------|
| cause_icd10 | character | ICD-10 condition name with code |
| n_male | numeric | Male death count |
| n_female | numeric | Female death count |
| n_persons | numeric | Total death count |
| crude_rate_male | numeric | Male crude rate per 100,000 |
| crude_rate_female | numeric | Female crude rate per 100,000 |
| crude_rate_persons | numeric | Total crude rate per 100,000 |
| asr_male | numeric | Male age-standardised rate per 100,000 |
| asr_female | numeric | Female age-standardised rate per 100,000 |
| asr_persons | numeric | Total age-standardised rate per 100,000 |
| ypll_male | numeric | Male years of potential life lost |
| ypll_female | numeric | Female years of potential life lost |
| ypll_persons | numeric | Total years of potential life lost |
| state | character | Full state name (e.g., "New South Wales") |
| state_abbr | character | State abbreviation (NSW, VIC, QLD, SA, WA, TAS, NT, ACT, AUS) |
| year | integer | Data year (2024) |
| icd10_code | character | ICD-10 code |
| cause_name | character | Cleaned condition name |
| is_chapter | logical | TRUE if chapter-level aggregate |
| is_total | logical | TRUE if "Total deaths" |

### deaths_multiple_cause_rates.csv
Multiple cause of death counts and age-standardised rates by sex.

| Column | Type | Description |
|--------|------|-------------|
| cause_icd10 | character | ICD-10 condition name with code |
| unit | character | Unit ("no.") |
| mc_count_male | numeric | Male multiple cause count |
| mc_count_female | numeric | Female multiple cause count |
| mc_count_persons | numeric | Total multiple cause count |
| mc_asr_male | numeric | Male multiple cause ASR per 100,000 |
| mc_asr_female | numeric | Female multiple cause ASR per 100,000 |
| mc_asr_persons | numeric | Total multiple cause ASR per 100,000 |
| icd10_code | character | ICD-10 code |
| cause_name | character | Cleaned condition name |
| is_chapter | logical | Chapter-level flag |
| is_total | logical | Total row flag |

### deaths_by_num_causes.csv
Death certificate complexity — how often each condition is reported alone vs with other causes.

| Column | Type | Description |
|--------|------|-------------|
| cause_icd10 | character | ICD-10 condition name with code |
| unit | character | Unit ("no.") |
| reported_alone | numeric | Deaths where this was the only cause listed |
| with_1_other | numeric | Deaths with exactly 1 other cause |
| with_2_other | numeric | Deaths with exactly 2 other causes |
| with_3_other | numeric | Deaths with exactly 3 other causes |
| with_4_other | numeric | Deaths with 4 other causes |
| with_5_plus | numeric | Deaths with 5+ other causes |
| icd10_code | character | ICD-10 code |
| cause_name | character | Cleaned condition name |
| is_chapter | logical | Chapter-level flag |
| is_total | logical | Total row flag |

### domain_summary.csv
Hospital-mortality domain concordance — linking hospital activity to mortality by clinical domain.

| Column | Type | Description |
|--------|------|-------------|
| clinical_domain | character | Clinical domain name (e.g., "Nervous system") |
| icd10_chapter_pattern | character | ICD-10 code pattern for matching |
| achi_chapter_pattern | character | ACHI procedure chapter pattern |
| diag_seps | numeric | Hospital diagnosis separations |
| proc_seps | numeric | Hospital procedure separations |
| underlying_deaths | numeric | Underlying cause deaths |

### concordance_proc_diag_mort.csv
Mapping between ICD-10 diagnosis chapters, ACHI procedure chapters, and clinical domains.

| Column | Type | Description |
|--------|------|-------------|
| clinical_domain | character | Clinical domain name |
| icd10_chapter_pattern | character | ICD-10 chapter code pattern |
| achi_chapter_pattern | character | ACHI chapter pattern |

### hospital_procedures_by_year.csv
Annual hospital procedure separations by ACHI chapter, 2017-18 to 2023-24.

| Column | Type | Description |
|--------|------|-------------|
| year | character | Financial year (e.g., "2017-18") |
| proc_chapter | character | ACHI procedure chapter name |
| total_separations | numeric | Total hospital separations |

### hospital_procedures_by_year_sex.csv
Annual hospital procedure separations by ACHI chapter and sex.

| Column | Type | Description |
|--------|------|-------------|
| year | character | Financial year |
| proc_chapter | character | ACHI procedure chapter name |
| sex | character | Sex category ("1- Male", "2- Female", "3- Persons") |
| total_separations | numeric | Total hospital separations |

### hospital_diagnoses_by_year.csv
Annual hospital diagnosis separations by ICD-10-AM chapter, 2017-18 to 2023-24.

| Column | Type | Description |
|--------|------|-------------|
| year | character | Financial year |
| diag_chapter | character | ICD-10-AM diagnosis chapter name |
| total_separations | numeric | Total hospital separations |
| total_patient_days | numeric | Total patient days |

### multimorbidity_complexity.csv
Death certificate complexity metrics by condition.

| Column | Type | Description |
|--------|------|-------------|
| cause_name | character | Condition name |
| icd10_code | character | ICD-10 code |
| is_chapter | logical | Chapter-level flag |
| total_mentions | numeric | Total multiple cause mentions |
| reported_alone | numeric | Times reported as sole cause |
| pct_alone | numeric | Percentage reported alone |
| pct_with_3plus | numeric | Percentage reported with 3+ other causes |
| avg_co_causes | numeric | Average number of co-reported causes |

### sex_stratified_hidden_burden.csv
Sex-stratified ratio analysis showing which sex has greater hidden burden.

| Column | Type | Description |
|--------|------|-------------|
| cause_name | character | Condition name |
| icd10_code | character | ICD-10 code |
| is_chapter | logical | Chapter-level flag |
| underlying_persons | numeric | Total underlying deaths |
| underlying_male | numeric | Male underlying deaths |
| underlying_female | numeric | Female underlying deaths |
| multiple_male | numeric | Male multiple cause mentions |
| multiple_female | numeric | Female multiple cause mentions |
| ratio_male | numeric | Male ratio |
| ratio_female | numeric | Female ratio |
| ratio_diff | numeric | Absolute difference (male - female ratio) |
| more_hidden_in | character | "Males" or "Females" — sex with higher ratio |

### state_variation_chapters.csv
Geographic coefficient of variation (CV) for ASR by ICD-10 chapter across states.

| Column | Type | Description |
|--------|------|-------------|
| cause_name | character | ICD-10 chapter name |
| icd10_code | character | ICD-10 code range |
| n_states | integer | Number of states with data |
| mean_asr | numeric | Mean ASR across states |
| sd_asr | numeric | Standard deviation of ASR |
| min_asr | numeric | Minimum state ASR |
| max_asr | numeric | Maximum state ASR |
| range_asr | numeric | Range (max - min) |
| cv | numeric | Coefficient of variation (%) |
| min_state | character | State with lowest ASR |
| max_state | character | State with highest ASR |
| total_deaths | numeric | Total national deaths |

### state_variation_specific.csv
Geographic CV for specific ICD-10 conditions (sub-chapter level).

| Column | Type | Description |
|--------|------|-------------|
| cause_name | character | Condition name |
| icd10_code | character | ICD-10 code |
| n_states | integer | Number of states with data |
| mean_asr | numeric | Mean ASR across states |
| sd_asr | numeric | Standard deviation of ASR |
| cv | numeric | Coefficient of variation (%) |
| min_state | character | State with lowest ASR |
| max_state | character | State with highest ASR |
| max_min_ratio | numeric | Ratio of highest to lowest state ASR |
| total_deaths | numeric | Total national deaths |

---

## Confirmatory Data (`data/confirmatory/`)

### confirmatory_h1_data.csv
H1 input data: Hypertension and related cardiovascular ratio by sex (2023 cross-section).

| Column | Type | Description |
|--------|------|-------------|
| cause | character | Condition name with ICD-10 code |
| unit | character | Unit ("no.") |
| underlying_male | numeric | Male underlying deaths |
| underlying_female | numeric | Female underlying deaths |
| underlying_persons | numeric | Total underlying deaths |
| multiple_male | numeric | Male multiple cause mentions |
| multiple_female | numeric | Female multiple cause mentions |
| multiple_persons | numeric | Total multiple cause mentions |
| ratio | numeric | ABS-computed ratio |
| mur_male | numeric | Computed male ratio |
| mur_female | numeric | Computed female ratio |
| mur_persons | numeric | Computed persons ratio |
| data_year | integer | Data year (2023) |

### confirmatory_h1a_cv_mur.csv
Full cardiovascular chapter ratio comparison data for H1 context.

| Column | Type | Description |
|--------|------|-------------|
| cause | character | Cardiovascular condition name |
| unit | character | Unit |
| underlying_male–multiple_persons | numeric | Death counts (same as h1_data) |
| ratio | numeric | ABS ratio |
| icd_code | character | ICD-10 code |
| mur_male | numeric | Male ratio |
| mur_female | numeric | Female ratio |
| mur_persons | numeric | Persons ratio |

### confirmatory_h1c_temporal.csv
H1 temporal data: Hypertension underlying deaths 2014-2024 with population-adjusted rates.

| Column | Type | Description |
|--------|------|-------------|
| cause | character | Condition name |
| icd_code | character | ICD-10 code |
| year | integer | Calendar year (2014-2024) |
| sex | character | "male" or "female" |
| deaths | numeric | Underlying cause death count |
| population | numeric | Mid-year population estimate |
| crude_rate_per_100k | numeric | Crude death rate per 100,000 |

### confirmatory_h2_data.csv
H2 input data: Geographic CVs with avoidability classification.

| Column | Type | Description |
|--------|------|-------------|
| cause | character | Condition name |
| icd_code | character | ICD-10 code |
| n_states | integer | Number of states with data |
| mean_rate | numeric | Mean rate across states |
| sd_rate | numeric | Standard deviation of rate |
| cv | numeric | Coefficient of variation (%) |
| min_rate | numeric | Minimum state rate |
| max_rate | numeric | Maximum state rate |
| total_deaths | numeric | Total national deaths |
| rate_type | character | Type of rate used ("crude_rate" or "asr") |
| avoidability | character | "preventable", "treatable", "not_avoidable", or NA |
| avoidable_group | character | Specific avoidable death category |

### confirmatory_h3_data.csv
H3 input data: Mental health ratio by sex (2023 cross-section).

| Column | Type | Description |
|--------|------|-------------|
| cause | character | Mental health condition name |
| unit | character | Unit |
| underlying_male–multiple_persons | numeric | Death counts |
| ratio | numeric | ABS ratio |
| mur_male | numeric | Male ratio |
| mur_female | numeric | Female ratio |
| mur_persons | numeric | Persons ratio |
| icd_code | character | ICD-10 code |
| data_year | integer | Data year (2023) |

### confirmatory_h3c_temporal.csv
H3 temporal data: Mental health underlying deaths 2014-2024 with population-adjusted rates.

Same structure as `confirmatory_h1c_temporal.csv`.

### h1_results.csv
Statistical test results for H1 (hypertension).

| Column | Type | Description |
|--------|------|-------------|
| hypothesis | character | Test identifier (H1a, H1b) |
| test | character | Test name |
| test_type | character | "primary" or "secondary" |
| statistic_name | character | Name of test statistic |
| statistic_value | numeric | Test statistic value |
| p_value_uncorrected | numeric | Uncorrected p-value |
| ci_lower | numeric | Lower confidence interval bound (if applicable) |
| ci_upper | numeric | Upper confidence interval bound |
| ci_method | character | CI method used |
| effect_size_r | numeric | Effect size (r) |
| n | integer | Sample size |
| significant_uncorrected | logical | Significant at alpha=0.05 uncorrected |
| deviation_note | character | Note on any deviation from pre-registration |

### h2_results.csv
Statistical test results for H2 (geographic variation).

| Column | Type | Description |
|--------|------|-------------|
| hypothesis | character | Test identifier (H2a, H2b) |
| test | character | Test name |
| test_type | character | "primary" or "secondary" |
| statistic_name | character | Test statistic name |
| statistic_value | numeric | Test statistic value |
| p_value_uncorrected | numeric | Uncorrected p-value |
| effect_size_name | character | Effect size metric name |
| effect_size | numeric | Effect size value |
| n_group1 | integer | Group 1 sample size |
| n_group2 | integer | Group 2 sample size |
| significant_uncorrected | logical | Significant uncorrected |
| deviation_note | character | Deviation note |

### h3_results.csv
Statistical test results for H3 (mental health). Same structure as `h1_results.csv`.

### holm_bonferroni_results.csv
Combined Holm-Bonferroni correction results across all hypothesis tests.

| Column | Type | Description |
|--------|------|-------------|
| test_id | character | Test identifier |
| hypothesis | character | Parent hypothesis (H1, H2, H3) |
| test_type | character | "primary" or "secondary" |
| description | character | Human-readable test description |
| p_uncorrected | numeric | Uncorrected p-value |
| p_holm_primary | numeric | Holm-corrected p-value (primary tests only) |
| p_holm_all | numeric | Holm-corrected p-value (all tests) |
| significant_uncorrected | logical | Significant at alpha=0.05 uncorrected |
| significant_corrected | logical | Significant after Holm-Bonferroni correction |

---

## Exploratory Data (`data/exploratory/`)

### cube10_full_mur_table.csv
Complete ratio table for all 663+ ICD-10 conditions from ABS Cube 10 (2023).

| Column | Type | Description |
|--------|------|-------------|
| cause | character | ICD-10 condition name with code |
| unit | character | Unit ("no.") |
| underlying_male | numeric | Male underlying deaths |
| underlying_female | numeric | Female underlying deaths |
| underlying_persons | numeric | Total underlying deaths |
| multiple_male | numeric | Male multiple cause mentions |
| multiple_female | numeric | Female multiple cause mentions |
| multiple_persons | numeric | Total multiple cause mentions |
| ratio | numeric | ABS-computed ratio |
| icd_code | character | ICD-10 code |
| mur_male | numeric | Male ratio |
| mur_female | numeric | Female ratio |
| mur_persons | numeric | Persons ratio |

### cube14_temporal_all_causes.csv
Temporal underlying cause death counts by year (2014-2024) from ABS Cube 14.

| Column | Type | Description |
|--------|------|-------------|
| cause | character | ICD-10 condition name |
| icd_code | character | ICD-10 code (NA for totals) |
| year | integer | Calendar year |
| sex | character | "male", "female", or "persons" |
| deaths | numeric | Underlying cause death count |

### pbs_cardiovascular.csv
PBS prescribing data for cardiovascular medications — wide monthly format.

| Column | Type | Description |
|--------|------|-------------|
| Type of script | character | ATC drug group name |
| Age group | character | Age band |
| Value | character | Metric type (e.g., "Benefits per ERP") |
| JAN-2015 through DEC-2025 | numeric | Monthly prescription values (132 columns) |

### hospital_mortality_linkage.csv
Hospital procedure/diagnosis volume linked to ratio by clinical domain.

| Column | Type | Description |
|--------|------|-------------|
| clinical_domain | character | Clinical domain name |
| icd10_chapter_pattern | character | ICD-10 matching pattern |
| achi_chapter_pattern | character | ACHI matching pattern |
| diag_seps | numeric | Diagnosis separations |
| proc_seps | numeric | Procedure separations |
| underlying_deaths | numeric | Underlying cause deaths |
| mur | numeric | Ratio (persons) |
| mur_male | numeric | Male ratio |
| mur_female | numeric | Female ratio |
| procs_per_death | numeric | Procedure separations per underlying death |
| diags_per_death | numeric | Diagnosis separations per underlying death |

### pbs_vs_mortality.csv
PBS prescribing trends vs hypertension mortality over time.

| Column | Type | Description |
|--------|------|-------------|
| cause | character | Condition name |
| icd_code | character | ICD-10 code |
| year | integer | Calendar year |
| deaths | numeric | Underlying cause deaths |
| total_deaths | numeric | Total deaths that year |
| circ_deaths | numeric | Total circulatory deaths |
| htn_pct_of_total | numeric | Hypertension as % of total deaths |
| htn_pct_of_circ | numeric | Hypertension as % of circulatory deaths |
| pbs_agents_acting_on_the_renin-angiotensin_system | numeric | RAS agent prescriptions per ERP |
| pbs_antihypertensives | numeric | Antihypertensive prescriptions per ERP |
| pbs_beta_blocking_agents | numeric | Beta blocker prescriptions per ERP |

### temporal_high_mur.csv
Summary of temporal trends for high-ratio conditions.

| Column | Type | Description |
|--------|------|-------------|
| icd_code | character | ICD-10 code |
| cause | character | Condition name |
| mur_persons | numeric | Cross-sectional ratio |
| n_years | integer | Number of years with data |
| first_year | integer | First year in series |
| last_year | integer | Last year in series |
| deaths_first | numeric | Deaths in first year |
| deaths_last | numeric | Deaths in last year |
| rho | numeric | Spearman correlation with time |
| p_value | numeric | P-value for trend |
| pct_change | numeric | Percentage change first to last year |
| direction | character | "increasing" or "decreasing" |
| condition_name | character | Cleaned condition name |

### temporal_high_mur_series.csv
Year-by-year death counts for high-ratio conditions.

| Column | Type | Description |
|--------|------|-------------|
| cause | character | Condition name |
| icd_code | character | ICD-10 code |
| year | integer | Calendar year |
| sex | character | "persons" |
| deaths | numeric | Underlying cause deaths |
| mur_persons | numeric | Cross-sectional ratio (constant across years) |

### mur_ranking_full.csv
Complete ranking of all conditions by ratio.

| Column | Type | Description |
|--------|------|-------------|
| rank | integer | Rank by ratio (1 = highest) |
| icd_code | character | ICD-10 code |
| condition_name | character | Condition name |
| level | character | "chapter", "block", or "3char" |
| chapter_name | character | Parent ICD-10 chapter |
| underlying_persons | numeric | Underlying cause deaths |
| multiple_persons | numeric | Multiple cause mentions |
| mur_persons | numeric | Persons ratio |
| mur_male | numeric | Male ratio |
| mur_female | numeric | Female ratio |
| hidden_deaths | numeric | Multiple minus underlying |
| pct_hidden | numeric | Percentage of mentions that are hidden |
| sex_ratio | numeric | Male ratio / Female ratio |

---

## Notes

- "np" or NA values indicate data suppressed by the ABS due to small counts (< 5 deaths).
- Ratio = Multiple cause mentions / Underlying cause deaths. A ratio of 1.0 means the condition is only ever the underlying cause.
- ASR = Age-standardised rate per 100,000 population (2001 Australian Standard Population).
- CV = Coefficient of variation (SD/mean * 100), measuring relative geographic dispersion.
- Data year is 2023 for cross-sectional analyses (ABS Cube 10, released 2024).
- Temporal data spans 2014-2024 (ABS Cube 14, year of occurrence).
- Hospital data covers financial years 2017-18 to 2023-24 (AIHW).
