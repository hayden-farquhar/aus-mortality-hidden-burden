# The Hidden Burden of Disease: A Multiple Cause of Death Analysis of Australian Mortality and Hospital Data

**Author:** Hayden Farquhar, MBBS MPHTM
**Affiliation:** Independent Researcher
**Email:** hayden.farquhar@icloud.com
**ORCID:** [0009-0002-6226-440X](https://orcid.org/0009-0002-6226-440X)
**Repository:** [github.com/hayden-farquhar/aus-mortality-hidden-burden](https://github.com/hayden-farquhar/aus-mortality-hidden-burden)

## Overview

This repository contains all analysis code, derived data, and results for a secondary data analysis of publicly available Australian mortality and hospital statistics. The primary contribution is quantifying the **Multiple-to-Underlying cause Ratio (MUR)** — how many times more often a condition appears anywhere on the death certificate compared to being selected as the underlying cause of death.

A MUR of 1.0 means a condition is only ever the underlying cause (e.g., intentional self-harm). A MUR of 27.7 (hypertension) means the condition appears nearly 28 times more often as *any* cause than as the *underlying* cause, revealing a massive hidden burden invisible to standard mortality statistics.

**Pre-registration:** [OSF](https://doi.org/10.17605/OSF.IO/K46RN)
**Ethics:** Not required (publicly available aggregated data, no individual-level records).
**Code:** https://github.com/hayden-farquhar/aus-mortality-hidden-burden

## Pre-Registered Hypotheses

| ID | Hypothesis | Primary Test | Result |
|----|-----------|-------------|--------|
| H1 | Hypertension MUR differs by sex | Wilcoxon signed-rank | Not supported (p_holm = 0.25) |
| H2 | Avoidable conditions have higher geographic CV | Mann-Whitney U | Not supported (p_holm = 0.87) |
| H3 | Mental health MUR differs by sex, driven by substance use | Wilcoxon signed-rank | Not supported (p_holm = 0.07) |

All results reported regardless of significance, per pre-registration commitment. Multiple testing corrected via Holm-Bonferroni across all tests.

## Data Sources

Raw data are **not included** in this repository as they are publicly available and in some cases very large. Download from:

| Source | Description | URL |
|--------|------------|-----|
| ABS Causes of Death | Cubes 01-15 (underlying, multiple, state, temporal) | https://www.abs.gov.au/statistics/health/causes-death/causes-death-australia |
| AIHW Hospital Procedures | Admitted patient procedure data cubes (2017-18 to 2023-24) | https://www.aihw.gov.au/reports-data/myhospitals/sectors/admitted-patients |
| AIHW Hospital Diagnoses | Principal diagnosis data cubes (2017-18 to 2023-24) | https://www.aihw.gov.au/reports-data/myhospitals/sectors/admitted-patients |
| AIHW Avoidable Deaths | NHA indicator 16 classification and state tables | https://www.aihw.gov.au/reports/life-expectancy-deaths/deaths-in-australia |
| AIHW Burden of Disease | ABDS 2024 update (YLL, YLD, DALY by state/disease) | https://www.aihw.gov.au/reports/burden-of-disease/australian-burden-of-disease-study |
| AIHW Mental Health | Emergency department presentations by state | https://www.aihw.gov.au/reports/mental-health-services/mental-health-services-in-australia |
| PBS Prescribing Statistics | ATC Level 2 prescriptions by month/age | https://www.pbs.gov.au/info/statistics/expenditure-prescriptions |
| ABS Population Estimates | Quarterly ERP by state, sex, age | https://www.abs.gov.au/statistics/people/population/national-state-and-territory-population |

Place downloaded files in the following structure relative to the project root:

```
Confirmatory exploration/Data from sources/
  1A ABS Causes of Death Data/
  1B AIHW Potentially Avoidable Deaths Classification/
  2A PBS Prescribing Statistics/
  2B AIHW Mental Health Services/
  2C AIHW Burden of Disease/
  2E ABS Population Estimates/
  3A AIHW Hospital Procedures Data Cubes/
  3B AIHW Principal Diagnosis Data Cubes/
  3C AIHW Admitted Patient Data/
```

## Software Requirements

- **R** >= 4.2.0
- **RStudio** (recommended, sets working directory via `.Rproj`)
- R packages: `tidyverse`, `readxl`, `scales`, `ggrepel`

Install packages:

```r
install.packages(c("tidyverse", "readxl", "scales", "ggrepel"))
```

## Reproduction Instructions

All scripts assume the working directory is `Analysis/` (set automatically by opening `Analysis/Analysis.Rproj` in RStudio). Run scripts in numerical order:

### Phase 1-2: Data Inspection and Cleaning
| Script | Description | Inputs | Outputs |
|--------|------------|--------|---------|
| `01_inspect_data.R` | Inspect all raw data files, report structure | Raw ABS/AIHW Excel files | `outputs/data_structure_report.txt` |
| `02_clean_abs_data.R` | Clean ABS mortality cubes into tidy CSVs | Raw ABS Excel cubes | Core mortality CSVs in `outputs/` |
| `03_clean_aihw_data.R` | Clean AIHW hospital cubes into tidy CSVs | Raw AIHW Excel cubes | Hospital CSVs in `outputs/` |

### Phase 3: Exploratory Analysis
| Script | Description |
|--------|------------|
| `04_exploratory_analysis.R` | MUR calculations, hospital-mortality concordance, initial figures |
| `04b_figure_fixes.R` | Revised figures with improved formatting |
| `04c_figure_fixes_round2.R` | Final figure revisions |
| `08_fig6_fix.R` | Fix hospital vs mortality scatter plot |
| `09_multimorbidity_complexity.R` | Death certificate complexity analysis |
| `10_sex_stratified_hidden_burden.R` | Sex-stratified MUR analysis |
| `11_state_mortality_profiles.R` | Geographic variation (CV by state) |

### Phase 4: Confirmatory Data Preparation
| Script | Description |
|--------|------------|
| `12_data_prep_confirmatory.R` | Extract all data needed for hypothesis tests |
| `12b_fix_extractions.R` | Structure H1/H3 data, fix PBS extraction |
| `12c_temporal_pbs_ed_cv.R` | Cube 14 temporal data, PBS fix, ED data, full MUR table |
| `12d_fix_issues.R` | Fix H2 rate conversion, population adjustment, avoidability merge |

### Phase 5: Confirmatory Tests
| Script | Description |
|--------|------------|
| `13_confirmatory_H1_hypertension.R` | H1: Hypertension MUR sex difference |
| `14_confirmatory_H2_geographic.R` | H2: Geographic variation by avoidability |
| `15_confirmatory_H3_mental_health.R` | H3: Mental health MUR sex difference |
| `16_holm_bonferroni_summary.R` | Holm-Bonferroni correction across all tests |

### Phase 6: Exploratory Extensions
| Script | Description |
|--------|------------|
| `17_mur_ranking_table.R` | Full MUR ranking for 663 conditions |
| `18_hospital_mortality_linkage.R` | Hospital procedure volume vs MUR linkage |
| `19_pbs_vs_mortality.R` | PBS prescribing vs hypertension mortality trends |
| `20_temporal_high_mur.R` | Temporal trends for high-MUR conditions |
| `21_fig1_mur_concept.R` | Figure 1: Conceptual MUR illustration |
| `22_reviewer_sensitivity_analyses.R` | Sensitivity analyses: eye domain, Poisson CIs, crude rate Spearman |

## File Descriptions

### `data/` — Derived Data
Core mortality tables, hospital data, and analysis outputs. See `CODEBOOK.md` for column-level documentation.

### `data/confirmatory/` — Hypothesis Test Data and Results
Input data and statistical results for the three pre-registered hypotheses plus Holm-Bonferroni correction.

### `data/exploratory/` — Exploratory Analysis Data
Complete MUR rankings, temporal trends, PBS prescribing linkage, and hospital-mortality concordance.

### `results/` — Human-Readable Result Summaries
Text-format summaries of confirmatory and exploratory analyses.

### `figures/` — All Figures
30 publication-quality PNG figures covering exploratory, confirmatory, and supplementary analyses.

## Key Findings

- **Average 3.5 causes per death certificate** (consistent with international literature)
- **Hypertension MUR = 27.7** (41.8 males, 21.2 females) — the largest hidden burden among common conditions
- **663 conditions ranked by MUR** from 1.0 (external causes like self-harm) to 281.1 (surgical complications)
- **71 conditions with MUR > 10** reveal conditions where standard mortality statistics capture < 10% of the true burden
- **No pre-registered hypotheses supported** after Holm-Bonferroni correction, though H3 (mental health) showed marginal significance (p_holm = 0.07) with consistent direction

## License

This work is licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). You are free to share and adapt this material with appropriate attribution.

## Citation

Farquhar, H. (2026). The Hidden Burden of Disease: A Multiple Cause of Death Analysis of Australian Mortality and Hospital Data. [Insert journal/DOI after publication]. Code and data available at https://github.com/hayden-farquhar/aus-mortality-hidden-burden
