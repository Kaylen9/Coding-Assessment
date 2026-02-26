# Coding-Assessment

This repository contains solutions for the Analytical Data Science Programmer Coding Assessment.

The objective of this assessment was to demonstrate proficiency in:

- SDTM dataset creation using `{sdtm.oak}`
- ADaM dataset derivation using `{admiral}`
- Regulatory-style TLG (Tables, Listings, and Graphs) generation
- Reproducible, well-documented, and clean R programming practices

All scripts were executed in a clean R session, and log files are included as evidence of error-free execution.

---

# Repository Structure

## question_1_sdtm/

**Objective:** Create SDTM DS domain using `{sdtm.oak}`.

Contents:
- `01_create_ds_domain.R` — Script to generate the DS domain
- `ds.csv` — Output SDTM dataset
- `01_create_ds_domain.log` — Log file confirming error-free execution
---

## question_2_adam/

**Objective:** Create ADaM ADSL dataset using `{admiral}`.

Contents:
- `create_adsl.R` — Script to generate ADSL dataset
- `adsl.csv` — Output ADaM dataset
- `create_adsl.log` — Log file confirming error-free execution

---

## question_3_tlg/

**Objective:** Generate regulatory-style AE summary outputs.

Contents:
- `01_create_ae_summary_table.R` — Script for TEAE summary table
- `02_create_visualizations.R` — Script for AE visualizations
- `ae_summary_table.html` — FDA-style AE table output
- `ae_severity_plot.png` — AE severity distribution plot
- `top10_ae_ci.png` — Top 10 AEs with 95% Clopper-Pearson CIs
- Log files for both scripts confirming error-free execution

---

# Reproducibility

Environment:
- R version ≥ 4.2.0
- Required packages:
  - admiral
  - sdtm.oak
  - gtsummary
  - ggplot2
  - dplyr
  - gt

Each script:
- Was executed in a clean R session
- Includes logging via `sink()` for traceability
- Produces reproducible outputs
---

Thank you for reviewing this submission.