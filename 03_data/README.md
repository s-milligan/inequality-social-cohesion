# Data

This directory contains the local data workflow for the Inequality and cohesion project.

The public repository does not redistribute the underlying survey or contextual datasets. Instead, it documents the sources, versions, data-handling rules and scripts required to reproduce the analytical workflow from locally available source files.

## Folder structure

```text
03_data/
├── raw/
│   ├── ess/
│   ├── inequality/
│   │   ├── oecd/
│   │   └── swiid/
│   └── vdem/
├── interim/
├── processed/
└── metadata_codebooks/
```

## Repository policy

The full data structure is used locally, but the underlying datasets are not tracked in Git.

By default:

- `raw/` is local only;
- `interim/` is local only;
- `processed/` is local only;
- metadata and documentation may be tracked where useful;
- restricted, licensed, copyrighted, sensitive or personally identifying data must never be committed;
- generated data products are included in the public repository only where they have a clear reproducibility or dissemination purpose.

The public repository contains the code needed to recreate intermediate and processed files from locally available source data.

# Current data sources

## European Social Survey

**Source:** European Social Survey  
**Rounds:** 1–11  
**Baseline analytical period:** Rounds 1–9  
**Extended analytical period:** Rounds 1–11  
**Format used locally:** Stata `.dta` extracts  
**Local location:** `03_data/raw/ess/`

The ESS provides the respondent-level survey data used in the analysis.

The Rounds 1–9 baseline is retained as a separate analytical dataset. Rounds 10 and 11 are harmonised and appended in a separate extended workflow so that the original baseline remains reproducible.

The ESS data contain variables required for:

- respondent and survey identification
- country and ESS round
- interview timing
- interview mode where available
- generalised social trust
- fairness and helpfulness
- age and gender
- education
- labour-force status
- household economic position
- migration and background indicators
- survey weights
- political variables used in later extensions

The primary outcome is:

- `ppltrst`: generalised social trust, measured on a 0–10 scale.

Related items retained for secondary or robustness analyses include:

- `pplfair`
- `pplhlp`

ESS fieldwork frequently spans more than one calendar year. Actual interview-year information is therefore used to construct country-round contextual exposures rather than assigning each ESS round a single nominal year.

The baseline Rounds 1–9 workflow uses the explicit local extract:

```text
ESS1e06_7-ESS2e03_6-ESS3e03_7-ESS4e04_6-
ESS5e03_6-ESS6e02_7-ESS7e02_3-ESS8e02_3-
ESS9e03_3-subset.dta
```

This explicit filename is used so that the baseline scripts do not accidentally import separate Rounds 10 or 11 `.dta` files stored in the same local directory.

Raw ESS files are not redistributed through this repository.

## Standardized World Income Inequality Database

**Source:** Standardized World Income Inequality Database (SWIID)  
**Version:** 9.92  
**Release:** April 2026  
**Primary variable:** disposable-income Gini (`gini_disp`)  
**Local location:** `03_data/raw/inequality/swiid/`

SWIID is the primary inequality source for the current analysis.

It was selected because the one-year-lagged disposable-income Gini provides complete coverage of all usable ESS country × interview-year cells in the original Rounds 1–9 coverage assessment.

The primary local R-format file is:

```text
swiid9_92.rda
```

This file contains both:

- `swiid_summary`, used for coverage checks and the original summary-series country-round exposure construction;
- `swiid`, containing 100 imputed or simulated inequality datasets used to propagate SWIID measurement uncertainty.

The summary-series estimates are used in the currently reported main models.

The uncertainty input has been audited. The R-format file contains the expected 100 imputations, and the imputation data and `swiid_summary` contain the same 6,628 country-years.

The `gini_disp` values in the imputation objects are converted to conventional Gini-point units by dividing by 100.

A dedicated uncertainty workflow reconstructs the lagged country-round inequality exposure separately within each SWIID imputation and then recalculates the within-country and between-country Gini components before fitting the models.

This workflow has been validated on the first three imputations for Rounds 1–9. The full 100-imputation analysis remains outstanding.

## OECD Income Distribution Database

**Source:** OECD Income Distribution Database (IDD)  
**Primary measure:** disposable-income Gini  
**Local location:** `03_data/raw/inequality/oecd/`

OECD IDD is retained as the principal alternative inequality source.

For the original ESS Rounds 1–9 coverage assessment, exact one-year-lagged OECD Gini coverage was substantially less complete than SWIID coverage and was geographically uneven.

The OECD series is therefore not used as the primary exposure, but it provides an important robustness comparison based more closely on directly observed national inequality statistics.

Any SWIID–OECD comparison must distinguish differences in inequality measurement from differences in the country-round sample generated by incomplete OECD coverage.

## V-Dem

**Source:** Varieties of Democracy  
**Dataset:** Country-Year Full+Others  
**Version:** 16  
**Local location:** `03_data/raw/vdem/`

The current local file is:

```text
V-Dem-CY-Full+Others-v16.csv
```

V-Dem provides the country-year political-context measures used in the political-discourse extension.

The principal current indicator is:

- `v2smpolhate`: major political parties' use of rhetoric intended to insult, offend or intimidate social groups.

The interval-scale estimate is multiplied by −1 in the constructed exposure so that higher values indicate more party hate speech.

Additional indicators retained for conceptual comparison include:

- `v2dlcountr`;
- `v2cacamps`.

Both contemporaneous and one-year-lagged country-round discourse exposures are constructed using the same ESS interview-year weighting principle used for inequality.

V-Dem uncertainty is not yet propagated through the substantive models.

# Contextual matching

The contextual survey unit is the **country-round**.

Annual contextual values are matched to actual ESS interview timing.

For country-rounds whose fieldwork spans more than one calendar year, annual contextual values are aggregated using the proportion of respondents with known interview years in each year.

For example, if:

```text
80% of dated respondents were interviewed in 2016
20% of dated respondents were interviewed in 2017
```

then the one-year-lagged inequality exposure is:

$$
0.80 × Gini(2015) + 0.20 × Gini(2016)
$$

The same timing logic is used for V-Dem country-year exposures.

The primary inequality specification uses a one-year lag.

The primary party hate-speech specification also uses a one-year lag. A contemporaneous hate-speech specification has been constructed and analysed as a sensitivity check.

Contemporaneous inequality remains a planned robustness specification.

## Missing annual components

A country-round contextual exposure is constructed only when all required annual components are available.

The workflow does not:

- interpolate missing annual observations;
- extrapolate beyond available source-series endpoints;
- renormalise interview-year weights over the subset of available contextual years.

This prevents a country-round spanning several interview years from being represented by only the subset for which contextual data happen to exist.

## Incomplete interview-year information

Interview-year shares are calculated among respondents with known interview years.

Respondents without an interview year remain in the respondent-level data and may receive their country-round contextual exposure where sufficient timing information exists for that country-round.

This assumes that the dated interviews adequately represent the timing of the whole country-round.

Several country-rounds have partial interview-year missingness. These remain a timing sensitivity issue where missingness is substantial.

Estonia R5 is different: no usable interview-year information is available in the relevant raw ESS timing variables for its 1,793 respondents. It therefore remains in the underlying ESS respondent dataset but does not receive a constructed contextual exposure.

# Contextual-data exclusions

The contextual-data audit reproduces the saved analytical samples exactly.

For the Rounds 1–9 macro-complete sample, two country-rounds are excluded:

| Country | Round | Respondents | Reason |
|---|---:|---:|---|
| Estonia | 5 | 1,793 | No usable ESS interview-year information |
| Kosovo | 6 | 1,295 | Required unemployment value unavailable |

These exclusions account for 3,088 respondents.

For the extended Rounds 1–11 contextual sample, seven country-rounds are excluded:

| Country | Round | Respondents | Main limitation |
|---|---:|---:|---|
| Estonia | 5 | 1,793 | ESS interview timing |
| Kosovo | 6 | 1,295 | Unemployment |
| Israel | 11 | 906 | SWIID series endpoint |
| Iceland | 10 | 903 | SWIID series endpoint |
| Iceland | 11 | 842 | SWIID series endpoint |
| Montenegro | 11 | 1,609 | SWIID series endpoint |
| Ukraine | 11 | 2,661 | SWIID and unemployment series endpoints |

These exclusions account for 10,009 respondents.

The exclusions reflect identifiable source-data or timing limitations rather than unexplained row loss during data processing.

# Survey weights

Relevant ESS survey-weight variables are retained in the processed data.

The harmonised `analysis_weight` uses the supplied `anweight` where available and otherwise reconstructs the equivalent weight from:

$$
pspwght × pweight
$$

The weight-construction audit found:

- valid `analysis_weight` values for all 518,597 respondents in the Rounds 1–11 processed dataset;
- exact agreement between the processed `analysis_weight` and the expected value from the raw ESS variables;
- only negligible floating-point differences between supplied `anweight` and reconstructed `pspwght × pweight`.

The components have different substantive implications:

- `pspwght` primarily adjusts respondent composition within national samples;
- `pweight` changes the relative influence of countries according to population size.

The current substantive REWB models remain unweighted.

The remaining weighting issue is therefore not data construction but the analytical estimand and the appropriate implementation of multilevel survey weighting.

# Interim data

The `interim/` directory contains generated coverage, diagnostic and merge files.

Examples include:

```text
ess_country_round_year_coverage.csv
ess_country_round_coverage.csv
ess_inequality_coverage.csv
ess_country_round_inequality.csv

ess_post9_country_round_year_coverage.csv
ess_post9_context.csv

ess_country_round_macro.csv
ess_country_round_macro_r1_r11.csv
ess_country_round_context_r1_r11.csv

ess_country_round_discourse_r1_r11.csv

inequality_coverage_summary.csv
inequality_coverage_by_country.csv

ess_analysis_missingness.csv
ess_country_round_trust.csv
```

These files are generated by scripts under `04_code/`.

They are not tracked in Git because they can be recreated from the documented source data and code.

# Processed data

The `processed/` directory contains analysis-ready respondent-level datasets generated from the raw and interim data.

The Rounds 1–9 baseline file is:

```text
ess_analysis.rds
```

It contains cleaned ESS respondent-level variables merged with the baseline country-round inequality exposure.

It is generated by:

```text
04_code/01_import-clean/04_clean_ess_analysis.R
```

The extended Rounds 1–11 file is:

```text
ess_analysis_r1_r11.rds
```

It preserves the Rounds 1–9 baseline observations while appending harmonised Rounds 10 and 11 respondents and retaining interview-mode information.

It is generated by:

```text
04_code/01_import-clean/08_build_ess_extended_analysis.R
```

The existence of the extended file does not replace the Rounds 1–9 baseline. Both are retained because the analysis explicitly compares the original pre-mode-change period with the extended ESS period.

Processed data remain local and are not redistributed through the public repository.

# Data-preparation workflow

The core inequality and ESS workflow is:

```text
ESS R1–R9 raw extract
    ↓
country × round × interview-year coverage
    ↓
SWIID and OECD coverage
    ↓
country-round inequality exposure construction
    ↓
ESS variable cleaning and recoding
    ↓
R1–R9 analysis dataset
```

The baseline scripts are:

```text
04_code/01_import-clean/
├── 01_ess_coverage.R
├── 02_inequality_coverage.R
├── 03_construct_inequality_exposure.R
└── 04_clean_ess_analysis.R
```

The extended workflow adds Rounds 10 and 11:

```text
ESS R10/R11 raw extracts
    ↓
post-R9 interview timing
    ↓
post-R9 inequality and macro context
    ↓
harmonisation with R1–R9 baseline
    ↓
R1–R11 analysis dataset
```

Relevant scripts include:

```text
04_code/01_import-clean/
├── 07_construct_post9_context.R
└── 08_build_ess_extended_analysis.R
```

The political-context workflow uses the extended ESS timing structure to construct country-round V-Dem exposures:

```text
V-Dem country-year data
    +
ESS interview-year timing
    ↓
contemporaneous and lagged country-round discourse exposures
    ↓
R1–R9 and R1–R11 moderation-model context
```

The principal construction and audit script is:

```text
04_code/02_descriptives/04_discourse_indicator_audit.R
```

Additional audit scripts document consequential data decisions:

```text
04_code/02_descriptives/
├── 06_party_hate_sample_audit.R
├── 07_gini_variation_audit.R
├── 08_context_source_audit.R
├── 09_weight_audit.R
└── 10_swiid_uncertainty_audit.R
```

SWIID uncertainty is propagated in the modelling stage rather than by replacing the summary-series processed dataset:

```text
04_code/03_models/10_swiid_uncertainty.R
```

# Data handling principles

1. Raw source data remain unchanged.
2. All cleaning and transformations are scripted.
3. Intermediate and processed data should be reproducible from the documented source files.
4. Variable-construction decisions are documented in code and research-design files.
5. Contextual timing is based on actual interview timing rather than nominal ESS round year wherever usable timing information exists.
6. Country-round contextual values are not interpolated, extrapolated or renormalised over missing annual components unless a future sensitivity analysis explicitly introduces and documents such a rule.
7. Rounds 1–9 remain reproducible as a distinct baseline rather than being overwritten by the Rounds 1–11 extension.
8. Survey-weight variables are retained even when substantive models are unweighted.
9. Raw, licensed or restricted data are not redistributed unless redistribution is explicitly permitted.
10. Only data products with a clear reproducibility or dissemination purpose should be considered for inclusion in the public repository.

# Known data limitations

## Estonia, ESS Round 5

Usable interview-year information is unavailable in the timing variables used for Estonia R5.

The 1,793 respondents remain in the individual-level ESS data, but no timing-weighted country-round contextual exposure is assigned.

The current analytical rule is therefore to exclude Estonia R5 from analyses requiring those contextual exposures rather than assigning an assumed nominal interview year.

If reliable country-specific fieldwork information becomes available, this treatment can be revisited as a sensitivity analysis.

## Contextual source endpoints

Several late ESS country-rounds require lagged inequality or macroeconomic values beyond the available endpoints of the corresponding local source series.

These include:

- Israel R11;
- Iceland R10;
- Iceland R11;
- Montenegro R11;
- Ukraine R11.

These country-rounds are not filled by extrapolation.

## Kosovo R6

Kosovo R6 has the required lagged SWIID inequality value but no finite unemployment observation in the local macroeconomic source used by the current workflow.

It is therefore excluded from analyses requiring the complete macroeconomic adjustment set.

## OECD coverage

OECD inequality coverage is incomplete for a substantial number of ESS country-year observations and is geographically uneven.

This is the main reason SWIID remains the primary inequality source.

OECD IDD is retained as a robustness source rather than used to define the primary analytical sample.

## Later-round survey mode

Rounds 10 and 11 contain both in-person and video interviews in the imported data.

Interview mode is retained explicitly in the extended dataset.

A completed in-person-only sensitivity analysis retains all 38 countries and 273 country-rounds and produces substantively very similar estimates to the all-mode Rounds 1–11 analysis.

This does not establish that survey mode is irrelevant in general; it documents the sensitivity of the current fitted models.

# Reproducibility

Scripts use project-relative paths via the `here` package.

Authorised users must obtain the required source datasets independently and place them in the expected local directories before running the workflow.

The numerical script prefixes indicate the approximate sequence within each code folder, but the project should not be interpreted as a single unconditional pipeline in which every numbered script must always be run from beginning to end. Some scripts construct alternative datasets, audits or later extensions.

The Rounds 1–9 baseline and Rounds 1–11 extended workflow should remain separately reproducible.

Software and package-version management will be documented more formally as the analysis stabilises.