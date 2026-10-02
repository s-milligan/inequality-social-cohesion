# Data

This directory supports the local data workflow for the project on economic inequality, political discourse and generalised trust.

The public repository documents data sources, construction procedures and analytical inputs. It does not redistribute the underlying survey or contextual datasets. Raw data, intermediate files and processed respondent datasets remain local.

## Local structure

The following directories contain the principal documented raw inputs:

```text
03_data/
    raw/
        ess/
            post9/
        inequality/
            oecd/
            swiid/
        vdem/
    interim/
    processed/
    metadata_codebooks/
```

Additional source files should retain the locations expected by their import scripts. Generated model results, diagnostics and figures are stored separately under `05_output/`.

## Repository policy

- `raw/`, `interim/` and `processed/` are local data directories and are not tracked by default.
- Metadata and documentation may be tracked where useful and redistribution is permitted.
- Restricted or identifying data must not be committed.
- Selected project-generated figures are published under `docs/figures/`.
- Fitted models, computational checkpoints and routine generated outputs remain local.

Access to the public repository alone does not provide every input required to reproduce the analysis.

## Data sources

### European Social Survey

**Source:** European Social Survey (ESS)  
**Coverage used:** Rounds 1–11, with Rounds 1–9 retained as the baseline comparison  
**Local input format:** Stata `.dta` files  
**Local source directory:** `03_data/raw/ess/`

The ESS supplies respondent identifiers, generalised trust, individual covariates, interview timing, mode information and survey weights. Additional extracts support party-affiliation preparation and descriptive assessment of alternative outcomes.

The primary outcome is `ppltrst`, measured from 0 to 10. Related measures include `pplfair` and `pplhlp`; their potential use in a composite remains subject to conceptual and measurement checks. Supplementary variables are not necessarily harmonised or available in every processed file.

The later-round inputs used by the extension script are:

```text
03_data/raw/ess/post9/ess10_f2f.dta
03_data/raw/ess/post9/ess11_f2f.dta
```

Despite the filenames, these imported inputs include video interviews as well as in-person interviews. The preparation workflow preserves mode information. The main models retain eligible respondents across imported modes; an in-person sensitivity analysis has also been completed.

For Rounds 1–9, the extended dataset assigns in-person mode based on the baseline survey-design assumption. This is distinct from verifying mode individually for every respondent.

The extended dataset retains `analysis_weight`, but the current models are unweighted. For later rounds, the preparation script uses a positive, non-missing `anweight` where available, otherwise a positive, non-missing product of `pspwght` and `pweight`. Separate weight components may need to be recovered from source files for subsequent weighting work.

Exact ESS file editions and download dates should accompany the local source inventory; round numbers alone do not fully identify a data release.

### ESS political and alternative-outcome supplements

Political variables covering Rounds 1–11 have been prepared to distinguish reported voting from party closeness and to support subsequent party linkage. These records have not yet been used in the substantive moderation models.

A supplementary extract, `ess_rounds1-11_cohesion_migration_variables.dta`, supports preparation and assessment of social connectedness, institutional trust and migration-related variables.

These supplements belong to the broader project. The current party hate-speech models use a national contextual indicator and do not require respondent-level party matching.

### Standardized World Income Inequality Database

**Source:** Standardized World Income Inequality Database (SWIID)  
**Version recorded for this analysis:** 9.92  
**Release recorded:** April 2026  
**Primary measure:** Disposable-income Gini (`gini_disp`)  
**Local source directory:** `03_data/raw/inequality/swiid/`

SWIID is the primary inequality source. The initial Rounds 1–9 assessment found one-year-lagged coverage for all 374 country × interview-year cells with known interview year. This historical coverage finding does not imply complete coverage of the extended Rounds 1–11 sample.

The summary series is used to construct the contextual point estimates in the current models. Some extended country-rounds lack the required Gini exposure, as documented below.

The 100 SWIID imputations have been loaded in earlier project work. Construction, fitting and pooling across those imputations remain outstanding; current models do not propagate SWIID measurement uncertainty.

### OECD Income Distribution Database

**Source:** OECD Income Distribution Database (IDD)  
**Measure:** Disposable-income Gini  
**Local source directory:** `03_data/raw/inequality/oecd/`

OECD IDD is retained for a planned alternative-source inequality analysis.

In the original Rounds 1–9 coverage assessment, exact one-year-lagged OECD values were available for 274 of 374 country × interview-year cells, representing approximately 71% of respondents. Coverage is geographically uneven.

Coverage assessment and exposure preparation do not mean the OECD-based model sensitivity analysis has been completed.

### Macroeconomic controls

The models use lagged log GDP per capita at purchasing power parity and lagged unemployment, represented by:

- `log_gdp_pc_ppp_lag1`
- `unemployment_lag1`

The combined modelling input is:

```text
03_data/interim/ess_country_round_macro_r1_r11.csv
```

The exact providers, source-series identifiers, original units and retrieval dates are not specified in this README and remain to be documented from the relevant import and construction steps.

### V-Dem political-discourse indicators

**Source:** V-Dem Country-Year Full+Others dataset  
**Version:** 16  
**Local source file:**

```text
03_data/raw/vdem/V-Dem-CY-Full+Others-v16.csv
```

The discourse audit examined:

| Indicator | Role in the project |
|---|---|
| `v2smpolhate` | Main indicator: political parties’ group-directed hate speech |
| `v2dlcountr` | Alternative contextual indicator concerning respect for counterarguments |
| `v2cacamps` | Distinct contextual indicator of societal political polarisation |

All three indicators have annual coverage for the 39 ESS countries examined over 2000–2025.

The unsuffixed interval-scale `v2smpolhate` estimate is multiplied by −1 so that higher values indicate more hate speech. The constructed variables `party_hate_lag1` and `party_hate_contemp` are already reversed and must not be reversed again during modelling.

Negative values are valid scale locations. Zero does not indicate an absence of hate speech. The current models use point estimates and do not propagate V-Dem measurement uncertainty.

V-Party is a separate planned extension and is not an input to the current national hate-speech models.

## Contextual matching

The contextual survey unit is the country-round. Annual exposures are matched to actual interview timing rather than a single nominal ESS round year.

For example, if 80% of dated interviews occurred in 2016 and 20% in 2017, the lagged inequality exposure is:

```text
0.80 × Gini(2015) + 0.20 × Gini(2016)
```

The principal timing inputs are:

- `ess_inequality_coverage.csv` for the baseline interview-year counts;
- `ess_post9_country_round_year_coverage.csv` for later rounds.

Historical timing information should be obtained from these coverage tables rather than assuming it is fully populated in the extended respondent dataset.

Interview-year shares are calculated among respondents with known dates. Undated respondents remain eligible for analysis and receive their country-round exposure where one can be constructed. This assumes that the dated interviews adequately represent the fieldwork timing of the whole country-round.

The discourse construction requires an observed value for each annual component. Missing annual components are not interpolated or silently omitted by renormalising the available values. Country-rounds without usable timing retain missing exposures.

The main models use lagged Gini, macro controls and hate speech. Contemporaneous hate speech has been examined on the same respondent samples. Contemporaneous inequality remains a separate outstanding model sensitivity check.

## Principal intermediate files

The following are selected inputs and diagnostic products, rather than an exhaustive directory inventory.

| File under `interim/` | Purpose |
|---|---|
| `ess_country_round_coverage.csv` | Baseline country-round coverage |
| `ess_country_round_year_coverage.csv` | Baseline interview-year coverage |
| `ess_inequality_coverage.csv` | Baseline interview-year counts and inequality availability |
| `ess_country_round_inequality.csv` | Constructed baseline inequality exposures |
| `ess_post9_country_round_year_coverage.csv` | Later-round interview-year counts |
| `ess_post9_context.csv` | Later-round contextual inputs |
| `ess_country_round_macro_r1_r11.csv` | Combined GDP and unemployment input |
| `ess_country_round_discourse_r1_r11.csv` | Constructed discourse exposures and timing diagnostics |

The discourse file includes contemporaneous and lagged values for the audited indicators, including the reversed party hate-speech measures. Exposures were constructed for 279 of 280 input country-rounds; Estonia R5 lacks usable interview-year information.

Intermediate files are generated by scripts in both `04_code/01_import-clean/` and `04_code/02_descriptives/`. In particular, the discourse audit script constructs an analytical input as well as producing diagnostics.

## Processed respondent data

| File under `processed/` | Purpose |
|---|---|
| `ess_analysis.rds` | Preserved Rounds 1–9 baseline dataset |
| `ess_analysis_r1_r11.rds` | Extended Rounds 1–11 respondent dataset |

The baseline dataset is generated by `04_code/01_import-clean/04_clean_ess_analysis.R`. The extended dataset is generated by `04_code/01_import-clean/08_build_ess_extended_analysis.R`.

The extended dataset contains cleaned respondent variables, interview-mode information and country-round inequality exposure. The moderation scripts read the macro and discourse files separately and join them by `cntry` and `essround`; those contextual variables should not be assumed to be permanently merged into the processed respondent file.

Model-specific complete-case restrictions are applied during modelling. The processed dataset therefore contains more respondents than the fitted analysis samples.

## Preparation and analysis dependencies

The workflow has several branches rather than one sequence determined solely by filename numbering.

1. Prepare baseline ESS coverage and interview timing, assess inequality availability, construct inequality exposures and clean the baseline respondent data.
2. Prepare the later-round inequality and macro context required by the ESS extension, then construct `ess_analysis_r1_r11.rds` and the combined macro input.
3. Use the baseline and later-round timing tables with V-Dem to construct the combined discourse input.
4. Join respondent, macro and discourse inputs in the moderation script and apply its common-sample restrictions.
5. Run the diagnostic and sensitivity scripts using the saved models, contextual samples and outputs.
6. Render the methods notebooks after their required outputs exist.

Documented entry points include:

| Script | Main purpose |
|---|---|
| `04_code/01_import-clean/01_ess_coverage.R` | Baseline survey coverage |
| `04_code/01_import-clean/02_inequality_coverage.R` | Inequality-source coverage |
| `04_code/01_import-clean/03_construct_inequality_exposure.R` | Baseline inequality exposure |
| `04_code/01_import-clean/04_clean_ess_analysis.R` | Baseline respondent preparation |
| `04_code/01_import-clean/08_build_ess_extended_analysis.R` | Rounds 1–11 respondent extension |
| `04_code/01_import-clean/09_construct_ess_political_exposure.R` | Political-variable preparation |
| `04_code/01_import-clean/10_add_cohesion_migration_variables.R` | Alternative-outcome and migration-variable preparation |
| `04_code/02_descriptives/04_discourse_indicator_audit.R` | V-Dem audit and country-round discourse construction |
| `04_code/03_models/06_party_hate_moderation.R` | Joining analytical inputs and estimating moderation models |

This list identifies principal entry points, not a complete executable build manifest. Consult each script’s input requirements and the accompanying methods documentation for additional contextual-data preparation dependencies.

## Known data limitations

### Contextual exclusions in the current moderation samples

| Country | Round | Respondents before individual exclusions | Missing contextual variables |
|---|---:|---:|---|
| Estonia | 5 | 1,793 | Gini, hate speech, GDP, unemployment |
| Kosovo | 6 | 1,295 | Unemployment |
| Israel | 11 | 906 | Gini |
| Iceland | 10 | 903 | Gini |
| Iceland | 11 | 842 | Gini |
| Montenegro | 11 | 1,609 | Gini |
| Ukraine | 11 | 2,661 | Gini, unemployment |

All these country-rounds have matching rows in the contextual tables. The missing-variable audit reproduces the fitted sample counts and country-round membership.

Estonia R5 remains in the underlying respondent data but is excluded from the current models because no usable interview-year information is available for contextual construction. Recovering timing information remains unresolved.

The upstream causes of the other missing values remain to be investigated. Missingness in a constructed exposure should not automatically be interpreted as absence of the underlying source series.

After contextual and individual exclusions, the moderation samples contain:

| Period | Respondents | Countries | Country-rounds |
|---|---:|---:|---:|
| R1–R9 | 419,618 | 37 | 226 |
| R1–R11 | 498,461 | 38 | 273 |

### Partially missing interview years

Incomplete interview-year information affects 24 country-rounds, including Estonia R5. Substantial partial gaps occur in Spain, Iceland, Latvia and Croatia in R9, and Czechia in R1.

Fieldwork documentation should be checked to assess whether undated interviews could alter the annual exposure weights. These limitations concern both inequality and discourse matching.

### Weighting and exposure uncertainty

Current models do not use ESS survey weights or propagate SWIID and V-Dem measurement uncertainty. These are outstanding methodological tasks, not properties resolved by the existing sample and coverage checks.

## Documentation and reproducibility

Scripts use project-relative paths through the `here` package. Reproduction requires the relevant source data, expected local file locations and prerequisite outputs.

The [research design](../02_research-design/research_design.md) records the current specifications, results and outstanding decisions. Explanatory notebooks are listed in the [main README](../README.md#methodological-documentation).

Raw inputs should remain unchanged. Cleaning, matching and transformations are scripted. Source versions, file editions, retrieval dates and access conditions should be recorded alongside the local source inventory.

The current modelling and diagnostic scripts save session information with their outputs. A fully specified software environment and complete source-provenance inventory remain to be consolidated.