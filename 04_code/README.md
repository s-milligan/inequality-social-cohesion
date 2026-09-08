# Code

This directory contains the reproducible R workflow for the project.

The code is organised by analytical stage rather than by data source.

## Structure

```text
04_code/
├── 01_import-clean/
│   ├── 01_ess_coverage.R
│   ├── 02_inequality_coverage.R
│   ├── 03_construct_inequality_exposure.R
│   └── 04_clean_ess_analysis.R
│
├── 02_descriptives/
│   ├── 01_trust_inequality_descriptives.R
│   └── 02_trends.R
│
└── 03_models/
    └── [model scripts added as analysis develops]
```

## Workflow

### 01_import-clean

The import and cleaning scripts:

1. establish ESS country × round × interview-year coverage;
2. compare OECD IDD and SWIID inequality coverage;
3. construct country-round inequality exposures using actual interview timing;
4. clean the individual-level ESS data and merge contextual inequality measures.

The resulting analytical datasets are stored locally under `03_data/processed/` and are not tracked in Git.

### 02_descriptives

The descriptive scripts examine:

- between-country associations between inequality and social trust;
- within-country deviations in inequality and trust;
- trends in trust and inequality across ESS rounds;
- country-specific trust trajectories.

Routine generated figures are saved locally under `05_output/`.

Selected public-facing figures may be copied to `docs/figures/` and tracked in Git.

### 03_models

This directory will contain the statistical models.

The initial modelling sequence will estimate multilevel within-between models that distinguish:

- persistent differences in inequality between countries;
- deviations from countries' own typical inequality levels over time.

The first models will progressively add:

- within- and between-country inequality;
- ESS round effects;
- individual-level covariates;
- contextual covariates.

Later analyses will assess alternative inequality measures, survey weighting, political-discourse moderation, and other robustness specifications.

## Reproducibility

Scripts use project-relative paths via the `here` package.

Raw data, interim datasets, processed analytical files, and routine generated outputs are excluded from Git. The repository contains the code required to recreate these objects from locally available source data.

Scripts are intended to be run in numerical order within each directory unless otherwise documented.
