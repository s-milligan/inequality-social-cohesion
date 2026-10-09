# Economic Inequality, Political Discourse, and Generalised Trust in Europe

## Project overview

This repository contains the computational work and methodological documentation for a research project examining economic inequality, political discourse and generalised trust in Europe. Social cohesion provides the broader motivation; the current analysis focuses specifically on generalised trust.

The core research question is:

> To what extent is economic inequality associated with lower generalised trust across European countries and over time?

The moderation question is:

> Is the within-country inequality–trust association more negative in country-periods characterised by more divisive political discourse?

The current empirical analysis operationalises one dimension of political discourse: political parties’ use of group-directed hate speech.

## Current status

The project analyses economic inequality, political discourse and generalised trust using European Social Survey (ESS) Rounds 1–11, SWIID income inequality estimates and V-Dem political-context indicators.

The extended common analytical sample comprises 498,461 respondents from 38 countries and 273 country-rounds. The models use a multilevel within-between (REWB) specification with individual covariates, macroeconomic controls, ESS-round fixed effects, and country and country-round random intercepts.

**Completed analytical work includes:**

- Baseline inequality–trust models and moderation analyses.
- SWIID uncertainty assessments using 100 imputations.
- ESS Rounds 10–11 extension and interview-mode sensitivity analyses.
- Country-influence, exposure-timing and contextual-sample checks.
- Comparative analysis of six political-context indicators, including party hate speech, party disinformation, political polarisation and common-good framing.
- Joint models of party hate speech, party disinformation and common-good framing.
- Visualisations of within-country, between-country and jointly adjusted model coefficients.

**Emerging findings:** The conditional within-country association between income inequality and generalised trust is weak. Party disinformation and party hate speech show positive within-country associations with trust in separate models. When both are included, the party-hate coefficient attenuates substantially, while the disinformation association remains more stable. Issue polarisation shows a negative between-country association with trust. Common-good framing has positive point estimates when oriented towards greater emphasis on the common good, but these estimates remain statistically uncertain.

The findings are exploratory and observational. Outstanding priorities include temporal stability, country influence, conservative country-level inference, political-discourse measurement validity and examination of heterogeneity across social groups. Survey weighting and remaining measurement-uncertainty questions also require attention.

The research focuses specifically on generalised trust, with social cohesion providing the broader theoretical motivation.

## Current empirical design

The primary outcome is ESS generalised trust (`ppltrst`), measured on a 0–10 scale.

Income inequality is measured using the disposable-income Gini coefficient from SWIID. OECD Income Distribution Database measures are retained for a planned robustness comparison. SWIID provides broader coverage in the initial source assessment, but some country-rounds are excluded from the fitted samples because required contextual values are unavailable.

Political discourse is operationalised using several V-Dem indicators, including party hate speech, party disinformation, issue polarisation, antagonistic political camps, deliberative discourse and common-good justification. The analysis distinguishes these theoretically related but empirically non-identical dimensions.
The main inequality and hate-speech exposures are lagged by one year. Because ESS fieldwork can span multiple calendar years, annual values are aggregated to country-rounds using respondent shares across known interview years.

The models include:

- separate within-country and between-country components of inequality and hate speech;
- primary moderation involving within-country inequality and within-country hate speech, with the interaction product itself decomposed into within-country and between-country components;
- secondary moderation involving within-country inequality and country-average hate speech;
- individual controls, lagged GDP and unemployment, and ESS-round fixed effects; and
- country and country-round random intercepts.

Models within each analytical period use a common complete-case sample. Current estimation is unweighted and uses contextual point estimates.

Further details, findings and outstanding decisions are documented in the [research design](02_research-design/research_design.md). Its editable source is [`research_design.qmd`](02_research-design/research_design.qmd).

## Preliminary descriptives

The initial exploratory figures illustrate the distinction between cross-national differences and within-country change. They precede the current adjusted moderation models.

![Between-country inequality and trust](docs/figures/inequality_trust_between.png)

![Within-country inequality and trust](docs/figures/inequality_trust_within.png)

These figures are descriptive and should not be interpreted as causal estimates or substitutes for the adjusted models.

## Methodological documentation

Companion Quarto notebooks in `docs/methods/` explain analysis steps, assumptions, diagnostics and interpretation. Executable analysis scripts remain in `04_code/`.

The methods notebooks cover:

- [Cohesion dimensions](docs/methods/03_cohesion_dimensions.qmd): conceptual distinctions and descriptive assessment of candidate trust, social connectedness and participation measures.
- [Inequality exposure construction](docs/methods/03_construct_inequality_exposure.qmd): inequality-source coverage, interview-year matching and construction of country-round exposures.
- [Party hate-speech moderation](docs/methods/06_party_hate_moderation.qmd): sample construction, within–between decomposition, interaction specification and conditional slopes.
- [Party hate-speech sensitivity checks](docs/methods/07_party_hate_sensitivity_checks.qmd): country influence, contextual exclusions, exposure timing, interview mode and available Gini variation.
- [Extended ESS analysis dataset](docs/methods/08_build_ess_extended_analysis.qmd): incorporation of Rounds 10–11, variable harmonisation, interview-mode handling and contextual-data integration.
- [ESS political exposure preparation](docs/methods/09_construct_ess_political_exposure.qmd): preparation of respondent voting and party-closeness information for subsequent party-affiliation analyses.

The party hate-speech notebooks read saved analysis outputs rather than refitting models during rendering. Rendering requires the relevant local inputs or outputs, as documented in each notebook.

## Repository structure

``` text
01_lit/
    evidence_tables/        Literature coding and evidence synthesis

02_research-design/
    research_design.qmd     Editable research design
    research_design.md      Rendered research design
    research_qns/           Research questions and project scope

03_data/
    README.md               Data sources, access and handling documentation

04_code/
    01_import-clean/        Data import, coverage checks, cleaning and merging
    02_descriptives/        Descriptive analysis, audits and figures
    03_models/              Statistical models and sensitivity analyses

docs/
    figures/                Selected project-generated figures
    methods/                Explanatory Quarto methods notebooks

05_output/                  Local generated outputs; not tracked
```
