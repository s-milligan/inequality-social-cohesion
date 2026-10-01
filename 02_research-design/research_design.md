# Research design


# Scope and research questions

This document specifies the current paper within the wider Inequality
and cohesion project. It incorporates the decision of 22 September 2026
to focus on generalised trust. Social cohesion remains the broader
motivation; the paper does not claim to measure cohesion in its
entirety.

The core research question is:

> To what extent is economic inequality associated with lower
> generalised trust across European countries and over time?

The moderation question is:

> Is the within-country association between economic inequality and
> generalised trust more negative in country-periods characterised by
> more divisive political discourse?

The intended journal is *European Societies*. The literature framing
should engage with Delhey and Newton (2003), Damhuis and Westheuser
(2024), and Hastings (2018). The proposed contribution concerns
political discourse as a conditioning context for the inequality–trust
relationship. Claims about novelty will require a fuller literature
assessment.

# Conceptual scope

Social cohesion concerns the quality and strength of relations among
members of a society and is broader than generalised trust. Generalised
trust concerns expectations of the trustworthiness of other people. It
is the substantive outcome of this paper, rather than a proxy for social
cohesion as a whole.

The earlier design distinguished attitudinal and behavioural dimensions
of horizontal cohesion. That distinction remains useful for the wider
project, but social connectedness and participation are no longer
co-primary outcomes of this paper. Political or institutional trust is
also conceptually distinct and is not a core outcome.

Economic equality, economic security, institutional quality and
political discourse are potential explanatory conditions, not components
of the outcome construct.

# Analytical focus

The paper distinguishes two forms of association:

1.  Between-country differences: Do countries with persistently higher
    inequality have lower generalised trust?
2.  Within-country change: When inequality changes within a country,
    does generalised trust change correspondingly?

The within-country relationship is the primary empirical focus.
Between-country differences remain substantively relevant but are more
vulnerable to confounding by persistent historical, institutional,
cultural and economic differences.

The analysis uses repeated cross-sections and does not follow changes in
trust within the same individuals. It is observational and does not
establish a definitive causal effect of inequality or discourse.

# Hypotheses

## Economic inequality and generalised trust

**H1a: Within-country inequality and trust.** Increases in economic
inequality within countries are associated with decreases in generalised
trust.

**H1b: Between-country inequality and trust.** Countries with
persistently higher economic inequality exhibit lower generalised trust.

## Political discourse

**H2: Political-discourse moderation.** The within-country association
between economic inequality and generalised trust is more negative in
country-periods characterised by more divisive political discourse.

Political discourse may shape how people interpret economic disparities,
social relations and group conflict. This motivates the moderation test
but does not establish the mechanism empirically.

H2 replaces H3 in the earlier two-outcome design. The former
behavioural-cohesion hypotheses are deferred to the wider project.
Existing scripts and output labels should be checked before adopting the
new numbering throughout the repository.

H2 remains conditional on constructing a theoretically defensible and
empirically feasible country-period measure.

# Outcome

The primary outcome is the European Social Survey item `ppltrst`,
measured from 0 (“you can’t be too careful”) to 10 (“most people can be
trusted”). This single item provides the direct operationalisation of
generalised trust.

Two related items are retained for a possible alternative
operationalisation:

- `pplfair`: whether most people would try to take advantage of the
  respondent or try to be fair;
- `pplhlp`: whether people mostly look out for themselves or mostly try
  to be helpful.

A composite trust/fairness/helpfulness scale is a planned sensitivity
analysis, subject to conceptual and measurement checks. It broadens the
measured content beyond trust alone and should be labelled accordingly.

# Main explanatory variable

## Economic inequality

The main contextual exposure is country-level inequality in equivalised
household disposable income after taxes and transfers.

The preferred operationalisation is the Gini coefficient.

The primary inequality measure is the disposable-income Gini coefficient
from the Standardized World Income Inequality Database (SWIID), version
9.92.

SWIID is used as the primary series because it provides complete
country-year coverage for the usable ESS Rounds 1–9 analytical sample.
Coverage checks showed that a one-year-lagged SWIID Gini was available
for all 374 ESS country × interview-year cells with known interview
year.

The OECD Income Distribution Database (IDD) will be used as a robustness
measure. Exact one-year-lagged OECD coverage was available for 274 of
374 ESS country × interview-year cells, representing approximately 71%
of respondents. Missing OECD coverage is geographically uneven and
disproportionately affects several Eastern and Southeastern European
countries.

The use of SWIID therefore preserves the wider European scope of the
analysis, while OECD provides an important robustness check using a more
directly observed inequality series.

Because SWIID values are model-based estimates with associated
uncertainty, the final analysis should propagate that uncertainty rather
than treat summary-series estimates as perfectly observed. The 100
imputations have been loaded according to the project work record;
fitting and pooling the models across them remains outstanding.

# Survey data

## European Social Survey

The analysis uses repeated cross-sectional individual-level ESS data.
Rounds 1–9 provide the baseline comparison, and an extended dataset
incorporates Rounds 10–11. The original baseline file,
`ess_analysis.rds`, is preserved.

The extension script, `08_build_ess_extended_analysis.R`, imports the
later rounds from `ess10_f2f.dta` and `ess11_f2f.dta`, which are
designated as interviewer-administered inputs in the preparation
workflow. It retains all respondents from these files rather than
imposing an additional respondent-level mode restriction.

For Rounds 10–11, the script retains the original interview-mode labels
and constructs a standardised variable distinguishing in-person and
video interviews where recognised. Other labels and missing values are
retained. For Rounds 1–9, the script assigns an in-person classification
based on the baseline survey-design assumption rather than checking
respondent-level mode information.

The exported mode diagnostics identify 35,263 in-person interviews
(93.8%), 2,312 video interviews (6.15%) and 36 interviews with mode
labelled “Not available” (0.10%) in Round 10. Round 11 contains 48,240
in-person interviews (96.3%), 1,846 video interviews (3.68%) and 30
interviews with mode labelled “Not available” (0.06%). These figures
describe the imported respondents before model-specific complete-case
exclusions. No self-administered category appears in these diagnostics.
The current extended models retain all three categories among otherwise
eligible respondents. A planned sensitivity analysis will restrict the
later rounds to confirmed in-person interviews, with attention to the
resulting changes in country-round coverage.

Analytical inclusion depends on available trust, inequality and
individual covariate data, with an additional contextual-data
requirement for the macro-adjusted models. The extension itself
preserves respondents with missing analytical variables; complete-case
exclusions are applied in the modelling script.

The decision about which sample leads the paper remains open.
Comparisons between Rounds 1–9 and Rounds 1–11 must consider changes in
country coverage, fieldwork period and interview mode.

The SWIID and OECD coverage figures reported above refer to the original
Rounds 1–9 assessment, not to the extended Rounds 1–11 sample.

# Time structure and contextual matching

The contextual survey unit is the country-round.

ESS fieldwork frequently spans two calendar years. In the Rounds 1–9
sample, a majority of country-rounds contain interviews conducted in
more than one calendar year. Contextual variables are therefore not
assigned using a single nominal ESS round year.

Instead, annual inequality values are aggregated to the country-round
level using the observed proportion of respondents interviewed in each
calendar year.

For example, if 80% of respondents in a country-round were interviewed
in 2016 and 20% in 2017, the one-year-lagged inequality exposure is:

$$
0.8 \times Gini_{2015} + 0.2 \times Gini_{2016}
$$

The primary inequality specification uses a one-year lag, linking social
trust measured during fieldwork at time (t) to inequality measured
approximately one year earlier:

$$
Inequality_{c,t-1} \rightarrow Trust_{i,c,t}
$$

Contemporaneous inequality will be examined as a robustness
specification.

The country-round remains the contextual unit rather than splitting
individual ESS rounds into separate country-year surveys. This preserves
the survey structure while incorporating actual fieldwork timing into
the contextual exposure.

One unresolved timing case remains: Estonia in ESS Round 5 lacks usable
interview-year information in the downloaded timing variables. The
earlier preparation record retained it in the underlying ESS dataset
without a constructed inequality exposure. Its current treatment must be
checked against the latest construction script before finalising the
sample.

# Data structure

The data have a hierarchical repeated-cross-sectional structure:

$$
Individuals_{i} \subset CountryPeriods_{ct} \subset Countries_{c}
$$

Individual respondents are observed once, while countries are observed
repeatedly across ESS periods.

The analytical dataset contains, or is intended to incorporate:

- individual-level ESS variables;
- country and period identifiers;
- country-period inequality;
- country-period macroeconomic controls;
- later, if feasible, country-period political-discourse measures.

# Empirical strategy

## Analytical samples

The current implementation is specified in
`04_code/03_models/04_extended_rewb.R`. It estimates the same model
sequence separately for ESS Rounds 1–9 and Rounds 1–11.

Within each period, M1–M3 use a common sample with non-missing
generalised trust, inequality, age, gender and years of education.
Requiring the individual covariates for all three models allows
comparisons without changes in sample composition.

The macro-adjusted analysis additionally requires non-missing lagged log
GDP per capita and unemployment. M3 is re-estimated on this
macro-complete sample before adding the macroeconomic controls in M4.

The modelling script applies no additional survey-mode restriction or
mode adjustment. The upstream extension uses the designated
interviewer-administered Rounds 10–11 input files and retains their mode
information, including video interviews where identified. The
input-sample diagnostics confirm that later-round interviews are
predominantly in-person, with smaller video and unavailable-mode
categories. Mode composition within the fitted samples and sensitivity
to excluding the latter categories remain to be assessed.

## Within-between decomposition

Country-level inequality is decomposed into between-country and
within-country components:

$$
Gini_{ct} =
\overline{Gini}_{c} +
(Gini_{ct} - \overline{Gini}_{c})
$$

Country means are calculated from unique country-round observations,
giving each available round equal weight within a country rather than
weighting it by the number of respondents.

The between-country component is centred on the unweighted mean of the
country means. The within-country component is the deviation from the
country’s own mean.

Decompositions are recalculated separately for the Rounds 1–9 and Rounds
1–11 analyses. For the macro-complete models, inequality, log GDP per
capita and unemployment are decomposed using the country-rounds with
complete data on all three contextual variables.

These contextual decompositions are calculated before individual
complete-case exclusions. They therefore refer to the available
contextual observations, rather than necessarily to the final set of
country-rounds contributing respondents to each fitted model.

## Model sequence

All models are linear mixed models with generalised trust (`ppltrst`,
0–10) as the outcome. They include random intercepts for countries and
country-rounds, with country-round identifiers unique within the pooled
dataset. No random slopes are fitted.

| Model | Specification |
|----|----|
| M1 | Within-country and between-country inequality |
| M2 | M1 plus ESS-round fixed effects |
| M3 | M2 plus age, age squared, gender and years of education |
| M3 macro sample | M3 re-estimated on the macro-complete sample, using its recalculated inequality components |
| M4 | M3 macro sample plus within-country and between-country components of lagged log GDP per capita and unemployment |

ESS-round fixed effects account for differences common to countries
participating in a given survey round. They are not interview-year fixed
effects.

Models are estimated by maximum likelihood using `lme4::lmer` with
`REML = FALSE`.

The script exports fixed-effect estimates, approximate 95% Wald
confidence intervals, log likelihood, AIC, BIC and singular-fit
indicators. Numerical convergence and model diagnostics still need to be
assessed from the fitted models and their outputs.

## Comparing specifications and periods

M1–M3 are compared on their common sample. M3 macro sample and M4 are
compared on the common macro-complete sample, which separates the
addition of macroeconomic controls from the sample restriction.

Differences between the Rounds 1–9 and Rounds 1–11 estimates are
descriptive. The two analyses overlap, and their country means and
within-country deviations are recalculated over different periods.
Subtracting their coefficients is not a formal test of a change in the
inequality–trust relationship.

This script includes neither a post-Round-9 interaction nor a
political-discourse interaction. Any separate period-interaction
analysis must be documented from its own script.

# Individual-level covariates

M3 and M4 adjust for:

- age, centred on the relevant respondent sample mean and divided by
  ten;
- the square of this centred age-in-decades variable;
- gender, with “Man” as the reference category and “Woman” as the other
  modelled category;
- years of education, centred on the relevant respondent sample mean.

Age and education are centred separately for the main and macro-complete
samples within each analytical period.

These covariates account for selected differences in respondent
composition. They are not decomposed into within-country and
between-country components.

Labour-force status is not included in the current model sequence. Its
inclusion remains a possible extension requiring a substantive
rationale.

# Country-level covariates

M4 includes one-year-lagged log GDP per capita at purchasing power
parity and the one-year-lagged unemployment rate.

Both variables enter as separate within-country and between-country
components. Between-country components are grand-mean centred.

The log GDP components are multiplied by ten, so their coefficients
refer to a 0.1 log-point difference in GDP per capita. Inequality and
unemployment retain the units supplied by the input data.

The upstream construction script must document the source series, units
and country-round aggregation of these contextual variables.

Additional contextual controls will require an explicit rationale as
potential confounders or competing explanations. Variables that may
represent mechanisms should not be added automatically.

# Survey weights

The current model sequence is unweighted: no ESS survey weights are
supplied to `lmer`.

The extended dataset retains `analysis_weight`. For Rounds 1–9, this is
inherited from the baseline dataset. For Rounds 10–11, the preparation
script uses a positive, non-missing `anweight` where available,
otherwise a positive, non-missing product of `pspwght` and `pweight`.
Retaining this variable does not mean it is used in the current models.
The separate weight components are not retained in this script’s output
and would need to be recovered from the source data if required for
alternative weighting strategies.

Equal weighting of country-round observations when constructing country
means does not imply equal country weighting in model estimation.

The final weighting strategy remains an open methodological decision.
Sensitivity analyses should consider the role of ESS design and
post-stratification weights, their implementation in the multilevel
framework, and the intended contribution of different countries to the
estimand.

# Political-discourse moderation

## Construct and operationalisation

The substantive hypothesis concerns whether divisive political discourse
conditions the association between economic inequality and generalised
trust. The initial empirical test operationalises one specific dimension
of that broader concept: political parties’ use of group-directed hate
speech. It should therefore be interpreted as a test involving party
hate speech, rather than a comprehensive measure of divisive discourse.

The selected indicator is V-Dem `v2smpolhate`, from the Country-Year
Full+Others dataset, version 16. It assesses the frequency with which
major political parties use rhetoric intended to insult, offend or
intimidate groups defined by characteristics such as race, religion,
sexual orientation, national origin or disability.

The interval-scale estimate is multiplied by minus one so that higher
values indicate more frequent hate speech. The resulting variable is
named `party_hate`. Negative values are valid locations on the reversed
measurement scale; zero does not represent the absence of hate speech.

The earlier candidate, reversed `v2dlcountr`, measures disregard for
counterarguments and also encompasses suppression and non-engagement. It
is retained as an alternative contextual measure, but is no longer the
preferred operationalisation of hostile rhetoric. `v2cacamps` concerns
societal political polarisation and is retained as a distinct contextual
alternative, not an interchangeable measure of party communication.

Source documentation: [V-Dem Codebook
v16](https://v-dem.net/documents/70/codebook_v16.pdf), sections 7.5.11
and 3.7.1.3.

## Country-round construction

Annual indicator values are aggregated to ESS country-rounds using the
proportion of dated interviews conducted in each calendar year. Baseline
timing counts are taken from `ess_inequality_coverage.csv`; later-round
counts are taken from `ess_post9_country_round_year_coverage.csv`.

Both contemporaneous and one-year-lagged exposures have been
constructed. The initial model will use the one-year lag to place the
contextual measure before the interview year. Contemporaneous exposure
is a planned sensitivity specification. This timing choice does not
establish causality.

Interview-year shares are calculated among respondents with known
interview years. Respondents without dates remain in the respondent
dataset and receive the contextual exposure of their country-round where
one can be constructed. This assumes that dated respondents adequately
represent the country-round’s fieldwork timing.

All annual components must have an observed indicator value for an
exposure to be calculated. Missing contextual years are not interpolated
or omitted through renormalisation. Country-rounds without usable timing
information retain missing exposures.

## Measurement audit completed on 1 October 2026

All three candidate indicators have complete annual coverage for the 39
ESS countries over 2000–2025. Both timing specifications were
successfully constructed for 279 of 280 ESS country-rounds. Estonia
Round 5 is the sole exception because no usable interview-year
information is available. There are no missing V-Dem matches among the
dated interview-year components.

At the observed ESS country-rounds, lagged party hate speech varies in
28 of the 33 countries with repeated observations in Rounds 1–9, and in
33 of the 35 countries with repeated observations in Rounds 1–11. These
are pre-model audit counts; complete-case exclusions may reduce the
available information.

Country trajectories were inspected before fitting discourse
interactions. Lithuania and Finland showed notable differences between
contemporaneous and lagged exposures. Inspection of their annual source
estimates confirmed that movements are present in the V-Dem data. This
check does not establish their substantive validity. Both countries are
retained, with country influence and indicator uncertainty reserved for
sensitivity assessment.

Incomplete timing information affects 24 country-rounds, including
Estonia Round 5. Particularly substantial partial gaps occur in Spain,
Iceland, Latvia and Croatia in Round 9, and Czechia in Round 1.
Fieldwork documentation should be checked to assess whether undated
interviews could alter the annual exposure weights. These timing
limitations also apply to the inequality exposures constructed from the
same baseline counts.

## Initial moderation specification

The initial analysis will extend M4 separately for Rounds 1–9 and Rounds
1–11. Nested models within each period will use the same complete-case
sample. Contextual decompositions will use unique country-rounds
contributing to that sample, giving each observed round equal weight
within a country.

Party hate speech will be decomposed into its country mean and its
deviation from that mean. The main moderation test concerns the
interaction between within-country inequality and within-country hate
speech. A second interaction concerns within-country inequality and
country-mean hate speech.

The first interaction asks whether the inequality–trust association is
more negative when hate speech is higher than usual within a country.
The second asks whether the association differs according to countries’
average levels of hate speech.

The product of the two within-country deviations can itself have a
nonzero country mean. Its country mean and its deviation from that mean
will therefore enter separately, allowing the within-country interaction
to be distinguished from between-country differences in the average
product.

Models will retain the existing M4 individual and macroeconomic
controls, ESS-round fixed effects, and country and country-round random
intercepts. Relevant lower-order terms will be included. Initial
estimation will remain unweighted, consistent with the existing model
sequence.

Interaction estimates will be interpreted using marginal inequality
slopes over supported combinations of the moderator components, with
uncertainty. The first models will use indicator point estimates; they
will not yet propagate V-Dem measurement uncertainty or SWIID imputation
uncertainty.

## Respondent party-affiliation extension

The ESS vote and party-closeness data have been prepared but have not
yet been used in substantive models. Linking these records to external
party characteristics remains a separate planned strand.

V-Party `v2paopresp` is a candidate measure of party leadership’s
demonisation of political opponents. Its expert-coded coverage through
2019 limits its application to the later ESS rounds. Historical party
scores will not automatically be carried forward as if they measured
current rhetoric.

Reported vote and party closeness will remain distinguishable.
Associations involving the rhetoric of an affiliated party will be
interpreted as variation by party affiliation, not as causal effects of
individual exposure to rhetoric.

A national aggregate based on party scores and election vote shares does
not require respondent-level ESS party matching. The
respondent-affiliation analysis does require that linkage.

Source documentation: [V-Party Codebook
v2](https://v-dem.net/documents/6/vparty_codebook_v2.pdf).

# Wider-project extensions

These extensions are retained for future scoping and are not co-primary
analyses in the current paper:

- **Social connectedness and participation:** `sclmeet` captures
  frequency of social contact; `sclact` captures participation relative
  to same-age peers. Retain the coverage and distribution audit. Analyse
  them separately unless a composite is justified conceptually and
  empirically.
- **Political or institutional trust:** retain the existing exploratory
  work, while distinguishing it from generalised trust.
- **Migration and diversity:** define the substantive question,
  candidate ESS and contextual variables, coverage, and their role as
  confounders, moderators or mechanisms before fitting an extension.
- **Perceived social conflict:** consider only if repeated, comparable
  measurement adds a distinct contribution; a complementary dataset may
  be required.

# Robustness and sensitivity analyses

Planned robustness checks include:

- OECD IDD rather than SWIID inequality;
- contemporaneous rather than one-year-lagged inequality;
- sensitivity to SWIID measurement uncertainty;
- alternative treatment of ESS fieldwork timing;
- inclusion of later ESS rounds where survey-mode comparability permits;
- alternative individual-level adjustment sets;
- alternative country-level adjustment sets;
- alternative survey-weighting strategies;
- a three-item trust/fairness/helpfulness scale rather than `ppltrst`
  alone;
- influence of individual countries or country-periods;
- alternative specifications of common temporal trends.

Prioritise these checks against the established baseline and extended
models. Their inclusion here records planned checks, not completed
analyses.

# Implementation status and next tasks

This status summary combines the uploaded design files with the recorded
September work sessions. The repository and current model scripts have
not been inspected for this revision.

## Work already reported as completed

- ESS country-round/interview-year coverage assessment and
  inequality-source coverage comparison.
- Construction of lagged SWIID inequality exposures and analysis-ready
  ESS data.
- Addition of GDP per capita and unemployment controls; estimation of
  baseline and extended REWB models.
- Construction of the Rounds 1–11 extension while preserving the Rounds
  1–9 baseline and interview-mode information.
- Preparation of the ESS political supplement and party inventory.
- Initial cohesion/migration-variable preparation and descriptive
  outcome audit.

## Next tasks

1.  Check the upstream data-construction scripts and saved model
    diagnostics, particularly survey-mode handling, contextual units,
    sample exclusions and the Estonia Round 5 timing anomaly.
2.  Verify and construct the provisional main V-Dem measure; inspect
    coverage, distributions and within-country variation.
3.  Specify and estimate the inequality–discourse moderation model,
    retaining clear baseline and extended-sample comparisons.
4.  Complete V-Party import, matching and contextual validation
    construction.
5.  Fit and pool models across the SWIID imputations and prioritise the
    remaining sensitivity checks.
6.  Synchronise methods pages and READMEs, including hypothesis
    numbering and the narrower trust framing.
7.  Scope migration/diversity and other extensions only after clarifying
    their contribution to the main paper.

# Open design decisions

- Which ESS sample leads the paper and the exact handling of later-round
  survey modes.
- Final survey-weighting strategy and sensitivity to alternative country
  weighting.
- Whether the current random-intercept structure, ESS-round fixed
  effects and covariate set are adequate for the final analysis,
  informed by model diagnostics and sensitivity checks.
- Treatment and pooling of SWIID estimation uncertainty.
- Sensitivity of party hate-speech moderation to contemporaneous versus
  lagged exposure, country influence, survey mode, weighting and
  alternative contextual measures.
- Treatment of V-Dem measurement uncertainty alongside SWIID imputation
  uncertainty.
- Verification of fieldwork timing in country-rounds with substantial
  missing interview-year information.
- V-Party election alignment, party mapping, vote-weight source and
  unmatched-party treatment.
- Current treatment of the Estonia Round 5 timing anomaly.

# Recorded design choices

- Generalised trust is the substantive focus of the paper; social
  cohesion supplies the broader motivation.
- `ppltrst` is the primary outcome.
- SWIID is the primary inequality source; OECD IDD is the planned
  inequality robustness source.
- The primary inequality exposure is lagged by one year and aggregated
  to country-rounds using interview-year respondent shares.
- Within-country inequality variation is the primary estimand;
  between-country inequality is estimated separately.
- Rounds 1–9 remain a baseline comparator; Rounds 10–11 have been
  incorporated into an extended dataset.
- The initial discourse analysis uses one-year-lagged party hate speech,
  measured by reversed V-Dem `v2smpolhate`, with separate within-country
  and between-country moderator components. This specification was
  selected after the measurement audit and before inspecting
  discourse-interaction results.
