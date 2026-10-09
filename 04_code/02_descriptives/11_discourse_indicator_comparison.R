# ------------------------------------------------------------
# Compare dimensions of political discourse and polarisation
# Project: Inequality and social cohesion
# V-Dem Country-Year Full+Others v16; ESS Rounds 1-11
#
# PURPOSE
#   Extend the already completed political-discourse audit with
#   four further V-Dem measures, without overwriting the
#   established ESS country-round exposure file.
#
#   Existing: party hate speech, disrespect for counterarguments,
#             antagonistic political camps.
#   Additional: party social-media disinformation, lack of common-
#               good justification, issue polarisation, lack of
#               reasoned justification.
#
#   Report annual and ESS-country-round coverage, variation by
#   period, and between-country versus within-country correlations.
#   All signs are harmonised for descriptive comparison: higher
#   values mean MORE of the unfavourable condition indicated.
#
#   No statistical models are fitted. No source data are modified.
#
# PREREQUISITE
#   Run 04_code/02_descriptives/04_discourse_indicator_audit.R
#   at least once to create ess_country_round_discourse_r1_r11.csv.
#
# OUTPUTS (local only)
#   05_output/tables/discourse_comparison/*.csv
# ------------------------------------------------------------

library(dplyr)
library(here)
library(readr)
library(tidyr)
library(tibble)

# 1. Files, indicators, orientation -----------------------------

source_file <- here(
  "03_data", "raw", "vdem", "V-Dem-CY-Full+Others-v16.csv"
)
existing_file <- here(
  "03_data", "interim", "ess_country_round_discourse_r1_r11.csv"
)
ess_file <- here(
  "03_data", "processed", "ess_analysis_r1_r11.rds"
)
macro_file <- here(
  "03_data", "interim", "ess_country_round_macro_r1_r11.csv"
)
output_dir <- here("05_output", "tables", "discourse_comparison")

for (f in c(source_file, existing_file, ess_file, macro_file)) {
  if (!file.exists(f)) stop("Required local file not found: ", f)
}

# Four new indicators from v16.  Direction verified in V-Dem v16 codebook.
new_variables <- c(
  "v2smpardom",  # Higher raw = LESS party disinformation
  "v2dlcommon",  # Higher raw = MORE common-good justification
  "v2smpolsoc",  # Higher raw = LESS issue polarisation
  "v2dlreason"   # Higher raw = MORE reasoned justification
)

indicator_dictionary <- tribble(
  ~indicator,                  ~source,        ~family,                  ~direction,
  "party_hate",                "v2smpolhate",  "Political rhetoric",      "Higher = more party hate speech",
  "counterarg_disrespect",    "v2dlcountr",  "Deliberative discourse",  "Higher = less respect for counterarguments",
  "antagonistic_camps",       "v2cacamps",   "Social polarisation",    "Higher = more antagonistic political camps",
  "party_disinformation",     "v2smpardom",  "Political rhetoric",      "Higher = more party social-media disinformation",
  "low_common_good",          "v2dlcommon",  "Deliberative discourse",  "Higher = less common-good justification",
  "issue_polarisation",       "v2smpolsoc",  "Issue polarisation",      "Higher = more issue polarisation",
  "low_reasoned_justification","v2dlreason", "Deliberative discourse",  "Higher = less reasoned justification"
)

signal_names <- indicator_dictionary$indicator

# 2. Import V-Dem and preserve original country matching --------

required_vdem <- c("country_name", "year", new_variables)
header <- names(read_csv(
  source_file, n_max = 0, show_col_types = FALSE
))
missing_vdem <- setdiff(required_vdem, header)
if (length(missing_vdem) > 0) {
  stop("Missing columns in V-Dem v16 file: ",
       paste(missing_vdem, collapse = ", "))
}

vdem <- read_csv(
  source_file, col_select = all_of(required_vdem),
  show_col_types = FALSE
)

# Match exactly the correspondence in 04_discourse_indicator_audit.R.
country_lookup <- tribble(
  ~country_name,          ~cntry,
  "Albania",             "AL",
  "Austria",             "AT",
  "Belgium",             "BE",
  "Bulgaria",            "BG",
  "Croatia",             "HR",
  "Cyprus",              "CY",
  "Czech Republic",      "CZ",
  "Czechia",             "CZ",
  "Denmark",             "DK",
  "Estonia",             "EE",
  "Finland",             "FI",
  "France",              "FR",
  "Germany",             "DE",
  "Greece",              "GR",
  "Hungary",             "HU",
  "Iceland",             "IS",
  "Ireland",             "IE",
  "Israel",              "IL",
  "Italy",               "IT",
  "Kosovo",              "XK",
  "Latvia",              "LV",
  "Lithuania",           "LT",
  "Luxembourg",          "LU",
  "Malta",               "MT",
  "Montenegro",          "ME",
  "Netherlands",         "NL",
  "North Macedonia",     "MK",
  "Macedonia",           "MK",
  "Norway",              "NO",
  "Poland",              "PL",
  "Portugal",            "PT",
  "Romania",             "RO",
  "Russia",              "RU",
  "Russian Federation",  "RU",
  "Serbia",              "RS",
  "Slovakia",            "SK",
  "Slovenia",            "SI",
  "Spain",               "ES",
  "Sweden",              "SE",
  "Switzerland",         "CH",
  "Turkey",              "TR",
  "Türkiye",             "TR",
  "Ukraine",             "UA",
  "United Kingdom",      "GB"
)

ess <- readRDS(ess_file)
ess_countries <- ess |> distinct(cntry)

vdem_ess <- vdem |>
  inner_join(country_lookup, by = "country_name") |>
  semi_join(ess_countries, by = "cntry")

unmatched <- anti_join(
  ess_countries, vdem_ess |> distinct(cntry), by = "cntry"
)
if (nrow(unmatched) > 0) {
  print(unmatched, n = Inf)
  stop("Some ESS countries lack a V-Dem country match.")
}

duplicate_vdem <- vdem_ess |>
  count(cntry, year) |>
  filter(n > 1)
if (nrow(duplicate_vdem) > 0) {
  print(duplicate_vdem, n = Inf)
  stop("Duplicate V-Dem country-years after country correspondence.")
}

# 3. Annual country-year coverage, including missing cells -------

annual_audit <- crossing(
  cntry = sort(unique(as.character(ess_countries$cntry))),
  year = 2000:2025
) |>
  left_join(
    vdem_ess |> select(cntry, year, all_of(new_variables)),
    by = c("cntry", "year")
  ) |>
  pivot_longer(
    cols = all_of(new_variables),
    names_to = "source", values_to = "value"
  )

annual_coverage <- annual_audit |>
  group_by(source) |>
  summarise(
    expected_country_years = n(),
    observed_country_years = sum(is.finite(value)),
    coverage_share = observed_country_years / expected_country_years,
    countries_with_any_data = n_distinct(cntry[is.finite(value)]),
    .groups = "drop"
  )

# 4. Reconstruct new ESS country-round exposure values -----------

base_timing <- read_csv(
  here("03_data", "interim", "ess_inequality_coverage.csv"),
  show_col_types = FALSE
) |>
  select(cntry, essround, interview_year, n_respondents) |>
  filter(essround <= 9)

post9_timing <- read_csv(
  here("03_data", "interim", "ess_post9_country_round_year_coverage.csv"),
  show_col_types = FALSE
) |>
  select(cntry, essround, interview_year, n_respondents) |>
  filter(essround >= 10, essround <= 11)

timing <- bind_rows(base_timing, post9_timing) |>
  filter(!is.na(interview_year))

if (nrow(timing |> count(cntry, essround, interview_year) |>
         filter(n > 1)) > 0) {
  stop("Duplicate country-round interview-year rows.")
}
if (any(!is.finite(timing$n_respondents) | timing$n_respondents <= 0)) {
  stop("Timing contains invalid respondent counts.")
}

timing <- timing |>
  group_by(cntry, essround) |>
  mutate(share = n_respondents / sum(n_respondents)) |>
  ungroup()

stopifnot(all(abs(
  timing |> group_by(cntry, essround) |>
    summarise(total = sum(share), .groups = "drop") |>
    pull(total) - 1
) < 1e-10))

vdem_values <- vdem_ess |>
  select(cntry, year, all_of(new_variables))

exposure_components <- bind_rows(
  timing |> mutate(specification = "contemp", context_year = interview_year),
  timing |> mutate(specification = "lag1", context_year = interview_year - 1L)
) |>
  left_join(vdem_values, by = c("cntry", "context_year" = "year")) |>
  pivot_longer(cols = all_of(new_variables),
               names_to = "source", values_to = "value")

# Do not renormalise over available annual values. A single missing
# annual component makes that country-round exposure unavailable.
new_exposures <- exposure_components |>
  group_by(cntry, essround, source, specification) |>
  summarise(
    exposure = if (all(is.finite(value))) {
      weighted.mean(value, share)
    } else {
      NA_real_
    },
    .groups = "drop"
  ) |>
  pivot_wider(
    id_cols = c(cntry, essround),
    names_from = c(source, specification),
    values_from = exposure,
    names_glue = "{source}_{specification}"
  )

# 5. Harmonise sign, join with original three indicators ---------

original <- read_csv(existing_file, show_col_types = FALSE)
required_existing <- c(
  "cntry", "essround", "party_hate_lag1",
  "counterarg_disrespect_lag1", "v2cacamps_lag1"
)
missing_existing <- setdiff(required_existing, names(original))
if (length(missing_existing) > 0) {
  stop("Existing audit output missing columns: ",
       paste(missing_existing, collapse = ", "))
}

if (nrow(original |> count(cntry, essround) |> filter(n > 1)) > 0) {
  stop("Existing discourse context contains duplicate country-rounds.")
}

# Existing audit's original columns remain untouched in its file.
# The new comparison uses an aligned scale where higher = more
# hostility, lower deliberative quality, or more polarisation.
comparison_context <- original |>
  select(all_of(required_existing)) |>
  left_join(new_exposures, by = c("cntry", "essround")) |>
  transmute(
    cntry, essround,
    party_hate = party_hate_lag1,
    counterarg_disrespect = counterarg_disrespect_lag1,
    antagonistic_camps = v2cacamps_lag1,
    party_disinformation = -v2smpardom_lag1,
    low_common_good = -v2dlcommon_lag1,
    issue_polarisation = -v2smpolsoc_lag1,
    low_reasoned_justification = -v2dlreason_lag1
  )

stopifnot(nrow(comparison_context) == nrow(original))

# 6. Construct the common M5 analytic country-round universe -----
# This prevents coverage estimates being inflated by country-rounds
# that could never enter the M5 trust model. No models are refitted.

macro <- read_csv(macro_file, show_col_types = FALSE) |>
  select(cntry, essround, log_gdp_pc_ppp_lag1, unemployment_lag1)

if (nrow(macro |> count(cntry, essround) |> filter(n > 1)) > 0) {
  stop("Duplicate country-rounds in macro controls.")
}

model_rows <- ess |>
  left_join(macro, by = c("cntry", "essround")) |>
  left_join(
    original |> select(cntry, essround, party_hate_lag1),
    by = c("cntry", "essround")
  ) |>
  filter(
    is.finite(ppltrst), is.finite(gini_swiid_lag1),
    is.finite(log_gdp_pc_ppp_lag1), is.finite(unemployment_lag1),
    is.finite(party_hate_lag1),
    is.finite(age), !is.na(gender),
    is.finite(education_years)
  )

cat("\nM5-like complete-case respondent count:", nrow(model_rows), "\n")
if (nrow(model_rows) != 498461L) {
  warning(
    "Expected 498,461 from previous M5; obtained ", nrow(model_rows),
    ". Check complete-case criteria before interpreting model-sample figures."
  )
}

model_country_rounds <- model_rows |> distinct(cntry, essround)
comparison_model <- comparison_context |>
  inner_join(model_country_rounds, by = c("cntry", "essround")) |>
  mutate(period = if_else(essround <= 9, "R1-R9", "R10-R11"))

# 7. Per-indicator ESS model-universe coverage --------------------

indicator_coverage <- comparison_model |>
  pivot_longer(cols = all_of(signal_names),
               names_to = "indicator", values_to = "value") |>
  group_by(indicator, period) |>
  summarise(
    n_country_rounds = n(),
    n_available = sum(is.finite(value)),
    coverage_share = n_available / n_country_rounds,
    n_countries_available = n_distinct(cntry[is.finite(value)]),
    .groups = "drop"
  ) |>
  left_join(indicator_dictionary, by = "indicator") |>
  arrange(indicator, period)

# All seven indicators observed on precisely the same country-rounds.
common_context <- comparison_model |>
  filter(if_all(all_of(signal_names), is.finite))

common_summary <- tibble(
  universe = c("M5-like context", "All-seven complete-case context"),
  n_country_rounds = c(nrow(comparison_model), nrow(common_context)),
  n_countries = c(n_distinct(comparison_model$cntry),
                  n_distinct(common_context$cntry))
)

common_by_period <- common_context |>
  count(period, name = "n_country_rounds")

# 8. Period profiles, pairwise versus common sample ----------------

profile <- function(dat, sample_name) {
  dat |>
    pivot_longer(cols = all_of(signal_names),
                 names_to = "indicator", values_to = "value") |>
    filter(is.finite(value)) |>
    group_by(period, indicator) |>
    summarise(
      n_country_rounds = n(),
      n_countries = n_distinct(cntry),
      mean = mean(value),
      sd = if (n() > 1) sd(value) else NA_real_,
      min = min(value), max = max(value),
      .groups = "drop"
    ) |>
    mutate(sample = sample_name, .before = 1)
}

period_profiles <- bind_rows(
  profile(comparison_model, "All observed for indicator"),
  profile(common_context, "Shared seven-indicator sample")
)

# In-country variation on common country-round sample.
country_demeaned <- common_context |>
  group_by(cntry) |>
  mutate(across(
    all_of(signal_names),
    ~ .x - mean(.x),
    .names = "{.col}"
  )) |>
  ungroup()

within_variation <- country_demeaned |>
  pivot_longer(cols = all_of(signal_names),
               names_to = "indicator", values_to = "within_deviation") |>
  group_by(indicator) |>
  summarise(
    n_country_rounds = n(),
    n_countries = n_distinct(cntry),
    sd_within = sd(within_deviation),
    min_within = min(within_deviation),
    max_within = max(within_deviation),
    .groups = "drop"
  )

# 9. Correlation matrices: contextual and within-country ----------

pairs <- combn(signal_names, 2L, simplify = FALSE)

pair_correlations <- function(dat, sample_name) {
  bind_rows(lapply(pairs, function(pair) {
    x <- dat[[pair[1]]]
    y <- dat[[pair[2]]]
    ok <- is.finite(x) & is.finite(y)
    n_ok <- sum(ok)
    estimate <- if (
      n_ok >= 5 && sd(x[ok]) > 0 && sd(y[ok]) > 0
    ) {
      cor(x[ok], y[ok])
    } else {
      NA_real_
    }
    tibble(
      sample = sample_name,
      indicator_1 = pair[1], indicator_2 = pair[2],
      n_country_rounds = n_ok, correlation = estimate
    )
  }))
}

pairwise_correlations <- bind_rows(
  pair_correlations(comparison_model, "All available country-round pairs"),
  pair_correlations(common_context, "Common seven-indicator sample"),
  pair_correlations(country_demeaned, "Within-country deviations; common sample")
)

# 10. Balanced-country descriptive check -------------------------
# Countries observed at least once on the shared complete-case sample
# in BOTH R1-R9 and R10-R11. This does NOT balance number of rounds.

balanced_countries <- common_context |>
  group_by(cntry) |>
  summarise(
    n_early = sum(essround <= 9),
    n_late = sum(essround >= 10),
    .groups = "drop"
  ) |>
  filter(n_early > 0, n_late > 0)

balanced_profiles <- common_context |>
  semi_join(balanced_countries, by = "cntry") |>
  pivot_longer(cols = all_of(signal_names),
               names_to = "indicator", values_to = "value") |>
  group_by(period, indicator) |>
  summarise(
    n_country_rounds = n(), n_countries = n_distinct(cntry),
    mean = mean(value), sd = if (n() > 1) sd(value) else NA_real_,
    .groups = "drop"
  )

# 11. Save small audit tables ------------------------------------

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(indicator_dictionary, file.path(output_dir, "indicator_dictionary.csv"))
write_csv(annual_coverage, file.path(output_dir, "new_indicators_annual_coverage.csv"))
write_csv(indicator_coverage, file.path(output_dir, "indicator_model_sample_coverage.csv"))
write_csv(common_summary, file.path(output_dir, "common_sample_summary.csv"))
write_csv(common_by_period, file.path(output_dir, "common_sample_by_period.csv"))
write_csv(period_profiles, file.path(output_dir, "indicator_period_profiles.csv"))
write_csv(within_variation, file.path(output_dir, "indicator_within_variation.csv"))
write_csv(pairwise_correlations, file.path(output_dir, "indicator_correlations.csv"))
write_csv(balanced_countries, file.path(output_dir, "balanced_country_counts.csv"))
write_csv(balanced_profiles, file.path(output_dir, "balanced_country_period_profiles.csv"))

# 12. Console: only decision-relevant summaries -------------------

cat("\n--- NEW V-DEM ANNUAL COVERAGE (2000-2025) ---\n")
print(annual_coverage, n = Inf, width = Inf)

cat("\n--- INDICATOR COVERAGE ON M5-LIKE COUNTRY-ROUNDS ---\n")
print(indicator_coverage |>
        select(indicator, period, n_available,
               n_country_rounds, coverage_share),
      n = Inf, width = Inf)

cat("\n--- COMMON SEVEN-INDICATOR SAMPLE ---\n")
print(common_summary, n = Inf, width = Inf)
print(common_by_period, n = Inf, width = Inf)

cat("\n--- COMMON-SAMPLE PERIOD PROFILES ---\n")
print(period_profiles |>
        filter(sample == "Shared seven-indicator sample") |>
        select(indicator, period, n_country_rounds, mean, sd),
      n = Inf, width = Inf)

cat("\n--- WITHIN-COUNTRY VARIATION ---\n")
print(within_variation, n = Inf, width = Inf)

cat("\n--- CORRELATIONS: COUNTRY-ROUND AND WITHIN-COUNTRY ---\n")
print(pairwise_correlations |>
        filter(sample != "All available country-round pairs") |>
        select(sample, indicator_1, indicator_2,
               n_country_rounds, correlation),
      n = Inf, width = Inf)

cat("\n--- BALANCED COUNTRY COUNT ---\n")
cat(nrow(balanced_countries),
    "countries observed both before and after R9\n")

cat("\nFinished. Tables saved in:\n", output_dir, "\n")
