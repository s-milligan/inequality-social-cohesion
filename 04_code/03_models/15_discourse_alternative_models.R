
# ------------------------------------------------------------
# Comparative political-discourse REWB models
# Project: Inequality and social cohesion
#
# ESS Rounds 1-11; V-Dem v16
#
# Models:
#   M4: No political-context indicator
#   M5: Party hate speech
#   A:   Disrespect for counterarguments
#   B:   Lack of common-good framing
#   C:   Party disinformation
#   D:   Issue polarisation
#   E:   Antagonistic political camps
#
# All models:
#   - Same respondents and country-rounds
#   - Same M5 individual and macro controls
#   - Within-between decomposition of contextual variables
#   - ESS-round fixed effects
#   - Country and country-round random intercepts
#   - ML estimation, without survey weights
#
# Additional indicators use interview-year-weighted,
# one-year-lagged V-Dem country-round exposures.
#
# No original data files are overwritten.
# Previous model outputs are compared before replacement.
# ------------------------------------------------------------

library(dplyr)
library(here)
library(lme4)
library(readr)
library(tibble)
library(tidyr)

# 1. Paths ----------------------------------------------------

ess_file <- here(
  "03_data", "processed", "ess_analysis_r1_r11.rds"
)

macro_file <- here(
  "03_data", "interim",
  "ess_country_round_macro_r1_r11.csv"
)

discourse_file <- here(
  "03_data", "interim",
  "ess_country_round_discourse_r1_r11.csv"
)

vdem_file <- here(
  "03_data", "raw", "vdem",
  "V-Dem-CY-Full+Others-v16.csv"
)

timing_base_file <- here(
  "03_data", "interim", "ess_inequality_coverage.csv"
)

timing_post9_file <- here(
  "03_data", "interim",
  "ess_post9_country_round_year_coverage.csv"
)

reference_file <- here(
  "05_output", "tables", "party_hate_moderation",
  "r1_r11_fixed_effects.csv"
)

output_dir <- here(
  "05_output", "tables", "discourse_alternative_models"
)

previous_results_file <- file.path(
  output_dir, "discourse_results.csv"
)

required_files <- c(
  ess_file, macro_file, discourse_file,
  vdem_file, timing_base_file, timing_post9_file
)

for (f in required_files) {
  if (!file.exists(f)) {
    stop("Required input file missing: ", f)
  }
}

# Read earlier estimates before overwriting any tables.

previous_results <- if (file.exists(previous_results_file)) {
  read_csv(
    previous_results_file,
    show_col_types = FALSE
  ) |>
    select(
      model, term,
      previous_estimate = estimate
    )
} else {
  tibble()
}

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

keys <- c("cntry", "essround")

check_keys <- function(dat, columns, label) {
  
  missing <- setdiff(columns, names(dat))
  
  if (length(missing)) {
    stop(
      label, ": missing columns: ",
      paste(missing, collapse = ", ")
    )
  }
  
  key_data <- as.data.frame(
    dat[, columns, drop = FALSE]
  )
  
  if (anyNA(key_data) || anyDuplicated(key_data)) {
    stop(label, ": missing or duplicate keys.")
  }
}

# 2. Load established analytical inputs -----------------------

ess <- readRDS(ess_file) |>
  select(
    cntry, essround, idno, interview_mode,
    ppltrst, gini_swiid_lag1,
    age, gender, education_years
  ) |>
  mutate(cntry = as.character(cntry))

check_keys(
  ess, c("cntry", "essround", "idno"),
  "ESS respondents"
)

macro <- read_csv(
  macro_file,
  show_col_types = FALSE
) |>
  select(
    cntry, essround,
    log_gdp_pc_ppp_lag1,
    unemployment_lag1
  )

discourse <- read_csv(
  discourse_file,
  show_col_types = FALSE
) |>
  select(
    cntry, essround,
    party_hate_lag1,
    counterarg_disrespect_lag1,
    v2cacamps_lag1
  ) |>
  rename(
    antagonistic_camps_lag1 = v2cacamps_lag1
  )

check_keys(macro, keys, "Macro data")
check_keys(discourse, keys, "Discourse data")

# 3. Import the additional V-Dem indicators -------------------
#
# These were previously included in the descriptive audit:
#
# v2dlcommon: Higher raw = more common-good justification
# v2smpardom: Higher raw = less party disinformation
# v2smpolsoc: Higher raw = less issue polarisation
#
# Reverse all three so higher indicates more of the
# unfavourable condition named by the constructed variable.
#
# v2cacamps is already oriented towards greater antagonism.

new_indicators <- c(
  "v2dlcommon",
  "v2smpardom",
  "v2smpolsoc"
)

country_lookup <- tribble(
  ~country_name,        ~cntry,
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

vdem <- read_csv(
  vdem_file,
  col_select = all_of(
    c("country_name", "year", new_indicators)
  ),
  show_col_types = FALSE
) |>
  inner_join(
    country_lookup,
    by = "country_name"
  ) |>
  semi_join(
    ess |> distinct(cntry),
    by = "cntry"
  ) |>
  select(
    cntry, year, all_of(new_indicators)
  )

check_keys(
  vdem,
  c("cntry", "year"),
  "V-Dem country-years"
)

unmatched <- ess |>
  distinct(cntry) |>
  anti_join(
    vdem |> distinct(cntry),
    by = "cntry"
  )

if (nrow(unmatched)) {
  print(unmatched, n = Inf)
  stop("Some ESS countries lack a V-Dem match.")
}

# 4. Construct lagged ESS country-round exposures -------------
#
# Use the same fieldwork timing and respondent shares as
# 04_discourse_indicator_audit.R and the previous script.
#
# A missing annual V-Dem component makes the entire
# corresponding country-round exposure missing.
# No renormalisation over available years is performed.

timing_base <- read_csv(
  timing_base_file,
  show_col_types = FALSE
) |>
  select(
    cntry, essround, interview_year, n_respondents
  ) |>
  filter(essround <= 9)

timing_post9 <- read_csv(
  timing_post9_file,
  show_col_types = FALSE
) |>
  select(
    cntry, essround, interview_year, n_respondents
  ) |>
  filter(
    essround >= 10,
    essround <= 11
  )

timing <- bind_rows(
  timing_base,
  timing_post9
) |>
  filter(!is.na(interview_year))

check_keys(
  timing,
  c("cntry", "essround", "interview_year"),
  "Interview timing"
)

if (any(
  !is.finite(timing$n_respondents) |
  timing$n_respondents <= 0
)) {
  stop("Invalid interview-year respondent counts.")
}

timing <- timing |>
  group_by(cntry, essround) |>
  mutate(
    share = n_respondents / sum(n_respondents)
  ) |>
  ungroup()

share_check <- timing |>
  group_by(cntry, essround) |>
  summarise(
    total_share = sum(share),
    .groups = "drop"
  )

stopifnot(
  all(abs(share_check$total_share - 1) < 1e-10)
)

new_exposures <- timing |>
  mutate(
    context_year = interview_year - 1L
  ) |>
  left_join(
    vdem,
    by = c("cntry", "context_year" = "year")
  ) |>
  pivot_longer(
    cols = all_of(new_indicators),
    names_to = "source",
    values_to = "value"
  ) |>
  group_by(cntry, essround, source) |>
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
    names_from = source,
    values_from = exposure
  ) |>
  transmute(
    cntry,
    essround,
    low_common_good_lag1 = -v2dlcommon,
    party_disinformation_lag1 = -v2smpardom,
    issue_polarisation_lag1 = -v2smpolsoc
  )

check_keys(
  new_exposures,
  keys,
  "New V-Dem country-round exposures"
)

# 5. Construct the common analytical sample ------------------

context_vars <- c(
  "gini_swiid_lag1",
  "party_hate_lag1",
  "counterarg_disrespect_lag1",
  "low_common_good_lag1",
  "party_disinformation_lag1",
  "issue_polarisation_lag1",
  "antagonistic_camps_lag1",
  "log_gdp_pc_ppp_lag1",
  "unemployment_lag1"
)

dat <- ess |>
  left_join(macro, by = keys) |>
  left_join(discourse, by = keys) |>
  left_join(new_exposures, by = keys)

model_data <- dat |>
  filter(
    if_all(all_of(context_vars), is.finite),
    is.finite(ppltrst),
    is.finite(age),
    is.finite(education_years),
    !is.na(gender)
  )

sample_summary <- tibble(
  n_respondents = nrow(model_data),
  n_countries = n_distinct(model_data$cntry),
  n_country_rounds = nrow(
    distinct(model_data, cntry, essround)
  ),
  n_r1_r9 = sum(model_data$essround <= 9),
  n_r10_r11 = sum(model_data$essround >= 10)
)

cat("\n--- ANALYTICAL SAMPLE ---\n")
print(sample_summary)

# The descriptive audit found full indicator coverage.
# All seven models should reproduce the established M5 sample.

if (
  nrow(model_data) != 498461L ||
  n_distinct(model_data$cntry) != 38L ||
  nrow(distinct(model_data, cntry, essround)) != 273L
) {
  stop(
    "The sample differs from established M5. ",
    "Investigate coverage or missingness before fitting."
  )
}

if (!all(
  unique(as.character(model_data$gender)) %in%
  c("Man", "Woman")
)) {
  stop("Unexpected gender coding.")
}

# 6. Contextual within-between decomposition ------------------
#
# Each country-round contributes equally to country means.
# Between-country components are grand-mean centred.
#
# Reproduce the decomposition used in the previous
# alternative-model script.

context <- model_data |>
  distinct(
    cntry, essround,
    across(all_of(context_vars))
  )

check_keys(
  context, keys, "Analytical country-round context"
)

variable_map <- c(
  gini = "gini_swiid_lag1",
  hate = "party_hate_lag1",
  counterarg = "counterarg_disrespect_lag1",
  low_common_good = "low_common_good_lag1",
  disinformation = "party_disinformation_lag1",
  issue_polarisation = "issue_polarisation_lag1",
  antagonistic_camps = "antagonistic_camps_lag1",
  gdp = "log_gdp_pc_ppp_lag1",
  unemployment = "unemployment_lag1"
)

for (prefix in names(variable_map)) {
  
  raw_name <- unname(variable_map[[prefix]])
  mean_name <- paste0(prefix, "_mean")
  within_name <- paste0(prefix, "_w")
  between_name <- paste0(prefix, "_b")
  
  context <- context |>
    group_by(cntry) |>
    mutate(
      !!mean_name := mean(.data[[raw_name]]),
      !!within_name :=
        .data[[raw_name]] - .data[[mean_name]]
    ) |>
    ungroup()
  
  country_means <- context |>
    distinct(cntry, .data[[mean_name]])
  
  grand_mean <- mean(
    country_means[[mean_name]]
  )
  
  context[[between_name]] <-
    context[[mean_name]] - grand_mean
}

context <- context |>
  mutate(
    gdp_w_10 = 10 * gdp_w,
    gdp_b_10 = 10 * gdp_b
  )

within_terms <- c(
  "gini_w",
  "hate_w",
  "counterarg_w",
  "low_common_good_w",
  "disinformation_w",
  "issue_polarisation_w",
  "antagonistic_camps_w",
  "gdp_w",
  "unemployment_w"
)

centering_check <- context |>
  group_by(cntry) |>
  summarise(
    across(all_of(within_terms), mean),
    .groups = "drop"
  ) |>
  select(-cntry) |>
  as.matrix()

stopifnot(
  all(abs(centering_check) < 1e-8)
)

# 7. Attach components and individual-level controls ----------

age_mean <- mean(model_data$age)
education_mean <- mean(model_data$education_years)

analysis_data <- model_data |>
  left_join(
    context |>
      select(
        cntry, essround,
        gini_w, gini_b,
        hate_w, hate_b,
        counterarg_w, counterarg_b,
        low_common_good_w, low_common_good_b,
        disinformation_w, disinformation_b,
        issue_polarisation_w, issue_polarisation_b,
        antagonistic_camps_w, antagonistic_camps_b,
        gdp_w_10, gdp_b_10,
        unemployment_w, unemployment_b
      ),
    by = keys
  ) |>
  mutate(
    cntry = factor(cntry),
    country_round = interaction(
      cntry, essround, drop = TRUE
    ),
    round_factor = factor(essround),
    gender = factor(
      as.character(gender),
      levels = c("Man", "Woman")
    ),
    age_decade = (age - age_mean) / 10,
    age_decade2 = age_decade^2,
    education_c = education_years - education_mean
  )

# 8. Specify the seven models ----------------------------------

control <- lmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 200000),
  check.rankX = "stop.deficient"
)

m4_formula <-
  ppltrst ~
  gini_w + gini_b +
  gdp_w_10 + gdp_b_10 +
  unemployment_w + unemployment_b +
  round_factor +
  age_decade + age_decade2 +
  gender + education_c +
  (1 | cntry) + (1 | country_round)

model_formulas <- list(
  "M4 no discourse" = m4_formula,
  
  "M5 party hate" = update(
    m4_formula,
    . ~ . + hate_w + hate_b
  ),
  
  "A counterargument disrespect" = update(
    m4_formula,
    . ~ . + counterarg_w + counterarg_b
  ),
  
  "B low common-good framing" = update(
    m4_formula,
    . ~ . + low_common_good_w + low_common_good_b
  ),
  
  "C party disinformation" = update(
    m4_formula,
    . ~ . + disinformation_w + disinformation_b
  ),
  
  "D issue polarisation" = update(
    m4_formula,
    . ~ . + issue_polarisation_w + issue_polarisation_b
  ),
  
  "E antagonistic camps" = update(
    m4_formula,
    . ~ . + antagonistic_camps_w + antagonistic_camps_b
  )
)

# 9. Estimate and extract results -----------------------------

fixed_results <- list()
fit_results <- list()
diagnostic_results <- list()

for (model_name in names(model_formulas)) {
  
  cat("\nFitting:", model_name, "\n")
  
  captured_warnings <- character()
  
  fit <- withCallingHandlers(
    lmer(
      model_formulas[[model_name]],
      data = analysis_data,
      REML = FALSE,
      na.action = na.fail,
      control = control
    ),
    warning = function(w) {
      captured_warnings <<- c(
        captured_warnings,
        conditionMessage(w)
      )
    }
  )
  
  if (nobs(fit) != nrow(analysis_data)) {
    stop("Unexpected model sample: ", model_name)
  }
  
  coefficients <- as.data.frame(
    coef(summary(fit))
  ) |>
    rownames_to_column("term")
  
  fixed_results[[model_name]] <- coefficients |>
    transmute(
      model = model_name,
      term,
      estimate = .data[["Estimate"]],
      std_error = .data[["Std. Error"]],
      statistic = estimate / std_error,
      conf_low = estimate - 1.96 * std_error,
      conf_high = estimate + 1.96 * std_error,
      p_value = 2 * pnorm(
        abs(statistic),
        lower.tail = FALSE
      )
    )
  
  fit_results[[model_name]] <- tibble(
    model = model_name,
    n_respondents = nobs(fit),
    n_parameters = attr(logLik(fit), "df"),
    log_likelihood = as.numeric(logLik(fit)),
    AIC = AIC(fit),
    BIC = BIC(fit)
  )
  
  messages <- fit@optinfo$conv$lme4$messages
  optimizer_code <- fit@optinfo$conv$opt
  
  diagnostic_results[[model_name]] <- tibble(
    model = model_name,
    n_respondents = nobs(fit),
    singular = isSingular(fit, tol = 1e-4),
    optimizer_code =
      if (is.null(optimizer_code)) {
        NA_integer_
      } else {
        as.integer(optimizer_code)
      },
    convergence_message =
      if (is.null(messages)) {
        NA_character_
      } else {
        paste(messages, collapse = "; ")
      },
    warnings =
      if (!length(captured_warnings)) {
        NA_character_
      } else {
        paste(
          unique(captured_warnings),
          collapse = "; "
        )
      }
  )
  
  rm(fit)
  invisible(gc())
}

fixed_effects <- bind_rows(fixed_results)
model_fit <- bind_rows(fit_results)
model_diagnostics <- bind_rows(diagnostic_results)

# 10. Compare model fit ----------------------------------------
#
# ML likelihood-ratio tests compare each two-term extension
# against M4 on the identical respondent sample.
#
# AIC and BIC differences are relative to M4.
# These exploratory comparisons do not establish causal effects
# or measurement validity.

model_fit <- model_fit |>
  mutate(
    delta_aic_vs_m4 = AIC - first(AIC),
    delta_bic_vs_m4 = BIC - first(BIC),
    df_added = n_parameters - first(n_parameters),
    lr_chisq = pmax(
      0,
      2 * (log_likelihood - first(log_likelihood))
    ),
    lr_p_value = if_else(
      df_added > 0,
      pchisq(
        lr_chisq,
        df = pmax(df_added, 1),
        lower.tail = FALSE
      ),
      NA_real_
    )
  )

# 11. Extract the political-context coefficients ---------------

discourse_terms <- c(
  "hate_w", "hate_b",
  "counterarg_w", "counterarg_b",
  "low_common_good_w", "low_common_good_b",
  "disinformation_w", "disinformation_b",
  "issue_polarisation_w", "issue_polarisation_b",
  "antagonistic_camps_w", "antagonistic_camps_b"
)

discourse_results <- fixed_effects |>
  filter(term %in% discourse_terms)

# Standardise only the WITHIN-country coefficients using
# country-round SDs, not respondent-weighted SDs.

within_discourse_terms <- c(
  "hate_w",
  "counterarg_w",
  "low_common_good_w",
  "disinformation_w",
  "issue_polarisation_w",
  "antagonistic_camps_w"
)

within_scales <- tibble(
  term = within_discourse_terms,
  sd_within = vapply(
    within_discourse_terms,
    function(v) sd(context[[v]]),
    numeric(1)
  )
)

standardized_within <- discourse_results |>
  inner_join(within_scales, by = "term") |>
  mutate(
    estimate_per_sd = estimate * sd_within,
    se_per_sd = std_error * sd_within,
    conf_low_per_sd = conf_low * sd_within,
    conf_high_per_sd = conf_high * sd_within
  ) |>
  select(
    model, term, sd_within,
    estimate, std_error,
    conf_low, conf_high,
    estimate_per_sd, se_per_sd,
    conf_low_per_sd, conf_high_per_sd,
    p_value
  )

# 12. Round-adjusted within-country correlations ---------------
#
# Remove country means (already achieved in *_w).
# Then remove the common ESS-round effects from each series.
#
# Country-rounds are weighted equally.
# These correlations are descriptive, not causal.

residualized <- as.data.frame(
  lapply(within_discourse_terms, function(v) {
    resid(
      lm(
        reformulate(
          "factor(essround)",
          response = v
        ),
        data = context
      )
    )
  })
)

names(residualized) <- within_discourse_terms

correlation_matrix <- cor(residualized)

correlation_table <- as.data.frame(
  as.table(correlation_matrix)
) |>
  as_tibble() |>
  rename(
    indicator_1 = Var1,
    indicator_2 = Var2,
    correlation = Freq
  )

# 13. Check reproduction of previous models --------------------

previous_comparison <- tibble()

if (nrow(previous_results) > 0) {
  
  previous_comparison <- discourse_results |>
    inner_join(
      previous_results,
      by = c("model", "term")
    ) |>
    transmute(
      model,
      term,
      previous_estimate,
      reproduced_estimate = estimate,
      difference =
        reproduced_estimate - previous_estimate
    )
  
  if (
    nrow(previous_comparison) > 0 &&
    any(abs(previous_comparison$difference) > 1e-4)
  ) {
    warning(
      "Previous alternative-model estimates changed. ",
      "Inspect previous_comparison."
    )
  }
}

# Original M5 reproduction check, independent of the
# immediately preceding alternative-model run.

reference_comparison <- tibble()

if (file.exists(reference_file)) {
  
  reference <- read_csv(
    reference_file,
    show_col_types = FALSE
  ) |>
    filter(
      model == "M5",
      term %in% c(
        "gini_w", "gini_b",
        "hate_w", "hate_b"
      )
    ) |>
    select(
      term,
      original_estimate = estimate
    )
  
  reference_comparison <- fixed_effects |>
    filter(
      model == "M5 party hate",
      term %in% c(
        "gini_w", "gini_b",
        "hate_w", "hate_b"
      )
    ) |>
    select(
      term,
      reproduced_estimate = estimate
    ) |>
    left_join(reference, by = "term") |>
    mutate(
      difference =
        reproduced_estimate - original_estimate
    )
  
  if (
    anyNA(reference_comparison$original_estimate) ||
    any(
      abs(reference_comparison$difference) > 1e-4,
      na.rm = TRUE
    )
  ) {
    warning(
      "M5 reproduction differs from the saved reference. ",
      "Inspect reference_comparison."
    )
  }
}

# 14. Save compact tables --------------------------------------

write_csv(
  sample_summary,
  file.path(output_dir, "sample_summary.csv")
)

write_csv(
  fixed_effects,
  file.path(output_dir, "fixed_effects.csv")
)

write_csv(
  discourse_results,
  file.path(output_dir, "discourse_results.csv")
)

write_csv(
  standardized_within,
  file.path(output_dir, "standardized_within.csv")
)

write_csv(
  model_fit,
  file.path(output_dir, "model_fit.csv")
)

write_csv(
  model_diagnostics,
  file.path(output_dir, "model_diagnostics.csv")
)

write_csv(
  correlation_table,
  file.path(
    output_dir, "round_adjusted_correlations.csv"
  )
)

if (nrow(previous_comparison)) {
  write_csv(
    previous_comparison,
    file.path(output_dir, "previous_model_comparison.csv")
  )
}

if (nrow(reference_comparison)) {
  write_csv(
    reference_comparison,
    file.path(output_dir, "reference_m5_comparison.csv")
  )
}

# 15. Print decision-relevant results --------------------------
#
# Explicit tibble conversion prevents the base-R print error
# encountered in the earlier version.

cat("\n--- DISCOURSE COEFFICIENTS ---\n")
print(
  as_tibble(discourse_results),
  n = Inf, width = Inf
)

cat("\n--- WITHIN-COUNTRY EFFECTS PER SD ---\n")
print(
  as_tibble(standardized_within),
  n = Inf, width = Inf
)

cat("\n--- MODEL FIT ---\n")
print(
  as_tibble(model_fit),
  n = Inf, width = Inf
)

cat("\n--- ROUND-ADJUSTED CORRELATIONS ---\n")
print(
  round(correlation_matrix, 3)
)

cat("\n--- MODEL DIAGNOSTICS ---\n")
print(
  as_tibble(model_diagnostics),
  n = Inf, width = Inf
)

if (nrow(previous_comparison)) {
  cat("\n--- PREVIOUS MODEL REPRODUCTION ---\n")
  print(
    as_tibble(previous_comparison),
    n = Inf, width = Inf
  )
}

if (nrow(reference_comparison)) {
  cat("\n--- ORIGINAL M5 REPRODUCTION ---\n")
  print(
    as_tibble(reference_comparison),
    n = Inf, width = Inf
  )
}

cat(
  "\nCompleted. Tables saved to:\n",
  output_dir, "\n"
)
