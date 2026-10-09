
# ------------------------------------------------------------
# Joint political-discourse models
# Project: Inequality and social cohesion
#
# ESS Rounds 1-11; V-Dem v16
#
# Models:
#   M5: Party hate speech
#   C:  Party disinformation
#   J1: Party hate + disinformation
#   J2: Party hate + disinformation + common-good framing
#
# Purpose:
#   Test whether the within-country associations of party hate
#   and disinformation persist after mutual adjustment, and
#   whether common-good framing adds explanatory information.
#
# Design:
#   - Same established M5 respondents and country-rounds
#   - One-year-lagged V-Dem contextual exposures
#   - Within-between decomposition of contextual predictors
#   - GDP, unemployment, individual controls and round FE
#   - Country and country-round random intercepts
#   - ML estimation; no survey weights
#
# Outputs:
#   05_output/tables/discourse_joint_models/
#
# No earlier scripts, datasets or output tables are overwritten.
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
  "03_data", "interim",
  "ess_inequality_coverage.csv"
)

timing_post9_file <- here(
  "03_data", "interim",
  "ess_post9_country_round_year_coverage.csv"
)

reference_file <- here(
  "05_output", "tables",
  "discourse_alternative_models",
  "fixed_effects.csv"
)

output_dir <- here(
  "05_output", "tables", "discourse_joint_models"
)

required_files <- c(
  ess_file, macro_file, discourse_file,
  vdem_file, timing_base_file, timing_post9_file,
  reference_file
)

for (f in required_files) {
  if (!file.exists(f)) {
    stop("Required input missing: ", f)
  }
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

# 2. Established ESS and contextual inputs --------------------

ess <- readRDS(ess_file) |>
  select(
    cntry, essround, idno,
    ppltrst, gini_swiid_lag1,
    age, gender, education_years
  ) |>
  mutate(cntry = as.character(cntry))

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
    cntry, essround, party_hate_lag1
  )

check_keys(
  ess, c("cntry", "essround", "idno"),
  "ESS respondents"
)

check_keys(macro, keys, "Macro data")
check_keys(discourse, keys, "Discourse data")

# 3. Reconstruct two additional V-Dem exposures --------------
#
# Same fieldwork timing as our earlier audit.
#
# Higher party_disinformation = more disinformation.
# Higher low_common_good = less common-good justification.
#
# The existing party-hate exposure is retained unchanged.

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
    c(
      "country_name", "year",
      "v2smpardom", "v2dlcommon"
    )
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
    cntry, year, v2smpardom, v2dlcommon
  )

check_keys(
  vdem, c("cntry", "year"), "V-Dem"
)

unmatched <- ess |>
  distinct(cntry) |>
  anti_join(
    vdem |> distinct(cntry),
    by = "cntry"
  )

if (nrow(unmatched)) {
  print(unmatched, n = Inf)
  stop("Unmatched ESS countries in V-Dem.")
}

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
  filter(essround %in% 10:11)

timing <- bind_rows(
  timing_base, timing_post9
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
  stop("Invalid interview-year counts.")
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
    total = sum(share),
    .groups = "drop"
  )

stopifnot(
  all(abs(share_check$total - 1) < 1e-10)
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
    cols = c(v2smpardom, v2dlcommon),
    names_to = "indicator",
    values_to = "value"
  ) |>
  group_by(cntry, essround, indicator) |>
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
    names_from = indicator,
    values_from = exposure
  ) |>
  transmute(
    cntry, essround,
    party_disinformation_lag1 = -v2smpardom,
    low_common_good_lag1 = -v2dlcommon
  )

check_keys(
  new_exposures,
  keys,
  "New discourse exposures"
)

# 4. Establish the common complete-case sample ---------------

context_vars <- c(
  "gini_swiid_lag1",
  "party_hate_lag1",
  "party_disinformation_lag1",
  "low_common_good_lag1",
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
  n_later_respondents = sum(
    model_data$essround >= 10
  ),
  n_later_country_rounds = nrow(
    model_data |>
      filter(essround >= 10) |>
      distinct(cntry, essround)
  )
)

cat("\n--- SAMPLE SUMMARY ---\n")
print(sample_summary)

if (
  nrow(model_data) != 498461L ||
  n_distinct(model_data$cntry) != 38L ||
  nrow(distinct(model_data, cntry, essround)) != 273L
) {
  stop(
    "Sample does not reproduce M5. ",
    "Investigate before fitting models."
  )
}

if (!all(
  unique(as.character(model_data$gender)) %in%
    c("Man", "Woman")
)) {
  stop("Unexpected gender coding.")
}

# 5. Contextual within-between decomposition -----------------
#
# Compute country means using unique country-rounds.
# Grand-mean centre between-country components using
# one mean per country, not respondent-weighted averages.

context <- model_data |>
  distinct(
    cntry, essround,
    across(all_of(context_vars))
  )

check_keys(
  context, keys, "Model context"
)

variable_map <- c(
  gini = "gini_swiid_lag1",
  hate = "party_hate_lag1",
  disinformation = "party_disinformation_lag1",
  low_common_good = "low_common_good_lag1",
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

# Check the decomposition.

within_terms <- c(
  "gini_w", "hate_w", "disinformation_w",
  "low_common_good_w", "gdp_w", "unemployment_w"
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

# 6. Individual controls and model data -----------------------

age_mean <- mean(model_data$age)
education_mean <- mean(model_data$education_years)

analysis_data <- model_data |>
  left_join(
    context |>
      select(
        cntry, essround,
        gini_w, gini_b,
        hate_w, hate_b,
        disinformation_w, disinformation_b,
        low_common_good_w, low_common_good_b,
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

# 7. Model specifications -------------------------------------

control <- lmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 200000),
  check.rankX = "stop.deficient"
)

# Identical specification to the established alternative models.

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
  "M5 party hate" = update(
    m4_formula,
    . ~ . + hate_w + hate_b
  ),

  "C party disinformation" = update(
    m4_formula,
    . ~ . + disinformation_w + disinformation_b
  ),

  "J1 hate + disinformation" = update(
    m4_formula,
    . ~ . +
      hate_w + hate_b +
      disinformation_w + disinformation_b
  ),

  "J2 hate + disinformation + common good" = update(
    m4_formula,
    . ~ . +
      hate_w + hate_b +
      disinformation_w + disinformation_b +
      low_common_good_w + low_common_good_b
  )
)

# 8. Fit and extract results ----------------------------------

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
    stop(
      "Model used an unexpected sample: ",
      model_name
    )
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
        abs(statistic), lower.tail = FALSE
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
          unique(captured_warnings), collapse = "; "
        )
      }
  )

  rm(fit)
  invisible(gc())
}

fixed_effects <- bind_rows(fixed_results)
model_fit <- bind_rows(fit_results)
model_diagnostics <- bind_rows(diagnostic_results)

# 9. Focused discourse coefficients ---------------------------

discourse_terms <- c(
  "hate_w", "hate_b",
  "disinformation_w", "disinformation_b",
  "low_common_good_w", "low_common_good_b"
)

discourse_results <- fixed_effects |>
  filter(term %in% discourse_terms)

# Report per one country-round SD of within-country change.

within_terms <- c(
  "hate_w",
  "disinformation_w",
  "low_common_good_w"
)

within_scales <- tibble(
  term = within_terms,
  sd_within = vapply(
    within_terms,
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
    estimate_per_sd, se_per_sd,
    conf_low_per_sd, conf_high_per_sd,
    p_value
  )

# 10. Incremental likelihood-ratio tests ----------------------
#
# All comparisons are nested and use the same data.
# Each extension adds one within- and one between-country term.

get_fit_row <- function(model_name) {

  result <- model_fit |>
    filter(model == model_name)

  if (nrow(result) != 1L) {
    stop("Model not found uniquely: ", model_name)
  }

  result
}

compare_nested <- function(smaller, larger) {

  a <- get_fit_row(smaller)
  b <- get_fit_row(larger)

  df_added <- b$n_parameters - a$n_parameters

  if (
    a$n_respondents != b$n_respondents ||
    df_added <= 0
  ) {
    stop("Invalid nested-model comparison.")
  }

  lr_chisq <- 2 * (
    b$log_likelihood - a$log_likelihood
  )

  if (lr_chisq < -1e-5) {
    warning(
      "Larger model has lower likelihood: ",
      larger
    )
  }

  lr_chisq <- max(0, lr_chisq)

  tibble(
    smaller_model = smaller,
    larger_model = larger,
    df_added = df_added,
    chisq = lr_chisq,
    p_value = pchisq(
      lr_chisq,
      df = df_added,
      lower.tail = FALSE
    ),
    delta_aic = b$AIC - a$AIC,
    delta_bic = b$BIC - a$BIC
  )
}

incremental_tests <- bind_rows(
  compare_nested(
    "M5 party hate",
    "J1 hate + disinformation"
  ),
  compare_nested(
    "C party disinformation",
    "J1 hate + disinformation"
  ),
  compare_nested(
    "J1 hate + disinformation",
    "J2 hate + disinformation + common good"
  )
)

# 11. Round-adjusted contextual correlations -----------------
#
# De-meaned within-country measures, adjusted for common
# ESS-round effects. Each country-round has equal weight.

residualized <- as.data.frame(
  lapply(within_terms, function(v) {
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

names(residualized) <- within_terms

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

# 12. Reproduction of previous single-indicator models --------

reference <- read_csv(
  reference_file,
  show_col_types = FALSE
) |>
  filter(
    model %in% c(
      "M5 party hate",
      "C party disinformation"
    )
  ) |>
  select(
    model, term,
    previous_estimate = estimate
  )

reference_comparison <- fixed_effects |>
  filter(
    model %in% c(
      "M5 party hate",
      "C party disinformation"
    )
  ) |>
  select(
    model, term,
    reproduced_estimate = estimate
  ) |>
  left_join(
    reference,
    by = c("model", "term")
  ) |>
  mutate(
    difference =
      reproduced_estimate - previous_estimate
  )

if (
  anyNA(reference_comparison$previous_estimate) ||
  any(
    abs(reference_comparison$difference) > 1e-4,
    na.rm = TRUE
  )
) {
  stop(
    "Previous model reproduction failed. ",
    "Inspect reference_comparison before interpreting."
  )
}

# 13. Save all output tables ----------------------------------

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
  incremental_tests,
  file.path(output_dir, "incremental_tests.csv")
)

write_csv(
  correlation_table,
  file.path(output_dir, "round_adjusted_correlations.csv")
)

write_csv(
  model_diagnostics,
  file.path(output_dir, "model_diagnostics.csv")
)

write_csv(
  reference_comparison,
  file.path(output_dir, "reference_comparison.csv")
)

# 14. Concise console output ----------------------------------

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

cat("\n--- NESTED MODEL TESTS ---\n")
print(
  as_tibble(incremental_tests),
  n = Inf, width = Inf
)

cat("\n--- MODEL FIT ---\n")
print(
  as_tibble(model_fit),
  n = Inf, width = Inf
)

cat("\n--- ROUND-ADJUSTED CORRELATIONS ---\n")
print(round(correlation_matrix, 3))

cat("\n--- MODEL DIAGNOSTICS ---\n")
print(
  as_tibble(model_diagnostics),
  n = Inf, width = Inf
)

cat("\n--- REPRODUCTION CHECK ---\n")
print(
  as_tibble(
    reference_comparison |>
      filter(
        term %in% c(
          "hate_w", "hate_b",
          "disinformation_w", "disinformation_b",
          "gini_w", "gini_b"
        )
      )
  ),
  n = Inf, width = Inf
)

cat(
  "\nCompleted. Tables saved to:\n",
  output_dir, "\n"
)
