
# ------------------------------------------------------------
# Later-period heterogeneity in party disinformation and trust
# Project: Inequality and social cohesion
#
# PURPOSE
#   Extend the party-hate period interaction analysis to
#   party social-media disinformation (V-Dem v2smpardom).
#
# MAIN QUESTION
#   Does the within-country association between party
#   disinformation and generalised trust differ between
#   ESS Rounds 1-9 and ESS Rounds 10-11?
#
# DESIGN
#   - Reconstruct the M5 complete-case sample.
#   - Preserve the established country-round universe.
#   - Construct lagged disinformation exposure using the
#     established ESS interview-year timing weights.
#   - Reverse V-Dem direction: higher = more disinformation.
#   - Calculate contextual within-between components once
#     on the full M5 analytical country-round sample.
#   - Fit common-slope and period-interaction specifications.
#   - Repeat for all respondents and in-person respondents.
#   - Retain ESS-round fixed effects.
#   - Use ML for likelihood-ratio comparisons.
#
# NOTE
#   This is the disinformation analogue of M5, not the
#   original hate-speech M5 specification.
#
# Run from project root:
# Rscript 04_code/03_models/15_party_disinformation_period_interaction.R
#
# Outputs:
# 05_output/tables/party_disinformation_period_interaction/
# ------------------------------------------------------------

library(dplyr)
library(here)
library(lme4)
library(readr)
library(tibble)
library(tidyr)

# 1. Configuration ------------------------------------------------

keys <- c("cntry", "essround")

context_vars <- c(
  "gini_swiid_lag1",
  "party_hate_lag1",
  "log_gdp_pc_ppp_lag1",
  "unemployment_lag1"
)

individual_vars <- c(
  "ppltrst",
  "age",
  "education_years"
)

out_dir <- here(
  "05_output",
  "tables",
  "party_disinformation_period_interaction"
)

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

require_columns <- function(dat, columns, label) {
  missing <- setdiff(columns, names(dat))

  if (length(missing)) {
    stop(
      label, ": missing columns: ",
      paste(missing, collapse = ", ")
    )
  }
}

check_keys <- function(dat, columns, label) {
  x <- as.data.frame(
    dat[, columns, drop = FALSE]
  )

  if (anyNA(x) || anyDuplicated(x)) {
    stop(label, ": missing or duplicate keys.")
  }
}

# 2. Reconstruct the established M5 sample -----------------------

ess <- readRDS(
  here(
    "03_data",
    "processed",
    "ess_analysis_r1_r11.rds"
  )
)

require_columns(
  ess,
  c(
    keys,
    "idno",
    "interview_mode",
    "gini_swiid_lag1",
    individual_vars,
    "gender"
  ),
  "ESS"
)

ess <- ess |>
  select(all_of(c(
    keys,
    "idno",
    "interview_mode",
    "gini_swiid_lag1",
    individual_vars,
    "gender"
  )))

check_keys(
  ess,
  c(keys, "idno"),
  "ESS respondents"
)

if (!all(ess$essround %in% seq_len(11))) {
  stop("Unexpected ESS round values.")
}

if (any(
  ess$essround <= 9 &
    (
      is.na(ess$interview_mode) |
        ess$interview_mode != "In-person"
    )
)) {
  stop("Unexpected R1-R9 interview-mode labels.")
}

macro <- read_csv(
  here(
    "03_data",
    "interim",
    "ess_country_round_macro_r1_r11.csv"
  ),
  show_col_types = FALSE
) |>
  select(
    all_of(keys),
    log_gdp_pc_ppp_lag1,
    unemployment_lag1
  )

discourse <- read_csv(
  here(
    "03_data",
    "interim",
    "ess_country_round_discourse_r1_r11.csv"
  ),
  show_col_types = FALSE
) |>
  select(
    all_of(keys),
    party_hate_lag1
  )

check_keys(macro, keys, "Macro")
check_keys(discourse, keys, "Discourse")

ess <- ess |>
  left_join(macro, by = keys) |>
  left_join(discourse, by = keys)

if (!all(vapply(
  ess[c(context_vars, individual_vars)],
  is.numeric,
  logical(1)
))) {
  stop("Expected numeric contextual and outcome measures.")
}

if (!all(
  unique(na.omit(as.character(ess$gender))) %in%
    c("Man", "Woman")
)) {
  stop("Unexpected gender coding.")
}

# Match the M5 inclusion criteria exactly.
context_complete <- ess |>
  filter(
    if_all(all_of(context_vars), is.finite)
  )

model_rows <- context_complete |>
  filter(
    if_all(all_of(individual_vars), is.finite),
    !is.na(gender)
  )

if (!nrow(model_rows)) {
  stop("No complete M5 observations.")
}

# Established R1-R11 M5 sample from the previous audit.
expected_n <- 498461L
expected_countries <- 38L
expected_country_rounds <- 273L

observed_countries <- n_distinct(model_rows$cntry)

observed_country_rounds <- nrow(
  distinct(model_rows, cntry, essround)
)

if (
  nrow(model_rows) != expected_n ||
  observed_countries != expected_countries ||
  observed_country_rounds != expected_country_rounds
) {
  stop(
    "Established M5 sample does not match expectations. ",
    "Observed N=", nrow(model_rows),
    ", countries=", observed_countries,
    ", country-rounds=", observed_country_rounds,
    ". Investigate before proceeding."
  )
}

sample_flow <- bind_rows(
  tibble(
    stage = "Raw ESS input",
    n_respondents = nrow(ess),
    n_countries = n_distinct(ess$cntry),
    n_country_rounds = nrow(
      distinct(ess, cntry, essround)
    )
  ),
  tibble(
    stage = "M5 contextual complete cases",
    n_respondents = nrow(context_complete),
    n_countries = n_distinct(context_complete$cntry),
    n_country_rounds = nrow(
      distinct(context_complete, cntry, essround)
    )
  ),
  tibble(
    stage = "Established M5 complete cases",
    n_respondents = nrow(model_rows),
    n_countries = observed_countries,
    n_country_rounds = observed_country_rounds
  )
)

# 3. Construct lagged disinformation exposure --------------------

# Same timing sources used by the discourse comparison audit.

timing_r1_r9 <- read_csv(
  here(
    "03_data",
    "interim",
    "ess_inequality_coverage.csv"
  ),
  show_col_types = FALSE
) |>
  select(
    cntry,
    essround,
    interview_year,
    n_respondents
  ) |>
  filter(essround <= 9)

timing_post9 <- read_csv(
  here(
    "03_data",
    "interim",
    "ess_post9_country_round_year_coverage.csv"
  ),
  show_col_types = FALSE
) |>
  select(
    cntry,
    essround,
    interview_year,
    n_respondents
  ) |>
  filter(
    essround >= 10,
    essround <= 11
  )

timing <- bind_rows(
  timing_r1_r9,
  timing_post9
) |>
  filter(!is.na(interview_year))

check_keys(
  timing,
  c(keys, "interview_year"),
  "ESS interview-year timing"
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
    share = n_respondents / sum(n_respondents),
    exposure_year = as.integer(interview_year) - 1L
  ) |>
  ungroup()

share_check <- timing |>
  group_by(cntry, essround) |>
  summarise(
    share_sum = sum(share),
    .groups = "drop"
  )

if (any(abs(share_check$share_sum - 1) > 1e-10)) {
  stop("Interview-year shares do not sum to one.")
}

# V-Dem country matching: same correspondence used by
# 05_discourse_indicator_comparison.R.

country_lookup <- tribble(
  ~country_name,         ~cntry,
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

vdem_path <- here(
  "03_data",
  "raw",
  "vdem",
  "V-Dem-CY-Full+Others-v16.csv"
)

vdem_header <- names(
  read_csv(
    vdem_path,
    n_max = 0,
    show_col_types = FALSE
  )
)

require_columns(
  as_tibble(setNames(
    rep(list(numeric(0)), length(vdem_header)),
    vdem_header
  )),
  c("country_name", "year", "v2smpardom"),
  "V-Dem"
)

vdem <- read_csv(
  vdem_path,
  col_select = all_of(c(
    "country_name",
    "year",
    "v2smpardom"
  )),
  show_col_types = FALSE
) |>
  inner_join(
    country_lookup,
    by = "country_name"
  ) |>
  semi_join(
    model_rows |> distinct(cntry),
    by = "cntry"
  )

check_keys(
  vdem,
  c("cntry", "year"),
  "V-Dem country-years"
)

# v2smpardom: higher raw values indicate LESS disinformation.
# Reverse after annual aggregation for comparability with the
# previous descriptive audit.

disinfo_exposure <- timing |>
  semi_join(
    model_rows |> distinct(cntry, essround),
    by = keys
  ) |>
  left_join(
    vdem |>
      select(cntry, year, v2smpardom),
    by = c(
      "cntry",
      "exposure_year" = "year"
    )
  ) |>
  group_by(cntry, essround) |>
  summarise(
    disinfo_lag1 = if (all(is.finite(v2smpardom))) {
      -weighted.mean(v2smpardom, share)
    } else {
      NA_real_
    },
    n_exposure_years = n(),
    .groups = "drop"
  )

check_keys(
  disinfo_exposure,
  keys,
  "Disinformation exposure"
)

# 4. Enforce common analytical sample ----------------------------

model_rows <- model_rows |>
  left_join(
    disinfo_exposure |>
      select(all_of(keys), disinfo_lag1),
    by = keys
  )

missing_disinfo <- model_rows |>
  filter(!is.finite(disinfo_lag1)) |>
  distinct(cntry, essround)

if (nrow(missing_disinfo)) {
  print(missing_disinfo, n = Inf)

  stop(
    "Disinformation exposure missing on established M5 ",
    "country-rounds. Do not silently shrink the sample."
  )
}

stopifnot(
  nrow(model_rows) == expected_n,
  n_distinct(model_rows$cntry) == expected_countries,
  nrow(distinct(model_rows, cntry, essround)) ==
    expected_country_rounds
)

# 5. Within-between contextual decomposition ---------------------

# Equal weight to each observed country-round in constructing
# country means, matching the previous M5 implementation.

context <- model_rows |>
  distinct(
    across(all_of(c(
      keys,
      context_vars,
      "disinfo_lag1"
    )))
  )

check_keys(
  context,
  keys,
  "Analytical context"
)

variable_map <- c(
  gini = "gini_swiid_lag1",
  disinfo = "disinfo_lag1",
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

  grand_mean <- context |>
    distinct(cntry, .data[[mean_name]]) |>
    summarise(
      value = mean(.data[[mean_name]])
    ) |>
    pull(value)

  context[[between_name]] <-
    context[[mean_name]] - grand_mean
}

context <- context |>
  mutate(
    gdp_w_10 = 10 * gdp_w,
    gdp_b_10 = 10 * gdp_b,
    post_r9 = as.integer(essround >= 10)
  )

centring_check <- context |>
  group_by(cntry) |>
  summarise(
    across(
      all_of(c(
        "gini_w",
        "disinfo_w",
        "gdp_w",
        "unemployment_w"
      )),
      mean
    ),
    .groups = "drop"
  )

if (any(
  abs(as.matrix(centring_check[, -1])) > 1e-8
)) {
  stop("Within-country centring check failed.")
}

# 6. Construct all-mode and in-person samples --------------------

age_mean <- mean(model_rows$age)
education_mean <- mean(model_rows$education_years)

all_data <- model_rows |>
  left_join(
    context |>
      select(
        all_of(keys),
        gini_w, gini_b,
        disinfo_w, disinfo_b,
        gdp_w_10, gdp_b_10,
        unemployment_w, unemployment_b,
        post_r9
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

inperson_data <- all_data |>
  filter(interview_mode == "In-person") |>
  mutate(
    cntry = droplevels(cntry),
    country_round = droplevels(country_round),
    round_factor = droplevels(round_factor)
  )

# Do not recalculate contextual decompositions or individual
# centring in the in-person subsample.

summarise_sample <- function(dat, label) {
  tibble(
    sample = label,
    n_respondents = nrow(dat),
    n_countries = n_distinct(as.character(dat$cntry)),
    n_country_rounds = nrow(
      distinct(dat, cntry, essround)
    ),
    n_later_respondents = sum(dat$post_r9 == 1),
    n_later_country_rounds = nrow(
      distinct(
        filter(dat, post_r9 == 1),
        cntry, essround
      )
    )
  )
}

sample_summary <- bind_rows(
  summarise_sample(all_data, "All modes"),
  summarise_sample(inperson_data, "In-person only")
)

mode_counts <- model_rows |>
  count(
    essround,
    interview_mode,
    name = "n_respondents"
  )

period_variation <- context |>
  mutate(
    period = if_else(
      post_r9 == 1,
      "R10-R11",
      "R1-R9"
    )
  ) |>
  group_by(period) |>
  summarise(
    n_country_rounds = n(),
    n_countries = n_distinct(cntry),
    mean_disinfo_w = mean(disinfo_w),
    sd_disinfo_w = sd(disinfo_w),
    min_disinfo_w = min(disinfo_w),
    max_disinfo_w = max(disinfo_w),
    .groups = "drop"
  )

if (any(
  !is.finite(period_variation$sd_disinfo_w) |
    period_variation$sd_disinfo_w < 1e-10
)) {
  stop("Insufficient within-country disinformation variation.")
}

# Compare sample counts with the earlier hate-speech analysis.

hate_summary_path <- here(
  "05_output",
  "tables",
  "party_hate_period_interaction",
  "sample_summary.csv"
)

sample_comparison <- tibble()

if (file.exists(hate_summary_path)) {

  hate_summary <- read_csv(
    hate_summary_path,
    show_col_types = FALSE
  )

  sample_comparison <- sample_summary |>
    inner_join(
      hate_summary,
      by = "sample",
      suffix = c("_disinfo", "_hate")
    )

  if (
    nrow(sample_comparison) != nrow(sample_summary) ||
    any(
      sample_comparison$n_respondents_disinfo !=
        sample_comparison$n_respondents_hate
    ) ||
    any(
      sample_comparison$n_country_rounds_disinfo !=
        sample_comparison$n_country_rounds_hate
    )
  ) {
    stop(
      "Disinformation and hate-speech model samples differ."
    )
  }
}

# 7. Model specifications ----------------------------------------

ctrl <- lmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 200000),
  check.rankX = "stop.deficient"
)

# Original M5 adjustment set, replacing hate_w/hate_b with
# disinfo_w/disinfo_b.

common_formula <-
  ppltrst ~
  gini_w + gini_b +
  gdp_w_10 + gdp_b_10 +
  unemployment_w + unemployment_b +
  round_factor +
  age_decade + age_decade2 +
  gender + education_c +
  disinfo_w + disinfo_b +
  (1 | cntry) +
  (1 | country_round)

# No main post_r9 term: absorbed by ESS-round fixed effects.

period_formula <- update(
  common_formula,
  . ~ . + disinfo_w:post_r9
)

fit_one <- function(formula, dat) {

  captured_warnings <- character()

  fit <- withCallingHandlers(
    lmer(
      formula,
      data = dat,
      REML = FALSE,
      na.action = na.fail,
      control = ctrl
    ),
    warning = function(w) {
      captured_warnings <<- c(
        captured_warnings,
        conditionMessage(w)
      )
    }
  )

  if (nobs(fit) != nrow(dat)) {
    stop("Model estimation changed the analytical sample.")
  }

  list(
    fit = fit,
    warnings = unique(captured_warnings)
  )
}

# 8. Results extraction helpers ----------------------------------

linear_combo <- function(
  fit,
  weights,
  sample_name,
  effect
) {

  b <- fixef(fit)

  if (!all(names(weights) %in% names(b))) {
    stop("Required coefficient missing in linear combination.")
  }

  V <- as.matrix(vcov(fit))

  terms <- names(weights)
  w <- unname(weights)

  estimate <- sum(w * b[terms])

  std_error <- sqrt(as.numeric(
    t(w) %*%
      V[terms, terms, drop = FALSE] %*%
      w
  ))

  statistic <- estimate / std_error

  tibble(
    sample = sample_name,
    model = "M5 disinformation analogue",
    effect = effect,
    estimate = estimate,
    std_error = std_error,
    statistic = statistic,
    conf_low = estimate - 1.96 * std_error,
    conf_high = estimate + 1.96 * std_error,
    p_value = 2 * pnorm(
      abs(statistic),
      lower.tail = FALSE
    )
  )
}

extract_slopes <- function(fit, sample_name) {

  names_b <- names(fixef(fit))

  possible <- c(
    "disinfo_w:post_r9",
    "post_r9:disinfo_w"
  )

  interaction_term <- intersect(
    possible,
    names_b
  )

  if (length(interaction_term) != 1L) {
    stop("Expected disinformation-period interaction missing.")
  }

  bind_rows(
    linear_combo(
      fit,
      c(disinfo_w = 1),
      sample_name,
      "R1-R9 slope"
    ),
    linear_combo(
      fit,
      setNames(
        c(1, 1),
        c("disinfo_w", interaction_term)
      ),
      sample_name,
      "R10-R11 slope"
    ),
    linear_combo(
      fit,
      setNames(1, interaction_term),
      sample_name,
      "R10-R11 minus R1-R9"
    )
  )
}

extract_lrt <- function(
  base,
  interaction,
  sample_name
) {

  tab <- anova(base, interaction)

  tibble(
    sample = sample_name,
    model = "M5 disinformation analogue",
    chisq = as.numeric(tab[["Chisq"]][2]),
    df = as.numeric(tab[["Df"]][2]),
    p_value = as.numeric(tab[["Pr(>Chisq)"]][2]),
    AIC_base = AIC(base),
    AIC_interaction = AIC(interaction),
    BIC_base = BIC(base),
    BIC_interaction = BIC(interaction)
  )
}

extract_diagnostics <- function(
  obj,
  sample_name,
  specification
) {

  fit <- obj$fit

  messages <- fit@optinfo$conv$lme4$messages
  optimizer <- fit@optinfo$conv$opt
  dropped <- attr(
    getME(fit, "X"),
    "col.dropped"
  )

  tibble(
    sample = sample_name,
    specification = specification,
    n_respondents = nobs(fit),
    singular = isSingular(fit, tol = 1e-4),
    optimizer_code = if (is.null(optimizer)) {
      NA_integer_
    } else {
      as.integer(optimizer)
    },
    convergence_message = if (is.null(messages)) {
      NA_character_
    } else {
      paste(messages, collapse = "; ")
    },
    warnings = if (!length(obj$warnings)) {
      NA_character_
    } else {
      paste(obj$warnings, collapse = "; ")
    },
    dropped_terms = if (is.null(dropped)) {
      NA_character_
    } else {
      paste(names(dropped), collapse = "; ")
    }
  )
}

extract_baseline <- function(fit, sample_name) {

  x <- as.data.frame(
    coef(summary(fit))
  )

  tibble(
    sample = sample_name,
    model = "M5 disinformation analogue",
    term = rownames(x),
    estimate = x[["Estimate"]],
    std_error = x[["Std. Error"]]
  ) |>
    filter(
      term %in% c(
        "gini_w",
        "gini_b",
        "disinfo_w",
        "disinfo_b"
      )
    )
}

# 9. Estimate models ---------------------------------------------

results <- list()

for (s in c("All modes", "In-person only")) {

  dat <- if (s == "All modes") {
    all_data
  } else {
    inperson_data
  }

  cat("\nFitting ", s, ": common slope...\n", sep = "")

  base <- fit_one(
    common_formula,
    dat
  )

  cat(
    "Fitting ", s,
    ": disinfo_w x post_r9...\n",
    sep = ""
  )

  interaction <- fit_one(
    period_formula,
    dat
  )

  results[[s]] <- list(
    slopes = extract_slopes(
      interaction$fit,
      s
    ),
    lrt = extract_lrt(
      base$fit,
      interaction$fit,
      s
    ),
    diagnostics = bind_rows(
      extract_diagnostics(
        base,
        s,
        "Common slope"
      ),
      extract_diagnostics(
        interaction,
        s,
        "R10-R11 interaction"
      )
    ),
    baseline = extract_baseline(
      base$fit,
      s
    )
  )

  rm(base, interaction)
  invisible(gc())
}

period_slopes <- bind_rows(
  lapply(results, `[[`, "slopes")
)

interaction_tests <- period_slopes |>
  filter(
    effect == "R10-R11 minus R1-R9"
  )

interaction_lrt <- bind_rows(
  lapply(results, `[[`, "lrt")
)

model_diagnostics <- bind_rows(
  lapply(results, `[[`, "diagnostics")
)

base_coefficients <- bind_rows(
  lapply(results, `[[`, "baseline")
)

# Descriptive comparison only: samples overlap.

mode_comparison <- tibble(
  model = "M5 disinformation analogue",
  all_mode_estimate = interaction_tests$estimate[
    interaction_tests$sample == "All modes"
  ],
  all_mode_se = interaction_tests$std_error[
    interaction_tests$sample == "All modes"
  ],
  inperson_estimate = interaction_tests$estimate[
    interaction_tests$sample == "In-person only"
  ],
  inperson_se = interaction_tests$std_error[
    interaction_tests$sample == "In-person only"
  ]
) |>
  mutate(
    inperson_minus_all_mode =
      inperson_estimate - all_mode_estimate
  )

# 10. Optional comparison with party hate ------------------------

hate_slopes_path <- here(
  "05_output",
  "tables",
  "party_hate_period_interaction",
  "period_slopes.csv"
)

rhetoric_comparison <- tibble()

if (file.exists(hate_slopes_path)) {

  hate_slopes <- read_csv(
    hate_slopes_path,
    show_col_types = FALSE
  )

  rhetoric_comparison <- bind_rows(
    hate_slopes |>
      mutate(
        indicator = "Party hate speech",
        .before = 1
      ),
    period_slopes |>
      mutate(
        indicator = "Party disinformation",
        .before = 1
      )
  )
}

# 11. Save outputs -----------------------------------------------

write_csv(
  sample_flow,
  file.path(out_dir, "sample_flow.csv")
)

write_csv(
  sample_summary,
  file.path(out_dir, "sample_summary.csv")
)

write_csv(
  mode_counts,
  file.path(out_dir, "mode_counts.csv")
)

write_csv(
  period_variation,
  file.path(out_dir, "period_variation.csv")
)

write_csv(
  period_slopes,
  file.path(out_dir, "period_slopes.csv")
)

write_csv(
  interaction_tests,
  file.path(out_dir, "interaction_tests.csv")
)

write_csv(
  interaction_lrt,
  file.path(out_dir, "interaction_lrt.csv")
)

write_csv(
  model_diagnostics,
  file.path(out_dir, "model_diagnostics.csv")
)

write_csv(
  base_coefficients,
  file.path(out_dir, "base_coefficients.csv")
)

write_csv(
  mode_comparison,
  file.path(out_dir, "mode_comparison.csv")
)

if (nrow(sample_comparison)) {
  write_csv(
    sample_comparison,
    file.path(out_dir, "sample_comparison_with_hate.csv")
  )
}

if (nrow(rhetoric_comparison)) {
  write_csv(
    rhetoric_comparison,
    file.path(out_dir, "rhetoric_period_slopes_comparison.csv")
  )
}

# 12. Concise console output -------------------------------------

cat("\n--- MODEL SAMPLE ---\n")
print(sample_summary, n = Inf, width = Inf)

cat("\n--- DISINFORMATION VARIATION ---\n")
print(period_variation, n = Inf, width = Inf)

cat("\n--- BASELINE COEFFICIENTS ---\n")
print(base_coefficients, n = Inf, width = Inf)

cat("\n--- PERIOD-SPECIFIC SLOPES ---\n")
print(period_slopes, n = Inf, width = Inf)

cat("\n--- LIKELIHOOD-RATIO TESTS ---\n")
print(interaction_lrt, n = Inf, width = Inf)

cat("\n--- MODEL DIAGNOSTICS ---\n")
print(model_diagnostics, n = Inf, width = Inf)

cat("\nCompleted. Outputs saved to:\n", out_dir, "\n")
