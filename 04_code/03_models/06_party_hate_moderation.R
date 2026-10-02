# Party hate-speech moderation
# Implements the working specification agreed on 1 October 2026.
#
# Outcome: generalised trust (ppltrst).
# Exposure: one-year-lagged, reversed V-Dem v2smpolhate.
# Samples: R1-R9 and R1-R11, prepared separately.
# Estimation: unweighted ML; country and country-round intercepts.
#
# All models within a period use identical complete cases.
# Contextual means use unique contributing country-rounds,
# with equal round weights within each country.
#
# Confidence intervals are approximate 95% Wald intervals.
# Exposure measurement uncertainty is not propagated here.

library(dplyr)
library(here)
library(lme4)
library(readr)
library(tibble)

# 1. Validation helpers ---------------------------------------------------

require_columns <- function(dat, columns, label) {
  missing <- setdiff(columns, names(dat))
  if (length(missing)) {
    stop(label, ": missing columns: ", paste(missing, collapse = ", "))
  }
}

check_keys <- function(dat, keys, label) {
  key_data <- as.data.frame(dat[, keys, drop = FALSE])
  if (anyNA(key_data) || anyDuplicated(key_data)) {
    stop(label, ": missing or duplicated keys.")
  }
}

context_vars <- c(
  "gini_swiid_lag1",
  "party_hate_lag1",
  "log_gdp_pc_ppp_lag1",
  "unemployment_lag1"
)

individual_vars <- c("ppltrst", "age", "education_years")
keys <- c("cntry", "essround")

# 2. Import ---------------------------------------------------------------

ess <- readRDS(
  here("03_data", "processed", "ess_analysis_r1_r11.rds")
)

ess_columns <- c(
  keys, "idno", individual_vars, "gender", "gini_swiid_lag1"
)
require_columns(ess, ess_columns, "ESS")
ess <- ess |> select(all_of(ess_columns))
check_keys(ess, c(keys, "idno"), "ESS respondents")

macro <- read_csv(
  here("03_data", "interim", "ess_country_round_macro_r1_r11.csv"),
  show_col_types = FALSE
) |>
  select(all_of(keys), log_gdp_pc_ppp_lag1, unemployment_lag1)

discourse <- read_csv(
  here("03_data", "interim", "ess_country_round_discourse_r1_r11.csv"),
  show_col_types = FALSE
) |>
  select(all_of(keys), party_hate_lag1)

check_keys(macro, keys, "Macro context")
check_keys(discourse, keys, "Discourse context")

# party_hate_lag1 is already reversed in the audit script.
# Do not reverse it again here.
ess <- ess |>
  left_join(macro, by = keys) |>
  left_join(discourse, by = keys)

stopifnot(all(ess$essround %in% 1:11))

numeric_vars <- c(context_vars, individual_vars)
if (!all(vapply(ess[numeric_vars], is.numeric, logical(1)))) {
  stop("Expected numeric contextual and individual variables.")
}

gender_values <- unique(na.omit(as.character(ess$gender)))
if (!all(gender_values %in% c("Man", "Woman"))) {
  stop("Unexpected gender coding; expected Man/Woman.")
}

ess <- ess |>
  mutate(
    gender = factor(
      as.character(gender),
      levels = c("Man", "Woman")
    )
  )

# Contextual values must be constant within country-rounds.
check_keys(
  ess |> distinct(across(all_of(c(keys, context_vars)))),
  keys,
  "Respondent-level contextual values"
)

# 3. Output directories ---------------------------------------------------

table_dir <- here("05_output", "tables", "party_hate_moderation")
model_dir <- here("05_output", "models", "party_hate_moderation")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

# 4. Prepare a common sample and decompose contextual variables -----------

sample_counts <- function(dat, sample_id, stage) {
  tibble(
    sample = sample_id,
    stage = stage,
    n_respondents = nrow(dat),
    n_countries = n_distinct(dat$cntry),
    n_country_rounds = nrow(distinct(dat, cntry, essround))
  )
}

prepare_period <- function(dat, sample_id) {
  context_complete <- dat |>
    filter(if_all(all_of(context_vars), ~ is.finite(.x)))

  model_dat <- context_complete |>
    filter(
      if_all(all_of(individual_vars), ~ is.finite(.x)),
      !is.na(gender)
    )

  if (!nrow(model_dat)) {
    stop(sample_id, ": no complete cases.")
  }

  sample_flow <- bind_rows(
    sample_counts(dat, sample_id, "Input"),
    sample_counts(context_complete, sample_id, "Context complete"),
    sample_counts(model_dat, sample_id, "Common model sample")
  )

  # Only country-rounds with at least one retained respondent contribute.
  context <- model_dat |>
    distinct(across(all_of(c(keys, context_vars))))

  variable_map <- c(
    gini = "gini_swiid_lag1",
    hate = "party_hate_lag1",
    gdp = "log_gdp_pc_ppp_lag1",
    unemployment = "unemployment_lag1"
  )

  centres <- list()

  for (prefix in names(variable_map)) {
    raw_name <- unname(variable_map[[prefix]])
    mean_name <- paste0(prefix, "_mean")
    within_name <- paste0(prefix, "_w")
    between_name <- paste0(prefix, "_b")

    context <- context |>
      group_by(cntry) |>
      mutate(
        !!mean_name := mean(.data[[raw_name]]),
        !!within_name := .data[[raw_name]] - .data[[mean_name]]
      ) |>
      ungroup()

    grand_mean <- context |>
      distinct(cntry, .data[[mean_name]]) |>
      summarise(value = mean(.data[[mean_name]])) |>
      pull(value)

    context[[between_name]] <- context[[mean_name]] - grand_mean

    centres[[prefix]] <- tibble(
      sample = sample_id,
      variable = raw_name,
      country_grand_mean = grand_mean
    )
  }

  # Decompose the within-within PRODUCT itself.
  context <- context |>
    mutate(gh_product = gini_w * hate_w) |>
    group_by(cntry) |>
    mutate(
      gh_mean = mean(gh_product),
      gh_w = gh_product - gh_mean,
      n_rounds = n(),
      gini_within_sd = if (n() > 1L) sd(gini_w) else NA_real_,
      hate_within_sd = if (n() > 1L) sd(hate_w) else NA_real_
    ) |>
    ungroup()

  product_grand_mean <- context |>
    distinct(cntry, gh_mean) |>
    summarise(value = mean(gh_mean)) |>
    pull(value)

  context <- context |>
    mutate(
      gh_b = gh_mean - product_grand_mean,
      # Its country mean is zero because hate_b is constant
      # within country and the country mean of gini_w is zero.
      gini_w_hate_b = gini_w * hate_b,
      gdp_w_10 = gdp_w * 10,
      gdp_b_10 = gdp_b * 10
    )

  centres$product <- tibble(
    sample = sample_id,
    variable = "gini_w * hate_w",
    country_grand_mean = product_grand_mean
  )

  # Check equal-country-round centring.
  centring_check <- context |>
    group_by(cntry) |>
    summarise(
      across(
        all_of(c(
          "gini_w", "hate_w", "gdp_w", "unemployment_w",
          "gh_w", "gini_w_hate_b"
        )),
        mean
      ),
      .groups = "drop"
    )

  if (any(abs(as.matrix(centring_check[, -1])) > 1e-8)) {
    stop(sample_id, ": contextual centring check failed.")
  }

  age_mean <- mean(model_dat$age)
  education_mean <- mean(model_dat$education_years)

  model_dat <- model_dat |>
    left_join(
      context |> select(-all_of(context_vars)),
      by = keys
    ) |>
    mutate(
      cntry = factor(cntry),
      country_round = interaction(cntry, essround, drop = TRUE),
      round_factor = factor(essround),
      age_decade = (age - age_mean) / 10,
      age_decade2 = age_decade^2,
      education_c = education_years - education_mean
    )

  list(
    data = model_dat,
    context = context |> mutate(sample = sample_id, .before = 1),
    flow = sample_flow,
    centres = bind_rows(centres),
    individual_centres = tibble(
      sample = sample_id,
      age_mean = age_mean,
      education_mean = education_mean
    )
  )
}

# 5. Model sequence -------------------------------------------------------

m4_formula <- ppltrst ~
  gini_w + gini_b +
  gdp_w_10 + gdp_b_10 +
  unemployment_w + unemployment_b +
  round_factor +
  age_decade + age_decade2 + gender + education_c +
  (1 | cntry) + (1 | country_round)

m5_formula <- update(
  m4_formula,
  . ~ . + hate_w + hate_b
)

m6_formula <- update(
  m5_formula,
  . ~ . + gh_w + gh_b
)

m7_formula <- update(
  m6_formula,
  . ~ . + gini_w_hate_b
)

formulas <- list(
  M4 = m4_formula,
  M5 = m5_formula,
  M6 = m6_formula,
  M7 = m7_formula
)

control <- lmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 200000),
  check.rankX = "stop.deficient"
)

# 6. Output helpers -------------------------------------------------------

extract_fixed <- function(fit, sample_id, model_name) {
  x <- as.data.frame(coef(summary(fit)))

  tibble(
    sample = sample_id,
    model = model_name,
    term = rownames(x),
    estimate = x[["Estimate"]],
    std_error = x[["Std. Error"]],
    statistic = x[["t value"]],
    conf_low = estimate - 1.96 * std_error,
    conf_high = estimate + 1.96 * std_error
  )
}

extract_diagnostics <- function(
  fit, sample_id, model_name, fit_warnings
) {
  messages <- fit@optinfo$conv$lme4$messages
  dropped <- attr(getME(fit, "X"), "col.dropped")

  tibble(
    sample = sample_id,
    model = model_name,
    n = nobs(fit),
    n_countries = nlevels(model.frame(fit)$cntry),
    n_country_rounds = nlevels(model.frame(fit)$country_round),
    log_likelihood = as.numeric(logLik(fit)),
    AIC = AIC(fit),
    BIC = BIC(fit),
    singular = isSingular(fit, tol = 1e-4),
    optimizer_code = paste(fit@optinfo$conv$opt, collapse = "; "),
    convergence_messages = paste(messages, collapse = "; "),
    warnings = paste(unique(fit_warnings), collapse = "; "),
    dropped_terms = paste(names(dropped), collapse = "; ")
  )
}

# Conditional slopes evaluated at observed moderator combinations.
#
# Country means, including gh_mean, are held fixed.
# These are model-based within-country Gini slopes, not estimates
# of changing one observed round and recomputing all country means.
#
# Countries without observed Gini variation are excluded from
# this descriptive slope table, but remain in model estimation.
extract_slopes <- function(fit, context, sample_id, model_name) {
  points <- context |>
    filter(n_rounds >= 2L, gini_within_sd > 1e-10)

  b <- fixef(fit)
  V <- as.matrix(vcov(fit))

  L <- matrix(
    0,
    nrow = nrow(points),
    ncol = length(b),
    dimnames = list(NULL, names(b))
  )

  L[, "gini_w"] <- 1
  L[, "gh_w"] <- points$hate_w

  if ("gini_w_hate_b" %in% names(b)) {
    L[, "gini_w_hate_b"] <- points$hate_b
  }

  estimate <- as.vector(L %*% b)
  variance <- rowSums((L %*% V) * L)
  std_error <- sqrt(pmax(variance, 0))

  points |>
    select(
      cntry, essround, n_rounds,
      party_hate_lag1, hate_w, hate_b,
      gini_within_sd, hate_within_sd
    ) |>
    mutate(
      sample = sample_id,
      model = model_name,
      estimate = estimate,
      std_error = std_error,
      conf_low = estimate - 1.96 * std_error,
      conf_high = estimate + 1.96 * std_error,
      .before = 1
    )
}

# 7. Fit and save each period ---------------------------------------------

run_period <- function(dat, sample_id, file_id) {
  prepared <- prepare_period(dat, sample_id)

  cat("\n", sample_id, ": sample flow\n", sep = "")
  print(prepared$flow, n = Inf, width = Inf)

  # Preserve the actual contextual design used for estimation.
  write_csv(
    prepared$context,
    file.path(table_dir, paste0(file_id, "_context.csv"))
  )
  write_csv(
    prepared$flow,
    file.path(table_dir, paste0(file_id, "_sample_flow.csv"))
  )
  write_csv(
    prepared$centres,
    file.path(table_dir, paste0(file_id, "_contextual_centres.csv"))
  )
  write_csv(
    prepared$individual_centres,
    file.path(table_dir, paste0(file_id, "_individual_centres.csv"))
  )

  fixed <- list()
  diagnostics <- list()
  variances <- list()
  slopes <- list()

  for (model_name in names(formulas)) {
    cat("\nFitting ", sample_id, " ", model_name, "...\n", sep = "")
    fit_warnings <- character()

    fit <- withCallingHandlers(
      lmer(
        formula = formulas[[model_name]],
        data = prepared$data,
        REML = FALSE,
        na.action = na.fail,
        control = control
      ),
      warning = function(w) {
        fit_warnings <<- c(fit_warnings, conditionMessage(w))
      }
    )

    stopifnot(nobs(fit) == nrow(prepared$data))

    # Save each fit immediately so completed fits survive a later error.
    saveRDS(
      fit,
      file.path(model_dir, paste0(file_id, "_", model_name, ".rds"))
    )

    fixed[[model_name]] <- extract_fixed(
      fit, sample_id, model_name
    )

    diagnostics[[model_name]] <- extract_diagnostics(
      fit, sample_id, model_name, fit_warnings
    )

    variances[[model_name]] <- as_tibble(as.data.frame(VarCorr(fit))) |>
      mutate(sample = sample_id, model = model_name, .before = 1)

    if (model_name %in% c("M6", "M7")) {
      slopes[[model_name]] <- extract_slopes(
        fit, prepared$context, sample_id, model_name
      )
    }

    # Refresh tables after every completed model.
    write_csv(
      bind_rows(fixed),
      file.path(table_dir, paste0(file_id, "_fixed_effects.csv"))
    )
    write_csv(
      bind_rows(diagnostics),
      file.path(table_dir, paste0(file_id, "_diagnostics.csv"))
    )
    write_csv(
      bind_rows(variances),
      file.path(table_dir, paste0(file_id, "_variance_components.csv"))
    )

    if (length(slopes)) {
      write_csv(
        bind_rows(slopes),
        file.path(table_dir, paste0(file_id, "_conditional_slopes.csv"))
      )
    }

    rm(fit)
    invisible(gc())
  }

  list(
    fixed = bind_rows(fixed),
    diagnostics = bind_rows(diagnostics),
    variances = bind_rows(variances),
    slopes = bind_rows(slopes),
    flow = prepared$flow
  )
}

results <- list(
  r1_r9 = run_period(
    ess |> filter(essround <= 9),
    sample_id = "R1-R9",
    file_id = "r1_r9"
  ),
  r1_r11 = run_period(
    ess,
    sample_id = "R1-R11",
    file_id = "r1_r11"
  )
)

# 8. Combined tables and console results ----------------------------------

fixed_effects <- bind_rows(lapply(results, function(x) x$fixed))
model_diagnostics <- bind_rows(lapply(results, function(x) x$diagnostics))
sample_flow <- bind_rows(lapply(results, function(x) x$flow))
conditional_slopes <- bind_rows(lapply(results, function(x) x$slopes))

focused_results <- fixed_effects |>
  filter(
    term %in% c(
      "gini_w", "gini_b", "hate_w", "hate_b",
      "gh_w", "gh_b", "gini_w_hate_b"
    )
  )

write_csv(
  focused_results,
  file.path(table_dir, "focused_results.csv")
)
write_csv(
  model_diagnostics,
  file.path(table_dir, "model_diagnostics.csv")
)
write_csv(
  sample_flow,
  file.path(table_dir, "sample_flow.csv")
)
write_csv(
  conditional_slopes,
  file.path(table_dir, "conditional_slopes.csv")
)

writeLines(
  capture.output(sessionInfo()),
  file.path(table_dir, "session_info.txt")
)

cat("\nSample flow:\n")
print(sample_flow, n = Inf, width = Inf)

cat("\nModel diagnostics:\n")
print(model_diagnostics, n = Inf, width = Inf)

cat("\nFocused coefficients:\n")
print(focused_results, n = Inf, width = Inf)