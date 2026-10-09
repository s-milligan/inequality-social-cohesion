# ------------------------------------------------------------
# Later-period heterogeneity in party hate speech and trust
# Project: Inequality and social cohesion
#
# Question:
#   Does the within-country party-hate/trust association differ
#   between ESS R1-R9 and ESS R10-R11?
#
# Specification:
#   Original M5 from 06_party_hate_moderation.R, plus one
#   interaction: hate_w : post_r9 (post_r9 = 1 for R10-R11).
#
# Design:
#   - Construct complete-case M5 sample across R1-R11.
#   - Use one observation per country-round for within-between
#     country-level contextual decomposition.
#   - Construct all contextual components and centre individual
#     controls ONCE, before the in-person restriction.
#   - Fit base and interaction M5 on all modes and in-person only.
#   - Same sample within each likelihood-ratio model comparison.
#   - Retain ESS round fixed effects; post_r9 main effect is
#     collinear with these and is deliberately omitted.
#   - No re-imputation and no survey weights (as in M5).
#
# Cautions:
#   - Observational, not a post-2020 causal effect.
#   - V-Dem series has already been reversed upstream so larger
#     party_hate_lag1 values mean MORE party hate speech.
#   - The periods coincide with other changes in context and
#     survey implementation. In-person restriction is a check,
#     not proof of complete measurement comparability.
#
# Run from project root:
#   Rscript 04_code/03_models/14_party_hate_period_interaction.R
# ------------------------------------------------------------

library(dplyr)
library(here)
library(lme4)
library(readr)
library(tibble)

out_dir <- here("05_output", "tables", "party_hate_period_interaction")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 1. Inputs and checks -----------------------------------------------------
keys <- c("cntry", "essround")
context_vars <- c(
  "gini_swiid_lag1", "party_hate_lag1",
  "log_gdp_pc_ppp_lag1", "unemployment_lag1"
)
individual_vars <- c("ppltrst", "age", "education_years")

require_columns <- function(dat, columns, label) {
  missing <- setdiff(columns, names(dat))
  if (length(missing)) {
    stop(label, ": missing columns: ", paste(missing, collapse = ", "))
  }
}

check_keys <- function(dat, columns, label) {
  x <- as.data.frame(dat[, columns, drop = FALSE])
  if (anyNA(x) || anyDuplicated(x)) {
    stop(label, ": missing or duplicate keys")
  }
}

ess <- readRDS(
  here("03_data", "processed", "ess_analysis_r1_r11.rds")
)
require_columns(
  ess,
  c(keys, "idno", "interview_mode", "gini_swiid_lag1",
    individual_vars, "gender"),
  "ESS"
)
ess <- ess |>
  select(all_of(c(
    keys, "idno", "interview_mode", "gini_swiid_lag1",
    individual_vars, "gender"
  )))
check_keys(ess, c(keys, "idno"), "ESS respondents")
if (!all(ess$essround %in% seq_len(11))) {
  stop("Unexpected ESS round values.")
}
if (any(ess$essround <= 9 &
        (is.na(ess$interview_mode) | ess$interview_mode != "In-person"))) {
  stop("R1-R9 contains unexpected interview-mode labels.")
}

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

check_keys(macro, keys, "Macro data")
check_keys(discourse, keys, "Discourse data")

ess <- ess |>
  left_join(macro, by = keys) |>
  left_join(discourse, by = keys)

if (!all(vapply(ess[c(context_vars, individual_vars)], is.numeric, logical(1)))) {
  stop("Expected numeric contextual and individual-level measures.")
}
if (!all(unique(na.omit(as.character(ess$gender))) %in% c("Man", "Woman"))) {
  stop("Unexpected gender coding; expected Man/Woman.")
}

check_keys(
  ess |> distinct(across(all_of(c(keys, context_vars)))),
  keys,
  "Contextual variables attached to respondents"
)

# 2. Form common M5 sample BEFORE restricting interview mode --------------
sample_flow <- tibble(
  stage = "Raw input",
  n_respondents = nrow(ess),
  n_countries = n_distinct(ess$cntry),
  n_country_rounds = nrow(distinct(ess, cntry, essround))
)

context_complete <- ess |>
  filter(if_all(all_of(context_vars), ~ is.finite(.x)))

individual_complete <- context_complete |>
  filter(
    if_all(all_of(individual_vars), ~ is.finite(.x)),
    !is.na(gender)
  )

if (!nrow(individual_complete)) stop("No complete M5 respondents.")

sample_flow <- bind_rows(
  sample_flow,
  tibble(
    stage = c("Context complete", "All-mode M5 complete cases"),
    n_respondents = c(nrow(context_complete), nrow(individual_complete)),
    n_countries = c(n_distinct(context_complete$cntry),
                    n_distinct(individual_complete$cntry)),
    n_country_rounds = c(
      nrow(distinct(context_complete, cntry, essround)),
      nrow(distinct(individual_complete, cntry, essround))
    )
  )
)

# This is the same equal-country-round decomposition used by M5.
# The reference contextual design is the all-mode COMPLETE-CASE sample.
context <- individual_complete |>
  distinct(across(all_of(c(keys, context_vars))))

variable_map <- c(
  gini = "gini_swiid_lag1", hate = "party_hate_lag1",
  gdp = "log_gdp_pc_ppp_lag1", unemployment = "unemployment_lag1"
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
      !!within_name := .data[[raw_name]] - .data[[mean_name]]
    ) |>
    ungroup()

  grand_mean <- context |>
    distinct(cntry, .data[[mean_name]]) |>
    summarise(value = mean(.data[[mean_name]])) |>
    pull(value)

  context[[between_name]] <- context[[mean_name]] - grand_mean
}

# The original M5 includes no Gini x party-hate product. As in the
# source specification, GDP is rescaled after decomposition.
context <- context |>
  mutate(
    gdp_w_10 = 10 * gdp_w,
    gdp_b_10 = 10 * gdp_b,
    post_r9 = as.integer(essround >= 10)
  )

centring_check <- context |>
  group_by(cntry) |>
  summarise(
    across(all_of(c("gini_w", "hate_w", "gdp_w", "unemployment_w")), mean),
    .groups = "drop"
  )
if (any(abs(as.matrix(centring_check[, -1])) > 1e-8)) {
  stop("Contextual centring check failed.")
}

age_mean <- mean(individual_complete$age)
education_mean <- mean(individual_complete$education_years)

all_data <- individual_complete |>
  left_join(
    context |>
      select(
        all_of(keys),
        gini_w, gini_b, hate_w, hate_b,
        gdp_w_10, gdp_b_10,
        unemployment_w, unemployment_b, post_r9
      ),
    by = keys
  ) |>
  mutate(
    cntry = factor(cntry),
    country_round = interaction(cntry, essround, drop = TRUE),
    round_factor = factor(essround),
    gender = factor(as.character(gender), levels = c("Man", "Woman")),
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

# Same country-round contextual design and individual centring in BOTH
# samples: no recalculation following respondent-mode restriction.
summarise_sample <- function(dat, sample_name) {
  tibble(
    sample = sample_name,
    n_respondents = nrow(dat),
    n_countries = n_distinct(as.character(dat$cntry)),
    n_country_rounds = nrow(distinct(dat, cntry, essround)),
    n_later_respondents = sum(dat$post_r9 == 1),
    n_later_country_rounds = nrow(
      distinct(filter(dat, post_r9 == 1), cntry, essround)
    )
  )
}

sample_summary <- bind_rows(
  summarise_sample(all_data, "All modes"),
  summarise_sample(inperson_data, "In-person only")
)

# Diagnoses the actual availability and country-round-level variation.
period_variation <- context |>
  mutate(period = if_else(post_r9 == 1, "R10-R11", "R1-R9")) |>
  group_by(period) |>
  summarise(
    n_country_rounds = n(),
    n_countries = n_distinct(cntry),
    mean_hate_w = mean(hate_w),
    sd_hate_w = sd(hate_w),
    min_hate_w = min(hate_w),
    max_hate_w = max(hate_w),
    .groups = "drop"
  )

mode_counts <- individual_complete |>
  count(essround, interview_mode, name = "n_respondents")

cat("\nM5 sample flow:\n")
print(sample_flow, n = Inf, width = Inf)
cat("\nR1-R11 M5 sample by mode:\n")
print(sample_summary, n = Inf, width = Inf)
cat("\nWithin-country party-hate variation by period:\n")
print(period_variation, n = Inf, width = Inf)

# 3. Fit common-slope and later-period M5 specifications -------------------
ctrl <- lmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 200000),
  check.rankX = "stop.deficient"
)

# Exact M5 fixed-effects structure in 06_party_hate_moderation.R.
# A post_r9 main effect is omitted because round_factor absorbs it.
m5_common <-
  ppltrst ~
  gini_w + gini_b +
  gdp_w_10 + gdp_b_10 +
  unemployment_w + unemployment_b +
  round_factor +
  age_decade + age_decade2 + gender + education_c +
  hate_w + hate_b +
  (1 | cntry) + (1 | country_round)

m5_interaction <- update(m5_common, . ~ . + hate_w:post_r9)

fit_one <- function(formula, dat) {
  captured_warnings <- character()
  fit <- withCallingHandlers(
    lmer(
      formula, data = dat, REML = FALSE,
      na.action = na.fail, control = ctrl
    ),
    warning = function(w) {
      captured_warnings <<- c(captured_warnings, conditionMessage(w))
    }
  )
  if (nobs(fit) != nrow(dat)) {
    stop("Unexpected change in model estimation sample.")
  }
  list(fit = fit, warnings = unique(captured_warnings))
}

extract_diagnostics <- function(obj, sample_name, specification) {
  fit <- obj$fit
  messages <- fit@optinfo$conv$lme4$messages
  optimizer <- fit@optinfo$conv$opt
  dropped <- attr(getME(fit, "X"), "col.dropped")
  tibble(
    sample = sample_name,
    specification = specification,
    n_respondents = nobs(fit),
    singular = isSingular(fit, tol = 1e-4),
    optimizer_code = if (is.null(optimizer)) NA_integer_
                     else as.integer(optimizer),
    convergence_message = if (is.null(messages)) NA_character_
                          else paste(messages, collapse = "; "),
    warnings = if (!length(obj$warnings)) NA_character_
               else paste(obj$warnings, collapse = "; "),
    dropped_terms = if (is.null(dropped)) NA_character_
                    else paste(names(dropped), collapse = "; ")
  )
}

linear_combo <- function(fit, weights, sample_name, effect) {
  b <- fixef(fit)
  if (!all(names(weights) %in% names(b))) {
    stop("Missing coefficient(s) in linear combination: ",
         paste(setdiff(names(weights), names(b)), collapse = ", "))
  }
  V <- as.matrix(vcov(fit))
  terms <- names(weights)
  estimate <- sum(unname(weights) * b[terms])
  std_error <- sqrt(as.numeric(
    t(unname(weights)) %*% V[terms, terms, drop = FALSE] %*%
      unname(weights)
  ))
  statistic <- estimate / std_error
  tibble(
    sample = sample_name,
    model = "M5",
    effect = effect,
    estimate = estimate,
    std_error = std_error,
    statistic = statistic,
    conf_low = estimate - 1.96 * std_error,
    conf_high = estimate + 1.96 * std_error,
    p_value = 2 * pnorm(abs(statistic), lower.tail = FALSE)
  )
}

extract_slopes <- function(fit, sample_name) {
  b <- names(fixef(fit))
  possible <- c("hate_w:post_r9", "post_r9:hate_w")
  interaction_term <- intersect(possible, b)
  if (length(interaction_term) != 1L) {
    stop("Expected one hate_w x post_r9 coefficient.")
  }
  bind_rows(
    linear_combo(
      fit, c(hate_w = 1), sample_name, "R1-R9 slope"
    ),
    linear_combo(
      fit, setNames(c(1, 1), c("hate_w", interaction_term)),
      sample_name, "R10-R11 slope"
    ),
    linear_combo(
      fit, setNames(1, interaction_term),
      sample_name, "R10-R11 minus R1-R9"
    )
  )
}

extract_lrt <- function(base, interaction, sample_name) {
  tab <- anova(base, interaction)
  tibble(
    sample = sample_name,
    model = "M5",
    chisq = as.numeric(tab[["Chisq"]][2]),
    df = as.numeric(tab[["Df"]][2]),
    p_value = as.numeric(tab[["Pr(>Chisq)"]][2]),
    AIC_base = AIC(base),
    AIC_interaction = AIC(interaction),
    BIC_base = BIC(base),
    BIC_interaction = BIC(interaction)
  )
}

extract_baseline <- function(fit, sample_name) {
  x <- as.data.frame(coef(summary(fit)))
  tibble(
    sample = sample_name,
    model = "M5",
    term = rownames(x),
    estimate = x[["Estimate"]],
    std_error = x[["Std. Error"]]
  ) |>
    filter(term %in% c("gini_w", "gini_b", "hate_w", "hate_b"))
}

results <- list()
for (s in c("All modes", "In-person only")) {
  dat <- if (s == "All modes") all_data else inperson_data
  cat("\nFitting ", s, " common M5...\n", sep = "")
  base <- fit_one(m5_common, dat)
  cat("Fitting ", s, " M5 with hate_w x post_r9...\n", sep = "")
  interaction <- fit_one(m5_interaction, dat)
  results[[s]] <- list(
    slopes = extract_slopes(interaction$fit, s),
    lrt = extract_lrt(base$fit, interaction$fit, s),
    diagnostics = bind_rows(
      extract_diagnostics(base, s, "Common slope"),
      extract_diagnostics(interaction, s, "R10-R11 interaction")
    ),
    base_coef = extract_baseline(base$fit, s)
  )
  rm(base, interaction)
  invisible(gc())
}

period_slopes <- bind_rows(lapply(results, `[[`, "slopes"))
interaction_tests <- period_slopes |>
  filter(effect == "R10-R11 minus R1-R9")
interaction_lrt <- bind_rows(lapply(results, `[[`, "lrt"))
model_diagnostics <- bind_rows(lapply(results, `[[`, "diagnostics"))
base_coefficients <- bind_rows(lapply(results, `[[`, "base_coef"))

# Single-row comparison; descriptive only (overlapping samples).
mode_comparison <- tibble(
  model = "M5",
  all_mode_estimate = interaction_tests$estimate[
    interaction_tests$sample == "All modes"],
  all_mode_se = interaction_tests$std_error[
    interaction_tests$sample == "All modes"],
  inperson_estimate = interaction_tests$estimate[
    interaction_tests$sample == "In-person only"],
  inperson_se = interaction_tests$std_error[
    interaction_tests$sample == "In-person only"]
) |>
  mutate(inperson_minus_all_mode = inperson_estimate - all_mode_estimate)

# 4. Reproduce main M5 all-mode coefficients as an integrity check --------
reference_path <- here(
  "05_output", "tables", "party_hate_moderation", "r1_r11_fixed_effects.csv"
)
reference_comparison <- tibble()
if (file.exists(reference_path)) {
  reference <- read_csv(reference_path, show_col_types = FALSE) |>
    filter(model == "M5", term %in% c("gini_w", "gini_b", "hate_w", "hate_b")) |>
    select(term, original_estimate = estimate)
  reference_comparison <- base_coefficients |>
    filter(sample == "All modes") |>
    select(term, reproduced_estimate = estimate) |>
    left_join(reference, by = "term") |>
    mutate(difference = reproduced_estimate - original_estimate)
  if (any(is.na(reference_comparison$original_estimate))) {
    warning("Some reference M5 terms missing; inspect benchmark output.")
  } else if (any(abs(reference_comparison$difference) > 1e-4)) {
    warning("All-mode M5 differs from saved reference by > 1e-4; inspect benchmark comparison.")
  }
} else {
  cat("\nReference M5 output not found; reference comparison skipped.\n")
}

# 5. Save small diagnostic tables (not raw data or fitted model objects) ---
write_csv(sample_flow, file.path(out_dir, "sample_flow.csv"))
write_csv(sample_summary, file.path(out_dir, "sample_summary.csv"))
write_csv(mode_counts, file.path(out_dir, "mode_counts_m5.csv"))
write_csv(period_variation, file.path(out_dir, "period_variation.csv"))
write_csv(period_slopes, file.path(out_dir, "period_slopes.csv"))
write_csv(interaction_tests, file.path(out_dir, "interaction_tests.csv"))
write_csv(interaction_lrt, file.path(out_dir, "interaction_lrt.csv"))
write_csv(model_diagnostics, file.path(out_dir, "model_diagnostics.csv"))
write_csv(base_coefficients, file.path(out_dir, "base_coefficients.csv"))
write_csv(mode_comparison, file.path(out_dir, "mode_comparison.csv"))
if (nrow(reference_comparison)) {
  write_csv(reference_comparison,
            file.path(out_dir, "reference_m5_comparison.csv"))
}

cat("\nM5 baseline coefficients:\n")
print(base_coefficients, n = Inf, width = Inf)
cat("\nImplied R1-R9 and R10-R11 party-hate slopes:\n")
print(period_slopes, n = Inf, width = Inf)
cat("\nJoint likelihood-ratio tests:\n")
print(interaction_lrt, n = Inf, width = Inf)
cat("\nMode comparison (descriptive, overlapping samples):\n")
print(mode_comparison, n = Inf, width = Inf)
cat("\nModel diagnostics:\n")
print(model_diagnostics, n = Inf, width = Inf)
if (nrow(reference_comparison)) {
  cat("\nOriginal M5 reproduction check:\n")
  print(reference_comparison, n = Inf, width = Inf)
}
cat("\nCompleted. Tables saved to:\n", out_dir, "\n")
