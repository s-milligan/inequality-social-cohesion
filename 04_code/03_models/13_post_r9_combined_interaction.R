# ------------------------------------------------------------
# Combined later-period inequality interaction: ESS Rounds 1-11
# Project: Inequality and social cohesion
#
# Question:
#   Does the within-country Gini/trust slope differ in R10-R11
#   from the slope in R1-R9?
#
# Design:
#   - Estimate M3 and M4 as in 05_period_interactions.R.
#   - Compare a common within-country Gini slope against a
#     single R10-R11 modification (post_r9 = essround >= 10).
#   - Repeat with in-person respondents only.
#   - Retain Rounds 1-11 country-round inequality and macro
#     decompositions in BOTH respondent samples, isolating mode.
#   - Retain ESS round fixed effects; no separate post_r9 main
#     effect, which would be collinear with the round effects.
#   - Estimate by ML without survey weights, consistent with
#     the existing REWB baseline. No SWIID re-imputation.
#
# Caution:
#   Later ESS rounds coincide with major historical and mode
#   changes. This is descriptive slope heterogeneity, not an
#   identified causal effect of the post-2020 environment.
#
# Run from the project root in RStudio or with:
#   Rscript 04_code/03_models/13_post_r9_combined_interaction.R
# ------------------------------------------------------------

library(dplyr)
library(here)
library(lme4)
library(readr)
library(tibble)

out_dir <- here(
  "05_output", "tables", "period_interactions_combined"
)

# Saving large fitted lmer objects is optional. Tables are always saved.
save_model_objects <- FALSE

# 1. Import ---------------------------------------------------------------

ess <- readRDS(here(
  "03_data", "processed", "ess_analysis_r1_r11.rds"
))

needed_ess <- c(
  "cntry", "essround", "interview_mode", "gini_swiid_lag1",
  "ppltrst", "age", "gender", "education_years"
)
if (length(setdiff(needed_ess, names(ess))) > 0) {
  stop("Required ESS variables missing: ",
       paste(setdiff(needed_ess, names(ess)), collapse = ", "))
}

macro <- read_csv(
  here("03_data", "interim", "ess_country_round_macro_r1_r11.csv"),
  show_col_types = FALSE
) |>
  select(cntry, essround, log_gdp_pc_ppp_lag1, unemployment_lag1)

if (nrow(macro) != nrow(distinct(macro, cntry, essround))) {
  stop("Duplicate country-round rows in macro input.")
}

ess <- ess |>
  left_join(macro, by = c("cntry", "essround"))

cat("\nR1-R11 input respondents:", nrow(ess), "\n")

mode_counts <- ess |>
  count(essround, interview_mode, name = "n_respondents") |>
  arrange(essround, interview_mode)

cat("\nInterview mode counts:\n")
print(mode_counts, n = Inf, width = Inf)

# All R1-R9 rows in the harmonised file should have the historical
# conventional face-to-face mode label. If this changes, revisit the
# definition of the sensitivity analysis.
if (any(ess$essround <= 9 &
        (is.na(ess$interview_mode) | ess$interview_mode != "In-person"))) {
  stop("Unexpected interview modes in R1-R9. Inspect input coding.")
}

# 2. Construct contextual components ONCE using all modes ----------------
# Use unique country-rounds, not respondent-weighted means.
# The same contextual values will be used in the in-person subset.

cr_gini <- ess |>
  distinct(cntry, essround, gini_swiid_lag1) |>
  filter(!is.na(gini_swiid_lag1)) |>
  group_by(cntry) |>
  mutate(
    gini_between = mean(gini_swiid_lag1),
    gini_within = gini_swiid_lag1 - gini_between
  ) |>
  ungroup()

gini_grand_mean <- cr_gini |>
  distinct(cntry, gini_between) |>
  summarise(value = mean(gini_between)) |>
  pull(value)

cr_gini <- cr_gini |>
  mutate(gini_between_c = gini_between - gini_grand_mean)

macro_context <- ess |>
  distinct(
    cntry, essround, gini_swiid_lag1,
    log_gdp_pc_ppp_lag1, unemployment_lag1
  ) |>
  filter(
    !is.na(gini_swiid_lag1),
    !is.na(log_gdp_pc_ppp_lag1),
    !is.na(unemployment_lag1)
  ) |>
  group_by(cntry) |>
  mutate(
    gini_between_macro = mean(gini_swiid_lag1),
    gini_within_macro = gini_swiid_lag1 - gini_between_macro,
    log_gdp_between = mean(log_gdp_pc_ppp_lag1),
    log_gdp_within = log_gdp_pc_ppp_lag1 - log_gdp_between,
    unemployment_between = mean(unemployment_lag1),
    unemployment_within = unemployment_lag1 - unemployment_between
  ) |>
  ungroup()

macro_means <- macro_context |>
  distinct(
    cntry, gini_between_macro,
    log_gdp_between, unemployment_between
  ) |>
  summarise(
    gini = mean(gini_between_macro),
    log_gdp = mean(log_gdp_between),
    unemployment = mean(unemployment_between)
  )

macro_context <- macro_context |>
  mutate(
    gini_between_macro_c = gini_between_macro - macro_means$gini,
    log_gdp_between_c = log_gdp_between - macro_means$log_gdp,
    unemployment_between_c = unemployment_between - macro_means$unemployment,
    log_gdp_within_10 = log_gdp_within * 10,
    log_gdp_between_10 = log_gdp_between_c * 10
  )

ess <- ess |>
  left_join(
    cr_gini |>
      select(cntry, essround, gini_within, gini_between_c),
    by = c("cntry", "essround")
  ) |>
  left_join(
    macro_context |>
      select(
        cntry, essround,
        gini_within_macro, gini_between_macro_c,
        log_gdp_within_10, log_gdp_between_10,
        unemployment_within, unemployment_between_c
      ),
    by = c("cntry", "essround")
  )

# 3. Model samples --------------------------------------------------------

prepare_model <- function(dat, model_name) {
  if (model_name == "M3") {
    x <- dat |>
      filter(
        !is.na(ppltrst), !is.na(gini_within),
        !is.na(gini_between_c), !is.na(age),
        !is.na(gender), !is.na(education_years)
      )
  } else if (model_name == "M4") {
    x <- dat |>
      filter(
        !is.na(ppltrst), !is.na(gini_within_macro),
        !is.na(gini_between_macro_c),
        !is.na(log_gdp_within_10), !is.na(log_gdp_between_10),
        !is.na(unemployment_within), !is.na(unemployment_between_c),
        !is.na(age), !is.na(gender), !is.na(education_years)
      )
  } else {
    stop("Unrecognised model: ", model_name)
  }

  x |>
    mutate(
      country_round = interaction(cntry, essround, drop = TRUE),
      round_factor = factor(essround),
      post_r9 = as.numeric(essround >= 10),
      gender = factor(as.character(gender), levels = c("Man", "Woman")),
      age_decade = (age - mean(age)) / 10,
      age_decade2 = age_decade^2,
      education_c = education_years - mean(education_years)
    )
}

summarise_sample <- function(dat, sample_name, model_name) {
  tibble(
    sample = sample_name,
    model = model_name,
    n_respondents = nrow(dat),
    n_countries = n_distinct(dat$cntry),
    n_country_rounds = n_distinct(dat$country_round),
    n_later_country_rounds = nrow(
      distinct(filter(dat, post_r9 == 1), cntry, essround)
    )
  )
}

period_variation_fn <- function(dat, sample_name, model_name) {
  var_name <- if (model_name == "M3") "gini_within" else "gini_within_macro"
  dat |>
    transmute(cntry, essround, post_r9,
              gini_value = .data[[var_name]]) |>
    distinct() |>
    mutate(period = if_else(post_r9 == 1, "R10-R11", "R1-R9")) |>
    group_by(period) |>
    summarise(
      n_country_rounds = n(),
      n_countries = n_distinct(cntry),
      sd_within_gini = sd(gini_value),
      min_within_gini = min(gini_value),
      max_within_gini = max(gini_value),
      .groups = "drop"
    ) |>
    mutate(sample = sample_name, model = model_name, .before = 1)
}

# 4. Fixed-effects models -------------------------------------------------

ctrl <- lmerControl(
  optimizer = "bobyqa", optCtrl = list(maxfun = 200000)
)

m3_common_formula <-
  ppltrst ~ gini_within + gini_between_c + round_factor +
  age_decade + age_decade2 + gender + education_c +
  (1 | cntry) + (1 | country_round)

m3_interaction_formula <-
  ppltrst ~ gini_within + gini_between_c + round_factor +
  gini_within:post_r9 +
  age_decade + age_decade2 + gender + education_c +
  (1 | cntry) + (1 | country_round)

m4_common_formula <-
  ppltrst ~ gini_within_macro + gini_between_macro_c +
  log_gdp_within_10 + log_gdp_between_10 +
  unemployment_within + unemployment_between_c + round_factor +
  age_decade + age_decade2 + gender + education_c +
  (1 | cntry) + (1 | country_round)

m4_interaction_formula <-
  ppltrst ~ gini_within_macro + gini_between_macro_c +
  log_gdp_within_10 + log_gdp_between_10 +
  unemployment_within + unemployment_between_c + round_factor +
  gini_within_macro:post_r9 +
  age_decade + age_decade2 + gender + education_c +
  (1 | cntry) + (1 | country_round)

fit_pair <- function(dat, model_name) {
  if (model_name == "M3") {
    base_formula <- m3_common_formula
    interaction_formula <- m3_interaction_formula
  } else {
    base_formula <- m4_common_formula
    interaction_formula <- m4_interaction_formula
  }

  base <- lmer(
    base_formula, data = dat, REML = FALSE, control = ctrl
  )
  interaction_fit <- lmer(
    interaction_formula, data = dat, REML = FALSE, control = ctrl
  )

  if (nobs(base) != nobs(interaction_fit) || nobs(base) != nrow(dat)) {
    stop("Base and interaction models use different samples: ", model_name)
  }

  list(base = base, interaction = interaction_fit)
}

# 5. Effect extraction ----------------------------------------------------

find_interaction_term <- function(fit, gini_term) {
  options <- c(
    paste0(gini_term, ":post_r9"),
    paste0("post_r9:", gini_term)
  )
  matches <- intersect(options, names(fixef(fit)))
  if (length(matches) != 1) {
    stop("Expected exactly one combined-period interaction term; found: ",
         paste(matches, collapse = ", "))
  }
  matches
}

linear_combo <- function(fit, weights, sample_name, model_name, effect) {
  beta <- fixef(fit)
  terms <- names(weights)
  if (!all(terms %in% names(beta))) {
    stop("Coefficient(s) not found: ",
         paste(setdiff(terms, names(beta)), collapse = ", "))
  }

  v <- as.matrix(vcov(fit))[terms, terms, drop = FALSE]
  estimate <- sum(weights * beta[terms])
  std_error <- sqrt(as.numeric(t(weights) %*% v %*% weights))
  z <- estimate / std_error

  tibble(
    sample = sample_name, model = model_name, effect = effect,
    estimate = estimate, std_error = std_error,
    statistic = z,
    conf_low = estimate - 1.96 * std_error,
    conf_high = estimate + 1.96 * std_error,
    p_value = 2 * pnorm(abs(z), lower.tail = FALSE)
  )
}

extract_slopes <- function(fit, sample_name, model_name) {
  gini <- if (model_name == "M3") "gini_within" else "gini_within_macro"
  interaction_term <- find_interaction_term(fit, gini)

  bind_rows(
    linear_combo(
      fit, setNames(1, gini), sample_name, model_name, "R1-R9 slope"
    ),
    linear_combo(
      fit, setNames(c(1, 1), c(gini, interaction_term)),
      sample_name, model_name, "R10-R11 slope"
    ),
    linear_combo(
      fit, setNames(1, interaction_term), sample_name, model_name,
      "R10-R11 minus R1-R9"
    )
  )
}

extract_lrt <- function(fits, sample_name, model_name) {
  x <- anova(fits$base, fits$interaction)
  tibble(
    sample = sample_name, model = model_name,
    chisq = as.numeric(x[["Chisq"]][2]),
    df = as.numeric(x[["Df"]][2]),
    p_value = as.numeric(x[["Pr(>Chisq)"]][2]),
    AIC_base = AIC(fits$base),
    AIC_interaction = AIC(fits$interaction),
    BIC_base = BIC(fits$base),
    BIC_interaction = BIC(fits$interaction)
  )
}

extract_diagnostics <- function(fit, sample_name, model_name, specification) {
  messages <- fit@optinfo$conv$lme4$messages
  optimizer_code <- fit@optinfo$conv$opt
  tibble(
    sample = sample_name, model = model_name,
    specification = specification,
    n_respondents = nobs(fit),
    singular = isSingular(fit, tol = 1e-4),
    optimizer_code = if (is.null(optimizer_code)) NA_integer_
                     else as.integer(optimizer_code),
    convergence_message = if (is.null(messages)) NA_character_
                          else paste(messages, collapse = " | ")
  )
}

# 6. Estimate all-mode and in-person models -------------------------------

samples <- list(
  "All modes" = ess,
  "In-person only" = filter(ess, interview_mode == "In-person")
)

all_sample_summaries <- list()
all_period_variation <- list()
all_slopes <- list()
all_lrt <- list()
all_diagnostics <- list()
model_store <- list()
i <- 0L

for (sample_name in names(samples)) {
  for (model_name in c("M3", "M4")) {
    i <- i + 1L
    cat("\nFitting", sample_name, model_name, "...\n")

    dat <- prepare_model(samples[[sample_name]], model_name)

    all_sample_summaries[[i]] <- summarise_sample(
      dat, sample_name, model_name
    )
    all_period_variation[[i]] <- period_variation_fn(
      dat, sample_name, model_name
    )

    fits <- fit_pair(dat, model_name)
    all_slopes[[i]] <- extract_slopes(
      fits$interaction, sample_name, model_name
    )
    all_lrt[[i]] <- extract_lrt(fits, sample_name, model_name)
    all_diagnostics[[2 * i - 1L]] <- extract_diagnostics(
      fits$base, sample_name, model_name, "Common slope"
    )
    all_diagnostics[[2 * i]] <- extract_diagnostics(
      fits$interaction, sample_name, model_name, "R10-R11 interaction"
    )

    if (save_model_objects) {
      model_store[[paste(sample_name, model_name, sep = "_")]] <- fits
    }

    rm(dat, fits)
    invisible(gc(verbose = FALSE))
  }
}

sample_summary <- bind_rows(all_sample_summaries)
period_variation <- bind_rows(all_period_variation)
period_slopes <- bind_rows(all_slopes)
interaction_tests <- period_slopes |>
  filter(effect == "R10-R11 minus R1-R9")
interaction_lrt <- bind_rows(all_lrt)
model_diagnostics <- bind_rows(all_diagnostics)

# The all-mode and in-person estimates are based on overlapping samples.
# The difference below is DESCRIPTIVE, not a formal independent-samples test.
all_mode_tests <- interaction_tests |>
  filter(sample == "All modes") |>
  transmute(model, all_mode_estimate = estimate,
            all_mode_se = std_error, all_mode_p = p_value)

inperson_tests <- interaction_tests |>
  filter(sample == "In-person only") |>
  transmute(model, inperson_estimate = estimate,
            inperson_se = std_error, inperson_p = p_value)

mode_comparison <- all_mode_tests |>
  inner_join(inperson_tests, by = "model") |>
  mutate(inperson_minus_all_mode = inperson_estimate - all_mode_estimate)

cat("\nSample summary:\n")
print(sample_summary, n = Inf, width = Inf)
cat("\nContextual Gini variation by period:\n")
print(period_variation, n = Inf, width = Inf)
cat("\nImplied R1-R9 and R10-R11 slopes, and their difference:\n")
print(period_slopes, n = Inf, width = Inf)
cat("\nCombined R10-R11 interaction tests:\n")
print(interaction_tests, n = Inf, width = Inf)
cat("\nLikelihood-ratio tests (1 added interaction parameter):\n")
print(interaction_lrt, n = Inf, width = Inf)
cat("\nMode sensitivity comparison (descriptive difference only):\n")
print(mode_comparison, n = Inf, width = Inf)
cat("\nModel diagnostics:\n")
print(model_diagnostics, n = Inf, width = Inf)

if (any(model_diagnostics$singular | !is.na(model_diagnostics$convergence_message) |
        (!is.na(model_diagnostics$optimizer_code) &
           model_diagnostics$optimizer_code != 0))) {
  warning("At least one model may have a fitting issue; inspect diagnostics.")
}

# 7. Save compact, local-only output tables -------------------------------

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(mode_counts, file.path(out_dir, "mode_counts.csv"))
write_csv(sample_summary, file.path(out_dir, "sample_summary.csv"))
write_csv(period_variation, file.path(out_dir, "gini_variation_by_period.csv"))
write_csv(period_slopes, file.path(out_dir, "period_slopes.csv"))
write_csv(interaction_tests, file.path(out_dir, "interaction_tests.csv"))
write_csv(interaction_lrt, file.path(out_dir, "interaction_lrt.csv"))
write_csv(mode_comparison, file.path(out_dir, "mode_comparison.csv"))
write_csv(model_diagnostics, file.path(out_dir, "model_diagnostics.csv"))

if (save_model_objects) {
  model_dir <- here("05_output", "models")
  dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(model_store,
          file.path(model_dir, "post_r9_combined_interaction_models.rds"))
}

cat("\nCompleted. Tables saved to:\n", out_dir, "\n")
