# ------------------------------------------------------------
# Period interactions: ESS Rounds 1-11
# Project: Inequality and social cohesion
#
# Purpose:
#   Test whether the within-country relationship between
#   income inequality and generalized social trust differs
#   in ESS Round 10 or Round 11 relative to Rounds 1-9.
#
# Strategy:
#   Rounds 1-9 provide the reference within-country Gini slope.
#
#   Estimate separate slope modifications for:
#     - Round 10
#     - Round 11
#
#   Round fixed effects remain in all models, so the
#   interactions test changes in the Gini slope rather than
#   changes in mean trust between rounds.
#
#   Models:
#     M3: individual controls
#     M4: + GDP and unemployment
#
# No political-discourse measure is introduced yet.
# ------------------------------------------------------------


library(dplyr)
library(here)
library(lme4)
library(readr)
library(tibble)


# 1. Import ---------------------------------------------------------------

ess <- readRDS(
  here(
    "03_data",
    "processed",
    "ess_analysis_r1_r11.rds"
  )
)


macro <- read_csv(
  here(
    "03_data",
    "interim",
    "ess_country_round_macro_r1_r11.csv"
  ),
  show_col_types = FALSE
) |>
  select(
    cntry,
    essround,
    log_gdp_pc_ppp_lag1,
    unemployment_lag1
  )


ess <- ess |>
  left_join(
    macro,
    by = c(
      "cntry",
      "essround"
    )
  )


cat(
  "\nExtended ESS:",
  nrow(ess),
  "respondents\n\n"
)


# 2. Helpers --------------------------------------------------------------

extract_fixed <- function(
  fit,
  model_name
) {

  x <- as.data.frame(
    coef(
      summary(fit)
    )
  )

  x$term <- rownames(x)

  rownames(x) <- NULL


  as_tibble(x) |>
    transmute(
      model = model_name,
      term,
      estimate = Estimate,
      std_error = `Std. Error`,
      statistic = `t value`,
      conf_low =
        estimate -
        1.96 * std_error,
      conf_high =
        estimate +
        1.96 * std_error
    )
}


linear_combo <- function(
  fit,
  terms,
  weights,
  model_name,
  effect_name
) {

  beta <-
    fixef(fit)

  v <-
    as.matrix(
      vcov(fit)
    )


  if (
    !all(
      terms %in%
        names(beta)
    )
  ) {

    stop(
      paste(
        "Terms missing from model:",
        paste(
          setdiff(
            terms,
            names(beta)
          ),
          collapse = ", "
        )
      )
    )
  }


  b <-
    beta[terms]

  vv <-
    v[
      terms,
      terms,
      drop = FALSE
    ]


  estimate <-
    sum(
      weights *
        b
    )


  variance <-
    as.numeric(
      t(weights) %*%
        vv %*%
        weights
    )


  std_error <-
    sqrt(
      variance
    )


  statistic <-
    estimate /
    std_error


  tibble(
    model = model_name,
    effect = effect_name,
    estimate,
    std_error,
    statistic,
    conf_low =
      estimate -
      1.96 * std_error,
    conf_high =
      estimate +
      1.96 * std_error,
    p_value =
      2 *
      pnorm(
        -abs(
          statistic
        )
      )
  )
}


extract_lrt <- function(
  base_fit,
  interaction_fit,
  model_name
) {

  x <- anova(
    base_fit,
    interaction_fit
  )


  tibble(
    model = model_name,

    chisq =
      x[["Chisq"]][2],

    df =
      x[["Df"]][2],

    p_value =
      x[["Pr(>Chisq)"]][2],

    AIC_base =
      AIC(base_fit),

    AIC_interaction =
      AIC(interaction_fit),

    BIC_base =
      BIC(base_fit),

    BIC_interaction =
      BIC(interaction_fit)
  )
}


# 3. Construct R1-R11 Gini decomposition ---------------------------------

# Use one observation per country-round.

country_round_gini <- ess |>
  distinct(
    cntry,
    essround,
    gini_swiid_lag1
  ) |>
  filter(
    !is.na(
      gini_swiid_lag1
    )
  ) |>
  group_by(
    cntry
  ) |>
  mutate(
    gini_between =
      mean(
        gini_swiid_lag1
      ),

    gini_within =
      gini_swiid_lag1 -
      gini_between
  ) |>
  ungroup()


gini_grand_mean <- country_round_gini |>
  distinct(
    cntry,
    gini_between
  ) |>
  summarise(
    value =
      mean(
        gini_between
      )
  ) |>
  pull(value)


country_round_gini <- country_round_gini |>
  mutate(
    gini_between_c =
      gini_between -
      gini_grand_mean
  )


ess <- ess |>
  left_join(
    country_round_gini,
    by = c(
      "cntry",
      "essround",
      "gini_swiid_lag1"
    )
  )


# 4. Prepare M3 sample ----------------------------------------------------

m3_dat <- ess |>
  filter(
    !is.na(
      ppltrst
    ),
    !is.na(
      gini_within
    ),
    !is.na(
      gini_between_c
    ),
    !is.na(
      age
    ),
    !is.na(
      gender
    ),
    !is.na(
      education_years
    )
  ) |>
  mutate(
    country_round =
      interaction(
        cntry,
        essround,
        drop = TRUE
      ),

    round_factor =
      factor(
        essround
      ),

    # R1-R9 are the reference slope.
    r10 =
      as.numeric(
        essround == 10
      ),

    r11 =
      as.numeric(
        essround == 11
      ),

    gender =
      factor(
        as.character(
          gender
        ),
        levels = c(
          "Man",
          "Woman"
        )
      ),

    age_decade =
      (
        age -
        mean(age)
      ) / 10,

    age_decade2 =
      age_decade^2,

    education_c =
      education_years -
      mean(
        education_years
      )
  )


# 5. Construct macro-complete decomposition ------------------------------

# Decompose all contextual variables using exactly the
# country-rounds available to the macro-adjusted model.

macro_context <- ess |>
  distinct(
    cntry,
    essround,
    gini_swiid_lag1,
    log_gdp_pc_ppp_lag1,
    unemployment_lag1
  ) |>
  filter(
    !is.na(
      gini_swiid_lag1
    ),
    !is.na(
      log_gdp_pc_ppp_lag1
    ),
    !is.na(
      unemployment_lag1
    )
  ) |>
  group_by(
    cntry
  ) |>
  mutate(
    # Gini
    gini_between_macro =
      mean(
        gini_swiid_lag1
      ),

    gini_within_macro =
      gini_swiid_lag1 -
      gini_between_macro,

    # GDP
    log_gdp_between =
      mean(
        log_gdp_pc_ppp_lag1
      ),

    log_gdp_within =
      log_gdp_pc_ppp_lag1 -
      log_gdp_between,

    # Unemployment
    unemployment_between =
      mean(
        unemployment_lag1
      ),

    unemployment_within =
      unemployment_lag1 -
      unemployment_between
  ) |>
  ungroup()


macro_means <- macro_context |>
  distinct(
    cntry,
    gini_between_macro,
    log_gdp_between,
    unemployment_between
  ) |>
  summarise(
    gini =
      mean(
        gini_between_macro
      ),

    log_gdp =
      mean(
        log_gdp_between
      ),

    unemployment =
      mean(
        unemployment_between
      )
  )


macro_context <- macro_context |>
  mutate(
    gini_between_macro_c =
      gini_between_macro -
      macro_means$gini,

    log_gdp_between_c =
      log_gdp_between -
      macro_means$log_gdp,

    unemployment_between_c =
      unemployment_between -
      macro_means$unemployment,

    # Express GDP coefficients per 0.1 log-point.
    log_gdp_within_10 =
      log_gdp_within * 10,

    log_gdp_between_10 =
      log_gdp_between_c * 10
  )


m4_dat <- ess |>
  left_join(
    macro_context |>
      select(
        cntry,
        essround,
        gini_within_macro,
        gini_between_macro_c,
        log_gdp_within_10,
        log_gdp_between_10,
        unemployment_within,
        unemployment_between_c
      ),
    by = c(
      "cntry",
      "essround"
    )
  ) |>
  filter(
    !is.na(
      ppltrst
    ),
    !is.na(
      gini_within_macro
    ),
    !is.na(
      gini_between_macro_c
    ),
    !is.na(
      log_gdp_within_10
    ),
    !is.na(
      log_gdp_between_10
    ),
    !is.na(
      unemployment_within
    ),
    !is.na(
      unemployment_between_c
    ),
    !is.na(
      age
    ),
    !is.na(
      gender
    ),
    !is.na(
      education_years
    )
  ) |>
  mutate(
    country_round =
      interaction(
        cntry,
        essround,
        drop = TRUE
      ),

    round_factor =
      factor(
        essround
      ),

    r10 =
      as.numeric(
        essround == 10
      ),

    r11 =
      as.numeric(
        essround == 11
      ),

    gender =
      factor(
        as.character(
          gender
        ),
        levels = c(
          "Man",
          "Woman"
        )
      ),

    # Re-centre individual controls on actual M4 sample.
    age_decade =
      (
        age -
        mean(age)
      ) / 10,

    age_decade2 =
      age_decade^2,

    education_c =
      education_years -
      mean(
        education_years
      )
  )


# 6. Sample diagnostics ---------------------------------------------------

sample_summary <- tibble(
  model = c(
    "M3",
    "M4"
  ),

  n_respondents = c(
    nrow(m3_dat),
    nrow(m4_dat)
  ),

  n_countries = c(
    n_distinct(
      m3_dat$cntry
    ),
    n_distinct(
      m4_dat$cntry
    )
  ),

  n_country_rounds = c(
    n_distinct(
      m3_dat$country_round
    ),
    n_distinct(
      m4_dat$country_round
    )
  )
)


cat(
  "\nAnalytical samples:\n"
)

print(
  sample_summary
)


# Examine contextual variation available in each period.

m3_period_variation <- m3_dat |>
  distinct(
    cntry,
    essround,
    gini_within
  ) |>
  mutate(
    period =
      case_when(
        essround <= 9 ~
          "R1-R9",
        essround == 10 ~
          "R10",
        essround == 11 ~
          "R11"
      )
  ) |>
  group_by(
    period
  ) |>
  summarise(
    n_country_rounds =
      n(),

    n_countries =
      n_distinct(
        cntry
      ),

    mean_gini_within =
      mean(
        gini_within
      ),

    sd_gini_within =
      sd(
        gini_within
      ),

    min_gini_within =
      min(
        gini_within
      ),

    max_gini_within =
      max(
        gini_within
      ),

    .groups = "drop"
  )


cat(
  "\nWithin-country Gini variation by period:\n"
)

print(
  m3_period_variation
)


# 7. Fit M3 ---------------------------------------------------------------

ctrl <- lmerControl(
  optimizer = "bobyqa",
  optCtrl = list(
    maxfun = 200000
  )
)


# Baseline/common-slope model.

m3_base <- lmer(
  ppltrst ~
    gini_within +
    gini_between_c +
    round_factor +
    age_decade +
    age_decade2 +
    gender +
    education_c +
    (1 | cntry) +
    (1 | country_round),
  data = m3_dat,
  REML = FALSE,
  control = ctrl
)


# Allow the within-Gini slope to differ in R10 and R11.
#
# No main effects for r10/r11 are included because those
# are already represented by round_factor.

m3_period <- lmer(
  ppltrst ~
    gini_within +
    gini_between_c +
    round_factor +
    gini_within:r10 +
    gini_within:r11 +
    age_decade +
    age_decade2 +
    gender +
    education_c +
    (1 | cntry) +
    (1 | country_round),
  data = m3_dat,
  REML = FALSE,
  control = ctrl
)


# 8. Fit M4 ---------------------------------------------------------------

m4_base <- lmer(
  ppltrst ~
    gini_within_macro +
    gini_between_macro_c +
    log_gdp_within_10 +
    log_gdp_between_10 +
    unemployment_within +
    unemployment_between_c +
    round_factor +
    age_decade +
    age_decade2 +
    gender +
    education_c +
    (1 | cntry) +
    (1 | country_round),
  data = m4_dat,
  REML = FALSE,
  control = ctrl
)


m4_period <- lmer(
  ppltrst ~
    gini_within_macro +
    gini_between_macro_c +
    log_gdp_within_10 +
    log_gdp_between_10 +
    unemployment_within +
    unemployment_between_c +
    round_factor +
    gini_within_macro:r10 +
    gini_within_macro:r11 +
    age_decade +
    age_decade2 +
    gender +
    education_c +
    (1 | cntry) +
    (1 | country_round),
  data = m4_dat,
  REML = FALSE,
  control = ctrl
)


cat(
  "\nModels fitted.\n"
)


# 9. Extract interaction coefficients ------------------------------------

interaction_coefficients <- bind_rows(
  extract_fixed(
    m3_period,
    "M3"
  ),

  extract_fixed(
    m4_period,
    "M4"
  )
) |>
  filter(
    grepl(
      "gini_within",
      term
    )
  )


cat(
  "\nGini interaction coefficients:\n"
)

print(
  interaction_coefficients
)


# 10. Calculate implied period-specific slopes ---------------------------

# M3 ---------------------------------------------------------------------

m3_slopes <- bind_rows(

  # R1-R9
  linear_combo(
    m3_period,
    terms = c(
      "gini_within"
    ),
    weights = c(
      1
    ),
    model_name = "M3",
    effect_name = "R1-R9 slope"
  ),

  # R10
  linear_combo(
    m3_period,
    terms = c(
      "gini_within",
      "gini_within:r10"
    ),
    weights = c(
      1,
      1
    ),
    model_name = "M3",
    effect_name = "R10 slope"
  ),

  # R11
  linear_combo(
    m3_period,
    terms = c(
      "gini_within",
      "gini_within:r11"
    ),
    weights = c(
      1,
      1
    ),
    model_name = "M3",
    effect_name = "R11 slope"
  )
)


# M4 ---------------------------------------------------------------------

m4_slopes <- bind_rows(

  linear_combo(
    m4_period,
    terms = c(
      "gini_within_macro"
    ),
    weights = c(
      1
    ),
    model_name = "M4",
    effect_name = "R1-R9 slope"
  ),

  linear_combo(
    m4_period,
    terms = c(
      "gini_within_macro",
      "gini_within_macro:r10"
    ),
    weights = c(
      1,
      1
    ),
    model_name = "M4",
    effect_name = "R10 slope"
  ),

  linear_combo(
    m4_period,
    terms = c(
      "gini_within_macro",
      "gini_within_macro:r11"
    ),
    weights = c(
      1,
      1
    ),
    model_name = "M4",
    effect_name = "R11 slope"
  )
)


period_slopes <- bind_rows(
  m3_slopes,
  m4_slopes
)


cat(
  "\nImplied within-country Gini slopes:\n"
)

print(
  period_slopes
)


# 11. Direct slope-change tests ------------------------------------------

# These are the direct comparisons implied by the interaction
# coefficients.

slope_tests <- bind_rows(

  # M3: R10 vs R1-R9
  linear_combo(
    m3_period,
    terms = c(
      "gini_within:r10"
    ),
    weights = c(
      1
    ),
    model_name = "M3",
    effect_name = "R10 minus R1-R9"
  ),

  # M3: R11 vs R1-R9
  linear_combo(
    m3_period,
    terms = c(
      "gini_within:r11"
    ),
    weights = c(
      1
    ),
    model_name = "M3",
    effect_name = "R11 minus R1-R9"
  ),

  # M3: R11 vs R10
  linear_combo(
    m3_period,
    terms = c(
      "gini_within:r11",
      "gini_within:r10"
    ),
    weights = c(
      1,
      -1
    ),
    model_name = "M3",
    effect_name = "R11 minus R10"
  ),

  # M4: R10 vs R1-R9
  linear_combo(
    m4_period,
    terms = c(
      "gini_within_macro:r10"
    ),
    weights = c(
      1
    ),
    model_name = "M4",
    effect_name = "R10 minus R1-R9"
  ),

  # M4: R11 vs R1-R9
  linear_combo(
    m4_period,
    terms = c(
      "gini_within_macro:r11"
    ),
    weights = c(
      1
    ),
    model_name = "M4",
    effect_name = "R11 minus R1-R9"
  ),

  # M4: R11 vs R10
  linear_combo(
    m4_period,
    terms = c(
      "gini_within_macro:r11",
      "gini_within_macro:r10"
    ),
    weights = c(
      1,
      -1
    ),
    model_name = "M4",
    effect_name = "R11 minus R10"
  )
)


cat(
  "\nSlope-change tests:\n"
)

print(
  slope_tests
)


# 12. Joint interaction tests --------------------------------------------

# Compare each interaction model with the corresponding
# common-slope model. Because models use ML and identical
# samples, the likelihood-ratio test evaluates whether allowing
# separate R10/R11 slopes improves model fit jointly.

interaction_lrt <- bind_rows(
  extract_lrt(
    m3_base,
    m3_period,
    "M3"
  ),

  extract_lrt(
    m4_base,
    m4_period,
    "M4"
  )
)


cat(
  "\nJoint tests of R10/R11 slope interactions:\n"
)

print(
  interaction_lrt
)


# 13. Convergence / singularity ------------------------------------------

model_diagnostics <- tibble(
  model = c(
    "M3 base",
    "M3 period interaction",
    "M4 base",
    "M4 period interaction"
  ),

  n = c(
    nobs(m3_base),
    nobs(m3_period),
    nobs(m4_base),
    nobs(m4_period)
  ),

  singular = c(
    isSingular(
      m3_base,
      tol = 1e-4
    ),
    isSingular(
      m3_period,
      tol = 1e-4
    ),
    isSingular(
      m4_base,
      tol = 1e-4
    ),
    isSingular(
      m4_period,
      tol = 1e-4
    )
  )
)


cat(
  "\nModel diagnostics:\n"
)

print(
  model_diagnostics
)


# 14. Save outputs --------------------------------------------------------

dir.create(
  here(
    "05_output",
    "models"
  ),
  recursive = TRUE,
  showWarnings = FALSE
)


dir.create(
  here(
    "05_output",
    "tables"
  ),
  recursive = TRUE,
  showWarnings = FALSE
)


saveRDS(
  list(
    m3_base = m3_base,
    m3_period = m3_period,
    m4_base = m4_base,
    m4_period = m4_period
  ),
  here(
    "05_output",
    "models",
    "period_interaction_models.rds"
  )
)


write_csv(
  sample_summary,
  here(
    "05_output",
    "tables",
    "period_interaction_sample_summary.csv"
  )
)


write_csv(
  m3_period_variation,
  here(
    "05_output",
    "tables",
    "period_interaction_gini_variation.csv"
  )
)


write_csv(
  interaction_coefficients,
  here(
    "05_output",
    "tables",
    "period_interaction_coefficients.csv"
  )
)


write_csv(
  period_slopes,
  here(
    "05_output",
    "tables",
    "period_interaction_slopes.csv"
  )
)


write_csv(
  slope_tests,
  here(
    "05_output",
    "tables",
    "period_interaction_tests.csv"
  )
)


write_csv(
  interaction_lrt,
  here(
    "05_output",
    "tables",
    "period_interaction_lrt.csv"
  )
)


write_csv(
  model_diagnostics,
  here(
    "05_output",
    "tables",
    "period_interaction_model_diagnostics.csv"
  )
)
