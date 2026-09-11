# ------------------------------------------------------------
# Extended REWB models: ESS Rounds 1-11
# Project: Inequality and social cohesion
#
# Purpose:
#   Examine whether extending the ESS analysis from Rounds 1-9
#   through Rounds 10-11 changes the estimated relationship
#   between income inequality and generalized social trust.
#
# Strategy:
#   Estimate the same REWB model sequence separately for:
#     1. Rounds 1-9
#     2. Rounds 1-11
#
#   Within-between decompositions are recalculated separately
#   for each analytical period from unique country-round
#   observations.
#
#   No post-R9 interaction is introduced yet.
# ------------------------------------------------------------


library(dplyr)
library(here)
library(lme4)
library(readr)
library(tidyr)


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
  "\nExtended ESS dataset:",
  nrow(ess),
  "respondents\n"
)

cat(
  "Rounds:",
  min(ess$essround),
  "to",
  max(ess$essround),
  "\n\n"
)


# 2. Helpers --------------------------------------------------------------

extract_fixed <- function(
  fit,
  sample_id,
  model_name
) {

  x <- as.data.frame(
    coef(
      summary(fit)
    )
  )

  x$term <- rownames(x)

  rownames(x) <- NULL


  x |>
    transmute(
      sample = sample_id,
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


extract_fit <- function(
  fit,
  sample_id,
  model_name
) {

  tibble(
    sample = sample_id,
    model = model_name,
    n = nobs(fit),
    logLik =
      as.numeric(
        logLik(fit)
      ),
    AIC = AIC(fit),
    BIC = BIC(fit),
    singular =
      isSingular(
        fit,
        tol = 1e-4
      )
  )
}


# 3. Prepare one analytical period ---------------------------------------

prepare_period <- function(
  dat,
  sample_id
) {

  # ----------------------------------------------------------
  # A. Gini within-between decomposition
  # ----------------------------------------------------------

  # Use one observation per country-round so that countries/
  # rounds with larger respondent samples do not receive
  # greater weight in the contextual decomposition.

  country_round_gini <- dat |>
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


  # Grand-mean centre between-country Gini.

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


  dat <- dat |>
    left_join(
      country_round_gini,
      by = c(
        "cntry",
        "essround",
        "gini_swiid_lag1"
      )
    )


  # ----------------------------------------------------------
  # B. Macro-sample contextual decomposition
  # ----------------------------------------------------------

  # Define the complete contextual-data sample before
  # calculating country means.

  macro_context <- dat |>
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
    )


  macro_context <- macro_context |>
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


  # Grand-mean centre between-country contextual components.

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

      # Express GDP coefficients per 0.1 log-point difference.
      log_gdp_within_10 =
        log_gdp_within * 10,

      log_gdp_between_10 =
        log_gdp_between_c * 10
    )


  dat <- dat |>
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
    )


  # ----------------------------------------------------------
  # C. Main M1-M3 sample
  # ----------------------------------------------------------

  model_dat <- dat |>
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

      # Age in decades avoids the scale warning encountered
      # in the earlier baseline specification.
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


  # ----------------------------------------------------------
  # D. Macro-complete sample
  # ----------------------------------------------------------

  macro_dat <- dat |>
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

      # Re-centre covariates on the actual macro-model sample.
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


  sample_summary <- tibble(
    sample = sample_id,

    analysis_sample = c(
      "M1-M3",
      "Macro"
    ),

    n_respondents = c(
      nrow(model_dat),
      nrow(macro_dat)
    ),

    n_countries = c(
      n_distinct(
        model_dat$cntry
      ),
      n_distinct(
        macro_dat$cntry
      )
    ),

    n_country_rounds = c(
      n_distinct(
        model_dat$country_round
      ),
      n_distinct(
        macro_dat$country_round
      )
    )
  )


  list(
    model_dat =
      model_dat,

    macro_dat =
      macro_dat,

    sample_summary =
      sample_summary
  )
}


# 4. Fit one model sequence ----------------------------------------------

fit_period <- function(
  prepared,
  sample_id
) {

  model_dat <-
    prepared$model_dat

  macro_dat <-
    prepared$macro_dat


  ctrl <- lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(
      maxfun = 200000
    )
  )


  # No survey weights are applied here, matching the existing
  # baseline REWB specification. Weighting can be examined
  # separately as a robustness issue.


  # M1: inequality only ---------------------------------------------------

  m1 <- lmer(
    ppltrst ~
      gini_within +
      gini_between_c +
      (1 | cntry) +
      (1 | country_round),
    data = model_dat,
    REML = FALSE,
    control = ctrl
  )


  # M2: + ESS-round fixed effects ----------------------------------------

  m2 <- lmer(
    ppltrst ~
      gini_within +
      gini_between_c +
      round_factor +
      (1 | cntry) +
      (1 | country_round),
    data = model_dat,
    REML = FALSE,
    control = ctrl
  )


  # M3: + individual controls --------------------------------------------

  m3 <- lmer(
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
    data = model_dat,
    REML = FALSE,
    control = ctrl
  )


  # M3 on the macro-complete contextual sample ---------------------------

  m3_macro <- lmer(
    ppltrst ~
      gini_within_macro +
      gini_between_macro_c +
      round_factor +
      age_decade +
      age_decade2 +
      gender +
      education_c +
      (1 | cntry) +
      (1 | country_round),
    data = macro_dat,
    REML = FALSE,
    control = ctrl
  )


  # M4: + GDP and unemployment -------------------------------------------

  m4 <- lmer(
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
    data = macro_dat,
    REML = FALSE,
    control = ctrl
  )


  fixed_effects <- bind_rows(
    extract_fixed(
      m1,
      sample_id,
      "M1"
    ),

    extract_fixed(
      m2,
      sample_id,
      "M2"
    ),

    extract_fixed(
      m3,
      sample_id,
      "M3"
    ),

    extract_fixed(
      m3_macro,
      sample_id,
      "M3 macro sample"
    ),

    extract_fixed(
      m4,
      sample_id,
      "M4"
    )
  )


  model_fit <- bind_rows(
    extract_fit(
      m1,
      sample_id,
      "M1"
    ),

    extract_fit(
      m2,
      sample_id,
      "M2"
    ),

    extract_fit(
      m3,
      sample_id,
      "M3"
    ),

    extract_fit(
      m3_macro,
      sample_id,
      "M3 macro sample"
    ),

    extract_fit(
      m4,
      sample_id,
      "M4"
    )
  )


  list(
    models = list(
      m1 = m1,
      m2 = m2,
      m3 = m3,
      m3_macro = m3_macro,
      m4 = m4
    ),

    fixed_effects =
      fixed_effects,

    model_fit =
      model_fit
  )
}


# 5. Prepare R1-R9 --------------------------------------------------------

prepared_r1_r9 <- prepare_period(
  ess |>
    filter(
      essround <= 9
    ),
  "R1-R9"
)


cat(
  "\nR1-R9 samples:\n"
)

print(
  prepared_r1_r9$sample_summary
)


# 6. Prepare R1-R11 -------------------------------------------------------

prepared_r1_r11 <- prepare_period(
  ess,
  "R1-R11"
)


cat(
  "\nR1-R11 samples:\n"
)

print(
  prepared_r1_r11$sample_summary
)


# 7. Estimate both periods ------------------------------------------------

cat(
  "\nFitting R1-R9 models...\n"
)

fits_r1_r9 <- fit_period(
  prepared_r1_r9,
  "R1-R9"
)


cat(
  "\nFitting R1-R11 models...\n"
)

fits_r1_r11 <- fit_period(
  prepared_r1_r11,
  "R1-R11"
)


cat(
  "\nModels complete.\n"
)


# 8. Combine outputs ------------------------------------------------------

fixed_effects <- bind_rows(
  fits_r1_r9$fixed_effects,
  fits_r1_r11$fixed_effects
)


model_fit <- bind_rows(
  fits_r1_r9$model_fit,
  fits_r1_r11$model_fit
)


sample_summary <- bind_rows(
  prepared_r1_r9$sample_summary,
  prepared_r1_r11$sample_summary
)


# 9. Focused inequality results ------------------------------------------

gini_results <- fixed_effects |>
  filter(
    term %in% c(
      "gini_within",
      "gini_between_c",
      "gini_within_macro",
      "gini_between_macro_c"
    )
  ) |>
  mutate(
    effect = case_when(
      term %in% c(
        "gini_within",
        "gini_within_macro"
      ) ~
        "Within-country Gini",

      term %in% c(
        "gini_between_c",
        "gini_between_macro_c"
      ) ~
        "Between-country Gini"
    )
  ) |>
  select(
    sample,
    model,
    effect,
    estimate,
    std_error,
    conf_low,
    conf_high
  )


cat(
  "\nFocused Gini comparison:\n"
)

print(
  as_tibble(gini_results),
  n = Inf
)


# 10. Main comparison -----------------------------------------------------

# Focus on the controlled specifications.
#
# The estimate difference below is descriptive only. It is NOT a
# formal statistical test of whether the coefficients differ.

main_comparison <- gini_results |>
  filter(
    model %in% c(
      "M3",
      "M3 macro sample",
      "M4"
    )
  ) |>
  select(
    sample,
    model,
    effect,
    estimate
  ) |>
  pivot_wider(
    names_from = sample,
    values_from = estimate
  ) |>
  mutate(
    difference_r1_r11_minus_r1_r9 =
      `R1-R11` -
      `R1-R9`
  )


cat(
  "\nR1-R9 versus R1-R11 coefficient comparison:\n"
)

print(
  main_comparison,
  n = Inf
)


# 11. Macro coefficients --------------------------------------------------

macro_results <- fixed_effects |>
  filter(
    model == "M4",
    term %in% c(
      "gini_within_macro",
      "gini_between_macro_c",
      "log_gdp_within_10",
      "log_gdp_between_10",
      "unemployment_within",
      "unemployment_between_c"
    )
  )


cat(
  "\nM4 contextual coefficients:\n"
)

print(
  as_tibble(macro_results),
  n = Inf
)


# 12. Save outputs --------------------------------------------------------

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
    r1_r9 =
      fits_r1_r9$models,

    r1_r11 =
      fits_r1_r11$models
  ),
  here(
    "05_output",
    "models",
    "extended_rewb_models.rds"
  )
)


write_csv(
  fixed_effects,
  here(
    "05_output",
    "tables",
    "extended_rewb_fixed_effects.csv"
  )
)


write_csv(
  model_fit,
  here(
    "05_output",
    "tables",
    "extended_rewb_model_fit.csv"
  )
)


write_csv(
  sample_summary,
  here(
    "05_output",
    "tables",
    "extended_rewb_sample_summary.csv"
  )
)


write_csv(
  gini_results,
  here(
    "05_output",
    "tables",
    "extended_rewb_gini_results.csv"
  )
)


write_csv(
  main_comparison,
  here(
    "05_output",
    "tables",
    "extended_rewb_main_comparison.csv"
  )
)


write_csv(
  macro_results,
  here(
    "05_output",
    "tables",
    "extended_rewb_macro_results.csv"
  )
)
