# ------------------------------------------------------------
# OECD inequality robustness
# Project: Inequality and social cohesion
#
# Purpose:
#   Compare SWIID and OECD disposable-income Gini estimates
#   using the same OECD-covered ESS sample.
#
#   Models are estimated both:
#     1. with round effects + individual controls
#     2. with additional GDP and unemployment controls
# ------------------------------------------------------------

library(dplyr)
library(here)
library(lme4)
library(readr)

# 1. Import individual ESS data ------------------------------------------

ess <- readRDS(
  here(
    "03_data",
    "processed",
    "ess_analysis.rds"
  )
)

# 2. Import macro controls ------------------------------------------------

macro <- read_csv(
  here(
    "03_data",
    "interim",
    "ess_country_round_macro.csv"
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

# 3. Define common country-round sample ----------------------------------

# Require both inequality measures and both macro controls.
# This ensures all robustness models use exactly the same
# contextual observations.

context <- ess |>
  distinct(
    cntry,
    essround,
    gini_swiid_lag1,
    gini_oecd_lag1,
    log_gdp_pc_ppp_lag1,
    unemployment_lag1
  ) |>
  filter(
    !is.na(gini_swiid_lag1),
    !is.na(gini_oecd_lag1),
    !is.na(log_gdp_pc_ppp_lag1),
    !is.na(unemployment_lag1)
  )

cat(
  "\nOECD robustness country-rounds:",
  nrow(context),
  "\n"
)

cat(
  "Countries:",
  n_distinct(context$cntry),
  "\n\n"
)

# 4. Within-between decomposition ----------------------------------------

context <- context |>
  group_by(cntry) |>
  mutate(

    # SWIID
    swiid_between =
      mean(gini_swiid_lag1),

    swiid_within =
      gini_swiid_lag1 -
      swiid_between,

    # OECD
    oecd_between =
      mean(gini_oecd_lag1),

    oecd_within =
      gini_oecd_lag1 -
      oecd_between,

    # GDP
    log_gdp_between =
      mean(log_gdp_pc_ppp_lag1),

    log_gdp_within =
      log_gdp_pc_ppp_lag1 -
      log_gdp_between,

    # Unemployment
    unemployment_between =
      mean(unemployment_lag1),

    unemployment_within =
      unemployment_lag1 -
      unemployment_between
  ) |>
  ungroup()

# 5. Grand-mean centre between-country components ------------------------

context_means <- context |>
  distinct(
    cntry,
    swiid_between,
    oecd_between,
    log_gdp_between,
    unemployment_between
  ) |>
  summarise(
    swiid =
      mean(swiid_between),

    oecd =
      mean(oecd_between),

    log_gdp =
      mean(log_gdp_between),

    unemployment =
      mean(unemployment_between)
  )

context <- context |>
  mutate(
    swiid_between_c =
      swiid_between -
      context_means$swiid,

    oecd_between_c =
      oecd_between -
      context_means$oecd,

    log_gdp_between_c =
      log_gdp_between -
      context_means$log_gdp,

    unemployment_between_c =
      unemployment_between -
      context_means$unemployment,

    # GDP in 0.1 log-point units
    log_gdp_within_10 =
      log_gdp_within * 10,

    log_gdp_between_10 =
      log_gdp_between_c * 10
  )

# 6. Merge contextual variables to individuals ---------------------------

ess_oecd <- ess |>
  inner_join(
    context |>
      select(
        cntry,
        essround,
        swiid_within,
        swiid_between_c,
        oecd_within,
        oecd_between_c,
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

# 7. Define common individual-level sample -------------------------------

model_dat <- ess_oecd |>
  filter(
    !is.na(ppltrst),
    !is.na(age),
    !is.na(gender),
    !is.na(education_years)
  ) |>
  mutate(
    country_round =
      interaction(
        cntry,
        essround,
        drop = TRUE
      ),

    round_factor =
      factor(essround),

    gender =
      factor(
        gender,
        levels = c(
          "Man",
          "Woman"
        )
      ),

    age_decade =
      (age - mean(age)) / 10,

    age_decade2 =
      age_decade^2,

    education_c =
      education_years -
      mean(education_years)
  )

cat(
  "\nIndividual analysis sample:",
  nrow(model_dat),
  "\n"
)

cat(
  "Countries:",
  n_distinct(model_dat$cntry),
  "\n"
)

cat(
  "Country-rounds:",
  n_distinct(model_dat$country_round),
  "\n\n"
)

# 8. Model control --------------------------------------------------------

ctrl <- lmerControl(
  optimizer = "bobyqa",
  optCtrl = list(
    maxfun = 200000
  )
)

# 9. SWIID model on OECD-covered sample ----------------------------------

m_swiid <- lmer(
  ppltrst ~
    swiid_within +
    swiid_between_c +
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

# 10. OECD equivalent -----------------------------------------------------

m_oecd <- lmer(
  ppltrst ~
    oecd_within +
    oecd_between_c +
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

# 11. SWIID + macro controls ---------------------------------------------

m_swiid_macro <- lmer(
  ppltrst ~
    swiid_within +
    swiid_between_c +
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
  data = model_dat,
  REML = FALSE,
  control = ctrl
)

# 12. OECD + macro controls ----------------------------------------------

m_oecd_macro <- lmer(
  ppltrst ~
    oecd_within +
    oecd_between_c +
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
  data = model_dat,
  REML = FALSE,
  control = ctrl
)

# 13. Helper functions ----------------------------------------------------

extract_fixed <- function(fit, model_name) {

  x <- as.data.frame(
    coef(summary(fit))
  )

  x$term <- rownames(x)

  rownames(x) <- NULL

  x |>
    transmute(
      model = model_name,
      term,
      estimate = Estimate,
      std_error = `Std. Error`,
      statistic = `t value`,
      conf_low =
        estimate - 1.96 * std_error,
      conf_high =
        estimate + 1.96 * std_error
    )
}

extract_fit <- function(fit, model_name) {

  tibble(
    model = model_name,
    n = nobs(fit),
    logLik =
      as.numeric(logLik(fit)),
    AIC =
      AIC(fit),
    BIC =
      BIC(fit)
  )
}

# 14. Extract focused inequality results ---------------------------------

fixed_effects <- bind_rows(
  extract_fixed(
    m_swiid,
    "SWIID"
  ),
  extract_fixed(
    m_oecd,
    "OECD"
  ),
  extract_fixed(
    m_swiid_macro,
    "SWIID + macro"
  ),
  extract_fixed(
    m_oecd_macro,
    "OECD + macro"
  )
)

inequality_results <- fixed_effects |>
  filter(
    term %in% c(
      "swiid_within",
      "swiid_between_c",
      "oecd_within",
      "oecd_between_c"
    )
  )

cat(
  "\nInequality robustness results:\n"
)

print(
  inequality_results
)

# 15. Model fit -----------------------------------------------------------

model_fit <- bind_rows(
  extract_fit(
    m_swiid,
    "SWIID"
  ),
  extract_fit(
    m_oecd,
    "OECD"
  ),
  extract_fit(
    m_swiid_macro,
    "SWIID + macro"
  ),
  extract_fit(
    m_oecd_macro,
    "OECD + macro"
  )
)

cat(
  "\nModel comparison:\n"
)

print(
  model_fit
)

# 16. Compare inequality measures ----------------------------------------

source_correlations <- tibble(

  comparison = c(
    "Country-round levels",
    "Within-country deviations",
    "Between-country means"
  ),

  correlation = c(

    cor(
      context$gini_swiid_lag1,
      context$gini_oecd_lag1
    ),

    cor(
      context$swiid_within,
      context$oecd_within
    ),

    cor(
      context$swiid_between,
      context$oecd_between
    )
  )
)

cat(
  "\nSWIID-OECD correlations:\n"
)

print(
  source_correlations
)

# 17. Save outputs --------------------------------------------------------

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
    m_swiid = m_swiid,
    m_oecd = m_oecd,
    m_swiid_macro = m_swiid_macro,
    m_oecd_macro = m_oecd_macro
  ),
  here(
    "05_output",
    "models",
    "oecd_robustness_models.rds"
  )
)

write_csv(
  fixed_effects,
  here(
    "05_output",
    "tables",
    "oecd_robustness_fixed_effects.csv"
  )
)

write_csv(
  inequality_results,
  here(
    "05_output",
    "tables",
    "oecd_robustness_inequality_results.csv"
  )
)

write_csv(
  model_fit,
  here(
    "05_output",
    "tables",
    "oecd_robustness_model_fit.csv"
  )
)

write_csv(
  source_correlations,
  here(
    "05_output",
    "tables",
    "oecd_swiid_correlations.csv"
  )
)
