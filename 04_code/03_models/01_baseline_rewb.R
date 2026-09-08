# ------------------------------------------------------------
# Baseline REWB models
# Project: Inequality and social cohesion
#
# Purpose:
#   Estimate baseline multilevel within-between models of
#   generalized social trust and income inequality.
#
#   Models:
#     M0       Empty three-level model
#     M1       Within- and between-country inequality
#     M2       + ESS round fixed effects
#     M3       + individual-level covariates
#
#   Macro-sample diagnostics:
#     M3_macro Same as M3 on macro-control sample
#     M4_gdp   + GDP
#     M4_unemp + unemployment
#     M4       + GDP and unemployment
# ------------------------------------------------------------

library(dplyr)
library(here)
library(lme4)
library(readr)

# 1. Import ---------------------------------------------------------------

ess <- readRDS(
  here(
    "03_data",
    "processed",
    "ess_analysis.rds"
  )
)

# Import country-round macro controls

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

# 2. Construct baseline within-between inequality variables ---------------

# Calculate country means from unique country-round observations,
# so ESS rounds with larger individual samples do not receive
# greater weight in the decomposition.

country_round_gini <- ess |>
  distinct(
    cntry,
    essround,
    gini_swiid_lag1
  ) |>
  filter(
    !is.na(gini_swiid_lag1)
  ) |>
  group_by(cntry) |>
  mutate(
    gini_between =
      mean(gini_swiid_lag1),
    
    gini_within =
      gini_swiid_lag1 -
      gini_between
  ) |>
  ungroup()

# Grand-mean centre the between-country component.
# This changes the intercept but not the between-country coefficient.

gini_grand_mean <- country_round_gini |>
  distinct(
    cntry,
    gini_between
  ) |>
  summarise(
    x = mean(gini_between)
  ) |>
  pull(x)

country_round_gini <- country_round_gini |>
  mutate(
    gini_between_c =
      gini_between -
      gini_grand_mean
  )

# Merge decomposition back to individuals

ess <- ess |>
  left_join(
    country_round_gini,
    by = c(
      "cntry",
      "essround",
      "gini_swiid_lag1"
    )
  )

# 3. Construct macro-model within-between variables -----------------------

# Define the country-round sample on which the macro models can be
# estimated. All contextual variables must be observed.

macro_context <- ess |>
  distinct(
    cntry,
    essround,
    gini_swiid_lag1,
    log_gdp_pc_ppp_lag1,
    unemployment_lag1
  ) |>
  filter(
    !is.na(gini_swiid_lag1),
    !is.na(log_gdp_pc_ppp_lag1),
    !is.na(unemployment_lag1)
  )

# Calculate country means using one observation per country-round.

macro_context <- macro_context |>
  group_by(cntry) |>
  mutate(
    
    # Gini
    gini_between_macro =
      mean(gini_swiid_lag1),
    
    gini_within_macro =
      gini_swiid_lag1 -
      gini_between_macro,
    
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

# Grand-mean centre the between-country components.

macro_means <- macro_context |>
  distinct(
    cntry,
    gini_between_macro,
    log_gdp_between,
    unemployment_between
  ) |>
  summarise(
    gini =
      mean(gini_between_macro),
    
    log_gdp =
      mean(log_gdp_between),
    
    unemployment =
      mean(unemployment_between)
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
    
    # Rescale logged GDP so a one-unit coefficient represents
    # a 0.1 log-point difference, approximately 10.5%.
    log_gdp_within_10 =
      log_gdp_within * 10,
    
    log_gdp_between_10 =
      log_gdp_between_c * 10
  )

# Merge macro decomposition back to individual-level data.

ess <- ess |>
  left_join(
    macro_context |>
      select(
        cntry,
        essround,
        gini_within_macro,
        gini_between_macro_c,
        log_gdp_within,
        log_gdp_between_c,
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

# 4. Construct common model variables ------------------------------------

ess <- ess |>
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
      )
  )

# 5. Define baseline analysis sample -------------------------------------

# Use the same complete-case sample for M0-M3 so comparisons are
# not driven by changes in sample size.

model_dat <- ess |>
  filter(
    !is.na(ppltrst),
    !is.na(gini_within),
    !is.na(gini_between_c),
    !is.na(age),
    !is.na(gender),
    !is.na(education_years)
  ) |>
  mutate(
    
    # Age expressed in centred decades.
    age_decade =
      (age - mean(age)) / 10,
    
    age_decade2 =
      age_decade^2,
    
    education_c =
      education_years -
      mean(education_years)
  )

cat(
  "\nBaseline analysis sample:",
  nrow(model_dat),
  "respondents\n"
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

# 6. Define common macro-analysis sample ---------------------------------

macro_dat <- ess |>
  filter(
    !is.na(ppltrst),
    !is.na(gini_within_macro),
    !is.na(gini_between_macro_c),
    !is.na(log_gdp_within_10),
    !is.na(log_gdp_between_10),
    !is.na(unemployment_within),
    !is.na(unemployment_between_c),
    !is.na(age),
    !is.na(gender),
    !is.na(education_years)
  ) |>
  mutate(
    
    # Re-centre covariates within the macro-model sample.
    age_decade =
      (age - mean(age)) / 10,
    
    age_decade2 =
      age_decade^2,
    
    education_c =
      education_years -
      mean(education_years)
  )

cat(
  "\nMacro analysis sample:",
  nrow(macro_dat),
  "respondents\n"
)

cat(
  "Countries:",
  n_distinct(macro_dat$cntry),
  "\n"
)

cat(
  "Country-rounds:",
  n_distinct(macro_dat$country_round),
  "\n\n"
)

# 7. Model control --------------------------------------------------------

ctrl <- lmerControl(
  optimizer = "bobyqa",
  optCtrl = list(
    maxfun = 200000
  )
)

# Models are estimated using maximum likelihood (REML = FALSE)
# because models with different fixed effects are compared.

# 8. Helper functions -----------------------------------------------------

extract_fixed <- function(model, model_name) {
  
  x <- as.data.frame(
    coef(summary(model))
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

# 9. M0: empty three-level model -----------------------------------------

m0 <- lmer(
  ppltrst ~
    1 +
    (1 | cntry) +
    (1 | country_round),
  data = model_dat,
  REML = FALSE,
  control = ctrl
)

# 10. M1: within- and between-country inequality -------------------------

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

# 11. M2: add ESS-round fixed effects ------------------------------------

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

# 12. M3: add individual-level covariates --------------------------------

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

# 13. Refit M3 on macro-model sample -------------------------------------

# This provides the correct reference model for M4 comparisons,
# because M3_macro and all M4 variants use exactly the same respondents.

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

# 14. M4a: GDP only -------------------------------------------------------

m4_gdp <- lmer(
  ppltrst ~
    gini_within_macro +
    gini_between_macro_c +
    log_gdp_within_10 +
    log_gdp_between_10 +
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

# 15. M4b: unemployment only ---------------------------------------------

m4_unemp <- lmer(
  ppltrst ~
    gini_within_macro +
    gini_between_macro_c +
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

# 16. M4: GDP and unemployment -------------------------------------------

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

# 17. Print model summaries ----------------------------------------------

cat("\n--- M0: Empty model ---\n")
print(summary(m0))

cat("\n--- M1: Inequality ---\n")
print(summary(m1))

cat("\n--- M2: + round effects ---\n")
print(summary(m2))

cat("\n--- M3: + individual controls ---\n")
print(summary(m3))

cat("\n--- M3 on macro sample ---\n")
print(summary(m3_macro))

cat("\n--- M4a: + GDP ---\n")
print(summary(m4_gdp))

cat("\n--- M4b: + unemployment ---\n")
print(summary(m4_unemp))

cat("\n--- M4: + GDP and unemployment ---\n")
print(summary(m4))

# 18. Null-model variance decomposition ----------------------------------

vc0 <- as.data.frame(
  VarCorr(m0)
)

var_country <- vc0 |>
  filter(
    grp == "cntry"
  ) |>
  pull(vcov)

var_country_round <- vc0 |>
  filter(
    grp == "country_round"
  ) |>
  pull(vcov)

var_residual <-
  sigma(m0)^2

var_total <-
  var_country +
  var_country_round +
  var_residual

icc_country <-
  var_country /
  var_total

icc_country_round <-
  var_country_round /
  var_total

cat(
  "\nNull-model variance components:\n",
  "Country variance:",
  var_country,
  "\n",
  "Country-round variance:",
  var_country_round,
  "\n",
  "Residual variance:",
  var_residual,
  "\n",
  "Country ICC:",
  icc_country,
  "\n",
  "Country-round ICC:",
  icc_country_round,
  "\n"
)

# 19. Baseline fixed effects ---------------------------------------------

fixed_effects <- bind_rows(
  extract_fixed(
    m0,
    "M0"
  ),
  extract_fixed(
    m1,
    "M1"
  ),
  extract_fixed(
    m2,
    "M2"
  ),
  extract_fixed(
    m3,
    "M3"
  )
)

# 20. Baseline model fit --------------------------------------------------

model_fit <- bind_rows(
  extract_fit(
    m0,
    "M0"
  ),
  extract_fit(
    m1,
    "M1"
  ),
  extract_fit(
    m2,
    "M2"
  ),
  extract_fit(
    m3,
    "M3"
  )
)

cat("\nBaseline model comparison:\n")
print(model_fit)

# 21. Focused baseline Gini results --------------------------------------

gini_results <- fixed_effects |>
  filter(
    term %in% c(
      "gini_within",
      "gini_between_c"
    )
  )

cat("\nBaseline Gini coefficients:\n")
print(gini_results)

# 22. Macro-model fixed effects ------------------------------------------

macro_fixed_effects <- bind_rows(
  extract_fixed(
    m3_macro,
    "M3 macro sample"
  ),
  extract_fixed(
    m4_gdp,
    "M4a + GDP"
  ),
  extract_fixed(
    m4_unemp,
    "M4b + unemployment"
  ),
  extract_fixed(
    m4,
    "M4 + GDP and unemployment"
  )
)

macro_results <- macro_fixed_effects |>
  filter(
    term %in% c(
      "gini_within_macro",
      "gini_between_macro_c",
      "log_gdp_within_10",
      "log_gdp_between_10",
      "unemployment_within",
      "unemployment_between_c"
    )
  )

cat("\nMacro-model coefficients:\n")
print(macro_results)

# 23. Macro-model fit -----------------------------------------------------

macro_model_fit <- bind_rows(
  extract_fit(
    m3_macro,
    "M3 macro sample"
  ),
  extract_fit(
    m4_gdp,
    "M4a + GDP"
  ),
  extract_fit(
    m4_unemp,
    "M4b + unemployment"
  ),
  extract_fit(
    m4,
    "M4 + GDP and unemployment"
  )
)

cat("\nMacro model comparison:\n")
print(macro_model_fit)

# 24. Contextual correlation matrix --------------------------------------

context_correlations <- macro_context |>
  select(
    gini_within_macro,
    gini_between_macro_c,
    log_gdp_within,
    log_gdp_between_c,
    unemployment_within,
    unemployment_between_c
  ) |>
  cor(
    use = "pairwise.complete.obs"
  ) |>
  round(3)

cat("\nContextual correlation matrix:\n")
print(context_correlations)

# Convert matrix to data frame for CSV output.

context_correlations_df <-
  as.data.frame(context_correlations) |>
  mutate(
    variable =
      rownames(context_correlations)
  ) |>
  select(
    variable,
    everything()
  )

rownames(context_correlations_df) <- NULL

# 25. Create output directories ------------------------------------------

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

# 26. Save model objects --------------------------------------------------

saveRDS(
  list(
    m0 = m0,
    m1 = m1,
    m2 = m2,
    m3 = m3,
    m3_macro = m3_macro,
    m4_gdp = m4_gdp,
    m4_unemp = m4_unemp,
    m4 = m4
  ),
  here(
    "05_output",
    "models",
    "baseline_rewb_models.rds"
  )
)

# 27. Save tables ---------------------------------------------------------

write_csv(
  fixed_effects,
  here(
    "05_output",
    "tables",
    "baseline_rewb_fixed_effects.csv"
  )
)

write_csv(
  model_fit,
  here(
    "05_output",
    "tables",
    "baseline_rewb_model_fit.csv"
  )
)

write_csv(
  gini_results,
  here(
    "05_output",
    "tables",
    "baseline_rewb_gini_results.csv"
  )
)

write_csv(
  macro_fixed_effects,
  here(
    "05_output",
    "tables",
    "baseline_rewb_macro_fixed_effects.csv"
  )
)

write_csv(
  macro_results,
  here(
    "05_output",
    "tables",
    "baseline_rewb_macro_results.csv"
  )
)

write_csv(
  macro_model_fit,
  here(
    "05_output",
    "tables",
    "baseline_rewb_macro_model_fit.csv"
  )
)

write_csv(
  context_correlations_df,
  here(
    "05_output",
    "tables",
    "baseline_rewb_context_correlations.csv"
  )
)
