# ------------------------------------------------------------
# SWIID uncertainty analysis
# Project: Inequality and social cohesion
#
# Purpose:
#   Propagate uncertainty in SWIID disposable-income Gini
#   estimates through the baseline REWB models.
#
#   For each SWIID imputation:
#     1. construct lagged country-round Gini exposure;
#     2. construct within/between Gini components;
#     3. estimate M3 and M4;
#     4. retain fixed-effect estimates and variances.
#
#   Estimates are then pooled using Rubin's rules.
# ------------------------------------------------------------

library(dplyr)
library(here)
library(lme4)
library(readr)

# 1. Select imputations ---------------------------------------------------

# TEST RUN:
imputations_to_run <- seq_along(swiid_draws)

# For the final analysis, replace with:
# imputations_to_run <- 1:100


# 2. Load multiply-imputed SWIID data ------------------------------------

swiid_file <- list.files(
  here(
    "03_data",
    "raw",
    "inequality",
    "swiid"
  ),
  pattern = "\\.(rda|RData)$",
  recursive = TRUE,
  full.names = TRUE
)

stopifnot(length(swiid_file) == 1)

swiid_env <- new.env()

load(
  swiid_file,
  envir = swiid_env
)

swiid_draws <- swiid_env$swiid

stopifnot(
  is.list(swiid_draws),
  length(swiid_draws) >= max(imputations_to_run)
)

cat(
  "\nSWIID imputations available:",
  length(swiid_draws),
  "\n"
)

cat(
  "Imputations to run:",
  paste(imputations_to_run, collapse = ", "),
  "\n\n"
)


# 3. Import ESS interview-year structure ---------------------------------

coverage <- read_csv(
  here(
    "03_data",
    "interim",
    "ess_inequality_coverage.csv"
  ),
  show_col_types = FALSE
) |>
  filter(
    !is.na(interview_year)
  ) |>
  mutate(
    interview_year =
      as.integer(interview_year),

    lag_year =
      interview_year - 1
  ) |>
  group_by(
    cntry,
    essround
  ) |>
  mutate(
    interview_year_share =
      n_respondents /
      sum(n_respondents)
  ) |>
  ungroup()


# 4. Import individual-level ESS data ------------------------------------

ess <- readRDS(
  here(
    "03_data",
    "processed",
    "ess_analysis.rds"
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


# 5. Prepare M3 individual-level base sample -----------------------------

m3_base <- ess |>
  filter(
    !is.na(ppltrst),
    !is.na(age),
    !is.na(gender),
    !is.na(education_years)
  ) |>
  mutate(
    age_decade =
      (age - mean(age)) / 10,

    age_decade2 =
      age_decade^2,

    education_c =
      education_years -
      mean(education_years)
  )


# 6. Prepare macro controls for M4 ---------------------------------------

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
  ) |>
  filter(
    !is.na(log_gdp_pc_ppp_lag1),
    !is.na(unemployment_lag1)
  )

# Decompose GDP and unemployment using one row per country-round.

macro_context <- macro |>
  group_by(cntry) |>
  mutate(
    log_gdp_between =
      mean(log_gdp_pc_ppp_lag1),

    log_gdp_within =
      log_gdp_pc_ppp_lag1 -
      log_gdp_between,

    unemployment_between =
      mean(unemployment_lag1),

    unemployment_within =
      unemployment_lag1 -
      unemployment_between
  ) |>
  ungroup()

macro_means <- macro_context |>
  distinct(
    cntry,
    log_gdp_between,
    unemployment_between
  ) |>
  summarise(
    log_gdp =
      mean(log_gdp_between),

    unemployment =
      mean(unemployment_between)
  )

macro_context <- macro_context |>
  mutate(
    log_gdp_between_c =
      log_gdp_between -
      macro_means$log_gdp,

    unemployment_between_c =
      unemployment_between -
      macro_means$unemployment,

    # 0.1 log-point GDP units
    log_gdp_within_10 =
      log_gdp_within * 10,

    log_gdp_between_10 =
      log_gdp_between_c * 10
  )


# 7. Model control --------------------------------------------------------

ctrl <- lmerControl(
  optimizer = "bobyqa",
  optCtrl = list(
    maxfun = 200000
  )
)


# 8. Helper: construct Gini for one SWIID imputation ---------------------

construct_gini <- function(draw) {

  annual <- draw |>
    transmute(
      swiid_country = country,
      year = as.integer(year),

      # SWIID MI file stores Gini in hundredths:
      # e.g. 3287 = 32.87
      gini = as.numeric(gini_disp) / 100
    )

  coverage |>
    left_join(
      annual,
      by = c(
        "swiid_country",
        "lag_year" = "year"
      )
    ) |>
    group_by(
      cntry,
      essround
    ) |>
    summarise(
      gini_lag1 =
        if (all(!is.na(gini))) {
          weighted.mean(
            gini,
            interview_year_share
          )
        } else {
          NA_real_
        },

      .groups = "drop"
    ) |>
    filter(
      !is.na(gini_lag1)
    )
}


# 9. Helper: Gini within-between decomposition ---------------------------

decompose_gini <- function(gini_context) {

  x <- gini_context |>
    group_by(cntry) |>
    mutate(
      gini_between =
        mean(gini_lag1),

      gini_within =
        gini_lag1 -
        gini_between
    ) |>
    ungroup()

  grand_mean <- x |>
    distinct(
      cntry,
      gini_between
    ) |>
    summarise(
      x = mean(gini_between)
    ) |>
    pull(x)

  x |>
    mutate(
      gini_between_c =
        gini_between -
        grand_mean
    )
}


# 10. Helper: extract coefficients ---------------------------------------

extract_terms <- function(
  fit,
  imputation,
  model_name
) {

  coefs <- coef(summary(fit))

  vc <- vcov(fit)

  tibble(
    imputation = imputation,
    model = model_name,
    term = rownames(coefs),
    estimate =
      coefs[, "Estimate"],
    std_error =
      sqrt(diag(vc))
  )
}


# 11. Fit models across imputations --------------------------------------

results <- vector(
  "list",
  length(imputations_to_run)
)

for (j in seq_along(imputations_to_run)) {

  imp <- imputations_to_run[j]

  cat(
    "\n---------------------------------\n",
    "Imputation",
    imp,
    "of",
    max(imputations_to_run),
    "\n"
  )

  # Construct imputation-specific Gini -------------------------------

  gini_all <- construct_gini(
    swiid_draws[[imp]]
  )

  # ------------------------------------------------------------
  # M3
  # ------------------------------------------------------------

  gini_m3 <- decompose_gini(
    gini_all
  )

  dat_m3 <- m3_base |>
    inner_join(
      gini_m3 |>
        select(
          cntry,
          essround,
          gini_within,
          gini_between_c
        ),
      by = c(
        "cntry",
        "essround"
      )
    )

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
    data = dat_m3,
    REML = FALSE,
    control = ctrl
  )

  # ------------------------------------------------------------
  # M4
  #
  # Restrict Gini decomposition to the macro-control sample before
  # calculating country means. This reproduces the logic of the
  # baseline M4 model.
  # ------------------------------------------------------------

  gini_m4 <- gini_all |>
    inner_join(
      macro_context,
      by = c(
        "cntry",
        "essround"
      )
    ) |>
    select(
      cntry,
      essround,
      gini_lag1,
      log_gdp_within_10,
      log_gdp_between_10,
      unemployment_within,
      unemployment_between_c
    )

  # Gini decomposition on M4 contextual sample

  gini_m4_decomp <- gini_m4 |>
    select(
      cntry,
      essround,
      gini_lag1
    ) |>
    decompose_gini()

  gini_m4 <- gini_m4 |>
    left_join(
      gini_m4_decomp |>
        select(
          cntry,
          essround,
          gini_within,
          gini_between_c
        ),
      by = c(
        "cntry",
        "essround"
      )
    )

  dat_m4 <- m3_base |>
    inner_join(
      gini_m4,
      by = c(
        "cntry",
        "essround"
      )
    )

  # Re-centre individual covariates on M4 sample to match baseline.

  dat_m4 <- dat_m4 |>
    mutate(
      age_decade =
        (age - mean(age)) / 10,

      age_decade2 =
        age_decade^2,

      education_c =
        education_years -
        mean(education_years)
    )

  m4 <- lmer(
    ppltrst ~
      gini_within +
      gini_between_c +
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
    data = dat_m4,
    REML = FALSE,
    control = ctrl
  )

  # Save fixed-effect information ------------------------------------

  results[[j]] <- bind_rows(
    extract_terms(
      m3,
      imp,
      "M3"
    ),

    extract_terms(
      m4,
      imp,
      "M4"
    )
  )

  cat(
    "M3 N:",
    nobs(m3),
    "| M4 N:",
    nobs(m4),
    "\n"
  )

  cat(
    "M3 within Gini:",
    fixef(m3)["gini_within"],
    "\n"
  )

  cat(
    "M4 within Gini:",
    fixef(m4)["gini_within"],
    "\n"
  )
}

draw_results <- bind_rows(results)


# 12. Pool estimates using Rubin's rules ---------------------------------

pool_rubin <- function(data) {

  m <- nrow(data)

  q_bar <-
    mean(data$estimate)

  u_bar <-
    mean(data$std_error^2)

  b <-
    var(data$estimate)

  total_var <-
    u_bar +
    (1 + 1 / m) * b

  pooled_se <-
    sqrt(total_var)

  # Relative increase in variance due to imputation uncertainty

  r <-
    if (u_bar > 0) {
      ((1 + 1 / m) * b) /
        u_bar
    } else {
      NA_real_
    }

  # Rubin degrees of freedom

  df <-
    if (
      is.finite(r) &&
      r > 0
    ) {
      (m - 1) *
        (1 + 1 / r)^2
    } else {
      Inf
    }

  critical <-
    if (is.finite(df)) {
      qt(
        0.975,
        df = df
      )
    } else {
      qnorm(0.975)
    }

  fmi <-
    if (total_var > 0) {
      ((1 + 1 / m) * b) /
        total_var
    } else {
      NA_real_
    }

  tibble(
    m = m,
    estimate = q_bar,
    within_model_variance = u_bar,
    between_imputation_variance = b,
    total_variance = total_var,
    std_error = pooled_se,
    df = df,
    fmi = fmi,
    conf_low =
      q_bar -
      critical * pooled_se,
    conf_high =
      q_bar +
      critical * pooled_se
  )
}


pooled_results <- draw_results |>
  group_by(
    model,
    term
  ) |>
  group_modify(
    ~ pool_rubin(.x)
  ) |>
  ungroup()


# 13. Focus on inequality coefficients -----------------------------------

pooled_gini <- pooled_results |>
  filter(
    term %in% c(
      "gini_within",
      "gini_between_c"
    )
  )

cat(
  "\n=================================\n",
  "Pooled Gini results\n",
  "=================================\n"
)

print(
  pooled_gini,
  n = Inf
)


# 14. Inspect between-imputation variation -------------------------------

gini_draws <- draw_results |>
  filter(
    term %in% c(
      "gini_within",
      "gini_between_c"
    )
  )

gini_draw_summary <- gini_draws |>
  group_by(
    model,
    term
  ) |>
  summarise(
    mean =
      mean(estimate),

    sd_across_imputations =
      sd(estimate),

    minimum =
      min(estimate),

    maximum =
      max(estimate),

    .groups = "drop"
  )

cat(
  "\nGini coefficient variation across imputations:\n"
)

print(
  gini_draw_summary
)


# 15. Save outputs --------------------------------------------------------

dir.create(
  here(
    "05_output",
    "tables"
  ),
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  draw_results,
  here(
    "05_output",
    "tables",
    "swiid_uncertainty_draw_results.csv"
  )
)

write_csv(
  pooled_results,
  here(
    "05_output",
    "tables",
    "swiid_uncertainty_pooled_results.csv"
  )
)

write_csv(
  pooled_gini,
  here(
    "05_output",
    "tables",
    "swiid_uncertainty_pooled_gini.csv"
  )
)

write_csv(
  gini_draw_summary,
  here(
    "05_output",
    "tables",
    "swiid_uncertainty_gini_draw_summary.csv"
  )
)

