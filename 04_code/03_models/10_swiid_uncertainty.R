# ------------------------------------------------------------
# SWIID uncertainty propagation: R1-R9 REWB models
# Project: Inequality and social cohesion
#
# Purpose:
#   Propagate uncertainty in SWIID disposable-income inequality
#   through the core R1-R9 REWB models.
#
# Strategy:
#
#   1. Load the 100 SWIID 9.92 imputations.
#   2. For each imputation:
#        a. construct the one-year-lagged country-round Gini
#           using actual ESS interview-year shares;
#        b. recalculate the within-between Gini decomposition;
#        c. fit M3 and M4 using exactly the same specification
#           as the current extended REWB analysis.
#   3. Pool fixed-effect estimates using Rubin's rules.
#
# IMPORTANT:
#   The initial validation run uses only the first 3 imputations.
#   Once the pipeline has been checked, change:
#
#       n_imputations_to_run <- 3L
#
#   to:
#
#       n_imputations_to_run <- 100L
#
# No survey weights are applied, matching the current primary
# REWB models.
# ------------------------------------------------------------


library(dplyr)
library(here)
library(lme4)
library(readr)
library(tibble)


# 1. Settings --------------------------------------------------------

# VALIDATION RUN:
# Keep this at 3 until the model pipeline has been checked.

n_imputations_to_run <- 3L


# 2. Load ESS R1-R9 data --------------------------------------------

ess <- readRDS(
  here(
    "03_data",
    "processed",
    "ess_analysis_r1_r11.rds"
  )
) |>
  filter(
    essround <= 9
  )


cat(
  "\nESS R1-R9 respondents:",
  nrow(ess),
  "\n"
)


# Remove the summary-series SWIID value.
#
# Each imputation will supply its own country-round inequality
# exposure instead.

ess <- ess |>
  select(
    -any_of(
      c(
        "gini_swiid_lag1",
        "gini_within",
        "gini_between",
        "gini_between_c",
        "gini_within_macro",
        "gini_between_macro",
        "gini_between_macro_c"
      )
    )
  )


# 3. Load macro controls --------------------------------------------

macro <- read_csv(
  here(
    "03_data",
    "interim",
    "ess_country_round_macro_r1_r11.csv"
  ),
  show_col_types = FALSE
) |>
  filter(
    essround <= 9
  ) |>
  select(
    cntry,
    essround,
    log_gdp_pc_ppp_lag1,
    unemployment_lag1
  )


macro_duplicates <- macro |>
  count(
    cntry,
    essround
  ) |>
  filter(
    n > 1
  )


if (
  nrow(
    macro_duplicates
  ) > 0
) {

  print(
    macro_duplicates,
    n = Inf
  )

  stop(
    "Duplicate country-rounds found in macro data."
  )
}


ess <- ess |>
  left_join(
    macro,
    by = c(
      "cntry",
      "essround"
    )
  )


# 4. Load ESS interview timing --------------------------------------
#
# This is the same R1-R9 timing structure used to construct the
# existing summary-series SWIID exposure.

timing <- read_csv(
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
  filter(
    essround <= 9,
    !is.na(
      interview_year
    )
  )


timing_duplicates <- timing |>
  count(
    cntry,
    essround,
    interview_year
  ) |>
  filter(
    n > 1
  )


if (
  nrow(
    timing_duplicates
  ) > 0
) {

  print(
    timing_duplicates,
    n = Inf
  )

  stop(
    "Duplicate country-round-year rows found in ESS timing data."
  )
}


# Calculate interview-year shares within country-round.
#
# For country-rounds spanning two years, these shares determine
# the contribution of each annual SWIID estimate.

timing <- timing |>
  group_by(
    cntry,
    essround
  ) |>
  mutate(
    interview_year_share =
      n_respondents /
      sum(
        n_respondents
      ),

    # Primary contextual specification:
    # inequality one year before the interview year.
    exposure_year =
      as.integer(
        interview_year
      ) -
      1L
  ) |>
  ungroup()


share_check <- timing |>
  group_by(
    cntry,
    essround
  ) |>
  summarise(
    share_sum =
      sum(
        interview_year_share
      ),

    .groups = "drop"
  )


stopifnot(
  all(
    abs(
      share_check$share_sum -
        1
    ) <
      1e-10
  )
)


cat(
  "\nCountry-rounds with usable interview timing:",
  n_distinct(
    interaction(
      timing$cntry,
      timing$essround
    )
  ),
  "\n"
)


# 5. Load SWIID 9.92 imputations ------------------------------------

swiid_file <- here(
  "03_data",
  "raw",
  "inequality",
  "swiid",
  "swiid9_92.rda"
)


if (
  !file.exists(
    swiid_file
  )
) {

  stop(
    "SWIID file not found: ",
    swiid_file
  )
}


swiid_env <- new.env()


load(
  swiid_file,
  envir = swiid_env
)


if (
  !exists(
    "swiid",
    envir = swiid_env
  )
) {

  stop(
    "Object `swiid` not found in swiid9_92.rda."
  )
}


swiid <- swiid_env$swiid


if (
  !is.list(
    swiid
  )
) {

  stop(
    "SWIID object is not a list."
  )
}


if (
  length(
    swiid
  ) != 100
) {

  stop(
    "Expected 100 SWIID imputations but found ",
    length(swiid),
    "."
  )
}


if (
  n_imputations_to_run >
    length(
      swiid
    )
) {

  stop(
    "Requested more imputations than are available."
  )
}


cat(
  "\nSWIID imputations available:",
  length(swiid),
  "\n"
)


cat(
  "SWIID imputations selected for this run:",
  n_imputations_to_run,
  "\n"
)


# 6. Country correspondence -----------------------------------------
#
# SWIID uses country names whereas ESS uses two-letter country codes.
#
# Alternative names are included where different source releases may
# use different labels. Only countries appearing in the ESS R1-R9
# dataset will ultimately be retained.

country_lookup <- tribble(
  ~country,              ~cntry,
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
  "Russia",               "RU",
  "Russian Federation",  "RU",
  "Serbia",               "RS",
  "Slovakia",             "SK",
  "Slovenia",             "SI",
  "Spain",                "ES",
  "Sweden",               "SE",
  "Switzerland",          "CH",
  "Turkey",               "TR",
  "Türkiye",              "TR",
  "Ukraine",              "UA",
  "United Kingdom",       "GB"
)


# Check that every ESS R1-R9 country has a possible lookup entry.

ess_countries <- ess |>
  distinct(
    cntry
  )


lookup_missing <- ess_countries |>
  anti_join(
    country_lookup |>
      distinct(
        cntry
      ),
    by = "cntry"
  )


if (
  nrow(
    lookup_missing
  ) > 0
) {

  print(
    lookup_missing,
    n = Inf
  )

  stop(
    "Some ESS countries are missing from country_lookup."
  )
}


# 7. Check actual SWIID country matching -----------------------------
#
# Use the first imputation to establish the set of country names
# actually present in SWIID 9.92.

swiid_country_names <- tibble(
  country =
    unique(
      as.character(
        swiid[[1]]$country
      )
    )
)


country_lookup_active <- country_lookup |>
  semi_join(
    swiid_country_names,
    by = "country"
  )


matched_ess_countries <- country_lookup_active |>
  distinct(
    cntry
  ) |>
  semi_join(
    ess_countries,
    by = "cntry"
  )


unmatched_ess_countries <- ess_countries |>
  anti_join(
    matched_ess_countries,
    by = "cntry"
  )


if (
  nrow(
    unmatched_ess_countries
  ) > 0
) {

  cat(
    "\nESS countries without an observed SWIID country-name match:\n"
  )

  print(
    unmatched_ess_countries,
    n = Inf
  )

  stop(
    "Resolve SWIID country correspondence before modelling."
  )
}


cat(
  "\nAll ESS R1-R9 countries have an observed SWIID country match.\n"
)


# 8. Model helpers ---------------------------------------------------

ctrl <- lmerControl(
  optimizer = "bobyqa",

  optCtrl = list(
    maxfun = 200000
  )
)


# Extract fixed effects and their model-based sampling variances.

extract_fixed <- function(
  fit,
  imputation,
  model_name
) {

  x <- as.data.frame(
    coef(
      summary(
        fit
      )
    )
  )


  x$term <- rownames(
    x
  )


  rownames(
    x
  ) <- NULL


  x |>
    transmute(
      imputation =
        imputation,

      model =
        model_name,

      term,

      estimate =
        Estimate,

      std_error =
        `Std. Error`,

      within_variance =
        std_error^2
    )
}


# Extract convergence information without storing the large fitted
# model objects for all imputations.

extract_diagnostics <- function(
  fit,
  imputation,
  model_name
) {

  convergence_messages <-
    fit@optinfo$conv$lme4$messages


  convergence_text <-
    if (
      is.null(
        convergence_messages
      )
    ) {

      NA_character_

    } else {

      paste(
        convergence_messages,
        collapse = " | "
      )
    }


  tibble(
    imputation =
      imputation,

    model =
      model_name,

    n =
      nobs(
        fit
      ),

    singular =
      isSingular(
        fit,
        tol = 1e-4
      ),

    convergence_message =
      convergence_text
  )
}


# 9. Construct one SWIID country-round exposure ---------------------
#
# IMPORTANT:
# All interview years within a country-round use values from the
# SAME SWIID imputation. We do not mix draw numbers across years.

construct_gini_exposure <- function(
  imputation_number
) {

  draw <- swiid[[
    imputation_number
  ]] |>
    transmute(
      country =
        as.character(
          country
        ),

      year =
        as.integer(
          year
        ),

      # SWIID R-format simulations store gini_disp in hundredths.
      # Convert to the conventional 0-100 Gini scale.
      gini_disp =
        as.numeric(
          gini_disp
        ) /
        100
    ) |>
    inner_join(
      country_lookup_active,
      by = "country"
    ) |>
    select(
      cntry,
      year,
      gini_disp
    )


  # Check that alternative country names have not produced duplicate
  # country-year observations.

  draw_duplicates <- draw |>
    count(
      cntry,
      year
    ) |>
    filter(
      n > 1
    )


  if (
    nrow(
      draw_duplicates
    ) > 0
  ) {

    print(
      draw_duplicates,
      n = Inf
    )

    stop(
      "Duplicate country-year values after SWIID country mapping."
    )
  }


  exposure_components <- timing |>
    left_join(
      draw,
      by = c(
        "cntry",
        "exposure_year" = "year"
      )
    )


  # Do not renormalise over available annual values.
  #
  # A country-round receives an exposure only when every interview-year
  # component required by its observed fieldwork timing is available.

  exposure <- exposure_components |>
    group_by(
      cntry,
      essround
    ) |>
    summarise(
      complete_swiid_draw =
        all(
          !is.na(
            gini_disp
          )
        ),

      gini_swiid_lag1 =
        if (
          all(
            !is.na(
              gini_disp
            )
          )
        ) {

          weighted.mean(
            gini_disp,
            interview_year_share
          )

        } else {

          NA_real_
        },

      .groups = "drop"
    )


  exposure
}


# 10. Prepare M3 and M4 data for one imputation ---------------------

prepare_imputation <- function(
  imputation_number
) {

  gini_exposure <-
    construct_gini_exposure(
      imputation_number
    )


  dat <- ess |>
    left_join(
      gini_exposure |>
        select(
          cntry,
          essround,
          gini_swiid_lag1
        ),
      by = c(
        "cntry",
        "essround"
      )
    )


  # ----------------------------------------------------------
  # M3 inequality decomposition
  # ----------------------------------------------------------
  #
  # Calculate country means from unique country-round
  # observations, not respondent-level rows.

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
    pull(
      value
    )


  country_round_gini <- country_round_gini |>
    mutate(
      gini_between_c =
        gini_between -
        gini_grand_mean
    )


  dat <- dat |>
    left_join(
      country_round_gini |>
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


  # Main M3 analytical sample.

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

      age_decade =
        (
          age -
          mean(
            age
          )
        ) /
        10,

      age_decade2 =
        age_decade^2,

      education_c =
        education_years -
        mean(
          education_years
        )
    )


  # ----------------------------------------------------------
  # M4 contextual decomposition
  # ----------------------------------------------------------
  #
  # Define the macro-complete country-round sample BEFORE
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


  # Grand-mean centre the between-country contextual components.

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

      # Match the current extended REWB scale:
      # coefficient per 0.1 log-point difference.
      log_gdp_within_10 =
        log_gdp_within *
        10,

      log_gdp_between_10 =
        log_gdp_between_c *
        10
    )


  dat_macro <- dat |>
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


  macro_dat <- dat_macro |>
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

      # Re-centre individual covariates on the actual M4 sample.
      age_decade =
        (
          age -
          mean(
            age
          )
        ) /
        10,

      age_decade2 =
        age_decade^2,

      education_c =
        education_years -
        mean(
          education_years
        )
    )


  sample_summary <- tibble(
    imputation =
      imputation_number,

    model =
      c(
        "M3",
        "M4"
      ),

    n_respondents =
      c(
        nrow(
          model_dat
        ),
        nrow(
          macro_dat
        )
      ),

    n_countries =
      c(
        n_distinct(
          model_dat$cntry
        ),
        n_distinct(
          macro_dat$cntry
        )
      ),

    n_country_rounds =
      c(
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


# 11. Fit M3 and M4 for one imputation --------------------------------

fit_imputation <- function(
  imputation_number
) {

  prepared <-
    prepare_imputation(
      imputation_number
    )


  model_dat <-
    prepared$model_dat


  macro_dat <-
    prepared$macro_dat


  # M3: inequality + round + individual controls

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


  # M4: M3 + macroeconomic controls

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
      m3,
      imputation_number,
      "M3"
    ),

    extract_fixed(
      m4,
      imputation_number,
      "M4"
    )
  )


  diagnostics <- bind_rows(
    extract_diagnostics(
      m3,
      imputation_number,
      "M3"
    ),

    extract_diagnostics(
      m4,
      imputation_number,
      "M4"
    )
  )


  list(
    fixed_effects =
      fixed_effects,

    diagnostics =
      diagnostics,

    sample_summary =
      prepared$sample_summary
  )
}


# 12. Run selected imputations --------------------------------------

all_fixed <- list()

all_diagnostics <- list()

all_samples <- list()


for (
  i in seq_len(
    n_imputations_to_run
  )
) {

  cat(
    "\n--------------------------------------------------\n"
  )

  cat(
    "Fitting SWIID imputation",
    i,
    "of",
    n_imputations_to_run,
    "\n"
  )


  result <- fit_imputation(
    i
  )


  all_fixed[[i]] <-
    result$fixed_effects


  all_diagnostics[[i]] <-
    result$diagnostics


  all_samples[[i]] <-
    result$sample_summary


  rm(
    result
  )

  invisible(
    gc()
  )
}


fixed_by_imputation <- bind_rows(
  all_fixed
)


model_diagnostics <- bind_rows(
  all_diagnostics
)


sample_by_imputation <- bind_rows(
  all_samples
)


cat(
  "\nAll selected imputations fitted.\n"
)


# 13. Verify analytical samples are stable ---------------------------
#
# The SWIID imputations represent uncertainty in the values of Gini,
# not uncertainty about which ESS observations belong in the model.
#
# We therefore expect identical respondent, country, and
# country-round counts across imputations.

sample_stability <- sample_by_imputation |>
  group_by(
    model
  ) |>
  summarise(
    n_imputations =
      n(),

    distinct_n_respondents =
      n_distinct(
        n_respondents
      ),

    min_n_respondents =
      min(
        n_respondents
      ),

    max_n_respondents =
      max(
        n_respondents
      ),

    distinct_n_countries =
      n_distinct(
        n_countries
      ),

    min_n_countries =
      min(
        n_countries
      ),

    max_n_countries =
      max(
        n_countries
      ),

    distinct_n_country_rounds =
      n_distinct(
        n_country_rounds
      ),

    min_n_country_rounds =
      min(
        n_country_rounds
      ),

    max_n_country_rounds =
      max(
        n_country_rounds
      ),

    .groups = "drop"
  )


cat(
  "\nSample stability across imputations:\n"
)


print(
  sample_stability,
  n = Inf,
  width = Inf
)


unstable_samples <- sample_stability |>
  filter(
    distinct_n_respondents != 1 |
      distinct_n_countries != 1 |
      distinct_n_country_rounds != 1
  )


if (
  nrow(
    unstable_samples
  ) > 0
) {

  stop(
    "Analytical sample changes across SWIID imputations."
  )
}


# 14. Check model diagnostics ----------------------------------------

cat(
  "\nModel diagnostics:\n"
)


print(
  model_diagnostics,
  n = Inf,
  width = Inf
)


problem_models <- model_diagnostics |>
  filter(
    singular |
      !is.na(
        convergence_message
      )
  )


if (
  nrow(
    problem_models
  ) > 0
) {

  cat(
    "\nModels requiring diagnostic inspection:\n"
  )

  print(
    problem_models,
    n = Inf,
    width = Inf
  )
}


# 15. Rubin pooling --------------------------------------------------
#
# For each coefficient:
#
#   Qbar = average coefficient across imputations
#   Ubar = average model-based sampling variance
#   B    = between-imputation variance
#
# Total variance:
#
#   T = Ubar + (1 + 1/m) * B
#
# The resulting uncertainty incorporates both ordinary sampling
# uncertainty and uncertainty in the SWIID inequality estimates.

pool_rubin <- function(
  estimates
) {

  estimates |>
    group_by(
      model,
      term
    ) |>
    summarise(
      m =
        n(),

      estimate =
        mean(
          estimate
        ),

      within_variance =
        mean(
          within_variance
        ),

      between_variance =
        var(
          estimate
        ),

      .groups = "drop"
    ) |>
    mutate(
      total_variance =
        within_variance +
        (
          1 +
            1 / m
        ) *
        between_variance,

      std_error =
        sqrt(
          total_variance
        ),

      relative_increase_variance =
        case_when(
          within_variance > 0 ~
            (
              1 +
                1 / m
            ) *
            between_variance /
            within_variance,

          TRUE ~
            NA_real_
        ),

      rubin_df =
        case_when(
          is.na(
            between_variance
          ) ~
            NA_real_,

          between_variance == 0 ~
            Inf,

          relative_increase_variance > 0 ~
            (
              m -
                1
            ) *
            (
              1 +
                1 /
                relative_increase_variance
            )^2,

          TRUE ~
            Inf
        ),

      fraction_missing_information =
        case_when(
          total_variance > 0 ~
            (
              1 +
                1 / m
            ) *
            between_variance /
            total_variance,

          TRUE ~
            NA_real_
        ),

      statistic =
        estimate /
        std_error,

      critical_value =
        case_when(
          is.finite(
            rubin_df
          ) ~
            qt(
              0.975,
              df = rubin_df
            ),

          TRUE ~
            qnorm(
              0.975
            )
        ),

      conf_low =
        estimate -
        critical_value *
        std_error,

      conf_high =
        estimate +
        critical_value *
        std_error,

      p_value =
        case_when(
          is.finite(
            rubin_df
          ) ~
            2 *
            pt(
              abs(
                statistic
              ),
              df = rubin_df,
              lower.tail = FALSE
            ),

          TRUE ~
            2 *
            pnorm(
              abs(
                statistic
              ),
              lower.tail = FALSE
            )
        )
    ) |>
    select(
      model,
      term,
      m,
      estimate,
      std_error,
      conf_low,
      conf_high,
      statistic,
      p_value,
      within_variance,
      between_variance,
      total_variance,
      relative_increase_variance,
      fraction_missing_information,
      rubin_df
    )
}


pooled_results <- pool_rubin(
  fixed_by_imputation
)


# Focused inequality results.

pooled_gini <- pooled_results |>
  filter(
    term %in%
      c(
        "gini_within",
        "gini_between_c",
        "gini_within_macro",
        "gini_between_macro_c"
      )
  ) |>
  mutate(
    effect =
      case_when(
        term %in%
          c(
            "gini_within",
            "gini_within_macro"
          ) ~
          "Within-country Gini",

        term %in%
          c(
            "gini_between_c",
            "gini_between_macro_c"
          ) ~
          "Between-country Gini"
      )
  ) |>
  select(
    model,
    effect,
    term,
    everything()
  )


cat(
  "\nPooled Gini results:\n"
)


print(
  pooled_gini,
  n = Inf,
  width = Inf
)


# 16. Variation of Gini estimates across imputations -----------------
#
# Useful diagnostic before interpreting the pooled results.

gini_imputation_summary <- fixed_by_imputation |>
  filter(
    term %in%
      c(
        "gini_within",
        "gini_between_c",
        "gini_within_macro",
        "gini_between_macro_c"
      )
  ) |>
  group_by(
    model,
    term
  ) |>
  summarise(
    n_imputations =
      n(),

    min_estimate =
      min(
        estimate
      ),

    mean_estimate =
      mean(
        estimate
      ),

    median_estimate =
      median(
        estimate
      ),

    max_estimate =
      max(
        estimate
      ),

    sd_estimate =
      sd(
        estimate
      ),

    mean_model_se =
      mean(
        std_error
      ),

    .groups = "drop"
  )


cat(
  "\nVariation in Gini coefficients across imputations:\n"
)


print(
  gini_imputation_summary,
  n = Inf,
  width = Inf
)


# 17. Compare with summary-series point estimates --------------------
#
# If the current extended REWB result file exists, compare the
# uncertainty-pooled coefficients against the existing point-estimate
# analysis based on swiid_summary.

point_result_file <- here(
  "05_output",
  "tables",
  "extended_rewb_gini_results.csv"
)


if (
  file.exists(
    point_result_file
  )
) {

  point_results <- read_csv(
    point_result_file,
    show_col_types = FALSE
  ) |>
    filter(
      sample == "R1-R9",
      model %in%
        c(
          "M3",
          "M4"
        )
    ) |>
    select(
      model,
      effect,
      point_estimate =
        estimate,

      point_std_error =
        std_error
    )


  pooled_comparison <- pooled_gini |>
    select(
      model,
      effect,
      pooled_estimate =
        estimate,

      pooled_std_error =
        std_error,

      pooled_conf_low =
        conf_low,

      pooled_conf_high =
        conf_high,

      fraction_missing_information
    ) |>
    left_join(
      point_results,
      by = c(
        "model",
        "effect"
      )
    ) |>
    mutate(
      estimate_difference =
        pooled_estimate -
        point_estimate,

      se_ratio =
        pooled_std_error /
        point_std_error
    )


  cat(
    "\nSummary-series versus SWIID-pooled Gini results:\n"
  )


  print(
    pooled_comparison,
    n = Inf,
    width = Inf
  )

} else {

  pooled_comparison <- tibble()


  cat(
    "\nExisting point-estimate result file not found; ",
    "comparison skipped.\n"
  )
}


# 18. Save outputs ---------------------------------------------------

table_dir <- here(
  "05_output",
  "tables",
  "swiid_uncertainty"
)


dir.create(
  table_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


write_csv(
  fixed_by_imputation,
  file.path(
    table_dir,
    "swiid_fixed_effects_by_imputation.csv"
  )
)


write_csv(
  model_diagnostics,
  file.path(
    table_dir,
    "swiid_model_diagnostics.csv"
  )
)


write_csv(
  sample_by_imputation,
  file.path(
    table_dir,
    "swiid_sample_by_imputation.csv"
  )
)


write_csv(
  sample_stability,
  file.path(
    table_dir,
    "swiid_sample_stability.csv"
  )
)


write_csv(
  pooled_results,
  file.path(
    table_dir,
    "swiid_pooled_fixed_effects.csv"
  )
)


write_csv(
  pooled_gini,
  file.path(
    table_dir,
    "swiid_pooled_gini_results.csv"
  )
)


write_csv(
  gini_imputation_summary,
  file.path(
    table_dir,
    "swiid_gini_imputation_summary.csv"
  )
)


if (
  nrow(
    pooled_comparison
  ) > 0
) {

  write_csv(
    pooled_comparison,
    file.path(
      table_dir,
      "swiid_point_vs_pooled_gini.csv"
    )
  )
}


cat(
  "\nSWIID uncertainty outputs saved in:\n",
  table_dir,
  "\n"
)


if (
  n_imputations_to_run <
    100
) {

  cat(
    "\nVALIDATION RUN ONLY.\n",
    "If the sample and model diagnostics are clean, ",
    "change n_imputations_to_run to 100 before treating ",
    "the pooled results as final.\n"
  )
}

