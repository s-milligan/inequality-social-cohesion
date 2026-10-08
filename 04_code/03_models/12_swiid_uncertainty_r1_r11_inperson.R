# ------------------------------------------------------------
# SWIID uncertainty propagation: R1-R11 in-person REWB models
# Project: Inequality and social cohesion
#
# Purpose:
#   Propagate uncertainty in SWIID disposable-income inequality
#   through the core ESS Rounds 1-11 REWB models after restricting
#   the respondent-level sample to in-person interviews.
#
# Strategy:
#   1. Load the SWIID 9.92 imputations.
#   2. Reconstruct one-year-lagged country-round Gini exposure
#      for R1-R11 using actual ESS interview-year shares.
#   3. Recalculate within-between Gini decompositions separately
#      within each selected SWIID imputation.
#   4. Fit M3 and M4 using the same specification as the current
#      extended REWB analysis.
#   5. Pool fixed-effect estimates using Rubin's rules.
#   6. Fit the corresponding summary-series in-person M3 and M4
#      models once and compare them with the validation results.
#
# IMPORTANT:
#   This script is designed as a 3-imputation validation of the
#   R1-R11 in-person sensitivity, not as a second full 100-draw
#   uncertainty analysis. The full 100-imputation uncertainty
#   propagation has already been completed for the all-mode R1-R11
#   sample. Rubin-pooled inferential quantities from m = 3 are
#   therefore diagnostic only.
#
# No survey weights are applied, matching the current primary
# REWB models.
# ------------------------------------------------------------


library(dplyr)
library(here)
library(lme4)
library(readr)
library(tibble)


# 1. Settings -------------------------------------------------------------

n_imputations_to_run <- 3L

sample_id <- "R1-R11 in-person"

table_dir <- here(
  "05_output",
  "tables",
  "swiid_uncertainty_r1_r11_inperson"
)


# 2. Load ESS R1-R11 data -------------------------------------------------

ess <- readRDS(
  here(
    "03_data",
    "processed",
    "ess_analysis_r1_r11.rds"
  )
)


cat(
  "\nESS R1-R11 respondents:",
  nrow(ess),
  "\n"
)

cat(
  "ESS rounds:",
  min(ess$essround, na.rm = TRUE),
  "to",
  max(ess$essround, na.rm = TRUE),
  "\n"
)


# Restrict only the respondent-level sample to in-person interviews.
# Contextual interview-year shares remain the all-mode country-round
# shares used in the main R1-R11 analysis.

mode_counts <- ess |>
  count(
    essround,
    interview_mode,
    name = "n_respondents"
  ) |>
  arrange(
    essround,
    interview_mode
  )

cat(
  "\nR1-R11 interview-mode counts:\n"
)

print(
  mode_counts,
  n = Inf
)

ess <- ess |>
  filter(
    interview_mode ==
      "In-person"
  )

cat(
  "\nR1-R11 in-person respondents before model complete-case restrictions:",
  nrow(ess),
  "\n"
)


# Remove summary-series SWIID variables that will be reconstructed
# separately within each imputation.

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


# 3. Load R1-R11 macro controls -------------------------------------------

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
    "Duplicate country-rounds found in R1-R11 macro data."
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


# 4. Build the complete R1-R11 interview-timing structure ----------------

# IMPORTANT: timing shares are intentionally based on all respondents,
# not recomputed within the in-person subset. This keeps contextual
# exposure identical to the main R1-R11 analysis and isolates mode.

# R1-R9 timing comes from the established inequality-coverage file.

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
  filter(
    essround <= 9,
    !is.na(
      interview_year
    )
  )


# R10-R11 timing comes from the post-R9 coverage construction.
# Respondents without a usable interview year do not contribute to
# interview-year shares, matching 07_construct_post9_context.R.

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
    essround <= 11,
    !is.na(
      interview_year
    )
  )


timing <- bind_rows(
  timing_r1_r9,
  timing_post9
) |>
  arrange(
    essround,
    cntry,
    interview_year
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
    "Duplicate country-round-year rows found in combined ESS timing data."
  )
}


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
  "\nR1-R11 country-rounds with usable interview timing:",
  nrow(
    timing |>
      distinct(
        cntry,
        essround
      )
  ),
  "\n"
)


# Identify ESS country-rounds for which no usable timing row exists.
# These are retained as a diagnostic and will receive missing Gini.

ess_country_rounds <- ess |>
  distinct(
    cntry,
    essround
  )


timing_missing_country_rounds <- ess_country_rounds |>
  anti_join(
    timing |>
      distinct(
        cntry,
        essround
      ),
    by = c(
      "cntry",
      "essround"
    )
  ) |>
  arrange(
    essround,
    cntry
  )


if (
  nrow(
    timing_missing_country_rounds
  ) > 0
) {

  cat(
    "\nESS country-rounds without usable interview timing:\n"
  )

  print(
    timing_missing_country_rounds,
    n = Inf
  )
}


# 5. Load SWIID 9.92 imputations ------------------------------------------

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


# 6. ESS -> SWIID country correspondence ---------------------------------

# Reuse the established project crosswalk and add North Macedonia,
# which first enters the relevant workflow after Round 9.

country_lookup <- read_csv(
  here(
    "03_data",
    "interim",
    "ess_inequality_coverage.csv"
  ),
  show_col_types = FALSE
) |>
  select(
    cntry,
    swiid_country
  ) |>
  filter(
    !is.na(
      swiid_country
    )
  ) |>
  distinct() |>
  bind_rows(
    tibble(
      cntry = "MK",
      swiid_country = "North Macedonia"
    )
  ) |>
  distinct()


duplicate_lookup <- country_lookup |>
  count(
    cntry
  ) |>
  filter(
    n > 1
  )


if (
  nrow(
    duplicate_lookup
  ) > 0
) {

  print(
    duplicate_lookup,
    n = Inf
  )

  stop(
    "Some ESS countries have multiple SWIID mappings."
  )
}


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
    "Some R1-R11 ESS countries are missing from country_lookup."
  )
}


# Verify that the mapped country names are actually present in the
# released SWIID imputations.

swiid_country_names <- tibble(
  swiid_country =
    unique(
      as.character(
        swiid[[1]]$country
      )
    )
)


country_lookup_active <- country_lookup |>
  semi_join(
    swiid_country_names,
    by = "swiid_country"
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
  "\nAll R1-R11 ESS countries have an observed SWIID country match.\n"
)


# 7. Model helpers --------------------------------------------------------

ctrl <- lmerControl(
  optimizer = "bobyqa",

  optCtrl = list(
    maxfun = 200000
  )
)


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


# 8. Construct one SWIID country-round exposure ---------------------------

construct_gini_exposure <- function(
  imputation_number
) {

  draw <- swiid[[
    imputation_number
  ]] |>
    transmute(
      swiid_country =
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
      by = "swiid_country"
    ) |>
    select(
      cntry,
      year,
      gini_disp
    )


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
  # Every annual component required by the observed interview-year
  # distribution must be available.

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


# 9. Prepare M3 and M4 data for one imputation ----------------------------

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
  # A. M3 Gini within-between decomposition
  # ----------------------------------------------------------

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
  # B. M4 macro-complete contextual decomposition
  # ----------------------------------------------------------

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
      gini_between_macro =
        mean(
          gini_swiid_lag1
        ),

      gini_within_macro =
        gini_swiid_lag1 -
        gini_between_macro,

      log_gdp_between =
        mean(
          log_gdp_pc_ppp_lag1
        ),

      log_gdp_within =
        log_gdp_pc_ppp_lag1 -
        log_gdp_between,

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


# 10. Fit M3 and M4 for one imputation ------------------------------------

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


# 11. Run selected imputations --------------------------------------------

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
    "Fitting R1-R11 in-person SWIID imputation",
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
  "\nAll selected R1-R11 imputations fitted.\n"
)


# 12. Verify analytical-sample stability ---------------------------------

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
  "\nR1-R11 in-person sample stability across imputations:\n"
)


print(
  sample_stability,
  n = Inf,
  width = Inf
)


if (
  any(
    sample_stability$distinct_n_respondents != 1 |
      sample_stability$distinct_n_countries != 1 |
      sample_stability$distinct_n_country_rounds != 1
  )
) {

  stop(
    "R1-R11 in-person analytical sample varies across SWIID imputations."
  )
}


# 13. Model diagnostics ---------------------------------------------------


problem_models <- model_diagnostics |>
  filter(
    singular |
      !is.na(
        convergence_message
      )
  )


cat(
  "\nR1-R11 in-person problem models:\n"
)


print(
  problem_models,
  n = Inf,
  width = Inf
)


# 14. Rubin pooling -------------------------------------------------------

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

      pooled_estimate =
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
        pooled_estimate /
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
        pooled_estimate -
        critical_value *
        std_error,

      conf_high =
        pooled_estimate +
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
    transmute(
      model,
      term,
      m,
      estimate =
        pooled_estimate,
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
  "\nR1-R11 in-person pooled Gini results:\n"
)


print(
  pooled_gini,
  n = Inf,
  width = Inf
)


# 15. Variation of Gini estimates across imputations ----------------------

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
  "\nR1-R11 in-person variation in Gini coefficients across imputations:\n"
)


print(
  gini_imputation_summary,
  n = Inf,
  width = Inf
)


# 16. Summary-series in-person benchmark ----------------------------------

# Fit the corresponding in-person models once using the SWIID summary
# series stored in ess_analysis_r1_r11.rds. These models provide the
# direct benchmark for the three-imputation validation above.

ess_point <- readRDS(
  here(
    "03_data",
    "processed",
    "ess_analysis_r1_r11.rds"
  )
) |>
  filter(
    interview_mode ==
      "In-person"
  ) |>
  left_join(
    macro,
    by = c(
      "cntry",
      "essround"
    )
  )


# M3 Gini decomposition ---------------------------------------------------

point_gini_context <- ess_point |>
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


point_gini_mean <- point_gini_context |>
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


point_gini_context <- point_gini_context |>
  mutate(
    gini_between_c =
      gini_between -
      point_gini_mean
  )


ess_point <- ess_point |>
  left_join(
    point_gini_context |>
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


point_m3_dat <- ess_point |>
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


# M4 macro-complete contextual decomposition -----------------------------

point_macro_context <- ess_point |>
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
    gini_between_macro =
      mean(
        gini_swiid_lag1
      ),

    gini_within_macro =
      gini_swiid_lag1 -
      gini_between_macro,

    log_gdp_between =
      mean(
        log_gdp_pc_ppp_lag1
      ),

    log_gdp_within =
      log_gdp_pc_ppp_lag1 -
      log_gdp_between,

    unemployment_between =
      mean(
        unemployment_lag1
      ),

    unemployment_within =
      unemployment_lag1 -
      unemployment_between
  ) |>
  ungroup()


point_macro_means <- point_macro_context |>
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


point_macro_context <- point_macro_context |>
  mutate(
    gini_between_macro_c =
      gini_between_macro -
      point_macro_means$gini,

    log_gdp_between_c =
      log_gdp_between -
      point_macro_means$log_gdp,

    unemployment_between_c =
      unemployment_between -
      point_macro_means$unemployment,

    log_gdp_within_10 =
      log_gdp_within *
      10,

    log_gdp_between_10 =
      log_gdp_between_c *
      10
  )


point_m4_dat <- ess_point |>
  left_join(
    point_macro_context |>
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


point_sample_summary <- tibble(
  model =
    c(
      "M3",
      "M4"
    ),

  n_respondents =
    c(
      nrow(
        point_m3_dat
      ),
      nrow(
        point_m4_dat
      )
    ),

  n_countries =
    c(
      n_distinct(
        point_m3_dat$cntry
      ),
      n_distinct(
        point_m4_dat$cntry
      )
    ),

  n_country_rounds =
    c(
      n_distinct(
        point_m3_dat$country_round
      ),
      n_distinct(
        point_m4_dat$country_round
      )
    )
)


cat(
  "\nSummary-series in-person sample:\n"
)


print(
  point_sample_summary,
  n = Inf,
  width = Inf
)


# Verify that the summary-series benchmark uses the same analytical
# sample dimensions as every selected SWIID imputation.

point_sample_comparison <- sample_stability |>
  select(
    model,

    imputed_n_respondents =
      min_n_respondents,

    imputed_n_countries =
      min_n_countries,

    imputed_n_country_rounds =
      min_n_country_rounds
  ) |>
  left_join(
    point_sample_summary |>
      rename(
        point_n_respondents =
          n_respondents,

        point_n_countries =
          n_countries,

        point_n_country_rounds =
          n_country_rounds
      ),
    by = "model"
  ) |>
  mutate(
    respondents_match =
      imputed_n_respondents ==
      point_n_respondents,

    countries_match =
      imputed_n_countries ==
      point_n_countries,

    country_rounds_match =
      imputed_n_country_rounds ==
      point_n_country_rounds
  )


cat(
  "\nSummary-series versus imputation sample comparison:\n"
)


print(
  point_sample_comparison,
  n = Inf,
  width = Inf
)


if (
  any(
    !point_sample_comparison$respondents_match |
      !point_sample_comparison$countries_match |
      !point_sample_comparison$country_rounds_match
  )
) {

  stop(
    "Summary-series and imputation analytical samples do not match."
  )
}


# Fit the two summary-series benchmark models once ------------------------

m3_point <- lmer(
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
  data = point_m3_dat,
  REML = FALSE,
  control = ctrl
)


m4_point <- lmer(
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
  data = point_m4_dat,
  REML = FALSE,
  control = ctrl
)


extract_point_fixed <- function(
  fit,
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
      model =
        model_name,

      term,

      estimate =
        Estimate,

      std_error =
        `Std. Error`
    )
}


point_fixed <- bind_rows(
  extract_point_fixed(
    m3_point,
    "M3"
  ),

  extract_point_fixed(
    m4_point,
    "M4"
  )
)


point_gini <- point_fixed |>
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
    estimate,
    std_error
  )


cat(
  "\nSummary-series in-person Gini results:\n"
)


print(
  as_tibble(point_gini),
  n = Inf,
  width = Inf
)


point_vs_pooled <- pooled_gini |>
  select(
    model,
    effect,

    pooled_estimate =
      estimate,

    pooled_std_error =
      std_error
  ) |>
  left_join(
    point_gini |>
      select(
        model,
        effect,

        point_estimate =
          estimate,

        point_std_error =
          std_error
      ),
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
  "\nSummary-series versus three-imputation pooled Gini results:\n"
)


print(
  point_vs_pooled,
  n = Inf,
  width = Inf
)


# 17. Save outputs --------------------------------------------------------


dir.create(
  table_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


write_csv(
  mode_counts,
  file.path(
    table_dir,
    "swiid_inperson_mode_counts.csv"
  )
)


write_csv(
  timing_missing_country_rounds,
  file.path(
    table_dir,
    "swiid_inperson_missing_timing_country_rounds.csv"
  )
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


write_csv(
  point_sample_summary,
  file.path(
    table_dir,
    "swiid_inperson_point_sample_summary.csv"
  )
)


write_csv(
  point_sample_comparison,
  file.path(
    table_dir,
    "swiid_inperson_point_sample_comparison.csv"
  )
)


write_csv(
  point_gini,
  file.path(
    table_dir,
    "swiid_inperson_point_gini_results.csv"
  )
)


write_csv(
  point_vs_pooled,
  file.path(
    table_dir,
    "swiid_point_vs_pooled_gini.csv"
  )
)


cat(
  "\nR1-R11 in-person SWIID uncertainty outputs saved in:\n",
  table_dir,
  "\n"
)


if (
  n_imputations_to_run ==
    3L
) {

  cat(
    "\nTHREE-IMPUTATION VALIDATION COMPLETE.\n",
    "Treat the pooled inferential quantities as diagnostic only. ",
    "The purpose of this run is to verify sample stability, clean model ",
    "fitting, and agreement with the summary-series in-person benchmark.\n"
  )
}

