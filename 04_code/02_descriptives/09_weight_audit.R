# ------------------------------------------------------------
# Audit ESS survey weights
# Project: Inequality and social cohesion
#
# Purpose:
#   Verify the construction, availability, and distribution of
#   ESS survey weights across Rounds 1-11 before deciding how
#   weights should enter the multilevel REWB models.
#
# The audit checks:
#
#   1. whether the ESS-provided analysis weight (ANWEIGHT)
#      corresponds closely to PSPWGHT × PWEIGHT;
#   2. whether the processed analysis_weight corresponds to the
#      relevant raw ESS analysis weight;
#   3. whether weights are missing, zero, negative, or extreme;
#   4. how weight distributions vary across rounds and
#      country-rounds.
#
# Differences between ANWEIGHT and PSPWGHT × PWEIGHT are
# summarised rather than classified using an arbitrary numerical
# tolerance at this stage. Small differences may arise because
# distributed weight variables are stored or rounded separately.
#
# This script is diagnostic only. It does not modify the
# analytical datasets and does not fit weighted models.
# ------------------------------------------------------------


library(dplyr)
library(haven)
library(here)
library(readr)


# Raw ESS files -------------------------------------------------------
#
# Rounds 1-9 are stored in the combined ESS extract used for the
# original analysis. Rounds 10 and 11 are stored separately.

ess_files <- c(
  here(
    "03_data",
    "raw",
    "ess",
    paste0(
      "ESS1e06_7-ESS2e03_6-ESS3e03_7-ESS4e04_6-",
      "ESS5e03_6-ESS6e02_7-ESS7e02_3-ESS8e02_3-",
      "ESS9e03_3-subset.dta"
    )
  ),
  here(
    "03_data",
    "raw",
    "ess",
    "post9",
    "ess10_f2f.dta"
  ),
  here(
    "03_data",
    "raw",
    "ess",
    "post9",
    "ess11_f2f.dta"
  )
)


stopifnot(
  all(
    file.exists(
      ess_files
    )
  )
)


# Helper functions ---------------------------------------------------
#
# Convert labelled ESS variables to ordinary numeric values.
# If a variable is absent, return an NA vector of the correct length.

get_num <- function(
    dat,
    variable
) {
  
  if (
    variable %in%
    names(dat)
  ) {
    
    as.numeric(
      zap_labels(
        dat[[variable]]
      )
    )
    
  } else {
    
    rep(
      NA_real_,
      nrow(dat)
    )
  }
}


# Safe summary functions avoid Inf/-Inf when an entire group is NA.

safe_min <- function(x) {
  
  if (
    all(
      is.na(x)
    )
  ) {
    
    NA_real_
    
  } else {
    
    min(
      x,
      na.rm = TRUE
    )
  }
}


safe_max <- function(x) {
  
  if (
    all(
      is.na(x)
    )
  ) {
    
    NA_real_
    
  } else {
    
    max(
      x,
      na.rm = TRUE
    )
  }
}


safe_mean <- function(x) {
  
  if (
    all(
      is.na(x)
    )
  ) {
    
    NA_real_
    
  } else {
    
    mean(
      x,
      na.rm = TRUE
    )
  }
}


safe_median <- function(x) {
  
  if (
    all(
      is.na(x)
    )
  ) {
    
    NA_real_
    
  } else {
    
    median(
      x,
      na.rm = TRUE
    )
  }
}


safe_quantile <- function(
    x,
    probability
) {
  
  if (
    all(
      is.na(x)
    )
  ) {
    
    NA_real_
    
  } else {
    
    as.numeric(
      quantile(
        x,
        probability,
        na.rm = TRUE,
        names = FALSE
      )
    )
  }
}


# Read raw weight variables ------------------------------------------
#
# Only variables needed for this audit are imported.

read_weight_data <- function(
    file
) {
  
  dat <- read_dta(
    file,
    col_select = any_of(
      c(
        "cntry",
        "essround",
        "idno",
        "dweight",
        "pspwght",
        "pweight",
        "anweight",
        "psu",
        "stratum"
      )
    )
  )
  
  
  tibble(
    source_file =
      basename(
        file
      ),
    
    cntry =
      as.character(
        dat$cntry
      ),
    
    essround =
      as.integer(
        get_num(
          dat,
          "essround"
        )
      ),
    
    idno =
      get_num(
        dat,
        "idno"
      ),
    
    dweight =
      get_num(
        dat,
        "dweight"
      ),
    
    pspwght =
      get_num(
        dat,
        "pspwght"
      ),
    
    pweight =
      get_num(
        dat,
        "pweight"
      ),
    
    anweight =
      get_num(
        dat,
        "anweight"
      ),
    
    psu =
      get_num(
        dat,
        "psu"
      ),
    
    stratum =
      get_num(
        dat,
        "stratum"
      )
  )
}


raw_weights <- bind_rows(
  lapply(
    ess_files,
    read_weight_data
  )
)


cat(
  "\nRaw respondents in weight audit:",
  nrow(raw_weights),
  "\n"
)


# Check respondent keys ---------------------------------------------
#
# Respondent IDs should uniquely identify observations within
# country × round. This is important before later joining the raw
# weight information to the processed analysis dataset.

duplicate_weight_ids <- raw_weights |>
  count(
    cntry,
    essround,
    idno
  ) |>
  filter(
    n > 1
  )


cat(
  "\nDuplicate respondent IDs in raw weight data:",
  nrow(duplicate_weight_ids),
  "\n"
)


if (
  nrow(
    duplicate_weight_ids
  ) > 0
) {
  
  print(
    duplicate_weight_ids |>
      slice_head(
        n = 20
      ),
    n = 20,
    width = Inf
  )
  
  stop(
    "Duplicate respondent IDs found in raw weight data."
  )
}


# Reconstruct the ESS analysis weight --------------------------------
#
# Construct PSPWGHT × PWEIGHT for comparison with the supplied
# ANWEIGHT variable.
#
# At this point we do NOT classify small numerical differences as
# errors. Instead, we examine their actual magnitude.

raw_weights <- raw_weights |>
  mutate(
    anweight_reconstructed =
      pspwght *
      pweight,
    
    anweight_difference =
      anweight -
      anweight_reconstructed,
    
    anweight_abs_difference =
      abs(
        anweight_difference
      ),
    
    anweight_relative_difference =
      case_when(
        !is.na(anweight) &
          anweight != 0 &
          !is.na(
            anweight_reconstructed
          ) ~
          anweight_abs_difference /
          abs(
            anweight
          ),
        
        TRUE ~
          NA_real_
      )
  )


# Availability of the component weights -----------------------------

construction_availability <- raw_weights |>
  summarise(
    n = n(),
    
    n_anweight_available =
      sum(
        !is.na(
          anweight
        )
      ),
    
    n_pspwght_available =
      sum(
        !is.na(
          pspwght
        )
      ),
    
    n_pweight_available =
      sum(
        !is.na(
          pweight
        )
      ),
    
    n_reconstructed_available =
      sum(
        !is.na(
          anweight_reconstructed
        )
      ),
    
    n_both_available =
      sum(
        !is.na(
          anweight
        ) &
          !is.na(
            anweight_reconstructed
          )
      ),
    
    n_anweight_nonpositive =
      sum(
        !is.na(
          anweight
        ) &
          anweight <= 0
      )
  )


cat(
  "\nRaw weight availability:\n"
)

print(
  construction_availability,
  width = Inf
)


# Magnitude of ANWEIGHT reconstruction differences ------------------
#
# This is the key diagnostic replacing the previous respondent-level
# discrepancy printout.

weight_difference_summary <- raw_weights |>
  filter(
    !is.na(
      anweight
    ),
    !is.na(
      anweight_reconstructed
    )
  ) |>
  summarise(
    n_compared = n(),
    
    n_exact =
      sum(
        anweight ==
          anweight_reconstructed
      ),
    
    share_exact =
      mean(
        anweight ==
          anweight_reconstructed
      ),
    
    median_abs_difference =
      median(
        anweight_abs_difference
      ),
    
    p95_abs_difference =
      quantile(
        anweight_abs_difference,
        0.95,
        names = FALSE
      ),
    
    p99_abs_difference =
      quantile(
        anweight_abs_difference,
        0.99,
        names = FALSE
      ),
    
    max_abs_difference =
      max(
        anweight_abs_difference
      ),
    
    median_relative_difference =
      median(
        anweight_relative_difference,
        na.rm = TRUE
      ),
    
    p99_relative_difference =
      quantile(
        anweight_relative_difference,
        0.99,
        na.rm = TRUE,
        names = FALSE
      ),
    
    max_relative_difference =
      max(
        anweight_relative_difference,
        na.rm = TRUE
      )
  )


cat(
  "\nMagnitude of ANWEIGHT reconstruction differences:\n"
)

print(
  weight_difference_summary,
  width = Inf
)


# Show only the twenty largest reconstruction differences.
#
# This allows visual inspection without flooding the console with
# respondent-level output.

largest_weight_differences <- raw_weights |>
  filter(
    !is.na(
      anweight
    ),
    !is.na(
      anweight_reconstructed
    )
  ) |>
  arrange(
    desc(
      anweight_abs_difference
    )
  ) |>
  select(
    cntry,
    essround,
    idno,
    pspwght,
    pweight,
    anweight,
    anweight_reconstructed,
    anweight_difference,
    anweight_relative_difference
  ) |>
  slice_head(
    n = 20
  )


cat(
  "\nTwenty largest ANWEIGHT reconstruction differences:\n"
)

print(
  largest_weight_differences,
  n = 20,
  width = Inf
)


# Weight availability and distribution by ESS round ------------------
#
# Inspect whether weight availability or scale changes substantially
# across ESS rounds.

round_weight_summary <- raw_weights |>
  group_by(
    essround
  ) |>
  summarise(
    n = n(),
    
    n_anweight =
      sum(
        !is.na(
          anweight
        )
      ),
    
    share_anweight =
      mean(
        !is.na(
          anweight
        )
      ),
    
    n_pspwght =
      sum(
        !is.na(
          pspwght
        )
      ),
    
    n_pweight =
      sum(
        !is.na(
          pweight
        )
      ),
    
    n_dweight =
      sum(
        !is.na(
          dweight
        )
      ),
    
    n_nonpositive_anweight =
      sum(
        !is.na(
          anweight
        ) &
          anweight <= 0
      ),
    
    min_anweight =
      safe_min(
        anweight
      ),
    
    p05_anweight =
      safe_quantile(
        anweight,
        0.05
      ),
    
    median_anweight =
      safe_median(
        anweight
      ),
    
    mean_anweight =
      safe_mean(
        anweight
      ),
    
    p95_anweight =
      safe_quantile(
        anweight,
        0.95
      ),
    
    max_anweight =
      safe_max(
        anweight
      ),
    
    .groups = "drop"
  )


cat(
  "\nWeight availability and distribution by ESS round:\n"
)

print(
  round_weight_summary,
  n = Inf,
  width = Inf
)


# Weight distribution by country-round -------------------------------
#
# The REWB models operate on repeated country-round observations.
# This table therefore examines the distribution of respondent-level
# weights within each country-round.

country_round_weight_summary <- raw_weights |>
  group_by(
    cntry,
    essround
  ) |>
  summarise(
    n = n(),
    
    n_weighted =
      sum(
        !is.na(
          anweight
        ) &
          anweight > 0
      ),
    
    share_weighted =
      mean(
        !is.na(
          anweight
        ) &
          anweight > 0
      ),
    
    min_weight =
      safe_min(
        anweight
      ),
    
    p05_weight =
      safe_quantile(
        anweight,
        0.05
      ),
    
    median_weight =
      safe_median(
        anweight
      ),
    
    mean_weight =
      safe_mean(
        anweight
      ),
    
    p95_weight =
      safe_quantile(
        anweight,
        0.95
      ),
    
    max_weight =
      safe_max(
        anweight
      ),
    
    sd_weight =
      if (
        sum(
          !is.na(
            anweight
          )
        ) >= 2
      ) {
        
        sd(
          anweight,
          na.rm = TRUE
        )
        
      } else {
        
        NA_real_
      },
    
    .groups = "drop"
  ) |>
  mutate(
    coefficient_of_variation =
      case_when(
        !is.na(
          mean_weight
        ) &
          mean_weight != 0 ~
          sd_weight /
          mean_weight,
        
        TRUE ~
          NA_real_
      )
  )


cat(
  "\nCountry-round weight summary created:",
  nrow(
    country_round_weight_summary
  ),
  "country-rounds\n"
)


# Rather than print the full country-round table, show only those with
# the greatest within-country-round variation in ANWEIGHT.

largest_country_round_variation <- country_round_weight_summary |>
  arrange(
    desc(
      coefficient_of_variation
    )
  ) |>
  slice_head(
    n = 20
  )


cat(
  "\nTwenty country-rounds with greatest weight variation:\n"
)

print(
  largest_country_round_variation,
  n = 20,
  width = Inf
)


# Processed R1-R11 analysis weight -----------------------------------
#
# Check the weight variable that actually survives into the processed
# ESS analysis dataset.

ess_processed <- readRDS(
  here(
    "03_data",
    "processed",
    "ess_analysis_r1_r11.rds"
  )
) |>
  transmute(
    cntry =
      as.character(
        cntry
      ),
    
    essround =
      as.integer(
        essround
      ),
    
    idno =
      as.numeric(
        idno
      ),
    
    analysis_weight =
      as.numeric(
        analysis_weight
      )
  )


cat(
  "\nProcessed respondents in weight audit:",
  nrow(
    ess_processed
  ),
  "\n"
)


# Match processed observations back to the raw ESS weight variables.

processed_weight_check <- ess_processed |>
  left_join(
    raw_weights |>
      select(
        cntry,
        essround,
        idno,
        anweight,
        anweight_reconstructed
      ),
    by = c(
      "cntry",
      "essround",
      "idno"
    )
  ) |>
  mutate(
    expected_weight =
      case_when(
        !is.na(
          anweight
        ) &
          anweight > 0 ~
          anweight,
        
        !is.na(
          anweight_reconstructed
        ) &
          anweight_reconstructed > 0 ~
          anweight_reconstructed,
        
        TRUE ~
          NA_real_
      ),
    
    processed_difference =
      analysis_weight -
      expected_weight,
    
    processed_abs_difference =
      abs(
        processed_difference
      ),
    
    processed_relative_difference =
      case_when(
        !is.na(
          expected_weight
        ) &
          expected_weight != 0 &
          !is.na(
            analysis_weight
          ) ~
          processed_abs_difference /
          abs(
            expected_weight
          ),
        
        TRUE ~
          NA_real_
      )
  )


# Check whether the raw join succeeded for every processed respondent.

processed_join_summary <- processed_weight_check |>
  summarise(
    n = n(),
    
    n_raw_match =
      sum(
        !is.na(
          anweight
        ) |
          !is.na(
            anweight_reconstructed
          )
      ),
    
    n_without_raw_weight_match =
      sum(
        is.na(
          anweight
        ) &
          is.na(
            anweight_reconstructed
          )
      ),
    
    n_analysis_weight_available =
      sum(
        !is.na(
          analysis_weight
        )
      ),
    
    share_analysis_weight_available =
      mean(
        !is.na(
          analysis_weight
        )
      )
  )


cat(
  "\nProcessed/raw weight linkage:\n"
)

print(
  processed_join_summary,
  width = Inf
)


# Summarise the difference between the processed analysis weight and
# the corresponding raw ESS weight.
#
# Again, inspect magnitude rather than applying an arbitrary tolerance.

processed_difference_summary <- processed_weight_check |>
  filter(
    !is.na(
      analysis_weight
    ),
    !is.na(
      expected_weight
    )
  ) |>
  summarise(
    n_compared = n(),
    
    n_exact =
      sum(
        analysis_weight ==
          expected_weight
      ),
    
    share_exact =
      mean(
        analysis_weight ==
          expected_weight
      ),
    
    median_abs_difference =
      median(
        processed_abs_difference
      ),
    
    p95_abs_difference =
      quantile(
        processed_abs_difference,
        0.95,
        names = FALSE
      ),
    
    p99_abs_difference =
      quantile(
        processed_abs_difference,
        0.99,
        names = FALSE
      ),
    
    max_abs_difference =
      max(
        processed_abs_difference
      ),
    
    median_relative_difference =
      median(
        processed_relative_difference,
        na.rm = TRUE
      ),
    
    p99_relative_difference =
      quantile(
        processed_relative_difference,
        0.99,
        na.rm = TRUE,
        names = FALSE
      ),
    
    max_relative_difference =
      max(
        processed_relative_difference,
        na.rm = TRUE
      )
  )


cat(
  "\nProcessed analysis_weight comparison:\n"
)

print(
  processed_difference_summary,
  width = Inf
)


# Show only the twenty largest processed-weight differences.

largest_processed_differences <- processed_weight_check |>
  filter(
    !is.na(
      analysis_weight
    ),
    !is.na(
      expected_weight
    )
  ) |>
  arrange(
    desc(
      processed_abs_difference
    )
  ) |>
  select(
    cntry,
    essround,
    idno,
    analysis_weight,
    anweight,
    anweight_reconstructed,
    expected_weight,
    processed_difference,
    processed_relative_difference
  ) |>
  slice_head(
    n = 20
  )


cat(
  "\nTwenty largest processed-weight differences:\n"
)

print(
  largest_processed_differences,
  n = 20,
  width = Inf
)


# Processed weight availability by round ----------------------------
#
# Confirm that weight availability in the processed dataset does not
# vary unexpectedly across the analytical period.

processed_round_summary <- processed_weight_check |>
  group_by(
    essround
  ) |>
  summarise(
    n = n(),
    
    n_weight_available =
      sum(
        !is.na(
          analysis_weight
        ) &
          analysis_weight > 0
      ),
    
    share_weight_available =
      mean(
        !is.na(
          analysis_weight
        ) &
          analysis_weight > 0
      ),
    
    min_weight =
      safe_min(
        analysis_weight
      ),
    
    median_weight =
      safe_median(
        analysis_weight
      ),
    
    mean_weight =
      safe_mean(
        analysis_weight
      ),
    
    max_weight =
      safe_max(
        analysis_weight
      ),
    
    .groups = "drop"
  )


cat(
  "\nProcessed weight availability by ESS round:\n"
)

print(
  processed_round_summary,
  n = Inf,
  width = Inf
)


# Save audit outputs -------------------------------------------------
#
# Detailed respondent-level data are not written out. The saved files
# contain compact summaries plus the small sets of observations needed
# to inspect the largest numerical differences.

output_dir <- here(
  "05_output",
  "tables",
  "weight_audit"
)


dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


write_csv(
  construction_availability,
  file.path(
    output_dir,
    "weight_construction_availability.csv"
  )
)


write_csv(
  weight_difference_summary,
  file.path(
    output_dir,
    "weight_reconstruction_difference_summary.csv"
  )
)


write_csv(
  largest_weight_differences,
  file.path(
    output_dir,
    "largest_weight_reconstruction_differences.csv"
  )
)


write_csv(
  round_weight_summary,
  file.path(
    output_dir,
    "weight_summary_by_round.csv"
  )
)


write_csv(
  country_round_weight_summary,
  file.path(
    output_dir,
    "weight_summary_by_country_round.csv"
  )
)


write_csv(
  processed_join_summary,
  file.path(
    output_dir,
    "processed_weight_linkage_summary.csv"
  )
)


write_csv(
  processed_difference_summary,
  file.path(
    output_dir,
    "processed_weight_difference_summary.csv"
  )
)


write_csv(
  largest_processed_differences,
  file.path(
    output_dir,
    "largest_processed_weight_differences.csv"
  )
)


write_csv(
  processed_round_summary,
  file.path(
    output_dir,
    "processed_weight_summary_by_round.csv"
  )
)


cat(
  "\nWeight audit outputs saved in:",
  output_dir,
  "\n"
)


# Decompose the analysis weight --------------------------------------
#
# ANWEIGHT combines two conceptually different components:
#
#   PSPWGHT:
#     adjusts respondent representation within countries for sample
#     design, nonresponse, noncoverage, and post-stratification.
#
#   PWEIGHT:
#     adjusts the relative contribution of countries according to
#     population size.
#
# Because the substantive analysis concerns within-country change,
# inspect these components separately before deciding how survey
# weighting should enter the multilevel models.


pspwght_summary_by_round <- raw_weights |>
  group_by(
    essround
  ) |>
  summarise(
    n = n(),
    
    min_pspwght =
      safe_min(
        pspwght
      ),
    
    p05_pspwght =
      safe_quantile(
        pspwght,
        0.05
      ),
    
    median_pspwght =
      safe_median(
        pspwght
      ),
    
    mean_pspwght =
      safe_mean(
        pspwght
      ),
    
    p95_pspwght =
      safe_quantile(
        pspwght,
        0.95
      ),
    
    max_pspwght =
      safe_max(
        pspwght
      ),
    
    .groups = "drop"
  )


cat(
  "\nPSPWGHT distribution by ESS round:\n"
)

print(
  pspwght_summary_by_round,
  n = Inf,
  width = Inf
)


# PWEIGHT should be constant within country-rounds. Verify this and
# obtain one population-size weight per country-round.

pweight_country_round <- raw_weights |>
  group_by(
    cntry,
    essround
  ) |>
  summarise(
    n = n(),
    
    n_distinct_pweight =
      n_distinct(
        pweight[
          !is.na(
            pweight
          )
        ]
      ),
    
    pweight =
      first(
        pweight[
          !is.na(
            pweight
          )
        ]
      ),
    
    .groups = "drop"
  )


pweight_not_constant <- pweight_country_round |>
  filter(
    n_distinct_pweight != 1
  )


cat(
  "\nCountry-rounds where PWEIGHT is not constant:",
  nrow(
    pweight_not_constant
  ),
  "\n"
)


# Summarise the population-size component itself.

pweight_summary <- pweight_country_round |>
  summarise(
    n_country_rounds = n(),
    
    min_pweight =
      safe_min(
        pweight
      ),
    
    median_pweight =
      safe_median(
        pweight
      ),
    
    mean_pweight =
      safe_mean(
        pweight
      ),
    
    max_pweight =
      safe_max(
        pweight
      )
  )


cat(
  "\nPWEIGHT distribution across country-rounds:\n"
)

print(
  pweight_summary,
  width = Inf
)


# Show the most heavily and least heavily population-weighted
# country-rounds.

pweight_extremes <- bind_rows(
  
  pweight_country_round |>
    arrange(
      pweight
    ) |>
    slice_head(
      n = 10
    ) |>
    mutate(
      extreme = "Lowest"
    ),
  
  pweight_country_round |>
    arrange(
      desc(
        pweight
      )
    ) |>
    slice_head(
      n = 10
    ) |>
    mutate(
      extreme = "Highest"
    )
)


cat(
  "\nLowest and highest population-size weights:\n"
)

print(
  pweight_extremes,
  n = Inf,
  width = Inf
)


# Save the additional audit outputs.

write_csv(
  pspwght_summary_by_round,
  file.path(
    output_dir,
    "pspwght_summary_by_round.csv"
  )
)


write_csv(
  pweight_country_round,
  file.path(
    output_dir,
    "pweight_by_country_round.csv"
  )
)


write_csv(
  pweight_summary,
  file.path(
    output_dir,
    "pweight_summary.csv"
  )
)

