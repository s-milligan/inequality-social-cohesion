# ------------------------------------------------------------
# Construct country-round inequality exposure
# Project: Inequality and social cohesion
#
# Purpose:
#   Aggregate annual inequality measures to ESS country-rounds
#   using the observed distribution of interviews across years.
#
#   SWIID is the provisional primary inequality source.
#   OECD IDD is retained for robustness analyses.
# ------------------------------------------------------------

library(dplyr)
library(readr)
library(here)

# 1. Import ESS country-round structure ----------------------------------

country_rounds <- read_csv(
  here(
    "03_data",
    "interim",
    "ess_country_round_coverage.csv"
  ),
  show_col_types = FALSE
)

# This contains one row per country × round × interview year,
# already matched to OECD and SWIID.
coverage <- read_csv(
  here(
    "03_data",
    "interim",
    "ess_inequality_coverage.csv"
  ),
  show_col_types = FALSE
)

# 2. Calculate interview-year shares -------------------------------------

# Some ESS country-rounds span two calendar years.
# Weight annual contextual values according to the proportion
# of respondents interviewed in each year.

coverage <- coverage |>
  group_by(cntry, essround) |>
  mutate(
    interview_year_share =
      n_respondents / sum(n_respondents)
  ) |>
  ungroup()

# Check that shares sum to 1
share_check <- coverage |>
  group_by(cntry, essround) |>
  summarise(
    share_sum = sum(interview_year_share),
    .groups = "drop"
  )

stopifnot(
  all(abs(share_check$share_sum - 1) < 1e-10)
)

# 3. Aggregate inequality to country-round -------------------------------

inequality_country_round <- coverage |>
  group_by(cntry, essround) |>
  summarise(

    # Fieldwork timing
    n_respondents_with_year =
      sum(n_respondents),

    first_interview_year =
      min(interview_year),

    last_interview_year =
      max(interview_year),

    mean_interview_year =
      weighted.mean(
        interview_year,
        interview_year_share
      ),

    # SWIID: primary series
    gini_swiid_contemp =
      weighted.mean(
        gini_swiid_contemp,
        interview_year_share
      ),

    gini_swiid_lag1 =
      weighted.mean(
        gini_swiid_lag1,
        interview_year_share
      ),

    # Average reported SWIID uncertainty.
    # Diagnostic only; NOT used as an inferential SE.
    swiid_lag1_se_mean =
      weighted.mean(
        gini_swiid_lag1_se,
        interview_year_share
      ),

    # OECD: use only when every interview-year component
    # of the country-round has an observed value.
    oecd_contemp_complete =
      all(!is.na(gini_oecd_contemp)),

    oecd_lag1_complete =
      all(!is.na(gini_oecd_lag1)),

    gini_oecd_contemp =
      if (all(!is.na(gini_oecd_contemp))) {
        weighted.mean(
          gini_oecd_contemp,
          interview_year_share
        )
      } else {
        NA_real_
      },

    gini_oecd_lag1 =
      if (all(!is.na(gini_oecd_lag1))) {
        weighted.mean(
          gini_oecd_lag1,
          interview_year_share
        )
      } else {
        NA_real_
      },

    .groups = "drop"
  )

# 4. Restore full ESS country-round structure ----------------------------

# Estonia Round 5 currently has no usable interview-year information
# and is therefore absent from the coverage file.
# Joining to the full country-round table preserves it explicitly
# rather than silently dropping it.

country_round_inequality <- country_rounds |>
  select(
    cntry,
    essround,
    n_respondents,
    first_year,
    last_year,
    n_interview_years,
    crosses_year
  ) |>
  left_join(
    inequality_country_round,
    by = c("cntry", "essround")
  ) |>
  arrange(cntry, essround)

# 5. Diagnostics ----------------------------------------------------------

cat(
  "\nESS country-rounds:",
  nrow(country_round_inequality),
  "\n"
)

cat(
  "Country-rounds with SWIID lagged Gini:",
  sum(!is.na(
    country_round_inequality$gini_swiid_lag1
  )),
  "\n"
)

cat(
  "Country-rounds with complete OECD lagged Gini:",
  sum(!is.na(
    country_round_inequality$gini_oecd_lag1
  )),
  "\n"
)

cat("\nMissing SWIID country-rounds:\n")

print(
  country_round_inequality |>
    filter(is.na(gini_swiid_lag1)) |>
    select(
      cntry,
      essround,
      n_respondents,
      first_year,
      last_year
    )
)

# 6. Inspect agreement between sources -----------------------------------

source_comparison <- country_round_inequality |>
  filter(
    !is.na(gini_swiid_lag1),
    !is.na(gini_oecd_lag1)
  )

cat(
  "\nCorrelation between SWIID and OECD lagged Gini:",
  cor(
    source_comparison$gini_swiid_lag1,
    source_comparison$gini_oecd_lag1
  ),
  "\n"
)

# 7. Save ----------------------------------------------------------------

write_csv(
  country_round_inequality,
  here(
    "03_data",
    "interim",
    "ess_country_round_inequality.csv"
  )
)

