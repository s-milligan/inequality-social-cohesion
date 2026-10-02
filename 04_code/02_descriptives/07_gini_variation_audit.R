# Within-country Gini variation diagnostic
#
# Prerequisite:
#   04_code/03_models/06_party_hate_moderation.R
#
# Input:
#   Saved country-round contexts for the actual model samples.
#
# Diagnostic regressions use one row per country-round:
#   1. Country effects
#   2. Country + round effects
#   3. Country + round effects + macro controls
#   4. Country + round effects + macro controls + hate speech
#
# All stages retain country effects to isolate within-country
# variation. Between-country predictors are absorbed by these
# effects and are therefore not entered separately.
#
# Equal country-round weights; no respondent-level regression.
# These diagnostics do not reproduce mixed-model GLS weighting
# or account for individual-level covariates.
#
# Outputs:
#   05_output/tables/gini_variation_audit/

library(dplyr)
library(here)
library(readr)
library(tibble)

input_dir <- here(
  "05_output", "tables", "party_hate_moderation"
)

output_dir <- here(
  "05_output", "tables", "gini_variation_audit"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

periods <- c(
  r1_r9 = "R1-R9",
  r1_r11 = "R1-R11"
)

diagnostic_formulas <- list(
  country = gini_w ~ cntry,
  round = gini_w ~ cntry + round_factor,
  macro = gini_w ~ cntry + round_factor +
    gdp_w_10 + unemployment_w,
  hate = gini_w ~ cntry + round_factor +
    gdp_w_10 + unemployment_w + hate_w
)

stage_labels <- c(
  country = "Country effects",
  round = "+ Round effects",
  macro = "+ GDP and unemployment",
  hate = "+ Hate speech"
)

required <- c(
  "cntry", "essround", "gini_swiid_lag1",
  "gini_w", "gdp_w_10", "unemployment_w", "hate_w"
)

numeric_columns <- setdiff(required, "cntry")

overview_rows <- list()
country_rows <- list()
adjustment_rows <- list()
residual_rows <- list()

# 1. Analyse each period --------------------------------------------------

for (file_id in names(periods)) {
  sample_id <- unname(periods[[file_id]])

  context <- read_csv(
    file.path(input_dir, paste0(file_id, "_context.csv")),
    show_col_types = FALSE
  ) |>
    select(all_of(required))

  stopifnot(
    !anyNA(context[c("cntry", "essround")]),
    !anyDuplicated(context[c("cntry", "essround")]),
    all(vapply(
      context[numeric_columns],
      function(x) all(is.finite(x)),
      logical(1)
    ))
  )

  # Check the saved Gini within-country decomposition.
  check <- context |>
    group_by(cntry) |>
    summarise(
      maximum_difference = max(abs(
        gini_w -
          (gini_swiid_lag1 - mean(gini_swiid_lag1))
      )),
      .groups = "drop"
    )

  stopifnot(all(check$maximum_difference < 1e-8))

  countries <- context |>
    group_by(cntry) |>
    summarise(
      n_rounds = n(),
      gini_mean = mean(gini_swiid_lag1),
      gini_min = min(gini_swiid_lag1),
      gini_max = max(gini_swiid_lag1),
      gini_range = gini_max - gini_min,
      gini_sd = if (n() > 1L) sd(gini_swiid_lag1) else NA_real_,
      within_sum_squares = sum(gini_w^2),
      .groups = "drop"
    ) |>
    mutate(
      sample = sample_id,
      repeated_country = n_rounds >= 2L,
      near_constant = repeated_country & gini_range < 1e-10,
      .before = 1
    )

  total_within_ss <- sum(countries$within_sum_squares)

  if (total_within_ss <= 1e-12) {
    stop(sample_id, ": insufficient Gini variation for this diagnostic.")
  }

  countries <- countries |>
    mutate(
      share_within_sum_squares =
        within_sum_squares / total_within_ss
    )

  repeated <- countries |> filter(repeated_country)

  overview_rows[[file_id]] <- tibble(
    sample = sample_id,
    n_countries = nrow(countries),
    n_country_rounds = nrow(context),
    n_repeated_countries = nrow(repeated),
    n_single_round_countries = sum(!countries$repeated_country),
    n_near_constant_repeated_countries = sum(countries$near_constant),
    overall_country_round_gini_sd = sd(context$gini_swiid_lag1),
    within_gini_sd = sd(context$gini_w),
    median_country_gini_sd = median(repeated$gini_sd),
    median_country_gini_range = median(repeated$gini_range)
  )

  dat <- context |>
    mutate(
      cntry = factor(cntry),
      round_factor = factor(essround)
    )

  stage_output <- list()
  baseline_ss <- NA_real_

  for (stage in names(diagnostic_formulas)) {
    fit <- lm(
      formula = diagnostic_formulas[[stage]],
      data = dat,
      na.action = na.fail
    )

    e <- residuals(fit)
    residual_ss <- sum(e^2)

    if (stage == "country") {
      baseline_ss <- residual_ss
    }

    stage_output[[stage]] <- tibble(
      sample = sample_id,
      stage = unname(stage_labels[[stage]]),
      n_country_rounds = nobs(fit),
      design_rank = fit$rank,
      residual_df = df.residual(fit),
      n_aliased_coefficients = sum(is.na(coef(fit))),
      # Use the same denominator at every stage for comparability.
      # This is residual SD across rows, not sigma(fit).
      residual_gini_sd = sd(e),
      residual_sum_squares = residual_ss,
      share_within_variation_remaining = residual_ss / baseline_ss,
      share_within_variation_explained = 1 - residual_ss / baseline_ss
    )

    dat[[paste0("gini_residual_", stage)]] <- unname(e)
  }

  adjustments <- bind_rows(stage_output) |>
    mutate(
      additional_share_explained =
        lag(share_within_variation_remaining, default = 1) -
        share_within_variation_remaining
    )

  # Descriptive distribution of remaining variation across countries.
  residual_by_country <- dat |>
    group_by(cntry) |>
    summarise(
      adjusted_sum_squares = sum(gini_residual_hate^2),
      .groups = "drop"
    ) |>
    mutate(cntry = as.character(cntry))

  adjusted_total <- sum(residual_by_country$adjusted_sum_squares)

  residual_by_country <- residual_by_country |>
    mutate(
      share_adjusted_sum_squares = if (adjusted_total > 1e-12) {
        adjusted_sum_squares / adjusted_total
      } else {
        NA_real_
      }
    )

  country_rows[[file_id]] <- countries |>
    left_join(residual_by_country, by = "cntry")

  adjustment_rows[[file_id]] <- adjustments

  residual_rows[[file_id]] <- dat |>
    mutate(
      sample = sample_id,
      cntry = as.character(cntry),
      .before = 1
    )
}

# 2. Combine results ------------------------------------------------------

variation_overview <- bind_rows(overview_rows)
country_variation <- bind_rows(country_rows)
adjustment_summary <- bind_rows(adjustment_rows)
country_round_residuals <- bind_rows(residual_rows)

limited_variation <- country_variation |>
  filter(!repeated_country | near_constant) |>
  select(sample, cntry, n_rounds, gini_range, gini_sd)

largest_raw_shares <- country_variation |>
  group_by(sample) |>
  slice_max(
    order_by = share_within_sum_squares,
    n = 5,
    with_ties = FALSE
  ) |>
  ungroup() |>
  select(
    sample, cntry, n_rounds, gini_sd, gini_range,
    share_within_sum_squares
  )

largest_adjusted_shares <- country_variation |>
  filter(is.finite(share_adjusted_sum_squares)) |>
  group_by(sample) |>
  slice_max(
    order_by = share_adjusted_sum_squares,
    n = 5,
    with_ties = FALSE
  ) |>
  ungroup() |>
  select(
    sample, cntry, n_rounds,
    share_within_sum_squares,
    share_adjusted_sum_squares
  )

# 3. Save and print -------------------------------------------------------

write_csv(
  variation_overview,
  file.path(output_dir, "variation_overview.csv")
)
write_csv(
  country_variation,
  file.path(output_dir, "country_variation.csv")
)
write_csv(
  adjustment_summary,
  file.path(output_dir, "adjustment_summary.csv")
)
write_csv(
  country_round_residuals,
  file.path(output_dir, "country_round_residuals.csv")
)
write_csv(
  largest_raw_shares,
  file.path(output_dir, "largest_raw_shares.csv")
)
write_csv(
  largest_adjusted_shares,
  file.path(output_dir, "largest_adjusted_shares.csv")
)

writeLines(
  capture.output(sessionInfo()),
  file.path(output_dir, "session_info.txt")
)

cat("\nGini variation overview:\n")
print(variation_overview, n = Inf, width = Inf)

cat("\nVariation remaining after successive adjustments:\n")
print(adjustment_summary, n = Inf, width = Inf)

cat("\nCountries with one round or near-constant Gini:\n")
print(limited_variation, n = Inf, width = Inf)

cat("\nLargest shares of original within-country Gini variation:\n")
print(largest_raw_shares, n = Inf, width = Inf)

cat("\nLargest shares of adjusted Gini variation:\n")
print(largest_adjusted_shares, n = Inf, width = Inf)
