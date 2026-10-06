# ------------------------------------------------------------
# Audit candidate political-discourse indicators
# Source: V-Dem Country-Year Full+Others v16
# ------------------------------------------------------------

library(dplyr)
library(here)
library(readr)
library(tidyr)

# 1. Import ---------------------------------------------------

vdem_file <- here(
  "03_data", "raw", "vdem",
  "V-Dem-CY-Full+Others-v16.csv"
)

stopifnot(file.exists(vdem_file))

indicators <- c(
  "v2smpolhate",
  "v2dlcountr",
  "v2cacamps"
)

required_columns <- c(
  "country_name",
  "country_text_id",
  "year",
  indicators
)

available_columns <- names(
  read_csv(
    vdem_file,
    n_max = 0,
    show_col_types = FALSE
  )
)

missing_columns <- setdiff(
  required_columns,
  available_columns
)

if (length(missing_columns) > 0) {
  stop(
    "Missing V-Dem columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

vdem <- read_csv(
  vdem_file,
  col_select = all_of(required_columns),
  show_col_types = FALSE
)

ess <- readRDS(
  here(
    "03_data", "processed",
    "ess_analysis_r1_r11.rds"
  )
)

# 2. Explicit country correspondence --------------------------
# Alternative names are included where releases may differ.
# Unmatched names are reported rather than silently dropped.

country_lookup <- tribble(
  ~country_name,        ~cntry,
  "Albania",           "AL",
  "Austria",           "AT",
  "Belgium",           "BE",
  "Bulgaria",          "BG",
  "Croatia",           "HR",
  "Cyprus",            "CY",
  "Czech Republic",    "CZ",
  "Czechia",           "CZ",
  "Denmark",           "DK",
  "Estonia",           "EE",
  "Finland",           "FI",
  "France",            "FR",
  "Germany",           "DE",
  "Greece",            "GR",
  "Hungary",           "HU",
  "Iceland",           "IS",
  "Ireland",           "IE",
  "Israel",            "IL",
  "Italy",             "IT",
  "Kosovo",            "XK",
  "Latvia",            "LV",
  "Lithuania",         "LT",
  "Luxembourg",        "LU",
  "Malta",             "MT",
  "Montenegro",        "ME",
  "Netherlands",       "NL",
  "North Macedonia",   "MK",
  "Macedonia",         "MK",
  "Norway",            "NO",
  "Poland",            "PL",
  "Portugal",          "PT",
  "Romania",           "RO",
  "Russia",            "RU",
  "Russian Federation","RU",
  "Serbia",            "RS",
  "Slovakia",          "SK",
  "Slovenia",          "SI",
  "Spain",             "ES",
  "Sweden",            "SE",
  "Switzerland",       "CH",
  "Turkey",            "TR",
  "Türkiye",           "TR",
  "Ukraine",           "UA",
  "United Kingdom",    "GB"
)

ess_countries <- ess |>
  transmute(cntry = as.character(cntry)) |>
  distinct()

vdem_ess <- vdem |>
  inner_join(country_lookup, by = "country_name") |>
  semi_join(ess_countries, by = "cntry")

unmatched_countries <- ess_countries |>
  anti_join(
    vdem_ess |> distinct(cntry),
    by = "cntry"
  )

if (nrow(unmatched_countries) > 0) {
  print(unmatched_countries, n = Inf)
  print(sort(unique(vdem$country_name)))

  stop(
    "Some ESS countries were not matched. ",
    "Review country_lookup before continuing."
  )
}

duplicate_country_years <- vdem_ess |>
  count(cntry, year) |>
  filter(n > 1)

if (nrow(duplicate_country_years) > 0) {
  print(duplicate_country_years, n = Inf)
  stop("Country-year correspondence is not unique.")
}

# 3. Coverage over a fixed screening window -------------------
# 2000-2025 covers the broad ESS era and preceding context.
# Exact ESS fieldwork and lag matching will follow separately.

audit_years <- 2000:2025

audit <- expand_grid(
  cntry = sort(ess_countries$cntry),
  year = audit_years
) |>
  left_join(
    vdem_ess |>
      select(cntry, year, all_of(indicators)) |>
      mutate(vdem_row_present = TRUE),
    by = c("cntry", "year")
  ) |>
  mutate(
    vdem_row_present = coalesce(
      vdem_row_present, FALSE
    )
  ) |>
  pivot_longer(
    cols = all_of(indicators),
    names_to = "indicator",
    values_to = "value"
  )

# Helpers avoid Inf/NaN when an entire series is missing.

safe_min <- function(x) {
  if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

country_summary <- audit |>
  group_by(cntry, indicator) |>
  summarise(
    n_years_expected = n(),
    n_years_available = sum(!is.na(value)),
    first_year = safe_min(year[!is.na(value)]),
    last_year = safe_max(year[!is.na(value)]),
    minimum = safe_min(value),
    maximum = safe_max(value),
    sd_over_time = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    share_available =
      n_years_available / n_years_expected
  )

overall_summary <- country_summary |>
  group_by(indicator) |>
  summarise(
    n_countries = n(),
    n_country_years_expected = sum(n_years_expected),
    n_country_years_available = sum(n_years_available),
    countries_with_full_coverage =
      sum(share_available == 1),
    countries_with_zero_variation =
      sum(sd_over_time == 0, na.rm = TRUE),
    median_country_sd =
      median(sd_over_time, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    share_available =
      n_country_years_available /
      n_country_years_expected
  )

missing_years <- audit |>
  filter(is.na(value)) |>
  select(
    cntry, year, indicator,
    vdem_row_present
  )

# 4. Check availability of ESS interview-year information ------

timing_summary <- if ("interview_year" %in% names(ess)) {
  ess |>
    group_by(essround) |>
    summarise(
      n_respondents = n(),
      n_with_interview_year =
        sum(!is.na(interview_year)),
      share_with_interview_year =
        mean(!is.na(interview_year)),
      .groups = "drop"
    )
} else {
  warning(
    "No interview_year column in the ESS analysis file."
  )
  tibble()
}

# 5. Print and save --------------------------------------------

cat("\nOverall indicator coverage, 2000-2025:\n")
print(overall_summary, n = Inf, width = Inf)

cat("\nIncomplete coverage or no observed annual variation:\n")
print(
  country_summary |>
    filter(
      share_available < 1 |
        is.na(sd_over_time) |
        sd_over_time == 0
    ),
  n = Inf,
  width = Inf
)

cat("\nESS interview-year availability:\n")
print(timing_summary, n = Inf, width = Inf)

output_dir <- here(
  "05_output", "tables", "discourse_audit"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  overall_summary,
  file.path(output_dir, "indicator_coverage.csv")
)

write_csv(
  country_summary,
  file.path(output_dir, "indicator_country_summary.csv")
)

write_csv(
  missing_years,
  file.path(output_dir, "indicator_missing_years.csv")
)

write_csv(
  timing_summary,
  file.path(output_dir, "ess_interview_year_coverage.csv")
)


# 6. Construct ESS country-round discourse exposures -----------

timing_base <- read_csv(
  here(
    "03_data", "interim",
    "ess_inequality_coverage.csv"
  ),
  show_col_types = FALSE
) |>
  select(
    cntry, essround,
    interview_year, n_respondents
  )

timing_post9 <- read_csv(
  here(
    "03_data", "interim",
    "ess_post9_country_round_year_coverage.csv"
  ),
  show_col_types = FALSE
)

stopifnot(
  all(timing_base$essround %in% 1:9),
  all(timing_post9$essround %in% 10:11)
)

timing <- bind_rows(
  timing_base,
  timing_post9
)

# Check keys and counts before constructing shares.

timing_duplicates <- timing |>
  count(cntry, essround, interview_year) |>
  filter(n > 1)

if (nrow(timing_duplicates) > 0) {
  print(timing_duplicates, n = Inf)
  stop("Duplicate country-round-year timing rows.")
}

stopifnot(
  all(!is.na(timing$cntry)),
  all(!is.na(timing$essround)),
  all(is.finite(timing$n_respondents)),
  all(timing$n_respondents > 0),
  all(timing$n_respondents == floor(timing$n_respondents))
)

# Preserve the full country-round structure of the ESS dataset.

ess_rounds <- ess |>
  count(
    cntry, essround,
    name = "n_respondents_total"
  )

unexpected_rounds <- timing |>
  distinct(cntry, essround) |>
  anti_join(
    ess_rounds,
    by = c("cntry", "essround")
  )

if (nrow(unexpected_rounds) > 0) {
  print(unexpected_rounds, n = Inf)
  stop("Timing rows do not match the ESS country-rounds.")
}

# Shares use respondents with known interview years only.

timing_known <- timing |>
  filter(!is.na(interview_year)) |>
  group_by(cntry, essround) |>
  mutate(
    interview_year_share =
      n_respondents / sum(n_respondents)
  ) |>
  ungroup()

stopifnot(
  all(is.finite(timing_known$interview_year)),
  all(
    timing_known$interview_year ==
      floor(timing_known$interview_year)
  )
)

share_check <- timing_known |>
  group_by(cntry, essround) |>
  summarise(
    share_sum = sum(interview_year_share),
    .groups = "drop"
  )

stopifnot(
  all(abs(share_check$share_sum - 1) < 1e-10)
)

timing_counts <- timing_known |>
  group_by(cntry, essround) |>
  summarise(
    n_respondents_with_year = sum(n_respondents),
    first_interview_year = min(interview_year),
    last_interview_year = max(interview_year),
    .groups = "drop"
  )

timing_diagnostics <- ess_rounds |>
  left_join(
    timing_counts,
    by = c("cntry", "essround")
  ) |>
  mutate(
    n_respondents_with_year =
      coalesce(n_respondents_with_year, 0),
    n_respondents_without_year =
      n_respondents_total - n_respondents_with_year,
    share_with_interview_year =
      n_respondents_with_year / n_respondents_total
  )

if (any(timing_diagnostics$n_respondents_without_year < 0)) {
  print(
    timing_diagnostics |>
      filter(n_respondents_without_year < 0),
    n = Inf
  )
  stop("Timing counts exceed the ESS respondent counts.")
}

# 7. Match annual V-Dem values at lag 0 and lag 1 ---------------

annual_discourse <- vdem_ess |>
  select(
    cntry, year,
    all_of(indicators)
  )

timing_matches <- bind_rows(
  timing_known |>
    mutate(
      specification = "contemp",
      context_year = interview_year
    ),
  timing_known |>
    mutate(
      specification = "lag1",
      context_year = interview_year - 1
    )
) |>
  left_join(
    annual_discourse,
    by = c(
      "cntry",
      "context_year" = "year"
    )
  )

# Retain missing annual matches for inspection.
# Do not renormalise over available V-Dem years.

annual_gaps <- timing_matches |>
  filter(
    if_any(
      all_of(indicators),
      ~ is.na(.x)
    )
  ) |>
  select(
    cntry, essround,
    interview_year, context_year,
    specification, interview_year_share,
    all_of(indicators)
  )

exposures <- timing_matches |>
  pivot_longer(
    cols = all_of(indicators),
    names_to = "indicator",
    values_to = "value"
  ) |>
  group_by(
    cntry, essround,
    specification, indicator
  ) |>
  summarise(
    exposure = if (all(is.finite(value))) {
      weighted.mean(
        value,
        interview_year_share
      )
    } else {
      NA_real_
    },
    .groups = "drop"
  ) |>
  pivot_wider(
    id_cols = c(cntry, essround),
    names_from = c(indicator, specification),
    values_from = exposure,
    names_glue = "{indicator}_{specification}"
  )

discourse_context <- timing_diagnostics |>
  left_join(
    exposures,
    by = c("cntry", "essround")
  ) |>
  mutate(
    # Reverse the interval-scale estimates by changing sign.
    # Higher values now indicate more of the named construct.
    
    party_hate_contemp = -v2smpolhate_contemp,
    party_hate_lag1 = -v2smpolhate_lag1,
    
    counterarg_disrespect_contemp = -v2dlcountr_contemp,
    counterarg_disrespect_lag1 = -v2dlcountr_lag1
  ) |>
  arrange(cntry, essround)

stopifnot(
  nrow(discourse_context) == nrow(ess_rounds)
)

# 8. Diagnostics and outputs ----------------------------------

exposure_columns <- c(
  "v2smpolhate_contemp",
  "v2smpolhate_lag1",
  "v2dlcountr_contemp",
  "v2dlcountr_lag1",
  "v2cacamps_contemp",
  "v2cacamps_lag1"
)

matched_coverage <- discourse_context |>
  group_by(essround) |>
  summarise(
    n_country_rounds = n(),
    across(
      all_of(exposure_columns),
      ~ sum(!is.na(.x)),
      .names = "available_{.col}"
    ),
    .groups = "drop"
  )

cat("\nCountry-round exposure coverage:\n")
print(matched_coverage, n = Inf, width = Inf)

cat("\nCountry-rounds with incomplete interview-year information:\n")
print(
  timing_diagnostics |>
    filter(n_respondents_without_year > 0),
  n = Inf,
  width = Inf
)

cat("\nMissing annual V-Dem matches:\n")
print(annual_gaps, n = Inf, width = Inf)

cat("\nCountry-rounds with any missing discourse exposure:\n")
print(
  discourse_context |>
    filter(
      if_any(
        all_of(exposure_columns),
        ~ is.na(.x)
      )
    ) |>
    select(
      cntry, essround,
      n_respondents_total,
      n_respondents_with_year,
      all_of(exposure_columns)
    ),
  n = Inf,
  width = Inf
)

write_csv(
  discourse_context,
  here(
    "03_data", "interim",
    "ess_country_round_discourse_r1_r11.csv"
  )
)

write_csv(
  matched_coverage,
  file.path(
    output_dir,
    "discourse_country_round_coverage.csv"
  )
)

write_csv(
  timing_diagnostics,
  file.path(
    output_dir,
    "discourse_timing_diagnostics.csv"
  )
)

write_csv(
  annual_gaps,
  file.path(
    output_dir,
    "discourse_annual_matching_gaps.csv"
  )
)

# 9. Variation at observed ESS country-rounds -------------------

variation_data <- bind_rows(
  discourse_context |>
    filter(essround <= 9) |>
    mutate(sample = "R1-R9"),
  discourse_context |>
    mutate(sample = "R1-R11")
) |>
  select(
    sample, cntry, essround,
    all_of(exposure_columns)
  ) |>
  pivot_longer(
    cols = all_of(exposure_columns),
    names_to = "indicator",
    values_to = "value"
  )

ess_variation <- variation_data |>
  group_by(sample, cntry, indicator) |>
  summarise(
    n_rounds_available = sum(!is.na(value)),
    country_mean = if (all(is.na(value))) {
      NA_real_
    } else {
      mean(value, na.rm = TRUE)
    },
    sd_across_rounds = sd(value, na.rm = TRUE),
    observed_range = safe_max(value) - safe_min(value),
    .groups = "drop"
  )

variation_summary <- ess_variation |>
  group_by(sample, indicator) |>
  summarise(
    n_countries = n(),
    countries_with_two_plus_rounds =
      sum(n_rounds_available >= 2),
    countries_with_no_variation = sum(
      n_rounds_available >= 2 &
        observed_range < 1e-10,
      na.rm = TRUE
    ),
    median_within_country_sd =
      median(sd_across_rounds, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nVariation at observed ESS country-rounds:\n")
print(variation_summary, n = Inf, width = Inf)

cat("\nCountries with insufficient rounds or no variation:\n")
print(
  ess_variation |>
    filter(
      n_rounds_available < 2 |
        observed_range < 1e-10
    ) |>
    select(
      sample, cntry, indicator,
      n_rounds_available, observed_range
    ),
  n = Inf,
  width = Inf
)

write_csv(
  ess_variation,
  file.path(
    output_dir,
    "discourse_variation_by_country.csv"
  )
)

write_csv(
  variation_summary,
  file.path(
    output_dir,
    "discourse_variation_summary.csv"
  )
)


# 10. Party hate-speech trajectories ----------------------------

library(ggplot2)

plot_data <- discourse_context |>
  filter(!is.na(first_interview_year)) |>
  select(
    cntry, essround,
    party_hate_contemp,
    party_hate_lag1
  ) |>
  pivot_longer(
    cols = c(
      party_hate_contemp,
      party_hate_lag1
    ),
    names_to = "timing",
    values_to = "score"
  ) |>
  mutate(
    timing = factor(
      timing,
      levels = c(
        "party_hate_contemp",
        "party_hate_lag1"
      ),
      labels = c(
        "Contemporaneous",
        "One-year lag"
      )
    )
  )

# Only draw lines for series with at least two observations.

line_data <- plot_data |>
  filter(!is.na(score)) |>
  group_by(cntry, timing) |>
  filter(n() >= 2) |>
  ungroup()

p_hate <- ggplot(
  plot_data,
  aes(
    x = essround,
    y = score,
    colour = timing,
    group = timing
  )
) +
  geom_line(
    data = line_data,
    linewidth = 0.5
  ) +
  geom_point(size = 1.2, na.rm = TRUE) +
  facet_wrap(~ cntry, ncol = 6) +
  scale_x_continuous(
    breaks = c(1, 5, 9, 11)
  ) +
  scale_colour_manual(
    values = c(
      "Contemporaneous" = "#0072B2",
      "One-year lag" = "#D55E00"
    )
  ) +
  labs(
    title = "Party hate speech at observed ESS country-rounds",
    subtitle = "Reversed V-Dem interval estimates; higher values indicate more hate speech",
    x = "ESS round",
    y = "Party hate-speech score",
    colour = NULL,
    caption = paste(
      "Common vertical scale across countries.",
      "Lines connect available rounds;",
      "gaps are not interpolated observations."
    )
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(p_hate)

figure_dir <- here(
  "05_output", "figures", "discourse_audit"
)

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  filename = file.path(
    figure_dir,
    "party_hate_ess_trajectories.png"
  ),
  plot = p_hate,
  width = 14,
  height = 14,
  dpi = 200
)

