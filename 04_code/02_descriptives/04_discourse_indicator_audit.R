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