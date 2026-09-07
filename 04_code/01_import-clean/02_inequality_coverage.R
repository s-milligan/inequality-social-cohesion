# ------------------------------------------------------------
# Inequality data coverage
# Project: Inequality and social cohesion
#
# Purpose:
#   Compare OECD IDD and SWIID disposable-income Gini
#   coverage against ESS Rounds 1–9 country × interview-year
#   structure.
# ------------------------------------------------------------

library(dplyr)
library(readr)
library(here)

# 1. Read ESS coverage ----------------------------------------------------

ess_coverage <- read_csv(
  here(
    "03_data",
    "interim",
    "ess_country_round_year_coverage.csv"
  ),
  show_col_types = FALSE
)

# Retain observations with known interview year
ess_years <- ess_coverage |>
  filter(!is.na(interview_year)) |>
  mutate(
    interview_year = as.integer(interview_year),
    lag_year = interview_year - 1
  )

# 2. ESS country crosswalk ------------------------------------------------

country_crosswalk <- tribble(
  ~cntry, ~iso3, ~swiid_country,
  "AL", "ALB", "Albania",
  "AT", "AUT", "Austria",
  "BE", "BEL", "Belgium",
  "BG", "BGR", "Bulgaria",
  "CH", "CHE", "Switzerland",
  "CY", "CYP", "Cyprus",
  "CZ", "CZE", "Czech Republic",
  "DE", "DEU", "Germany",
  "DK", "DNK", "Denmark",
  "EE", "EST", "Estonia",
  "ES", "ESP", "Spain",
  "FI", "FIN", "Finland",
  "FR", "FRA", "France",
  "GB", "GBR", "United Kingdom",
  "GR", "GRC", "Greece",
  "HR", "HRV", "Croatia",
  "HU", "HUN", "Hungary",
  "IE", "IRL", "Ireland",
  "IL", "ISR", "Israel",
  "IS", "ISL", "Iceland",
  "IT", "ITA", "Italy",
  "LT", "LTU", "Lithuania",
  "LU", "LUX", "Luxembourg",
  "LV", "LVA", "Latvia",
  "ME", "MNE", "Montenegro",
  "NL", "NLD", "Netherlands",
  "NO", "NOR", "Norway",
  "PL", "POL", "Poland",
  "PT", "PRT", "Portugal",
  "RO", "ROU", "Romania",
  "RS", "SRB", "Serbia",
  "RU", "RUS", "Russia",
  "SE", "SWE", "Sweden",
  "SI", "SVN", "Slovenia",
  "SK", "SVK", "Slovakia",
  "TR", "TUR", "Turkey",
  "UA", "UKR", "Ukraine",
  "XK", "XKX", "Kosovo"
)

ess_years <- ess_years |>
  left_join(country_crosswalk, by = "cntry")

# 3. Import OECD IDD ------------------------------------------------------

oecd_file <- list.files(
  here("03_data", "raw", "inequality", "oecd"),
  pattern = "\\.csv$",
  full.names = TRUE
)

stopifnot(length(oecd_file) == 1)

oecd_raw <- read_csv(
  oecd_file,
  show_col_types = FALSE
)

# Disposable-income Gini, total population,
# current definition
oecd_gini <- oecd_raw |>
  filter(
    MEASURE == "INC_DISP_GINI",
    AGE == "_T",
    DEFINITION == "D_CUR"
  ) |>
  transmute(
    iso3 = REF_AREA,
    year = as.integer(TIME_PERIOD),
    gini_oecd = as.numeric(OBS_VALUE),
    methodology = METHODOLOGY
  )

# Some country-years occur under both methodology definitions.
# Prefer the newer methodology where both are available.
oecd_gini <- oecd_gini |>
  mutate(
    methodology_order = case_when(
      methodology == "METH2012" ~ 1L,
      methodology == "METH2011" ~ 2L,
      TRUE ~ 3L
    )
  ) |>
  arrange(iso3, year, methodology_order) |>
  distinct(iso3, year, .keep_all = TRUE) |>
  select(-methodology_order)

# Convert OECD 0–1 Gini to 0–100 for comparability with SWIID
oecd_gini <- oecd_gini |>
  mutate(
    gini_oecd = gini_oecd * 100
  )

# 4. Import SWIID ---------------------------------------------------------

swiid_file <- list.files(
  here("03_data", "raw", "inequality", "swiid"),
  pattern = "summary\\.csv$",
  full.names = TRUE
)

stopifnot(length(swiid_file) == 1)

swiid <- read_csv(
  swiid_file,
  show_col_types = FALSE
) |>
  select(
    country,
    year,
    gini_disp,
    gini_disp_se
  )

# 5. Match contemporaneous inequality ------------------------------------

coverage <- ess_years |>
  left_join(
    oecd_gini |>
      select(
        iso3,
        year,
        gini_oecd_contemp = gini_oecd
      ),
    by = c(
      "iso3",
      "interview_year" = "year"
    )
  ) |>
  left_join(
    swiid |>
      select(
        swiid_country = country,
        year,
        gini_swiid_contemp = gini_disp
      ),
    by = c(
      "swiid_country",
      "interview_year" = "year"
    )
  )

# 6. Match one-year-lagged inequality ------------------------------------

coverage <- coverage |>
  left_join(
    oecd_gini |>
      select(
        iso3,
        year,
        gini_oecd_lag1 = gini_oecd
      ),
    by = c(
      "iso3",
      "lag_year" = "year"
    )
  ) |>
  left_join(
    swiid |>
      select(
        swiid_country = country,
        year,
        gini_swiid_lag1 = gini_disp,
        gini_swiid_lag1_se = gini_disp_se
      ),
    by = c(
      "swiid_country",
      "lag_year" = "year"
    )
  )

# 7. Coverage indicators --------------------------------------------------

coverage <- coverage |>
  mutate(
    oecd_contemp = !is.na(gini_oecd_contemp),
    oecd_lag1 = !is.na(gini_oecd_lag1),
    swiid_contemp = !is.na(gini_swiid_contemp),
    swiid_lag1 = !is.na(gini_swiid_lag1)
  )

# 8. Interview-year-cell coverage ----------------------------------------

coverage_summary <- tibble(
  source = c(
    "OECD contemporaneous",
    "OECD lagged 1 year",
    "SWIID contemporaneous",
    "SWIID lagged 1 year"
  ),
  cells_covered = c(
    sum(coverage$oecd_contemp),
    sum(coverage$oecd_lag1),
    sum(coverage$swiid_contemp),
    sum(coverage$swiid_lag1)
  ),
  total_cells = nrow(coverage),
  respondents_covered = c(
    sum(coverage$n_respondents[coverage$oecd_contemp]),
    sum(coverage$n_respondents[coverage$oecd_lag1]),
    sum(coverage$n_respondents[coverage$swiid_contemp]),
    sum(coverage$n_respondents[coverage$swiid_lag1])
  ),
  total_respondents = sum(coverage$n_respondents)
) |>
  mutate(
    respondent_coverage =
      respondents_covered / total_respondents
  )

print(coverage_summary)

# 9. Country-round coverage ----------------------------------------------

country_round_coverage <- coverage |>
  group_by(cntry, essround) |>
  summarise(
    n_year_cells = n(),
    n_respondents = sum(n_respondents),

    oecd_contemp_all =
      all(oecd_contemp),

    oecd_lag1_all =
      all(oecd_lag1),

    swiid_contemp_all =
      all(swiid_contemp),

    swiid_lag1_all =
      all(swiid_lag1),

    .groups = "drop"
  )

print(
  country_round_coverage |>
    summarise(
      country_rounds = n(),
      oecd_contemp_complete =
        sum(oecd_contemp_all),
      oecd_lag1_complete =
        sum(oecd_lag1_all),
      swiid_contemp_complete =
        sum(swiid_contemp_all),
      swiid_lag1_complete =
        sum(swiid_lag1_all)
    )
)

# 10. Coverage by country -------------------------------------------------

country_coverage <- coverage |>
  group_by(cntry) |>
  summarise(
    ess_rounds = n_distinct(essround),
    interview_year_cells = n(),
    respondents = sum(n_respondents),

    oecd_lag1_cells =
      sum(oecd_lag1),

    swiid_lag1_cells =
      sum(swiid_lag1),

    .groups = "drop"
  ) |>
  mutate(
    oecd_lag1_share =
      oecd_lag1_cells / interview_year_cells,

    swiid_lag1_share =
      swiid_lag1_cells / interview_year_cells
  ) |>
  arrange(oecd_lag1_share, cntry)

print(country_coverage, n = Inf)

# 11. Save local diagnostic outputs --------------------------------------

write_csv(
  coverage_summary,
  here(
    "03_data",
    "interim",
    "inequality_coverage_summary.csv"
  )
)

write_csv(
  country_coverage,
  here(
    "03_data",
    "interim",
    "inequality_coverage_by_country.csv"
  )
)

write_csv(
  coverage,
  here(
    "03_data",
    "interim",
    "ess_inequality_coverage.csv"
  )
)