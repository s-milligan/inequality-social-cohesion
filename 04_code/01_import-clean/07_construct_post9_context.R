# ------------------------------------------------------------
# Construct post-Round-9 contextual exposures
# Project: Inequality and social cohesion
#
# Purpose:
#   Construct country-round contextual exposures for
#   ESS Rounds 10 and 11 using actual interview timing.
#
#   Contextual variables:
#     - SWIID disposable-income Gini
#     - World Bank GDP per capita, PPP
#     - World Bank unemployment rate
#
#   Primary specification:
#     - one-year lag
#     - annual contextual values weighted by the share of
#       respondents interviewed in each calendar year
#
#   This script does NOT yet append R10/R11 respondents to
#   the main ESS analysis dataset.
# ------------------------------------------------------------


library(dplyr)
library(here)
library(readr)
library(tidyr)


# 1. Import ESS post-R9 timing coverage ----------------------------------

year_coverage <- read_csv(
  here(
    "03_data",
    "interim",
    "ess_post9_country_round_year_coverage.csv"
  ),
  show_col_types = FALSE
)

country_round_coverage <- read_csv(
  here(
    "03_data",
    "interim",
    "ess_post9_country_round_coverage.csv"
  ),
  show_col_types = FALSE
)


# Keep only respondents with usable interview year.
#
# The very small number without an interview date are excluded
# from calculation of interview-year shares. The country-round
# contextual value will therefore be based on the observed
# fieldwork-year distribution.

year_shares <- year_coverage |>
  filter(
    !is.na(interview_year)
  ) |>
  group_by(
    cntry,
    essround
  ) |>
  mutate(
    n_known_date =
      sum(n_respondents),

    interview_year_share =
      n_respondents /
      n_known_date
  ) |>
  ungroup() |>
  left_join(
    country_round_coverage |>
      select(
        cntry,
        essround,
        n_total = n_respondents,
        missing_interview_year
      ),
    by = c(
      "cntry",
      "essround"
    )
  ) |>
  mutate(
    known_date_share =
      n_known_date /
      n_total,

    lag_year =
      interview_year - 1
  )


cat(
  "\nPost-R9 country × interview-year cells:",
  nrow(year_shares),
  "\n"
)

cat(
  "Country-rounds:",
  n_distinct(
    interaction(
      year_shares$cntry,
      year_shares$essround
    )
  ),
  "\n\n"
)


# Check that year shares sum to 1 -----------------------------------------

share_check <- year_shares |>
  group_by(
    cntry,
    essround
  ) |>
  summarise(
    share_sum =
      sum(interview_year_share),

    .groups = "drop"
  )

stopifnot(
  all(
    abs(
      share_check$share_sum - 1
    ) < 1e-10
  )
)


# 2. ESS -> SWIID country mapping ----------------------------------------

# Reuse the mapping already established during the R1-R9
# inequality-coverage analysis.

swiid_crosswalk <- read_csv(
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
    !is.na(swiid_country)
  ) |>
  distinct()


# Add post-R9 country not present in the R1-R9 crosswalk.

swiid_crosswalk <- bind_rows(
  swiid_crosswalk,
  tibble(
    cntry = "MK",
    swiid_country = "North Macedonia"
  )
) |>
  distinct()


# Verify one SWIID country per ESS country.

duplicate_swiid_map <- swiid_crosswalk |>
  count(cntry) |>
  filter(n > 1)

if (
  nrow(duplicate_swiid_map) > 0
) {
  stop(
    "Some ESS countries have multiple SWIID mappings."
  )
}


# Check that every post-R9 ESS country can be mapped.

unmatched_swiid <- year_shares |>
  distinct(cntry) |>
  anti_join(
    swiid_crosswalk,
    by = "cntry"
  )

if (
  nrow(unmatched_swiid) > 0
) {
  
  print(unmatched_swiid)
  
  stop(
    "Post-R9 ESS countries missing from SWIID crosswalk."
  )
}


year_shares <- year_shares |>
  left_join(
    swiid_crosswalk,
    by = "cntry"
  )

# 3. Import SWIID summary data -------------------------------------------

swiid_dir <- here(
  "03_data",
  "raw",
  "inequality",
  "swiid"
)

swiid_files <- list.files(
  swiid_dir,
  pattern = "\\.(rda|rdata)$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)


if (
  length(swiid_files) == 0
) {
  stop(
    "No SWIID .rda/.RData file found."
  )
}


# If there is more than one RDA file, prefer one with SWIID
# in the filename.

if (
  length(swiid_files) > 1
) {

  candidate <-
    swiid_files[
      grepl(
        "swiid",
        basename(swiid_files),
        ignore.case = TRUE
      )
    ]

  if (
    length(candidate) == 1
  ) {
    swiid_file <- candidate
  } else {
    stop(
      "More than one possible SWIID .rda file found."
    )
  }

} else {

  swiid_file <- swiid_files
}


cat(
  "Loading SWIID file:\n",
  swiid_file,
  "\n\n"
)


swiid_env <- new.env()

load(
  swiid_file,
  envir = swiid_env
)


if (
  !"swiid_summary" %in%
    ls(swiid_env)
) {
  stop(
    "Object 'swiid_summary' not found in SWIID file."
  )
}


swiid_summary_raw <-
  swiid_env$swiid_summary


# Handle either a direct data frame or a list containing
# draws_summary.

if (
  is.list(swiid_summary_raw) &&
    "draws_summary" %in%
      names(swiid_summary_raw)
) {

  swiid_summary_raw <-
    swiid_summary_raw$draws_summary
}


required_swiid_vars <- c(
  "country",
  "year",
  "gini_disp"
)

if (
  !all(
    required_swiid_vars %in%
      names(swiid_summary_raw)
  )
) {

  stop(
    "Expected SWIID summary variables not found."
  )
}


swiid_summary <- swiid_summary_raw |>
  transmute(
    swiid_country =
      as.character(country),

    year =
      as.integer(year),

    gini_swiid =
      as.numeric(gini_disp)
  )


# NOTE:
# gini_disp in swiid_summary is already on the conventional
# 0-100 Gini scale. The /100 conversion used for the
# individual SWIID imputation draws is NOT required here.


# 4. Attach SWIID current and lagged values -------------------------------

swiid_current <- swiid_summary |>
  transmute(
    swiid_country,
    interview_year = year,
    gini_swiid_current_year =
      gini_swiid
  )


swiid_lag1 <- swiid_summary |>
  transmute(
    swiid_country,
    lag_year = year,
    gini_swiid_lag1_year =
      gini_swiid
  )


annual_context <- year_shares |>
  left_join(
    swiid_current,
    by = c(
      "swiid_country",
      "interview_year"
    )
  ) |>
  left_join(
    swiid_lag1,
    by = c(
      "swiid_country",
      "lag_year"
    )
  )


# 5. ESS -> World Bank ISO3 mapping --------------------------------------

# ESS country codes are largely ISO2. Use an explicit mapping so
# the construction does not depend on another package.

wb_crosswalk <- tribble(
  ~cntry, ~wb_iso3,
  "AL", "ALB",
  "AT", "AUT",
  "BE", "BEL",
  "BG", "BGR",
  "CH", "CHE",
  "CY", "CYP",
  "CZ", "CZE",
  "DE", "DEU",
  "DK", "DNK",
  "EE", "EST",
  "ES", "ESP",
  "FI", "FIN",
  "FR", "FRA",
  "GB", "GBR",
  "GR", "GRC",
  "HR", "HRV",
  "HU", "HUN",
  "IE", "IRL",
  "IL", "ISR",
  "IS", "ISL",
  "IT", "ITA",
  "LT", "LTU",
  "LU", "LUX",
  "LV", "LVA",
  "ME", "MNE",
  "MK", "MKD",
  "NL", "NLD",
  "NO", "NOR",
  "PL", "POL",
  "PT", "PRT",
  "RO", "ROU",
  "RS", "SRB",
  "RU", "RUS",
  "SE", "SWE",
  "SI", "SVN",
  "SK", "SVK",
  "TR", "TUR",
  "UA", "UKR",
  "XK", "XKX"
)


unmatched_wb <- annual_context |>
  distinct(cntry) |>
  anti_join(
    wb_crosswalk,
    by = "cntry"
  )


if (
  nrow(unmatched_wb) > 0
) {

  print(unmatched_wb)

  stop(
    "Post-R9 ESS countries missing from World Bank crosswalk."
  )
}


annual_context <- annual_context |>
  left_join(
    wb_crosswalk,
    by = "cntry"
  )


# 6. Import World Bank indicators ----------------------------------------

wb_dir <- here(
  "03_data",
  "raw",
  "macro",
  "world_bank"
)


wb_files <- list.files(
  wb_dir,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)


if (
  length(wb_files) == 0
) {
  stop(
    "No World Bank CSV files found."
  )
}


# Helper:
# Find and import a standard World Bank wide-format CSV by
# Indicator Code.

read_wb_indicator <- function(
  indicator_code
) {

  for (
    f in wb_files
  ) {

    test <- tryCatch(
      read_csv(
        f,
        skip = 4,
        n_max = 10,
        show_col_types = FALSE
      ),
      error = function(e) NULL
    )


    if (
      is.null(test)
    ) {
      next
    }


    year_cols <- grep(
      "^\\d{4}$",
      names(test),
      value = TRUE
    )


    if (
      "Indicator Code" %in%
        names(test) &&
        length(year_cols) > 0 &&
        indicator_code %in%
          test[["Indicator Code"]]
    ) {

      dat <- read_csv(
        f,
        skip = 4,
        show_col_types = FALSE
      )


      year_cols <- grep(
        "^\\d{4}$",
        names(dat),
        value = TRUE
      )


      cat(
        "Using World Bank file for",
        indicator_code,
        ":\n",
        f,
        "\n\n"
      )


      return(
        dat |>
          filter(
            `Indicator Code` ==
              indicator_code
          ) |>
          select(
            wb_iso3 =
              `Country Code`,
            all_of(year_cols)
          ) |>
          pivot_longer(
            cols =
              all_of(year_cols),
            names_to = "year",
            values_to = "value"
          ) |>
          mutate(
            year =
              as.integer(year),

            value =
              as.numeric(value)
          )
      )
    }
  }


  stop(
    paste(
      "Could not find World Bank indicator:",
      indicator_code
    )
  )
}


# GDP per capita, PPP
# constant 2021 international dollars

gdp <- read_wb_indicator(
  "NY.GDP.PCAP.PP.KD"
) |>
  mutate(
    log_gdp =
      if_else(
        value > 0,
        log(value),
        NA_real_
      )
  ) |>
  select(
    wb_iso3,
    year,
    log_gdp
  )


# Unemployment, total
# modeled ILO estimate (% of total labour force)

unemployment <- read_wb_indicator(
  "SL.UEM.TOTL.ZS"
) |>
  rename(
    unemployment =
      value
  ) |>
  select(
    wb_iso3,
    year,
    unemployment
  )


# 7. Attach current and lagged macro data --------------------------------

gdp_current <- gdp |>
  transmute(
    wb_iso3,
    interview_year = year,
    log_gdp_current_year =
      log_gdp
  )


gdp_lag1 <- gdp |>
  transmute(
    wb_iso3,
    lag_year = year,
    log_gdp_lag1_year =
      log_gdp
  )


unemployment_current <- unemployment |>
  transmute(
    wb_iso3,
    interview_year = year,
    unemployment_current_year =
      unemployment
  )


unemployment_lag1 <- unemployment |>
  transmute(
    wb_iso3,
    lag_year = year,
    unemployment_lag1_year =
      unemployment
  )


annual_context <- annual_context |>
  left_join(
    gdp_current,
    by = c(
      "wb_iso3",
      "interview_year"
    )
  ) |>
  left_join(
    gdp_lag1,
    by = c(
      "wb_iso3",
      "lag_year"
    )
  ) |>
  left_join(
    unemployment_current,
    by = c(
      "wb_iso3",
      "interview_year"
    )
  ) |>
  left_join(
    unemployment_lag1,
    by = c(
      "wb_iso3",
      "lag_year"
    )
  )


# 8. Annual-cell coverage diagnostics ------------------------------------

annual_coverage_row <- function(
  label,
  variable
) {

  x <-
    annual_context[[variable]]


  tibble(
    measure = label,

    n_cells =
      length(x),

    covered_cells =
      sum(
        !is.na(x)
      ),

    coverage_share =
      mean(
        !is.na(x)
      )
  )
}


annual_coverage_summary <- bind_rows(
  annual_coverage_row(
    "SWIID Gini current",
    "gini_swiid_current_year"
  ),

  annual_coverage_row(
    "SWIID Gini lag1",
    "gini_swiid_lag1_year"
  ),

  annual_coverage_row(
    "Log GDP current",
    "log_gdp_current_year"
  ),

  annual_coverage_row(
    "Log GDP lag1",
    "log_gdp_lag1_year"
  ),

  annual_coverage_row(
    "Unemployment current",
    "unemployment_current_year"
  ),

  annual_coverage_row(
    "Unemployment lag1",
    "unemployment_lag1_year"
  )
)


cat(
  "\nAnnual contextual-data coverage:\n"
)

print(
  annual_coverage_summary
)


# 9. Construct country-round contextual exposures ------------------------

# Require complete annual coverage across all interview years in a
# country-round. Do not construct a weighted country-round value from
# only a subset of fieldwork years.

weighted_if_complete <- function(
  x,
  w
) {

  if (
    all(
      !is.na(x)
    )
  ) {

    sum(
      x * w
    )

  } else {

    NA_real_
  }
}


post9_context <- annual_context |>
  group_by(
    cntry,
    essround
  ) |>
  summarise(
    n_respondents =
      first(n_total),

    n_known_date =
      first(n_known_date),

    known_date_share =
      first(known_date_share),

    first_interview_year =
      min(interview_year),

    last_interview_year =
      max(interview_year),

    n_interview_years =
      n_distinct(interview_year),

    # SWIID
    gini_swiid_current =
      weighted_if_complete(
        gini_swiid_current_year,
        interview_year_share
      ),

    gini_swiid_lag1 =
      weighted_if_complete(
        gini_swiid_lag1_year,
        interview_year_share
      ),

    # GDP
    log_gdp_pc_ppp_current =
      weighted_if_complete(
        log_gdp_current_year,
        interview_year_share
      ),

    log_gdp_pc_ppp_lag1 =
      weighted_if_complete(
        log_gdp_lag1_year,
        interview_year_share
      ),

    # Unemployment
    unemployment_current =
      weighted_if_complete(
        unemployment_current_year,
        interview_year_share
      ),

    unemployment_lag1 =
      weighted_if_complete(
        unemployment_lag1_year,
        interview_year_share
      ),

    .groups = "drop"
  ) |>
  arrange(
    essround,
    cntry
  )


# 10. Country-round coverage diagnostics ---------------------------------

country_round_coverage_row <- function(
  label,
  variable
) {

  x <-
    post9_context[[variable]]

  covered <-
    !is.na(x)


  tibble(
    measure = label,

    n_country_rounds =
      nrow(post9_context),

    covered_country_rounds =
      sum(covered),

    country_round_coverage =
      mean(covered),

    n_respondents =
      sum(
        post9_context$n_respondents
      ),

    covered_respondents =
      sum(
        post9_context$n_respondents[
          covered
        ]
      ),

    respondent_coverage =
      covered_respondents /
      n_respondents
  )
}


country_round_coverage_summary <- bind_rows(
  country_round_coverage_row(
    "SWIID Gini lag1",
    "gini_swiid_lag1"
  ),

  country_round_coverage_row(
    "Log GDP lag1",
    "log_gdp_pc_ppp_lag1"
  ),

  country_round_coverage_row(
    "Unemployment lag1",
    "unemployment_lag1"
  )
)


cat(
  "\nCountry-round contextual-data coverage:\n"
)

print(
  country_round_coverage_summary
)


# 11. Complete contextual-data coverage ----------------------------------

complete_context_summary <- post9_context |>
  summarise(
    n_country_rounds =
      n(),
    
    complete_country_rounds =
      sum(
        complete_primary_context
      ),
    
    country_round_share =
      mean(
        complete_primary_context
      ),
    
    total_respondents =
      sum(
        n_respondents
      ),
    
    complete_respondents =
      sum(
        n_respondents[
          complete_primary_context
        ]
      ),
    
    respondent_share =
      complete_respondents /
      total_respondents
  )

# 12. Identify gaps -------------------------------------------------------

context_gaps <- post9_context |>
  filter(
    is.na(gini_swiid_lag1) |
      is.na(log_gdp_pc_ppp_lag1) |
      is.na(unemployment_lag1)
  ) |>
  mutate(
    missing_gini =
      is.na(gini_swiid_lag1),

    missing_gdp =
      is.na(log_gdp_pc_ppp_lag1),

    missing_unemployment =
      is.na(unemployment_lag1)
  )


cat(
  "\nCountry-rounds with missing primary contextual data:\n"
)

print(
  context_gaps,
  n = Inf
)


# 13. Inspect annual matched data -----------------------------------------

annual_context_check <- annual_context |>
  select(
    cntry,
    essround,
    interview_year,
    lag_year,
    n_respondents,
    interview_year_share,
    known_date_share,
    swiid_country,
    wb_iso3,
    gini_swiid_current_year,
    gini_swiid_lag1_year,
    log_gdp_current_year,
    log_gdp_lag1_year,
    unemployment_current_year,
    unemployment_lag1_year
  ) |>
  arrange(
    essround,
    cntry,
    interview_year
  )


# 14. Save outputs --------------------------------------------------------

dir.create(
  here(
    "03_data",
    "interim"
  ),
  recursive = TRUE,
  showWarnings = FALSE
)


write_csv(
  annual_context_check,
  here(
    "03_data",
    "interim",
    "ess_post9_context_year_coverage.csv"
  )
)


write_csv(
  post9_context,
  here(
    "03_data",
    "interim",
    "ess_post9_context.csv"
  )
)


write_csv(
  annual_coverage_summary,
  here(
    "03_data",
    "interim",
    "ess_post9_context_annual_coverage_summary.csv"
  )
)


write_csv(
  country_round_coverage_summary,
  here(
    "03_data",
    "interim",
    "ess_post9_context_country_round_coverage_summary.csv"
  )
)


write_csv(
  complete_context_summary,
  here(
    "03_data",
    "interim",
    "ess_post9_context_complete_summary.csv"
  )
)


write_csv(
  context_gaps,
  here(
    "03_data",
    "interim",
    "ess_post9_context_gaps.csv"
  )
)
