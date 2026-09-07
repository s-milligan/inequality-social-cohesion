# ------------------------------------------------------------
# ESS coverage
# Project: Inequality and social cohesion
#
# Purpose:
#   Import the ESS Rounds 1–9 extract and establish
#   country × round × interview-year coverage.
# ------------------------------------------------------------

library(dplyr)
library(haven)
library(here)
library(readr)

# 1. Locate source file ----------------------------------------------------

ess_files <- list.files(
  here("03_data", "raw", "ess"),
  pattern = "\\.dta$",
  full.names = TRUE
)

if (length(ess_files) != 1) {
  stop(
    "Expected exactly one .dta file in 03_data/raw/ess; found ",
    length(ess_files)
  )
}

# 2. Import ---------------------------------------------------------------

ess <- read_dta(ess_files)

cat("\nRows:", nrow(ess), "\n")
cat("Columns:", ncol(ess), "\n\n")

# 3. Check requested variables -------------------------------------------

expected_vars <- c(
  # identifiers/timing
  "essround", "cntry", "idno",
  "inwyr", "inwyys", "inwmm", "inwmms",

  # trust
  "ppltrst", "pplfair", "pplhlp",

  # demographics
  "agea", "gndr", "eduyrs",

  # labour-force status
  "pdwrk", "edctn", "uempla", "uempli",
  "dsbld", "rtrd", "cmsrv", "hswrk",
  "dngoth", "mainact", "mnactic",

  # economic position
  "hincfel", "hinctnt", "hinctnta",

  # migration/background
  "brncntr", "ctzcntr",

  # weights
  "dweight", "pspwght", "pweight", "anweight"
)

variable_check <- data.frame(
  variable = expected_vars,
  present = expected_vars %in% names(ess)
)

print(variable_check, row.names = FALSE)

cat("\nVariables not present:\n")
print(variable_check$variable[!variable_check$present])

# 4. Inspect rounds -------------------------------------------------------

cat("\nCases by ESS round:\n")
print(table(ess$essround, useNA = "ifany"))

# 5. Harmonise interview year/month --------------------------------------

# ESS timing-variable names changed across rounds.
# INWYYS denotes start-of-interview year in later rounds.

year_new <- if ("inwyys" %in% names(ess)) {
  as.numeric(ess$inwyys)
} else {
  rep(NA_real_, nrow(ess))
}

year_old <- if ("inwyr" %in% names(ess)) {
  as.numeric(ess$inwyr)
} else {
  rep(NA_real_, nrow(ess))
}

month_new <- if ("inwmms" %in% names(ess)) {
  as.numeric(ess$inwmms)
} else {
  rep(NA_real_, nrow(ess))
}

month_old <- if ("inwmm" %in% names(ess)) {
  as.numeric(ess$inwmm)
} else {
  rep(NA_real_, nrow(ess))
}

ess <- ess |>
  mutate(
    interview_year = coalesce(year_new, year_old),
    interview_month = coalesce(month_new, month_old),

    # Values outside the ESS1–9 period are treated as invalid here
    interview_year = if_else(
      interview_year >= 2002 & interview_year <= 2019,
      interview_year,
      NA_real_
    ),

    interview_month = if_else(
      interview_month >= 1 & interview_month <= 12,
      interview_month,
      NA_real_
    )
  )

# 6. Country × round × interview-year coverage ---------------------------

ess_coverage <- ess |>
  count(
    cntry,
    essround,
    interview_year,
    name = "n_respondents"
  ) |>
  arrange(cntry, essround, interview_year)

print(ess_coverage, n = Inf)

# 7. Country × round summary ---------------------------------------------

ess_country_round <- ess |>
  group_by(cntry, essround) |>
  summarise(
    n_respondents = n(),
    first_year = if (all(is.na(interview_year))) {
      NA_real_
    } else {
      min(interview_year, na.rm = TRUE)
    },
    last_year = if (all(is.na(interview_year))) {
      NA_real_
    } else {
      max(interview_year, na.rm = TRUE)
    },
    n_interview_years = n_distinct(interview_year, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(cntry, essround)

# Flag rounds whose fieldwork crosses calendar years
ess_country_round <- ess_country_round |>
  mutate(
    crosses_year = n_interview_years > 1
  )

print(ess_country_round, n = Inf)

# 8. Save local diagnostic tables ----------------------------------------

dir.create(
  here("03_data", "interim"),
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  ess_coverage,
  here(
    "03_data",
    "interim",
    "ess_country_round_year_coverage.csv"
  )
)

write_csv(
  ess_country_round,
  here(
    "03_data",
    "interim",
    "ess_country_round_coverage.csv"
  )
)
