# ------------------------------------------------------------
# ESS post-Round-9 coverage
# Project: Inequality and social cohesion
#
# Purpose:
#   Inspect ESS Round 10 face-to-face and Round 11
#   face-to-face data before extending the main analysis.
#
#   Establish:
#     - country coverage;
#     - interview-year coverage;
#     - variable availability;
#     - trust-item missingness;
#     - continuity with ESS Round 9;
#     - country-round fieldwork spanning calendar years.
#
# No analytical recoding or modelling is performed here.
# ------------------------------------------------------------

library(dplyr)
library(haven)
library(here)
library(readr)
library(tidyr)

# 1. File locations -------------------------------------------------------

post9_dir <- here(
  "03_data",
  "raw",
  "ess",
  "post9"
)

r10_file <- here(
  "03_data",
  "raw",
  "ess",
  "post9",
  "ess10_f2f.dta"
)

r11_file <- here(
  "03_data",
  "raw",
  "ess",
  "post9",
  "ess11_f2f.dta"
)

stopifnot(
  file.exists(r10_file),
  file.exists(r11_file)
)

# 2. Import ---------------------------------------------------------------

ess10_raw <- read_dta(r10_file)
ess11_raw <- read_dta(r11_file)

cat(
  "\nRound 10:",
  nrow(ess10_raw),
  "rows ×",
  ncol(ess10_raw),
  "columns\n"
)

cat(
  "Round 11:",
  nrow(ess11_raw),
  "rows ×",
  ncol(ess11_raw),
  "columns\n\n"
)

# 3. Expected variables ---------------------------------------------------

expected_vars <- c(
  # identifiers and timing
  "essround",
  "cntry",
  "idno",
  "inwyys",
  "inwmms",
  "mode",

  # trust
  "ppltrst",
  "pplfair",
  "pplhlp",

  # demographics
  "agea",
  "gndr",
  "eduyrs",

  # labour-force status
  "pdwrk",
  "edctn",
  "uempla",
  "uempli",
  "dsbld",
  "rtrd",
  "cmsrv",
  "hswrk",
  "dngoth",
  "mainact",
  "mnactic",

  # household economic position
  "hincfel",
  "hinctnt",
  "hinctnta",

  # background
  "brncntr",
  "ctzcntr",

  # weights
  "dweight",
  "pspwght",
  "pweight",
  "anweight"
)

variable_availability <- bind_rows(
  tibble(
    essround = 10,
    variable = expected_vars,
    present =
      expected_vars %in%
      names(ess10_raw)
  ),

  tibble(
    essround = 11,
    variable = expected_vars,
    present =
      expected_vars %in%
      names(ess11_raw)
  )
)

cat(
  "\nVariable availability:\n"
)

print(
  variable_availability,
  n = Inf
)

cat(
  "\nVariables missing in Round 10:\n"
)

print(
  variable_availability |>
    filter(
      essround == 10,
      !present
    ) |>
    pull(variable)
)

cat(
  "\nVariables missing in Round 11:\n"
)

print(
  variable_availability |>
    filter(
      essround == 11,
      !present
    ) |>
    pull(variable)
)


# 4. Helper functions -----------------------------------------------------

num <- function(x) {
  as.numeric(
    zap_labels(x)
  )
}

get_numeric <- function(
  dat,
  variable
) {

  if (
    variable %in%
    names(dat)
  ) {
    num(
      dat[[variable]]
    )
  } else {
    rep(
      NA_real_,
      nrow(dat)
    )
  }
}

get_character <- function(
  dat,
  variable
) {

  if (
    variable %in%
    names(dat)
  ) {
    as.character(
      dat[[variable]]
    )
  } else {
    rep(
      NA_character_,
      nrow(dat)
    )
  }
}

get_labelled_factor <- function(
  dat,
  variable
) {

  if (
    variable %in%
    names(dat)
  ) {
    as.character(
      as_factor(
        dat[[variable]]
      )
    )
  } else {
    rep(
      NA_character_,
      nrow(dat)
    )
  }
}

# 5. Standardise diagnostic variables ------------------------------------

prepare_post9 <- function(
  dat,
  round_number
) {

  tibble(
    essround =
      get_numeric(
        dat,
        "essround"
      ),

    cntry =
      get_character(
        dat,
        "cntry"
      ),

    idno =
      get_numeric(
        dat,
        "idno"
      ),

    interview_year =
      get_numeric(
        dat,
        "inwyys"
      ),

    interview_month =
      get_numeric(
        dat,
        "inwmms"
      ),

    mode =
      get_labelled_factor(
        dat,
        "mode"
      ),

    ppltrst =
      get_numeric(
        dat,
        "ppltrst"
      ),

    pplfair =
      get_numeric(
        dat,
        "pplfair"
      ),

    pplhlp =
      get_numeric(
        dat,
        "pplhlp"
      )
  ) |>
    mutate(
      # If ESSROUND is unexpectedly absent,
      # retain the known source round.
      essround =
        if_else(
          is.na(essround),
          as.numeric(round_number),
          essround
        ),

      source_round =
        round_number,

      interview_year =
        if_else(
          interview_year >= 2020 &
            interview_year <= 2025,
          interview_year,
          NA_real_
        ),

      interview_month =
        if_else(
          interview_month >= 1 &
            interview_month <= 12,
          interview_month,
          NA_real_
        ),

      ppltrst =
        if_else(
          ppltrst >= 0 &
            ppltrst <= 10,
          ppltrst,
          NA_real_
        ),

      pplfair =
        if_else(
          pplfair >= 0 &
            pplfair <= 10,
          pplfair,
          NA_real_
        ),

      pplhlp =
        if_else(
          pplhlp >= 0 &
            pplhlp <= 10,
          pplhlp,
          NA_real_
        )
    )
}

ess10 <- prepare_post9(
  ess10_raw,
  10
)

ess11 <- prepare_post9(
  ess11_raw,
  11
)

post9 <- bind_rows(
  ess10,
  ess11
)

# 6. Basic round diagnostics ---------------------------------------------

cat(
  "\nCases by round:\n"
)

print(
  post9 |>
    count(
      essround,
      name = "n_respondents"
    )
)

cat(
  "\nCountries by round:\n"
)

print(
  post9 |>
    distinct(
      essround,
      cntry
    ) |>
    count(
      essround,
      name = "n_countries"
    )
)

# 7. Inspect mode ---------------------------------------------------------

cat(
  "\nMode values by round:\n"
)

print(
  post9 |>
    count(
      essround,
      mode,
      name = "n"
    ) |>
    arrange(
      essround,
      desc(n)
    ),
  n = Inf
)

# 8. Country × round × interview-year coverage ---------------------------

post9_year_coverage <- post9 |>
  count(
    cntry,
    essround,
    interview_year,
    name = "n_respondents"
  ) |>
  arrange(
    cntry,
    essround,
    interview_year
  )

cat(
  "\nCountry × round × interview-year coverage:\n"
)

print(
  post9_year_coverage,
  n = Inf
)

# 9. Country-round summary ------------------------------------------------

post9_country_round <- post9 |>
  group_by(
    cntry,
    essround
  ) |>
  summarise(
    n_respondents =
      n(),

    n_with_interview_year =
      sum(
        !is.na(
          interview_year
        )
      ),

    missing_interview_year =
      sum(
        is.na(
          interview_year
        )
      ),

    missing_interview_year_share =
      mean(
        is.na(
          interview_year
        )
      ),

    first_year =
      if (
        all(
          is.na(
            interview_year
          )
        )
      ) {
        NA_real_
      } else {
        min(
          interview_year,
          na.rm = TRUE
        )
      },

    last_year =
      if (
        all(
          is.na(
            interview_year
          )
        )
      ) {
        NA_real_
      } else {
        max(
          interview_year,
          na.rm = TRUE
        )
      },

    n_interview_years =
      n_distinct(
        interview_year,
        na.rm = TRUE
      ),

    mean_trust =
      mean(
        ppltrst,
        na.rm = TRUE
      ),

    trust_missing_share =
      mean(
        is.na(
          ppltrst
        )
      ),

    .groups = "drop"
  ) |>
  mutate(
    crosses_year =
      n_interview_years > 1
  ) |>
  arrange(
    cntry,
    essround
  )

cat(
  "\nCountry-round summary:\n"
)

print(
  post9_country_round,
  n = Inf
)

# 10. Overall trust-item availability ------------------------------------

trust_availability <- post9 |>
  group_by(
    essround
  ) |>
  summarise(
    n =
      n(),

    ppltrst_valid =
      sum(
        !is.na(
          ppltrst
        )
      ),

    ppltrst_missing_share =
      mean(
        is.na(
          ppltrst
        )
      ),

    pplfair_valid =
      sum(
        !is.na(
          pplfair
        )
      ),

    pplfair_missing_share =
      mean(
        is.na(
          pplfair
        )
      ),

    pplhlp_valid =
      sum(
        !is.na(
          pplhlp
        )
      ),

    pplhlp_missing_share =
      mean(
        is.na(
          pplhlp
        )
      ),

    .groups = "drop"
  )

cat(
  "\nTrust-item availability:\n"
)

print(
  trust_availability
)

# 11. Continuity with Rounds 1-9 -----------------------------------------

baseline_coverage <- read_csv(
  here(
    "03_data",
    "interim",
    "ess_country_round_coverage.csv"
  ),
  show_col_types = FALSE
)

baseline_country_rounds <- baseline_coverage |>
  distinct(
    cntry,
    essround
  )

continuity <- bind_rows(
  baseline_country_rounds |>
    filter(
      essround == 9
    ) |>
    transmute(
      cntry,
      round = "R9"
    ),

  post9_country_round |>
    filter(
      essround == 10
    ) |>
    transmute(
      cntry,
      round = "R10"
    ),

  post9_country_round |>
    filter(
      essround == 11
    ) |>
    transmute(
      cntry,
      round = "R11"
    )
) |>
  distinct() |>
  mutate(
    present = TRUE
  ) |>
  pivot_wider(
    names_from = round,
    values_from = present,
    values_fill = FALSE
  ) |>
  mutate(
    r9_r10 =
      R9 & R10,

    r9_r11 =
      R9 & R11,

    r9_r10_r11 =
      R9 & R10 & R11
  ) |>
  arrange(cntry)

cat(
  "\nContinuity with Round 9:\n"
)

print(
  continuity,
  n = Inf
)

# 12. Continuity summary --------------------------------------------------

continuity_summary <- tibble(
  criterion = c(
    "Countries in R9",
    "Countries in R10 F2F",
    "Countries in R11 F2F",
    "Countries in both R9 and R10",
    "Countries in both R9 and R11",
    "Countries in R9, R10 and R11"
  ),

  n_countries = c(
    sum(
      continuity$R9
    ),

    sum(
      continuity$R10
    ),

    sum(
      continuity$R11
    ),

    sum(
      continuity$r9_r10
    ),

    sum(
      continuity$r9_r11
    ),

    sum(
      continuity$r9_r10_r11
    )
  )
)

cat(
  "\nContinuity summary:\n"
)

print(
  continuity_summary
)

# 13. Flag potentially problematic country-rounds ------------------------

post9_issues <- post9_country_round |>
  filter(
    missing_interview_year_share > 0 |
      trust_missing_share > 0.05 |
      n_interview_years == 0
  )

cat(
  "\nCountry-rounds requiring attention:\n"
)

print(
  post9_issues,
  n = Inf
)

# 14. Save diagnostic outputs --------------------------------------------

dir.create(
  here(
    "03_data",
    "interim"
  ),
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  variable_availability,
  here(
    "03_data",
    "interim",
    "ess_post9_variable_availability.csv"
  )
)

write_csv(
  post9_year_coverage,
  here(
    "03_data",
    "interim",
    "ess_post9_country_round_year_coverage.csv"
  )
)

write_csv(
  post9_country_round,
  here(
    "03_data",
    "interim",
    "ess_post9_country_round_coverage.csv"
  )
)

write_csv(
  trust_availability,
  here(
    "03_data",
    "interim",
    "ess_post9_trust_availability.csv"
  )
)

write_csv(
  continuity,
  here(
    "03_data",
    "interim",
    "ess_post9_continuity.csv"
  )
)

write_csv(
  continuity_summary,
  here(
    "03_data",
    "interim",
    "ess_post9_continuity_summary.csv"
  )
)

write_csv(
  post9_issues,
  here(
    "03_data",
    "interim",
    "ess_post9_issues.csv"
  )
)
