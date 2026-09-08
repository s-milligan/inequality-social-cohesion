# ------------------------------------------------------------
# Construct country-round macro controls
# Project: Inequality and social cohesion
#
# Purpose:
#   Construct GDP per capita and unemployment controls
#   matched to ESS country-rounds using actual interview timing.
#
# Indicators:
#   NY.GDP.PCAP.PP.KD  GDP per capita, PPP (constant 2021
#                      international $)
#   SL.UEM.TOTL.ZS     Unemployment, total (% of total labour
#                      force), modeled ILO estimate
# ------------------------------------------------------------

library(dplyr)
library(readr)
library(tidyr)
library(here)

# 1. Locate World Bank data files -----------------------------------------

wb_dir <- here(
  "03_data",
  "raw",
  "macro",
  "world_bank"
)

wb_files <- list.files(
  wb_dir,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

gdp_file <- wb_files[
  grepl(
    "NY.GDP.PCAP.PP.KD",
    basename(wb_files),
    fixed = TRUE
  )
]

unemp_file <- wb_files[
  grepl(
    "SL.UEM.TOTL.ZS",
    basename(wb_files),
    fixed = TRUE
  )
]

# World Bank downloads also contain metadata CSVs.
# Keep only the main API data files if necessary.

gdp_file <- gdp_file[
  grepl("^API_", basename(gdp_file))
]

unemp_file <- unemp_file[
  grepl("^API_", basename(unemp_file))
]

stopifnot(length(gdp_file) == 1)
stopifnot(length(unemp_file) == 1)

cat("\nGDP file:\n", gdp_file, "\n")
cat("\nUnemployment file:\n", unemp_file, "\n")

# 2. Helper to read World Bank wide-format files --------------------------

read_wb_indicator <- function(file, value_name) {
  
  x <- read_csv(
    file,
    skip = 4,
    show_col_types = FALSE
  )
  
  year_cols <- names(x)[
    grepl("^\\d{4}$", names(x))
  ]
  
  x |>
    select(
      iso3 = `Country Code`,
      all_of(year_cols)
    ) |>
    pivot_longer(
      cols = all_of(year_cols),
      names_to = "year",
      values_to = value_name
    ) |>
    mutate(
      year = as.integer(year)
    )
}

# 3. Import indicators ----------------------------------------------------

gdp <- read_wb_indicator(
  gdp_file,
  "gdp_pc_ppp"
)

unemployment <- read_wb_indicator(
  unemp_file,
  "unemployment"
)

macro_annual <- full_join(
  gdp,
  unemployment,
  by = c("iso3", "year")
) |>
  mutate(
    log_gdp_pc_ppp =
      if_else(
        gdp_pc_ppp > 0,
        log(gdp_pc_ppp),
        NA_real_
      )
  )

# 4. Import ESS interview-year structure ---------------------------------

coverage <- read_csv(
  here(
    "03_data",
    "interim",
    "ess_inequality_coverage.csv"
  ),
  show_col_types = FALSE
) |>
  filter(
    !is.na(interview_year)
  ) |>
  mutate(
    interview_year =
      as.integer(interview_year),
    
    lag_year =
      interview_year - 1
  )

# 5. Add interview-year shares -------------------------------------------

coverage <- coverage |>
  group_by(
    cntry,
    essround
  ) |>
  mutate(
    interview_year_share =
      n_respondents /
      sum(n_respondents)
  ) |>
  ungroup()

# 6. Match contemporaneous macro variables -------------------------------

macro_match <- coverage |>
  left_join(
    macro_annual |>
      select(
        iso3,
        year,
        gdp_pc_ppp_contemp = gdp_pc_ppp,
        log_gdp_pc_ppp_contemp = log_gdp_pc_ppp,
        unemployment_contemp = unemployment
      ),
    by = c(
      "iso3",
      "interview_year" = "year"
    )
  )

# 7. Match one-year-lagged macro variables -------------------------------

macro_match <- macro_match |>
  left_join(
    macro_annual |>
      select(
        iso3,
        year,
        gdp_pc_ppp_lag1 = gdp_pc_ppp,
        log_gdp_pc_ppp_lag1 = log_gdp_pc_ppp,
        unemployment_lag1 = unemployment
      ),
    by = c(
      "iso3",
      "lag_year" = "year"
    )
  )

# 8. Coverage diagnostics -------------------------------------------------

coverage_summary <- tibble(
  variable = c(
    "GDP contemporaneous",
    "GDP lagged 1 year",
    "Unemployment contemporaneous",
    "Unemployment lagged 1 year"
  ),
  
  cells_covered = c(
    sum(!is.na(
      macro_match$gdp_pc_ppp_contemp
    )),
    sum(!is.na(
      macro_match$gdp_pc_ppp_lag1
    )),
    sum(!is.na(
      macro_match$unemployment_contemp
    )),
    sum(!is.na(
      macro_match$unemployment_lag1
    ))
  ),
  
  total_cells =
    nrow(macro_match),
  
  respondents_covered = c(
    sum(
      macro_match$n_respondents[
        !is.na(
          macro_match$gdp_pc_ppp_contemp
        )
      ]
    ),
    
    sum(
      macro_match$n_respondents[
        !is.na(
          macro_match$gdp_pc_ppp_lag1
        )
      ]
    ),
    
    sum(
      macro_match$n_respondents[
        !is.na(
          macro_match$unemployment_contemp
        )
      ]
    ),
    
    sum(
      macro_match$n_respondents[
        !is.na(
          macro_match$unemployment_lag1
        )
      ]
    )
  ),
  
  total_respondents =
    sum(macro_match$n_respondents)
) |>
  mutate(
    respondent_coverage =
      respondents_covered /
      total_respondents
  )

cat("\nMacro coverage:\n")
print(coverage_summary)

# 9. Aggregate annual values to country-rounds ----------------------------

macro_country_round <- macro_match |>
  group_by(
    cntry,
    essround
  ) |>
  summarise(
    
    n_respondents_with_year =
      sum(n_respondents),
    
    # Require complete annual coverage across the fieldwork period
    gdp_lag1_complete =
      all(!is.na(gdp_pc_ppp_lag1)),
    
    unemployment_lag1_complete =
      all(!is.na(unemployment_lag1)),
    
    log_gdp_pc_ppp_lag1 =
      if (all(
        !is.na(log_gdp_pc_ppp_lag1)
      )) {
        weighted.mean(
          log_gdp_pc_ppp_lag1,
          interview_year_share
        )
      } else {
        NA_real_
      },
    
    unemployment_lag1 =
      if (all(
        !is.na(unemployment_lag1)
      )) {
        weighted.mean(
          unemployment_lag1,
          interview_year_share
        )
      } else {
        NA_real_
      },
    
    log_gdp_pc_ppp_contemp =
      if (all(
        !is.na(log_gdp_pc_ppp_contemp)
      )) {
        weighted.mean(
          log_gdp_pc_ppp_contemp,
          interview_year_share
        )
      } else {
        NA_real_
      },
    
    unemployment_contemp =
      if (all(
        !is.na(unemployment_contemp)
      )) {
        weighted.mean(
          unemployment_contemp,
          interview_year_share
        )
      } else {
        NA_real_
      },
    
    .groups = "drop"
  )

# 10. Restore full country-round structure --------------------------------

country_rounds <- read_csv(
  here(
    "03_data",
    "interim",
    "ess_country_round_coverage.csv"
  ),
  show_col_types = FALSE
) |>
  select(
    cntry,
    essround,
    n_respondents
  )

macro_country_round <- country_rounds |>
  left_join(
    macro_country_round,
    by = c(
      "cntry",
      "essround"
    )
  ) |>
  arrange(
    cntry,
    essround
  )

# 11. Country-round diagnostics ------------------------------------------

cat(
  "\nESS country-rounds:",
  nrow(macro_country_round),
  "\n"
)

cat(
  "Country-rounds with lagged GDP:",
  sum(
    !is.na(
      macro_country_round$
        log_gdp_pc_ppp_lag1
    )
  ),
  "\n"
)

cat(
  "Country-rounds with lagged unemployment:",
  sum(
    !is.na(
      macro_country_round$
        unemployment_lag1
    )
  ),
  "\n"
)

cat("\nMissing lagged GDP:\n")

print(
  macro_country_round |>
    filter(
      is.na(log_gdp_pc_ppp_lag1)
    ) |>
    select(
      cntry,
      essround,
      n_respondents
    )
)

cat("\nMissing lagged unemployment:\n")

print(
  macro_country_round |>
    filter(
      is.na(unemployment_lag1)
    ) |>
    select(
      cntry,
      essround,
      n_respondents
    )
)

# 12. Save local outputs --------------------------------------------------

write_csv(
  coverage_summary,
  here(
    "03_data",
    "interim",
    "macro_coverage_summary.csv"
  )
)

write_csv(
  macro_match,
  here(
    "03_data",
    "interim",
    "ess_macro_year_coverage.csv"
  )
)

write_csv(
  macro_country_round,
  here(
    "03_data",
    "interim",
    "ess_country_round_macro.csv"
  )
)
