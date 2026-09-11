library(haven)
library(dplyr)

# Load existing analysis dataset
ess_analysis <- readRDS(
  "03_data/processed/ess_analysis_r1_r11.rds"
)

# Load cohesion / migration extract
cohesion_migration <- read_dta(
  "03_data/raw/ess/ess_rounds1-11_cohesion_migration_variables.dta"
)

# Keep only identifiers and new variables
cohesion_migration_add <- cohesion_migration |>
  select(
    essround,
    cntry,
    idno,

    # Horizontal cohesion
    sclmeet,
    sclact,
    inmdisc,

    # Discrimination / exclusion
    dscrgrp,
    dscretn,
    dscrntn,
    dscrrce,
    dscrrlg,

    # Migration / minority background
    blgetmg,
    brncntr,
    ctzcntr,
    livecntr,
    facntr,
    mocntr,
    facntn,
    mocntn
  )

# Check uniqueness of merge key
duplicate_keys <- cohesion_migration_add |>
  count(essround, cntry, idno) |>
  filter(n > 1)

stopifnot(nrow(duplicate_keys) == 0)

# Check that all respondents in the analysis data have a match
unmatched <- ess_analysis |>
  anti_join(
    cohesion_migration_add,
    by = c("essround", "cntry", "idno")
  )

stopifnot(nrow(unmatched) == 0)

# Merge
n_before <- nrow(ess_analysis)

ess_analysis_extended <- ess_analysis |>
  left_join(
    cohesion_migration_add,
    by = c("essround", "cntry", "idno")
  )

stopifnot(nrow(ess_analysis_extended) == n_before)

# Save extended analysis dataset
saveRDS(
  ess_analysis_extended,
  "03_data/processed/ess_analysis_r1_r11_cohesion_migration.rds"
)
