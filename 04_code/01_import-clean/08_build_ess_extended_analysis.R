# ------------------------------------------------------------
# Build extended ESS analysis dataset: Rounds 1-11
# Project: Inequality and social cohesion
#
# Purpose:
#   Extend the cleaned R1-R9 respondent-level ESS dataset
#   with interviewer-administered ESS Rounds 10 and 11.
#
# Design:
#   - Preserve ess_analysis.rds as the R1-R9 baseline.
#   - Harmonise R10/R11 to the existing analytical structure.
#   - Attach one-year-lagged SWIID Gini.
#   - Keep macro controls in a separate country-round file.
#   - Retain interview mode for later sensitivity analyses.
#
# Outputs:
#   03_data/processed/ess_analysis_r1_r11.rds
#   03_data/interim/ess_country_round_macro_r1_r11.csv
#   03_data/interim/ess_country_round_context_r1_r11.csv
#   diagnostic CSV files
# ------------------------------------------------------------


library(dplyr)
library(haven)
library(here)
library(readr)


# 1. Helper functions -----------------------------------------------------

get_num <- function(
  dat,
  variable
) {

  if (
    variable %in%
      names(dat)
  ) {

    as.numeric(
      zap_labels(
        dat[[variable]]
      )
    )

  } else {

    rep(
      NA_real_,
      nrow(dat)
    )
  }
}


get_chr <- function(
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


clean_range <- function(
  x,
  lower,
  upper
) {

  if_else(
    !is.na(x) &
      x >= lower &
      x <= upper,
    x,
    NA_real_
  )
}


# 2. Import existing R1-R9 analysis dataset -------------------------------

ess_base_raw <- readRDS(
  here(
    "03_data",
    "processed",
    "ess_analysis.rds"
  )
)


cat(
  "\nExisting R1-R9 dataset:",
  nrow(ess_base_raw),
  "respondents\n"
)


required_base_vars <- c(
  "cntry",
  "essround",
  "idno",
  "ppltrst",
  "age",
  "gender",
  "education_years",
  "analysis_weight",
  "gini_swiid_lag1"
)


missing_base_vars <- setdiff(
  required_base_vars,
  names(ess_base_raw)
)


if (
  length(missing_base_vars) > 0
) {

  stop(
    paste(
      "Required variables missing from ess_analysis.rds:",
      paste(
        missing_base_vars,
        collapse = ", "
      )
    )
  )
}


# 3. Standardise R1-R9 structure -----------------------------------------

ess_base <- tibble(
  essround =
    as.integer(
      ess_base_raw$essround
    ),

  cntry =
    as.character(
      ess_base_raw$cntry
    ),

  idno =
    as.numeric(
      ess_base_raw$idno
    ),

  interview_year =
    if (
      "interview_year" %in%
        names(ess_base_raw)
    ) {
      as.numeric(
        ess_base_raw$interview_year
      )
    } else {
      NA_real_
    },

  interview_month =
    if (
      "interview_month" %in%
        names(ess_base_raw)
    ) {
      as.numeric(
        ess_base_raw$interview_month
      )
    } else {
      NA_real_
    },

  # Rounds 1-9 use the conventional face-to-face design.
  interview_mode =
    "In-person",

  interview_mode_raw =
    "Face-to-face",

  ppltrst =
    as.numeric(
      ess_base_raw$ppltrst
    ),

  pplfair =
    get_num(
      ess_base_raw,
      "pplfair"
    ),

  pplhlp =
    get_num(
      ess_base_raw,
      "pplhlp"
    ),

  age =
    as.numeric(
      ess_base_raw$age
    ),

  gender =
    as.character(
      ess_base_raw$gender
    ),

  education_years =
    as.numeric(
      ess_base_raw$education_years
    ),

  # Labour-force variables retained but not yet harmonised
  pdwrk =
    get_num(
      ess_base_raw,
      "pdwrk"
    ),

  edctn =
    get_num(
      ess_base_raw,
      "edctn"
    ),

  uempla =
    get_num(
      ess_base_raw,
      "uempla"
    ),

  uempli =
    get_num(
      ess_base_raw,
      "uempli"
    ),

  dsbld =
    get_num(
      ess_base_raw,
      "dsbld"
    ),

  rtrd =
    get_num(
      ess_base_raw,
      "rtrd"
    ),

  cmsrv =
    get_num(
      ess_base_raw,
      "cmsrv"
    ),

  hswrk =
    get_num(
      ess_base_raw,
      "hswrk"
    ),

  dngoth =
    get_num(
      ess_base_raw,
      "dngoth"
    ),

  mainact =
    get_num(
      ess_base_raw,
      "mainact"
    ),

  mnactic =
    get_num(
      ess_base_raw,
      "mnactic"
    ),

  # Household economic position
  hincfel =
    get_num(
      ess_base_raw,
      "hincfel"
    ),

  hinctnta =
    get_num(
      ess_base_raw,
      "hinctnta"
    ),

  # Migration/background
  brncntr =
    get_num(
      ess_base_raw,
      "brncntr"
    ),

  ctzcntr =
    get_num(
      ess_base_raw,
      "ctzcntr"
    ),

  analysis_weight =
    as.numeric(
      ess_base_raw$analysis_weight
    ),

  gini_swiid_lag1 =
    as.numeric(
      ess_base_raw$gini_swiid_lag1
    )
)


# 4. Import R10 and R11 raw data -----------------------------------------

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


ess10_raw <- read_dta(
  r10_file
)

ess11_raw <- read_dta(
  r11_file
)


cat(
  "Round 10:",
  nrow(ess10_raw),
  "respondents\n"
)

cat(
  "Round 11:",
  nrow(ess11_raw),
  "respondents\n\n"
)


# 5. Harmonise later-round respondent data -------------------------------

clean_post9 <- function(
  dat,
  round_number
) {

  # Interview date
  inwds <-
    dat[["inwds"]]


  if (
    !inherits(
      inwds,
      c(
        "POSIXct",
        "POSIXt",
        "Date"
      )
    )
  ) {

    stop(
      paste(
        "inwds is not a recognised date/time variable in Round",
        round_number
      )
    )
  }


  # Interview-mode labels
  mode_raw <- if (
    "mode" %in%
      names(dat)
  ) {

    as.character(
      as_factor(
        dat$mode
      )
    )

  } else {

    rep(
      NA_character_,
      nrow(dat)
    )
  }


  mode_standard <- case_when(
    grepl(
      "video",
      mode_raw,
      ignore.case = TRUE
    ) ~
      "Video",

    grepl(
      "face|capi|in.person|personal",
      mode_raw,
      ignore.case = TRUE
    ) ~
      "In-person",

    is.na(mode_raw) ~
      NA_character_,

    TRUE ~
      mode_raw
  )


  # Core variables
  essround <-
    get_num(
      dat,
      "essround"
    )

  cntry <-
    get_chr(
      dat,
      "cntry"
    )

  idno <-
    get_num(
      dat,
      "idno"
    )


  ppltrst <-
    clean_range(
      get_num(
        dat,
        "ppltrst"
      ),
      0,
      10
    )

  pplfair <-
    clean_range(
      get_num(
        dat,
        "pplfair"
      ),
      0,
      10
    )

  pplhlp <-
    clean_range(
      get_num(
        dat,
        "pplhlp"
      ),
      0,
      10
    )


  age <-
    clean_range(
      get_num(
        dat,
        "agea"
      ),
      15,
      120
    )


  gndr <-
    get_num(
      dat,
      "gndr"
    )

  gender <- case_when(
    gndr == 1 ~
      "Man",

    gndr == 2 ~
      "Woman",

    TRUE ~
      NA_character_
  )


  education_years <-
    clean_range(
      get_num(
        dat,
        "eduyrs"
      ),
      0,
      50
    )


  # Analysis weights:
  # use ANWEIGHT when available; otherwise reconstruct
  # from PSPWGHT × PWEIGHT.

  anweight <-
    get_num(
      dat,
      "anweight"
    )

  pspwght <-
    get_num(
      dat,
      "pspwght"
    )

  pweight <-
    get_num(
      dat,
      "pweight"
    )


  fallback_weight <-
    pspwght *
    pweight


  analysis_weight <- case_when(
    !is.na(anweight) &
      anweight > 0 ~
      anweight,

    !is.na(fallback_weight) &
      fallback_weight > 0 ~
      fallback_weight,

    TRUE ~
      NA_real_
  )


  tibble(
    essround =
      as.integer(essround),

    cntry,

    idno,

    interview_year =
      as.numeric(
        format(
          inwds,
          "%Y"
        )
      ),

    interview_month =
      as.numeric(
        format(
          inwds,
          "%m"
        )
      ),

    interview_mode =
      mode_standard,

    interview_mode_raw =
      mode_raw,

    ppltrst,
    pplfair,
    pplhlp,

    age,
    gender,
    education_years,

    # labour/activity
    pdwrk =
      get_num(
        dat,
        "pdwrk"
      ),

    edctn =
      get_num(
        dat,
        "edctn"
      ),

    uempla =
      get_num(
        dat,
        "uempla"
      ),

    uempli =
      get_num(
        dat,
        "uempli"
      ),

    dsbld =
      get_num(
        dat,
        "dsbld"
      ),

    rtrd =
      get_num(
        dat,
        "rtrd"
      ),

    cmsrv =
      get_num(
        dat,
        "cmsrv"
      ),

    hswrk =
      get_num(
        dat,
        "hswrk"
      ),

    dngoth =
      get_num(
        dat,
        "dngoth"
      ),

    mainact =
      get_num(
        dat,
        "mainact"
      ),

    mnactic =
      get_num(
        dat,
        "mnactic"
      ),

    # household economic position
    hincfel =
      get_num(
        dat,
        "hincfel"
      ),

    hinctnta =
      get_num(
        dat,
        "hinctnta"
      ),

    # migration/background
    brncntr =
      get_num(
        dat,
        "brncntr"
      ),

    ctzcntr =
      get_num(
        dat,
        "ctzcntr"
      ),

    analysis_weight
  ) |>
    mutate(
      # Protect against unexpected ESSROUND coding.
      essround =
        if_else(
          is.na(essround),
          as.integer(round_number),
          essround
        ),

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
        )
    )
}


ess10 <- clean_post9(
  ess10_raw,
  10
)

ess11 <- clean_post9(
  ess11_raw,
  11
)


ess_post9 <- bind_rows(
  ess10,
  ess11
)


# 6. Attach post-R9 SWIID exposure ---------------------------------------

post9_context <- read_csv(
  here(
    "03_data",
    "interim",
    "ess_post9_context.csv"
  ),
  show_col_types = FALSE
)


# Check uniqueness before joining.

context_duplicates <- post9_context |>
  count(
    cntry,
    essround
  ) |>
  filter(
    n > 1
  )


if (
  nrow(context_duplicates) > 0
) {

  stop(
    "Duplicate country-rounds found in ess_post9_context.csv."
  )
}


ess_post9 <- ess_post9 |>
  left_join(
    post9_context |>
      select(
        cntry,
        essround,
        gini_swiid_lag1
      ),
    by = c(
      "cntry",
      "essround"
    )
  )


# 7. Combine R1-R9 and R10-R11 -------------------------------------------

ess_extended <- bind_rows(
  ess_base,
  ess_post9
) |>
  mutate(
    post_r9 =
      essround >= 10,

    period =
      case_when(
        essround <= 9 ~
          "R1-R9",

        essround == 10 ~
          "R10",

        essround == 11 ~
          "R11",

        TRUE ~
          NA_character_
      )
  ) |>
  arrange(
    essround,
    cntry,
    idno
  )


# 8. Basic integrity checks ----------------------------------------------

# Baseline respondents must remain untouched in number.

if (
  sum(
    ess_extended$essround <= 9
  ) !=
    nrow(ess_base_raw)
) {

  stop(
    "R1-R9 respondent count changed during extension."
  )
}


# All R10/R11 respondents must survive the append.

expected_post9_n <-
  nrow(ess10_raw) +
  nrow(ess11_raw)


observed_post9_n <-
  sum(
    ess_extended$essround >= 10
  )


if (
  expected_post9_n !=
    observed_post9_n
) {

  stop(
    "R10/R11 respondent count changed during extension."
  )
}


# Respondent IDs should be unique within country × round.

duplicate_ids <- ess_extended |>
  count(
    cntry,
    essround,
    idno
  ) |>
  filter(
    n > 1
  )


if (
  nrow(duplicate_ids) > 0
) {

  print(
    duplicate_ids,
    n = Inf
  )

  stop(
    "Duplicate respondent IDs found within country-rounds."
  )
}


# 9. Build combined macro-context file -----------------------------------

macro_base <- read_csv(
  here(
    "03_data",
    "interim",
    "ess_country_round_macro.csv"
  ),
  show_col_types = FALSE
) |>
  select(
    cntry,
    essround,
    log_gdp_pc_ppp_lag1,
    unemployment_lag1
  )


macro_post9 <- post9_context |>
  select(
    cntry,
    essround,
    log_gdp_pc_ppp_lag1,
    unemployment_lag1
  )


macro_extended <- bind_rows(
  macro_base,
  macro_post9
) |>
  arrange(
    essround,
    cntry
  )


macro_duplicates <- macro_extended |>
  count(
    cntry,
    essround
  ) |>
  filter(
    n > 1
  )


if (
  nrow(macro_duplicates) > 0
) {

  print(
    macro_duplicates,
    n = Inf
  )

  stop(
    "Duplicate country-rounds found in extended macro data."
  )
}


# 10. Combined country-round context -------------------------------------

context_extended <- ess_extended |>
  distinct(
    cntry,
    essround,
    gini_swiid_lag1
  ) |>
  left_join(
    macro_extended,
    by = c(
      "cntry",
      "essround"
    )
  ) |>
  mutate(
    complete_gini =
      !is.na(
        gini_swiid_lag1
      ),

    complete_macro =
      !is.na(
        log_gdp_pc_ppp_lag1
      ) &
      !is.na(
        unemployment_lag1
      ),

    complete_primary_context =
      complete_gini &
      complete_macro
  ) |>
  arrange(
    essround,
    cntry
  )


# 11. Respondent-level diagnostics ---------------------------------------

build_summary <- ess_extended |>
  group_by(
    essround
  ) |>
  summarise(
    n_respondents =
      n(),

    n_countries =
      n_distinct(
        cntry
      ),

    trust_valid =
      sum(
        !is.na(
          ppltrst
        )
      ),

    trust_missing_share =
      mean(
        is.na(
          ppltrst
        )
      ),

    age_missing_share =
      mean(
        is.na(
          age
        )
      ),

    gender_missing_share =
      mean(
        is.na(
          gender
        )
      ),

    education_missing_share =
      mean(
        is.na(
          education_years
        )
      ),

    weight_missing_share =
      mean(
        is.na(
          analysis_weight
        )
      ),

    gini_missing_share =
      mean(
        is.na(
          gini_swiid_lag1
        )
      ),

    mean_trust =
      mean(
        ppltrst,
        na.rm = TRUE
      ),

    .groups = "drop"
  )


cat(
  "\nR1-R11 respondent-level summary:\n"
)

print(
  build_summary,
  n = Inf
)


# 12. Contextual-data diagnostics ----------------------------------------

context_summary <- context_extended |>
  mutate(
    period =
      case_when(
        essround <= 9 ~
          "R1-R9",

        essround == 10 ~
          "R10",

        essround == 11 ~
          "R11"
      )
  ) |>
  group_by(
    period
  ) |>
  summarise(
    n_country_rounds =
      n(),

    gini_complete =
      sum(
        complete_gini
      ),

    macro_complete =
      sum(
        complete_macro
      ),

    primary_context_complete =
      sum(
        complete_primary_context
      ),

    primary_context_share =
      mean(
        complete_primary_context
      ),

    .groups = "drop"
  )


cat(
  "\nR1-R11 country-round context summary:\n"
)

print(
  context_summary
)


# 13. Mode diagnostic -----------------------------------------------------

mode_summary <- ess_extended |>
  filter(
    essround >= 10
  ) |>
  count(
    essround,
    interview_mode,
    name = "n_respondents"
  ) |>
  group_by(
    essround
  ) |>
  mutate(
    share =
      n_respondents /
      sum(n_respondents)
  ) |>
  ungroup()


cat(
  "\nPost-R9 interview mode:\n"
)

print(
  mode_summary,
  n = Inf
)


# 14. Save ---------------------------------------------------------------

dir.create(
  here(
    "03_data",
    "processed"
  ),
  recursive = TRUE,
  showWarnings = FALSE
)


dir.create(
  here(
    "03_data",
    "interim"
  ),
  recursive = TRUE,
  showWarnings = FALSE
)


saveRDS(
  ess_extended,
  here(
    "03_data",
    "processed",
    "ess_analysis_r1_r11.rds"
  )
)


write_csv(
  macro_extended,
  here(
    "03_data",
    "interim",
    "ess_country_round_macro_r1_r11.csv"
  )
)


write_csv(
  context_extended,
  here(
    "03_data",
    "interim",
    "ess_country_round_context_r1_r11.csv"
  )
)


write_csv(
  build_summary,
  here(
    "03_data",
    "interim",
    "ess_r1_r11_build_summary.csv"
  )
)


write_csv(
  context_summary,
  here(
    "03_data",
    "interim",
    "ess_r1_r11_context_summary.csv"
  )
)


write_csv(
  mode_summary,
  here(
    "03_data",
    "interim",
    "ess_r1_r11_mode_summary.csv"
  )
)


cat(
  "\nExtended ESS dataset saved successfully.\n"
)

cat(
  "Total respondents:",
  nrow(ess_extended),
  "\n"
)

cat(
  "Countries:",
  n_distinct(
    ess_extended$cntry
  ),
  "\n"
)

cat(
  "Country-rounds:",
  nrow(context_extended),
  "\n"
)

