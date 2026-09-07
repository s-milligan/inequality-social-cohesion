# ------------------------------------------------------------
# Clean ESS analysis data
# Project: Inequality and social cohesion
#
# Purpose:
#   Clean core ESS variables for Rounds 1–9,
#   merge country-round inequality exposure,
#   and create the first individual-level analysis file.
# ------------------------------------------------------------

library(dplyr)
library(haven)
library(here)
library(readr)

# 1. Locate and import ESS extract ----------------------------------------

ess_file <- list.files(
  here("03_data", "raw", "ess"),
  pattern = "\\.dta$",
  full.names = TRUE
)

stopifnot(length(ess_file) == 1)

ess_raw <- read_dta(ess_file)

cat(
  "\nRaw ESS file:",
  nrow(ess_raw), "rows ×",
  ncol(ess_raw), "columns\n"
)

# 2. Helper ---------------------------------------------------------------

# Remove value labels while retaining the underlying numeric values.
num <- function(x) {
  as.numeric(zap_labels(x))
}

# 3. Clean core variables -------------------------------------------------

ess <- ess_raw |>
  transmute(

    # Identifiers
    essround = num(essround),
    cntry = as.character(cntry),
    idno = num(idno),

    # Outcomes ------------------------------------------------------------
    ppltrst = {
      x <- num(ppltrst)
      if_else(x >= 0 & x <= 10, x, NA_real_)
    },

    pplfair = {
      x <- num(pplfair)
      if_else(x >= 0 & x <= 10, x, NA_real_)
    },

    pplhlp = {
      x <- num(pplhlp)
      if_else(x >= 0 & x <= 10, x, NA_real_)
    },

    # Demographics --------------------------------------------------------
    age = {
      x <- num(agea)
      if_else(x >= 15 & x <= 123, x, NA_real_)
    },

    age2 = age^2,

    gender = case_when(
      num(gndr) == 1 ~ "Man",
      num(gndr) == 2 ~ "Woman",
      TRUE ~ NA_character_
    ),

    education_years = {
      x <- num(eduyrs)

      # ESS special missing codes begin at 77.
      if_else(x >= 0 & x < 77, x, NA_real_)
    },

    # Labour-force variables retained for later harmonisation -------------
    mainact = if ("mainact" %in% names(ess_raw)) {
      num(ess_raw$mainact)
    } else {
      NA_real_
    },

    mnactic = if ("mnactic" %in% names(ess_raw)) {
      num(ess_raw$mnactic)
    } else {
      NA_real_
    },

    pdwrk = if ("pdwrk" %in% names(ess_raw)) {
      num(ess_raw$pdwrk)
    } else {
      NA_real_
    },

    edctn = if ("edctn" %in% names(ess_raw)) {
      num(ess_raw$edctn)
    } else {
      NA_real_
    },

    uempla = if ("uempla" %in% names(ess_raw)) {
      num(ess_raw$uempla)
    } else {
      NA_real_
    },

    uempli = if ("uempli" %in% names(ess_raw)) {
      num(ess_raw$uempli)
    } else {
      NA_real_
    },

    dsbld = if ("dsbld" %in% names(ess_raw)) {
      num(ess_raw$dsbld)
    } else {
      NA_real_
    },

    rtrd = if ("rtrd" %in% names(ess_raw)) {
      num(ess_raw$rtrd)
    } else {
      NA_real_
    },

    hswrk = if ("hswrk" %in% names(ess_raw)) {
      num(ess_raw$hswrk)
    } else {
      NA_real_
    },

    # Weights -------------------------------------------------------------
    dweight = if ("dweight" %in% names(ess_raw)) {
      num(ess_raw$dweight)
    } else {
      NA_real_
    },

    pspwght = if ("pspwght" %in% names(ess_raw)) {
      num(ess_raw$pspwght)
    } else {
      NA_real_
    },

    pweight = if ("pweight" %in% names(ess_raw)) {
      num(ess_raw$pweight)
    } else {
      NA_real_
    },

    anweight_raw = if ("anweight" %in% names(ess_raw)) {
      num(ess_raw$anweight)
    } else {
      NA_real_
    }
  )

# 4. Construct analysis weight -------------------------------------------

# ESS analysis weight is pspwght × pweight.
# Prefer supplied anweight where available; otherwise derive it.

ess <- ess |>
  mutate(
    analysis_weight = case_when(
      !is.na(anweight_raw) &
        anweight_raw > 0 ~ anweight_raw,

      !is.na(pspwght) &
        !is.na(pweight) &
        pspwght > 0 &
        pweight > 0 ~ pspwght * pweight,

      TRUE ~ NA_real_
    )
  )

# ESS recommends ANWEIGHT as the general-purpose analysis weight.
# Exact treatment in the multilevel model remains a design decision.

# 5. Import country-round inequality -------------------------------------

inequality <- read_csv(
  here(
    "03_data",
    "interim",
    "ess_country_round_inequality.csv"
  ),
  show_col_types = FALSE
) |>
  select(
    cntry,
    essround,
    mean_interview_year,
    gini_swiid_contemp,
    gini_swiid_lag1,
    swiid_lag1_se_mean,
    gini_oecd_contemp,
    gini_oecd_lag1
  )

# 6. Merge ---------------------------------------------------------------

ess <- ess |>
  left_join(
    inequality,
    by = c("cntry", "essround")
  )

# 7. Baseline eligibility flag -------------------------------------------

# Do not drop observations yet.
# Flag cases available for the simplest inequality–trust model.

ess <- ess |>
  mutate(
    baseline_eligible =
      !is.na(ppltrst) &
      !is.na(gini_swiid_lag1)
  )

# 8. Diagnostics ----------------------------------------------------------

cat("\nTrust outcome:\n")

print(
  ess |>
    summarise(
      n = n(),
      valid_trust = sum(!is.na(ppltrst)),
      missing_trust = sum(is.na(ppltrst)),
      mean_trust = mean(ppltrst, na.rm = TRUE),
      sd_trust = sd(ppltrst, na.rm = TRUE)
    )
)

cat("\nBaseline analysis availability:\n")

print(
  ess |>
    count(baseline_eligible)
)

# Missingness in candidate baseline variables
missingness <- ess |>
  summarise(
    across(
      c(
        ppltrst,
        age,
        gender,
        education_years,
        analysis_weight,
        gini_swiid_lag1
      ),
      ~ mean(is.na(.x))
    )
  ) |>
  tidyr::pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "missing_share"
  )

print(missingness)

# 9. Country-round descriptive trust -------------------------------------

trust_country_round <- ess |>
  group_by(
    cntry,
    essround
  ) |>
  summarise(
    n = n(),
    n_trust = sum(!is.na(ppltrst)),

    mean_trust =
      mean(ppltrst, na.rm = TRUE),

    weighted_mean_trust =
      if (
        any(
          !is.na(ppltrst) &
            !is.na(analysis_weight)
        )
      ) {
        weighted.mean(
          ppltrst,
          analysis_weight,
          na.rm = TRUE
        )
      } else {
        NA_real_
      },

    gini_swiid_lag1 =
      first(gini_swiid_lag1),

    .groups = "drop"
  )

print(trust_country_round, n = Inf)

# 10. Save local outputs --------------------------------------------------

dir.create(
  here("03_data", "processed"),
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  ess,
  here(
    "03_data",
    "processed",
    "ess_analysis.rds"
  )
)

write_csv(
  missingness,
  here(
    "03_data",
    "interim",
    "ess_analysis_missingness.csv"
  )
)

write_csv(
  trust_country_round,
  here(
    "03_data",
    "interim",
    "ess_country_round_trust.csv"
  )
)
