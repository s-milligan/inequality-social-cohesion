# Trace contextual exclusions to saved annual cells and current local sources.
# Also inspect original ESS timing and weighting fields. No model refitting.
# Run from the project in a fresh R session; outputs stay in 05_output/.
library(dplyr)
library(tidyr)
library(readr)
library(haven)
library(here)

out <- here("05_output", "tables", "context_source_audit")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
read_local <- function(name) {
  read_csv(here("03_data", "interim", name), show_col_types = FALSE)
}
save_table <- function(x, name) write_csv(x, file.path(out, paste0(name, ".csv")))
unique_keys <- function(x, keys) {
  if (anyNA(x[keys]) || anyDuplicated(x[keys])) stop("Invalid keys: ", paste(keys, collapse = ", "))
  x
}
one_file <- function(x, label) {
  if (length(x) != 1L) stop(label, ": expected one file; found ", length(x), ". Check local source versions.")
  x
}
keys <- c("cntry", "essround")
targets <- read_csv(
  here("05_output", "tables", "party_hate_sample_audit", "excluded_country_rounds.csv"),
  show_col_types = FALSE
) |>
  filter(sample == "R1-R11") |>
  select(cntry, essround, missing_variables) |>
  unique_keys(keys)

# 1. Saved annual cells: retain all dated components of excluded rounds.
base <- read_local("ess_macro_year_coverage.csv") |>
  transmute(cntry, essround, interview_year, lag_year, interview_year_share,
            swiid_country, iso3, gini = gini_swiid_lag1,
            unemployment = unemployment_lag1)
post <- read_local("ess_post9_context_year_coverage.csv") |>
  transmute(cntry, essround, interview_year, lag_year, interview_year_share,
            swiid_country, iso3 = wb_iso3, gini = gini_swiid_lag1_year,
            unemployment = unemployment_lag1_year)
annual <- bind_rows(base, post) |>
  unique_keys(c(keys, "interview_year"))
cells <- targets |>
  left_join(annual, by = keys) |>
  pivot_longer(c(gini, unemployment), names_to = "variable", values_to = "saved_value") |>
  mutate(required_source_year = interview_year - 1)
stopifnot(all(cells$required_source_year == cells$lag_year, na.rm = TRUE))
save_table(cells, "saved_annual_cells")

# 2. Read the local source files used by the construction pipeline.
# A present country-year row containing NA differs from an absent row.
swiid_dir <- here("03_data", "raw", "inequality", "swiid")
rda_files <- list.files(swiid_dir, "\\.(rda|rdata)$", recursive = TRUE,
                        full.names = TRUE, ignore.case = TRUE)
if (length(rda_files) > 1L) {
  rda_files <- rda_files[grepl("swiid", basename(rda_files), ignore.case = TRUE)]
}
rda_file <- one_file(rda_files, "SWIID RData")
swiid_env <- new.env()
load(rda_file, envir = swiid_env)
if (!exists("swiid_summary", envir = swiid_env, inherits = FALSE)) stop("No swiid_summary object.")
sw <- swiid_env$swiid_summary
if (is.list(sw) && "draws_summary" %in% names(sw)) sw <- sw$draws_summary
sw <- sw |>
  transmute(source_country = as.character(country), year = as.integer(year),
            value = as.numeric(gini_disp)) |>
  unique_keys(c("source_country", "year"))

# Baseline uses a CSV summary; later rounds use RData. Keep both explicit.
csv_file <- one_file(list.files(swiid_dir, "summary\\.csv$", full.names = TRUE), "SWIID CSV")
sw_base <- read_csv(csv_file, show_col_types = FALSE) |>
  transmute(source_country = as.character(country), year = as.integer(year),
            value = as.numeric(gini_disp)) |>
  unique_keys(c("source_country", "year"))

wb_files <- list.files(here("03_data", "raw", "macro", "world_bank"),
                       "\\.csv$", recursive = TRUE, full.names = TRUE)
wb_file <- one_file(wb_files[
  grepl("^API_", basename(wb_files)) &
    grepl("SL.UEM.TOTL.ZS", basename(wb_files), fixed = TRUE)
], "World Bank unemployment")
wb <- read_csv(wb_file, skip = 4, show_col_types = FALSE) |>
  filter(`Indicator Code` == "SL.UEM.TOTL.ZS") |>
  select(source_country = `Country Code`, matches("^[0-9]{4}$")) |>
  pivot_longer(-source_country, names_to = "year", values_to = "value") |>
  mutate(year = as.integer(year), value = as.numeric(value)) |>
  unique_keys(c("source_country", "year"))

sources <- bind_rows(
  mutate(sw_base, source = "SWIID CSV"),
  mutate(sw, source = "SWIID RData"),
  mutate(wb, source = "World Bank unemployment")
)
ranges <- sources |>
  group_by(source, source_country) |>
  summarise(n_finite_years = sum(is.finite(value)),
            first_available = if (any(is.finite(value))) min(year[is.finite(value)]) else NA_integer_,
            last_available = if (any(is.finite(value))) max(year[is.finite(value)]) else NA_integer_,
            .groups = "drop")
trace <- cells |>
  mutate(source = case_when(variable == "unemployment" ~ "World Bank unemployment",
                            essround <= 9 ~ "SWIID CSV", TRUE ~ "SWIID RData"),
         source_country = if_else(variable == "gini", swiid_country, iso3)) |>
  left_join(mutate(sources, source_row_found = TRUE),
            by = c("source", "source_country", "required_source_year" = "year")) |>
  left_join(ranges, by = c("source", "source_country")) |>
  mutate(status = case_when(
    is.na(interview_year) ~ "No saved dated cell: inspect raw timing",
    is.na(source_country) ~ "Missing country mapping",
    is.na(n_finite_years) ~ "Mapped country absent from local source",
    is.finite(value) & !is.finite(saved_value) ~ "Source available but saved value missing: investigate pipeline/version",
    is.finite(value) & is.finite(saved_value) & abs(value - saved_value) > 1e-8 ~ "Source differs from saved value: investigate version/pipeline",
    is.finite(value) ~ "Source and saved value agree",
    n_finite_years == 0 ~ "No finite values for country in local source",
    required_source_year > last_available ~ "Beyond last available source year",
    required_source_year < first_available ~ "Before first available source year",
    !coalesce(source_row_found, FALSE) ~ "Country-year row absent within available span",
    TRUE ~ "Missing value within available span"
  )) |>
  arrange(cntry, essround, variable, interview_year)
save_table(trace, "source_trace")
print(trace |> select(cntry, essround, variable, interview_year, required_source_year,
                       interview_year_share, saved_value, value, last_available, status),
      n = Inf, width = Inf)

# Check agreement between the two SWIID inputs on overlapping country-years.
sw_compare <- inner_join(sw_base, sw, by = c("source_country", "year"), suffix = c("_csv", "_rdata")) |>
  filter(is.finite(value_csv) != is.finite(value_rdata) |
           (is.finite(value_csv) & is.finite(value_rdata) & abs(value_csv - value_rdata) > 1e-8))
save_table(sw_compare, "swiid_source_disagreements")

# 3. Inspect raw timing and weight fields, without recoding source data.
ess_files <- c(
  here(
    "03_data", "raw", "ess",
    paste0(
      "ESS1e06_7-ESS2e03_6-ESS3e03_7-ESS4e04_6-",
      "ESS5e03_6-ESS6e02_7-ESS7e02_3-ESS8e02_3-",
      "ESS9e03_3-subset.dta"
    )
  ),
  here("03_data", "raw", "ess", "post9", "ess10_f2f.dta"),
  here("03_data", "raw", "ess", "post9", "ess11_f2f.dta")
)

stopifnot(all(file.exists(ess_files)))

fields <- c("cntry", "essround", "inwyr", "inwyys", "inwyye",
            "dweight", "pspwght", "pweight", "anweight", "psu", "stratum", "prob")
get_num <- function(x, field) {
  if (field %in% names(x)) as.numeric(zap_labels(x[[field]])) else rep(NA_real_, nrow(x))
}
positive <- function(x) is.finite(x) & x > 0
raw_checks <- lapply(ess_files, function(f) {
  x <- read_dta(f, col_select = any_of(fields))
  inventory <- tibble(file = basename(f), variable = fields, present = fields %in% names(x))
  z <- tibble(cntry = as.character(x$cntry), essround = get_num(x, "essround"),
              inwyr = get_num(x, "inwyr"), inwyys = get_num(x, "inwyys"),
              inwyye = get_num(x, "inwyye"))
  z$raw_coalesced_year <- coalesce(z$inwyys, z$inwyr)
  # Reproduce the baseline rule only to identify records to investigate.
  z$baseline_rule_discards <- z$essround <= 9 &
    (is.na(z$raw_coalesced_year) | z$raw_coalesced_year < 2002 | z$raw_coalesced_year > 2019)
  timing <- z |>
    filter(baseline_rule_discards | (cntry == "EE" & essround == 5)) |>
    count(cntry, essround, inwyr, inwyys, inwyye, raw_coalesced_year, name = "n_respondents")
  weights <- bind_rows(lapply(c("dweight", "pspwght", "pweight", "anweight"), function(v) {
    tibble(cntry = z$cntry, essround = z$essround, variable = v, value = get_num(x, v))
  })) |>
    group_by(cntry, essround, variable) |>
    summarise(n_respondents = n(), n_positive_finite = sum(positive(value)),
              minimum = if (any(positive(value))) min(value[positive(value)]) else NA_real_,
              maximum = if (any(positive(value))) max(value[positive(value)]) else NA_real_,
              .groups = "drop")
  list(inventory = inventory, timing = timing, weights = weights)
})
for (name in c("inventory", "timing", "weights")) {
  save_table(bind_rows(lapply(raw_checks, function(x) x[[name]])), paste0("raw_ess_", name))
}
cat("\nRaw timing requiring inspection (not automatically valid dates):\n")
print(bind_rows(lapply(raw_checks, function(x) x$timing)), n = Inf, width = Inf)
cat("\nWeight/source-field inventory (raw samples, not model samples):\n")
print(bind_rows(lapply(raw_checks, function(x) x$inventory)), n = Inf, width = Inf)

# Record exactly which CURRENT files were audited; this cannot establish
# which historical versions generated the saved intermediate tables.
input_files <- c(csv_file, rda_file, wb_file, ess_files,
                 here("03_data", "interim", c("ess_macro_year_coverage.csv", "ess_post9_context_year_coverage.csv")))
save_table(tibble(file = input_files, md5 = unname(tools::md5sum(input_files)),
                  modified = as.character(file.info(input_files)$mtime)), "input_manifest")
writeLines(capture.output(sessionInfo()), file.path(out, "session_info.txt"))
cat("\nAudit outputs saved in:", out, "\n")


# Final contextual-data exclusion audit -------------------------------
#
# Purpose:
#   Create a compact, human-readable record of the country-rounds
#   excluded from the contextual model sample because one or more
#   required contextual variables could not be constructed.
#
# This table synthesises the preceding automated source and timing
# checks. It is intended for audit/documentation purposes only.
# It does NOT determine the analytical sample or perform any additional
# filtering.
#
# The exclusions fall into two broad categories:
#
#   1. ESS timing limitation
#      Estonia Round 5 has no usable respondent interview-year
#      information. Because contextual exposures are constructed from
#      actual interview-year shares rather than nominal ESS round years,
#      no timing-weighted contextual measures can be constructed.
#
#   2. External source-data limitations
#      For the remaining country-rounds, the required interview timing
#      is known, but the relevant lagged SWIID Gini and/or World Bank
#      unemployment observation is unavailable.
#
# No interpolation, extrapolation, or renormalisation over only the
# available interview years is used to recover these observations.


context_exclusion_audit <- tribble(
  
  ~cntry,
  ~essround,
  ~n_respondents,
  ~missing_context,
  ~reason,
  
  # Estonia R5:
  # No usable interview-year information is available for the country-round.
  "EE", 5, 1793,
  "Gini; GDP; unemployment; party hate",
  "No usable respondent interview-year information",
  
  # Israel R11:
  # The 2024 portion of fieldwork requires a 2023 SWIID value,
  # which is unavailable.
  "IL", 11, 906,
  "Gini",
  "SWIID unavailable for required 2023 exposure year",
  
  # Iceland R10:
  # Fieldwork requires 2020 and 2021 SWIID values, which are unavailable.
  "IS", 10, 903,
  "Gini",
  "SWIID unavailable for required 2020/2021 exposure years",
  
  # Iceland R11:
  # 2024 interviews require a 2023 SWIID value.
  "IS", 11, 842,
  "Gini",
  "SWIID unavailable for required 2023 exposure year",
  
  # Montenegro R11:
  # 2024 interviews require a 2023 SWIID value.
  "ME", 11, 1609,
  "Gini",
  "SWIID unavailable for required 2023 exposure year",
  
  # Ukraine R11:
  # 2024 interviews require 2023 Gini and unemployment values,
  # neither of which is available in the relevant source series.
  "UA", 11, 2661,
  "Gini; unemployment",
  "SWIID and WDI unemployment unavailable for required 2023 exposure year",
  
  # Kosovo R6:
  # The local WDI unemployment source contains no finite values
  # for Kosovo.
  "XK", 6, 1295,
  "Unemployment",
  "No finite Kosovo unemployment observations in WDI source"
)


cat(
  "\nFinal contextual-data exclusion audit:\n"
)

print(
  context_exclusion_audit,
  n = Inf,
  width = Inf
)


write_csv(
  context_exclusion_audit,
  file.path(
    output_dir,
    "context_exclusion_audit.csv"
  )
)

