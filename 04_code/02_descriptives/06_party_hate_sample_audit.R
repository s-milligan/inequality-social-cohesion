# Audit exclusions from party hate-speech moderation samples
#
# Prerequisite:
#   04_code/03_models/06_party_hate_moderation.R
#
# Outputs:
#   05_output/tables/party_hate_sample_audit/

library(dplyr)
library(here)
library(readr)
library(tidyr)

keys <- c("cntry", "essround")

check_keys <- function(dat, label) {
  key_data <- as.data.frame(dat[, keys, drop = FALSE])
  if (anyNA(key_data) || anyDuplicated(key_data)) {
    stop(label, ": missing or duplicated country-round keys.")
  }
}

# 1. Import ---------------------------------------------------------------

ess <- readRDS(
  here("03_data", "processed", "ess_analysis_r1_r11.rds")
) |>
  select(
    cntry, essround, gini_swiid_lag1,
    ppltrst, age, education_years, gender
  )

stopifnot(all(ess$essround %in% 1:11))

gender_values <- unique(na.omit(as.character(ess$gender)))
if (!all(gender_values %in% c("Man", "Woman"))) {
  stop("Unexpected gender coding.")
}

gini_context <- ess |>
  distinct(cntry, essround, gini_swiid_lag1)

check_keys(gini_context, "Gini context")

macro <- read_csv(
  here("03_data", "interim", "ess_country_round_macro_r1_r11.csv"),
  show_col_types = FALSE
) |>
  select(cntry, essround, log_gdp_pc_ppp_lag1, unemployment_lag1) |>
  mutate(macro_row_found = TRUE)

discourse <- read_csv(
  here("03_data", "interim", "ess_country_round_discourse_r1_r11.csv"),
  show_col_types = FALSE
) |>
  select(cntry, essround, party_hate_lag1) |>
  mutate(discourse_row_found = TRUE)

check_keys(macro, "Macro context")
check_keys(discourse, "Discourse context")

# 2. Construct country-round audit ----------------------------------------

counts <- ess |>
  mutate(
    individual_complete =
      is.finite(ppltrst) &
      is.finite(age) &
      is.finite(education_years) &
      as.character(gender) %in% c("Man", "Woman")
  ) |>
  group_by(cntry, essround) |>
  summarise(
    n_respondents = n(),
    n_individual_complete = sum(individual_complete),
    .groups = "drop"
  )

audit <- counts |>
  left_join(gini_context, by = keys) |>
  left_join(macro, by = keys) |>
  left_join(discourse, by = keys) |>
  mutate(
    macro_row_found = coalesce(macro_row_found, FALSE),
    discourse_row_found = coalesce(discourse_row_found, FALSE),
    missing_gini = !is.finite(gini_swiid_lag1),
    missing_hate = !is.finite(party_hate_lag1),
    missing_gdp = !is.finite(log_gdp_pc_ppp_lag1),
    missing_unemployment = !is.finite(unemployment_lag1),
    context_complete = !(
      missing_gini | missing_hate |
        missing_gdp | missing_unemployment
    ),
    n_model_respondents = if_else(
      context_complete, n_individual_complete, 0L
    )
  )

audit_by_period <- bind_rows(
  audit |>
    filter(essround <= 9) |>
    mutate(sample = "R1-R9", .before = 1),
  audit |>
    mutate(sample = "R1-R11", .before = 1)
)

reason_names <- c(
  missing_gini = "gini_swiid_lag1",
  missing_hate = "party_hate_lag1",
  missing_gdp = "log_gdp_pc_ppp_lag1",
  missing_unemployment = "unemployment_lag1"
)

missing_detail <- audit_by_period |>
  select(
    sample, cntry, essround, n_respondents,
    all_of(names(reason_names))
  ) |>
  pivot_longer(
    cols = all_of(names(reason_names)),
    names_to = "reason",
    values_to = "missing"
  ) |>
  filter(missing) |>
  mutate(variable = unname(reason_names[reason]))

missing_labels <- missing_detail |>
  group_by(sample, cntry, essround) |>
  summarise(
    missing_variables = paste(variable, collapse = "; "),
    .groups = "drop"
  )

excluded_country_rounds <- audit_by_period |>
  filter(!context_complete) |>
  left_join(
    missing_labels,
    by = c("sample", "cntry", "essround")
  ) |>
  select(
    sample, cntry, essround, n_respondents,
    missing_variables, macro_row_found, discourse_row_found
  ) |>
  arrange(sample, cntry, essround)

# Counts across reasons can overlap.
missing_by_variable <- missing_detail |>
  group_by(sample, variable) |>
  summarise(
    n_country_rounds = n(),
    n_respondents = sum(n_respondents),
    .groups = "drop"
  )

sample_check <- audit_by_period |>
  group_by(sample) |>
  summarise(
    n_context_excluded = sum(!context_complete),
    n_respondents_context_excluded =
      sum(n_respondents[!context_complete]),
    n_respondents_reconstructed = sum(n_model_respondents),
    n_countries_reconstructed =
      n_distinct(cntry[n_model_respondents > 0]),
    n_country_rounds_reconstructed =
      sum(n_model_respondents > 0),
    .groups = "drop"
  )

# 3. Compare with saved sample counts -------------------------------------

original_table_dir <- here(
  "05_output", "tables", "party_hate_moderation"
)

saved_counts <- bind_rows(
  read_csv(
    file.path(original_table_dir, "r1_r9_sample_flow.csv"),
    show_col_types = FALSE
  ),
  read_csv(
    file.path(original_table_dir, "r1_r11_sample_flow.csv"),
    show_col_types = FALSE
  )
) |>
  filter(stage == "Common model sample") |>
  transmute(
    sample,
    n_respondents_saved = n_respondents,
    n_countries_saved = n_countries,
    n_country_rounds_saved = n_country_rounds
  )

sample_check <- sample_check |>
  left_join(saved_counts, by = "sample") |>
  mutate(
    matches_saved_counts =
      n_respondents_reconstructed == n_respondents_saved &
      n_countries_reconstructed == n_countries_saved &
      n_country_rounds_reconstructed == n_country_rounds_saved
  )

# Also compare the identities of included country-rounds.
saved_keys <- bind_rows(
  read_csv(
    file.path(original_table_dir, "r1_r9_context.csv"),
    show_col_types = FALSE
  ) |> select(sample, cntry, essround),
  read_csv(
    file.path(original_table_dir, "r1_r11_context.csv"),
    show_col_types = FALSE
  ) |> select(sample, cntry, essround)
)

reconstructed_keys <- audit_by_period |>
  filter(n_model_respondents > 0) |>
  select(sample, cntry, essround)

key_differences <- bind_rows(
  anti_join(
    reconstructed_keys, saved_keys,
    by = c("sample", "cntry", "essround")
  ) |> mutate(discrepancy = "Reconstructed only"),
  anti_join(
    saved_keys, reconstructed_keys,
    by = c("sample", "cntry", "essround")
  ) |> mutate(discrepancy = "Saved only")
)

# 4. Save and print -------------------------------------------------------

output_dir <- here("05_output", "tables", "party_hate_sample_audit")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(audit_by_period, file.path(output_dir, "country_round_audit.csv"))
write_csv(
  excluded_country_rounds,
  file.path(output_dir, "excluded_country_rounds.csv")
)
write_csv(
  missing_by_variable,
  file.path(output_dir, "missing_by_variable.csv")
)
write_csv(sample_check, file.path(output_dir, "sample_check.csv"))
write_csv(key_differences, file.path(output_dir, "key_differences.csv"))

cat("\nExcluded country-rounds:\n")
print(excluded_country_rounds, n = Inf, width = Inf)

cat("\nMissing variables (reasons can overlap):\n")
print(missing_by_variable, n = Inf, width = Inf)

cat("\nReconstructed versus saved samples:\n")
print(sample_check, n = Inf, width = Inf)

cat("\nCountry-round key discrepancies:\n")
print(key_differences, n = Inf, width = Inf)

if (
  !isTRUE(all(sample_check$matches_saved_counts)) ||
  nrow(key_differences) > 0
) {
  stop("Audit does not reproduce the saved samples; resolve before proceeding.")
}

