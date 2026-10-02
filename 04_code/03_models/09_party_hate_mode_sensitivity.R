# In-person interview sensitivity: ESS R1-R11
#
# Prerequisite:
#   04_code/03_models/06_party_hate_moderation.R
#
# Compare existing all-mode M4-M7 with in-person-only M4-M7.
# Retain lagged Gini, hate speech and macro controls.
# Recalculate decompositions on contributing country-rounds.
# Retain original country-round exposure values.
#
# Four additional fits. Outputs remain under 05_output.

library(dplyr)
library(here)
library(lme4)
library(readr)
library(tibble)
library(tidyr)

keys <- c("cntry", "essround")

raw_context_vars <- c(
  "gini_swiid_lag1", "party_hate_lag1",
  "log_gdp_pc_ppp_lag1", "unemployment_lag1"
)

original_model_dir <- here(
  "05_output", "models", "party_hate_moderation"
)
original_table_dir <- here(
  "05_output", "tables", "party_hate_moderation"
)
model_dir <- here(
  "05_output", "models", "party_hate_mode_sensitivity"
)
table_dir <- here(
  "05_output", "tables", "party_hate_mode_sensitivity"
)

dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

# 1. Reconstruct the original complete-case sample ------------------------

ess <- readRDS(
  here("03_data", "processed", "ess_analysis_r1_r11.rds")
) |>
  select(
    cntry, essround, idno, ppltrst, age,
    education_years, gender, interview_mode
  )

stopifnot(
  all(ess$essround %in% 1:11),
  !anyNA(ess[c("cntry", "essround", "idno")]),
  !anyDuplicated(ess[c("cntry", "essround", "idno")])
)

gender_values <- unique(na.omit(as.character(ess$gender)))
if (!all(gender_values %in% c("Man", "Woman"))) {
  stop("Unexpected gender coding.")
}

context <- read_csv(
  file.path(original_table_dir, "r1_r11_context.csv"),
  show_col_types = FALSE
) |>
  select(all_of(c(keys, raw_context_vars)))

stopifnot(
  !anyNA(context[keys]),
  !anyDuplicated(context[keys]),
  all(vapply(
    context[raw_context_vars],
    function(x) all(is.finite(x)),
    logical(1)
  ))
)

# The saved context contains only contributing country-rounds.
dat <- ess |>
  inner_join(context, by = keys) |>
  filter(
    is.finite(ppltrst),
    is.finite(age),
    is.finite(education_years),
    as.character(gender) %in% c("Man", "Woman")
  ) |>
  mutate(
    interview_mode = coalesce(
      as.character(interview_mode), "Not available"
    )
  )

# 2. Sample preparation ---------------------------------------------------

prepare_sample <- function(dat) {
  contextual <- dat |>
    distinct(across(all_of(c(keys, raw_context_vars))))

  stopifnot(!anyDuplicated(contextual[keys]))

  variable_map <- c(
    gini = "gini_swiid_lag1",
    hate = "party_hate_lag1",
    gdp = "log_gdp_pc_ppp_lag1",
    unemployment = "unemployment_lag1"
  )

  for (prefix in names(variable_map)) {
    raw_name <- unname(variable_map[[prefix]])
    mean_name <- paste0(prefix, "_mean")
    within_name <- paste0(prefix, "_w")
    between_name <- paste0(prefix, "_b")

    contextual <- contextual |>
      group_by(cntry) |>
      mutate(
        !!mean_name := mean(.data[[raw_name]]),
        !!within_name := .data[[raw_name]] - .data[[mean_name]]
      ) |>
      ungroup()

    grand_mean <- contextual |>
      distinct(cntry, .data[[mean_name]]) |>
      summarise(value = mean(.data[[mean_name]])) |>
      pull(value)

    contextual[[between_name]] <-
      contextual[[mean_name]] - grand_mean
  }

  contextual <- contextual |>
    mutate(gh_product = gini_w * hate_w) |>
    group_by(cntry) |>
    mutate(
      gh_mean = mean(gh_product),
      gh_w = gh_product - gh_mean
    ) |>
    ungroup()

  product_grand_mean <- contextual |>
    distinct(cntry, gh_mean) |>
    summarise(value = mean(gh_mean)) |>
    pull(value)

  contextual <- contextual |>
    mutate(
      gh_b = gh_mean - product_grand_mean,
      gini_w_hate_b = gini_w * hate_b,
      gdp_w_10 = gdp_w * 10,
      gdp_b_10 = gdp_b * 10
    )

  model_dat <- dat |>
    left_join(
      contextual |> select(-all_of(raw_context_vars)),
      by = keys
    ) |>
    mutate(
      cntry = factor(cntry),
      country_round = interaction(cntry, essround, drop = TRUE),
      round_factor = factor(essround),
      gender = factor(as.character(gender), levels = c("Man", "Woman")),
      age_decade = (age - mean(age)) / 10,
      age_decade2 = age_decade^2,
      education_c = education_years - mean(education_years)
    )

  list(data = model_dat, context = contextual)
}

# Verify the reconstructed model variables against the saved M6 frame.
reference_fit <- readRDS(
  file.path(original_model_dir, "r1_r11_M6.rds")
)
reference_frame <- model.frame(reference_fit)
reconstructed <- prepare_sample(dat)

stopifnot(nrow(reconstructed$data) == nrow(reference_frame))

for (variable in names(reference_frame)) {
  expected <- reference_frame[[variable]]
  actual <- reconstructed$data[[variable]]

  if (is.null(actual)) {
    stop("Missing reconstructed model variable: ", variable)
  }

  agrees <- if (is.factor(expected) || is.character(expected)) {
    identical(as.character(actual), as.character(expected))
  } else {
    isTRUE(all.equal(
      as.numeric(actual),
      as.numeric(expected),
      tolerance = 1e-8
    ))
  }

  if (!agrees) {
    stop("Reconstruction differs from saved M6: ", variable)
  }
}

cat("\nOriginal M6 model frame successfully reproduced.\n")

rm(reference_fit, reference_frame, reconstructed)
invisible(gc())

# 3. Describe the restriction ---------------------------------------------

mode_counts <- dat |>
  count(essround, interview_mode, name = "n_respondents") |>
  group_by(essround) |>
  mutate(share = n_respondents / sum(n_respondents)) |>
  ungroup() |>
  mutate(retained = interview_mode == "In-person")

inperson <- dat |>
  filter(interview_mode == "In-person")

stopifnot(nrow(inperson) > 0)

count_sample <- function(x, label) {
  tibble(
    sample = label,
    n_respondents = nrow(x),
    n_countries = n_distinct(x$cntry),
    n_country_rounds = nrow(distinct(x, cntry, essround))
  )
}

sample_summary <- bind_rows(
  count_sample(dat, "All modes"),
  count_sample(inperson, "In-person")
)

country_round_retention <- dat |>
  group_by(cntry, essround) |>
  summarise(
    n_original = n(),
    n_inperson = sum(interview_mode == "In-person"),
    n_excluded = n_original - n_inperson,
    .groups = "drop"
  )

lost_country_rounds <- country_round_retention |>
  filter(n_inperson == 0)

write_csv(mode_counts, file.path(table_dir, "mode_counts.csv"))
write_csv(sample_summary, file.path(table_dir, "sample_summary.csv"))
write_csv(
  country_round_retention,
  file.path(table_dir, "country_round_retention.csv")
)
write_csv(
  lost_country_rounds,
  file.path(table_dir, "lost_country_rounds.csv")
)

prepared <- prepare_sample(inperson)

write_csv(
  prepared$context,
  file.path(table_dir, "inperson_context.csv")
)

# 4. Fit in-person models and extract comparisons --------------------------

control <- lmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 200000),
  check.rankX = "stop.deficient"
)

extract_fixed <- function(fit, label, model_name) {
  b <- fixef(fit)
  se <- sqrt(diag(vcov(fit)))

  tibble(
    sample = label,
    model = model_name,
    term = names(b),
    estimate = unname(b),
    std_error = unname(se),
    conf_low = estimate - 1.96 * std_error,
    conf_high = estimate + 1.96 * std_error
  )
}

fixed_rows <- list()
diagnostic_rows <- list()

for (model_name in c("M4", "M5", "M6", "M7")) {
  original_fit <- readRDS(
    file.path(original_model_dir, paste0("r1_r11_", model_name, ".rds"))
  )

  fixed_rows[[paste0(model_name, "_all")]] <- extract_fixed(
    original_fit, "All modes", model_name
  )

  model_formula <- formula(original_fit)
  environment(model_formula) <- environment()

  fit_warnings <- character()
  cat("\nFitting in-person ", model_name, "...\n", sep = "")

  fit <- withCallingHandlers(
    lmer(
      formula = model_formula,
      data = prepared$data,
      REML = FALSE,
      na.action = na.fail,
      control = control
    ),
    warning = function(w) {
      fit_warnings <<- c(fit_warnings, conditionMessage(w))
    }
  )

  stopifnot(nobs(fit) == nrow(inperson))

  saveRDS(
    fit,
    file.path(model_dir, paste0("r1_r11_inperson_", model_name, ".rds"))
  )

  fixed_rows[[paste0(model_name, "_inperson")]] <- extract_fixed(
    fit, "In-person", model_name
  )

  diagnostic_rows[[model_name]] <- tibble(
    sample = "In-person",
    model = model_name,
    n = nobs(fit),
    singular = isSingular(fit, tol = 1e-4),
    optimizer_code = paste(fit@optinfo$conv$opt, collapse = "; "),
    convergence_messages = paste(
      fit@optinfo$conv$lme4$messages,
      collapse = "; "
    ),
    warnings = paste(unique(fit_warnings), collapse = "; ")
  )

  write_csv(
    bind_rows(fixed_rows),
    file.path(table_dir, "fixed_effects.csv")
  )
  write_csv(
    bind_rows(diagnostic_rows),
    file.path(table_dir, "model_diagnostics.csv")
  )

  rm(fit, original_fit)
  invisible(gc())
}

# 5. Save and print focused results ---------------------------------------

model_diagnostics <- bind_rows(diagnostic_rows)

focused_results <- bind_rows(fixed_rows) |>
  filter(
    term %in% c(
      "gini_w", "gini_b", "hate_w", "hate_b",
      "gh_w", "gh_b", "gini_w_hate_b"
    )
  )

coefficient_comparison <- focused_results |>
  select(model, term, sample, estimate) |>
  pivot_wider(names_from = sample, values_from = estimate) |>
  mutate(
    inperson_minus_all = `In-person` - `All modes`
  )

write_csv(focused_results, file.path(table_dir, "focused_results.csv"))
write_csv(
  coefficient_comparison,
  file.path(table_dir, "coefficient_comparison.csv")
)
writeLines(
  capture.output(sessionInfo()),
  file.path(table_dir, "session_info.txt")
)

cat("\nMode distribution within the original model sample, R10-R11:\n")
print(
  mode_counts |> filter(essround >= 10),
  n = Inf, width = Inf
)

cat("\nSample comparison:\n")
print(sample_summary, n = Inf, width = Inf)

cat("\nCountry-rounds lost entirely:\n")
print(lost_country_rounds, n = Inf, width = Inf)

cat("\nIn-person model diagnostics:\n")
print(model_diagnostics, n = Inf, width = Inf)

cat("\nFocused coefficients:\n")
print(focused_results, n = Inf, width = Inf)

cat("\nDescriptive coefficient comparison:\n")
print(coefficient_comparison, n = Inf, width = Inf)

