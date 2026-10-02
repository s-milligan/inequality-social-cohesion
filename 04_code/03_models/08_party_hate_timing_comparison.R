# Lagged versus contemporaneous party hate-speech exposure
#
# Prerequisites:
#   04_code/03_models/06_party_hate_moderation.R
#   04_code/02_descriptives/06_party_hate_sample_audit.R
#
# Only the timing of party hate speech changes.
# Gini, GDP and unemployment remain lagged.
#
# Within each period:
#   - Start from the saved M6 respondent sample.
#   - Require both hate-speech timing measures.
#   - Recalculate decompositions on contributing country-rounds.
#   - Fit all specifications to identical respondents.
#
# Models use unweighted ML, the original controls, and country
# and country-round random intercepts.
#
# Coefficient differences across timing specifications are
# descriptive, not formal tests of their difference.

library(dplyr)
library(here)
library(lme4)
library(readr)
library(tibble)
library(tidyr)

keys <- c("cntry", "essround")

original_model_dir <- here(
  "05_output", "models", "party_hate_moderation"
)
original_table_dir <- here(
  "05_output", "tables", "party_hate_moderation"
)

table_dir <- here(
  "05_output", "tables", "party_hate_timing_comparison"
)
model_dir <- here(
  "05_output", "models", "party_hate_timing_comparison"
)

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

check_keys <- function(dat, label) {
  key_data <- as.data.frame(dat[, keys, drop = FALSE])
  if (anyNA(key_data) || anyDuplicated(key_data)) {
    stop(label, ": missing or duplicated country-round keys.")
  }
}

# 1. Require a successful exclusion audit ---------------------------------

audit_dir <- here("05_output", "tables", "party_hate_sample_audit")

audit_check <- read_csv(
  file.path(audit_dir, "sample_check.csv"),
  show_col_types = FALSE
)
audit_key_differences <- read_csv(
  file.path(audit_dir, "key_differences.csv"),
  show_col_types = FALSE
)

stopifnot(
  nrow(audit_check) == 2L,
  all(audit_check$matches_saved_counts),
  nrow(audit_key_differences) == 0L
)

# 2. Import the alternative exposure --------------------------------------

discourse <- read_csv(
  here("03_data", "interim", "ess_country_round_discourse_r1_r11.csv"),
  show_col_types = FALSE
) |>
  select(cntry, essround, party_hate_lag1, party_hate_contemp)

check_keys(discourse, "Discourse context")

raw_context_vars <- c(
  "gini_swiid_lag1", "party_hate_lag1",
  "log_gdp_pc_ppp_lag1", "unemployment_lag1"
)

# 3. Prepare one timing specification -------------------------------------

prepare_timing <- function(dat, exposure) {
  raw_vars <- c(raw_context_vars, "party_hate_contemp")

  context <- dat |>
    distinct(across(all_of(c(keys, raw_vars))))

  check_keys(context, "Common-sample context")

  variable_map <- c(
    gini = "gini_swiid_lag1",
    hate = exposure,
    gdp = "log_gdp_pc_ppp_lag1",
    unemployment = "unemployment_lag1"
  )

  for (prefix in names(variable_map)) {
    raw_name <- unname(variable_map[[prefix]])
    mean_name <- paste0(prefix, "_mean")
    within_name <- paste0(prefix, "_w")
    between_name <- paste0(prefix, "_b")

    context <- context |>
      group_by(cntry) |>
      mutate(
        !!mean_name := mean(.data[[raw_name]]),
        !!within_name := .data[[raw_name]] - .data[[mean_name]]
      ) |>
      ungroup()

    grand_mean <- context |>
      distinct(cntry, .data[[mean_name]]) |>
      summarise(value = mean(.data[[mean_name]])) |>
      pull(value)

    context[[between_name]] <- context[[mean_name]] - grand_mean
  }

  context <- context |>
    mutate(gh_product = gini_w * hate_w) |>
    group_by(cntry) |>
    mutate(
      gh_mean = mean(gh_product),
      gh_w = gh_product - gh_mean
    ) |>
    ungroup()

  product_grand_mean <- context |>
    distinct(cntry, gh_mean) |>
    summarise(value = mean(gh_mean)) |>
    pull(value)

  context <- context |>
    mutate(
      gh_b = gh_mean - product_grand_mean,
      gini_w_hate_b = gini_w * hate_b,
      gdp_w_10 = gdp_w * 10,
      gdp_b_10 = gdp_b * 10
    )

  model_dat <- dat |>
    left_join(context |> select(-all_of(raw_vars)), by = keys) |>
    mutate(
      cntry = factor(cntry),
      country_round = interaction(cntry, essround, drop = TRUE),
      round_factor = factor(essround),
      gender = factor(as.character(gender), levels = c("Man", "Woman")),
      # Recentring the saved age-in-decades variable is equivalent
      # to centring raw age on the new sample and dividing by ten.
      age_decade = age_decade - mean(age_decade),
      age_decade2 = age_decade^2,
      education_c = education_c - mean(education_c)
    )

  list(data = model_dat, context = context)
}

# 4. Formulas and estimation settings -------------------------------------

m4 <- ppltrst ~
  gini_w + gini_b +
  gdp_w_10 + gdp_b_10 +
  unemployment_w + unemployment_b +
  round_factor +
  age_decade + age_decade2 + gender + education_c +
  (1 | cntry) + (1 | country_round)

m5 <- update(m4, . ~ . + hate_w + hate_b)
m6 <- update(m5, . ~ . + gh_w + gh_b)
m7 <- update(m6, . ~ . + gini_w_hate_b)

formulas <- list(M4 = m4, M5 = m5, M6 = m6, M7 = m7)

control <- lmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 200000),
  check.rankX = "stop.deficient"
)

periods <- c(r1_r9 = "R1-R9", r1_r11 = "R1-R11")
timings <- c(lag1 = "party_hate_lag1", contemp = "party_hate_contemp")

fixed_rows <- list()
diagnostic_rows <- list()
sample_rows <- list()
excluded_rows <- list()

# 5. Fit the comparison ---------------------------------------------------

for (file_id in names(periods)) {
  sample_id <- unname(periods[[file_id]])

  full_fit <- readRDS(
    file.path(original_model_dir, paste0(file_id, "_M6.rds"))
  )

  # Recover the actual respondents used in the original model.
  original_dat <- as_tibble(model.frame(full_fit)) |>
    transmute(
      cntry = as.character(cntry),
      essround = as.integer(as.character(round_factor)),
      ppltrst, age_decade, gender, education_c
    )

  original_context <- read_csv(
    file.path(original_table_dir, paste0(file_id, "_context.csv")),
    show_col_types = FALSE
  ) |>
    select(all_of(c(keys, raw_context_vars)))

  check_keys(original_context, "Saved original context")

  if (nrow(anti_join(
    distinct(original_dat, cntry, essround),
    original_context,
    by = keys
  )) > 0) {
    stop("Saved context does not cover the saved model sample.")
  }

  # Check that the current lagged exposure agrees with the saved one.
  context <- original_context |>
    left_join(
      discourse |>
        rename(current_party_hate_lag1 = party_hate_lag1),
      by = keys
    )

  if (
    any(!is.finite(context$current_party_hate_lag1)) ||
    any(abs(
      context$party_hate_lag1 - context$current_party_hate_lag1
    ) > 1e-8)
  ) {
    stop("Current lagged hate speech differs from the saved model context.")
  }

  context <- context |> select(-current_party_hate_lag1)

  linked <- original_dat |> left_join(context, by = keys)

  common <- linked |>
    filter(
      if_all(
        all_of(c(raw_context_vars, "party_hate_contemp")),
        ~ is.finite(.x)
      )
    )

  if (!nrow(common)) {
    stop(sample_id, ": no common timing-comparison sample.")
  }

  excluded_rows[[file_id]] <- linked |>
    filter(!is.finite(party_hate_contemp)) |>
    count(cntry, essround, name = "n_respondents") |>
    mutate(sample = sample_id, .before = 1)

  sample_rows[[file_id]] <- tibble(
    sample = sample_id,
    original_n = nrow(original_dat),
    common_n = nrow(common),
    additional_exclusions = nrow(original_dat) - nrow(common),
    n_countries = n_distinct(common$cntry),
    n_country_rounds = nrow(distinct(common, cntry, essround))
  )

  cat("\nTiming-comparison sample:\n")
  print(sample_rows[[file_id]], width = Inf)

  rm(full_fit)
  invisible(gc())

  for (timing in names(timings)) {
    prepared <- prepare_timing(
      common,
      exposure = unname(timings[[timing]])
    )

    write_csv(
      prepared$context,
      file.path(table_dir, paste0(file_id, "_", timing, "_context.csv"))
    )

    # M4 has no hate-speech terms and is identical across timings.
    model_names <- if (timing == "lag1") {
      c("M4", "M5", "M6", "M7")
    } else {
      c("M5", "M6", "M7")
    }

    for (model_name in model_names) {
      timing_label <- if (model_name == "M4") "shared" else timing
      run_id <- paste(file_id, timing_label, model_name, sep = "_")
      fit_warnings <- character()

      cat("\nFitting ", run_id, "...\n", sep = "")

      fit <- withCallingHandlers(
        lmer(
          formula = formulas[[model_name]],
          data = prepared$data,
          REML = FALSE,
          na.action = na.fail,
          control = control
        ),
        warning = function(w) {
          fit_warnings <<- c(fit_warnings, conditionMessage(w))
        }
      )

      stopifnot(nobs(fit) == nrow(common))

      saveRDS(fit, file.path(model_dir, paste0(run_id, ".rds")))

      b <- fixef(fit)
      se <- sqrt(diag(vcov(fit)))

      fixed_rows[[run_id]] <- tibble(
        sample = sample_id,
        timing = timing_label,
        model = model_name,
        term = names(b),
        estimate = unname(b),
        std_error = unname(se),
        conf_low = estimate - 1.96 * std_error,
        conf_high = estimate + 1.96 * std_error
      )

      diagnostic_rows[[run_id]] <- tibble(
        sample = sample_id,
        timing = timing_label,
        model = model_name,
        n = nobs(fit),
        log_likelihood = as.numeric(logLik(fit)),
        AIC = AIC(fit),
        BIC = BIC(fit),
        singular = isSingular(fit, tol = 1e-4),
        optimizer_code = paste(fit@optinfo$conv$opt, collapse = "; "),
        convergence_messages = paste(
          fit@optinfo$conv$lme4$messages,
          collapse = "; "
        ),
        warnings = paste(unique(fit_warnings), collapse = "; ")
      )

      # Save completed results as the run progresses.
      write_csv(
        bind_rows(fixed_rows),
        file.path(table_dir, "fixed_effects.csv")
      )
      write_csv(
        bind_rows(diagnostic_rows),
        file.path(table_dir, "model_diagnostics.csv")
      )

      rm(fit)
      invisible(gc())
    }

    rm(prepared)
    invisible(gc())
  }
}

# 6. Summarise and save ----------------------------------------------------

sample_summary <- bind_rows(sample_rows)
additional_exclusions <- bind_rows(excluded_rows)
model_diagnostics <- bind_rows(diagnostic_rows)

focused_results <- bind_rows(fixed_rows) |>
  filter(
    term %in% c(
      "gini_w", "gini_b", "hate_w", "hate_b",
      "gh_w", "gh_b", "gini_w_hate_b"
    )
  )

timing_comparison <- focused_results |>
  filter(
    model != "M4",
    term %in% c("gini_w", "hate_w", "gh_w", "gini_w_hate_b")
  ) |>
  select(sample, model, term, timing, estimate) |>
  pivot_wider(names_from = timing, values_from = estimate) |>
  mutate(contemp_minus_lag1 = contemp - lag1)

write_csv(sample_summary, file.path(table_dir, "sample_summary.csv"))
write_csv(
  additional_exclusions,
  file.path(table_dir, "additional_exclusions.csv")
)
write_csv(focused_results, file.path(table_dir, "focused_results.csv"))
write_csv(timing_comparison, file.path(table_dir, "timing_comparison.csv"))

writeLines(
  capture.output(sessionInfo()),
  file.path(table_dir, "session_info.txt")
)

cat("\nCommon timing-comparison samples:\n")
print(sample_summary, n = Inf, width = Inf)

cat("\nAdditional exclusions for contemporaneous exposure:\n")
print(additional_exclusions, n = Inf, width = Inf)

cat("\nModel diagnostics:\n")
print(model_diagnostics, n = Inf, width = Inf)

cat("\nFocused coefficients:\n")
print(focused_results, n = Inf, width = Inf)

cat("\nDescriptive timing comparison:\n")
print(timing_comparison, n = Inf, width = Inf)

