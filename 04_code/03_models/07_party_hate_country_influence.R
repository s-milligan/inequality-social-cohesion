# Country influence in party hate-speech moderation models
#
# Prerequisite:
#   Run party_hate_moderation.R first.
#
# Method:
#   Refit M5 and M6 after omitting each country in turn.
#   Retain the original analysis sample and variable centring
#   for all remaining respondents.
#
# Removing a whole country leaves the remaining countries'
# within-country means and deviations unchanged.
#
# Outputs:
#   Compact checkpoints, coefficient comparisons, diagnostics,
#   and summaries under:
#   05_output/tables/party_hate_country_influence/
#
# This is a sensitivity analysis, not a rule for excluding countries.

library(dplyr)
library(here)
library(lme4)
library(readr)
library(tibble)

# 1. Settings -------------------------------------------------------------

model_dir <- here(
  "05_output", "models", "party_hate_moderation"
)

output_dir <- here(
  "05_output", "tables", "party_hate_country_influence"
)

checkpoint_dir <- file.path(output_dir, "checkpoints")

dir.create(
  checkpoint_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# Increment this if changing the fitting or extraction procedure.
analysis_version <- "1"

periods <- c(
  r1_r9 = "R1-R9",
  r1_r11 = "R1-R11"
)

control <- lmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 200000),
  check.rankX = "stop.deficient"
)

# 2. Coefficient extraction ------------------------------------------------

extract_terms <- function(fit, terms) {
  b <- fixef(fit)
  se <- sqrt(diag(vcov(fit)))

  if (!all(terms %in% names(b))) {
    stop("A requested coefficient is missing.")
  }

  tibble(
    term = terms,
    estimate = unname(b[terms]),
    std_error = unname(se[terms]),
    conf_low = estimate - 1.96 * std_error,
    conf_high = estimate + 1.96 * std_error
  )
}

# 3. Refit after deleting one country -------------------------------------

fit_without_country <- function(
  dat, model_formula, omitted_country, terms
) {
  reduced <- dat |>
    filter(as.character(cntry) != omitted_country) |>
    droplevels()

  fit_warnings <- character()

  attempt <- tryCatch(
    withCallingHandlers(
      lmer(
        formula = model_formula,
        data = reduced,
        REML = FALSE,
        na.action = na.fail,
        control = control
      ),
      warning = function(w) {
        fit_warnings <<- c(fit_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )

  if (inherits(attempt, "error")) {
    return(list(
      coefficients = tibble(
        term = terms,
        estimate = NA_real_,
        std_error = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_
      ),
      diagnostics = tibble(
        status = "error",
        n_respondents = nrow(reduced),
        n_countries = n_distinct(reduced$cntry),
        n_country_rounds = n_distinct(reduced$country_round),
        singular = NA,
        optimizer_code = NA_character_,
        convergence_messages = "",
        warnings = paste(unique(fit_warnings), collapse = "; "),
        error = conditionMessage(attempt),
        clean_fit = FALSE
      )
    ))
  }

  fit <- attempt
  stopifnot(nobs(fit) == nrow(reduced))

  optimizer_code <- paste(
    fit@optinfo$conv$opt,
    collapse = "; "
  )

  convergence_messages <- paste(
    fit@optinfo$conv$lme4$messages,
    collapse = "; "
  )

  singular <- isSingular(fit, tol = 1e-4)

  list(
    coefficients = extract_terms(fit, terms),
    diagnostics = tibble(
      status = "fitted",
      n_respondents = nobs(fit),
      n_countries = n_distinct(reduced$cntry),
      n_country_rounds = n_distinct(reduced$country_round),
      singular = singular,
      optimizer_code = optimizer_code,
      convergence_messages = convergence_messages,
      warnings = paste(unique(fit_warnings), collapse = "; "),
      error = "",
      clean_fit =
        optimizer_code == "0" &&
        !nzchar(convergence_messages) &&
        !singular &&
        length(fit_warnings) == 0L
    )
  )
}

# 4. Run both periods and models ------------------------------------------

all_results <- list()
all_diagnostics <- list()
all_baselines <- list()

for (file_id in names(periods)) {
  sample_id <- unname(periods[[file_id]])

  for (model_name in c("M5", "M6")) {
    model_path <- file.path(
      model_dir,
      paste0(file_id, "_", model_name, ".rds")
    )

    if (!file.exists(model_path)) {
      stop("Missing saved model: ", model_path)
    }

    # Detect replacement of a source model before reusing checkpoints.
    source_hash <- unname(tools::md5sum(model_path))

    full_fit <- readRDS(model_path)
    dat <- model.frame(full_fit)

    model_formula <- formula(full_fit)
    environment(model_formula) <- environment()

    terms <- if (model_name == "M5") {
      c("gini_w", "hate_w")
    } else {
      c("gini_w", "hate_w", "gh_w")
    }

    baseline <- extract_terms(full_fit, terms) |>
      rename(
        full_estimate = estimate,
        full_std_error = std_error,
        full_conf_low = conf_low,
        full_conf_high = conf_high
      )

    run_id <- paste(file_id, model_name, sep = "_")

    all_baselines[[run_id]] <- baseline |>
      mutate(
        sample = sample_id,
        model = model_name,
        .before = 1
      )

    countries <- sort(unique(as.character(dat$cntry)))

    for (i in seq_along(countries)) {
      omitted_country <- countries[[i]]

      checkpoint_path <- file.path(
        checkpoint_dir,
        paste0(run_id, "_omit_", omitted_country, ".rds")
      )

      cached <- if (file.exists(checkpoint_path)) {
        readRDS(checkpoint_path)
      } else {
        NULL
      }

      reuse <- !is.null(cached) &&
        identical(cached$source_hash, source_hash) &&
        identical(cached$analysis_version, analysis_version) &&
        identical(cached$result$diagnostics$status, "fitted")

      cat(
        "\n", sample_id, " ", model_name,
        ": omit ", omitted_country,
        " (", i, "/", length(countries), ")",
        if (reuse) " [cached]" else "",
        "\n",
        sep = ""
      )

      if (reuse) {
        result <- cached$result
      } else {
        result <- fit_without_country(
          dat = dat,
          model_formula = model_formula,
          omitted_country = omitted_country,
          terms = terms
        )

        saveRDS(
          list(
            source_hash = source_hash,
            analysis_version = analysis_version,
            result = result
          ),
          checkpoint_path
        )
      }

      result_id <- paste(run_id, omitted_country, sep = "_")

      all_diagnostics[[result_id]] <- result$diagnostics |>
        mutate(
          sample = sample_id,
          model = model_name,
          omitted_country = omitted_country,
          .before = 1
        )

      all_results[[result_id]] <- result$coefficients |>
        left_join(baseline, by = "term") |>
        mutate(
          sample = sample_id,
          model = model_name,
          omitted_country = omitted_country,
          clean_fit = result$diagnostics$clean_fit,
          change = estimate - full_estimate,
          # Descriptive change measured in full-model SE units.
          # This is not a test statistic.
          change_in_full_se = change / full_std_error,
          sign_changed = sign(estimate) != sign(full_estimate),
          .before = 1
        )

      # Refresh combined tables after each completed attempt.
      write_csv(
        bind_rows(all_results),
        file.path(output_dir, "country_influence_coefficients.csv")
      )

      write_csv(
        bind_rows(all_diagnostics),
        file.path(output_dir, "country_influence_diagnostics.csv")
      )

      invisible(gc())
    }

    rm(full_fit, dat)
    invisible(gc())
  }
}

# 5. Summaries ------------------------------------------------------------

influence_results <- bind_rows(all_results)
influence_diagnostics <- bind_rows(all_diagnostics)
baseline_results <- bind_rows(all_baselines)

diagnostic_summary <- influence_diagnostics |>
  group_by(sample, model) |>
  summarise(
    n_attempts = n(),
    n_clean = sum(clean_fit),
    n_errors = sum(status == "error"),
    n_flagged_fits = sum(status == "fitted" & !clean_fit),
    .groups = "drop"
  )

# Only clean refits enter numerical summaries.
# Flagged fits remain available in the full output tables.
influence_summary <- influence_results |>
  filter(clean_fit) |>
  group_by(sample, model, term) |>
  summarise(
    n_clean_refits = n(),
    full_estimate = first(full_estimate),
    minimum_estimate = min(estimate),
    maximum_estimate = max(estimate),
    maximum_absolute_change = max(abs(change)),
    n_sign_changes = sum(sign_changed),
    .groups = "drop"
  )

largest_changes <- influence_results |>
  filter(clean_fit) |>
  group_by(sample, model, term) |>
  slice_max(
    order_by = abs(change_in_full_se),
    n = 3,
    with_ties = FALSE
  ) |>
  ungroup() |>
  select(
    sample, model, term, omitted_country,
    full_estimate, estimate, std_error,
    conf_low, conf_high, change, change_in_full_se
  )

flagged_fits <- influence_diagnostics |>
  filter(!clean_fit)

write_csv(
  baseline_results,
  file.path(output_dir, "full_sample_reference.csv")
)

write_csv(
  diagnostic_summary,
  file.path(output_dir, "diagnostic_summary.csv")
)

write_csv(
  influence_summary,
  file.path(output_dir, "influence_summary.csv")
)

write_csv(
  largest_changes,
  file.path(output_dir, "largest_changes.csv")
)

writeLines(
  capture.output(sessionInfo()),
  file.path(output_dir, "session_info.txt")
)

cat("\nDiagnostic summary:\n")
print(diagnostic_summary, n = Inf, width = Inf)

cat("\nFlagged fits:\n")
print(flagged_fits, n = Inf, width = Inf)

cat("\nCoefficient ranges across clean country-deletion refits:\n")
print(influence_summary, n = Inf, width = Inf)

cat("\nLargest changes for hate speech and primary moderation:\n")
print(
  largest_changes |>
    filter(term %in% c("hate_w", "gh_w")),
  n = Inf,
  width = Inf
)

