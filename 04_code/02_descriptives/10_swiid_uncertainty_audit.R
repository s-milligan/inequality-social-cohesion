# ------------------------------------------------------------
# Audit SWIID uncertainty draws
# Project: Inequality and social cohesion
#
# Purpose:
#   Verify the structure and scale of the 100 SWIID imputations
#   before propagating SWIID uncertainty through the ESS REWB
#   models.
#
# The SWIID R-format file contains:
#
#   swiid
#     A list of multiply imputed country-year inequality series.
#
#   swiid_summary
#     A country-year summary dataset containing the mean SWIID
#     estimates and their associated uncertainty.
#
# This audit checks:
#
#   1. that exactly 100 imputations are available;
#   2. that all imputations have the expected structure;
#   3. that gini_disp is stored in hundredths in the imputation
#      objects and is correctly rescaled to the conventional
#      0-100 Gini scale;
#   4. that the country-year structure of the imputations matches
#      swiid_summary, and to describe how the empirical mean and
#      standard deviation across imputations compare with the
#      published summary values.
#
# No ESS data are modified and no models are fitted here.
# ------------------------------------------------------------


library(dplyr)
library(here)
library(readr)


# Locate the SWIID R-format file -------------------------------------
#
# Search recursively so that the code does not depend on the exact
# SWIID filename, provided that the relevant .rda/.RData file is kept
# somewhere under the expected raw-data directory.

swiid_dir <- here(
  "03_data",
  "raw",
  "inequality",
  "swiid"
)


swiid_files <- list.files(
  swiid_dir,
  pattern = "\\.(rda|RData)$",
  recursive = TRUE,
  full.names = TRUE
)


if (
  length(
    swiid_files
  ) == 0
) {

  stop(
    "No SWIID .rda/.RData file found under: ",
    swiid_dir
  )
}


# If multiple R-format files are present, first try to identify
# Version 9.92 from the filename.

if (
  length(
    swiid_files
  ) > 1
) {

  version_992 <- swiid_files[
    grepl(
      "9[_\\.-]?92",
      basename(
        swiid_files
      ),
      ignore.case = TRUE
    )
  ]


  if (
    length(
      version_992
    ) == 1
  ) {

    swiid_file <- version_992

  } else {

    cat(
      "\nMultiple SWIID R-format files found:\n"
    )

    print(
      swiid_files
    )

    stop(
      "Could not uniquely identify the SWIID 9.92 R-format file."
    )
  }

} else {

  swiid_file <- swiid_files
}


cat(
  "\nSWIID file:\n",
  swiid_file,
  "\n"
)


# Load into a temporary environment ----------------------------------
#
# This avoids silently overwriting any similarly named objects already
# present in the R session.

swiid_env <- new.env()


loaded_objects <- load(
  swiid_file,
  envir = swiid_env
)


cat(
  "\nObjects loaded from SWIID file:\n"
)

print(
  loaded_objects
)


required_objects <- c(
  "swiid",
  "swiid_summary"
)


missing_objects <- setdiff(
  required_objects,
  loaded_objects
)


if (
  length(
    missing_objects
  ) > 0
) {

  stop(
    "Expected SWIID objects missing: ",
    paste(
      missing_objects,
      collapse = ", "
    )
  )
}


swiid <- swiid_env$swiid

swiid_summary <- swiid_env$swiid_summary


# Check imputation structure -----------------------------------------
#
# The main SWIID object should be a list containing one complete
# country-year dataset for each imputation.

if (
  !is.list(
    swiid
  )
) {

  stop(
    "Object `swiid` is not a list."
  )
}


n_imputations <- length(
  swiid
)


cat(
  "\nNumber of SWIID imputations:",
  n_imputations,
  "\n"
)


if (
  n_imputations != 100
) {

  stop(
    "Expected 100 SWIID imputations but found ",
    n_imputations,
    "."
  )
}


# Check dimensions of every imputation.

imputation_dimensions <- tibble(
  imputation =
    seq_along(
      swiid
    ),

  n_rows =
    vapply(
      swiid,
      nrow,
      integer(1)
    ),

  n_columns =
    vapply(
      swiid,
      ncol,
      integer(1)
    )
)


cat(
  "\nSWIID imputation dimensions:\n"
)

print(
  imputation_dimensions |>
    summarise(
      n_imputations = n(),

      min_rows =
        min(
          n_rows
        ),

      max_rows =
        max(
          n_rows
        ),

      min_columns =
        min(
          n_columns
        ),

      max_columns =
        max(
          n_columns
        )
    ),
  width = Inf
)


if (
  length(
    unique(
      imputation_dimensions$n_rows
    )
  ) != 1
) {

  stop(
    "SWIID imputations do not all have the same number of rows."
  )
}


if (
  length(
    unique(
      imputation_dimensions$n_columns
    )
  ) != 1
) {

  stop(
    "SWIID imputations do not all have the same number of columns."
  )
}


# Check required variables -------------------------------------------
#
# Only disposable-income Gini is needed for the present analysis,
# but retain the structural check for country and year as well.

required_draw_variables <- c(
  "country",
  "year",
  "gini_disp"
)


variable_check <- tibble(
  imputation =
    seq_along(
      swiid
    ),

  required_variables_present =
    vapply(
      swiid,
      function(x) {

        all(
          required_draw_variables %in%
            names(x)
        )
      },
      logical(1)
    )
)


if (
  !all(
    variable_check$required_variables_present
  )
) {

  print(
    variable_check |>
      filter(
        !required_variables_present
      ),
    n = Inf
  )

  stop(
    "Required SWIID variables are missing from one or more imputations."
  )
}


cat(
  "\nVariables in first SWIID imputation:\n"
)

print(
  names(
    swiid[[1]]
  )
)


# Check the raw Gini scale -------------------------------------------
#
# In the imputation objects, Gini values are stored in hundredths.
# For example, a stored value around 3287 corresponds to a Gini of
# approximately 32.87.
#
# The model therefore requires division by 100 before the draws are
# compared with the summary series or used in ESS models.

raw_gini_range <- range(
  as.numeric(
    swiid[[1]]$gini_disp
  ),
  na.rm = TRUE
)


scaled_gini_range <- raw_gini_range /
  100


cat(
  "\nRaw gini_disp range in first imputation:\n"
)

print(
  raw_gini_range
)


cat(
  "\nGini range after division by 100:\n"
)

print(
  scaled_gini_range
)


if (
  max(
    raw_gini_range
  ) < 100
) {

  stop(
    paste(
      "The raw gini_disp values do not appear to be stored",
      "in hundredths. Recheck the SWIID format before continuing."
    )
  )
}


# Stack all 100 imputations ------------------------------------------
#
# Create a long-format dataset with one row per
# country × year × imputation.
#
# Keep only the variables needed for the inequality-uncertainty
# analysis. gini_disp is converted to the conventional 0-100 scale.

swiid_draws <- bind_rows(
  lapply(
    seq_along(
      swiid
    ),
    function(i) {

      swiid[[i]] |>
        transmute(
          imputation =
            i,

          country =
            as.character(
              country
            ),

          year =
            as.integer(
              year
            ),

          gini_disp =
            as.numeric(
              gini_disp
            ) /
            100
        )
    }
  )
)


cat(
  "\nStacked SWIID draw rows:",
  nrow(
    swiid_draws
  ),
  "\n"
)


# Check uniqueness ---------------------------------------------------
#
# Each imputation should contain exactly one value for each
# country-year.

duplicate_draw_keys <- swiid_draws |>
  count(
    imputation,
    country,
    year
  ) |>
  filter(
    n > 1
  )


cat(
  "\nDuplicate country-year-imputation keys:",
  nrow(
    duplicate_draw_keys
  ),
  "\n"
)


if (
  nrow(
    duplicate_draw_keys
  ) > 0
) {

  print(
    duplicate_draw_keys |>
      slice_head(
        n = 20
      ),
    n = 20,
    width = Inf
  )

  stop(
    "Duplicate SWIID country-year-imputation keys found."
  )
}


# Summarise uncertainty across imputations ---------------------------
#
# For each country-year, calculate:
#
#   - the empirical mean Gini across the 100 imputations;
#   - the empirical standard deviation across the 100 imputations.
#
# These provide descriptive checks on the scale and distribution of
# the released imputations. Exact agreement with swiid_summary is not
# required or assumed.

draw_summary <- swiid_draws |>
  group_by(
    country,
    year
  ) |>
  summarise(
    n_imputations =
      sum(
        !is.na(
          gini_disp
        )
      ),

    gini_disp_draw_mean =
      mean(
        gini_disp,
        na.rm = TRUE
      ),

    gini_disp_draw_sd =
      sd(
        gini_disp,
        na.rm = TRUE
      ),

    .groups = "drop"
  )


# Check that all country-years have all 100 draws.

imputation_coverage_summary <- draw_summary |>
  count(
    n_imputations,
    name = "n_country_years"
  ) |>
  arrange(
    n_imputations
  )


cat(
  "\nNumber of imputations available per country-year:\n"
)

print(
  imputation_coverage_summary,
  n = Inf,
  width = Inf
)


# Compare imputations with swiid_summary ------------------------------
#
# Compare the empirical distribution of the released imputations with
# the published SWIID summary values as a descriptive quality check.
#
# The audit does not assume that the 100 imputations are independent
# Monte Carlo draws whose empirical mean must reproduce the summary
# estimate, or that gini_disp_se is the Monte Carlo standard error of
# that empirical mean.

required_summary_variables <- c(
  "country",
  "year",
  "gini_disp",
  "gini_disp_se"
)


missing_summary_variables <- setdiff(
  required_summary_variables,
  names(
    swiid_summary
  )
)


if (
  length(
    missing_summary_variables
  ) > 0
) {

  stop(
    "Required variables missing from swiid_summary: ",
    paste(
      missing_summary_variables,
      collapse = ", "
    )
  )
}


summary_values <- swiid_summary |>
  transmute(
    country =
      as.character(
        country
      ),

    year =
      as.integer(
        year
      ),

    gini_disp_summary =
      as.numeric(
        gini_disp
      ),

    gini_disp_se_summary =
      as.numeric(
        gini_disp_se
      )
  )


summary_comparison <- summary_values |>
  full_join(
    draw_summary,
    by = c(
      "country",
      "year"
    )
  ) |>
  mutate(
    mean_difference =
      gini_disp_draw_mean -
      gini_disp_summary,

    mean_abs_difference =
      abs(
        mean_difference
      ),

    se_difference =
      gini_disp_draw_sd -
      gini_disp_se_summary,

    se_abs_difference =
      abs(
        se_difference
      )
  )


# Check whether the country-year structures agree.

structure_comparison <- summary_comparison |>
  summarise(
    n_country_years = n(),

    n_summary_only =
      sum(
        !is.na(
          gini_disp_summary
        ) &
          is.na(
            gini_disp_draw_mean
          )
      ),

    n_draws_only =
      sum(
        is.na(
          gini_disp_summary
        ) &
          !is.na(
            gini_disp_draw_mean
          )
      ),

    n_both =
      sum(
        !is.na(
          gini_disp_summary
        ) &
          !is.na(
            gini_disp_draw_mean
          )
      )
  )


cat(
  "\nSWIID summary/draw country-year structure:\n"
)

print(
  structure_comparison,
  width = Inf
)


# Quantify the descriptive agreement between the supplied summary
# estimates and the empirical moments of the 100 imputations.
#
# Differences are reported rather than evaluated against a formal
# Monte Carlo tolerance. The released imputations and summary values
# need not reproduce one another exactly.

agreement_summary <- summary_comparison |>
  filter(
    !is.na(
      gini_disp_summary
    ),
    !is.na(
      gini_disp_draw_mean
    )
  ) |>
  summarise(
    n_compared = n(),

    median_mean_abs_difference =
      median(
        mean_abs_difference
      ),

    p95_mean_abs_difference =
      quantile(
        mean_abs_difference,
        0.95,
        names = FALSE
      ),

    max_mean_abs_difference =
      max(
        mean_abs_difference
      ),

    median_se_abs_difference =
      median(
        se_abs_difference,
        na.rm = TRUE
      ),

    p95_se_abs_difference =
      quantile(
        se_abs_difference,
        0.95,
        na.rm = TRUE,
        names = FALSE
      ),

    max_se_abs_difference =
      max(
        se_abs_difference,
        na.rm = TRUE
      ),

    correlation_means =
      cor(
        gini_disp_summary,
        gini_disp_draw_mean,
        use = "complete.obs"
      ),

    correlation_se =
      cor(
        gini_disp_se_summary,
        gini_disp_draw_sd,
        use = "complete.obs"
      )
  )


cat(
  "\nAgreement between SWIID summary and 100 draws:\n"
)

print(
  agreement_summary,
  width = Inf
)

# Show only the twenty largest point-estimate differences.
#
# Do not print every country-year difference to the console.

largest_mean_differences <- summary_comparison |>
  filter(
    !is.na(
      gini_disp_summary
    ),
    !is.na(
      gini_disp_draw_mean
    )
  ) |>
  arrange(
    desc(
      mean_abs_difference
    )
  ) |>
  select(
    country,
    year,
    gini_disp_summary,
    gini_disp_draw_mean,
    mean_difference,
    gini_disp_se_summary,
    gini_disp_draw_sd,
    se_difference
  ) |>
  slice_head(
    n = 20
  )


cat(
  "\nTwenty largest summary-versus-draw differences:\n"
)

print(
  largest_mean_differences,
  n = 20,
  width = Inf
)


# Save compact audit outputs -----------------------------------------

output_dir <- here(
  "05_output",
  "tables",
  "swiid_uncertainty_audit"
)


dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


write_csv(
  imputation_dimensions,
  file.path(
    output_dir,
    "swiid_imputation_dimensions.csv"
  )
)


write_csv(
  imputation_coverage_summary,
  file.path(
    output_dir,
    "swiid_imputation_coverage.csv"
  )
)


write_csv(
  structure_comparison,
  file.path(
    output_dir,
    "swiid_summary_draw_structure.csv"
  )
)


write_csv(
  agreement_summary,
  file.path(
    output_dir,
    "swiid_summary_draw_agreement.csv"
  )
)


write_csv(
  largest_mean_differences,
  file.path(
    output_dir,
    "swiid_largest_summary_draw_differences.csv"
  )
)


cat(
  "\nSWIID uncertainty audit outputs saved in:",
  output_dir,
  "\n"
)
