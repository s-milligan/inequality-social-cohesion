
# ============================================================
# Working paper figures
# Project: Inequality and social cohesion
#
# Purpose:
#   Prepare the four figures for the working paper using
#   existing analytical outputs. No models are refitted.
#
# Figure 1: Inequality coefficients across model specifications
# Figure 2: Political-discourse within-between coefficients
# Figure 3: Previously generated conditional Gini slopes
# Figure 4: Period-specific political-discourse slopes
#
# Output:
#   05_output/figures/working_paper/
#
# Figure 3 is staged from an existing PNG. Figures 1, 2,
# and 4 are generated directly from saved CSV results.
# ============================================================

library(dplyr)
library(readr)
library(ggplot2)
library(here)

# ------------------------------------------------------------
# 1. Directories and input helpers
# ------------------------------------------------------------

root <- here()
output_root <- here("05_output")

figure_dir <- here(
  "05_output", "figures", "working_paper"
)

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# Locate named output files without assuming every analysis
# uses the same subdirectory under 05_output.

find_output <- function(filename) {

  candidates <- list.files(
    output_root,
    recursive = TRUE,
    full.names = TRUE
  )

  candidates <- candidates[
    basename(candidates) == filename
  ]

  if (length(candidates) == 0) {
    stop(
      "Could not find: ", filename,
      "\nExpected somewhere under 05_output/.",
      "\nCheck that the relevant analysis has been run."
    )
  }

  if (length(candidates) > 1) {
    stop(
      "More than one file named ", filename, " found:\n",
      paste(candidates, collapse = "\n"),
      "\nPlease resolve the duplicate before continuing."
    )
  }

  candidates
}

read_output <- function(filename) {
  read_csv(
    find_output(filename),
    show_col_types = FALSE
  )
}

check_columns <- function(data, required, filename) {

  missing <- setdiff(required, names(data))

  if (length(missing) > 0) {
    stop(
      filename, " is missing columns: ",
      paste(missing, collapse = ", ")
    )
  }

  invisible(TRUE)
}

save_figure <- function(plot, filename, width, height) {

  ggsave(
    filename = file.path(figure_dir, filename),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 320,
    bg = "white"
  )
}

# Shared visual style.
# Keep figure titles plain rather than bold.

paper_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(
      face = "plain",
      size = 12,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      face = "plain",
      size = 10
    ),
    plot.title.position = "plot",
    strip.text = element_text(
      face = "plain",
      size = 11
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.caption = element_text(
      size = 8,
      hjust = 0
    ),
    plot.margin = margin(12, 15, 12, 12)
  )

# ------------------------------------------------------------
# 2. Figure 1
# Income inequality and generalised trust
# ------------------------------------------------------------

gini <- read_output(
  "extended_rewb_gini_results.csv"
)

check_columns(
  gini,
  c(
    "sample", "model", "effect", "estimate",
    "conf_low", "conf_high"
  ),
  "extended_rewb_gini_results.csv"
)

model_levels <- c(
  "M1",
  "M2",
  "M3",
  "M3 macro sample",
  "M4"
)

gini_plot_data <- gini |>
  filter(
    model %in% model_levels,
    effect %in% c(
      "Within-country Gini",
      "Between-country Gini"
    ),
    sample %in% c("R1-R9", "R1-R11")
  ) |>
  mutate(
    model = factor(
      model,
      levels = rev(model_levels)
    ),
    sample = factor(
      sample,
      levels = c("R1-R9", "R1-R11"),
      labels = c("ESS R1–R9", "ESS R1–R11")
    ),
    effect = factor(
      effect,
      levels = c(
        "Within-country Gini",
        "Between-country Gini"
      ),
      labels = c(
        "Within-country association",
        "Between-country association"
      )
    )
  )

if (nrow(gini_plot_data) != 20) {
  stop(
    "Figure 1 expected 20 estimates; found ",
    nrow(gini_plot_data), "."
  )
}

dodge_1 <- position_dodge(width = 0.55)

figure_1 <- ggplot(
  gini_plot_data,
  aes(
    x = estimate,
    y = model,
    xmin = conf_low,
    xmax = conf_high,
    shape = sample
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.4,
    colour = "grey55"
  ) +
  geom_errorbarh(
    height = 0.18,
    linewidth = 0.5,
    position = dodge_1
  ) +
  geom_point(
    size = 2.5,
    position = dodge_1
  ) +
  facet_wrap(
    ~ effect,
    ncol = 2,
    scales = "free_x"
  ) +
  scale_shape_manual(
    values = c(16, 17)
  ) +
  labs(
    title = "Income inequality and generalised trust",
    x = "Estimated difference in trust per Gini point",
    y = NULL,
    caption = paste(
      "Approximate 95% Wald confidence intervals.",
      "M3 macro sample is re-estimated on the M4 sample.",
      "Models differ in covariate adjustment."
    )
  ) +
  paper_theme

save_figure(
  figure_1,
  "figure_1_gini_coefficients.png",
  width = 10,
  height = 5.8
)

# ------------------------------------------------------------
# 3. Figure 2
# Political discourse: within-between comparison
# ------------------------------------------------------------

discourse <- read_output(
  "03_discourse_within_between_coefficients.csv"
)

check_columns(
  discourse,
  c(
    "indicator", "component", "estimate_sd",
    "low_sd", "high_sd"
  ),
  "03_discourse_within_between_coefficients.csv"
)

indicator_order <- c(
  "Party hate speech",
  "Party disinformation",
  "Counterargument disrespect",
  "More common-good framing",
  "Issue polarisation",
  "Antagonistic camps"
)

discourse_plot_data <- discourse |>
  filter(
    indicator %in% indicator_order,
    component %in% c(
      "Within-country change",
      "Between-country differences"
    )
  ) |>
  mutate(
    indicator = factor(
      indicator,
      levels = rev(indicator_order)
    ),
    component = factor(
      component,
      levels = c(
        "Within-country change",
        "Between-country differences"
      ),
      labels = c(
        "Within-country association",
        "Between-country association"
      )
    )
  )

if (nrow(discourse_plot_data) != 12) {
  stop(
    "Figure 2 expected 12 estimates; found ",
    nrow(discourse_plot_data), "."
  )
}

figure_2 <- ggplot(
  discourse_plot_data,
  aes(
    x = estimate_sd,
    y = indicator,
    xmin = low_sd,
    xmax = high_sd
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.4,
    colour = "grey55"
  ) +
  geom_errorbarh(
    height = 0.17,
    linewidth = 0.55
  ) +
  geom_point(size = 2.5) +
  facet_wrap(
    ~ component,
    ncol = 2,
    scales = "free_x"
  ) +
  labs(
    title = "Political discourse and generalised trust",
    x = "Estimated difference in trust per contextual SD",
    y = NULL,
    caption = paste(
      "Separate indicator models; approximate 95% Wald confidence intervals.",
      "Coefficients are standardised by the observed contextual SD.",
      "Indicators are not necessarily measures of one common construct."
    )
  ) +
  paper_theme

save_figure(
  figure_2,
  "figure_2_discourse_coefficients.png",
  width = 11,
  height = 5.7
)

# ------------------------------------------------------------
# 4. Figure 3
# Previously generated conditional inequality slopes
# ------------------------------------------------------------

# Use the original analytical figure rather than the copy
# previously placed in docs/figures/.

figure_3_source <- here(
  "05_output",
  "figures",
  "party_hate_moderation",
  "m6_conditional_gini_slopes.png"
)

figure_3_destination <- file.path(
  figure_dir,
  "figure_3_conditional_gini_slopes.png"
)

if (!file.exists(figure_3_source)) {
  stop(
    "Figure 3 source not found:\n",
    figure_3_source,
    "\nRun the original party-hate moderation plotting analysis."
  )
}

copied <- file.copy(
  from = figure_3_source,
  to = figure_3_destination,
  overwrite = TRUE
)

if (!copied) {
  stop("Could not copy Figure 3 into the working paper directory.")
}

cat("Figure 3 copied successfully.\n")
# ------------------------------------------------------------
# 5. Figure 4
# Temporal heterogeneity in political discourse
# ------------------------------------------------------------

period_slopes <- read_output(
  "rhetoric_period_slopes_comparison.csv"
)

check_columns(
  period_slopes,
  c(
    "indicator", "sample", "effect", "estimate",
    "conf_low", "conf_high"
  ),
  "rhetoric_period_slopes_comparison.csv"
)

period_plot_data <- period_slopes |>
  filter(
    sample == "All modes",
    indicator %in% c(
      "Party hate speech",
      "Party disinformation"
    ),
    effect %in% c(
      "R1-R9 slope",
      "R10-R11 slope"
    )
  ) |>
  mutate(
    indicator = factor(
      indicator,
      levels = c(
        "Party hate speech",
        "Party disinformation"
      )
    ),
    effect = factor(
      effect,
      levels = c("R10-R11 slope", "R1-R9 slope"),
      labels = c("ESS R10–R11", "ESS R1–R9")
    )
  )

if (nrow(period_plot_data) != 4) {
  stop(
    "Figure 4 expected four estimates; found ",
    nrow(period_plot_data), "."
  )
}

figure_4 <- ggplot(
  period_plot_data,
  aes(
    x = estimate,
    y = effect,
    xmin = conf_low,
    xmax = conf_high
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.4,
    colour = "grey55"
  ) +
  geom_errorbarh(
    height = 0.15,
    linewidth = 0.6
  ) +
  geom_point(size = 2.8) +
  facet_wrap(
    ~ indicator,
    ncol = 1,
  ) +
  labs(
    title = "Political discourse and trust across ESS periods",
    x = "Within-country discourse coefficient",
    y = NULL,
    ) +
  paper_theme +
  theme(
    strip.text = element_text(
      face = "plain",
      hjust = 0
    )
  )

save_figure(
  figure_4,
  "figure_4_period_discourse_slopes.png",
  width = 8.8,
  height = 5.2
)

# ------------------------------------------------------------
# 6. Final summary
# ------------------------------------------------------------

expected_files <- c(
  "figure_1_gini_coefficients.png",
  "figure_2_discourse_coefficients.png",
  "figure_3_conditional_gini_slopes.png",
  "figure_4_period_discourse_slopes.png"
)

cat("\nWorking paper figure preparation completed.\n")
cat("\nOutput directory:\n", figure_dir, "\n\n")

for (filename in expected_files) {

  path <- file.path(figure_dir, filename)

  cat(
    if (file.exists(path)) "[OK] " else "[MISSING] ",
    filename,
    "\n",
    sep = ""
  )
}

cat(
  "\nNote: Figure 3 is a copy of the existing analytical",
  " figure. The others were generated from CSV outputs.\n"
)
