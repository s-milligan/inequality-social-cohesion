
# ------------------------------------------------------------
# Visualise political-discourse REWB model results
# Project: Inequality and social cohesion
#
# Figure 1:
#   All six single-indicator within-country associations
#
# Figure 2:
#   Coefficient changes across separate and joint models
#
# Estimates expressed per one country-round SD of
# within-country variation.
#
# Common-good framing is oriented positively for plotting.
# No statistical models are refitted.
# ------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(here)
library(readr)

# 1. Input and output paths -----------------------------------

single_file <- here(
  "05_output", "tables",
  "discourse_alternative_models",
  "standardized_within.csv"
)

joint_file <- here(
  "05_output", "tables",
  "discourse_joint_models",
  "standardized_within.csv"
)

figure_dir <- here(
  "05_output", "figures", "discourse_models"
)

stopifnot(
  file.exists(single_file),
  file.exists(joint_file)
)

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# 2. Labels and coefficient orientation -----------------------

indicator_names <- c(
  disinformation_w = "Party disinformation",
  hate_w = "Party hate speech",
  issue_polarisation_w = "Issue polarisation",
  antagonistic_camps_w = "Antagonistic camps",
  counterarg_w = "Counterargument disrespect",
  low_common_good_w = "More common-good framing"
)

# Original low_common_good_w is higher for LESS
# common-good framing.
#
# Reverse the estimate and swap/reverse the CI endpoints
# to show MORE common-good framing instead.

prepare_coefficients <- function(dat) {

  dat |>
    mutate(
      indicator = unname(indicator_names[term]),

      plot_estimate = if_else(
        term == "low_common_good_w",
        -estimate_per_sd,
        estimate_per_sd
      ),

      plot_low = if_else(
        term == "low_common_good_w",
        -conf_high_per_sd,
        conf_low_per_sd
      ),

      plot_high = if_else(
        term == "low_common_good_w",
        -conf_low_per_sd,
        conf_high_per_sd
      )
    )
}

single <- read_csv(
  single_file,
  show_col_types = FALSE
) |>
  filter(term %in% names(indicator_names)) |>
  prepare_coefficients()

joint <- read_csv(
  joint_file,
  show_col_types = FALSE
) |>
  filter(term %in% names(indicator_names)) |>
  prepare_coefficients()

stopifnot(
  nrow(single) == 6L,
  nrow(joint) == 7L,
  !anyNA(single$indicator),
  !anyNA(joint$indicator)
)

# 3. Shared figure theme --------------------------------------

figure_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(
      size = 9,
      hjust = 0
    ),
    plot.margin = margin(12, 18, 12, 12)
  )

# 4. Figure 1: all six alternative indicators ------------------

indicator_order <- c(
  "Party disinformation",
  "Party hate speech",
  "Issue polarisation",
  "Antagonistic camps",
  "Counterargument disrespect",
  "More common-good framing"
)

single <- single |>
  mutate(
    indicator = factor(
      indicator,
      levels = rev(indicator_order)
    )
  )

p1 <- ggplot(
  single,
  aes(
    x = plot_estimate,
    y = indicator
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey55",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(
      x = plot_low,
      xend = plot_high,
      yend = indicator
    ),
    colour = "#345C80",
    linewidth = 0.8
  ) +
  geom_point(
    colour = "#125B9A",
    size = 3
  ) +
  labs(
    title = "Political context and generalised trust",
    subtitle = paste(
      "Within-country coefficients from",
      "separate REWB models"
    ),
    x = paste(
      "Estimated trust difference per one",
      "within-country SD"
    ),
    y = NULL,
    caption = paste(
      "Points: estimates; lines: 95% Wald confidence",
      "intervals.\n",
      "Higher scores indicate more of each named condition.",
      "Common-good framing is positively oriented.\n",
      "All models use the same 498,461 respondents."
    )
  ) +
  figure_theme

print(p1)

ggsave(
  filename = file.path(
    figure_dir,
    "01_discourse_single_indicator_coefficients.png"
  ),
  plot = p1,
  width = 10,
  height = 5.6,
  dpi = 240
)

# 5. Figure 2: separate versus joint models --------------------

joint_plot <- joint |>
  mutate(
    model_display = case_when(
      model %in% c(
        "M5 party hate",
        "C party disinformation"
      ) ~ "Standalone",

      startsWith(model, "J1") ~ "Joint J1",

      startsWith(model, "J2") ~ "Joint J2",

      TRUE ~ NA_character_
    )
  )

# Add common-good framing from its standalone model.
# It enters the joint sequence only in J2.

common_good_alone <- single |>
  filter(term == "low_common_good_w") |>
  mutate(
    model_display = "Standalone"
  )

joint_plot <- bind_rows(
  joint_plot,
  common_good_alone
) |>
  mutate(
    model_display = factor(
      model_display,
      levels = c(
        "Joint J2",
        "Joint J1",
        "Standalone"
      )
    ),
    indicator = factor(
      as.character(indicator),
      levels = c(
        "Party hate speech",
        "Party disinformation",
        "More common-good framing"
      )
    )
  )

p2 <- ggplot(
  joint_plot,
  aes(
    x = plot_estimate,
    y = model_display
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey55",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(
      x = plot_low,
      xend = plot_high,
      yend = model_display
    ),
    colour = "#345C80",
    linewidth = 0.8
  ) +
  geom_point(
    colour = "#125B9A",
    size = 2.8
  ) +
  facet_wrap(
    ~ indicator,
    nrow = 1
  ) +
  labs(
    title = "How joint adjustment changes the associations",
    subtitle = paste(
      "Standalone models compared with",
      "joint specifications J1 and J2"
    ),
    x = paste(
      "Estimated trust difference per one",
      "within-country SD"
    ),
    y = NULL,
    caption = paste(
      "Points: estimates; lines: 95% Wald intervals.\n",
      "J1 includes party hate and disinformation.",
      "J2 additionally includes common-good framing.\n",
      "All models use the same respondents.",
      "Common-good framing is positively oriented."
    )
  ) +
  figure_theme +
  theme(
    strip.text = element_text(
      face = "bold",
      size = 10
    )
  )

print(p2)

ggsave(
  filename = file.path(
    figure_dir,
    "02_discourse_joint_model_coefficients.png"
  ),
  plot = p2,
  width = 13,
  height = 5.2,
  dpi = 240
)

cat(
  "\nFigures saved to:\n",
  figure_dir,
  "\n"
)
