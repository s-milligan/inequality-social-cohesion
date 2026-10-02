# Conditional inequality slopes from party hate-speech moderation models
#
# Prerequisite:
#   Run 04_code/03_models/party_hate_moderation.R first.
#
# Input:
#   05_output/tables/party_hate_moderation/conditional_slopes.csv
#
# Output:
#   05_output/figures/party_hate_moderation/m6_conditional_gini_slopes.png
#
# Displays M6 conditional slopes and approximate 95% Wald intervals
# over the observed moderator range, with country summaries held fixed.

library(dplyr)
library(ggplot2)
library(here)
library(readr)

slopes <- read_csv(
  here(
    "05_output", "tables", "party_hate_moderation",
    "conditional_slopes.csv"
  ),
  show_col_types = FALSE
)

slopes <- slopes |>
  mutate(
    sample = factor(sample, levels = c("R1-R9", "R1-R11"))
  )

m6_observed <- slopes |>
  filter(model == "M6")

m6_curve <- m6_observed |>
  distinct(sample, hate_w, estimate, conf_low, conf_high) |>
  arrange(sample, hate_w)

slope_plot <- ggplot(
  m6_curve,
  aes(x = hate_w, y = estimate)
) +
  geom_ribbon(
    aes(ymin = conf_low, ymax = conf_high),
    fill = "#2878A5",
    alpha = 0.18
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "grey45"
  ) +
  geom_line(colour = "#2878A5", linewidth = 0.8) +
  geom_rug(
    data = m6_observed,
    aes(x = hate_w),
    inherit.aes = FALSE,
    sides = "b",
    alpha = 0.25
  ) +
  facet_wrap(~ sample) +
  labs(
    x = "Lagged party hate speech: deviation from country mean",
    y = "Conditional trust slope per one-point increase in Gini",
    title = "Party hate speech and the inequality–trust relationship",
    subtitle = "M6: primary within-country moderation",
    caption = paste(
      "Shading: approximate 95% Wald intervals.",
      "Rug: observed country-rounds with within-country Gini variation.",
      "Country summaries held fixed."
    )
  ) +
  theme_minimal(base_size = 12)

print(slope_plot)

figure_dir <- here(
  "05_output", "figures", "party_hate_moderation"
)

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  file.path(figure_dir, "m6_conditional_gini_slopes.png"),
  slope_plot,
  width = 10,
  height = 5,
  dpi = 200
)
