# ------------------------------------------------------------
# Trends in trust and inequality
# Project: Inequality and social cohesion
#
# Purpose:
#   Examine common temporal patterns in trust and inequality
#   before estimating within-between models.
# ------------------------------------------------------------

library(dplyr)
library(readr)
library(ggplot2)
library(here)

# 1. Import ---------------------------------------------------------------

dat <- read_csv(
  here(
    "03_data",
    "interim",
    "ess_country_round_trust.csv"
  ),
  show_col_types = FALSE
) |>
  filter(
    !is.na(weighted_mean_trust),
    !is.na(gini_swiid_lag1)
  )

# 2. Identify balanced-panel countries ----------------------------------

balanced_countries <- dat |>
  count(cntry) |>
  filter(n == 9) |>
  pull(cntry)

cat(
  "\nBalanced-panel countries:\n",
  paste(balanced_countries, collapse = ", "),
  "\n"
)

# 3. Round-level averages -------------------------------------------------

round_trends <- dat |>
  group_by(essround) |>
  summarise(
    n_countries = n(),

    # Countries receive equal weight
    mean_trust_all =
      mean(weighted_mean_trust),

    mean_gini_all =
      mean(gini_swiid_lag1),

    .groups = "drop"
  )

balanced_trends <- dat |>
  filter(cntry %in% balanced_countries) |>
  group_by(essround) |>
  summarise(
    mean_trust_balanced =
      mean(weighted_mean_trust),

    mean_gini_balanced =
      mean(gini_swiid_lag1),

    .groups = "drop"
  )

round_trends <- round_trends |>
  left_join(
    balanced_trends,
    by = "essround"
  )

print(round_trends)

# 4. Trust trend ----------------------------------------------------------

p_trust <- ggplot(
  round_trends,
  aes(x = essround)
) +
  geom_line(
    aes(
      y = mean_trust_all,
      linetype = "All available countries"
    )
  ) +
  geom_point(
    aes(
      y = mean_trust_all,
      shape = "All available countries"
    )
  ) +
  geom_line(
    aes(
      y = mean_trust_balanced,
      linetype = "Balanced panel"
    )
  ) +
  geom_point(
    aes(
      y = mean_trust_balanced,
      shape = "Balanced panel"
    )
  ) +
  scale_x_continuous(
    breaks = 1:9
  ) +
  labs(
    title = "Social trust across ESS rounds",
    subtitle = "All participating countries and balanced nine-round panel",
    x = "ESS round",
    y = "Mean generalized social trust",
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal()

print(p_trust)

# 5. Inequality trend -----------------------------------------------------

p_gini <- ggplot(
  round_trends,
  aes(x = essround)
) +
  geom_line(
    aes(
      y = mean_gini_all,
      linetype = "All available countries"
    )
  ) +
  geom_point(
    aes(
      y = mean_gini_all,
      shape = "All available countries"
    )
  ) +
  geom_line(
    aes(
      y = mean_gini_balanced,
      linetype = "Balanced panel"
    )
  ) +
  geom_point(
    aes(
      y = mean_gini_balanced,
      shape = "Balanced panel"
    )
  ) +
  scale_x_continuous(
    breaks = 1:9
  ) +
  labs(
    title = "Income inequality across ESS rounds",
    subtitle = "All participating countries and balanced nine-round panel",
    x = "ESS round",
    y = "Disposable-income Gini (SWIID, lagged)",
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal()

print(p_gini)

# 6. Country trajectories: trust -----------------------------------------

p_country_trust <- dat |>
  filter(cntry %in% balanced_countries) |>
  ggplot(
    aes(
      x = essround,
      y = weighted_mean_trust,
      group = cntry
    )
  ) +
  geom_line(alpha = 0.5) +
  geom_point(size = 1) +
  facet_wrap(~ cntry) +
  scale_x_continuous(
    breaks = c(1, 3, 5, 7, 9)
  ) +
  labs(
    title = "Country trajectories in social trust",
    subtitle = "Countries participating in all nine ESS rounds",
    x = "ESS round",
    y = "Mean generalized social trust"
  ) +
  theme_minimal()

print(p_country_trust)

# 7. Save ----------------------------------------------------------------

dir.create(
  here("05_output", "figures"),
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  here("05_output", "figures", "trust_by_round.png"),
  p_trust,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  here("05_output", "figures", "gini_by_round.png"),
  p_gini,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  here(
    "05_output",
    "figures",
    "country_trust_trajectories.png"
  ),
  p_country_trust,
  width = 10,
  height = 8,
  dpi = 300
)