# ------------------------------------------------------------
# Trust and inequality descriptives
# Project: Inequality and social cohesion
#
# Purpose:
#   Describe between-country and within-country relationships
#   between income inequality and social trust.
# ------------------------------------------------------------

library(dplyr)
library(readr)
library(ggplot2)
library(here)

# 1. Import country-round data --------------------------------------------

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

# 2. Overall country-round association ------------------------------------

cat(
  "\nCountry-round correlation:",
  cor(
    dat$gini_swiid_lag1,
    dat$weighted_mean_trust
  ),
  "\n"
)

# 3. Between-country relationship -----------------------------------------

# Average each country's country-round observations.
# Each ESS round receives equal weight here.

between <- dat |>
  group_by(cntry) |>
  summarise(
    n_rounds = n(),
    mean_gini = mean(gini_swiid_lag1),
    mean_trust = mean(weighted_mean_trust),
    .groups = "drop"
  )

cat(
  "\nBetween-country correlation:",
  cor(
    between$mean_gini,
    between$mean_trust
  ),
  "\n"
)

p_between <- ggplot(
  between,
  aes(
    x = mean_gini,
    y = mean_trust
  )
) +
  geom_point(size = 2) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  geom_text(
    aes(label = cntry),
    vjust = -0.7,
    check_overlap = TRUE,
    size = 3
  ) +
  labs(
    title = "Income inequality and social trust across countries",
    subtitle = "Country averages across ESS Rounds 1–9",
    x = "Mean disposable-income Gini (SWIID, lagged)",
    y = "Mean generalized social trust"
  ) +
  theme_minimal()

print(p_between)

# 4. Within-country deviations --------------------------------------------

within <- dat |>
  group_by(cntry) |>
  mutate(
    gini_country_mean =
      mean(gini_swiid_lag1),

    trust_country_mean =
      mean(weighted_mean_trust),

    gini_within =
      gini_swiid_lag1 -
      gini_country_mean,

    trust_within =
      weighted_mean_trust -
      trust_country_mean
  ) |>
  ungroup()

cat(
  "\nWithin-country correlation:",
  cor(
    within$gini_within,
    within$trust_within
  ),
  "\n"
)

p_within <- ggplot(
  within,
  aes(
    x = gini_within,
    y = trust_within
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.3
  ) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.3
  ) +
  geom_point(
    alpha = 0.6,
    size = 1.7
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Within-country deviations in inequality and social trust",
    subtitle = "Deviations from each country's own mean, ESS Rounds 1–9",
    x = "Deviation from country mean Gini",
    y = "Deviation from country mean social trust"
  ) +
  theme_minimal()

print(p_within)

# 5. Save figures locally -------------------------------------------------

dir.create(
  here("05_output", "figures"),
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  here(
    "05_output",
    "figures",
    "inequality_trust_between.png"
  ),
  p_between,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  here(
    "05_output",
    "figures",
    "inequality_trust_within.png"
  ),
  p_within,
  width = 7,
  height = 5,
  dpi = 300
)
