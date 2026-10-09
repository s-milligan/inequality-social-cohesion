
# ------------------------------------------------------------
# Within-country versus between-country political context
# Project: Inequality and social cohesion
#
# ESS Rounds 1-11; V-Dem v16
#
# Purpose:
#   Compare within-country and between-country coefficients
#   from the six separate political-context REWB models.
#
# Standardisation:
#   Within:  SD of country-demeaned country-round exposures
#   Between: SD of country mean exposures (38 countries)
#
# No models are refitted.
#
# Output:
#   05_output/figures/discourse_models/
# ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(ggplot2)
library(here)

# 1. File paths -----------------------------------------------

ess_file <- here(
  "03_data", "processed", "ess_analysis_r1_r11.rds"
)

macro_file <- here(
  "03_data", "interim",
  "ess_country_round_macro_r1_r11.csv"
)

discourse_file <- here(
  "03_data", "interim",
  "ess_country_round_discourse_r1_r11.csv"
)

vdem_file <- here(
  "03_data", "raw", "vdem",
  "V-Dem-CY-Full+Others-v16.csv"
)

timing_base_file <- here(
  "03_data", "interim", "ess_inequality_coverage.csv"
)

timing_post9_file <- here(
  "03_data", "interim",
  "ess_post9_country_round_year_coverage.csv"
)

fixed_file <- here(
  "05_output", "tables",
  "discourse_alternative_models", "fixed_effects.csv"
)

previous_sd_file <- here(
  "05_output", "tables",
  "discourse_alternative_models", "standardized_within.csv"
)

figure_dir <- here(
  "05_output", "figures", "discourse_models"
)

required_files <- c(
  ess_file, macro_file, discourse_file,
  vdem_file, timing_base_file, timing_post9_file,
  fixed_file, previous_sd_file
)

for (f in required_files) {
  if (!file.exists(f)) {
    stop("Missing required file: ", f)
  }
}

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

keys <- c("cntry", "essround")

check_keys <- function(dat, columns, label) {

  if (anyDuplicated(dat[, columns, drop = FALSE])) {
    stop("Duplicate keys: ", label)
  }

  if (anyNA(dat[, columns, drop = FALSE])) {
    stop("Missing keys: ", label)
  }
}

# 2. Indicator definitions ------------------------------------
#
# Each indicator uses the same separate REWB model as
# the previously generated Figure 1.
#
# Common-good framing is sign-reversed for presentation.

indicators <- tribble(
  ~model, ~prefix, ~variable, ~indicator, ~orientation,
  "M5 party hate",
  "hate",
  "party_hate_lag1",
  "Party hate speech",
  1,

  "A counterargument disrespect",
  "counterarg",
  "counterarg_disrespect_lag1",
  "Counterargument disrespect",
  1,

  "B low common-good framing",
  "low_common_good",
  "low_common_good_lag1",
  "More common-good framing",
  -1,

  "C party disinformation",
  "disinformation",
  "party_disinformation_lag1",
  "Party disinformation",
  1,

  "D issue polarisation",
  "issue_polarisation",
  "issue_polarisation_lag1",
  "Issue polarisation",
  1,

  "E antagonistic camps",
  "antagonistic_camps",
  "antagonistic_camps_lag1",
  "Antagonistic camps",
  1
)

indicator_vars <- indicators$variable

# 3. Read established ESS inputs ------------------------------

ess <- readRDS(ess_file) |>
  select(
    cntry, essround,
    ppltrst, gini_swiid_lag1,
    age, gender, education_years
  ) |>
  mutate(cntry = as.character(cntry))

macro <- read_csv(
  macro_file,
  show_col_types = FALSE
) |>
  select(
    cntry, essround,
    log_gdp_pc_ppp_lag1,
    unemployment_lag1
  )

discourse <- read_csv(
  discourse_file,
  show_col_types = FALSE
) |>
  select(
    cntry, essround,
    party_hate_lag1,
    counterarg_disrespect_lag1,
    v2cacamps_lag1
  ) |>
  rename(
    antagonistic_camps_lag1 = v2cacamps_lag1
  )

check_keys(macro, keys, "Macro")
check_keys(discourse, keys, "Discourse")

# 4. Recreate the three additional contextual exposures -------
#
# Identical lag and fieldwork-year weighting to script 15.
# Only required for standardisation; no models are estimated.

country_lookup <- tribble(
  ~country_name,        ~cntry,
  "Albania",             "AL",
  "Austria",             "AT",
  "Belgium",             "BE",
  "Bulgaria",            "BG",
  "Croatia",             "HR",
  "Cyprus",              "CY",
  "Czech Republic",      "CZ",
  "Czechia",             "CZ",
  "Denmark",             "DK",
  "Estonia",             "EE",
  "Finland",             "FI",
  "France",              "FR",
  "Germany",             "DE",
  "Greece",              "GR",
  "Hungary",             "HU",
  "Iceland",             "IS",
  "Ireland",             "IE",
  "Israel",              "IL",
  "Italy",               "IT",
  "Kosovo",              "XK",
  "Latvia",              "LV",
  "Lithuania",           "LT",
  "Luxembourg",          "LU",
  "Malta",               "MT",
  "Montenegro",          "ME",
  "Netherlands",         "NL",
  "North Macedonia",     "MK",
  "Macedonia",           "MK",
  "Norway",              "NO",
  "Poland",              "PL",
  "Portugal",            "PT",
  "Romania",             "RO",
  "Russia",              "RU",
  "Russian Federation",  "RU",
  "Serbia",              "RS",
  "Slovakia",            "SK",
  "Slovenia",            "SI",
  "Spain",               "ES",
  "Sweden",              "SE",
  "Switzerland",         "CH",
  "Turkey",              "TR",
  "Türkiye",             "TR",
  "Ukraine",             "UA",
  "United Kingdom",      "GB"
)

new_vars <- c(
  "v2dlcommon",
  "v2smpardom",
  "v2smpolsoc"
)

vdem <- read_csv(
  vdem_file,
  col_select = all_of(
    c("country_name", "year", new_vars)
  ),
  show_col_types = FALSE
) |>
  inner_join(
    country_lookup,
    by = "country_name"
  ) |>
  semi_join(
    ess |> distinct(cntry),
    by = "cntry"
  ) |>
  select(
    cntry, year, all_of(new_vars)
  )

check_keys(vdem, c("cntry", "year"), "V-Dem")

timing_base <- read_csv(
  timing_base_file,
  show_col_types = FALSE
) |>
  select(
    cntry, essround, interview_year, n_respondents
  ) |>
  filter(essround <= 9)

timing_post9 <- read_csv(
  timing_post9_file,
  show_col_types = FALSE
) |>
  select(
    cntry, essround, interview_year, n_respondents
  ) |>
  filter(essround %in% 10:11)

timing <- bind_rows(
  timing_base, timing_post9
) |>
  filter(!is.na(interview_year))

check_keys(
  timing,
  c("cntry", "essround", "interview_year"),
  "Interview timing"
)

if (any(
  !is.finite(timing$n_respondents) |
    timing$n_respondents <= 0
)) {
  stop("Invalid timing counts.")
}

timing <- timing |>
  group_by(cntry, essround) |>
  mutate(
    share = n_respondents / sum(n_respondents)
  ) |>
  ungroup()

new_exposures <- timing |>
  mutate(
    context_year = interview_year - 1L
  ) |>
  left_join(
    vdem,
    by = c("cntry", "context_year" = "year")
  ) |>
  pivot_longer(
    cols = all_of(new_vars),
    names_to = "source",
    values_to = "value"
  ) |>
  group_by(cntry, essround, source) |>
  summarise(
    exposure = if (all(is.finite(value))) {
      weighted.mean(value, share)
    } else {
      NA_real_
    },
    .groups = "drop"
  ) |>
  pivot_wider(
    id_cols = c(cntry, essround),
    names_from = source,
    values_from = exposure
  ) |>
  transmute(
    cntry, essround,
    low_common_good_lag1 = -v2dlcommon,
    party_disinformation_lag1 = -v2smpardom,
    issue_polarisation_lag1 = -v2smpolsoc
  )

check_keys(new_exposures, keys, "New exposures")

# 5. Recover the common M5 analytical context ----------------

context_vars <- c(
  "gini_swiid_lag1",
  "log_gdp_pc_ppp_lag1",
  "unemployment_lag1",
  indicator_vars
)

model_rows <- ess |>
  left_join(macro, by = keys) |>
  left_join(discourse, by = keys) |>
  left_join(new_exposures, by = keys) |>
  filter(
    if_all(all_of(context_vars), is.finite),
    is.finite(ppltrst),
    is.finite(age),
    is.finite(education_years),
    !is.na(gender)
  )

context <- model_rows |>
  distinct(
    cntry, essround,
    across(all_of(indicator_vars))
  )

check_keys(context, keys, "Analytical context")

n_country_rounds <- nrow(context)
n_countries <- n_distinct(context$cntry)

cat("\nAnalytical context:\n")
cat(
  nrow(model_rows), "respondents,",
  n_countries, "countries,",
  n_country_rounds, "country-rounds\n"
)

if (
  nrow(model_rows) != 498461L ||
  n_countries != 38L ||
  n_country_rounds != 273L
) {
  stop("Sample does not match the established M5 context.")
}

# 6. Calculate the two standard deviations --------------------
#
# Within SD:
#   Equal weight for each of the 273 country-rounds.
#
# Between SD:
#   Equal weight for each of the 38 country means.
#
# Country means are calculated over observed country-rounds,
# matching the earlier REWB decompositions.

country_means <- context |>
  group_by(cntry) |>
  summarise(
    across(
      all_of(indicator_vars),
      mean
    ),
    .groups = "drop"
  )

mean_long <- country_means |>
  pivot_longer(
    cols = all_of(indicator_vars),
    names_to = "variable",
    values_to = "country_mean"
  )

context_long <- context |>
  pivot_longer(
    cols = all_of(indicator_vars),
    names_to = "variable",
    values_to = "value"
  ) |>
  left_join(
    mean_long,
    by = c("cntry", "variable")
  ) |>
  mutate(
    within_deviation = value - country_mean
  )

within_sd <- context_long |>
  group_by(variable) |>
  summarise(
    sd_within = sd(within_deviation),
    .groups = "drop"
  )

between_sd <- mean_long |>
  group_by(variable) |>
  summarise(
    sd_between = sd(country_mean),
    .groups = "drop"
  )

scales <- within_sd |>
  inner_join(
    between_sd,
    by = "variable"
  )

if (any(
  !is.finite(scales$sd_within) |
    scales$sd_within <= 0 |
    !is.finite(scales$sd_between) |
    scales$sd_between <= 0
)) {
  stop("Invalid within/between SD.")
}

# 7. Verify within-SDs against script 15 ----------------------
#
# This provides a direct check that the scaling sample and
# contextual decomposition have been reproduced correctly.

previous_sd <- read_csv(
  previous_sd_file,
  show_col_types = FALSE
) |>
  distinct(
    term,
    recorded_sd = sd_within
  )

recalculated_sd <- indicators |>
  select(prefix, variable) |>
  inner_join(
    scales,
    by = "variable"
  ) |>
  transmute(
    term = paste0(prefix, "_w"),
    recalculated_sd = sd_within
  )

sd_check <- recalculated_sd |>
  inner_join(
    previous_sd,
    by = "term"
  ) |>
  mutate(
    difference = recalculated_sd - recorded_sd
  )

if (
  nrow(sd_check) != 6L ||
  any(abs(sd_check$difference) > 1e-6)
) {
  stop(
    "Within-country SDs do not reproduce script 15. ",
    "Inspect sd_check before plotting."
  )
}

# 8. Read and standardise the model coefficients --------------

fixed_effects <- read_csv(
  fixed_file,
  show_col_types = FALSE
) |>
  select(
    model, term, estimate,
    std_error, conf_low, conf_high, p_value
  )

indicator_scales <- indicators |>
  inner_join(
    scales,
    by = "variable"
  )

plot_specifications <- bind_rows(
  indicator_scales |>
    transmute(
      model, indicator, orientation,
      term = paste0(prefix, "_w"),
      component = "Within-country change",
      standardising_sd = sd_within
    ),

  indicator_scales |>
    transmute(
      model, indicator, orientation,
      term = paste0(prefix, "_b"),
      component = "Between-country differences",
      standardising_sd = sd_between
    )
)

plot_data <- plot_specifications |>
  left_join(
    fixed_effects,
    by = c("model", "term")
  ) |>
  mutate(
    estimate_sd =
      orientation * estimate * standardising_sd,

    low_sd = if_else(
      orientation == 1,
      conf_low * standardising_sd,
      -conf_high * standardising_sd
    ),

    high_sd = if_else(
      orientation == 1,
      conf_high * standardising_sd,
      -conf_low * standardising_sd
    )
  )

if (
  nrow(plot_data) != 12L ||
  anyNA(plot_data$estimate_sd) ||
  anyNA(plot_data$low_sd) ||
  anyNA(plot_data$high_sd)
) {
  stop("Missing or duplicate coefficient matches.")
}

# 9. Plot -----------------------------------------------------

indicator_order <- c(
  "Party disinformation",
  "Party hate speech",
  "Issue polarisation",
  "Antagonistic camps",
  "Counterargument disrespect",
  "More common-good framing"
)

plot_data <- plot_data |>
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
      )
    )
  )

p3 <- ggplot(
  plot_data,
  aes(
    x = estimate_sd,
    y = indicator
  )
) +
  geom_vline(
    xintercept = 0,
    colour = "grey55",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  geom_segment(
    aes(
      x = low_sd,
      xend = high_sd,
      yend = indicator
    ),
    colour = "#345C80",
    linewidth = 0.85
  ) +
  geom_point(
    colour = "#125B9A",
    size = 2.8
  ) +
  facet_grid(
    . ~ component,
    scales = "free_x"
  ) +
  scale_x_continuous(
    labels = function(x) sprintf("%.2f", x)
  ) +
  labs(
    title = paste(
      "Political context and generalised trust:",
      "within versus between"
    ),
    subtitle = paste(
      "Coefficients from separate REWB models,",
      "standardised within each contextual level"
    ),
    x = paste(
      "Estimated trust difference",
      "per one component-specific SD"
    ),
    y = NULL,
    caption = paste0(
      "Points: estimates; lines: 95% Wald confidence intervals.\n",
      "Within-country SD: 273 country-round deviations. ",
      "Between-country SD: 38 country means.\n",
      "Panels use different horizontal scales. ",
      "Common-good framing is positively oriented.\n",
      "All models use the same 498,461 respondents."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(
      face = "bold", size = 11
    ),
    plot.title = element_text(
      face = "bold"
    ),
    plot.subtitle = element_text(
      size = 10
    ),
    plot.caption = element_text(
      size = 9,
      hjust = 0
    ),
    panel.spacing.x = unit(1.6, "lines"),
    plot.margin = margin(12, 18, 12, 12)
  )

print(p3)

# 10. Save figure and compact result table --------------------

figure_file <- file.path(
  figure_dir,
  "03_discourse_within_between_coefficients.png"
)

table_file <- file.path(
  figure_dir,
  "03_discourse_within_between_coefficients.csv"
)

ggsave(
  filename = figure_file,
  plot = p3,
  width = 13,
  height = 6.2,
  dpi = 240
)

write_csv(
  plot_data |>
    mutate(
      indicator = as.character(indicator),
      component = as.character(component)
    ) |>
    select(
      indicator, component, model,
      standardising_sd,
      estimate_sd, low_sd, high_sd,
      p_value
    ),
  table_file
)

cat("\n--- STANDARDISED RESULTS ---\n")

print(
  plot_data |>
    transmute(
      indicator = as.character(indicator),
      component = as.character(component),
      sd = round(standardising_sd, 3),
      estimate = round(estimate_sd, 3),
      lower = round(low_sd, 3),
      upper = round(high_sd, 3),
      p_value = signif(p_value, 3)
    ),
  n = Inf,
  width = Inf
)

cat(
  "\nFigure saved to:\n",
  figure_file, "\n"
)
