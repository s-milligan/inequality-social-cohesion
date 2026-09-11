library(dplyr)
library(here)

# ============================================================
# Cohesion dimensions: descriptive exploration
# ============================================================

# This script evaluates alternative ESS indicators that may capture
# dimensions of horizontal social cohesion alongside generalized trust.
#
# Main candidates:
#   - ppltrst: generalized social trust
#   - sclmeet: frequency of social meetings
#   - sclact: social activity relative to others of the same age
#   - inmdisc: someone available to discuss intimate/personal matters
#
# Input:
#   03_data/processed/ess_analysis_r1_r11_cohesion_migration.rds

ess <- readRDS(
  here::here(
    "03_data",
    "processed",
    "ess_analysis_r1_r11_cohesion_migration.rds"
  )
)

weighted_mean_safe <- function(x, w) {
  x <- vctrs::vec_data(x)
  w <- vctrs::vec_data(w)
  
  ok <- !is.na(x) & !is.na(w) &
    is.finite(x) & is.finite(w) &
    w > 0
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  stats::weighted.mean(x[ok], w[ok])
}

# ------------------------------------------------------------
# 1. Coverage by ESS round
# ------------------------------------------------------------

coverage <- ess |>
  group_by(essround) |>
  summarise(
    n = n(),
    n_ppltrst = sum(!is.na(ppltrst)),
    n_sclmeet = sum(!is.na(sclmeet)),
    pct_sclmeet = 100 * mean(!is.na(sclmeet)),
    n_sclact = sum(!is.na(sclact)),
    pct_sclact = 100 * mean(!is.na(sclact)),
    n_inmdisc = sum(!is.na(inmdisc)),
    pct_inmdisc = 100 * mean(!is.na(inmdisc)),
    .groups = "drop"
  )

print(coverage)

# ------------------------------------------------------------
# 2. Overall distributions
# ------------------------------------------------------------

sclmeet_distribution <- ess |>
  count(sclmeet) |>
  mutate(
    pct = 100 * n / sum(n)
  )

sclact_distribution <- ess |>
  count(sclact) |>
  mutate(
    pct = 100 * n / sum(n)
  )

print(sclmeet_distribution)
print(sclact_distribution)

# ------------------------------------------------------------
# 3. Individual-level associations
# ------------------------------------------------------------

correlations <- ess |>
  summarise(
    cor_trust_sclmeet = cor(
      ppltrst,
      sclmeet,
      use = "pairwise.complete.obs"
    ),
    cor_trust_sclact = cor(
      ppltrst,
      sclact,
      use = "pairwise.complete.obs"
    ),
    cor_sclmeet_sclact = cor(
      sclmeet,
      sclact,
      use = "pairwise.complete.obs"
    )
  )

print(correlations)

correlations_by_round <- ess |>
  group_by(essround) |>
  summarise(
    cor_trust_sclmeet = cor(
      ppltrst,
      sclmeet,
      use = "pairwise.complete.obs"
    ),
    cor_trust_sclact = cor(
      ppltrst,
      sclact,
      use = "pairwise.complete.obs"
    ),
    cor_sclmeet_sclact = cor(
      sclmeet,
      sclact,
      use = "pairwise.complete.obs"
    ),
    .groups = "drop"
  )

print(correlations_by_round)

# ------------------------------------------------------------
# 4. Weighted country-round means
# ------------------------------------------------------------

cohesion_country_round <- ess |>
  group_by(cntry, essround) |>
  summarise(
    mean_trust = weighted_mean_safe(
      ppltrst,
      analysis_weight
    ),
    mean_sclmeet = weighted_mean_safe(
      sclmeet,
      analysis_weight
    ),
    mean_sclact = weighted_mean_safe(
      sclact,
      analysis_weight
    ),
    n = n(),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 5. Within-country variation across rounds
# ------------------------------------------------------------

within_country_variation <- cohesion_country_round |>
  group_by(cntry) |>
  summarise(
    rounds = n(),
    sd_trust = sd(mean_trust, na.rm = TRUE),
    sd_sclmeet = sd(mean_sclmeet, na.rm = TRUE),
    sd_sclact = sd(mean_sclact, na.rm = TRUE),
    range_trust = max(mean_trust, na.rm = TRUE) -
      min(mean_trust, na.rm = TRUE),
    range_sclmeet = max(mean_sclmeet, na.rm = TRUE) -
      min(mean_sclmeet, na.rm = TRUE),
    range_sclact = max(mean_sclact, na.rm = TRUE) -
      min(mean_sclact, na.rm = TRUE),
    .groups = "drop"
  )

print(
  within_country_variation |>
    filter(rounds >= 5),
  n = Inf
)

# ------------------------------------------------------------
# 6. Associations at the country-round level
# ------------------------------------------------------------

country_round_correlations <- cohesion_country_round |>
  summarise(
    cor_trust_sclmeet = cor(
      mean_trust,
      mean_sclmeet,
      use = "pairwise.complete.obs"
    ),
    cor_trust_sclact = cor(
      mean_trust,
      mean_sclact,
      use = "pairwise.complete.obs"
    ),
    cor_sclmeet_sclact = cor(
      mean_sclmeet,
      mean_sclact,
      use = "pairwise.complete.obs"
    )
  )

print(country_round_correlations)

# ------------------------------------------------------------
# 7. Between- and within-country associations
# ------------------------------------------------------------

cohesion_country_round_decomp <- cohesion_country_round |>
  group_by(cntry) |>
  mutate(
    trust_between = mean(mean_trust, na.rm = TRUE),
    sclmeet_between = mean(mean_sclmeet, na.rm = TRUE),
    sclact_between = mean(mean_sclact, na.rm = TRUE),
    trust_within = mean_trust - trust_between,
    sclmeet_within = mean_sclmeet - sclmeet_between,
    sclact_within = mean_sclact - sclact_between
  ) |>
  ungroup()

between_correlations <- cohesion_country_round_decomp |>
  distinct(
    cntry,
    trust_between,
    sclmeet_between,
    sclact_between
  ) |>
  summarise(
    cor_trust_sclmeet = cor(
      trust_between,
      sclmeet_between,
      use = "pairwise.complete.obs"
    ),
    cor_trust_sclact = cor(
      trust_between,
      sclact_between,
      use = "pairwise.complete.obs"
    ),
    cor_sclmeet_sclact = cor(
      sclmeet_between,
      sclact_between,
      use = "pairwise.complete.obs"
    )
  )

print(between_correlations)

within_correlations <- cohesion_country_round_decomp |>
  summarise(
    cor_trust_sclmeet = cor(
      trust_within,
      sclmeet_within,
      use = "pairwise.complete.obs"
    ),
    cor_trust_sclact = cor(
      trust_within,
      sclact_within,
      use = "pairwise.complete.obs"
    ),
    cor_sclmeet_sclact = cor(
      sclmeet_within,
      sclact_within,
      use = "pairwise.complete.obs"
    )
  )

print(within_correlations)

# ------------------------------------------------------------
# 8. Round-level patterns
# ------------------------------------------------------------

round_means <- cohesion_country_round |>
  group_by(essround) |>
  summarise(
    trust = mean(mean_trust, na.rm = TRUE),
    sclmeet = mean(mean_sclmeet, na.rm = TRUE),
    sclact = mean(mean_sclact, na.rm = TRUE),
    .groups = "drop"
  )

print(round_means)

# Sensitivity check excluding Rounds 10-11
within_r1_r9 <- cohesion_country_round |>
  filter(essround <= 9) |>
  group_by(cntry) |>
  filter(n() >= 3) |>
  mutate(
    trust_within = mean_trust - mean(mean_trust, na.rm = TRUE),
    sclmeet_within = mean_sclmeet - mean(mean_sclmeet, na.rm = TRUE),
    sclact_within = mean_sclact - mean(mean_sclact, na.rm = TRUE)
  ) |>
  ungroup() |>
  summarise(
    cor_trust_sclmeet = cor(
      trust_within,
      sclmeet_within,
      use = "pairwise.complete.obs"
    ),
    cor_trust_sclact = cor(
      trust_within,
      sclact_within,
      use = "pairwise.complete.obs"
    ),
    cor_sclmeet_sclact = cor(
      sclmeet_within,
      sclact_within,
      use = "pairwise.complete.obs"
    )
  )

print(within_r1_r9)

# ------------------------------------------------------------
# 9. Two-way country + round adjusted associations
# ------------------------------------------------------------

cr_complete <- cohesion_country_round |>
  filter(
    !is.na(mean_trust),
    !is.na(mean_sclmeet),
    !is.na(mean_sclact)
  )

trust_fe <- lm(
  mean_trust ~ factor(cntry) + factor(essround),
  data = cr_complete
)

sclmeet_fe <- lm(
  mean_sclmeet ~ factor(cntry) + factor(essround),
  data = cr_complete
)

sclact_fe <- lm(
  mean_sclact ~ factor(cntry) + factor(essround),
  data = cr_complete
)

cr_residualised <- cr_complete |>
  mutate(
    trust_resid = residuals(trust_fe),
    sclmeet_resid = residuals(sclmeet_fe),
    sclact_resid = residuals(sclact_fe)
  )

two_way_correlations <- cr_residualised |>
  summarise(
    cor_trust_sclmeet = cor(
      trust_resid,
      sclmeet_resid
    ),
    cor_trust_sclact = cor(
      trust_resid,
      sclact_resid
    ),
    cor_sclmeet_sclact = cor(
      sclmeet_resid,
      sclact_resid
    )
  )

print(two_way_correlations)
