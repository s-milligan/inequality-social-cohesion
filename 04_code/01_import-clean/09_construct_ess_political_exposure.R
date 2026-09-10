# ------------------------------------------------------------
# Construct ESS political exposure variables
# Project: Inequality and social cohesion
#
# Purpose:
#   Prepare respondent-level political variables from the
#   ESS Rounds 1-11 supplementary political extract.
#
# This script is the executable counterpart to:
#   09_construct_ess_political_exposure.qmd
#
# Key decisions documented in the companion Quarto notebook:
#   - Party labels, not local numeric codes, are used for
#     substantive party identification.
#   - Germany uses ballot 2 (party-list Zweitstimme).
#   - Lithuania uses ballot 1 (nationwide party-list vote).
#   - Respondents with only non-preferred ballots remain
#     missing on the preferred party-vote measure.
#   - Party vote and party closeness remain separate measures.
#   - Political interest, ideology, redistribution, immigration
#     attitudes, and party attachment remain distinct constructs.
#   - Party-linkage coverage is assessed conditionally among
#     voters / party identifiers.
#   - Tagged ESS missing values are treated as explicit
#     nonresponse rather than linkage failures.
# ------------------------------------------------------------


# --------------------------------------------------------------------
# Setup
# --------------------------------------------------------------------
# Load packages used throughout the preparation and diagnostics.

library(dplyr)
library(haven)
library(here)
library(stringr)


# --------------------------------------------------------------------
# 1. Import the supplementary ESS political extract
# --------------------------------------------------------------------
# Read the separate Rounds 1-11 political supplement. The existing ESS
# analysis data remain the project backbone and will be merged later.

ess_pol_raw <- read_dta(
  here(
    "03_data",
    "raw",
    "ess",
    "ess_rounds1-11_political_variables.dta"
  )
)


# Basic coverage

basic_summary <- tibble(
  n_respondents = nrow(ess_pol_raw),
  n_variables = ncol(ess_pol_raw),
  first_round = min(ess_pol_raw$essround, na.rm = TRUE),
  last_round = max(ess_pol_raw$essround, na.rm = TRUE),
  n_countries = n_distinct(ess_pol_raw$cntry)
)

basic_summary


# Basic coverage

sort(unique(ess_pol_raw$essround))


# --------------------------------------------------------------------
# 2. Verify respondent identifiers
# --------------------------------------------------------------------
# Confirm that essround + cntry + idno uniquely identify respondents before
# any joins or reshaping.

key_check <- ess_pol_raw %>%
  count(essround, cntry, idno) %>%
  filter(n > 1)

cat("Duplicate respondent keys:", nrow(key_check), "\n")
stopifnot(nrow(key_check) == 0)


# --------------------------------------------------------------------
# 3. Identify the country-specific party variables
# --------------------------------------------------------------------
# Identify vote and party-closeness fields. ESS stores these in many
# country- and round-specific variables; Stata value labels contain the
# substantive party names.

party_vote_vars <- names(ess_pol_raw)[
  grepl("^prtv", names(ess_pol_raw), ignore.case = TRUE)
]

party_close_vars <- names(ess_pol_raw)[
  grepl("^prtcl", names(ess_pol_raw), ignore.case = TRUE)
]

tibble(
  measure = c("Party vote", "Party closeness"),
  n_variables = c(length(party_vote_vars), length(party_close_vars))
)


# --------------------------------------------------------------------
# 4. Inventory the country-specific party variables
# --------------------------------------------------------------------
# Document where each party variable is observed by round and country before
# collapsing the wide structure.

get_var_info <- function(v, type) {
  x <- ess_pol_raw[[v]]
  observed <- !is.na(x)
  var_label <- attr(x, "label")

  if (is.null(var_label)) var_label <- NA_character_

  tibble(
    variable = v,
    type = type,
    label = var_label,
    n_nonmissing = sum(observed),
    rounds = paste(sort(unique(ess_pol_raw$essround[observed])), collapse = ", "),
    countries = paste(
      sort(unique(as.character(ess_pol_raw$cntry[observed]))),
      collapse = ", "
    )
  )
}

party_var_inventory <- bind_rows(
  lapply(party_vote_vars, get_var_info, type = "vote"),
  lapply(party_close_vars, get_var_info, type = "closeness")
)


party_var_inventory %>%
  filter(n_nonmissing > 0) %>%
  arrange(type, countries, rounds) %>%
  slice_head(n = 30)


# --------------------------------------------------------------------
# 5. Collapse the wide party variables into long form
# --------------------------------------------------------------------
# Keep only substantive observed responses and preserve the source variable,
# original code, and labelled party name for traceability.

extract_party_responses <- function(vars, type) {
  bind_rows(
    lapply(vars, function(v) {
      x <- ess_pol_raw[[v]]

      # Tagged ESS nonresponse codes are NA to is.na().
      # Keep only substantive observed responses here.
      keep <- !is.na(x)
      if (!any(keep)) return(NULL)

      var_label <- attr(x, "label")
      if (is.null(var_label)) var_label <- NA_character_

      tibble(
        essround = ess_pol_raw$essround[keep],
        cntry = as.character(ess_pol_raw$cntry[keep]),
        idno = ess_pol_raw$idno[keep],
        source_var = v,
        source_label = var_label,
        party_code = as.numeric(x[keep]),
        party_label = as.character(
          haven::as_factor(x[keep], levels = "default")
        ),
        party_measure = type
      )
    })
  )
}

party_vote_long <- extract_party_responses(
  party_vote_vars,
  type = "vote"
)

party_close_long <- extract_party_responses(
  party_close_vars,
  type = "closeness"
)

tibble(
  measure = c("Party vote", "Party closeness"),
  n_records = c(nrow(party_vote_long), nrow(party_close_long))
)


# --------------------------------------------------------------------
# 6. Diagnose multiple party records per respondent
# --------------------------------------------------------------------
# Check whether respondents have more than one substantive vote or closeness
# record.

vote_records_per_person <- party_vote_long %>%
  count(essround, cntry, idno, name = "n_vote_records")

close_records_per_person <- party_close_long %>%
  count(essround, cntry, idno, name = "n_close_records")

vote_record_distribution <- vote_records_per_person %>%
  count(n_vote_records, name = "n_respondents") %>%
  arrange(n_vote_records)

close_record_distribution <- close_records_per_person %>%
  count(n_close_records, name = "n_respondents") %>%
  arrange(n_close_records)

vote_record_distribution
close_record_distribution


# --------------------------------------------------------------------
# 7. Locate multiple-ballot systems
# --------------------------------------------------------------------
# Identify the country-rounds requiring an explicit ballot-selection rule.

vote_multiplicity_by_country <- party_vote_long %>%
  count(essround, cntry, idno, name = "n_vote_records") %>%
  count(essround, cntry, n_vote_records, name = "n_respondents") %>%
  filter(n_vote_records > 1) %>%
  arrange(desc(n_vote_records), cntry, essround)

vote_multiplicity_by_country


multi_vote_country_rounds <- vote_multiplicity_by_country %>%
  distinct(essround, cntry)

multi_vote_sources <- party_vote_long %>%
  semi_join(
    multi_vote_country_rounds,
    by = c("essround", "cntry")
  ) %>%
  distinct(essround, cntry, source_var, source_label) %>%
  arrange(cntry, essround, source_var)

multi_vote_sources


# --------------------------------------------------------------------
# 8. Define the preferred ballot
# --------------------------------------------------------------------
# Use the party-list vote for Germany and Lithuania so the later V-Party
# linkage reflects a party choice rather than a constituency-candidate
# choice.

ballot_rules <- tibble(
  cntry = c("DE", "LT"),
  preferred_ballot = c(2L, 1L),
  rationale = c(
    "Party-list vote (Zweitstimme)",
    "Nationwide party-list vote"
  )
)

ballot_rules


# --------------------------------------------------------------------
# 9. Select one preferred party-vote record per respondent
# --------------------------------------------------------------------
# Apply the explicit ballot rules and retain the source ESS variable as an
# audit trail.

party_vote_selected <- party_vote_long %>%
  mutate(
    ballot = case_when(
      cntry %in% c("DE", "LT") ~
        as.integer(str_extract(source_var, "[123]$")),
      TRUE ~ 1L
    )
  ) %>%
  left_join(
    ballot_rules %>% select(cntry, preferred_ballot),
    by = "cntry"
  ) %>%
  mutate(
    preferred = case_when(
      is.na(preferred_ballot) ~ TRUE,
      ballot == preferred_ballot ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(preferred) %>%
  select(
    essround,
    cntry,
    idno,
    party_vote_code = party_code,
    party_vote_label = party_label,
    party_vote_source = source_var
  )

vote_selected_check <- party_vote_selected %>%
  count(essround, cntry, idno) %>%
  filter(n > 1)

tibble(
  selected_party_vote_records = nrow(party_vote_selected),
  duplicate_respondents = nrow(vote_selected_check)
)


# --------------------------------------------------------------------
# 10. Diagnose respondents lost through preferred-ballot selection
# --------------------------------------------------------------------
# Quantify respondents who have only a non-preferred ballot; do not
# substitute incomparable ballot types.

all_vote_respondents <- party_vote_long %>%
  distinct(essround, cntry, idno)

selected_vote_respondents <- party_vote_selected %>%
  distinct(essround, cntry, idno)

vote_not_selected <- all_vote_respondents %>%
  anti_join(
    selected_vote_respondents,
    by = c("essround", "cntry", "idno")
  )

vote_not_selected %>%
  count(essround, cntry, name = "n") %>%
  arrange(cntry, essround)


# --------------------------------------------------------------------
# 11. Create the harmonised party-closeness record
# --------------------------------------------------------------------
# Party closeness has at most one substantive record per respondent, so no
# ballot rule is required.

party_close_selected <- party_close_long %>%
  select(
    essround,
    cntry,
    idno,
    party_close_code = party_code,
    party_close_label = party_label,
    party_close_source = source_var
  )

party_close_check <- party_close_selected %>%
  count(essround, cntry, idno) %>%
  filter(n > 1)

tibble(
  selected_party_close_records = nrow(party_close_selected),
  duplicate_respondents = nrow(party_close_check)
)


# --------------------------------------------------------------------
# 12. Select the ordinary political variables
# --------------------------------------------------------------------
# Retain political attention, alignment, redistribution, immigration, and
# political-trust variables as separate constructs.

political_vars <- c(
  "essround",
  "cntry",
  "idno",
  "vote",
  "clsprty",
  "prtdgcl",
  "polintr",
  "lrscale",
  "gincdif",
  "imbgeco",
  "imueclt",
  "imwbcnt",
  "imsmetn",
  "imdfetn",
  "impcntr",
  "trstprl",
  "trstplt",
  "trstprt",
  "stfdem"
)

ess_pol_core <- ess_pol_raw %>%
  select(any_of(political_vars))


# --------------------------------------------------------------------
# 13. Build the respondent-level political supplement
# --------------------------------------------------------------------
# Join the harmonised party fields to the ordinary political variables using
# the unique respondent keys.

ess_political <- ess_pol_core %>%
  left_join(
    party_vote_selected,
    by = c("essround", "cntry", "idno")
  ) %>%
  left_join(
    party_close_selected,
    by = c("essround", "cntry", "idno")
  )

duplicate_keys <- ess_political %>%
  count(essround, cntry, idno) %>%
  filter(n > 1)

tibble(
  n_respondents = nrow(ess_political),
  n_variables = ncol(ess_political),
  duplicate_keys = nrow(duplicate_keys)
)


# --------------------------------------------------------------------
# 14. Inspect coverage of the political variables
# --------------------------------------------------------------------
# Describe raw item coverage by round; a non-missing vote item is not yet
# equivalent to having voted.

political_coverage_round <- ess_political %>%
  group_by(essround) %>%
  summarise(
    n = n(),
    n_vote_item_response = sum(!is.na(vote)),
    n_party_vote = sum(!is.na(party_vote_label)),
    n_closeness_item_response = sum(!is.na(clsprty)),
    n_party_close = sum(!is.na(party_close_label)),
    n_polintr = sum(!is.na(polintr)),
    n_lrscale = sum(!is.na(lrscale)),
    n_gincdif = sum(!is.na(gincdif)),
    n_immigration_complete = sum(
      !is.na(imbgeco) &
        !is.na(imueclt) &
        !is.na(imwbcnt)
    ),
    .groups = "drop"
  )

political_coverage_round


# --------------------------------------------------------------------
# 15. Confirm the coding of the core variables
# --------------------------------------------------------------------
# Retain the original Stata label definitions for reproducibility before
# deriving new variables.

vars_to_inspect <- c(
  "vote", "clsprty", "prtdgcl", "polintr", "lrscale", "gincdif",
  "imbgeco", "imueclt", "imwbcnt", "imsmetn", "imdfetn", "impcntr",
  "trstprl", "trstplt", "trstprt", "stfdem"
)

coding_labels <- lapply(
  vars_to_inspect,
  function(v) {
    if (v %in% names(ess_pol_raw)) attr(ess_pol_raw[[v]], "labels") else NULL
  }
)

names(coding_labels) <- vars_to_inspect


# --------------------------------------------------------------------
# 16. Recode the core individual-level political variables
# --------------------------------------------------------------------
# Create transparent derived variables with higher values pointing in
# substantively intuitive directions.

ess_political <- ess_political %>%
  mutate(
    voted = case_when(
      as.numeric(vote) == 1 ~ 1L,
      as.numeric(vote) == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),

    close_to_party = case_when(
      as.numeric(clsprty) == 1 ~ 1L,
      as.numeric(clsprty) == 2 ~ 0L,
      TRUE ~ NA_integer_
    ),

    # Higher values = greater political interest
    political_interest = case_when(
      !is.na(polintr) ~ 5 - as.numeric(polintr),
      TRUE ~ NA_real_
    ),

    lr_position = as.numeric(lrscale),

    # 0 = centre, 5 = either extreme
    lr_extremity = case_when(
      !is.na(lrscale) ~ abs(as.numeric(lrscale) - 5),
      TRUE ~ NA_real_
    ),

    # Higher values = stronger support for reducing income differences
    redistribution = case_when(
      !is.na(gincdif) ~ 6 - as.numeric(gincdif),
      TRUE ~ NA_real_
    ),

    # Higher values = stronger party attachment
    party_closeness_strength = case_when(
      !is.na(prtdgcl) ~ 5 - as.numeric(prtdgcl),
      TRUE ~ NA_real_
    )
  )


# --------------------------------------------------------------------
# 17. Assess usable party-linkage coverage
# --------------------------------------------------------------------
# Evaluate named-party linkage among voters and among respondents who say
# they feel close to a party; exclude generic Other categories from
# matching.

party_linkage_coverage <- ess_political %>%
  mutate(
    party_vote_identified =
      !is.na(party_vote_label) &
      !grepl("^other", party_vote_label, ignore.case = TRUE),

    party_close_identified =
      !is.na(party_close_label) &
      !grepl("^other", party_close_label, ignore.case = TRUE)
  ) %>%
  group_by(essround, cntry) %>%
  summarise(
    n = n(),
    n_voters = sum(voted == 1, na.rm = TRUE),
    n_voters_with_party = sum(
      voted == 1 & party_vote_identified,
      na.rm = TRUE
    ),
    pct_voters_with_party = 100 * n_voters_with_party / n_voters,
    n_party_identifiers = sum(close_to_party == 1, na.rm = TRUE),
    n_identifiers_with_party = sum(
      close_to_party == 1 & party_close_identified,
      na.rm = TRUE
    ),
    pct_identifiers_with_party =
      100 * n_identifiers_with_party / n_party_identifiers,
    .groups = "drop"
  )


# Round-level coverage

party_linkage_round <- ess_political %>%
  mutate(
    party_vote_identified =
      !is.na(party_vote_label) &
      !grepl("^other", party_vote_label, ignore.case = TRUE),
    party_close_identified =
      !is.na(party_close_label) &
      !grepl("^other", party_close_label, ignore.case = TRUE)
  ) %>%
  group_by(essround) %>%
  summarise(
    n = n(),
    n_voters = sum(voted == 1, na.rm = TRUE),
    n_voters_with_party = sum(
      voted == 1 & party_vote_identified,
      na.rm = TRUE
    ),
    pct_voters_with_party = 100 * n_voters_with_party / n_voters,
    n_party_identifiers = sum(close_to_party == 1, na.rm = TRUE),
    n_identifiers_with_party = sum(
      close_to_party == 1 & party_close_identified,
      na.rm = TRUE
    ),
    pct_identifiers_with_party =
      100 * n_identifiers_with_party / n_party_identifiers,
    .groups = "drop"
  )

party_linkage_round


# Country-round anomalies

low_linkage_country_rounds <- party_linkage_coverage %>%
  filter(
    n_voters > 0,
    pct_voters_with_party < 70
  ) %>%
  arrange(pct_voters_with_party)

low_linkage_country_rounds


# --------------------------------------------------------------------
# 18. Decompose party-vote linkage failures
# --------------------------------------------------------------------
# Separate named-party linkage, Other responses, and cases without a
# substantive party value.

vote_linkage_diagnostics <- ess_political %>%
  mutate(
    party_vote_other =
      !is.na(party_vote_label) &
      grepl("^other", party_vote_label, ignore.case = TRUE),
    party_vote_named =
      !is.na(party_vote_label) &
      !party_vote_other
  ) %>%
  filter(voted == 1) %>%
  group_by(essround, cntry) %>%
  summarise(
    n_voters = n(),
    n_named_party = sum(party_vote_named),
    n_other_party = sum(party_vote_other),
    n_no_substantive_party = sum(is.na(party_vote_label)),
    pct_named_party = 100 * n_named_party / n_voters,
    pct_other_party = 100 * n_other_party / n_voters,
    pct_no_substantive_party =
      100 * n_no_substantive_party / n_voters,
    .groups = "drop"
  ) %>%
  arrange(pct_named_party)

vote_linkage_overall <- vote_linkage_diagnostics %>%
  summarise(
    voters = sum(n_voters),
    named = sum(n_named_party),
    other = sum(n_other_party),
    no_substantive_party = sum(n_no_substantive_party),
    pct_named = 100 * named / voters,
    pct_other = 100 * other / voters,
    pct_no_substantive_party = 100 * no_substantive_party / voters
  )

vote_linkage_overall


# --------------------------------------------------------------------
# 19. Check ESS tagged missing values
# --------------------------------------------------------------------
# Audit haven tagged missings so explicit ESS nonresponse is not mistaken
# for a failed party-variable linkage.

get_missing_tags <- function(v) {
  labs <- attr(ess_pol_raw[[v]], "labels")

  tibble(
    label = names(labs),
    tagged_missing = haven::is_tagged_na(labs),
    tag = haven::na_tag(labs)
  ) %>%
    filter(tagged_missing)
}

missing_tag_map <- get_missing_tags("prtvde2")
missing_tag_map


# Missingness audit examples

audit_party_missing <- function(country, round, source_var) {
  x <- ess_pol_raw[[source_var]]

  voters <- (
    ess_pol_raw$essround == round &
      ess_pol_raw$cntry == country &
      as.numeric(ess_pol_raw$vote) == 1
  )

  n_missing <- sum(is.na(x[voters]))
  n_tagged <- sum(haven::is_tagged_na(x[voters]))

  tibble(
    cntry = country,
    essround = round,
    source_var = source_var,
    n_voters = sum(voters, na.rm = TRUE),
    n_missing_party_item = n_missing,
    n_tagged_missing = n_tagged,
    n_untagged_missing = n_missing - n_tagged
  )
}

de1_source <- party_vote_selected %>%
  filter(cntry == "DE", essround == 1) %>%
  distinct(party_vote_source) %>%
  pull(party_vote_source)

bg9_source <- party_vote_selected %>%
  filter(cntry == "BG", essround == 9) %>%
  distinct(party_vote_source) %>%
  pull(party_vote_source)

missingness_examples <- bind_rows(
  audit_party_missing("DE", 1, de1_source),
  audit_party_missing("BG", 9, bg9_source)
)

missingness_examples


# --------------------------------------------------------------------
# 20. Create the ESS party inventory for V-Party matching
# --------------------------------------------------------------------
# Reduce respondent-level party information to country-round-party entries
# in preparation for a party crosswalk.

ess_party_inventory <- party_vote_selected %>%
  filter(
    !is.na(party_vote_label),
    !grepl("^other", party_vote_label, ignore.case = TRUE)
  ) %>%
  count(
    cntry,
    essround,
    party_vote_label,
    party_vote_source,
    name = "n_respondents"
  ) %>%
  arrange(
    cntry,
    essround,
    desc(n_respondents),
    party_vote_label
  )

tibble(
  country_round_party_entries = nrow(ess_party_inventory),
  unique_country_party_labels = ess_party_inventory %>%
    distinct(cntry, party_vote_label) %>%
    nrow()
)


# --------------------------------------------------------------------
# 21. Build the candidate crosswalk table
# --------------------------------------------------------------------
# Collapse repeated rounds to unique country x party-label combinations and
# quantify the matching burden.

ess_party_crosswalk <- ess_party_inventory %>%
  group_by(cntry, party_vote_label) %>%
  summarise(
    first_round = min(essround),
    last_round = max(essround),
    n_rounds = n_distinct(essround),
    n_respondents = sum(n_respondents),
    .groups = "drop"
  ) %>%
  arrange(
    cntry,
    desc(n_respondents),
    party_vote_label
  )

party_crosswalk_by_country <- ess_party_crosswalk %>%
  count(
    cntry,
    name = "n_unique_party_labels"
  ) %>%
  arrange(desc(n_unique_party_labels))

party_crosswalk_by_country


# --------------------------------------------------------------------
# 22. Save the crosswalk candidates
# --------------------------------------------------------------------
# Write only the compact candidate crosswalk to processed data; the
# respondent-level supplement remains unfinished until V-Party linkage is
# complete.

dir.create(
  here("03_data", "processed"),
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  ess_party_crosswalk,
  here(
    "03_data",
    "processed",
    "ess_party_crosswalk_candidates.csv"
  ),
  row.names = FALSE
)


# --------------------------------------------------------------------
# Next step
# --------------------------------------------------------------------
# Import V-Party, link ESS country x party labels to stable V-Party
# identifiers, validate the crosswalk, and then attach party-level
# divisive-discourse measures before merging into the main analysis data.
