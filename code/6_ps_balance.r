# Propensity score, part 2: is the weighted cohort balanced? ------------------
# Input:  output/ps_cohort.rds   the cohort with ps and weight, from script 5
# Output: output/ps_balance.rds  the cobalt balance table, before and after weighting
#
# The weights from script 5 are meant to make drug A and drug B look alike on
# the confounders, as if the drug had been given at random. This script checks
# whether they did. If the groups are still unbalanced after weighting, the
# effect in script 7 is still confounded, so this check comes first.
#
# The yardstick is the standardised mean difference (SMD): the difference in
# means between the two drugs, divided by the pooled SD. It does not depend on
# the sample size, unlike a p-value. An absolute SMD below 0.1 counts as
# balanced. cobalt takes B minus A, so a negative SMD means drug A has the
# higher mean.

#%%@note step 1: packages and paths -------------------------------------------

# install.packages(c("cobalt", "survey"))

library(dplyr)      # verbs
library(ggplot2)    # theme_classic() on the cobalt plots
library(cobalt)     # bal.tab(), love.plot(), bal.plot(): balance for PS work
library(gtsummary)  # Table 1 with an SMD column
library(survey)     # svydesign(): the weighted cohort for the weighted Table 1

path_ps      <- "output/ps_cohort.rds"
path_balance <- "output/ps_balance.rds"

#%%@note step 2: import --------------------------------------------------------

df <- readRDS(path_ps)

#%%@note step 3: SMD before and after weighting --------------------------------

# The formula holds the same five confounders that went into the PS model.
# un = TRUE adds the column before weighting (Diff.Un) beside the column after
# weighting (Diff.Adj). s.d.denom = "pooled" divides by the pooled SD of the
# two drugs; binary = "std" gives sex an SMD too, not a raw difference.
# thresholds = c(m = 0.1) adds a column that says Balanced or Not Balanced.
# The bottom of the print-out has the effective sample size (step 6).
bal <- cobalt::bal.tab(
  treatment ~ age + sex + bmi + dm_duration + fbs_m0,
  data = df,
  weights = "weight",
  estimand = "ATE",
  s.d.denom = "pooled",
  binary = "std",
  continuous = "std",
  un = TRUE,
  thresholds = c(m = 0.1)
)

bal

# the confounders still at 0.1 or above after weighting, kept here to look at
still_unbalanced <- bal$Balance |>
  tibble::rownames_to_column("variable") |>
  dplyr::filter(abs(Diff.Adj) >= 0.1)

still_unbalanced

#%%@note step 4: love plot -----------------------------------------------------

# One dot per confounder: before weighting (Unadjusted) and after (Adjusted).
# Every Adjusted dot should sit left of the dashed line at 0.1.
p_love <- cobalt::love.plot(
  bal,
  abs = TRUE,
  thresholds = c(m = 0.1),
  var.order = "unadjusted",
  var.names = c(
    age = "Age (years)",
    sex_Male = "Sex, male",
    bmi = "Body mass index (kg/m²)",
    dm_duration = "Diabetes duration (years)",
    fbs_m0 = "FBS at month 0 (mg/dL)"
  ),
  title = NULL
) +
  ggplot2::labs(x = "Absolute standardised mean difference") +
  ggplot2::theme_classic()

p_love

#%%@note step 5: mirror histogram, the PS before and after weighting ----------

# Drug A on top, drug B below. Left panel: the raw cohort.
# Right panel: the weighted cohort, the pseudo-population.
# After weighting, the two halves should look like mirror images.
p_mirror <- cobalt::bal.plot(
  treatment ~ ps,
  data = df,
  weights = "weight",
  var.name = "ps",
  which = "both",
  type = "histogram",
  mirror = TRUE
) +
  ggplot2::labs(
    title = NULL,
    x = "Propensity score, probability of drug B"
  ) +
  ggplot2::theme_classic()

p_mirror

#%%@note step 6: effective sample size -----------------------------------------

# Large weights mean a few patients stand in for many, so the weighted cohort
# carries less information than its row count. The effective sample size (ESS)
# says how many unweighted patients it is worth. A big drop from n warns that
# the estimate in script 7 will be imprecise.
bal$Observations

#%%@note step 7: Table 1 with SMD, before and after weighting ------------------

# Before weighting: the plain cohort. add_difference() with "smd" adds one
# SMD per row. No p values: the question is the size of the gap, not chance.
# gtsummary takes A minus B, the opposite sign to cobalt. Read the size.
# modify_column_hide() drops the CI of the SMD, which nobody reads here.
tbl_raw <- df |>
  gtsummary::tbl_summary(
    include = c(age, sex, bmi, dm_duration, fbs_m0),
    by = treatment,
    label = list(
      age ~ "Age (years)",
      sex ~ "Sex",
      bmi ~ "Body mass index (kg/m²)",
      dm_duration ~ "Diabetes duration (years)",
      fbs_m0 ~ "FBS at month 0 (mg/dL)"
    ),
    statistic = list(
      c(age, bmi, fbs_m0) ~ "{mean} ({sd})",
      dm_duration ~ "{median} ({p25}, {p75})"
    )
  ) |>
  gtsummary::add_difference(everything() ~ "smd") |>
  gtsummary::modify_header(estimate = "**SMD**") |>
  gtsummary::modify_column_hide(conf.low)

# After weighting: the same table on the weighted cohort. svydesign() tells
# gtsummary that each row counts as `weight` patients, so N in the header is
# the size of the pseudo-population (the sum of the weights), not 600.
df_svy <- survey::svydesign(
  ids = ~1,
  weights = ~weight,
  data = df
)

tbl_wt <- df_svy |>
  gtsummary::tbl_svysummary(
    include = c(age, sex, bmi, dm_duration, fbs_m0),
    by = treatment,
    label = list(
      age ~ "Age (years)",
      sex ~ "Sex",
      bmi ~ "Body mass index (kg/m²)",
      dm_duration ~ "Diabetes duration (years)",
      fbs_m0 ~ "FBS at month 0 (mg/dL)"
    ),
    statistic = list(
      c(age, bmi, fbs_m0) ~ "{mean} ({sd})",
      dm_duration ~ "{median} ({p25}, {p75})"
    )
  ) |>
  gtsummary::add_difference(everything() ~ "smd") |>
  gtsummary::modify_header(estimate = "**SMD**") |>
  gtsummary::modify_column_hide(conf.low)

tbl_ps <- gtsummary::tbl_merge(
  list(tbl_raw, tbl_wt),
  tab_spanner = c("**Before weighting**", "**After weighting**")
)

tbl_ps

#%%@note step 8: check and save -----------------------------------------------

stopifnot(
  nrow(df) == 600,
  nrow(bal$Balance) == 5  # 5 confounders
)

saveRDS(bal, path_balance)
