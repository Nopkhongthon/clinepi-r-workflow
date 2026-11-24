# Propensity score, part 1: the model, the weights and the overlap ------------
# Input:  data/fbs_mockup_cohort.xlsx, a retrospective cohort, one row per patient
# Output: output/ps_cohort.rds   the cohort with ps, weight and common support
#
# The question: does drug B lower FBS at month 6 more than drug A?
# Nobody was randomised. The doctor chose the drug, and chose B more often for
# patients whose diabetes looked worse. So the two groups differ before the
# drug is even started, and a plain comparison of month 6 FBS is confounded.
#
# The propensity score (PS) is each patient's probability of getting drug B,
# given the confounders. Patients with the same PS are comparable, whichever
# drug they actually got. This script builds the PS. Script 6 checks balance,
# and script 7 estimates the effect.

#%%@note step 1: packages and paths -------------------------------------------

# install.packages(c("readxl", "dplyr", "ggplot2"))

library(readxl)   # read the Excel sheet
library(dplyr)    # verbs
library(ggplot2)  # the overlap histogram

path_raw <- "data/fbs_mockup_cohort.xlsx"
path_ps  <- "output/ps_cohort.rds"

#%%@note step 2: import --------------------------------------------------------

# This sheet is already clean, so the types can be set straight away.
# Drug A is the first level, so the model below gives the probability of B.
df <- readxl::read_excel(path_raw, sheet = "data") |>
  dplyr::mutate(
    treatment = factor(treatment, levels = c("A", "B")),
    sex = factor(sex, levels = c("Female", "Male"))
  )

#%%@note step 3: the propensity score model -----------------------------------

# Logistic regression of the treatment on the confounders: the variables that
# made the doctor choose B AND that also change FBS at month 6.
# The outcome fbs_m6 is never in this model. The PS describes who got which
# drug, not what happened afterwards.
ps_model <- glm(
  treatment ~ age + sex + bmi + dm_duration + fbs_m0,
  family = binomial(link = "logit"),
  data = df
)

summary(ps_model)

#%%@note step 4: each patient's propensity score -------------------------------

# type = "response" gives a probability between 0 and 1, not the log odds
df <- df |>
  dplyr::mutate(ps = predict(ps_model, type = "response"))

#%%@note step 5: the weights, ATE ---------------------------------------------

# Inverse probability of treatment weighting (IPTW).
# A patient on B gets 1 / ps, a patient on A gets 1 / (1 - ps).
# It is NOT simply 1 / ps for everyone. Each patient is weighted by the
# probability of the drug they actually got.
# A patient on B with ps = 0.2 was unusual for B, so stands in for 5 patients.
# This weight answers the average treatment effect (ATE): everyone on B
# compared with everyone on A.
df <- df |>
  dplyr::mutate(
    weight = dplyr::case_when(
      treatment == "B" ~ 1 / ps,
      treatment == "A" ~ 1 / (1 - ps)
    )
  )

summary(df$weight)

#%%@note step 6: overlap, the PS of each group ---------------------------------

# Patients on B should have higher PS than patients on A; that is confounding
# by indication. The two histograms must still overlap. Where only one group
# exists, there is nobody to compare with (the positivity assumption).
p_overlap <- ggplot2::ggplot(
  df,
  ggplot2::aes(x = ps, fill = treatment)
) +
  ggplot2::geom_histogram(
    ggplot2::aes(y = ggplot2::after_stat(density)),
    binwidth = 0.05,
    alpha = 0.5,
    position = "identity"
  ) +
  ggplot2::labs(
    x = "Propensity score, probability of drug B",
    y = "Density",
    fill = "Drug"
  ) +
  ggplot2::theme_classic()

p_overlap

#%%@note step 7: common support -----------------------------------------------

# Common support is the PS range where both groups have patients: from the
# larger of the two minimums to the smaller of the two maximums.
ps_range <- df |>
  dplyr::summarise(
    ps_min = min(ps),
    ps_max = max(ps),
    .by = treatment
  )

ps_range

cs_low <- max(ps_range$ps_min)
cs_high <- min(ps_range$ps_max)

df <- df |>
  dplyr::mutate(comsup = dplyr::between(ps, cs_low, cs_high))

# the patients outside common support, kept here to look at
off_support <- df |>
  dplyr::filter(!comsup) |>
  dplyr::select(id, treatment, ps)

off_support

#%%@note step 8: check and save -----------------------------------------------

stopifnot(
  nrow(df) == 600,
  all(df$ps > 0 & df$ps < 1),
  all(df$weight >= 1)
)

saveRDS(df, path_ps)
