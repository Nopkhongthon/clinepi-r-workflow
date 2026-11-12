# Simulate a fasting blood sugar (FBS) trial, drug A vs drug B ----------------
# Output: data/fbs_sim_long.csv  one row per patient per visit (for mixed models)
#         data/fbs_sim_wide.csv  one row per patient, the layout a clinic types
#
# Every number is made up by this script. No real patient is in it, so the
# files can go on GitHub with no privacy concern.
#
# The design: patients with type 2 diabetes are randomised 1:1 to drug A or
# drug B, and FBS is measured at month 0 (baseline), 3, 6, 9 and 12.
# The true effects are written in step 1. A correct analysis should find them.

#%%@note step 1: packages, paths and the truth --------------------------------

# install.packages(c("dplyr", "tidyr", "tibble", "readr"))

library(dplyr)   # verbs
library(tidyr)   # pivot_wider() for the wide file
library(tibble)  # tibble(): build the patients from scratch
library(readr)   # write_csv(): plain files that Stata and Excel can open too

path_long <- "data/fbs_sim_long.csv"
path_wide <- "data/fbs_sim_wide.csv"

set.seed(77454)

n_patients <- 200
months     <- c(0, 3, 6, 9, 12)

# THE TRUTH. Change a number here and every file below changes with it.
fbs_baseline <- 170   # mg/dL, average FBS before treatment
drop_a       <- 25    # mg/dL, how far drug A lowers FBS once it has fully worked
drop_b       <- 40    # mg/dL for drug B, so B beats A by 15 mg/dL  <- TREATMENT EFFECT
speed        <- 4     # months; about 63% of the drop is done by month 4, then levels off
sd_intercept <- 18    # patients differ in their own FBS level       (random intercept)
sd_slope     <- 0.5   # ... and in how fast it drifts, mg/dL a month (random slope)
sd_residual  <- 12    # visit to visit noise: last meal, sleep, the lab

#%%@note step 2: one row per patient, the baseline characteristics ------------

df <- tibble::tibble(
  id          = 1:n_patients,
  # exactly half on each drug, in random order
  treatment   = sample(rep(c("A", "B"), length.out = n_patients)),
  age         = round(rnorm(n_patients, mean = 58, sd = 9)),
  sex         = dplyr::if_else(rbinom(n_patients, 1, 0.45) == 1, "Male", "Female"),
  bmi         = round(rnorm(n_patients, mean = 27, sd = 4), 1),
  # years since diagnosis, right skewed like the real thing, mean 8
  dm_duration = round(rgamma(n_patients, shape = 2, scale = 4)),
  enrol_date  = as.Date("2024-01-08") + sample(0:364, n_patients, replace = TRUE),
  # each patient's own deviation from the average, never shown to students
  u_intercept = rnorm(n_patients, 0, sd_intercept),
  u_slope     = rnorm(n_patients, 0, sd_slope)
)

#%%@note step 3: one row per patient per visit --------------------------------

# every patient gets all five visits; dropout is added in step 5
df <- df |>
  dplyr::cross_join(tibble::tibble(month = months))

#%%@note step 4: the FBS value at each visit ----------------------------------

df <- df |>
  dplyr::mutate(
    # the drug effect grows over the first months, then levels off
    drug_drop = dplyr::if_else(treatment == "B", drop_b, drop_a) * (1 - exp(-month / speed)),
    # older, heavier, longer diagnosed and male patients start higher
    fbs_true  = fbs_baseline +
                0.3 * (age - 58) + 1.2 * (bmi - 27) + 2 * (dm_duration - 8) + 3 * (sex == "Male") +
                u_intercept + u_slope * month - drug_drop,
    fbs       = round(fbs_true + rnorm(dplyr::n(), 0, sd_residual)),
    # a follow up visit happens within 14 days of its due date
    visit_date = enrol_date + round(month * 30.44) +
                 dplyr::if_else(month == 0, 0, sample(-14:14, dplyr::n(), replace = TRUE))
  )

#%%@note step 5: patients drop out ---------------------------------------------

# A patient whose sugar stayed high at the last visit is more likely to stop
# coming. That makes the missing data "missing at random": a mixed model that
# uses every visit stays unbiased if the model is correctly specified, an
# analysis of completers does not.
df <- df |>
  dplyr::arrange(id, month) |>
  dplyr::mutate(
    fbs_last   = dplyr::lag(fbs),
    p_drop     = dplyr::if_else(month == 0, 0, plogis(-3 + 0.03 * (fbs_last - fbs_baseline))),
    drop_now   = rbinom(dplyr::n(), 1, p_drop),
    dropped    = cumsum(drop_now) > 0,
    .by = id
  ) |>
  dplyr::filter(!dropped)

message(sprintf("patients still in the study at month 12: %d of %d",
                sum(df$month == 12), n_patients))

#%%@note step 6: keep only what a real study would record ---------------------

# the truth columns (u_*, fbs_true, drug_drop) and the dropout working columns
# are removed, so students see only what a clinic could have measured
df <- df |>
  dplyr::select(id, treatment, age, sex, bmi, dm_duration, enrol_date,
                month, visit_date, fbs)

#%%@note step 7: check --------------------------------------------------------

stopifnot(
  # every patient has a baseline visit
  dplyr::n_distinct(df$id) == n_patients,
  sum(df$month == 0) == n_patients,
  # at most one row per patient per visit
  nrow(dplyr::distinct(df, id, month)) == nrow(df),
  # the values look like a diabetes clinic. A treated patient can dip below
  # 70 (low sugar), but under 40 would be an emergency, not a clinic visit.
  all(dplyr::between(df$fbs, 40, 400)),
  all(df$age >= 18)
)

#%%@note step 8: save, long and wide ------------------------------------------

readr::write_csv(df, path_long)

df |>
  tidyr::pivot_wider(names_from = month, values_from = c(visit_date, fbs),
                     names_glue = "{.value}_m{month}") |>
  readr::write_csv(path_wide)
