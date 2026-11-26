# Propensity score, part 3: the effect of drug B, as a marginal effect --------
# Input:  output/ps_cohort.rds   the cohort with ps, weight and comsup, script 5
# Output: output/ps_effect.rds   one row per method: B minus A, 95% CI, p value
#
# The question from script 5: does drug B lower FBS at month 6 more than drug A?
# Script 6 showed the weights balance the confounders. Now the effect itself.
#
# Every method below ends the same way, with a MARGINAL effect. emmeans
# predicts month 6 FBS for every patient twice, once as if on drug A and once
# as if on drug B (counterfactual), averages each set, and takes B minus A.
# That is the average treatment effect (ATE) in mg/dL, the same estimand as the
# weights in script 5. A model coefficient is a CONDITIONAL effect, for a
# patient with fixed covariates. The two agree only in simple linear models.
#
# The data are a mock-up, so the truth is known: B minus A = -15 mg/dL.

#%%@note step 1: packages and paths -------------------------------------------

# install.packages(c("emmeans", "MatchIt", "optmatch", "sandwich", "broom", "forcats"))

library(dplyr)     # verbs
library(ggplot2)   # the forest plot
library(emmeans)   # emmeans() and contrast(): the marginal effect
library(survey)    # svydesign() and svyglm(): the weighted model, robust SE
library(MatchIt)   # matchit(): propensity score matching
library(sandwich)  # vcovCL(): SE that respect the matched sets
library(broom)     # tidy(): each contrast as one row
library(forcats)   # fct_inorder(): keep the methods in table order in the plot

path_ps     <- "output/ps_cohort.rds"
path_effect <- "output/ps_effect.rds"

truth <- -15  # mg/dL, set in the simulation

#%%@note step 2: import --------------------------------------------------------

df <- readRDS(path_ps)

# The PS methods use the patients inside common support only (script 5, step 7)
df_cs <- df |>
  dplyr::filter(comsup)

#%%@note step 3: crude, no adjustment -----------------------------------------

# The plain difference in mean month 6 FBS. Confounded by indication.
fit_crude <- lm(
  fbs_m6 ~ treatment,
  data = df
)

emm_crude <- emmeans::emmeans(
  fit_crude,
  specs = ~ treatment
)

# revpairwise gives B minus A, the second level minus the first
con_crude <- emmeans::contrast(
  emm_crude,
  method = "revpairwise"
)

con_crude

#%%@note step 4: multivariable regression -------------------------------------

# The outcome model adjusts for the confounders directly. No PS.
# counterfactual = "treatment": every patient is predicted on A and on B.
fit_multi <- lm(
  fbs_m6 ~ treatment + age + sex + bmi + dm_duration + fbs_m0,
  data = df
)

emm_multi <- emmeans::emmeans(
  fit_multi,
  specs = ~ treatment,
  counterfactual = "treatment"
)

emm_multi

con_multi <- emmeans::contrast(
  emm_multi,
  method = "revpairwise"
)

con_multi

#%%@note step 5: PS as a covariate --------------------------------------------

# One number, the PS, stands in for all five confounders.
fit_pscov <- lm(
  fbs_m6 ~ treatment + ps,
  data = df_cs
)

emm_pscov <- emmeans::emmeans(
  fit_pscov,
  specs = ~ treatment,
  counterfactual = "treatment"
)

con_pscov <- emmeans::contrast(
  emm_pscov,
  method = "revpairwise"
)

con_pscov

#%%@note step 6: PS strata ----------------------------------------------------

# Cut the PS into five groups of equal size (quintiles). Inside one stratum,
# A and B patients have similar PS, so they are comparable.
df_cs <- df_cs |>
  dplyr::mutate(ps_strata = factor(dplyr::ntile(ps, 5)))

# treatment * ps_strata: a separate B minus A in each stratum.
# The coefficient of treatment is now the effect in stratum 1 only.
# The marginal effect averages the five stratum effects over all patients.
fit_strata <- lm(
  fbs_m6 ~ treatment * ps_strata,
  data = df_cs
)

emm_strata <- emmeans::emmeans(
  fit_strata,
  specs = ~ treatment,
  counterfactual = "treatment"
)

con_strata <- emmeans::contrast(
  emm_strata,
  method = "revpairwise"
)

con_strata

#%%@note step 7: IPTW ---------------------------------------------------------

# The weight from script 5 builds the pseudo-population. svyglm() gives a
# robust (sandwich) SE, because the weights are not real patient counts.
design_iptw <- survey::svydesign(
  ids = ~1,
  weights = ~weight,
  data = df_cs
)

fit_iptw <- survey::svyglm(
  fbs_m6 ~ treatment,
  design = design_iptw,
  family = gaussian(link = "identity")
)

# The model holds only the drug, so no counterfactual is needed: the two
# emmeans are already the weighted mean FBS of the pseudo-population.
emm_iptw <- emmeans::emmeans(
  fit_iptw,
  specs = ~ treatment
)

con_iptw <- emmeans::contrast(
  emm_iptw,
  method = "revpairwise"
)

con_iptw

#%%@note step 8: PS matching --------------------------------------------------

# Full matching puts every patient in a matched set with at least one A and
# one B of similar PS, and gives each patient a matching weight.
# estimand = "ATE" keeps the same target as the other methods.
# method = "full" needs the optmatch package installed.
match_full <- MatchIt::matchit(
  treatment ~ age + sex + bmi + dm_duration + fbs_m0,
  data = df_cs,
  method = "full",
  estimand = "ATE",
  distance = "glm"
)

match_full

# the matched data: the cohort plus `weights` and `subclass` (the matched set)
dfm <- MatchIt::match.data(match_full)

fit_match <- lm(
  fbs_m6 ~ treatment,
  data = dfm,
  weights = weights
)

# patients in one matched set are not independent: cluster the SE by subclass
# the model holds only the drug, as in IPTW
emm_match <- emmeans::emmeans(
  fit_match,
  specs = ~ treatment,
  vcov. = sandwich::vcovCL(fit_match, cluster = ~subclass)
)

con_match <- emmeans::contrast(
  emm_match,
  method = "revpairwise"
)

con_match

#%%@note step 9: all methods side by side -------------------------------------

# One row per method, all on the same scale: B minus A in mg/dL
effect_table <- dplyr::bind_rows(
  broom::tidy(con_crude, conf.int = TRUE) |> dplyr::mutate(method = "Crude"),
  broom::tidy(con_multi, conf.int = TRUE) |> dplyr::mutate(method = "Multivariable regression"),
  broom::tidy(con_pscov, conf.int = TRUE) |> dplyr::mutate(method = "PS as a covariate"),
  broom::tidy(con_strata, conf.int = TRUE) |> dplyr::mutate(method = "PS strata"),
  broom::tidy(con_iptw, conf.int = TRUE) |> dplyr::mutate(method = "IPTW"),
  broom::tidy(con_match, conf.int = TRUE) |> dplyr::mutate(method = "PS full matching")
) |>
  dplyr::select(method, estimate, conf.low, conf.high, p.value)

effect_table

#%%@note step 10: forest plot against the truth --------------------------------

# The dashed line is the truth, -15. The crude estimate should miss it.
# Every adjusted method should cover it.
p_effect <- ggplot2::ggplot(
  effect_table,
  ggplot2::aes(
    x = estimate,
    y = forcats::fct_rev(forcats::fct_inorder(method))
  )
) +
  ggplot2::geom_vline(
    xintercept = truth,
    linetype = "dashed"
  ) +
  ggplot2::geom_vline(xintercept = 0) +
  ggplot2::geom_pointrange(
    ggplot2::aes(xmin = conf.low, xmax = conf.high)
  ) +
  ggplot2::labs(
    x = "FBS at month 6, drug B minus drug A (mg/dL), 95% CI",
    y = NULL
  ) +
  ggplot2::theme_classic()

p_effect

#%%@note step 11: check and save ----------------------------------------------

# every adjusted method covers the truth; the crude one does not
adjusted <- effect_table |>
  dplyr::filter(method != "Crude")

stopifnot(
  nrow(effect_table) == 6,
  all(adjusted$conf.low < truth & adjusted$conf.high > truth),
  effect_table$conf.low[effect_table$method == "Crude"] > truth
)

saveRDS(effect_table, path_effect)
