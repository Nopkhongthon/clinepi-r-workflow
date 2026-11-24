# Explore the fasting blood sugar (FBS) trial, drug A vs drug B --------------
# Input:  output/fbs_clean.rds, written by 1_clean_data.r (one row per visit)
# Output: output/fbs_explore.rds, the result tables this script builds.
#
# Nothing here changes the data. The tables come from epicalc, the package
# covered in this site; the pictures come from ggplot2.
# The question a later script (not written yet) answers with a mixed model: does
# FBS fall more on drug B than on drug A over 12 months?

#%%@note step 1: packages and paths -------------------------------------------

# epicalc left CRAN in 2015. A maintained version is hosted here:
# install.packages("epicalc", repos = c("https://medipe.psu.ac.th/epicalc", "https://cloud.r-project.org"))
# install.packages(c("dplyr", "tidyr", "ggplot2", "rstatix"))

library(epicalc)  # tab1(), tabpct(), tableStack(), aggregate(): the tables
library(dplyr)    # filter(), mutate(): the verbs
library(tidyr)    # pivot_wider(): one FBS column per visit in step 10
library(ggplot2)  # the pictures
library(rstatix)  # cor_test(): every Spearman pair with p, epicalc has none

# epicalc, MASS (loaded by epicalc), dplyr and rstatix each have a select(),
# and epicalc and dplyr each have a recode() and a rename(). The package loaded
# last wins, which is easy to get wrong, so every call below names its package.

path_clean   <- "output/fbs_clean.rds"
path_explore <- "output/fbs_explore.rds"

#%%@note step 2: load the cleaned data ----------------------------------------

dfl <- readRDS(path_clean)

# the baseline characteristics repeat on every visit row, so describe them
# from the month 0 rows only: one row per patient. epicalc was written before
# tibbles, so hand it a plain data frame.
df_base <- dfl |>
  dplyr::filter(month == 0) |>
  as.data.frame()

#%%@note step 3: baseline, the categorical variable ---------------------------

# tab1() is Stata's `tab sex`: counts, % with and without the missing, and a
# bar chart. tabpct() is `tab treatment sex, row col`.
# In a randomised trial the arms differ at baseline only by chance, so we look
# at the balance by eye. No p values here: CONSORT advises against them.
# horiz = TRUE gives the bars 20% of extra room (upright bars get only 10%),
# so the count label of the longest bar is not cut off. tab1 fixes the axis
# limit itself, so a ylim argument is refused.
tab_sex     <- epicalc::tab1(df_base$sex, horiz = TRUE)
print(tab_sex)
tab_sex_arm <- epicalc::tabpct(df_base$treatment, df_base$sex, graph = FALSE)

#%%@note step 4: baseline table, every variable by arm ------------------------

# tableStack() builds the whole "Table 1" in one call: n (%) for factors,
# mean (SD) for numbers. test = FALSE drops the p values (CONSORT again).
# Left on "auto", it picks median (IQR) with a Shapiro test. A good rule is to
# judge by eye instead, so name the skewed variable yourself: diabetes
# duration has a long right tail in the histograms of step 7.
tab_base <- epicalc::tableStack(
  c(age, sex, bmi, dm_duration, fbs),
  dataFrame = df_base,
  by        = treatment,
  test      = FALSE,
  iqr       = c(dm_duration)
)

print(tab_base)

#%%@note step 5: who came to each visit ---------------------------------------

# The dropout pattern: visits by month (rows) and arm (columns). A mixed model
# keeps every visit a patient did attend, so a patient who left at month 6
# still counts for month 0 and 3.
tab_visits <- epicalc::tabpct(dfl$month, dfl$treatment, graph = FALSE)

#%%@note step 6: FBS at each visit, by arm ------------------------------------

# This table is the trial in numbers: both arms start near the same level,
# then watch how far each one falls. epicalc's aggregate() takes several
# statistics at once in FUN.
tab_fbs <- epicalc::aggregate.numeric(
  dfl$fbs,
  by  = list(treatment = dfl$treatment, month = dfl$month),
  FUN = c("mean", "sd", "count")
)

print(tab_fbs)

#%%@note step 7: are the baseline variables roughly normal? Judge by eye ------

# Judge normality by eye, not by a test: look at a histogram and a box plot of
# each variable. The three numbers differ in unit, so each panel has its own
# scale (scales = "free") and its own axes.
df_base_long <- df_base |>
  dplyr::select(age, bmi, dm_duration) |>
  tidyr::pivot_longer(dplyr::everything(), names_to = "variable", values_to = "value")

p_base_hist <- ggplot2::ggplot(df_base_long, ggplot2::aes(x = value)) +
  ggplot2::geom_histogram(bins = 20, fill = "grey70", colour = "white") +
  ggplot2::facet_wrap(~ variable, scales = "free", axes = "all", axis.labels = "all") +
  ggplot2::labs(x = NULL, y = "Patients") +
  ggplot2::theme_classic()

p_base_box <- ggplot2::ggplot(df_base_long, ggplot2::aes(y = value)) +
  ggplot2::geom_boxplot(fill = "grey85") +
  ggplot2::facet_wrap(~ variable, scales = "free", axes = "all", axis.labels = "all") +
  ggplot2::labs(y = NULL) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank()
  )

p_base_hist
p_base_box

#%%@note step 8: is FBS roughly normal? Judge by eye ---------------------------

# Same rule for FBS itself, at every visit.
p_hist <- ggplot2::ggplot(dfl, ggplot2::aes(x = fbs)) +
  ggplot2::geom_histogram(binwidth = 10, fill = "grey70", colour = "white") +
  # axes = "all" draws the x and y axis on every panel, not just the outer
  # ones; the panels still share one scale, so they stay comparable
  ggplot2::facet_wrap(
    ~ month,
    nrow = 1,
    labeller = ggplot2::label_both,
    axes = "all",
    axis.labels = "all"
  ) +
  ggplot2::labs(x = "FBS (mg/dL)", y = "Patients") +
  ggplot2::theme_classic()

p_box <- ggplot2::ggplot(
  dfl,
  ggplot2::aes(x = factor(month), y = fbs, fill = treatment)
) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(x = "Month", y = "FBS (mg/dL)", fill = "Drug") +
  ggplot2::theme_classic()

p_hist
p_box

#%%@note step 9: every patient's own line -------------------------------------

# The "spaghetti plot" is why we need a mixed model. Each grey line is one
# patient: they start at different heights (random intercept) and fall at
# different speeds (random slope). The thick lines are the arm averages.
p_spaghetti <- ggplot2::ggplot(dfl, ggplot2::aes(x = month, y = fbs)) +
  ggplot2::geom_line(ggplot2::aes(group = id), colour = "grey80", linewidth = 0.3) +
  ggplot2::stat_summary(
    ggplot2::aes(colour = treatment),
    fun = mean,
    geom = "line",
    linewidth = 1.2
  ) +
  ggplot2::stat_summary(
    ggplot2::aes(colour = treatment),
    fun = mean,
    geom = "point",
    size = 2
  ) +
  ggplot2::facet_wrap(
    ~ treatment,
    labeller = ggplot2::label_both,
    axes = "all",
    axis.labels = "all"
  ) +
  ggplot2::scale_x_continuous(breaks = c(0, 3, 6, 9, 12)) +
  ggplot2::labs(x = "Month", y = "FBS (mg/dL)", colour = "Drug") +
  ggplot2::theme_classic()

p_spaghetti

#%%@note step 10: visits of the same patient are correlated --------------------

# One row per patient again, one FBS column per visit. If the visits were
# independent, rho would be near 0. It is not: a patient high at month 0 tends
# to stay high. That correlation is what a plain regression ignores and a
# mixed model handles.
cor_visits <- dfl |>
  dplyr::select(id, month, fbs) |>
  tidyr::pivot_wider(names_from = month, values_from = fbs, names_prefix = "fbs_m") |>
  rstatix::cor_test(
    vars = c("fbs_m0", "fbs_m3", "fbs_m6", "fbs_m9", "fbs_m12"),
    method = "spearman"
  ) |>
  # var1 < var2 compares the names as text: it drops each visit paired with
  # itself and keeps one of each mirrored pair
  dplyr::filter(var1 < var2) |>
  dplyr::select(var1, var2, rho = cor, p_value = p)

print(cor_visits, n = Inf)

#%%@note step 11: did the patients who left differ? ---------------------------

# If the patients who dropped out had higher sugar, an analysis of completers
# only would make both drugs look better than they are.
df_dropout <- dfl |>
  dplyr::mutate(completed = any(month == 12), .by = id) |>
  dplyr::filter(month == 0)

tab_dropout <- epicalc::aggregate.numeric(
  df_dropout$fbs,
  by  = list(treatment = df_dropout$treatment, completed = df_dropout$completed),
  FUN = c("mean", "sd", "count")
)

print(tab_dropout)

#%%@note step 12: check -------------------------------------------------------

stopifnot(
  # the numbers we already know from 1_clean_data.r
  nrow(df_base) == 200,
  nrow(dfl) == 912,
  # one row of FBS summary for each arm at each of the five visits
  nrow(tab_fbs) == 2 * 5,
  # every pair of visits got a rho
  nrow(cor_visits) == choose(5, 2)
)

#%%@note step 13: save --------------------------------------------------------

saveRDS(
  list(
    tab_sex = tab_sex,
    tab_sex_arm = tab_sex_arm,
    tab_base = tab_base,
    tab_visits = tab_visits,
    tab_fbs = tab_fbs,
    cor_visits = cor_visits,
    tab_dropout = tab_dropout
  ),
  path_explore
)
