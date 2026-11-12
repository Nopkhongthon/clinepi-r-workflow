# Table 1: baseline characteristics of the FBS trial, by treatment arm ---------
# Input:  output/fbs_clean.rds, written by 1_clean_data.r (one row per visit)
# Output: output/table1.rds   the gtsummary object, to reuse or restyle later
#         output/table1.docx  the table in Word, ready to paste into a report
#
# Every number in the table is computed by gtsummary. Nothing is typed by hand,
# so if the data change, re-run the script and the table changes with them.

#%%@note step 1: packages and paths -------------------------------------------

# install.packages(c("dplyr", "gtsummary", "gt"))

library(dplyr)      # filter(): keep the baseline visit
library(gtsummary)  # tbl_summary(): the whole Table 1 from one call
library(gt)         # gtsave(): send the table to Word

path_clean <- "output/fbs_clean.rds"
path_rds   <- "output/table1.rds"
path_docx  <- "output/table1.docx"

#%%@note step 2: one row per patient ------------------------------------------

# Table 1 describes the patients as they entered the trial, so it uses the
# month 0 visit only. The long data repeat age and sex on every visit, and
# counting all 912 rows would count each patient up to five times.
df_base <- readRDS(path_clean) |>
  dplyr::filter(month == 0)

message(sprintf("patients: %d, drug A: %d, drug B: %d",
                nrow(df_base), sum(df_base$treatment == "A"), sum(df_base$treatment == "B")))

#%%@note step 3: the basic table ----------------------------------------------

# Run this cell alone first and look at the default: gtsummary picks a
# statistic for each variable by itself. The next steps change only what we
# decided on purpose.
tbl1 <- df_base |>
  gtsummary::tbl_summary(
    include = c(age, sex, bmi, dm_duration, fbs),
    by      = treatment
  )

tbl1

#%%@note step 4: labels, statistics and missing data --------------------------

# statistic: mean (SD) for the variables that looked symmetric in the
# histograms of 2_basic_explore.r, median (Q1, Q3) for diabetes duration,
# which has a long right tail. The choice is made by eye, not by a test.
tbl1 <- df_base |>
  gtsummary::tbl_summary(
    include = c(age, sex, bmi, dm_duration, fbs),
    by      = treatment,
    label   = list(
      age         ~ "Age (years)",
      sex         ~ "Sex",
      bmi         ~ "Body mass index (kg/m²)",
      dm_duration ~ "Diabetes duration (years)",
      fbs         ~ "Fasting blood sugar (mg/dL)"
    ),
    type      = list(age ~ "continuous", dm_duration ~ "continuous"),
    statistic = list(
      c(age, bmi, fbs) ~ "{mean} ({sd})",
      dm_duration      ~ "{median} ({p25}, {p75})",
      sex              ~ "{n} ({p}%)"
    ),
    # diabetes duration was recorded in whole years, so no decimals
    digits    = list(c(age, bmi, fbs) ~ 1, dm_duration ~ 0),
    # no "Unknown" rows inside the table: missing data get their own column
    missing   = "no"
  ) |>
  # one column, right after the characteristic, with the missing count and
  # its % of all 200 patients. add_n() places it next to the label.
  gtsummary::add_n(
    statistic = "{N_miss} ({p_miss}%)",
    col_label = "**Missing data, n (%)**"
  )

tbl1

#%%@note step 5: overall column, headers and title ----------------------------

# No p values: in a randomised trial any baseline difference is chance by
# design, so a test answers a question nobody asked. CONSORT says leave them out.
# no "Table 1." in the text: Word numbers its tables itself ("Table 1:")
table_title <- "Baseline characteristics by treatment arm"

tbl1 <- tbl1 |>
  gtsummary::add_overall(col_label = "**Overall**, N = {N}") |>
  gtsummary::modify_header(
    label          = "**Characteristic**",
    gtsummary::all_stat_cols(stat_0 = FALSE) ~ "**Drug {level}**, N = {n}"
  ) |>
  gtsummary::bold_labels() |>
  # the caption shows in the Viewer pane; step 7 adds it again for Word
  gtsummary::modify_caption(table_title)

tbl1

#%%@note step 6: check --------------------------------------------------------

# the N in each column header comes from the data, so check it against the
# numbers we already know from 1_clean_data.r
n_cols <- tbl1$table_styling$header |>
  dplyr::filter(column %in% c("stat_0", "stat_1", "stat_2")) |>
  dplyr::pull(modify_stat_n)

stopifnot(
  nrow(df_base) == 200,
  # overall, drug A, drug B
  all(n_cols == c(200, 100, 100))
)

#%%@note step 7: save --------------------------------------------------------

saveRDS(tbl1, path_rds)

# as_gt() turns the gtsummary table into a gt table; gtsave() picks the
# file type from the extension, so ".docx" gives a Word file.
# gtsave() leaves the gtsummary caption out of a Word file, so the title goes
# in again as a gt header. Word shows it as a numbered caption, "Table 1: ...".
tbl1 |>
  gtsummary::as_gt() |>
  gt::tab_header(title = table_title) |>
  gt::gtsave(filename = path_docx)
